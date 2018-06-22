module Jenkins
  ( getAllBuilds
  , getBuildNumber
  , getBuildStats
  , getJobName
  , getMasterPrJobUrls
  , getTestFailures
  , getUnstableBuilds
  , BuildUrl(BuildUrl)
  , JobUrl(JobUrl)
  , BuildStats(..)
  , BuildResult(..)
  ) where

import           Control.Lens         (Fold, filtered, to, (^.), (^..))
import           Data.Aeson           (FromJSON, parseJSON, withObject, (.!=),
                                       (.:), (.:?))
import           Data.Aeson.Lens      (key, _Array, _JSON, _Number, _String)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.List            as List
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time.Clock      (UTCTime)
import           Data.Time.Format     (defaultTimeLocale, parseTimeM)
import           Failure              (TestFailure, isFailure, toFailure)
import qualified Network.Wreq         as Wreq
import qualified Text.Atom.Feed       as Atom
import           Text.Feed.Import     (parseFeedSource)
import           Text.Feed.Types      (Feed (AtomFeed))
import           Text.Read            (readMaybe)

newtype JobUrl = JobUrl Text deriving Show

getJobName :: JobUrl -> Text
getJobName (JobUrl url) = lastUrlComponent url

newtype BuildUrl = BuildUrl Text deriving Show

getBuildNumber :: BuildUrl -> Text
getBuildNumber (BuildUrl url) = lastUrlComponent url

newtype TestReportUrl = TestReportUrl Text
--------------------------------------------------------------------------------
{-| Extract list of PR builder job URLs that build from master -}
getMasterPrJobUrls :: IO [JobUrl]
getMasterPrJobUrls = do
    resp <- Wreq.get "https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/api/json"
    return $ resp ^. Wreq.responseBody . to extractMasterPrJobUrls

extractMasterPrJobUrls :: ByteString -> [JobUrl]
extractMasterPrJobUrls body =
    fmap JobUrl $ body ^..
          key "jobs" . _Array . traverse
        . key "url" . _String
        -- focus on master jobs by filtering out URLs ending like "-7.7.x" etc.
        . filtered (Text.isSuffixOf "-pullrequests/")

--------------------------------------------------------------------------------
{-| Extract unstable build URLs from Job's "rssFailed" Atom feed -}
getUnstableBuilds :: JobUrl -> IO [(BuildUrl, UTCTime)]
getUnstableBuilds (JobUrl jobUrl) = do
    resp <- Wreq.get . Text.unpack $ jobUrl <> "rssFailed"
    return $ resp ^. Wreq.responseBody . to extractUnstableBuilds

extractUnstableBuilds :: ByteString -> [(BuildUrl, UTCTime)]
extractUnstableBuilds body =
    maybe [] fromFeed (parseFeedSource body)
  where
    fromFeed :: Feed -> [(BuildUrl, UTCTime)]
    fromFeed (AtomFeed feed) =
        fmap fromEntry
        . filter isUnstableBuild
        $ Atom.feedEntries feed
    fromFeed _               = []

    fromEntry :: Atom.Entry -> (BuildUrl, UTCTime)
    fromEntry entry =
        ( BuildUrl . Atom.linkHref . head $ Atom.entryLinks entry
        , parseTime $ Atom.entryUpdated entry
        )

    -- exclude broken (=red) and aborted (=gray) builds and just leave unstable ones (=yellow)
    isUnstableBuild :: Atom.Entry -> Bool
    isUnstableBuild = not
        . (\title -> List.isInfixOf "(broken" title || List.isInfixOf "(aborted)" title)
        . Atom.txtToString . Atom.entryTitle

    parseTime :: Text -> UTCTime
    parseTime t = fromMaybe
        (error $ "unable to parse time " <> Text.unpack t)
        (parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" $ Text.unpack t)

--------------------------------------------------------------------------------
{-| Get urls of all builds associated with given job -}
getAllBuilds :: JobUrl -> IO [BuildUrl]
getAllBuilds (JobUrl jobUrl) = do
    resp <- Wreq.get . Text.unpack $ jobUrl <> "api/json"
    return $ resp ^. Wreq.responseBody . to (fmap mkBuildUrl . extractBuildNumbers)
  where
    mkBuildUrl num = BuildUrl (jobUrl <> Text.pack (show num) <> "/")

extractBuildNumbers :: ByteString -> [Int]
extractBuildNumbers body = body ^.. key "builds" . _Array . traverse . key "number" . _Number . to round

--------------------------------------------------------------------------------
{-| Get info about given build -}
data BuildStats = BuildStats
    { buildDurationMilis :: Integer
    , buildResult        :: BuildResult
    } deriving Show

instance FromJSON BuildStats where
    parseJSON = withObject "BuildStats" $ \o -> BuildStats
        <$> o .: "duration"
        <*> (parseResult <$> o .:? "result" .!= "RUNNING") -- "result": null -> build is still running

data BuildResult = SUCCESS | FAILURE | UNSTABLE | ABORTED | RUNNING deriving (Eq, Read, Show)

getBuildStats :: BuildUrl -> IO BuildStats
getBuildStats (BuildUrl buildUrl) = do
    resp <- Wreq.asJSON =<< Wreq.get (Text.unpack (buildUrl <> "api/json"))
    return $ resp ^. Wreq.responseBody

parseResult :: Text -> BuildResult
parseResult text =
    let str = Text.unpack text
    in fromMaybe (error $ "Failed to parse BuildResult from " <> str) $ readMaybe str

--------------------------------------------------------------------------------
{-| Extract test failures from json "testReport" associated with each build -}
getTestFailures :: (BuildUrl, UTCTime) -> IO [TestFailure]
getTestFailures (bu@(BuildUrl buildUrl), utcTime) = do
    resp <- Wreq.get $ Text.unpack testReportUrl
    let failures = resp ^.. Wreq.responseBody . collectFailures
    return failures
  where
    (TestReportUrl testReportUrl) = toTestReportUrl bu

    collectFailures :: Fold ByteString TestFailure
    collectFailures =
        key "suites" . _Array . traverse
        . key "cases" . _Array . traverse . _JSON
        . filtered isFailure . to (toFailure buildUrl utcTime)

toTestReportUrl :: BuildUrl -> TestReportUrl
toTestReportUrl (BuildUrl buildUrl) =
    -- base64-encode the value of "tree" parameter "suites[cases[className,errorDetails,errorStackTrace,name,status]]"
    -- so that wreq doesn't throw "Invalid URL"
    TestReportUrl $ buildUrl <> "testReport/api/json?tree=suites%5Bcases%5BclassName%2CerrorDetails%2CerrorStackTrace%2Cname%2Cstatus%5D%5D"

--------------------------------------------------------------------------------
lastUrlComponent :: Text -> Text
lastUrlComponent = Text.takeWhileEnd (/='/') . Text.init
