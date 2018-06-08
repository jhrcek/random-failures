{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Conduit              (concatC, concatMapMC, iterMC, mapMC,
                                       runConduit, sinkList, yieldMany, (.|))
import qualified Config
import           Control.Lens         (Fold, filtered, to, (^.), (^..))
import qualified Data.Aeson           as Aeson
import           Data.Aeson.Lens      (key, _Array, _JSON, _String)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS (writeFile)
import qualified Data.List            as List
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text
import           Data.Time.Clock      (UTCTime, getCurrentTime)
import           Data.Time.Format     (defaultTimeLocale, formatTime,
                                       parseTimeM)
import           Failure              (TestFailure, isFailure, toFailure)
import           Merge                (mergeReports)
import qualified Network.Wreq         as Wreq
import           System.FilePath      ((</>))
import qualified Text.Atom.Feed       as Atom
import           Text.Feed.Import     (parseFeedSource)
import           Text.Feed.Types      (Feed (AtomFeed))

main :: IO ()
main = do
    jobUrls <- getMasterPrJobUrls
    Text.putStrLn $ lengthText jobUrls <> " Jenkins jobs to analyze"
    testFailures <- runConduit
        $ yieldMany jobUrls
        .| iterMC (\(JobUrl jobUrl) -> Text.putStrLn $ "Analysing job " <> lastUrlComponent jobUrl)
        .| concatMapMC getUnstableBuilds
        .| iterMC (\(BuildUrl buildUrl, _dateTime) ->
                      Text.putStr $ "    Unstable build #" <> lastUrlComponent buildUrl)
        .| mapMC getTestFailures
        .| iterMC (\failures -> Text.putStrLn $ " has " <> lengthText failures <> " test failure(s)")
        .| concatC
        .| sinkList
    reportsDir <- Config.getReportsDir
    saveReport reportsDir testFailures
    mergeReports reportsDir

saveReport :: FilePath -> [TestFailure] -> IO ()
saveReport reportsDir failures = do
    utcTime <- getCurrentTime
    let reportFileName = "failures_" <> formatTime defaultTimeLocale "%F" utcTime <> ".json"
        reportPath = reportsDir </> reportFileName
    Text.putStrLn $ "Saving " <> lengthText failures <> " to " <> Text.pack reportPath
    BS.writeFile reportPath (Aeson.encode failures)

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
newtype JobUrl = JobUrl Text
newtype BuildUrl = BuildUrl Text deriving Show
newtype TestReportUrl = TestReportUrl Text

lastUrlComponent :: Text -> Text
lastUrlComponent = Text.takeWhileEnd (/='/') . Text.init

lengthText :: [a] -> Text
lengthText = Text.pack . show . length
