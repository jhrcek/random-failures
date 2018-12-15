module Jenkins
  ( getAllBuilds
  , getBuildNumber
  , getBuildStats
  , getFolderItems
  , getJobName
  , getJobsRecursively
  , getTestFailures
  , getUnstableBuilds
  , BuildUrl(BuildUrl)
  , FolderUrl(FolderUrl)
  , JobUrl(JobUrl)
  , BuildStats(..)
  , BuildResult(..)
  , FolderItem(..)
  ) where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.Wreq as Wreq
import qualified Text.Atom.Feed as Atom

import Control.Lens (Fold, filtered, to, (^.), (^..))
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON, Value (String), object, parseJSON, toJSON,
                   withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Lens (key, _Array, _JSON, _Number, _String)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Failure (TestFailure, isFailure, toFailure)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed (AtomFeed))
import Text.Read (readMaybe)

newtype JobUrl = JobUrl Text deriving (Eq, Ord, Show)

getJobName :: JobUrl -> Text
getJobName (JobUrl url) = lastUrlComponent url

newtype BuildUrl = BuildUrl Text deriving Show

getBuildNumber :: BuildUrl -> Text
getBuildNumber (BuildUrl url) = lastUrlComponent url

newtype TestReportUrl = TestReportUrl Text

newtype FolderUrl = FolderUrl Text deriving Show

--------------------------------------------------------------------------------
{-| Item in the listing of jobs -}
data FolderItem = FolderItem
    { fiItemType :: FolderItemType
    , fiName     :: Text
    , fiUrl      :: Text
    } deriving Show

data FolderItemType
    = BuildFlow
    | Folder
    | FreestyleProject
    | MatrixProject
    | MavenModuleSet
    | MultiJobProject
    | WorkflowJob
    | UnkwnownItemType Text
    deriving Show

instance FromJSON FolderItemType where
    parseJSON = withText "FolderItemType" $ \t -> return $ case t of
        "com.cloudbees.hudson.plugins.folder.Folder"         -> Folder
        "com.cloudbees.plugins.flow.BuildFlow"               -> BuildFlow
        "com.tikal.jenkins.plugins.multijob.MultiJobProject" -> MultiJobProject
        "hudson.matrix.MatrixProject"                        -> MatrixProject
        "hudson.maven.MavenModuleSet"                        -> MavenModuleSet
        "hudson.model.FreeStyleProject"                      -> FreestyleProject
        "org.jenkinsci.plugins.workflow.job.WorkflowJob"     -> WorkflowJob
        other                                                -> UnkwnownItemType other

instance ToJSON FolderItemType where
    toJSON = String . Text.pack . show

instance FromJSON FolderItem where
    parseJSON = withObject "FolderItem" $ \o -> FolderItem
        <$> o .: "_class"
        <*> o .: "name"
        <*> o .: "url"

instance ToJSON FolderItem where
    toJSON (FolderItem c n u) = object
        [ "_class" .= c
        , "name" .= n
        , "url" .= u
        ]

getFolderItems :: FolderUrl -> IO [FolderItem]
getFolderItems (FolderUrl folderUrl) = do
    resp <- Wreq.get . Text.unpack $ folderUrl <> "/api/json"
    return $ resp ^. Wreq.responseBody . to extractFolderItems

extractFolderItems :: ByteString -> [FolderItem]
extractFolderItems body =
  body ^.. key "jobs" . _Array . traverse . _JSON

{-| Traverse directory recursively and collect all the jobs -}
getJobsRecursively :: FolderUrl -> IO [JobUrl]
getJobsRecursively = go [] <=< getFolderItems
  where
    go :: [JobUrl] -> [FolderItem] -> IO [JobUrl]
    go jobsSoFar [] = return jobsSoFar
    go jobsSoFar (item:items) =
      let itemUrl = fiUrl item
          ignore = do
              Text.putStrLn $ "Ignoring " <> Text.pack (show (fiItemType item)) <> " " <> itemUrl
              go jobsSoFar items
      in case fiItemType item of
        Folder             -> do
            Text.putStrLn $ "Retrieving jobs from Folder " <> itemUrl
            subItems <- getFolderItems $ FolderUrl itemUrl
            go jobsSoFar (items ++ subItems)
        MatrixProject      -> do
            Text.putStrLn $ "Retrieving jobs from MatrixProject " <> itemUrl
            jobsOfMatrix <- getMatrixJobConfigurations . JobUrl $ itemUrl
            go (jobsOfMatrix ++ jobsSoFar) items
        FreestyleProject   -> go (JobUrl itemUrl:jobsSoFar) items
        WorkflowJob        -> ignore
        MavenModuleSet     -> ignore
        MultiJobProject    -> ignore
        BuildFlow          -> ignore
        UnkwnownItemType _ ->
            error $ "Found item with unknown _class: " <> show item
                   <> ". Please add it to the FolderItemType"


getMatrixJobConfigurations :: JobUrl -> IO [JobUrl]
getMatrixJobConfigurations (JobUrl matrixJobUrl) = do
    resp <- Wreq.get . Text.unpack $ matrixJobUrl <> "/api/json"
    return $ resp ^. Wreq.responseBody . to extractActiveConfigurations
  where
    extractActiveConfigurations :: ByteString -> [JobUrl]
    extractActiveConfigurations body =
      fmap JobUrl $ body ^..
          key "activeConfigurations" . _Array . traverse
          . key "url" . _String

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
    return $ resp ^.. Wreq.responseBody . collectFailures
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
