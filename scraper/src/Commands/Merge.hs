{-# LANGUAGE NamedFieldPuns #-}
module Commands.Merge (mergeReports) where

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GitHub
import qualified Turtle

import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import Failure (TestFailure (..), url)
import GitHub (FQN (FQN), GitInfo)

mergeReports :: FilePath -> FilePath -> FilePath -> Turtle.FilePath -> IO ()
mergeReports archivedReport newReport outputReport kieGroupDir = do
    putStrLn "Loading failures..."
    eitherFailures <- sequence <$> traverse loadFailures [archivedReport, newReport]
    isLessThanHalfYearOld <- createDateFilter
    validUrlSet <- getValidBuildUrls newReport
    fqnToGitInfo <- GitHub.loadFqnToGitInfoMap kieGroupDir
    let failures = case eitherFailures of
            Left err -> error $ "Something went wrong when loading failures: " <> err
            Right fss -> List.nub
                        . fmap (addGitInfo fqnToGitInfo . removeInvalidUrl validUrlSet)
                        . filter isLessThanHalfYearOld
                        $ concat fss
    Aeson.encodeFile outputReport failures
    putStrLn $ show (length failures) <> " unique failures saved to " <> outputReport

{- Create predicate that accepts failures from past 6 months -}
createDateFilter :: IO (TestFailure -> Bool)
createDateFilter = do
    now <- getCurrentTime
    let halfYearAgo = addUTCTime (-(365/2) * nominalDay) now
    return $ \TestFailure{date} -> date >= halfYearAgo

loadFailures :: FilePath -> IO (Either String [TestFailure])
loadFailures = Aeson.eitherDecodeFileStrict

{- We want to avoid showing build URLs which are no longer valid
   (= corresponding build has been removed from Jenkins).
   At the time of scraping, only URLs that end up in the latest report are valid.
-}
getValidBuildUrls :: FilePath -> IO (Set Text)
getValidBuildUrls newReport =
    Set.fromList . either (const []) (fmap Failure.url) <$> loadFailures newReport

removeInvalidUrl :: Set Text -> TestFailure -> TestFailure
removeInvalidUrl validUrls failure
    | url failure `Set.member` validUrls = failure
    | otherwise                          = failure {url = ""}

addGitInfo :: Map FQN GitInfo -> TestFailure -> TestFailure
addGitInfo fqnToGitInfo failure =
    failure {Failure.gitInfo = gitInfo}
  where
    gitInfo = Map.lookup (FQN clz) fqnToGitInfo
    clz = Failure.testClass failure
