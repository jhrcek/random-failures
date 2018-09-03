module Merge (mergeReports) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Failure
import qualified System.Directory as Dir
import qualified System.FilePath as FP

import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import Failure (TestFailure, url)
import System.FilePath ((</>))

mergeReports :: FilePath -> IO ()
mergeReports reportsDir = do
    reports <- listReports reportsDir
    putStrLn $ "Found " <> show (length reports) <> " failure report files. Loading failures..."
    eitherFailures <- traverse loadFailures reports
    isLessThanHalfYearOld <- createDateFilter
    validUrlSet <- getValidBuildUrls reportsDir
    let failures = case sequence eitherFailures of
            Left err -> error $ "Something went wrong when loading failures: " <> err
            Right fs -> fmap (removeInvalidUrl validUrlSet)
                        . filter isLessThanHalfYearOld
                        . List.nub
                        $ concat fs
    finalReport <- Dir.makeAbsolute "../frontend/dist/failures.json"
    BS.writeFile finalReport (Aeson.encode failures)
    putStrLn $ show (length failures) <> " unique failures saved to " <> finalReport


{- Create filter that accepts failures from past 6 months -}
createDateFilter :: IO (TestFailure -> Bool)
createDateFilter = do
    now <- getCurrentTime
    let pastDate = addUTCTime (-(365/2) * nominalDay) now
    return $ \failure -> Failure.date failure >= pastDate

listReports :: FilePath -> IO [FilePath]
listReports reportsDir = do
  reports <- Dir.listDirectory reportsDir
  return $ (reportsDir </>) <$> filter (List.isPrefixOf "failures_" . FP.takeFileName) reports

loadFailures :: FilePath -> IO (Either String [TestFailure])
loadFailures = fmap Aeson.eitherDecode . BS.readFile

{- We want to avoid showing build URLs which are no longer valid (= corresponding build has been removed from Jenkins).
   At the time of scraping, only URLs that end up in the latest report are valid.
-}
getValidBuildUrls :: FilePath -> IO (Set Text)
getValidBuildUrls reportsDir =
    fmap (Set.fromList . either (const []) (fmap Failure.url))
    . loadFailures . List.maximum {- latest report = max in lexicographical order -}
     =<< listReports reportsDir

removeInvalidUrl :: Set Text -> TestFailure -> TestFailure
removeInvalidUrl validUrls failure
    | url failure `Set.member` validUrls = failure
    | otherwise                          = failure {url = ""}
