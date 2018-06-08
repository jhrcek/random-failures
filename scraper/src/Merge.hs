module Merge (mergeReports) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.List            as List
import           Data.Monoid          ((<>))
import qualified Failure              as F
import qualified System.Directory     as Dir
import           System.FilePath      ((</>))
import qualified System.FilePath      as FP

mergeReports :: FilePath -> IO ()
mergeReports reportsDir = do
    reports <- listReports reportsDir
    putStrLn $ "Found " <> show (length reports) <> " failure report files. Loading failures..."
    eitherFailures <- traverse loadFailures reports
    let failures = either
            (error . ("Something went wrong when loading failures: " <> ))
            concat
            $ sequence eitherFailures
        uniqueFailures = List.nub failures
    putStrLn $ show (length failures) <> " failures loaded. Removing duplicates..."
    finalReport <- Dir.makeAbsolute "../frontend/dist/failures.json"
    BS.writeFile finalReport (Aeson.encode uniqueFailures)
    putStrLn $ show (length uniqueFailures) <> " unique failures saved to " <> finalReport

listReports :: FilePath -> IO [FilePath]
listReports reportsDir = do
  reports <- Dir.listDirectory reportsDir
  return $ (reportsDir </>) <$> filter (List.isPrefixOf "failures_" . FP.takeFileName) reports

loadFailures :: FilePath -> IO (Either String [F.TestFailure])
loadFailures = fmap Aeson.eitherDecode . BS.readFile
