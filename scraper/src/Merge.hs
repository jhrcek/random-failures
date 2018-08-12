module Merge (mergeReports) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.List            as List
import           Data.Monoid          ((<>))
import qualified Failure              as F
import qualified System.Directory     as Dir
import           System.FilePath      ((</>))
import qualified System.FilePath      as FP
import Data.Time.Clock (getCurrentTime, nominalDay, addUTCTime)

mergeReports :: FilePath -> IO ()
mergeReports reportsDir = do
    reports <- listReports reportsDir
    putStrLn $ "Found " <> show (length reports) <> " failure report files. Loading failures..."
    eitherFailures <- traverse loadFailures reports
    isLessThanHalfYearOld <- createDateFilter
    let failures = either
            (error . ("Something went wrong when loading failures: " <> ))
            concat
            $ sequence eitherFailures
        uniqueFailuresFromLastSixMonths = filter isLessThanHalfYearOld $ List.nub failures
    putStrLn $ show (length failures) <> " failures loaded. Removing duplicates..."
    finalReport <- Dir.makeAbsolute "../frontend/dist/failures.json"
    BS.writeFile finalReport (Aeson.encode uniqueFailuresFromLastSixMonths)
    putStrLn $ show (length uniqueFailuresFromLastSixMonths) <> " unique failures saved to " <> finalReport

{- Create filter that accepts failures from past 6 months -}
createDateFilter :: IO (F.TestFailure -> Bool)
createDateFilter = do
    now <- getCurrentTime
    let pastDate = addUTCTime (-(365/2) * nominalDay) now
    return $ \failure -> F.date failure >= pastDate

listReports :: FilePath -> IO [FilePath]
listReports reportsDir = do
  reports <- Dir.listDirectory reportsDir
  return $ (reportsDir </>) <$> filter (List.isPrefixOf "failures_" . FP.takeFileName) reports

loadFailures :: FilePath -> IO (Either String [F.TestFailure])
loadFailures = fmap Aeson.eitherDecode . BS.readFile
