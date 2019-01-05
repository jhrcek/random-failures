module Commands.Scrape (scrapeFailures) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Jenkins as J

import Conduit (concatC, concatMapMC, filterMC, iterMC, mapMC, runConduit,
                sinkList, yieldMany, (.|))
import Data.Monoid ((<>))
import Failure (TestFailure)
import Util (lengthText)

scrapeFailures :: J.FolderUrl -> FilePath -> IO ()
scrapeFailures jenkinsJobsFolder outputReport = do
    jobUrls <- J.getJobsRecursively jenkinsJobsFolder
    Text.putStrLn $ lengthText jobUrls <> " Jenkins jobs to analyze"
    testFailures <- runConduit
        $ yieldMany jobUrls
        .| iterMC (\jobUrl -> Text.putStrLn $ "Analysing job " <> J.getJobName jobUrl)
        .| concatMapMC J.getUnstableBuilds
        .| iterMC (\(buildUrl, _dateTime) ->
                      Text.putStr $ "    Unstable build #" <> J.getBuildNumber buildUrl)
        .| mapMC J.getTestFailures
        .| filterMC (ignoreBuildsWithMoreThanFailures 50)
        .| iterMC (\failures -> Text.putStrLn $ " has " <> lengthText failures <> " test failure(s)")
        .| concatC
        .| sinkList
    saveReport outputReport testFailures

saveReport :: FilePath -> [TestFailure] -> IO ()
saveReport reportFile failures = do
    Text.putStrLn $ "Saving " <> lengthText failures <> " failures to " <> Text.pack reportFile
    Aeson.encodeFile reportFile failures

ignoreBuildsWithMoreThanFailures :: Int -> [a] -> IO Bool
ignoreBuildsWithMoreThanFailures limit items =
    if length items > limit then do
        Text.putStrLn $ " has " <> lengthText items
            <> " failures - these won't be included in the final report, because there's more than "
            <> Text.pack (show limit) <> " of them"
        return False
    else return True
