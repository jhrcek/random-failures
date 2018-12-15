{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Config
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Jenkins as J

import Conduit (concatC, concatMapMC, filterMC, iterMC, mapMC, runConduit,
                sinkList, yieldMany, (.|))
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Failure (TestFailure)
import Jenkins (FolderUrl (FolderUrl))
import Merge (mergeReports)
import System.FilePath ((</>))
import Util (lengthText)

main :: IO ()
main = do
    jobUrls <- J.getJobsRecursively $ FolderUrl "https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest"
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
    reportsDir <- Config.getReportsDir
    saveReport reportsDir testFailures
    mergeReports reportsDir

--------------------------------------------------------------------------------
saveReport :: FilePath -> [TestFailure] -> IO ()
saveReport reportsDir failures = do
    utcTime <- getCurrentTime
    let reportFileName = "failures_" <> formatTime defaultTimeLocale "%F" utcTime <> ".json"
        reportPath = reportsDir </> reportFileName
    Text.putStrLn $ "Saving " <> lengthText failures <> " failures to " <> Text.pack reportPath
    Aeson.encodeFile reportPath failures

--------------------------------------------------------------------------------
ignoreBuildsWithMoreThanFailures :: Int -> [a] -> IO Bool
ignoreBuildsWithMoreThanFailures limit items =
    if length items > limit then do
        Text.putStrLn $ " has " <> lengthText items
            <> " failures - these won't be included in the final report, because there's more than "
            <> Text.pack (show limit) <> " of them"
        return False
    else return True
