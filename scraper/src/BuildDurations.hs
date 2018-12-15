module BuildDurations (printBuildTimesOfFinishedDownstreamPrJobs) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vect
import qualified Jenkins as J
import qualified Statistics.Sample as Stat
import qualified Util

import Conduit (filterC, mapM_C, runConduit, yieldMany, (.|))
import Data.List (genericLength)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.LocalTime (timeToTimeOfDay)
import Jenkins (BuildResult (SUCCESS, UNSTABLE), BuildStats,
                FolderUrl (FolderUrl), JobUrl)

{-| Scrape durations of downstream PR job builds with result SUCCESS/UNSTABLE builds -}
printBuildTimesOfFinishedDownstreamPrJobs :: IO ()
printBuildTimesOfFinishedDownstreamPrJobs = do
    jobUrls <- J.getJobsRecursively $ FolderUrl "https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest"
    runConduit
        $ yieldMany jobUrls
        .| filterC (Text.isSuffixOf "downstream-pullrequests" . J.getJobName)
        .| mapM_C printJobStats

printJobStats :: JobUrl -> IO ()
printJobStats jobUrl = do
    Text.putStrLn $ J.getJobName jobUrl
    builds <- J.getAllBuilds jobUrl
    stats <- traverse J.getBuildStats builds
    let finishedBuildDurations = J.buildDurationMilis <$> filter isFinished stats
        averageDurationMilis = sum finishedBuildDurations `div` genericLength finishedBuildDurations
    Text.putStrLn $ if null finishedBuildDurations
        then "    - No finished (green or yellow) builds"
        else Text.intercalate ", "
            [ "    - avg = " <> formatDuration averageDurationMilis
            , "σ = " <> (formatDuration . round . Stat.stdDev . Vect.fromList $ fmap fromIntegral finishedBuildDurations)
            , "based on " <> Util.lengthText finishedBuildDurations <> " finished build(s)"
            ]
  where
    formatDuration :: Integer -> Text
    formatDuration milis = Text.pack . show . timeToTimeOfDay . secondsToDiffTime $ milis `div` 1000

    isFinished :: BuildStats -> Bool
    isFinished stat = J.buildResult stat `elem` [SUCCESS, UNSTABLE]
