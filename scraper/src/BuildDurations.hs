module BuildDurations where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vect
import qualified Jenkins as J
import qualified Statistics.Sample as Stat
import qualified Util

import Conduit (filterC, iterMC, mapM_C, runConduit, yieldMany, (.|))
import Data.List (genericLength)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.LocalTime (timeToTimeOfDay)

{-| Scrape durations of downstream PR job builds with result SUCCESS/UNSTABLE builds -}
printBuildTimesOfFinishedDownstreamPrJobs :: IO ()
printBuildTimesOfFinishedDownstreamPrJobs = do
    jobUrls <- J.getMasterPrJobUrls
    runConduit
        $ yieldMany jobUrls
        .| filterC (Text.isSuffixOf "downstream-pullrequests" . J.getJobName)
        .| iterMC (\jobUrl -> Text.putStr $  J.getJobName jobUrl <> " | ")
        .| mapM_C (\jobUrl -> do
              builds <- J.getAllBuilds jobUrl
              stats <- traverse J.getBuildStats builds
              let finishedBuildDurations = J.buildDurationMilis <$> filter isFinished stats
                  averageDurationMilis = sum finishedBuildDurations `div` genericLength finishedBuildDurations
              if not (null finishedBuildDurations)
                  then Text.putStrLn $ Text.intercalate ","
                          [ "avg = " <> formatDuration averageDurationMilis
                          , "σ = " <> (formatDuration . round . Stat.stdDev . Vect.fromList $ fmap fromIntegral finishedBuildDurations)
                          , "n = " <> Util.lengthText finishedBuildDurations
                          ]
                  else Text.putStrLn "N/A"
            )
      where
        isFinished :: J.BuildStats -> Bool
        isFinished stat = J.buildResult stat `elem` [J.SUCCESS, J.UNSTABLE]

        formatDuration :: Integer -> Text
        formatDuration milis = Text.pack . show . timeToTimeOfDay . secondsToDiffTime $ div milis 1000
