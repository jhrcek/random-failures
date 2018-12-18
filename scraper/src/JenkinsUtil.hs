{-# LANGUAGE OverloadedStrings #-}
module JenkinsUtil where

import Data.Coerce
import Data.Foldable
import qualified Data.Text as T
import Jenkins
import Turtle

-- Sample usage:
-- downloadLogsFromDbMatrix (JobUrl "https://rhba-qe-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/BxMS/job/RHPAM-master-nightly/job/certification/job/business-central/job/reporting/job/blessed-business-central-reporting-db") 35

downloadLogsFromDbMatrix :: JobUrl -> Int -> IO ()
downloadLogsFromDbMatrix jobUrl buildNumber = do
    urls <- getMatrixJobConfigurations jobUrl
    let logUrls = (\matrixConfUrl -> matrixConfUrl <> repr buildNumber <> "/consoleText") <$> (coerce urls :: [Text])
    traverse_  (\url -> procs "wget" [url, "-O", getDB url <> ".log"] empty) logUrls
  where
    getDB :: Text -> Text
    getDB matrixConfUrl = T.takeWhile (/=',') . (!!1) $ T.splitOn "DATABASE=" matrixConfUrl
