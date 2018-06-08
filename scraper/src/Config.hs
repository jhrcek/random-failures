module Config (getReportsDir) where
import           System.Environment (getEnv)

getReportsDir :: IO FilePath
getReportsDir = (++ "/Dropbox/Projects/randomFailuresAnalysis/") <$> getEnv "HOME"
