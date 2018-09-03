module Config (getReportsDir) where
import           System.Environment (getEnv)

getReportsDir :: IO FilePath
getReportsDir = (++ "/Dropbox/Projects/random-failures/") <$> getEnv "HOME"
