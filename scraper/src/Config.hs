module Config (getReportsDir, getFrontendDistDir, getKieGroupDir) where
import Data.Functor ((<&>))
import System.Environment (getEnv)

getReportsDir :: IO FilePath
getReportsDir = getEnv "HOME" <&> (<> "/Dropbox/Projects/random-failures/")

getFrontendDistDir :: IO FilePath
getFrontendDistDir = getEnv "HOME" <&> (<> "/Devel/github.com/jhrcek/random-failures/frontend/dist/")

getKieGroupDir :: IO FilePath
getKieGroupDir = getEnv "HOME" <&> (<> "/Devel/github.com/kiegroup/")
