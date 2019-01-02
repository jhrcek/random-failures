module Config
    ( getReportsDir
    , getFrontendDistDir
    , parse
    , Config(..)
    ) where

import qualified Turtle

import Data.Functor ((<&>))
import Jenkins (FolderUrl (..))
import System.Environment (getEnv)
import Turtle.Options (Parser, optPath, optText, options)

getReportsDir :: IO FilePath
getReportsDir = getEnv "HOME" <&> (<> "/Dropbox/Projects/random-failures/")

getFrontendDistDir :: IO FilePath
getFrontendDistDir = getEnv "HOME" <&> (<> "/Devel/github.com/jhrcek/random-failures/frontend/dist/")


data Config = Config
    { kiegroupDir       :: Turtle.FilePath
    , jenkinsJobsFolder :: FolderUrl
    }

parse :: IO Config
parse =
    options "Jenkins job test failures scraper" configParser

configParser :: Parser Config
configParser = Config
    <$> optPath "kiegroup-dir" 'd' "Directory, where all kiegroup projects have been cloned"
    <*> folderUrlParser

folderUrlParser :: Parser FolderUrl
folderUrlParser =
    FolderUrl <$> optText "jenkins-folder" 'j' "Jenkins folder containing jobs from which failures should be scraped"
