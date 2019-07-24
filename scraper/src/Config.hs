module Config
    ( parseCommand
    , getReportsDir
    , Command(..)
    ) where

import qualified Turtle

import Data.Functor ((<&>))
import Jenkins (FolderUrl (..))
import Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc,
                            help, helper, info, long, metavar, progDesc,
                            strOption, subparser, value)
import System.Environment (getEnv)

getReportsDir :: IO FilePath
getReportsDir = getEnv "HOME" <&> (<> "/Dropbox/Projects/random-failures/")

data Command
    = Scrape
        { jenkinsJobsFolder :: FolderUrl
        , outputReport      :: FilePath }
    | Merge
        { kiegroupDir    :: Turtle.FilePath
        , archivedReport :: FilePath -- ^ The old report containing past test failures
        , newReport      :: FilePath -- ^ The fresh report containing data for the past week or so
        , outputReport   :: FilePath -- ^ The output report containing everything, deduplicated and enriched with github link info + updated build URLs
        }
    deriving Show

commandParser :: Parser Command
commandParser = subparser
    ( command "scrape" (info (helper <*> scrapeParser)
        (progDesc "Scrape failures from all jobs in given Jenkins directory and save them to a file"))
   <> command "merge" (info (helper <*> mergeParser)
        (progDesc "Merge failures into single file"))
    )

scrapeParser :: Parser Command
scrapeParser = Scrape
    <$> (FolderUrl <$> strOption
            ( long "jenkins-directory"
           <> metavar "URL"
           <> value "https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest"
           <> help "Jenkins directory containing jobs from which failures should be scraped"
            )
        )
    <*> strOption
            ( long "output"
           <> metavar "FILE"
           <> help "Output file where the failures will be saved"
            )

mergeParser :: Parser Command
mergeParser = Merge
    <$> strOption
        ( long "kiegroup-dir"
       <> metavar "DIRECTORY"
       <> help "Directory, where all kiegroup projects have been cloned"
        )
    <*> strOption
        ( long "archived-report"
       <> metavar "FILE"
       <> help "JSON File containing test failures from the past"
        )
    <*> strOption
        ( long "new-report"
       <> metavar "FILE"
       <> help "JSON File containing freshly scraped test failures"
        )
    <*> strOption
        ( long "output"
       <> metavar "FILE"
       <> help "Output file where the failures will be saved"
        )

parseCommand :: IO Command
parseCommand = execParser cliParser

cliParser :: ParserInfo Command
cliParser = info (helper <*> commandParser) fullDesc
