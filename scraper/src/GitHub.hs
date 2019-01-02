{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

 module GitHub(FQN(FQN), GitInfo(GitInfo), loadFqnToGitInfoMap) where

import qualified Control.Foldl as Fold
import qualified Data.Map as Map
import qualified Data.Text as Text

import Control.Arrow ((&&&))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (FilePath)
import Turtle (FilePath, empty, fold, format, inshell, l, pushd, with)

-- Relative path of test class java file in kiegroup folder
newtype TestPath = TestPath Text deriving Show
-- Fully Qualified Name of the test class
newtype FQN = FQN Text
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- Info required to construct path of the source code of the class on Git Hub
-- The url can be constructed like so: "https://github.com/kiegroup/" <> repo <> "/blob/master/" <> pathInRepo
data GitInfo = GitInfo {repo :: Text, pathInRepo :: Text} deriving (Eq, Show, Generic, ToJSON, FromJSON)

loadFqnToGitInfoMap :: FilePath -> IO (Map FQN GitInfo)
loadFqnToGitInfoMap kieGroupDir =
    Map.fromList . fmap (toFqn &&& toGitInfo) <$> findTestPaths kieGroupDir

findTestPaths :: FilePath -> IO [TestPath]
findTestPaths kieGroupDir =
    with (pushd kieGroupDir) $ \_ ->
      fmap (TestPath . format l) <$>
          fold (inshell "find . -wholename '*src/test/java/*Test.java' -or -wholename '*src/test/java/*IT.java'" empty) Fold.list

toFqn :: TestPath -> FQN
toFqn (TestPath tp) =
  case Text.stripSuffix ".java" afterSrcTestJava of
    Nothing             -> error $ "The class name didn't end with java: " <> show tp
    Just fqnWithSlashes -> FQN $ Text.replace "/" "." fqnWithSlashes
  where
    (_, afterSrcTestJava) = Text.breakOnEnd "src/test/java/" tp

toGitInfo :: TestPath -> GitInfo
toGitInfo (TestPath tp) = GitInfo{..}
  where
    (repo, slashPathInRepo) = Text.breakOn "/" $ Text.drop 2 {- drop "./" -} tp
    pathInRepo = Text.tail slashPathInRepo
