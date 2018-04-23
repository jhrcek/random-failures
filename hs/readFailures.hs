{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Main where
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import GHC.Generics

main :: IO ()
main = readFailures >>= print

readFailures :: IO (Either String [Failure])
readFailures = eitherDecode <$> LB.readFile "../frontend/dist/failures.json"

data Failure = Failure
    { url        :: Text
    , date       :: [Int]
    , testClass  :: Text
    , testMethod :: Text
    , stackTrace :: Text
    } deriving (Show, Generic, FromJSON)
