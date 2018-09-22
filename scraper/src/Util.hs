module Util (lengthText) where
import Data.Text (Text, pack)

lengthText :: [a] -> Text
lengthText = pack . show . length
