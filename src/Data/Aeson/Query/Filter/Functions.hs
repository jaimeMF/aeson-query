module Data.Aeson.Query.Filter.Functions where

import Data.Text (Text)
import Control.Monad (foldM)
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import qualified Data.Vector as V

import Data.Aeson.Query.AesonUtils

type FilterFunction = (Value -> Result Value)

attributeFilter :: Text -> FilterFunction
attributeFilter attr v = toObject v >>= flip getAttribute attr

indexFilter n value = do
    vect <- toArray value
    return $ toJSON $ vect V.!? n

objectFilter :: [(Text, FilterFunction)] -> FilterFunction
objectFilter pairs o = Success $ object [k .=  runFilter f o | (k, f) <- pairs]

runFilter :: FilterFunction -> Value -> Value
runFilter f o = case f o of
    Error _ -> Null
    Success a -> a

concatFilters :: [FilterFunction] -> FilterFunction
concatFilters [] o = return o
concatFilters (f:fs) o = f o >>= concatFilters fs
