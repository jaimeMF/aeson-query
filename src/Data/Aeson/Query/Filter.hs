module Data.Aeson.Query.Filter (
    Filter(..),
    applyFilter
) where


import Data.String (fromString)
import Data.Aeson (Value, object, (.=))

import Data.Aeson.Query.Filter.Functions

data Filter = Chain [Filter] | Object [(String, Filter)] | Attribute String | ArrayElement Int | Id
    deriving Show

applyFilter :: Filter -> Value -> Value
applyFilter (Chain fs) v = case fs of
    [] -> v
    (f:fs') -> applyFilter (Chain fs') $ applyFilter f v
applyFilter (Attribute s) v = runFilter (attributeFilter (fromString s)) v
applyFilter (Object pairs) v = object [(fromString k) .=  applyFilter f v | (k, f) <- pairs]
applyFilter (ArrayElement n) v = runFilter (indexFilter n) v
applyFilter Id v = v
