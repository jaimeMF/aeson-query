module Data.Aeson.Query.AesonUtils where 

import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types

getAttribute o k = parse (.: k) o :: Result Value

toObject (Object o) = Success o
toObject _ = Error "o"

toArray (Array a) = Success a
toArray _ = Error "o"
