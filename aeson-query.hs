module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import Data.Aeson (decode, Value)
import Data.Aeson.Encode.Pretty (encodePretty)

import Data.Aeson.Query

printJson j = BL.putStrLn $ encodePretty j

main = do
    arg <- fmap head getArgs
    case parseFilter arg of 
        Left e -> error $ show e
        Right filter -> do
            Just ast <- fmap decode $ BL.getContents
            printJson $ applyFilter filter ast
        
