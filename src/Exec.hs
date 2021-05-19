
{-# LANGUAGE ScopedTypeVariables #-}

module Exec (exec) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V ((!), forM_, map)
import Data.Csv
import qualified Data.String.HIUtils as DSU
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BL
import Data.String.Utils (strip, split)
import GHC.Float

datafile = "/Users/jxxcarlson/dev/haskell/scripts/logger2/log.csv"

exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStrLn "??"
     (cmd:args) -> 
       case cmd of
         "help" -> help
         "show" -> showData
         "log" -> logItem
         "read" -> readCSV datafile
         _ -> putStrLn "??"

help = putStrLn "Commands: quit, show, log"


showData = do
  contents <- TI.readFile datafile
  TI.putStrLn contents
 

logItem = do
    input <- getLine
    contents <- TI.readFile datafile
    let newContents = T.append contents (T.pack ("\n" ++ input))
    TI.writeFile datafile newContents
    putStrLn $ "data written to " ++ datafile



readCSV :: FilePath -> IO ()
readCSV filePath = do
    content <- BL.readFile filePath
    case decode NoHeader content of
        Left err -> print err
        Right xs -> V.forM_ xs $ \(date :: String, time :: String, value:: Double, event:: String, food:: String) -> print (date, time, value, event, food)


roundTo n f =  (fromInteger $ round $ f * (10^n)) / (10.0^^n)