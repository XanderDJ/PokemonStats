{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Pokemon.Replays.API
import Pokemon.Replays.Parsers
import Pokemon.Replays.Types
import Text.Parsec
import Data.List

main :: IO ()
main = do
    replaystxt <- readFile "replays.txt"
    let replays = map (filter (/= '\r')) (lines replaystxt)
    fails <- testParseReplays replays
    let filestring =  intercalate "\n" . concat $ fails
    writeFile "unabletoparse.txt" filestring

testParseOneReplay :: String -> IO [String]
testParseOneReplay base = do
  putStrLn $ "PARSING: " ++ base
  replay <- getReplay base
  if isNothing replay
    then do 
      print ("Couldn't get file: " ++ base)
      putStrLn ""
      return []
    else do
      let rms = fromJust replay
      let filteredRms = filter (not . isSupported) rms
      if null filteredRms
        then do 
          print $ "Replay: " ++ base ++ " was parsed succesfully"
          putStrLn ""
          return []
        else do
          print "PARSING FAILED."
          let failures = map getUnsupportedText filteredRms
          putStrLn ""
          return (map T.unpack failures)

testParseReplays :: [String] -> IO [[String]]
testParseReplays = mapM testParseOneReplay


isSupported :: ReplayMessage -> Bool
isSupported (RMUnsupported _) = False
isSupported _ = True

getUnsupportedText :: ReplayMessage  -> T.Text 
getUnsupportedText (RMUnsupported (Unsupported unsupportedText)) = unsupportedText
getUnsupportedText _ = error "getUnsupportedText called on non RMUnsupported datatype" 