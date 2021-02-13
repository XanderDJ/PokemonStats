{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Pokemon.Replays.API
import Pokemon.Replays.Parsers
import Text.Parsec
import Data.List

main :: IO ()
main = do
    replaystxt <- readFile "replays.txt"
    let replays = map (filter (/= '\r')) (lines replaystxt)
    fails <- testParseReplays replays
    let filestring =  intercalate "\n" . map (intercalate "\n") $ fails
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
      let msgs = T.splitOn "\n" . fromJust $ replay
      let rms = map (\x -> (parse parseReplayMessage "test" x, x)) msgs
      let filteredRms = filter (\(result, str) -> isLeft result) rms
      if null filteredRms
        then do 
          print $ "Replay: " ++ base ++ " was parsed succesfully"
          putStrLn ""
          return []
        else do
          print "PARSING FAILED."
          let failures = map snd filteredRms
          putStrLn ""
          return (map T.unpack failures)

testParseReplays :: [String] -> IO [[String]]
testParseReplays = mapM testParseOneReplay