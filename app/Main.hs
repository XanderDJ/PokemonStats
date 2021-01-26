{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Xlsx
  ( CellValue (CellDouble, CellText),
    atSheet,
    def,
    fromXlsx,
  )
import Control.Lens ((&), (?~))
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    decode,
    (.:),
  )
import Data.Aeson.Internal ()
import Data.Aeson.Parser ()
import qualified Data.ByteString.Lazy as L
import Data.List (sortBy)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Excel
  ( ExcelTable (ExcelTable),
    TableMode (HORIZONTAL),
    emptySheet,
    insertTable,
  )
import Network.HTTP.Client
  ( Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

import Pokemon.DataTypes
import Pokemon.Functions
import Pokemon.PokeApi

main :: IO ()
main = pokemonStats

pokemonStats :: IO ()
pokemonStats = do
  args <- getArgs
  if null args
    then error "A filepath to a list of pokemon is needed"
    else
      if length args == 1
        then mainOneFile args
        else
          if length args == 2
            then mainTwoFiles args
            else error "only up to 2 files allowed"

mainOneFile :: [String] -> IO ()
mainOneFile [file] = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  let pokemons' = words contents
  pokemons <- mapM getPokemon pokemons'
  let pokemonSorted = sortOnSpeed pokemons
      table' = speedTable pokemonSorted
      sheet = insertTable "A1" table' emptySheet
      xl = def & atSheet "Speeds" ?~ sheet
  ct <- getPOSIXTime
  L.writeFile "speed.xlsx" $ fromXlsx ct xl
  hClose handle

mainTwoFiles :: [String] -> IO ()
mainTwoFiles [file1, file2] = do
  handle1 <- openFile file1 ReadMode
  handle2 <- openFile file2 ReadMode
  contents <- hGetContents handle1
  contents2 <- hGetContents handle2
  let pokemons' = words contents
      pokemons'2 = words contents2
  pokemons <- mapM getPokemon pokemons'
  pokemons2 <- mapM getPokemon pokemons'2
  let pokemonSorted = sortOnSpeed pokemons
      pokemonSorted2 = sortOnSpeed pokemons2
      table1 = speedTable pokemonSorted
      table2 = speedTable pokemonSorted2
      sheet = (insertTable "I1" table2 . insertTable "A1" table1) emptySheet
      xl = def & atSheet "Speeds" ?~ sheet
  ct <- getPOSIXTime
  let team1 = getTeamName file1
      team2 = getTeamName file2
      fileName = team1 ++ "vs" ++ team2 ++ ".xlsx"
  L.writeFile fileName $ fromXlsx ct xl
  hClose handle1
  hClose handle2

speedTable :: [Pokemon] -> ExcelTable
speedTable poks = table
  where
    headers' = [CellText "Name", CellText "Base speed", CellText "Min speed", CellText "No invest speed", CellText "Max speed", CellText "Max speed with scarf"]
    contents' = map pokemonSpeedRow poks
    table = ExcelTable headers' contents' HORIZONTAL

pokemonSpeedRow :: Pokemon -> [CellValue]
pokemonSpeedRow pok = row
  where
    speed = getBaseStat "speed" pok
    pokName = T.pack $ pName pok
    row =
      [ CellText pokName,
        (CellDouble . fromIntegral . getValue) speed,
        (CellDouble . fromIntegral . minStatAt 100) speed,
        (CellDouble . fromIntegral . noInvestStatAt 100) speed,
        (CellDouble . fromIntegral . maxSpeed) pok,
        (CellDouble . fromIntegral . maxSpeedWithScarf) pok
      ]

getTeamName :: FilePath -> String
getTeamName = head . splitOn "." . last . splitWhen (\x -> x == '\\' || x == '/')
