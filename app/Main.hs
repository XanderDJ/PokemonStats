{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Xlsx (atSheet, def, fromXlsx)
import Control.Lens ((&), (?~))
import qualified Data.ByteString.Lazy as L
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromJust, isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Excel (emptySheet, insertTable)
import Pokemon.DataTypes (Pokemon (pName))
import Pokemon.Excel (speedTable)
import Pokemon.Functions (sortOnSpeed)
import Pokemon.PokeApi (getPokemon)
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

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
  pokemons <- getPokemons pokemons'
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
  pokemons <- getPokemons pokemons'
  pokemons2 <- getPokemons pokemons'2
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

getPokemons :: [String] -> IO [Pokemon]
getPokemons [] = return []
getPokemons (pokemon : pokemons) = do
  pokemon' <- getPokemon pokemon
  if isJust pokemon'
    then do
      let pokemon'' = fromJust pokemon'
      pokemons' <- getPokemons pokemons
      return $ pokemon'' : pokemons'
    else error $ "Couldn't find pokemon: " ++ pokemon

getTeamName :: FilePath -> String
getTeamName = head . splitOn "." . last . splitWhen (\x -> x == '\\' || x == '/')
