{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Xlsx
import Control.Lens ((&), (?~))
import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import Data.Default
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Excel
import Pokemon.Excel (pokemonMoveMap, speedTable)
import Pokemon.Functions (sortOnSpeed)
import Pokemon.PokeApi (getPokemon, getPokemonNoMoves)
import Pokemon.Replays.API
import Pokemon.Replays.Parsers (parseReplayMessage)
import Pokemon.Stats.Stats
import Pokemon.Types (Pokemon (pName))
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )
import Text.Parsec (parse)
import Text.Pretty.Simple

main :: IO ()
main = do
  (Just replay) <- getReplay "https://replay.pokemonshowdown.com/gen8unratedrandombattle-1296331007"
  let (a, s) = runState (getStats replay) def
  pPrint s

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
      sheet = insertTable (1, 1) table' emptySheet
      xl = emptyXlsx & atSheet "Speeds" ?~ sheet
      moveMaps = map (\pokemon -> (pName pokemon, pokemonMoveMap HORIZONTAL pokemon)) pokemonSorted
      xl' = insertMoveMaps xl moveMaps
  ct <- getPOSIXTime
  L.writeFile (getTeamName file ++ ".xlsx") $ fromXlsx ct xl'
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
      sheet = (insertTable (1, 8) table2 . insertTable (1, 1) table1) emptySheet
      xl = emptyXlsx & atSheet "Speeds" ?~ sheet
      moveMaps = map (\pokemon -> (pName pokemon, pokemonMoveMap HORIZONTAL pokemon)) pokemonSorted2
      xl' = insertMoveMaps xl moveMaps
  ct <- getPOSIXTime
  let team1 = getTeamName file1
      team2 = getTeamName file2
      fileName = team1 ++ "vs" ++ team2 ++ ".xlsx"
  L.writeFile fileName $ fromXlsx ct xl'
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

getPokemonsNoMoves :: [String] -> IO [Pokemon]
getPokemonsNoMoves [] = return []
getPokemonsNoMoves (pokemon : pokemons) = do
  pokemon' <- getPokemonNoMoves pokemon
  if isJust pokemon'
    then do
      let pokemon'' = fromJust pokemon'
      pokemons' <- getPokemons pokemons
      return $ pokemon'' : pokemons'
    else error $ "Couldn't find pokemon: " ++ pokemon

getTeamName :: FilePath -> String
getTeamName = head . splitOn "." . last . splitWhen (\x -> x == '\\' || x == '/')

insertMoveMaps :: Xlsx -> [(String, Maybe ExcelMap)] -> Xlsx
insertMoveMaps xl [] = xl
insertMoveMaps xl ((sheetName, Nothing) : items) = insertMoveMaps xl items
insertMoveMaps xl ((sheetName, Just em) : items) =
  let newSheet = insertMap (1, 1) em emptySheet
      newXl = xl & atSheet (T.pack sheetName) ?~ newSheet
   in insertMoveMaps newXl items