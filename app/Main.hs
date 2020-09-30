{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    decode,
    (.:),
  )
import Data.Aeson.Internal ()
import Data.Aeson.Parser ()
import Data.Maybe (fromJust)
import Data.Ord ( Down(Down) )
import Data.List ( sortBy )
import Lib (someFunc)
import Network.HTTP.Client
  ( Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment ( getArgs )
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then error "A filepath to a list of pokemon is needed"
    else
      if length args == 1
        then mainOneFile args
        else
          if length args == 2
            then mainTwoFiles args
            else error "only up to 2 files allowed"

mainOneFile :: [String] -> IO ()
mainOneFile ([file]) = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  let pokemons' = words contents
  pokemons <- mapM getPokemon pokemons'
  let pokemonSorted = sortOnSpeed pokemons
  print pokemonSorted
  hClose handle

mainTwoFiles :: [String] -> IO ()
mainTwoFiles args = undefined

getPokemon :: String -> IO Pokemon
getPokemon pokemon = do
  tlsManager <- newManager tlsManagerSettings
  request <- parseRequest $ "https://pokeapi.co/api/v2/pokemon/" ++ pokemon
  bs <- httpLbs request tlsManager
  let json = responseBody bs
      pokemon = (fromJust . decode) json
  return pokemon

data Type
  = NORMAL
  | FIGHTING
  | FLYING
  | POISON
  | GROUND
  | ROCK
  | BUG
  | GHOST
  | STEEL
  | FIRE
  | WATER
  | GRASS
  | ELECTRIC
  | PSYCHIC
  | ICE
  | DRAGON
  | DARK
  | FAIRY
  deriving (Show)

type Typing = [Type]

data Stat = HP Int | ATK Int | DEF Int | SPATK Int | SPDEF Int | SPEED Int | NEUTRAL deriving (Show)

data Nature = Nature String Stat Stat deriving (Show)

type EV = Int

type IV = Int

type Level = Int

data BaseStats = BaseStats
  { baseHpStat :: Stat,
    baseAtkStat :: Stat,
    baseDefStat :: Stat,
    baseSpatkStat :: Stat,
    baseSpdefStat :: Stat,
    baseSpdStat :: Stat
  }
  deriving (Show)

data IVs = IVs
  { hpIV :: IV,
    atkIV :: IV,
    defIV :: IV,
    spatkIV :: IV,
    spdefIV :: IV,
    spdIV :: IV
  }
  deriving (Show)

data EVs = EVs
  { hpEV :: EV,
    atkEV :: EV,
    defEV :: EV,
    spatkEV :: EV,
    spdefEV :: EV,
    spdEV :: EV
  }
  deriving (Show)

data Pokemon = Pokemon
  { name :: String,
    baseStats :: BaseStats,
    typing :: Typing
  }
  deriving (Show)

mkStat :: String -> Int -> Stat
mkStat name value
  | name == "hp" = HP value
  | name == "attack" = ATK value
  | name == "defense" = DEF value
  | name == "special-attack" = SPATK value
  | name == "special-defense" = SPDEF value
  | name == "speed" = SPEED value
  | otherwise = NEUTRAL

getBaseStats :: [Stat] -> BaseStats
getBaseStats (hp : atk : def : spatk : spdef : spd : []) = BaseStats hp atk def spatk spdef spd

getTyping :: [String] -> Typing
getTyping strs = map stringToType strs

stringToType :: String -> Type
stringToType str
  | str == "normal" = NORMAL
  | str == "fighting" = FIGHTING
  | str == "flying" = FLYING
  | str == "poison" = POISON
  | str == "ground" = GROUND
  | str == "rock" = ROCK
  | str == "bug" = BUG
  | str == "ghost" = GHOST
  | str == "steel" = STEEL
  | str == "fire" = FIRE
  | str == "water" = WATER
  | str == "grass" = GRASS
  | str == "electric" = ELECTRIC
  | str == "psychic" = PSYCHIC
  | str == "ice" = ICE
  | str == "dragon" = DRAGON
  | str == "dark" = DARK
  | str == "fairy" = FAIRY
  | otherwise = error $ "Illegal type: " ++ str

instance FromJSON Stat where
  parseJSON (Object jsn) = do
    baseValue <- jsn .: "base_stat"
    statInfo <- jsn .: "stat"
    name <- statInfo .: "name"
    return $ mkStat name baseValue

instance FromJSON Pokemon where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    stats <- jsn .: "stats"
    objs <- jsn .: "types"
    typesObjs <- mapM (\obj -> obj .: "type") objs
    types <- mapM (\obj -> obj .: "name") typesObjs
    let baseStats = getBaseStats stats
        typing = getTyping types
    return $ Pokemon name baseStats typing

minStatAt :: Level -> Stat -> Int
minStatAt lvl (HP value) = ((2 * value + (div 0 4)) * (div lvl 100)) + 10 + lvl
minStatAt lvl stat = div (((2 * (getValue stat) + (div 0 4)) * (div lvl 100) + 5) * 9) 10

maxStatAt :: Level -> Stat -> Int
maxStatAt lvl (HP value) = ((31 + 2 * value + (div 252 4)) * (div lvl 100)) + 10 + lvl
maxStatAt lvl stat = div (((31 + 2 * (getValue stat) + (div 252 4)) * (div lvl 100) + 5) * 11) 10

getValue :: Stat -> Int
getValue (HP x) = x
getValue (ATK x) = x
getValue (DEF x) = x
getValue (SPATK x) = x
getValue (SPDEF x) = x
getValue (SPEED x) = x
getValue _ = error "Tried to get the value of an empty Stat"

getStat :: String -> Pokemon -> Stat
getStat name pok
  | name == "hp" = baseHpStat basestats
  | name == "atk" = baseAtkStat basestats
  | name == "def" = baseDefStat basestats
  | name == "spatk" = baseSpatkStat basestats
  | name == "spdef" = baseSpdefStat basestats
  | name == "spd" = baseSpdStat basestats
  | otherwise = error $ name ++ " isn't an abbreviation of a stat"
  where
    basestats = baseStats pok

maxSpeed :: Pokemon -> Int
maxSpeed = (maxStatAt 100 . getStat "spd")

maxSpeedWithScarf :: Pokemon -> Int
maxSpeedWithScarf = (*// 1.5) . fromIntegral . maxSpeed

serious :: Nature
serious = Nature "Serious" NEUTRAL NEUTRAL

sortOnSpeed :: [Pokemon] -> [Pokemon]
sortOnSpeed = sortBy (sortPokemon "spd")

-- | Ordering is reversed to make it descending instead of ascending. Shown by the use of Down
sortPokemon :: String -> Pokemon -> Pokemon ->  Ordering
sortPokemon stat pok1 pok2 = compare (Down stat1) (Down stat2)
  where
    baseStat1 = getStat stat pok1
    stat1 = getValue baseStat1
    baseStat2 = getStat stat pok2
    stat2 = getValue baseStat2

-- | floored multiplication
(*//) :: (RealFrac a,Num a, Integral b) => a -> a -> b
(*//) a b = floor c
  where
    c = a * b
    
infixl 7 *//