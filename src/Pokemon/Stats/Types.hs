{-# LANGUAGE OverloadedStrings #-}

module Pokemon.Stats.Types where

import Data.Default
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Pokemon.Replays.Types
import Pokemon.Types

-- Individual stats for a pokemon in one match
type UtilityFactor = Double

type AccumulateHazardDmg = Double

type SelfDestructs = Int

type HazardDamageTaken = Double

--  Aggregate stats for a pokemon in multiple matches
type Turns = Double

type Kills = Int

type IndirectKills = Int

type TimesBrought = Int

data PokeStats = PokeStats
  { accumulateHazardDmg :: AccumulateHazardDmg,
    selfDestructs :: SelfDestructs,
    hzrdDmgTaken :: HazardDamageTaken,
    kills :: Kills,
    indirectKills :: IndirectKills,
    timesBrought :: TimesBrought,
    turnsActive :: Int,
    turnsUtil :: Int,
    utilityFactor :: UtilityFactor
  }
  deriving (Show)

data PlayerStats = PlayerStats
  { pName :: T.Text,
    totalTurns :: Turns,
    pokeStats :: M.Map T.Text PokeStats
  }
  deriving (Show)

data Match = Match
  { players :: M.Map Position PlayerStats,
    currentMajorAction :: ReplayMessage,
    outcome :: ReplayMessage,
    weather :: M.Map Weather T.Text,
    terrain :: M.Map Terrain T.Text,
    hazards :: M.Map ((Position, Position), Hazards) T.Text,
    nicks :: M.Map T.Text T.Text,
    pokeHealth :: M.Map T.Text Health,
    pokeStatusCause :: M.Map (T.Text, Status) T.Text
  }
  deriving (Show)

instance Default PokeStats where
  def = PokeStats 0 0 0 0 0 1 0 0 0

instance Default PlayerStats where
  def = PlayerStats "" 0 M.empty

instance Default Match where
  def = Match M.empty (RMD DELIMITER) (RMD DELIMITER) M.empty M.empty M.empty M.empty M.empty M.empty

instance Semigroup PokeStats where
  (<>) ps1 ps2 = PokeStats accumulateHazardDmg' selfDestructs' hazardDamageTaken' kills' indirectKills' timesBrought' turnsActive' turnsUtil' utilityFactor'
    where
      accumulateHazardDmg' = average (accumulateHazardDmg ps1) (accumulateHazardDmg ps2)
      selfDestructs' = selfDestructs ps1 + selfDestructs ps2
      hazardDamageTaken' = average (hzrdDmgTaken ps1) (hzrdDmgTaken ps2)
      kills' = kills ps1 + kills ps2
      indirectKills' = indirectKills ps1 + indirectKills ps2
      timesBrought' = timesBrought ps1 + timesBrought ps2
      turnsActive' = turnsActive ps1 + turnsActive ps2
      turnsUtil' = turnsUtil ps1 + turnsUtil ps2
      utilityFactor' = average (utilityFactor ps1) (utilityFactor ps2)

instance Semigroup PlayerStats where
  (<>) ps1 ps2 = combine ps1 ps2

combine :: PlayerStats -> PlayerStats -> PlayerStats
combine (PlayerStats name1 turns1 map1) (PlayerStats name2 turns2 map2) = if name1 == name2 then PlayerStats name1 (average turns1 turns2) (combineMaps map1 map2) else PlayerStats name1 turns1 map1

combineMaps = M.unionWith (<>)

average :: (Num a, Fractional a) => a -> a -> a
average x y = (x + y) / 2