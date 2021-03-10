module Pokemon.Stats.Stats where

import Control.Monad.State
import Data.Default
import qualified Data.Map as M
import Pokemon.Replays.Types
import Pokemon.Stats.Types

type ReplayMatch = [ReplayMessage]

getStats :: ReplayMatch -> State Match [PlayerStats]
getStats [] = returnPlayers
getStats (RMW win : rms) = returnPlayers
getStats (RMTie tie : rms) = returnPlayers
getStats (RMD delimiter : rms) = handleDelimiter (RMD delimiter) >> getStats rms
getStats (RMPoke poke : rms) = handlePoke poke >> getStats rms
getStats (RMPL player : rms) = handlePlayer player >> getStats rms
getStats (RMTurn turn : rms) = handleTurn turn >> getStats rms
getStats (RMSwitch switch : rms) = handleSwitch switch >> getStats rms
getStats (RMDrag (Drag nick name health tags) : rms) = handleSwitch (Switch nick name health tags) >> getStats rms
getStats (rm : rms) = getStats rms

-- | Handle switch action, put it as major action, initialize pokemon if not already initialized. Put nick into map. Store current hp
handleSwitch :: Switch -> State Match [PlayerStats]
handleSwitch (Switch (pos, nick) name (current, total, status) tags) = do
  match <- get
  -- First initialize mon in map if not already present
  let ps = players match M.! pos
      ps' = ps {pokeStats = M.insertWith keepOld name def (pokeStats ps)}
      -- put nick into map
      ns = nicks match
      ns' = M.insertWith keepOld nick name ns
      -- store current hp
      hps = pokeHealth match
      hps' = M.insert nick (current, total, status) hps
      match' =
        match
          { players = M.insert pos ps' (players match),
            nicks = ns',
            pokeHealth = hps',
            currentMajorAction = RMSwitch (Switch (pos, nick) name (current, total, status) tags)
          }
  put match'
  return []

-- | Put major action to delimiter, making sure that when minor actions happen at the end of the turn this can be tracked
handleDelimiter :: ReplayMessage -> State Match [PlayerStats]
handleDelimiter delimiter = do
  match <- get
  let match' = match {currentMajorAction = delimiter}
  put match'
  returnPlayers

-- | Correctly initialize players in players map with their positions given
handlePlayer :: Player -> State Match [PlayerStats]
handlePlayer (Pl pos (Just name) _ _) = do
  match <- get
  let match' = match {players = M.insertWith (\a b -> b) pos def {pName = name} (players match)}
  put match'
  returnPlayers
handlePlayer (Pl pos _ _ _) = returnPlayers

-- | Update turns for all players
handleTurn :: Turn -> State Match [PlayerStats]
handleTurn (Turn turn) = do
  match <- get
  let updateTurns turns ps = ps {totalTurns = turns}
      match' = match {players = M.map (updateTurns (fromIntegral turn)) (players match)}
  put match'
  returnPlayers

-- | Initialize pokemon for the correct player, for randbats the mons need to be initialized from the switch command
handlePoke :: Poke -> State Match [PlayerStats]
handlePoke (Poke pos name mItem) = do
  match <- get
  let ps = players match M.! pos
      ps' = ps {pokeStats = M.insertWith keepOld name def (pokeStats ps)}
      match' = match {players = M.insert pos ps' (players match)}
  put match'
  returnPlayers

returnPlayers :: State Match [PlayerStats]
returnPlayers = gets matchToResult

-- HELPER FUNCTIONS

keepOld :: a -> b -> b
keepOld a b = b

matchToResult :: Match -> [PlayerStats]
matchToResult m = pls
  where
    pMap = players m
    pls = M.elems pMap