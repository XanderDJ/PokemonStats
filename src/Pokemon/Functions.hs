module Pokemon.Functions where

import Data.List (sortBy)
import Data.Maybe
import Data.Ord (Down (Down))
import Pokemon.DataTypes
import Pokemon.Nature
import Text.Read (readMaybe)

getType :: String -> Maybe Type
getType = readMaybe

getTyping :: String -> Maybe Typing
getTyping = readMaybe

getTypes :: [String] -> Maybe Typing
getTypes = mapM getType

-- | given a string and a value make a basestat
mkStat :: String -> Int -> BaseStat
mkStat name value = let stat = getStat name in BaseStat stat value

-- | Changes string representing stat into Stat type
getStat :: String -> Stat
getStat "hp" = HP
getStat "attack" = ATK
getStat "defense" = DEF
getStat "special-attack" = SPATK
getStat "special-defense" = SPDEF
getStat "speed" = SPEED
getStat s = error $ s ++ " is not a stat."

-- | Get the base stat of a pokemon using a string as a name
getBaseStat :: String -> Pokemon -> BaseStat
getBaseStat name pok = findBaseStat basestats stat
  where
    basestats = baseStats pok
    stat = getStat name

findBaseStat :: BaseStats -> Stat -> BaseStat
findBaseStat [] s = error $ "Couldn't find stat \"" ++ show s ++ "\" in base stats"
findBaseStat ((BaseStat s' val) : bs) s = if s' == s then BaseStat s val else findBaseStat bs s

minStatAt :: Level -> BaseStat -> Int
minStatAt lvl (BaseStat HP value) = ((2 * value) * div lvl 100) + 10 + lvl
minStatAt lvl stat = fromIntegral ((2 * getValue stat) * div lvl 100 + 5) *// 0.9

noInvestStatAt :: Level -> BaseStat -> Int
noInvestStatAt lvl (BaseStat HP value) = ((31 + 2 * value) * div lvl 100) + 10 + lvl
noInvestStatAt lvl stat = (31 + 2 * getValue stat) * div lvl 100 + 5

maxStatAt :: Level -> BaseStat -> Int
maxStatAt lvl (BaseStat HP value) = ((31 + 2 * value + div 252 4) * div lvl 100) + 10 + lvl
maxStatAt lvl stat = fromIntegral ((31 + 2 * getValue stat + div 252 4) * div lvl 100 + 5) *// 1.1

getValue :: BaseStat -> Int
getValue (BaseStat _ val) = val

-- | floored multiplication
(*//) :: (RealFrac a, Num a, Integral b) => a -> a -> b
a *// b = floor (a * b)

infixl 7 *//

sortOnSpeed :: [Pokemon] -> [Pokemon]
sortOnSpeed = sortBy (sortPokemon "speed")

-- | Ordering is reversed to make it descending instead of ascending. Shown by the use of Down
sortPokemon :: String -> Pokemon -> Pokemon -> Ordering
sortPokemon stat pok1 pok2 = compare (Down stat1) (Down stat2)
  where
    baseStat1 = getBaseStat stat pok1
    stat1 = getValue baseStat1
    baseStat2 = getBaseStat stat pok2
    stat2 = getValue baseStat2

getSpeed :: Pokemon -> Int
getSpeed = getValue . getBaseStat "speed"

maxSpeed :: Pokemon -> Int
maxSpeed = maxStatAt 100 . getBaseStat "speed"

maxSpeedWithScarf :: Pokemon -> Int
maxSpeedWithScarf = (*// 1.5) . fromIntegral . maxSpeed

getMoveType :: Move -> MoveType
getMoveType (Move name tipe dClass bp' accuracy _)
 | name `elem` hazards = HAZARD 
 | name `elem` utility = UTILITY 
 | name `elem` recovery = RECOVERY
 | name `elem` statusMoves = STATUS 
 | name `elem` boostMoves = BOOST 
 | isJust bp' = ATTACK
 | otherwise = OTHER

hazards :: [[Char]]
hazards = ["spikes", "toxic-spikes", "stealth-rocks", "sticky-web"]

utility :: [[Char]]
utility =
  [ "trick-room",
    "rain-dance",
    "sunny-day",
    "hail",
    "sandstorm",
    "reflect",
    "light-screen",
    "aurora-veil",
    "psychic-terrain",
    "misty-terrain",
    "grassy-terrain",
    "electric-terrain",
    "teleport",
    "knock-off",
    "defog",
    "rapid-spin",
    "trick",
    "switcheroo",
    "magic-coat",
    "protect",
    "substitute",
    "aromatherapy",
    "court-change",
    "encore",
    "taunt",
    "haze",
    "heal-bell",
    "memento",
    "parting-shot",
    "baton-pass",
    "torment"
  ]

recovery :: [[Char]]
recovery =
  [
    "moonlight",
    "morning-sun",
    "roost",
    "milk-drink",
    "soft-boiled",
    "wish",
    "shore-up",
    "synthesis",
    "rest",
    "leech-life",
    "absorb",
    "mega-drain",
    "giga-drain",
    "draining-kiss",
    "drain-punch",
    "pain-split",
    "leech-seed",
    "jungle-healing",
    "floral-healing",
    "lunar-dance",
    "life-dew",
    "aqua-ring",
    "healing-wish",
    "heal-pulse",
    "pollen-puff",
    "ingrain",
    "present",
    "slack-off",
    "strength-sap",
    "oblivion-wing"
  ]

statusMoves :: [[Char]]
statusMoves =
  [
    "toxic",
    "will-o-wisp",
    "thunder-wave",
    "hypnosis",
    "glare",
    "sleep-powder",
    "stun-spore",
    "dark-void",
    "yawn",
    "confuse-ray",
    "swagger",
    "flatter",
    "poison-powder",
    "poison-gas",
    "sing",
    "sweet-kiss",
    "teeter-dance"
  ]

boostMoves :: [[Char]]
boostMoves = 
  [
    "acid-armor",
    "acupressure",
    "agility",
    "amnesia",
    "autotomize",
    "belly-drum",
    "bulk-up",
    "calm-mind",
    "charge",
    "clangorous-soul",
    "coil",
    "cosmic-power",
    "cotton-guard",
    "defend-order",
    "defense-curl",
    "double-team",
    "dragon-dance",
    "focus-energy",
    "geomancy",
    "growth",
    "harden",
    "hone-claws",
    "howl",
    "iron-defense",
    "nasty-plot",
    "no-retreat",
    "quiver-dance",
    "rock-polish",
    "sharpen",
    "rototiller",
    "shell-smash",
    "shift-gear",
    "swords-dance",
    "withdraw",
    "work-up"
  ]