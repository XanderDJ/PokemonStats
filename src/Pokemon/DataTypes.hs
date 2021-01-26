module Pokemon.DataTypes where

import Data.Char
import qualified Data.Map as M

-- | In memory store of names to dt types, when implemented should lessen the load on pokéapi
type DTCache = M.Map String DTType

-- | Wrapper around all different data types for the /dt command from showdown
data DTType = DtPokemon Pokemon | DtItem Item | DtMove Move | DtNature Nature | DtAbility Ability

instance Show DTType where
  show (DtPokemon x) = show x
  show (DtItem x) = show x
  show (DtMove x) = show x
  show (DtAbility x) = show x
  show (DtNature x) = show x

type Name = String

type Description = String

-- | Ability contains the name of the ability and it's description
data Ability = Ability Name (Maybe Description)

instance Show Ability where
  show (Ability name (Just description)) = name ++ ": " ++ description
  show (Ability name _) = name ++ ": no description yet in the api."

-- | Item contains the name of an item and it's description
data Item = Item Name (Maybe Description)

instance Show Item where
  show (Item name (Just description)) = name ++ ": " ++ description
  show (Item name _) = name ++ ": no description yet in the api."

-- | Data type representing a move, dClass is either physical or special, bp can be battle power, accuracy is only applicable to moves that have accuracy
data Move = Move
  { name :: Name,
    tipe :: Typing,
    dClass :: String,
    bp :: Maybe Int,
    accuracy :: Maybe Int,
    description :: Maybe Description
  }
  deriving (Show)

-- | Nature datatype, contains the name of the nature, the positive stat increase and then the negative. If the stats are neutral then there is no change.
data Nature = Nature String Stat Stat

instance Show Nature where
  show (Nature name NEUTRAL NEUTRAL) = name ++ ": No effects on stats"
  show (Nature name positive negative) = name ++ ": 10 % increase for " ++ show positive ++ " and 10% decrease for " ++ show negative

-- | All different stats for a pokemon
data Stat = HP | ATK | DEF | SPATK | SPDEF | SPEED | NEUTRAL deriving (Eq)

instance Show Stat where
  show HP = "hp"
  show ATK = "attack"
  show DEF = "defense"
  show SPATK = "special attack"
  show SPDEF = "special defense"
  show SPEED = "speed"
  show NEUTRAL = "neutral"

-- | Data type for a pokemon stat, the stat it represents and the value of that stat
data BaseStat = BaseStat Stat Int deriving (Show, Eq)

-- | List of base stats
type BaseStats = [BaseStat]

-- | List of Pokemon types
type Typing = [Type]

-- | Pokemon types
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
  deriving (Show, Eq)

-- | Make read be able to parse strings into types, "normal" -> NORMAL
instance Read Type where
  readsPrec _ input = case map toLower input of
    'n' : 'o' : 'r' : 'm' : 'a' : 'l' : rest -> [(NORMAL, rest)]
    'f' : 'i' : 'g' : 'h' : 't' : 'i' : 'n' : 'g' : rest -> [(FIGHTING, rest)]
    'f' : 'l' : 'y' : 'i' : 'n' : 'g' : rest -> [(FLYING, rest)]
    'p' : 'o' : 'i' : 's' : 'o' : 'n' : rest -> [(POISON, rest)]
    'g' : 'r' : 'o' : 'u' : 'n' : 'd' : rest -> [(GROUND, rest)]
    'r' : 'o' : 'c' : 'k' : rest -> [(ROCK, rest)]
    'b' : 'u' : 'g' : rest -> [(BUG, rest)]
    'g' : 'h' : 'o' : 's' : 't' : rest -> [(GHOST, rest)]
    's' : 't' : 'e' : 'e' : 'l' : rest -> [(STEEL, rest)]
    'f' : 'i' : 'r' : 'e' : rest -> [(FIRE, rest)]
    'w' : 'a' : 't' : 'e' : 'r' : rest -> [(WATER, rest)]
    'g' : 'r' : 'a' : 's' : 's' : rest -> [(GRASS, rest)]
    'e' : 'l' : 'e' : 'c' : 't' : 'r' : 'i' : 'c' : rest -> [(ELECTRIC, rest)]
    'p' : 's' : 'y' : 'c' : 'h' : 'i' : 'c' : rest -> [(PSYCHIC, rest)]
    'i' : 'c' : 'e' : rest -> [(ICE, rest)]
    'd' : 'r' : 'a' : 'g' : 'o' : 'n' : rest -> [(DRAGON, rest)]
    'd' : 'a' : 'r' : 'k' : rest -> [(DARK, rest)]
    'f' : 'a' : 'i' : 'r' : 'y' : rest -> [(FAIRY, rest)]
    _ -> []

-- | Pokemon represented from pokemon api
-- Should contain more maybes to cover for data not being present in pokemon api
data Pokemon = Pokemon
  { pName :: Name,
    pTyping :: Typing,
    abilities :: [Name],
    hiddenAbilities :: Maybe Name,
    baseStats :: BaseStats,
    weight :: Int
  }
  deriving (Show)

-- | Level of a pokemon 0 - 100
type Level = Int
