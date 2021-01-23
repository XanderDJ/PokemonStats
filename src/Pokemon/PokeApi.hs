{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pokemon.PokeApi where

import Data.Aeson
    ( decode, (.:), (.:?), FromJSON(parseJSON), Value(Object) )
import Data.ByteString.Lazy (ByteString)
import Data.List ( intercalate )
import Data.Maybe ( fromJust, isJust )
import qualified Data.Text as T
import Network.HTTP.Client
    ( ManagerSettings,
      httpLbs,
      newManager,
      parseRequest_,
      Response(responseBody) )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Pokemon.DataTypes
    ( Pokemon(Pokemon),
      BaseStat(..),
      Move(Move),
      Item(..),
      Ability(..),
      DTType(..),
      getStat )
import Pokemon.Nature ( getNature )

-- | Manager settings for tls connections
settings :: ManagerSettings
settings = tlsManagerSettings

-- | Implement the /dt command from showdown with pokéapi. Should use DTCache in the future to store looked up items in a cache
getDt :: String -> IO DTType
getDt name = do
  let name' = (intercalate "-" . words) name
      nature = getNature name'
  if isJust nature
    then return $ DtNature (fromJust nature)
    else do
      let allUrls = tryAllBases name
          allRequests = map4 parseRequest_ allUrls
      manager <- newManager settings
      responses <- mapM4 ( `httpLbs` manager) allRequests
      let bodies = map4 responseBody responses
          dts = fromBS4 bodies
          dt = getDts dts
      return dt


-- | Get a pokemon from pokéapi, TODO: Handle errors
getPokemon :: String -> IO Pokemon
getPokemon name = do
  let base = pokemonBase ++ name
  manager <- newManager settings
  response <- httpLbs (parseRequest_ base) manager
  return $ fromJust $ decode (responseBody response)

-- | Get an ability from pokéapi, TODO: Handle errors
getAbility :: String -> IO Ability
getAbility name = do
  let base = abilityBase ++ name
  manager <- newManager settings
  response <- httpLbs (parseRequest_ base) manager
  return $ fromJust $ decode (responseBody response)

-- | Get an item from pokéapi, TODO: Handle errors
getItem :: String -> IO Item
getItem name = do
  let base = itemBase ++ name
  manager <- newManager settings
  response <- httpLbs (parseRequest_ base) manager
  return $ fromJust $ decode (responseBody response)

-- | Get a move from pokéapi. TODO: Handle errors
getMove :: String -> IO Move
getMove name = do
  let base = moveBase ++ name
  manager <- newManager settings
  response <- httpLbs (parseRequest_ base) manager
  return $ fromJust $ decode (responseBody response)

-- | Api base for pokéapi, make sure this remains up to date
apiBase :: String
apiBase = "https://pokeapi.co/api/v2/"

pokemonBase :: String
pokemonBase = apiBase ++ "pokemon/"

abilityBase :: String
abilityBase = apiBase ++ "ability/"

moveBase :: String
moveBase = apiBase ++ "move/"

itemBase :: String
itemBase = apiBase ++ "item/"

allBases :: (String, String, String, String)
allBases = (pokemonBase, abilityBase, moveBase, itemBase)

-- | When running a dt command we don't know what the user has given us, so we try all bases
tryAllBases :: String -> (String, String, String, String)
tryAllBases name =
  let name' = (intercalate "-" . words) name
   in map4 (++ name') allBases
-- | Map over a 4 tuple
map4 :: (t -> d) -> (t, t, t, t) -> (d, d, d, d)
map4 f (a, b, c, d) = (f a, f b, f c, f d)

-- | Monadic map over 4 tuple, will sequentially call the function on each element.
mapM4 :: (a -> IO b) -> (a, a, a, a) -> IO (b, b, b, b)
mapM4 f (x1, x2, x3, x4) = do
  y1 <- f x1
  y2 <- f x2
  y3 <- f x3
  y4 <- f x4
  return (y1, y2, y3, y4)

fromBS4 :: (ByteString, ByteString, ByteString, ByteString) -> (Maybe Pokemon, Maybe Ability, Maybe Move, Maybe Item)
fromBS4 (x1, x2, x3, x4) = (decode x1, decode x2, decode x3, decode x4)

getDts :: (Maybe Pokemon, Maybe Ability, Maybe Move, Maybe Item) -> DTType
getDts (Just dt, Nothing, Nothing, Nothing) = DtPokemon dt
getDts (Nothing, Just dt, Nothing, Nothing) = DtAbility dt
getDts (Nothing, Nothing, Just dt, Nothing) = DtMove dt
getDts (Nothing, Nothing, Nothing, Just dt) = DtItem dt
getDts _ = error "Not possible to change into dt"

data AbilityJson = AbilityJson
  { _name :: String,
    isHidden :: Bool
  }

instance FromJSON AbilityJson where
  parseJSON (Object jsn) = do
    hidden <- jsn .: "is_hidden"
    ability <- jsn .: "ability"
    name <- ability .: "name"
    return $ AbilityJson name hidden

instance FromJSON BaseStat where
  parseJSON (Object jsn) = do
    value <- jsn .: "base_stat"
    stat <- jsn .: "stat"
    name <- stat .: "name"
    return $ BaseStat (getStat name) value

instance FromJSON Pokemon where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    objs <- jsn .: "types"
    typesObjs <- mapM (.: "type") objs
    types <- mapM (.: "name") typesObjs
    weight <- jsn .: "weight"
    allAbilities <- jsn .: "abilities"
    baseStats <- jsn .: "stats"
    let typing = map read types
        abilities = (map _name . filter (not . isHidden)) allAbilities
        hiddenAbility = (map _name . filter isHidden) allAbilities
        ha = if length hiddenAbility == 1 then Just $ head hiddenAbility else Nothing
    return $ Pokemon name typing abilities ha baseStats (div weight 10)

data EffectEntry = EffectEntry
  { language :: String,
    eDescription :: String
  }

data Effect = Effect (Maybe Int) String

instance FromJSON EffectEntry where
  parseJSON (Object jsn) = EffectEntry <$> ((jsn .: "language") >>= (.: "name")) <*> (jsn .: "effect")

instance FromJSON Ability where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    effects <- jsn .: "effect_entries"
    let effect = (head . filter (\(EffectEntry lang _) -> lang == "en")) effects
    return $ Ability name (eDescription effect)

instance FromJSON Item where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    effects <- jsn .: "effect_entries"
    let effect = (head . filter (\(EffectEntry lang _) -> lang == "en")) effects
    return $ Item name (eDescription effect)

instance FromJSON Move where
  parseJSON (Object jsn) = do
    name <- jsn .: "name"
    acc <- jsn .:? "accuracy"
    obj <- jsn .: "damage_class"
    dClass <- obj .: "name"
    power <- jsn .:? "power"
    effects <- jsn .: "effect_entries"
    chance <- jsn .:? "effect_chance"
    typesObj <- jsn .: "type"
    tipe <- typesObj .: "name"
    let effect = (head . filter (\(EffectEntry lang _) -> lang == "en")) effects
        description = eDescription effect
        typing = [read tipe]
        effect' = Effect chance description
    return $ Move name typing dClass power acc (getCompleteDescription effect')

getCompleteDescription :: Effect -> String
getCompleteDescription (Effect Nothing desc) = desc
getCompleteDescription (Effect (Just x) desc) = newDesc
  where
    valueText = T.pack $ show x
    tDesc = T.pack desc
    newDesc = T.unpack $ T.replace "$effect_chance" valueText tDesc