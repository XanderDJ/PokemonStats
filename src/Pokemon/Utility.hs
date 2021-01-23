module Pokemon.Utility where

import Pokemon.DataTypes
import Pokemon.Nature
import Data.Maybe
import Text.Read

getType :: String -> Maybe Type 
getType = readMaybe

getTyping :: String -> Maybe Typing 
getTyping = readMaybe

getTypes :: [String] -> Maybe Typing 
getTypes = mapM getType