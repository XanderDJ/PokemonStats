{-# LANGUAGE OverloadedStrings #-}

module Pokemon.Excel where

import Pokemon.DataTypes
import Pokemon.Functions
import Excel
import Codec.Xlsx
import qualified Data.Text as T

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

pokemonMoveMap :: Pokemon -> TableMode -> ExcelMap 
pokemonMoveMap mon mode = undefined 