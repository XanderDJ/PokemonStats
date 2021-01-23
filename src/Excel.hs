module Excel (ExcelTable (..), TableContent, TableMode (..), insertTable, emptyXlsx, emptySheet, nextPoint) where

import Codec.Xlsx
import Control.Lens
import qualified Data.Text as T

data ExcelTable = ExcelTable
  { headers :: [CellValue],
    contents :: [TableContent],
    mode :: TableMode
  }

data TableMode = HORIZONTAL | VERTICAL

type TableContent = [CellValue]

insertTable :: T.Text -> ExcelTable -> Worksheet -> Worksheet
insertTable startingCorner table ws = finalWs
  where
    pos = fromSingleCellRefNoting (CellRef startingCorner)
    insertMethod = insertContent (mode table)
    next = nextPoint (reverseMode $ mode table)
    ws' = insertMethod pos (headers table) ws
    nextPos = next pos
    finalWs = foldrIndexed ws' nextPos next insertMethod (contents table)

-- | a = start value type.
-- b = index type.
-- c = list of other values.
-- (b -> b) = next index function.
-- (b -> c -> a -> a) = function that takes the index, a list of other values and a start value to produce another value.
-- This function will fold over a list of cs to produce an a that takes an index like a table of contents to insert cs into.
foldrIndexed :: a -> b -> (b -> b) -> (b -> c -> a -> a) -> [c] -> a
foldrIndexed startValue startIndex nextIndexF f (value : values) = let nextValue = f startIndex value startValue in foldrIndexed nextValue (nextIndexF startIndex) nextIndexF f values
foldrIndexed startValue _ _ _ [] = startValue

nextPoint :: TableMode -> (Int, Int) -> (Int, Int)
nextPoint HORIZONTAL = _2 %~ (+ 1)
nextPoint VERTICAL = _1 %~ (+ 1)

reverseMode :: TableMode -> TableMode
reverseMode HORIZONTAL = VERTICAL
reverseMode VERTICAL = HORIZONTAL

insertContent :: TableMode -> (Int, Int) -> [CellValue] -> Worksheet -> Worksheet
insertContent HORIZONTAL = insertRow
insertContent VERTICAL = insertCol

insertRow :: (Int, Int) -> [CellValue] -> Worksheet -> Worksheet
insertRow (row, col) (cv : cvs) ws = ws & cellValueAt (row, col) ?~ cv & insertRow (row, col + 1) cvs
insertRow _ [] ws = ws

insertCol :: (Int, Int) -> [CellValue] -> Worksheet -> Worksheet
insertCol (row, col) (cv : cvs) ws = ws & cellValueAt (row, col) ?~ cv & insertCol (row + 1, col) cvs
insertCol _ [] ws = ws

emptyXlsx :: Xlsx
emptyXlsx = def

emptySheet :: Worksheet
emptySheet = def

len :: ExcelTable -> Int
len table =
  let rows = length (contents table)
   in rows + 1