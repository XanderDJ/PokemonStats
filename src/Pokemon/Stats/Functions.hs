module Pokemon.Stats.Functions where

import Pokemon.Replays.Types

getOppositePositions :: Position -> (Position, Position)
getOppositePositions P1 = (P2, P4)
getOppositePositions P2 = (P1, P3)
getOppositePositions P3 = (P2, P4)
getOppositePositions P4 = (P1, P3)