module ClashUtil where

import Clash
import Data.Array (null, filter, head, sortBy, dropWhile)
import Data.Maybe(Maybe())
import Data.Tuple
import Prelude

positionIsValid :: Position -> GameEnvironment -> Boolean
positionIsValid (Position pos) (GameEnvironment game) =
            pos.row >= 0
         && pos.row <  game.gridSize
         && pos.col >= 0
         && pos.col <  game.gridSize

positionIsNorth :: Position -> Position -> Boolean
positionIsNorth (Position target) (Position viewpoint) = target.row < viewpoint.row

positionIsEast :: Position -> Position -> Boolean
positionIsEast (Position target) (Position viewpoint) = target.col > viewpoint.col

positionIsSouth :: Position -> Position -> Boolean
positionIsSouth (Position target) (Position viewpoint) = target.row > viewpoint.row

positionIsWest :: Position -> Position -> Boolean
positionIsWest (Position target) (Position viewpoint) = target.col < viewpoint.col

posSameCol :: Position -> Position -> Boolean
posSameCol (Position x) (Position y) = x.col == y.col

posSameRow :: Position -> Position -> Boolean
posSameRow (Position x) (Position y) = x.row == y.row

posInline :: Position -> Position -> Boolean
posInline x y = posSameRow x y || posSameCol x y

northOfPos :: Position -> Position
northOfPos (Position pos) = Position { row: (pos.row - 1)
                                     , col:  pos.col      }

eastOfPos :: Position -> Position
eastOfPos (Position pos) = Position { row:  pos.row
                                    , col: (pos.col + 1) }

southOfPos :: Position -> Position
southOfPos (Position pos) = Position { row: (pos.row + 1)
                                     , col:  pos.col      }

westOfPos :: Position -> Position
westOfPos (Position pos) = Position { row:  pos.row
                                    , col: (pos.col - 1) }

nextPos :: Direction -> Position -> Position
nextPos North = northOfPos
nextPos East  = eastOfPos
nextPos South = southOfPos
nextPos West  = westOfPos

absInt :: Int -> Int
absInt x = if x < 0 then -x else x

distBetweenPos :: Position -> Position -> Int
distBetweenPos (Position x) (Position y) = (absInt $ x.row - y.row) + (absInt $ x.col - y.col)

posWithDistFromTarget :: Position -> Position -> Tuple Position Int
posWithDistFromTarget source target = Tuple source $ distBetweenPos source target

posWithDistCompare :: Tuple Position Int -> Tuple Position Int -> Ordering
posWithDistCompare x y = compare (snd x) (snd y)

closestPos :: Position -> Array Position -> Maybe Position
closestPos target xs = let posWithDists = (map (\source-> posWithDistFromTarget source target) xs)
                        in head $ sortBy posWithDistCompare posWithDists >>= \posWithDist-> return $ fst posWithDist

closestNonEqualPos :: Position -> Array Position -> Maybe Position
closestNonEqualPos target xs = closestPos target $ dropWhile (\x-> x == target) xs

hasAmmo :: PlayerState -> Boolean
hasAmmo (PlayerState player) = player.ammo > 0

playerDir :: PlayerState -> Direction
playerDir (PlayerState player) = player.direction

playerPos :: PlayerState -> Position
playerPos (PlayerState player) = player.position

pointingAtPos :: Direction -> Position -> Position -> Boolean
pointingAtPos predDir predPosition preyPosition =  ( predDir == North && positionIsNorth preyPosition predPosition && posSameCol preyPosition predPosition )
                                                || ( predDir == East  && positionIsEast  preyPosition predPosition && posSameRow preyPosition predPosition )
                                                || ( predDir == South && positionIsSouth preyPosition predPosition && posSameCol preyPosition predPosition )
                                                || ( predDir == West  && positionIsWest  preyPosition predPosition && posSameRow preyPosition predPosition )

enRouteForPos :: Direction -> Position -> Position -> Boolean
enRouteForPos predDir predPosition preyPosition =  ( predDir == North && positionIsNorth preyPosition predPosition )
                                                || ( predDir == East  && positionIsEast  preyPosition predPosition )
                                                || ( predDir == South && positionIsSouth preyPosition predPosition )
                                                || ( predDir == West  && positionIsWest  preyPosition predPosition )

playerEnRouteForPos :: PlayerState -> Position -> Boolean
playerEnRouteForPos player pos = enRouteForPos (playerDir player) (playerPos player) pos

pointingAtPlayer :: PlayerState -> PlayerState -> Boolean
pointingAtPlayer predator prey = pointingAtPos (playerDir predator) (playerPos predator) (playerPos prey)

pointingAtPlayers :: PlayerState -> Array PlayerState -> Boolean
pointingAtPlayers predator targets = not $ null $ filter (\prey-> pointingAtPlayer predator prey) targets

ammoPos :: GameEnvironment -> AmmoPosition
ammoPos (GameEnvironment game) = game.ammoPosition
