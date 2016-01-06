module TimPS (main, playerTim) where

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Clash
import qualified ClashPrim as ClashPrim
import ClashPrimConversion (toPrimPlayer)
import ClashUtil
import Data.Array(head, filter)
import Data.Maybe (Maybe(..))
import Prelude

myInfo :: PlayerInfo
myInfo = { style: 1
         , name: "TimPS" }

playerTim_ :: Player
playerTim_ = { ai:   myAi
             , info: myInfo }

playerTim :: ClashPrim.Player
playerTim = toPrimPlayer playerTim_


myAi :: Ai
myAi self enemies game = if hasAmmo self && pointingAtPlayers self enemies
                          then Shoot
                          else myNoShootMove self enemies game

myNoShootMove :: Ai
myNoShootMove self enemies game = case turnToInlineEnemies self enemies game of
                                    (Just m)  -> m
                                    (Nothing) -> case turnToImminentInlineEnemies self enemies game of
                                      (Just m)  -> m
                                      (Nothing) -> seekAmmo self enemies game

turnToInlineEnemies :: PlayerState -> EnemiesState -> GameEnvironment -> Maybe Move
turnToInlineEnemies self enemies game = if not (hasAmmo self)
                                          then Nothing
                                          else let myPos      = playerPos self
                                                   enemiesPos = map playerPos enemies
                                                   inline     = filter (\pos-> posInline myPos pos) enemiesPos
                                                    in head inline >>= \pos-> return $ turnRouteForPos self pos

turnToImminentInlineEnemies :: PlayerState -> EnemiesState -> GameEnvironment -> Maybe Move
turnToImminentInlineEnemies self enemies game = if not (hasAmmo self)
                                                  then Nothing
                                                  else let myPos          = playerPos self
                                                           enemiesNextPos = map playerNextPos enemies
                                                           imminentInline = filter (\pos-> posInline myPos pos) enemiesNextPos
                                                            in head imminentInline >>= \pos-> return $ turnRouteForPos self pos

seekAmmo :: Ai
seekAmmo self enemies game = case closestPos (playerPos self) (ammoPos game) of
                              (Just pos) -> moveToPos pos self enemies game
                              (Nothing)  -> NoOp

moveToPos :: Position -> Ai
moveToPos pos self enemies game = if playerEnRouteForPos self pos
                                    then Move
                                    else turnRouteForPos self pos

turnRouteForPos :: PlayerState -> Position -> Move
turnRouteForPos self pos = let myPos = playerPos self in
                            if      positionIsNorth pos myPos then Turn North
                            else if positionIsEast  pos myPos then Turn East
                            else if positionIsSouth pos myPos then Turn South
                            else if positionIsWest  pos myPos then Turn West
                            else NoOp

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Loading TimPS"


