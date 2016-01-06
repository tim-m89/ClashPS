module TimPS (main, playerTim) where

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Clash
import qualified ClashPrim as ClashPrim
import ClashPrimConversion (toPrimPlayer)
import ClashUtil
import Control.Alt
import Data.Array(head, filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude

-- Like Clash.Ai, except the Move is wrapped in a Maybe
type AiMaybe = PlayerState -> EnemiesState -> GameEnvironment -> Maybe Move

-- Our player info
myInfo :: PlayerInfo
myInfo = { style: 1
         , name: "TimPS" }

-- Our player
playerTim_ :: Player
playerTim_ = { ai:   myAi
             , info: myInfo }

-- Our player converted to the native interface without curried functions and constructors
playerTim :: ClashPrim.Player
playerTim = toPrimPlayer playerTim_

-- Main ai function
myAi :: Ai
myAi self enemies game = if hasAmmo self && pointingAtPlayers self enemies
                          then Shoot
                          else myNoShootMove self enemies game

-- Move to be cacrried out when theres no point in shooting on this round
myNoShootMove :: Ai
myNoShootMove self enemies game = fromMaybe NoOp $  turnToInlineEnemiesCurrent self enemies game
                                                <|> turnToInlineEnemiesImminent self enemies game
                                                <|> seekAmmo self enemies game

turnToInlineEnemies :: (PlayerState -> Position) -> AiMaybe
turnToInlineEnemies fPlayerPos self enemies game = if not (hasAmmo self)
                                                    then Nothing
                                                    else let myPos      = playerPos self
                                                             enemiesPos = map fPlayerPos enemies
                                                             inline     = filter (\pos-> posInline myPos pos) enemiesPos
                                                              in head inline >>= \pos-> return $ turnRouteForPos self pos

-- Turn to enemies that are currently inline with self
turnToInlineEnemiesCurrent :: AiMaybe
turnToInlineEnemiesCurrent = turnToInlineEnemies playerPos

-- Turn to where an enemy is about to be inline if they are to go straight ahead on their next move
turnToInlineEnemiesImminent :: AiMaybe
turnToInlineEnemiesImminent = turnToInlineEnemies playerNextPos

-- Move forward if it will get the ship closer to the closest ammo, otherwise turn if reguired for a move to the closest ammo, otherwise noop
seekAmmo :: AiMaybe
seekAmmo self enemies game = closestNonEqualPos (playerPos self) (ammoPos game) >>= \pos-> return $ moveToPos pos self enemies game

-- Attemp to go forward if it will get us closer to supplied position, otherwise turn for it
moveToPos :: Position -> Ai
moveToPos pos self enemies game = if playerEnRouteForPos self pos
                                    then Move
                                    else turnRouteForPos self pos

-- If supplied position is north of us, turn north, etc for each direction
turnRouteForPos :: PlayerState -> Position -> Move
turnRouteForPos self pos = let myPos = playerPos self in
                            if      positionIsNorth pos myPos then Turn North
                            else if positionIsEast  pos myPos then Turn East
                            else if positionIsSouth pos myPos then Turn South
                            else if positionIsWest  pos myPos then Turn West
                            else NoOp

-- A main entry point to keep the compiler happy
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Loading TimPS"


