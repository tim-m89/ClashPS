module ClashPrimConversion (toPrimPlayer) where

import qualified Clash as Clash
import qualified ClashPrim as ClasPrim

import Data.Array.Unsafe (unsafeIndex)
import Data.Function (mkFn3)
import Prelude (map, ($))

fromPrimPosition :: ClashPrim.Position -> Clash.Position
fromPrimPosition primPos = Clash.Position { row: (unsafeIndex primPos 0)
                                          , col: (unsafeIndex primPos 1) }

fromPrimDirection :: ClashPrim.Direction -> Clash.Direction
fromPrimDirection "north" = Clash.North
fromPrimDirection "east"  = Clash.East
fromPrimDirection "south" = Clash.South
fromPrimDirection "west"  = Clash.West

toPrimDirection :: Clash.Direction -> ClashPrim.Direction
toPrimDirection Clash.North = "north"
toPrimDirection Clash.East  = "east"
toPrimDirection Clash.South = "south"
toPrimDirection Clash.West  = "west"

fromPrimPlayerState :: ClashPrim.PlayerState -> Clash.PlayerState
fromPrimPlayerState primPs = Clash.PlayerState { position:  fromPrimPosition primPs.position
                                               , direction: fromPrimDirection primPs.direction
                                               , ammo:      primPs.ammo
                                               , isAlive:   primPs.isAlive                     }

fromPrimEnemiesState :: ClashPrim.EnemiesState -> Clash.EnemiesState
fromPrimEnemiesState primEs = map fromPrimPlayerState primEs

fromPrimAmmoPosition :: ClashPrim.AmmoPosition -> Clash.AmmoPosition
fromPrimAmmoPosition primAp = map fromPrimPosition primAp

fromPrimGameEnvironment :: ClashPrim.GameEnvironment -> Clash.GameEnvironment
fromPrimGameEnvironment primGe = Clash.GameEnvironment { gridSize:     (primGe.gridSize )
                                                       , ammoPosition: (fromPrimAmmoPosition primGe.ammoPosition) }

toPrimMove :: Clash.Move -> ClashPrim.Move
toPrimMove (Clash.Turn dir) = toPrimDirection dir
toPrimMove  Clash.Move      = "move"
toPrimMove  Clash.Shoot     = "shoot"
toPrimMove  Clash.NoOp      = ""

toPrimAi_ :: Clash.Ai -> ClashPrim.Ai_
toPrimAi_ ai = \ps-> \es-> \ge-> toPrimMove $ ai (fromPrimPlayerState ps) (fromPrimEnemiesState es) (fromPrimGameEnvironment ge)

toPrimAi :: Clash.Ai -> ClashPrim.Ai
toPrimAi ai = mkFn3 $ toPrimAi_ ai

toPrimPlayer :: Clash.Player -> ClashPrim.Player
toPrimPlayer player = { ai:   toPrimAi player.ai
                      , info: player.info        }
