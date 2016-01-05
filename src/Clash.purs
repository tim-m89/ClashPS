module Clash where

import Prelude (Eq, (==), (&&))

-- Position used to locate both players and ammo
newtype Position = Position { row::Int, col::Int }

instance eqPosition :: Eq Position where
  eq (Position x) (Position y) = x.row == y.row && x.col == y.col

-- Direction is one of "north", "east", "south", "west"
data Direction = North | East | South | West

instance eqDirection :: Eq Direction where
  eq North North = true
  eq East  East  = true
  eq South South = true
  eq West  West  = true
  eq _     _     = false

-- A count of ammo held by a player
type Ammo = Int

-- Indication of a players living status
type IsAlive = Boolean

-- Overal state of a player
newtype PlayerState = PlayerState { position :: Position, direction :: Direction, ammo :: Ammo, isAlive :: IsAlive }

-- State of the enemies is jsut an array of player states
type EnemiesState = Array PlayerState

-- Grid size is indicated using a single number (despite clashjs documentation stating it is a 2 element array)
type GridSize = Int

-- Ammo locations are stored together in an array
type AmmoPosition = Array Position

-- The game enivornment is a combination of the grid size and ammo locations
newtype GameEnvironment = GameEnvironment { gridSize :: GridSize, ammoPosition :: AmmoPosition }

-- A valid move is either turning direction, moving forward, shooting, or doing nothing
data Move = Turn Direction | Shoot | Move | NoOp

-- The main ai hook is a function from own player state, enemies state, game environment, and returns the chosen move
type Ai = PlayerState -> EnemiesState -> GameEnvironment -> Move

-- Players each have 1 of a few different appearances chosen using an int value
type PlayerStyle = Int

-- Players must each have a unique name that appears when watching the battle
type PlayerName = String

-- Player info object is just the style and name
type PlayerInfo = { style :: PlayerStyle, name :: PlayerName }

-- A player is an object containing its info object and ai function
type Player = { ai :: Ai, info :: PlayerInfo }

