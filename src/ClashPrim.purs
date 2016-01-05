{- Primitive native clashjs interop -}

module ClashPrim where

import Data.Function (Fn3())

-- Clash JS uses an array of 2 numbers in places
type TwoNumbers = Array Int

-- Position used to locate both players and ammo
type Position = TwoNumbers

-- Direction is one of "north", "east", "south", "west"
type Direction = String

-- A count of ammo held by a player
type Ammo = Int

-- Indication of a players living status
type IsAlive = Boolean

-- Overal state of a player
type PlayerState = { position :: Position, direction :: Direction, ammo :: Ammo, isAlive :: IsAlive }

-- State of the enemies is jsut an array of player states
type EnemiesState = Array PlayerState

-- Grid size is indicated using a single number (despite clashjs documentation stating it is a 2 element array)
type GridSize = Int

-- Ammo locations are stored together in an array
type AmmoPosition = Array Position

-- The game enivornment is a combination of the grid size and ammo locations
type GameEnvironment = { gridSize :: GridSize, ammoPosition :: AmmoPosition }

-- A players move is denoted as a string. This will either be a direction to turn, "move", "shoot", or null to indicate do nothing
type Move = String

-- The main ai hook is a function from own player state, enemies state, game environment, and returns the chosen move
type Ai_=       PlayerState -> EnemiesState -> GameEnvironment -> Move
-- Marshalled so that it is in normal js function format rather than curried for partial application
type Ai =  Fn3  PlayerState    EnemiesState    GameEnvironment    Move

-- Players each have 1 of a few different appearances chosen using an int value
type PlayerStyle = Int

-- Players must each have a unique name that appears when watching the battle
type PlayerName = String

-- Player info object is just the style and name
type PlayerInfo = { style :: PlayerStyle, name :: PlayerName }

-- A player is an object containing its info object and ai function
type Player = { ai :: Ai, info :: PlayerInfo }

