{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Board.BasicTurnGame
  ( Player (..),
    MoveType (..),
    Piece (..),
    Tile (..),
    GameChange (..),
    GameState (..),
    PlayableGame (..),
    player1Id,
    player2Id,
  )
where

import Data.Ix (Ix)
import Data.Maybe (isJust, listToMaybe)

player1Id :: Int
player1Id = 0

player2Id :: Int
player2Id = 1

data Player = Player1 | Player2 deriving (Eq)

data Piece = Normal | Queen deriving (Eq)

data MoveType = Move | Capture deriving (Eq)

data Tile = Tile deriving (Eq)

-- Mudancas de Estado do jogo
-- Obs: O booleano diz se a mudanca de estado estah na lista de pecas normais ou damas
data GameChange index player piece moveType
  = AddPiece (index, index) player piece Bool
  | RemovePiece (index, index) Bool
  | RemovePieceOtherPlayer (index, index) Bool
  | MovePiece (index, index) (index, index) Bool
  | TurnPieceQueen (index, index)
  | BlockBoard (index, index)
  | ReleaseBoard
  | SwitchPlayer player
  | FinishMove (index, index) player moveType

data GameState index tile player piece = GameState
  { curPlayer' :: player,
    boardPos :: [(index, index, tile)],
    piecesP1 :: [(index, index, player, piece)],
    piecesP2 :: [(index, index, player, piece)],
    requiredPieceMove :: Maybe (index, index, player, piece)
  }

class PlayableGame a index tile player piece moveType | a -> index, a -> tile, a -> player, a -> piece, a -> moveType where
  curPlayer :: a -> player
  curPlayerId :: a -> Int
  allPiecesP1 :: a -> [(index, index, player, piece)]
  allPiecesP2 :: a -> [(index, index, player, piece)]
  allPos :: a -> [(index, index, tile)]

  moveEnabled :: a -> Bool
  moveEnabled _ = False

  canMove :: a -> player -> (index, index) -> Bool
  canMove _ _ _ = False

  canMoveTo :: a -> player -> (index, index) -> (index, index) -> Bool
  canMoveTo _ _ _ _ = False

  move :: a -> player -> (index, index) -> (index, index) -> [GameChange index player piece moveType]
  move _ _ _ _ = []

  activateEnabled :: a -> Bool
  activateEnabled _ = False

  canActivate :: a -> player -> (index, index) -> Bool
  canActivate _ _ _ = False

  activate :: a -> player -> (index, index) -> [GameChange index player piece moveType]
  activate _ _ _ = []

  applyChange :: a -> GameChange index player piece moveType -> a
  applyChange g _ = g

  applyChanges :: a -> [GameChange index player piece moveType] -> a
  applyChanges a ls = foldl applyChange a ls
