{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Board.BasicTurnGame
  ( Player (..),
    GameChange (..),
    GameState (..),
    PlayableGame (..),
    hasPiece,
    getPieceAt,
    player1Id,
    player2Id,
  )
where

import Data.Ix
import Data.Maybe

player1Id :: Int
player1Id = 0

player2Id :: Int
player2Id = 1

data Player = Player1 | Player2 deriving (Eq)

data GameChange index player piece
  = AddPiece (index, index) player piece
  | RemovePiece (index, index) player
  | MovePiece (index, index) (index, index) player
  | FinishMove player

-- FIXME: Wrong data structure => use unmutable matrices
hasPiece :: Ix index => GameState index tile player piece -> (index, index) -> Bool
hasPiece game ix = isJust (getPieceAt game ix)

getPieceAt :: Ix index => GameState index tile player piece -> (index, index) -> Maybe (player, piece)
getPieceAt game (posX, posY)
  | curPlayer' game == Player1 = listToMaybe [(player, piece) | (x, y, player, piece) <- piecesPlayerA game, x == posX, y == posY]
  | curPlayer' game == Player2 = listToMaybe [(player, piece) | (x, y, player, piece) <- piecesPlayerB game, x == posX, y == posY]
  | otherwise = Nothing

data GameState index tile player piece = GameState
  { curPlayer' :: Player,
    boardPos :: [(index, index, tile)],
    piecesPlayerA :: [(index, index, player, piece)],
    piecesPlayerB :: [(index, index, player, piece)]
  }

class PlayableGame a index tile player piece | a -> index, a -> tile, a -> player, a -> piece where
  curPlayer :: a -> Player
  allPiecesPA :: a -> [(index, index, player, piece)]
  allPiecesPB :: a -> [(index, index, player, piece)]
  allPos :: a -> [(index, index, tile)]

  moveEnabled :: a -> Bool
  moveEnabled _ = False

  canMove :: a -> Player -> (index, index) -> Bool
  canMove _ _ _ = False

  canMoveTo :: a -> Player -> (index, index) -> (index, index) -> Bool
  canMoveTo _ _ _ _ = False

  move :: a -> Player -> (index, index) -> (index, index) -> [GameChange index player piece]
  move _ _ _ _ = []

  activateEnabled :: a -> Bool
  activateEnabled _ = False

  canActivate :: a -> Player -> (index, index) -> Bool
  canActivate _ _ _ = False

  activate :: a -> player -> (index, index) -> [GameChange index player piece]
  activate _ _ _ = []

  applyChange :: a -> GameChange index player piece -> a
  applyChange g _ = g

  applyChanges :: a -> [GameChange index player piece] -> a
  applyChanges a ls = foldl applyChange a ls
