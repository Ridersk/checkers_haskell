{-# LANGUAGE MultiParamTypeClasses #-}

module CheckersRules
  ( Piece,
    Tile,
    Player,
    CheckersGame,
    defaultCheckersGame,
  )
where

import Game.Board.BasicTurnGame

data Piece = Piece

data Tile = Tile

data Player = Player

newtype CheckersGame = CheckersGame (GameState Int Tile Player Piece)

-- Basic Game Definition
defaultCheckersGame :: CheckersGame
defaultCheckersGame =
  CheckersGame $
    GameState
      { curPlayer' = Player,
        boardPos = allTiles,
        boardPieces' = pieces
      }
  where
    allTiles = [(x, y, Tile) | x <- [0 .. 6] :: [Int], y <- [0 .. 6] :: [Int]]
    inCorner x y =
      ((x > 4 || x < 2) && (y == 0 || y == 6))
        || ((y > 4 || y < 2) && (x == 0 || x == 6))
    pieces = [(x, y, Player, Piece) | (x, y, _) <- allTiles, (x /= 3 || y /= 3)]

instance PlayableGame CheckersGame Int Tile Player Piece where
  -- "Static" game view
  curPlayer (CheckersGame game) = curPlayer' game
  allPieces (CheckersGame game) = boardPieces' game
  allPos (CheckersGame game) = boardPos game

  -- Kind of moves that are allowed
  moveEnabled _ = True
  canMove _ _ _ = True
  canMoveTo _ _ _ _ = True

  -- Convert a "move" to a sequence of changes
  move (CheckersGame game) _player posOrig posDest
    | hasPiece game posOrig && hasPiece game posIntermed && not (hasPiece game posDest) && correctDiff =
      [MovePiece posOrig posDest, RemovePiece posIntermed]
    | otherwise =
      []
    where
      diffX = abs (fst posOrig - fst posDest)
      diffY = abs (snd posOrig - snd posDest)
      correctDiff = (diffX == 0 && diffY == 2) || (diffX == 2 && diffY == 0)
      posIntermed = ((fst posOrig + fst posDest) `div` 2, (snd posOrig + snd posDest) `div` 2)

  -- Apply a change to the game
  applyChange psg@(CheckersGame game) (MovePiece posOrig posDest)
    | Just (player, piece) <- getPieceAt game posOrig =
      applyChanges psg [RemovePiece posOrig, RemovePiece posDest, AddPiece posDest player piece]
    | otherwise =
      psg
  applyChange (CheckersGame game) (AddPiece (x, y) player piece) =
    CheckersGame (game {boardPieces' = (x, y, player, piece) : boardPieces' game})
  -- Filter Removed Piece
  applyChange (CheckersGame game) (RemovePiece (x, y)) =
    CheckersGame
      ( game
          { boardPieces' =
              [ (x', y', player, piece)
                | (x', y', player, piece) <- boardPieces' game,
                  (x /= x' || y /= y')
              ]
          }
      )
