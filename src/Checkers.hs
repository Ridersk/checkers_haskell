{-# LANGUAGE MultiParamTypeClasses #-}

module Checkers
  ( Piece,
    Tile,
    Player,
    CheckersGame,
    defaultCheckersGame,
  )
where

import Game.Board.BasicTurnGame
  ( GameChange (AddPiece, FinishMove, MovePiece, RemovePiece),
    GameState (..),
    PlayableGame (..),
    Player (..),
    getPieceAt,
    hasPiece,
    player1Id,
    player2Id,
  )

data Piece = Piece1 | Piece2

data Tile = Tile

boardSize = 8

piecesPlayerAStart = 5

piecesPlayerBEnd = 2

newtype CheckersGame = CheckersGame (GameState Int Tile Player Piece)

-- Game Initial State
defaultCheckersGame :: CheckersGame
defaultCheckersGame =
  CheckersGame $
    GameState
      { curPlayer' = Player1,
        boardPos = allTiles,
        piecesPlayerA = piecesPA,
        piecesPlayerB = piecesPB
      }
  where
    allTiles = [(x, y, Tile) | x <- [0 .. boardSize - 1] :: [Int], y <- [0 .. boardSize - 1] :: [Int], (x + y) `mod` 2 == 0]
    piecesPA = [(x, y, Player1, Piece1) | (x, y, _) <- allTiles, y >= piecesPlayerAStart]
    piecesPB = [(x, y, Player2, Piece2) | (x, y, _) <- allTiles, y <= piecesPlayerBEnd]

instance PlayableGame CheckersGame Int Tile Player Piece where
  -- "Static" game view
  curPlayer (CheckersGame game) = curPlayer' game
  allPiecesPA (CheckersGame game) = piecesPlayerA game
  allPiecesPB (CheckersGame game) = piecesPlayerB game
  allPos (CheckersGame game) = boardPos game

  -- Kind of moves that are allowed
  moveEnabled _ = True
  canMove (CheckersGame game) player _ = player == curPlayer' game
  canMoveTo _ _ _ _ = True

  -- Convert a "move" to a sequence of changes
  move (CheckersGame game) _player posOrig posDest
    | hasPiece game posOrig && not (hasPiece game posDest) && correctDiffMove =
      [MovePiece posOrig posDest _player]
    | hasPiece game posOrig && hasPiece game posIntermed && not (hasPiece game posDest) && correctDiffCapture =
      [MovePiece posOrig posDest _player, RemovePiece posIntermed _player]
    | otherwise =
      []
    where
      diffY = snd posOrig - snd posDest
      diffXAbs = abs (fst posOrig - fst posDest)
      diffYAbs = abs (snd posOrig - snd posDest)
      correctDiffMove
        | isPlayer1 = diffY == 1
        | otherwise = diffY == -1
      correctDiffCapture = (diffXAbs == 2 && diffYAbs == 2)
      posIntermed = ((fst posOrig + fst posDest) `div` 2, (snd posOrig + snd posDest) `div` 2)
      isPlayer1 = curPlayer' game == Player1

  -- Apply a change to the game
  applyChange psg@(CheckersGame game) (MovePiece posOrig posDest player)
    | Just (player, piece) <- getPieceAt game posOrig =
      applyChanges psg [RemovePiece posOrig player, RemovePiece posDest player, AddPiece posDest player piece]
    | otherwise =
      psg
  applyChange (CheckersGame game) (AddPiece (x, y) player piece) =
    CheckersGame (game {piecesPlayerA = (x, y, player, piece) : piecesPlayerA game, piecesPlayerB = (x, y, player, piece) : piecesPlayerB game})
  -- Filter Removed Piece
  applyChange (CheckersGame game) (RemovePiece (x, y) player) =
    CheckersGame
      ( game
          { piecesPlayerA =
              [ (x', y', player, piece)
                | (x', y', player, piece) <- piecesPlayerA game,
                  (x /= x' || y /= y')
              ],
            piecesPlayerB =
              [ (x', y', player, piece)
                | (x', y', player, piece) <- piecesPlayerB game,
                  (x /= x' || y /= y')
              ]
          }
      )
  applyChange (CheckersGame game) (FinishMove player) =
    CheckersGame (game {curPlayer' = Player2})
