{-# LANGUAGE MultiParamTypeClasses #-}

module Checkers
  ( Piece,
    Tile,
    Player,
    CheckersGame,
    defaultCheckersGame,
  )
where

import Data.Maybe ()
import Game.Board.BasicTurnGame
  ( GameChange (AddPiece, FinishMove, MovePiece, RemovePiece, RemovePieceOtherPlayer, TurnPieceQueen),
    GameState (..),
    Piece (..),
    PlayableGame (..),
    Player (..),
    Tile (..),
    getPieceAt,
    hasPiece,
    player1Id,
    player2Id,
  )

boardSize :: Int
boardSize = 8

piecesPlayerAStart :: Int
piecesPlayerAStart = 5

piecesPlayerBEnd :: Int
piecesPlayerBEnd = 2

newtype CheckersGame = CheckersGame (GameState Int Tile Player Piece)

-- Estado Inicial do Jogo
defaultCheckersGame :: CheckersGame
defaultCheckersGame =
  CheckersGame $
    GameState
      { curPlayer' = Player1,
        boardPos = allTiles,
        piecesP1 = piecesPA,
        piecesP2 = piecesPB
      }
  where
    allTiles = [(x, y, Tile) | x <- [0 .. boardSize - 1] :: [Int], y <- [0 .. boardSize - 1] :: [Int], (x + y) `mod` 2 == 0]
    piecesPA = [(x, y, Player1, Piece1) | (x, y, _) <- allTiles, y >= piecesPlayerAStart]
    piecesPB = [(x, y, Player2, Piece2) | (x, y, _) <- allTiles, y <= piecesPlayerBEnd]

piecesCurrentPlayer :: GameState index tile Player piece -> [(index, index, Player, piece)]
piecesCurrentPlayer game
  | curPlayer' game == Player1 = piecesP1 game
  | otherwise = piecesP2 game

piecesOtherPlayer :: GameState index tile Player piece -> [(index, index, Player, piece)]
piecesOtherPlayer game
  | curPlayer' game == Player1 = piecesP2 game
  | otherwise = piecesP1 game

instance PlayableGame CheckersGame Int Tile Player Piece where
  -- "Static" game view
  curPlayer (CheckersGame game) = curPlayer' game
  curPlayerId (CheckersGame game)
    | curPlayer' game == Player1 = player1Id
    | otherwise = player2Id
  allPiecesP1 (CheckersGame game) = piecesP1 game
  allPiecesP2 (CheckersGame game) = piecesP2 game

  allPos (CheckersGame game) = boardPos game

  -- Habilar Movimento das peças se o jogador atual for o dono da peça
  moveEnabled _ = True
  canMove (CheckersGame game) player pos
    | Just (playerPiece, piece) <- (getPieceAt game (piecesCurrentPlayer game) pos) =
      player == playerPiece
    | otherwise = False

  canMoveTo _ _ _ _ = True

  -- Regras que definem os movimentos permitidos
  move (CheckersGame game) _player posOrig (xDest, yDest)
    | not becomeQueen && isNormalMove =
      [ MovePiece posOrig (xDest, yDest) False,
        FinishMove _player
      ]
    | becomeQueen && isNormalMove =
      [ MovePiece posOrig (xDest, yDest) False,
        TurnPieceQueen (xDest, yDest),
        FinishMove _player
      ]
    | not becomeQueen && isCaptureMove =
      [ MovePiece posOrig (xDest, yDest) False,
        RemovePieceOtherPlayer posIntermed False,
        FinishMove _player
      ]
    | becomeQueen && isCaptureMove =
      [ MovePiece posOrig (xDest, yDest) False,
        RemovePieceOtherPlayer posIntermed False,
        TurnPieceQueen (xDest, yDest),
        FinishMove _player
      ]
    | otherwise =
      []
    where
      isNormalMove =
        hasPiece game (piecesCurrentPlayer game) posOrig
          && not (hasPiece game (piecesCurrentPlayer game) (xDest, yDest))
          && correctDiffMove
      isCaptureMove =
        hasPiece game (piecesCurrentPlayer game) posOrig && hasPieceIntermedOtherPlayer
          && not (hasPiece game (piecesCurrentPlayer game) (xDest, yDest))
          && correctDiffCapture
      becomeQueen = (_player == Player1 && yDest == 0) || (_player == Player2 && yDest == boardSize - 1)
      diffY = snd posOrig - snd (xDest, yDest)
      diffXAbs = abs (fst posOrig - fst (xDest, yDest))
      diffYAbs = abs (snd posOrig - snd (xDest, yDest))
      correctDiffMove
        | isPlayer1 = diffY == 1
        | otherwise = diffY == -1
      correctDiffCapture = (diffXAbs == 2 && diffYAbs == 2)
      posIntermed = ((fst posOrig + fst (xDest, yDest)) `div` 2, (snd posOrig + snd (xDest, yDest)) `div` 2)
      isPlayer1 = curPlayer' game == Player1
      hasPieceIntermedOtherPlayer = hasPiece game (piecesOtherPlayer game) posIntermed

  -- Funcao que define as Mudanças no Tabuleiro
  -- Em BoardLink.hs ha uma funcao que observa essas mudanças e aplica elas a parte grafica do tabuleiro
  applyChange psg@(CheckersGame game) (MovePiece posOrig posDest queen)
    | Just (player, piece) <- getPieceAt game (piecesCurrentPlayer game) posOrig =
      applyChanges psg [RemovePiece posOrig False, RemovePiece posDest False, AddPiece posDest player piece False]
    | otherwise =
      psg
  applyChange (CheckersGame game) (AddPiece (x, y) player piece queen)
    | curPlayer' game == Player1 = CheckersGame (game {piecesP1 = (x, y, player, piece) : piecesP1 game})
    | otherwise = CheckersGame (game {piecesP2 = (x, y, player, piece) : piecesP2 game})
  -- Filtra a peca selecionada do jogador atual para remocao
  applyChange (CheckersGame game) (RemovePiece (x, y) queen) =
    CheckersGame
      ( game
          { piecesP1 =
              [ (x', y', player, piece)
                | (x', y', player, piece) <- piecesP1 game,
                  (x /= x' || y /= y')
              ],
            piecesP2 =
              [ (x', y', player, piece)
                | (x', y', player, piece) <- piecesP2 game,
                  (x /= x' || y /= y')
              ]
          }
      )
  -- Filtra a peca selecionada do outro jogador atual para remocao
  applyChange (CheckersGame game) (RemovePieceOtherPlayer (x, y) queen) =
    CheckersGame
      ( game
          { piecesP1 =
              [ (x', y', player, piece)
                | (x', y', player, piece) <- piecesP1 game,
                  (x /= x' || y /= y')
              ],
            piecesP2 =
              [ (x', y', player, piece)
                | (x', y', player, piece) <- piecesP2 game,
                  (x /= x' || y /= y')
              ]
          }
      )
  applyChange psg@(CheckersGame game) (TurnPieceQueen pos)
    | Just (player, piece) <- getPieceAt game (piecesCurrentPlayer game) pos =
      applyChanges psg [RemovePiece pos False, AddPiece pos player piece True]
    | otherwise =
      psg
  -- Funcao que troca o jogador atual
  applyChange (CheckersGame game) (FinishMove player)
    | player == Player1 =
      CheckersGame (game {curPlayer' = Player2})
    | otherwise = CheckersGame (game {curPlayer' = Player1})
