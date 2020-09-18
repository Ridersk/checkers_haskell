{-# LANGUAGE MultiParamTypeClasses #-}

module Checkers
  ( Piece,
    Tile,
    Player,
    CheckersGame,
    defaultCheckersGame,
  )
where

import CheckersUtils
  ( getPieceAt,
    getPosFromJust,
    getPosPieceDiagonalCapture,
    getTileAt,
    hasNearPiecesToBeCaptured,
    hasPiece,
    hasPieceDiagonalCapture,
    hasPieceDiagonalMove,
    hasTile,
    piecesCurrentPlayer,
    piecesOtherPlayer,
  )
import Data.Ix (Ix)
import Data.Maybe (isJust, listToMaybe)
import Game.Board.BasicTurnGame
  ( GameChange
      ( AddPiece,
        BlockBoard,
        FinishMove,
        MovePiece,
        ReleaseBoard,
        RemovePiece,
        RemovePieceOtherPlayer,
        SwitchPlayer,
        TurnPieceQueen
      ),
    GameState (..),
    MoveType (..),
    Piece (..),
    PlayableGame (..),
    Player (..),
    Tile (..),
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
        piecesP2 = piecesPB,
        requiredPieceMove = Nothing
      }
  where
    allTiles = [(x, y, Tile) | x <- [0 .. boardSize - 1] :: [Int], y <- [0 .. boardSize - 1] :: [Int], (x + y) `mod` 2 == 0]
    piecesPA = [(x, y, Player1, Normal) | (x, y, _) <- allTiles, y >= piecesPlayerAStart]
    piecesPB = [(x, y, Player2, Normal) | (x, y, _) <- allTiles, y <= piecesPlayerBEnd]

instance PlayableGame CheckersGame Int Tile Player Piece MoveType where
  -- "Static" game view
  curPlayer (CheckersGame game) = curPlayer' game
  curPlayerId (CheckersGame game)
    | curPlayer' game == Player1 = player1Id
    | otherwise = player2Id
  allPiecesP1 (CheckersGame game) = piecesP1 game
  allPiecesP2 (CheckersGame game) = piecesP2 game

  allPos (CheckersGame game) = boardPos game

  -- Habilita Movimento das peças se o jogador atual for o dono da peça
  moveEnabled _ = True
  canMove (CheckersGame game) _player pos@(xOrig, yOrig)
    | Just (x, y, player, piece) <- (requiredPieceMove game) =
      player == _player && x == xOrig && y == yOrig
    | Just (player, piece) <- (getPieceAt game (piecesCurrentPlayer game) pos) =
      player == _player
    | otherwise = False

  canMoveTo (CheckersGame game) _player posOrig posDest = hasTile game (boardPos game) posDest

  -- Regras que definem os movimentos permitidos
  move (CheckersGame game) _player posOrig posDest@(xDest, yDest)
    | not reachOtherSide && isCaptureMove =
      do
        let posPieceToBeRemoved = getPieceOtherPlayerOnPath
        [ MovePiece posOrig posDest (pieceIsQueen (piecesCurrentPlayer game) posOrig),
          RemovePieceOtherPlayer posPieceToBeRemoved (pieceIsQueen (piecesOtherPlayer game) posPieceToBeRemoved),
          FinishMove posDest _player Capture
          ]
    | reachOtherSide && isCaptureMove =
      do
        let posPieceToBeRemoved = getPieceOtherPlayerOnPath
        [ MovePiece posOrig posDest False,
          RemovePieceOtherPlayer posPieceToBeRemoved (pieceIsQueen (piecesOtherPlayer game) posPieceToBeRemoved),
          TurnPieceQueen posDest,
          FinishMove posDest _player Capture
          ]
    | not boardBlocked && not reachOtherSide && isNormalMove =
      [ MovePiece posOrig posDest (pieceIsQueen (piecesCurrentPlayer game) posOrig),
        FinishMove posDest _player Move
      ]
    | not boardBlocked && reachOtherSide && isNormalMove =
      [ MovePiece posOrig posDest False,
        TurnPieceQueen posDest,
        FinishMove posDest _player Move
      ]
    | otherwise =
      []
    where
      isCaptureMove =
        hasPiece game (piecesCurrentPlayer game) posOrig && hasPieceOtherPlayerOnPath
          && not (hasPiece game (piecesCurrentPlayer game) posDest)
          && not (hasPiece game (piecesOtherPlayer game) posDest)
          && correctDiffCapture
      isNormalMove =
        hasPiece game (piecesCurrentPlayer game) posOrig
          && not (hasPiece game (piecesCurrentPlayer game) posDest)
          && not (hasPiece game (piecesOtherPlayer game) posDest)
          && not (hasPieceDiagonalMove game (piecesCurrentPlayer game) posOrig posDest)
          && not (hasPieceDiagonalMove game (piecesOtherPlayer game) posOrig posDest)
          && correctDiffMove
      reachOtherSide =
        ((_player == Player1 && yDest == 0) || (_player == Player2 && yDest == boardSize - 1)) && not (pieceIsQueen (piecesCurrentPlayer game) posOrig)
      diffY = snd posOrig - snd posDest
      diffXAbs = abs (fst posOrig - fst posDest)
      diffYAbs = abs (snd posOrig - snd posDest)
      correctDiffMove
        | pieceIsQueen (piecesCurrentPlayer game) posOrig = (diffYAbs < boardSize)
        | isPlayer1 = diffY == 1
        | otherwise = diffY == -1
      correctDiffCapture
        | pieceIsQueen (piecesCurrentPlayer game) posOrig = (diffXAbs < boardSize && diffYAbs < boardSize)
        | otherwise = (diffXAbs == 2 && diffYAbs == 2)
      hasPieceOtherPlayerOnPath = hasPieceDiagonalCapture game (piecesOtherPlayer game) posOrig posDest
      getPieceOtherPlayerOnPath = getPosFromJust (getPosPieceDiagonalCapture game (piecesOtherPlayer game) posOrig posDest)
      isPlayer1 = curPlayer' game == Player1
      pieceIsQueen pieces pos
        | Just (player, piece) <- (getPieceAt game pieces pos) =
          piece == Queen
        | otherwise = False
      boardBlocked = isJust (requiredPieceMove game)

  -- Funcao que define as Mudanças no Tabuleiro
  -- Em BoardLink.hs ha uma funcao que observa essas mudanças e aplica elas a parte grafica do tabuleiro
  applyChange psg@(CheckersGame game) (MovePiece posOrig posDest isQueen)
    | Just (player, piece) <- getPieceAt game (piecesCurrentPlayer game) posOrig =
      applyChanges psg [RemovePiece posOrig isQueen, RemovePiece posDest isQueen, AddPiece posDest player piece isQueen]
    | otherwise =
      psg
  applyChange (CheckersGame game) (AddPiece (x, y) player piece isQueen)
    | curPlayer' game == Player1 = CheckersGame (game {piecesP1 = (x, y, player, pieceType) : piecesP1 game})
    | otherwise = CheckersGame (game {piecesP2 = (x, y, player, pieceType) : piecesP2 game})
    where
      pieceType
        | isQueen = Queen
        | otherwise = Normal
  -- Filtra a peca selecionada do jogador atual para remocao
  applyChange (CheckersGame game) (RemovePiece (x, y) isQueen) =
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
  applyChange (CheckersGame game) (RemovePieceOtherPlayer (x, y) isQueen) =
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
  applyChange psg@(CheckersGame game) (BlockBoard pos@(xPos, yPos))
    | Just (player, piece) <- getPieceAt game (piecesCurrentPlayer game) pos =
      CheckersGame (game {requiredPieceMove = Just (xPos, yPos, player, piece)})
    | otherwise = psg
  applyChange psg@(CheckersGame game) (ReleaseBoard) =
    CheckersGame (game {requiredPieceMove = Nothing})
  applyChange psg@(CheckersGame game) (SwitchPlayer player)
    | player == Player1 =
      CheckersGame (game {curPlayer' = Player2})
    | otherwise = CheckersGame (game {curPlayer' = Player1})
  -- Funcao que troca o jogador atual ou chama a mudanca para bloquear o
  -- tabuleiro para obrigar uma continuacao de capturas de pecas
  applyChange psg@(CheckersGame game) (FinishMove posDest player moveType)
    | moveType == Capture && (hasNearPiecesToBeCaptured game (piecesOtherPlayer game) posDest (boardSize - 1)) =
      applyChanges psg [BlockBoard posDest]
    | moveType == Capture && not (hasNearPiecesToBeCaptured game (piecesOtherPlayer game) posDest (boardSize - 1)) =
      applyChanges psg [ReleaseBoard, SwitchPlayer player]
    | otherwise = applyChanges psg [SwitchPlayer player]
