{-# LANGUAGE MultiParamTypeClasses #-}

module Checkers
  ( Piece,
    Tile,
    Player,
    CheckersGame,
    defaultCheckersGame,
  )
where

import Data.Ix (Ix)
import Data.Maybe (isJust, listToMaybe)
import Game.Board.BasicTurnGame
  ( GameChange (AddPiece, FinishMove, MovePiece, RemovePiece, RemovePieceOtherPlayer, TurnPieceQueen),
    GameState (..),
    Piece (..),
    PlayableGame (..),
    Player (..),
    Tile (..),
    getPieceAt,
    getTileAt,
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
    piecesPA = [(x, y, Player1, Normal) | (x, y, _) <- allTiles, y >= piecesPlayerAStart]
    piecesPB = [(x, y, Player2, Normal) | (x, y, _) <- allTiles, y <= piecesPlayerBEnd]

piecesCurrentPlayer :: GameState index tile Player piece -> [(index, index, Player, piece)]
piecesCurrentPlayer game
  | curPlayer' game == Player1 = piecesP1 game
  | otherwise = piecesP2 game

piecesOtherPlayer :: GameState index tile Player piece -> [(index, index, Player, piece)]
piecesOtherPlayer game
  | curPlayer' game == Player1 = piecesP2 game
  | otherwise = piecesP1 game

hasPieceDiagonalCapture ::
  (Ix index, Num index) =>
  GameState index tile player piece ->
  [(index, index, player, piece)] ->
  (index, index) ->
  (index, index) ->
  Bool
hasPieceDiagonalCapture game pieces (xOrig, yOrig) (xDest, yDest) =
  isJust (getPosPieceDiagonalCapture game pieces (xOrig, yOrig) (xDest, yDest))

getPosPieceDiagonalCapture ::
  (Ix index, Num index) =>
  GameState index tile player piece ->
  [(index, index, player, piece)] ->
  (index, index) ->
  (index, index) ->
  Maybe (index, index)
getPosPieceDiagonalCapture game pieces (xOrig, yOrig) (xDest, yDest) =
  returnIfFoundOnlyOnePiece [(x, y) | (x, y, _, _) <- pieces, abs (x - xOrig) == abs (y - yOrig), x > minX, x < maxX, y > minY, y < maxY]
  where
    minX = (min xOrig xDest)
    maxX = (max xOrig xDest)
    minY = (min yOrig yDest)
    maxY = (max yOrig yDest)
    returnIfFoundOnlyOnePiece (h : t)
      | length t == 0 = Just h
      | otherwise = Nothing
    returnIfFoundOnlyOnePiece [] =
      Nothing

hasPieceDiagonalMove ::
  (Ix index, Num index) =>
  GameState index tile player piece ->
  [(index, index, player, piece)] ->
  (index, index) ->
  (index, index) ->
  Bool
hasPieceDiagonalMove game pieces (xOrig, yOrig) (xDest, yDest) =
  isJust (getPosPieceDiagonalMove game pieces (xOrig, yOrig) (xDest, yDest))

getPosPieceDiagonalMove ::
  (Ix index, Num index) =>
  GameState index tile player piece ->
  [(index, index, player, piece)] ->
  (index, index) ->
  (index, index) ->
  Maybe (index, index)
getPosPieceDiagonalMove game pieces (xOrig, yOrig) (xDest, yDest) =
  listToMaybe [(x, y) | (x, y, _, _) <- pieces, abs (x - xOrig) == abs (y - yOrig), x > minX, x < maxX, y > minY, y < maxY]
  where
    minX = (min xOrig xDest)
    maxX = (max xOrig xDest)
    minY = (min yOrig yDest)
    maxY = (max yOrig yDest)

getPosFromJust :: Maybe (index, index) -> (index, index)
getPosFromJust (Just (x, y)) = (x, y)

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
    | Just (player, piece) <- (getPieceAt game (piecesCurrentPlayer game) pos) =
      player == player
    | otherwise = False

  canMoveTo (CheckersGame game) player posOrig posDest
    | Just (tile) <- getTileAt game (boardPos game) posDest =
      True
    | otherwise = False

  -- Regras que definem os movimentos permitidos
  move (CheckersGame game) _player posOrig (xDest, yDest)
    | not reachOtherSide && isCaptureMove =
      do
        let posPieceToBeRemoved = getPieceOtherPlayerOnPath
        [ MovePiece posOrig (xDest, yDest) (pieceIsQueen (piecesCurrentPlayer game) posOrig),
          RemovePieceOtherPlayer posPieceToBeRemoved (pieceIsQueen (piecesOtherPlayer game) posPieceToBeRemoved),
          FinishMove _player
          ]
    | reachOtherSide && isCaptureMove =
      do
        let posPieceToBeRemoved = getPieceOtherPlayerOnPath
        [ MovePiece posOrig (xDest, yDest) False,
          RemovePieceOtherPlayer posPieceToBeRemoved (pieceIsQueen (piecesOtherPlayer game) posPieceToBeRemoved),
          TurnPieceQueen (xDest, yDest),
          FinishMove _player
          ]
    | not reachOtherSide && isNormalMove =
      [ MovePiece posOrig (xDest, yDest) (pieceIsQueen (piecesCurrentPlayer game) posOrig),
        FinishMove _player
      ]
    | reachOtherSide && isNormalMove =
      [ MovePiece posOrig (xDest, yDest) False,
        TurnPieceQueen (xDest, yDest),
        FinishMove _player
      ]
    | otherwise =
      []
    where
      isCaptureMove =
        hasPiece game (piecesCurrentPlayer game) posOrig && hasPieceOtherPlayerOnPath
          && not (hasPiece game (piecesCurrentPlayer game) (xDest, yDest))
          && not (hasPiece game (piecesOtherPlayer game) (xDest, yDest))
          && correctDiffCapture
      isNormalMove =
        hasPiece game (piecesCurrentPlayer game) posOrig
          && not (hasPiece game (piecesCurrentPlayer game) (xDest, yDest))
          && not (hasPiece game (piecesOtherPlayer game) (xDest, yDest))
          && not (hasPieceDiagonalMove game (piecesCurrentPlayer game) posOrig (xDest, yDest))
          && not (hasPieceDiagonalMove game (piecesOtherPlayer game) posOrig (xDest, yDest))
          && correctDiffMove
      reachOtherSide =
        ((_player == Player1 && yDest == 0) || (_player == Player2 && yDest == boardSize - 1)) && not (pieceIsQueen (piecesCurrentPlayer game) posOrig)
      diffY = snd posOrig - snd (xDest, yDest)
      diffXAbs = abs (fst posOrig - fst (xDest, yDest))
      diffYAbs = abs (snd posOrig - snd (xDest, yDest))
      correctDiffMove
        | pieceIsQueen (piecesCurrentPlayer game) posOrig = (diffYAbs < boardSize)
        | isPlayer1 = diffY == 1
        | otherwise = diffY == -1
      correctDiffCapture
        | pieceIsQueen (piecesCurrentPlayer game) posOrig = (diffXAbs < boardSize && diffYAbs < boardSize)
        | otherwise = (diffXAbs == 2 && diffYAbs == 2)
      hasPieceOtherPlayerOnPath = hasPieceDiagonalCapture game (piecesOtherPlayer game) posOrig (xDest, yDest)
      getPieceOtherPlayerOnPath = getPosFromJust (getPosPieceDiagonalCapture game (piecesOtherPlayer game) posOrig (xDest, yDest))
      isPlayer1 = curPlayer' game == Player1
      pieceIsQueen pieces pos
        | Just (player, piece) <- (getPieceAt game pieces pos) =
          piece == Queen
        | otherwise = False

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
  -- Funcao que troca o jogador atual
  applyChange (CheckersGame game) (FinishMove player)
    | player == Player1 =
      CheckersGame (game {curPlayer' = Player2})
    | otherwise = CheckersGame (game {curPlayer' = Player1})
