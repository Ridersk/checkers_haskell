module CheckersUtils
  ( hasPiece,
    getPieceAt,
    getTileAt,
    piecesCurrentPlayer,
    piecesOtherPlayer,
    hasPieceDiagonalCapture,
    getPosPieceDiagonalCapture,
    hasPieceDiagonalMove,
    getPosPieceDiagonalMove,
    getPosFromJust,
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
    player1Id,
    player2Id,
  )

hasPiece :: Ix index => GameState index tile player piece -> [(index, index, player, piece)] -> (index, index) -> Bool
hasPiece game pieces ix = isJust (getPieceAt game pieces ix)

getPieceAt :: Ix index => GameState index tile player piece -> [(index, index, player, piece)] -> (index, index) -> Maybe (player, piece)
getPieceAt game pieces (posX, posY) =
  listToMaybe [(player, piece) | (x, y, player, piece) <- pieces, x == posX, y == posY]

getTileAt :: Ix index => GameState index tile player piece -> [(index, index, tile)] -> (index, index) -> Maybe (tile)
getTileAt game tiles (posX, posY) =
  listToMaybe [(tile) | (x, y, tile) <- tiles, x == posX, y == posY]

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
