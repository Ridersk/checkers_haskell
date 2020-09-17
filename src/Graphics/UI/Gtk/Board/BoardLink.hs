{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.Gtk.Board.BoardLink where

import Control.Monad
import Data.Board.GameBoardIO
import Data.IORef
import Data.Ix
import Data.Maybe
import Game.Board.BasicTurnGame
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard

attachGameRules ::
  (PlayableGame pg index tile player piece, Ix index) =>
  Game pg index tile player piece ->
  IO (Board index tile (player, piece))
attachGameRules game = do
  board <- boardNew (allPos $ gameS game) (tileF $ visual game) (pieceA $ visual game) (pieceB $ visual game)

  let (r, g, b) = bgColor (visual game)
      (r', g', b') = (fromIntegral r, fromIntegral g, fromIntegral b)
  mapM_ (\s -> widgetModifyBg board s (Color r' g' b')) [StateNormal, StateActive, StatePrelight, StateSelected]
  when (isJust (bg $ visual game)) $ boardSetBackground board (bg $ visual game)

  vgRef <- newIORef game

  -- Set the initial board state
  let allPiecesP1' = [((x, y), (pl, pc)) | (x, y, pl, pc) <- allPiecesP1 (gameS game)]
  mapM_ (\(pos, piece) -> boardSetPiece pos piece board (boardPiecesP1 board)) $ allPiecesP1'
  let allPiecesP2' = [((x, y), (pl, pc)) | (x, y, pl, pc) <- allPiecesP2 (gameS game)]
  mapM_ (\(pos, piece) -> boardSetPiece pos piece board (boardPiecesP2 board)) $ allPiecesP2'

  board `boardOnPieceDragStart` \pos -> do
    visualGame <- readIORef vgRef
    let game' = gameS visualGame
    return (moveEnabled game' && canMove game' (curPlayer game') pos)

  board `boardOnPieceDragOver` \posF posT -> do
    visualGame <- readIORef vgRef
    let game' = gameS visualGame
    return (moveEnabled game' && canMoveTo game' (curPlayer game') posF posT)

  board `boardOnPieceDragDrop` \posF posT -> do
    visualGame <- readIORef vgRef
    let game' = gameS visualGame
        moves = move game' (curPlayer game') posF posT
        game'' = foldl applyChange game' moves
    writeIORef vgRef (visualGame {gameS = game''})
    forM_ moves (applyBoardChange game' board)

  when (moveEnabled (gameS game)) $ boardEnableDrag board

  return board

applyBoardChange ::
  (PlayableGame pg index tile player piece, Ix index) =>
  pg ->
  Board index tile (player, piece) ->
  GameChange index player piece ->
  IO ()
applyBoardChange game board (AddPiece pos player piece) = boardSetPiece pos (player, piece) board (boardPiecesCurrentPlayer game board)
applyBoardChange game board (RemovePiece pos player) = boardRemovePiece pos board (boardPiecesCurrentPlayer game board)
applyBoardChange game board (RemovePieceOtherPlayer pos player) = boardRemovePiece pos board (boardPiecesOtherPlayer game board)
applyBoardChange game board (MovePiece posO posD player) = boardMovePiece posO posD board (boardPiecesCurrentPlayer game board)
applyBoardChange game board (FinishMove posO posD player) = return ()

boardPiecesCurrentPlayer ::
  PlayableGame pg index tile player piece =>
  pg ->
  Board index tile (player, piece) ->
  GameBoard index (player, piece)
boardPiecesCurrentPlayer game board
  | (curPlayerId game) == player1Id = (boardPiecesP1 board)
  | otherwise = (boardPiecesP2 board)

boardPiecesOtherPlayer ::
  PlayableGame pg index tile player piece =>
  pg ->
  Board index tile (player, piece) ->
  GameBoard index (player, piece)
boardPiecesOtherPlayer game board
  | (curPlayerId game) == player1Id = (boardPiecesP2 board)
  | otherwise = (boardPiecesP1 board)

data VisualGameAspects index tile player piece = VisualGameAspects
  { tileF :: PixmapsFor tile,
    pieceA :: PixmapsFor (player, piece),
    pieceB :: PixmapsFor (player, piece),
    bgColor :: (Int, Int, Int),
    bg :: Maybe (Pixbuf, SizeAdjustment)
  }

data Game pg index tile player piece = Game
  { visual :: VisualGameAspects index tile player piece,
    gameS :: pg
  }
