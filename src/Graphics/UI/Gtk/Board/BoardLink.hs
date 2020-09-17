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
    forM_ moves (applyBoardChange (curPlayerId game') board)

  when (moveEnabled (gameS game)) $ boardEnableDrag board

  return board

applyBoardChange :: Ix index => Int -> Board index tile (player, piece) -> GameChange index player piece -> IO ()
applyBoardChange currentPlayer board (AddPiece pos player piece)
  | currentPlayer == player1Id = boardSetPiece pos (player, piece) board (boardPiecesP1 board)
  | otherwise = boardSetPiece pos (player, piece) board (boardPiecesP2 board)
applyBoardChange curPlayerId board (RemovePiece pos player)
  | curPlayerId == player1Id = boardRemovePiece pos board (boardPiecesP1 board)
  | otherwise = boardRemovePiece pos board (boardPiecesP2 board)
applyBoardChange curPlayerId board (RemovePieceOtherPlayer pos player)
  | curPlayerId == player1Id = boardRemovePiece pos board (boardPiecesP2 board)
  | otherwise = boardRemovePiece pos board (boardPiecesP1 board)
applyBoardChange curPlayerId board (MovePiece posO posD player)
  | curPlayerId == player1Id = boardMovePiece posO posD board (boardPiecesP1 board)
  | otherwise = boardMovePiece posO posD board (boardPiecesP2 board)
applyBoardChange curPlayerId board (FinishMove posO posD player)
  | curPlayerId == player1Id = boardMovePiece posO posD board (boardPiecesP1 board)
  | otherwise = boardMovePiece posO posD board (boardPiecesP2 board)

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
