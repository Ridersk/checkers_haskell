{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.Gtk.Board.BoardLink where

import Control.Monad (forM_, when)
import Data.Board.GameBoardIO (GameBoard)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Ix (Ix)
import Data.Maybe (isJust)
import Game.Board.BasicTurnGame
  ( GameChange (..),
    PlayableGame
      ( allPiecesP1,
        allPiecesP2,
        allPos,
        applyChange,
        canMove,
        canMoveTo,
        curPlayer,
        curPlayerId,
        move,
        moveEnabled
      ),
    player1Id,
  )
import Graphics.UI.Gtk
  ( Color (Color),
    Pixbuf,
    StateType (StateActive, StateNormal, StatePrelight, StateSelected),
    widgetModifyBg,
  )
import Graphics.UI.Gtk.Board.TiledBoard
  ( Board (boardPiecesP1, boardPiecesP1Queen, boardPiecesP2, boardPiecesP2Queen),
    PixmapsFor,
    SizeAdjustment,
    boardEnableDrag,
    boardMovePiece,
    boardNew,
    boardOnPieceDragDrop,
    boardOnPieceDragOver,
    boardOnPieceDragStart,
    boardRemovePiece,
    boardSetBackground,
    boardSetPiece,
  )

attachGameRules ::
  (PlayableGame pg index tile player piece moveType, Ix index) =>
  Game pg index tile player piece ->
  IO (Board index tile (player, piece))
attachGameRules game = do
  board <-
    boardNew
      (allPos $ gameS game)
      (tileF $ visual game)
      (pieceP1 $ visual game)
      (pieceP1Queen $ visual game)
      (pieceP2 $ visual game)
      (pieceP2Queen $ visual game)

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
  (PlayableGame pg index tile player piece moveType, Ix index) =>
  pg ->
  Board index tile (player, piece) ->
  GameChange index player piece moveType ->
  IO ()
applyBoardChange game board (AddPiece pos player piece queen) = boardSetPiece pos (player, piece) board (boardPiecesCurrentPlayer game board queen)
applyBoardChange game board (RemovePiece pos queen) = boardRemovePiece pos board (boardPiecesCurrentPlayer game board queen)
applyBoardChange game board (RemovePieceOtherPlayer pos queen) = boardRemovePiece pos board (boardPiecesOtherPlayer game board queen)
applyBoardChange game board (MovePiece posO posD queen) =
  boardMovePiece posO posD board (boardPiecesCurrentPlayer game board queen) (boardPiecesCurrentPlayer game board queen)
applyBoardChange game board (TurnPieceQueen pos) =
  boardMovePiece pos pos board (boardPiecesCurrentPlayer game board False) (boardPiecesCurrentPlayer game board True)
applyBoardChange game board (BlockBoard _) = return ()
applyBoardChange game board (ReleaseBoard) = return ()
applyBoardChange game board (SwitchPlayer _) = return ()
applyBoardChange game board (FinishMove _ _ _) = return ()

boardPiecesCurrentPlayer ::
  PlayableGame pg index tile player piece moveType =>
  pg ->
  Board index tile (player, piece) ->
  Bool ->
  GameBoard index (player, piece)
boardPiecesCurrentPlayer game board False
  | (curPlayerId game) == player1Id = (boardPiecesP1 board)
  | otherwise = (boardPiecesP2 board)
boardPiecesCurrentPlayer game board True
  | (curPlayerId game) == player1Id = (boardPiecesP1Queen board)
  | otherwise = (boardPiecesP2Queen board)

boardPiecesOtherPlayer ::
  PlayableGame pg index tile player piece moveType =>
  pg ->
  Board index tile (player, piece) ->
  Bool ->
  GameBoard index (player, piece)
boardPiecesOtherPlayer game board False
  | (curPlayerId game) == player1Id = (boardPiecesP2 board)
  | otherwise = (boardPiecesP1 board)
boardPiecesOtherPlayer game board True
  | (curPlayerId game) == player1Id = (boardPiecesP2Queen board)
  | otherwise = (boardPiecesP1Queen board)

data VisualGameAspects index tile player piece = VisualGameAspects
  { tileF :: PixmapsFor tile,
    pieceP1 :: PixmapsFor (player, piece),
    pieceP1Queen :: PixmapsFor (player, piece),
    pieceP2 :: PixmapsFor (player, piece),
    pieceP2Queen :: PixmapsFor (player, piece),
    bgColor :: (Int, Int, Int),
    bg :: Maybe (Pixbuf, SizeAdjustment)
  }

data Game pg index tile player piece = Game
  { visual :: VisualGameAspects index tile player piece,
    gameS :: pg
  }
