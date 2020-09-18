module Graphics.UI.Gtk.Board.TiledBoard where

import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import Data.Array.IO (Ix (..), MArray (getBounds))
-- Local imports
import Data.Board.GameBoardIO
  ( GameBoard (..),
    gameBoardClear,
    gameBoardClone,
    gameBoardFoldM,
    gameBoardGetBoundaries,
    gameBoardGetPiece,
    gameBoardMapM_,
    gameBoardMovePiece,
    gameBoardNew,
    gameBoardNewEmpty,
    gameBoardRemovePiece,
    gameBoardSetPiece,
  )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, isJust)
import Graphics.UI.Gtk
  ( Dither (RgbDitherNone),
    DrawableClass,
    DrawingArea,
    EButton,
    EMotion,
    EventM,
    EventMask (ButtonPressMask, ButtonReleaseMask, PointerMotionMask),
    GObjectClass (..),
    ObjectClass,
    Pixbuf,
    Rectangle (Rectangle),
    WidgetClass,
    buttonPressEvent,
    buttonReleaseEvent,
    drawPixbuf,
    drawWindowBeginPaintRect,
    drawWindowClear,
    drawWindowEndPaint,
    drawingAreaNew,
    eventCoordinates,
    exposeEvent,
    motionNotifyEvent,
    on,
    pixbufGetHeight,
    pixbufGetWidth,
    realize,
    widgetAddEvents,
    widgetGetDrawWindow,
    widgetGetRealized,
    widgetGetSize,
    widgetQueueDraw,
    widgetSetSizeRequest,
  )
import Graphics.UI.Gtk.Gdk.GC (GC, gcNew)
import System.Glib.Types (GObjectClass (unsafeCastGObject))

data Board index tile piece = Board
  { boardDrawingArea :: DrawingArea,
    boardTiles :: GameBoard index tile,
    boardPiecesP1 :: GameBoard index piece,
    boardPiecesP1Queen :: GameBoard index piece,
    boardPiecesP2 :: GameBoard index piece,
    boardPiecesP2Queen :: GameBoard index piece,
    tilePixmaps :: PixmapsFor tile,
    pieceP1Pixmaps :: PixmapsFor piece,
    pieceP1QueenPixmaps :: PixmapsFor piece,
    pieceP2Pixmaps :: PixmapsFor piece,
    pieceP2QueenPixmaps :: PixmapsFor piece,
    tileSize :: (Int, Int),
    background :: IORef (Maybe (Pixbuf, SizeAdjustment)),
    overlay :: IORef (Maybe (Pixbuf, SizeAdjustment)),
    dragEnabled :: IORef Bool,
    draggingFrom :: IORef (Maybe (index, index)),
    draggingTo :: IORef (Maybe (index, index)),
    draggingMouseOrig :: IORef (Maybe (Int, Int)),
    draggingMousePos :: IORef (Maybe (Int, Int)),
    movingStatus :: IORef (Maybe (MovingStatus index))
  }

data MovingStatus index = MovingStatus
  { movingFrom :: (index, index),
    movingTo :: (index, index),
    stepsPerUnit :: Double,
    timePerUnit :: Double,
    movingStep :: Double
  }

instance WidgetClass (Board index tile piece)

instance ObjectClass (Board index tile piece)

instance GObjectClass (Board index tile piece) where
  toGObject = toGObject . boardDrawingArea
  unsafeCastGObject x =
    Board
      (unsafeCastGObject x)
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined

type PixmapsFor a = a -> Pixbuf

data SizeAdjustment = SizeAdjustment

boardNew ::
  Ix index =>
  [(index, index, tile)] ->
  PixmapsFor tile ->
  PixmapsFor piece ->
  PixmapsFor piece ->
  PixmapsFor piece ->
  PixmapsFor piece ->
  IO (Board index tile piece)
boardNew tileList tilePixs pieceP1Asset pieceP1QueenAsset pieceP2Asset pieceP2QueenAsset = do
  da <- drawingAreaNew

  tb <- gameBoardNew tileList
  piecesP1 <- gameBoardNewEmpty (map (\(x, y, _) -> (x, y)) tileList)
  piecesP1Queen <- gameBoardNewEmpty (map (\(x, y, _) -> (x, y)) tileList)
  piecesP2 <- gameBoardNewEmpty (map (\(x, y, _) -> (x, y)) tileList)
  piecesP2Queen <- gameBoardNewEmpty (map (\(x, y, _) -> (x, y)) tileList)

  ts <- getTileSize tileList tilePixs

  bg <- newIORef Nothing
  ov <- newIORef Nothing

  dragging <- newIORef False
  draggingF <- newIORef Nothing
  draggingT <- newIORef Nothing
  draggingO <- newIORef Nothing
  draggingP <- newIORef Nothing

  movingSt <- newIORef Nothing

  let board =
        Board
          da
          tb
          piecesP1
          piecesP1Queen
          piecesP2
          piecesP2Queen
          tilePixs
          pieceP1Asset
          pieceP1QueenAsset
          pieceP2Asset
          pieceP2QueenAsset
          ts
          bg
          ov
          dragging
          draggingF
          draggingT
          draggingO
          draggingP
          movingSt

  (pixW, pixH) <- boardGetPixelSize board

  da `on` realize $ widgetSetSizeRequest da pixW pixH
  da `on` exposeEvent $ liftIO (boardRefresh board) >> return False

  return board

getTileSize :: [(index, index, tile)] -> PixmapsFor tile -> IO (Int, Int)
getTileSize [] _ = return (0, 0)
getTileSize ((_, _, t) : _) pixs = do
  let pb = pixs t
  w <- pixbufGetWidth pb
  h <- pixbufGetHeight pb
  return (w, h)

boardGetPiece :: Ix index => (index, index) -> GameBoard index piece -> IO (Maybe piece)
boardGetPiece pos boardPiece = gameBoardGetPiece pos boardPiece

boardSetPiece :: Ix index => (index, index) -> piece -> Board index tile piece -> GameBoard index piece -> IO ()
boardSetPiece pos piece board boardPieces = do
  boardSetPieceNoRefresh pos piece board boardPieces
  boardInvalidate board

--   boardRefresh board

boardSetPieceNoRefresh :: Ix index => (index, index) -> piece -> Board index tile piece -> GameBoard index piece -> IO ()
boardSetPieceNoRefresh pos piece board boardPieces = do
  -- check that there's a tile there
  posOk <- fmap isJust $ gameBoardGetPiece pos $ boardTiles board
  when posOk $ do
    -- if there is, place the piece on the pieces board
    gameBoardSetPiece pos piece boardPieces

boardRemovePiece :: Ix index => (index, index) -> Board index tile piece -> GameBoard index piece -> IO ()
boardRemovePiece pos board boardPieces = do
  -- check that there's a tile there
  posOk <- fmap isJust $ gameBoardGetPiece pos $ boardTiles board
  when posOk $ do
    -- if there is, remove the piece from the pieces board
    gameBoardRemovePiece pos boardPieces
    -- refresh the image
    boardInvalidate board

-- boardRefresh board

boardMovePiece :: Ix index => (index, index) -> (index, index) -> Board index tile piece -> GameBoard index piece -> GameBoard index piece -> IO ()
boardMovePiece posO posD board boardPiecesOrig boardPiecesDest = do
  -- check that there's a tile there
  posOrigOk <- fmap isJust $ gameBoardGetPiece posO $ boardTiles board
  posDestOk <- fmap isJust $ gameBoardGetPiece posD $ boardTiles board
  when (posOrigOk && posDestOk) $ do
    -- Move the piece
    gameBoardMovePiece posO posD boardPiecesOrig boardPiecesDest
    -- Refresh the UI
    boardInvalidate board

-- boardRefresh board

boardInvalidate :: Ix index => Board index tile piece -> IO ()
boardInvalidate = widgetQueueDraw

boardRefresh :: Ix index => Board index tile piece -> IO ()
boardRefresh board = do
  realized <- widgetGetRealized board
  when realized $ do
    dw <- widgetGetDrawWindow (boardDrawingArea board)

    (w, h) <- widgetGetSize (boardDrawingArea board)

    bg <- readIORef (background board)
    ov <- readIORef (overlay board)

    drawWindowBeginPaintRect dw (Rectangle 0 0 w h)

    -- Clear Drawing area
    drawWindowClear dw
    gc <- gcNew dw
    when (isJust bg) $ do
      ((posBgX, posBgY), bg') <- uncurry (adjustPixbuf (w, h)) (fromJust bg)
      drawPixbuf dw gc bg' 0 0 posBgX posBgY (-1) (-1) RgbDitherNone (-1) (-1)

    -- Dragging status (used to determine what to draw)
    posM <- readIORef (draggingFrom board)
    mpOrig <- readIORef (draggingMouseOrig board)
    mpPos <- readIORef (draggingMousePos board)

    -- Draw tiles
    drawPixmaps dw (tileSize board) (boardTiles board) (tilePixmaps board)

    -- Draw Pieces P1
    piecesBoardP1 <-
      if isJust posM && isJust mpOrig && isJust mpPos
        then do
          pieces' <- (gameBoardClone $ boardPiecesP1 board)
          gameBoardRemovePiece (fromJust posM) pieces'
          return pieces'
        else return $ boardPiecesP1 board

    -- Draw Pieces P1 Queen
    piecesBoardP1Queen <-
      if isJust posM && isJust mpOrig && isJust mpPos
        then do
          pieces' <- (gameBoardClone $ boardPiecesP1Queen board)
          gameBoardRemovePiece (fromJust posM) pieces'
          return pieces'
        else return $ boardPiecesP1Queen board

    -- Draw Pieces P2
    piecesBoardP2 <-
      if isJust posM && isJust mpOrig && isJust mpPos
        then do
          pieces' <- (gameBoardClone $ boardPiecesP2 board)
          gameBoardRemovePiece (fromJust posM) pieces'
          return pieces'
        else return $ boardPiecesP2 board

    -- Draw Pieces P2 Queen
    piecesBoardP2Queen <-
      if isJust posM && isJust mpOrig && isJust mpPos
        then do
          pieces' <- (gameBoardClone $ boardPiecesP2Queen board)
          gameBoardRemovePiece (fromJust posM) pieces'
          return pieces'
        else return $ boardPiecesP2Queen board

    -- drawPixmaps dw (tileSize board) (boardPieces board) (piecePixmaps board)
    drawPixmaps dw (tileSize board) piecesBoardP1 (pieceP1Pixmaps board)
    drawPixmaps dw (tileSize board) piecesBoardP1Queen (pieceP1QueenPixmaps board)
    drawPixmaps dw (tileSize board) piecesBoardP2 (pieceP2Pixmaps board)
    drawPixmaps dw (tileSize board) piecesBoardP2Queen (pieceP2QueenPixmaps board)

    -- Draw moving piece P1
    when (isJust posM && isJust mpOrig && isJust mpPos) $ do
      pieceM <- boardGetPiece (fromJust posM) (boardPiecesP1 board)
      when (isJust pieceM) $ do
        let pb = pieceP1Pixmaps board (fromJust pieceM)
        let (mpPosX, mpPosY) = fromJust mpPos
            (mpOrigX, mpOrigY) = fromJust mpOrig
            (x, y) = (mpPosX - mpOrigX, mpPosY - mpOrigY)
        drawPixbuf dw gc pb 0 0 x y (-1) (-1) RgbDitherNone (-1) (-1)

    -- Draw moving piece P1 Queen
    when (isJust posM && isJust mpOrig && isJust mpPos) $ do
      pieceM <- boardGetPiece (fromJust posM) (boardPiecesP1Queen board)
      when (isJust pieceM) $ do
        let pb = pieceP1QueenPixmaps board (fromJust pieceM)
        let (mpPosX, mpPosY) = fromJust mpPos
            (mpOrigX, mpOrigY) = fromJust mpOrig
            (x, y) = (mpPosX - mpOrigX, mpPosY - mpOrigY)
        drawPixbuf dw gc pb 0 0 x y (-1) (-1) RgbDitherNone (-1) (-1)

    -- Draw moving piece P2
    when (isJust posM && isJust mpOrig && isJust mpPos) $ do
      pieceM <- boardGetPiece (fromJust posM) (boardPiecesP2 board)
      when (isJust pieceM) $ do
        let pb = pieceP2Pixmaps board (fromJust pieceM)
        let (mpPosX, mpPosY) = fromJust mpPos
            (mpOrigX, mpOrigY) = fromJust mpOrig
            (x, y) = (mpPosX - mpOrigX, mpPosY - mpOrigY)
        drawPixbuf dw gc pb 0 0 x y (-1) (-1) RgbDitherNone (-1) (-1)

    -- Draw moving piece P2 Queen
    when (isJust posM && isJust mpOrig && isJust mpPos) $ do
      pieceM <- boardGetPiece (fromJust posM) (boardPiecesP2Queen board)
      when (isJust pieceM) $ do
        let pb = pieceP2QueenPixmaps board (fromJust pieceM)
        let (mpPosX, mpPosY) = fromJust mpPos
            (mpOrigX, mpOrigY) = fromJust mpOrig
            (x, y) = (mpPosX - mpOrigX, mpPosY - mpOrigY)
        drawPixbuf dw gc pb 0 0 x y (-1) (-1) RgbDitherNone (-1) (-1)

    when (isJust ov) $ do
      ((posOvX, posOvY), ov') <- uncurry (adjustPixbuf (w, h)) (fromJust ov)
      drawPixbuf dw gc ov' 0 0 posOvX posOvY (-1) (-1) RgbDitherNone (-1) (-1)

    drawWindowEndPaint dw

-- FIXME: To be completed
adjustPixbuf :: (Int, Int) -> Pixbuf -> SizeAdjustment -> IO ((Int, Int), Pixbuf)
adjustPixbuf _ pb _ = return ((0, 0), pb)

mouseMotionHandler :: Ix index => Board index tile piece -> ((index, index) -> EventM EMotion Bool) -> EventM EMotion Bool
mouseMotionHandler board p = do
  coords <- eventCoordinates
  pos <- liftIO $ getMouseCoordinates board coords
  maybe (return False) p pos

mouseButtonHandler :: Ix index => Board index tile piece -> ((index, index) -> EventM EButton Bool) -> EventM EButton Bool
mouseButtonHandler board p = do
  coords <- eventCoordinates
  pos <- liftIO $ getMouseCoordinates board coords
  maybe (return False) p pos

getMouseCoordinates :: Ix index => Board index tile piece -> (Double, Double) -> IO (Maybe (index, index))
getMouseCoordinates board (x, y) = do
  let (tileW, tileH) = tileSize board
      tileCol = round x `div` tileW
      tileRow = round y `div` tileH
  let (GameBoard array) = boardTiles board
  ((xm, ym), (xM, yM)) <- getBounds $ array
  let xs = range (xm, xM)
      ys = range (ym, yM)
  if (inRange (0, length xs - 1) tileCol && inRange (0, length ys - 1) tileRow)
    then return $ Just ((head (drop tileCol xs)), (head (drop tileRow ys)))
    else return Nothing

clickHandler :: Ix index => Board index tile piece -> ((index, index) -> IO ()) -> EventM EButton Bool
clickHandler board p = do
  (x, y) <- eventCoordinates
  liftIO $ do
    let (tileW, tileH) = tileSize board
        tileCol = round x `div` tileW
        tileRow = round y `div` tileH
    let (GameBoard array) = boardTiles board
    ((xm, ym), (xM, yM)) <- getBounds $ array
    let xs = range (xm, xM)
        ys = range (ym, yM)
    when (inRange (0, length xs - 1) tileCol && inRange (0, length ys - 1) tileRow) $
      p ((head (drop tileCol xs)), (head (drop tileRow ys)))
    return False

boardGetPixelSize :: Ix index => Board index tile piece -> IO (Int, Int)
boardGetPixelSize board = do
  let (GameBoard array) = boardTiles board

  ((xm, ym), (xM, yM)) <- getBounds $ array
  let htiles = rangeSize (xm, xM)
      vtiles = rangeSize (ym, yM)
      (tileW, tileH) = tileSize board

  return (htiles * tileW, vtiles * tileH)

drawPixmaps :: (Ix index, DrawableClass d) => d -> (Int, Int) -> GameBoard index e -> PixmapsFor e -> IO ()
drawPixmaps d tileSize@(tw, th) gameBoard@(GameBoard array) pixmaps = do
  gc <- gcNew d
  ((xm, ym), (xM, yM)) <- gameBoardGetBoundaries gameBoard

  let paintPixmap (x, y) elem = do
        let pixmap = pixmaps elem
            ix = index (xm, xM) x
            iy = index (ym, yM) y
            posX = ix * tw
            posY = iy * th
        drawPixbuf d gc pixmap 0 0 posX posY (-1) (-1) RgbDitherNone (-1) (-1)

  gameBoardMapM_ gameBoard paintPixmap

boardFoldM :: (Ix index) => GameBoard index piece -> (b -> ((index, index), piece) -> IO b) -> b -> IO b
boardFoldM boardPieces f def = gameBoardFoldM boardPieces f def

boardClear :: Ix index => Board index tile piece -> IO ()
boardClear board = do
  gameBoardClear (boardPiecesP1 board)
  gameBoardClear (boardPiecesP1Queen board)
  gameBoardClear (boardPiecesP2 board)
  gameBoardClear (boardPiecesP2Queen board)
  boardInvalidate board

boardOnClick :: Ix index => Board index tile piece -> ((index, index) -> IO ()) -> IO ()
boardOnClick board p = boardOnPress board (\c -> liftIO (p c) >> return False)

boardOnPress :: Ix index => Board index tile piece -> ((index, index) -> EventM EButton Bool) -> IO ()
boardOnPress board f = void $ do
  widgetAddEvents (boardDrawingArea board) [ButtonPressMask]
  (boardDrawingArea board) `on` buttonPressEvent $ mouseButtonHandler board f

boardOnRelease :: Ix index => Board index tile piece -> ((index, index) -> EventM EButton Bool) -> IO ()
boardOnRelease board f = void $ do
  widgetAddEvents (boardDrawingArea board) [ButtonPressMask, ButtonReleaseMask]
  (boardDrawingArea board) `on` buttonReleaseEvent $ mouseButtonHandler board f

boardOnMotion :: Ix index => Board index tile piece -> ((index, index) -> EventM EMotion Bool) -> IO ()
boardOnMotion board f = void $ do
  widgetAddEvents board [PointerMotionMask]
  (boardDrawingArea board) `on` motionNotifyEvent $ mouseMotionHandler board f

boardSetBackground :: Ix index => Board index tile piece -> Maybe (Pixbuf, SizeAdjustment) -> IO ()
boardSetBackground board bg = do
  writeIORef (background board) bg
  boardInvalidate board

boardSetOverlay :: Ix index => Board index tile piece -> Maybe (Pixbuf, SizeAdjustment) -> IO ()
boardSetOverlay board bg = do
  writeIORef (overlay board) bg
  boardInvalidate board

boardEnableDrag :: Ix index => Board index tile piece -> IO ()
boardEnableDrag board = writeIORef (dragEnabled board) True

boardDisableDrag :: Ix index => Board index tile piece -> IO ()
boardDisableDrag board = do
  writeIORef (dragEnabled board) False
  boardInvalidate board

boardStartDrag :: Ix index => Board index tile piece -> GameBoard index piece -> (index, index) -> IO ()
boardStartDrag board boardPieces ix@(i, j) = do
  writeIORef (draggingFrom board) (Just ix)
  ((xm, ym), (xM, yM)) <- gameBoardGetBoundaries $ boardPieces
  let (w, h) = tileSize board
      x = round ((0.5 + fromIntegral (rangeSize (xm, i))) * fromIntegral w)
      y = round ((0.5 + fromIntegral (rangeSize (ym, j))) * fromIntegral h)
  writeIORef (draggingMouseOrig board) (Just (x, y))

boardStopDrag :: Ix index => Board index tile piece -> IO ()
boardStopDrag board = do
  writeIORef (draggingFrom board) Nothing
  writeIORef (draggingTo board) Nothing
  boardInvalidate board

boardOnPieceDragStart :: Ix index => Board index tile piece -> ((index, index) -> IO Bool) -> IO ()
boardOnPieceDragStart board f = boardOnPress board $ \ix -> do
  (x, y) <- eventCoordinates
  returning False $
    liftIO $ do
      drag <- readIORef (dragEnabled board)
      when drag $ do
        canDragThis <- f ix
        let from = if canDragThis then Just ix else Nothing
            orig = if canDragThis then Just (relativePos board ix (round x, round y)) else Nothing
        writeIORef (draggingFrom board) from
        writeIORef (draggingMouseOrig board) orig
        boardInvalidate board

boardOnPieceDragOver :: Ix index => Board index tile piece -> ((index, index) -> (index, index) -> IO Bool) -> IO ()
boardOnPieceDragOver board f = boardOnMotion board $ \ix -> do
  (x, y) <- eventCoordinates
  returning False $
    liftIO $ do
      drag <- readIORef (dragEnabled board)
      origM <- readIORef (draggingFrom board)
      when (drag && isJust origM) $ do
        canDropHere <- f (fromJust origM) ix
        let newDest = if canDropHere then Just ix else Nothing
        writeIORef (draggingTo board) newDest
        writeIORef (draggingMousePos board) (Just (round x, round y))
      boardInvalidate board

boardOnPieceDragDrop :: Ix index => Board index tile piece -> ((index, index) -> (index, index) -> IO ()) -> IO ()
boardOnPieceDragDrop board f = void $ do
  widgetAddEvents (boardDrawingArea board) [ButtonPressMask, ButtonReleaseMask]
  (boardDrawingArea board) `on` buttonReleaseEvent $
    returning False $
      liftIO $ do
        drag <- readIORef (dragEnabled board)
        origM <- readIORef (draggingFrom board)
        destM <- readIORef (draggingTo board)
        let notSame = origM /= destM
        when (drag && isJust origM) $ do
          -- No longer dragging
          writeIORef (draggingFrom board) Nothing
          writeIORef (draggingTo board) Nothing
          writeIORef (draggingMouseOrig board) Nothing
          writeIORef (draggingMousePos board) Nothing

          -- When possible, call the handler
          when (isJust destM && notSame) $ f (fromJust origM) (fromJust destM)

          -- In any case, the board must be repainted
          boardInvalidate board

boardIsDragging :: Ix index => Board index tile piece -> IO Bool
boardIsDragging = fmap isJust . readIORef . draggingFrom

relativePos :: Ix index => Board index tile piece -> (index, index) -> (Int, Int) -> (Int, Int)
relativePos board (ix, iy) (x, y) = (x', y')
  where
    (w, h) = tileSize board
    x' = x `mod` w
    y' = y `mod` h

returning :: Monad m => a -> m b -> m a
returning v f = f >> return v
