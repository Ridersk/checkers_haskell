{-# LANGUAGE MultiParamTypeClasses #-}

module GtkCheckers where

import Checkers
  ( CheckersGame,
    Piece,
    Player,
    Tile,
    defaultCheckersGame,
  )
import Graphics.UI.Gtk (pixbufNewFromFile)
import Graphics.UI.Gtk.Board.BoardLink
  ( Game (Game),
    VisualGameAspects
      ( VisualGameAspects,
        bg,
        bgColor,
        pieceP1,
        pieceP1Queen,
        pieceP2,
        pieceP2Queen,
        tileF
      ),
  )
import Graphics.UI.Gtk.Board.TiledBoard
  ( SizeAdjustment (SizeAdjustment),
  )

gtkGame :: IO (Game CheckersGame Int Tile Player Piece)
gtkGame = do
  -- The images used for tiles and pieces
  tile <- pixbufNewFromFile "assets/brow-tile.png"
  pieceP1Asset <- pixbufNewFromFile "assets/white-piece.png"
  pieceP1QueenAsset <- pixbufNewFromFile "assets/white-queen-piece.png"
  pieceP2Asset <- pixbufNewFromFile "assets/black-piece.png"
  pieceP2QueenAsset <- pixbufNewFromFile "assets/black-queen-piece.png"
  pb <- pixbufNewFromFile "assets/board-background.png"

  let game = Game visualAspects defaultCheckersGame
      visualAspects =
        VisualGameAspects
          { tileF = \_ -> tile,
            pieceP1 = \_ -> pieceP1Asset,
            pieceP1Queen = \_ -> pieceP1QueenAsset,
            pieceP2 = \_ -> pieceP2Asset,
            pieceP2Queen = \_ -> pieceP2QueenAsset,
            bgColor = (65000, 50000, 50000),
            bg = Just (pb, SizeAdjustment)
          }
  return game
