{-# LANGUAGE MultiParamTypeClasses #-}

module GtkCheckers where

import Checkers
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.BoardLink
import Graphics.UI.Gtk.Board.TiledBoard

gtkGame :: IO (Game CheckersGame Int Tile Player Piece)
gtkGame = do
  -- The images used for tiles and pieces
  tile <- pixbufNewFromFile "assets/brow-tile.png"
  pieceP1 <- pixbufNewFromFile "assets/white-piece.png"
  pieceQueenP1 <- pixbufNewFromFile "assets/white-queen-piece.png"
  pieceP2 <- pixbufNewFromFile "assets/black-piece.png"
  pieceQueenP2 <- pixbufNewFromFile "assets/black-queen-piece.png"
  pb <- pixbufNewFromFile "assets/board-background.png"

  let game = Game visualAspects defaultCheckersGame
      visualAspects =
        VisualGameAspects
          { tileF = \_ -> tile,
            pieceA = \_ -> pieceP1,
            pieceB = \_ -> pieceP2,
            bgColor = (65000, 50000, 50000),
            bg = Just (pb, SizeAdjustment)
          }
  return game
