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
  piecePA <- pixbufNewFromFile "assets/black-piece.png"
  piecePB <- pixbufNewFromFile "assets/white-piece.png"
  pb <- pixbufNewFromFile "assets/board-background.png"

  let game = Game visualAspects defaultCheckersGame
      visualAspects =
        VisualGameAspects
          { tileF = \_ -> tile,
            pieceA = \_ -> piecePA,
            pieceB = \_ -> piecePB,
            bgColor = (65000, 50000, 50000),
            bg = Just (pb, SizeAdjustment)
          }
  return game
