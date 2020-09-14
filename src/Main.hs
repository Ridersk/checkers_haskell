{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.BoardLink
import Graphics.UI.Gtk.Layout.BackgroundContainer
import GtkCheckers

main :: IO ()
main = do
  -- Initialize Gtk
  _ <- initGUI

  -- Create interface
  window <- windowNew
  bgBin <- backgroundContainerNewWithPicture "assets/table-background.jpg"
  align <- alignmentNew 0.5 0.5 0 0

  -- Create game and board
  game <- gtkGame
  board <- attachGameRules game

  -- Add hierarchy of widgets to window
  containerAdd align board
  containerAdd bgBin align
  containerAdd window bgBin

  -- Set window size
  widgetSetSizeRequest window 800 600

  -- Close progrtam if window is closed
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Launch program with the main window
  widgetShowAll window
  mainGUI
