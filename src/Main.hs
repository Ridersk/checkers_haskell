{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.BoardLink (attachGameRules)
import Graphics.UI.Gtk.Layout.BackgroundContainer
  ( backgroundContainerNewWithPicture,
  )
import GtkCheckers (gtkGame)

main :: IO ()
main = do
  -- Inicializa o GTK
  initGUI

  game <- gtkGame
  board <- attachGameRules game

  -- Cria Interface
  window <- windowNew
  set
    window
    [ windowTitle := "Checkers",
      windowDefaultWidth := 800,
      windowDefaultHeight := 600,
      containerBorderWidth := 10
    ]

  background <- backgroundContainerNewWithPicture "assets/table-background.jpg"
  align <- alignmentNew 0.5 0.5 0 0

  -- Adiciona hierarquia de Widgets
  containerAdd window background
  containerAdd background align
  containerAdd align board

  -- Fecha programa se a janela eh fechada
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Inicializa programa na Janela Principal
  widgetShowAll window
  mainGUI
