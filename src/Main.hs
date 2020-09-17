{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
  ( alignmentNew,
    containerAdd,
    deleteEvent,
    initGUI,
    mainGUI,
    mainQuit,
    on,
    widgetSetSizeRequest,
    widgetShowAll,
    windowNew,
  )
import Graphics.UI.Gtk.Board.BoardLink (attachGameRules)
import Graphics.UI.Gtk.Layout.BackgroundContainer
  ( backgroundContainerNewWithPicture,
  )
import GtkCheckers (gtkGame)

main :: IO ()
main = do
  -- Inicializa o GTK
  _ <- initGUI

  -- Cria Interface
  window <- windowNew
  background <- backgroundContainerNewWithPicture "assets/table-background.jpg"
  align <- alignmentNew 0.5 0.5 0 0

  -- Cria Tabuleiro
  game <- gtkGame
  board <- attachGameRules game

  -- Adiciona hierarquia de Widgets
  containerAdd align board
  containerAdd background align
  containerAdd window background

  -- Tamanho da Janela
  widgetSetSizeRequest window 800 600

  -- Fecha programa se a janela eh fechada
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Inicializa programa na Janela Principal
  widgetShowAll window
  mainGUI
