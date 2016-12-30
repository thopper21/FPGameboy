module Main where

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)

main = do
   initGUI
   window <- windowNew
   window `on` deleteEvent $ liftIO mainQuit >> return False
   widgetShowAll window
   mainGUI