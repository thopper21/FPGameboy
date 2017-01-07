module Main where

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo
import Processor
import Memory
import Register

white = Color 65535 65535 65535

defaultWidth = 300
defaultHeight = 300

main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "FPGameboy",
                 windowDefaultWidth := truncate defaultWidth, windowDefaultHeight := truncate defaultHeight]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas
     widgetModifyBg canvas StateNormal white

     widgetShowAll window 
     drawWin <- widgetGetDrawWindow canvas
     onExpose canvas $ handleExpose drawWin
    
     onDestroy window mainQuit
     mainGUI

handleExpose drawWin x = do
   renderWithDrawable drawWin $ render defaultWidth defaultHeight
   return False

render width height =
   let
      renderFunc = uncurry $ renderPixel width height
      pixels = [(x, y) | x <- [0..width], y <- [0..height]]
   in
      mapM renderFunc pixels
  
renderPixel width height x y = do
   Cairo.setSourceRGB r g b
   Cairo.rectangle x y 1 1
   Cairo.fill
   where (r, g, b) = getPixel x y width height
      

getPixel x y width height =
   let
      red = x / width
      green = y / height
   in
      (red, green, 0)