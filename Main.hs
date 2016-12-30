module Main where

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as C

white = Color 65535 65535 65535

defaultWidth = 300

defaultHeight = 300

main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "FPGameboy",
                 windowDefaultWidth := 300, windowDefaultHeight := 300]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas
     widgetModifyBg canvas StateNormal white

     widgetShowAll window 
     drawin <- widgetGetDrawWindow canvas
     onExpose canvas (handleExpose drawin)
    
     onDestroy window mainQuit
     mainGUI

handleExpose drawin x = do
   renderWithDrawable drawin (render defaultWidth defaultHeight)
   return False

render width height =
   let
      renderFunc = uncurry $ renderPixel width height
      pixels = [(x, y) | x <- [0..width], y <- [0..height]]
   in
      mapM renderFunc pixels
  
renderPixel width height x y = do
   C.setSourceRGB r g b
   C.rectangle x y 1 1
   C.fill
   where (r, g, b) = getPixel x y width height
      

getPixel x y width height =
   let
      red = x / width
      green = y / height
   in
      (red, green, 0)