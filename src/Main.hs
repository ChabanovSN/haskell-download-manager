module Main where

import Control.Monad
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import HDMWindow

main :: IO ()
main = do
  void initGUI
  window <- makeWindow
  widgetShowAll window
  mainGUI
