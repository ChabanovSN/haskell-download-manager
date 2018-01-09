module Main where

import Control.Monad
import Graphics.UI.Gtk
import HDMWindow

main :: IO ()
main = do
  void initGUI
  makeWindow >>= widgetShowAll
  mainGUI
