module HDMWindow
  ( makeWindow
  ) where

import Control.Monad.IO.Class
import Graphics.UI.Gtk

makeWindow :: IO Window
makeWindow = do
  window <- windowNew
  menu <- makeMenu
  box <- vBoxNew False 0
  downloads <- scrolledWindowNew Nothing Nothing
  containerAdd window box
  boxPackStart box menu PackNatural 0
  boxPackEnd box downloads PackNatural 0
  set window [windowTitle := "Download Manager", windowResizable := True]
  _ <-
    on window deleteEvent $ do
      liftIO mainQuit
      return False
  return window

makeMenu :: IO MenuBar
makeMenu = do
  menuBar <- menuBarNew
  filesButton <- menuItemNewWithLabel "File"
  editButton <- menuItemNewWithLabel "Edit"
  menuShellAppend menuBar filesButton
  menuShellAppend menuBar editButton
  return menuBar
