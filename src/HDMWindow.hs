module HDMWindow
  ( makeWindow
  ) where

import Control.Monad.IO.Class
import Download
import Graphics.UI.Gtk
import Settings

makeWindow :: IO Window
makeWindow = do
  w <- window
  box <- vBoxNew False 0
  makeMenu >>= \m -> boxPackStart box m PackNatural 0
  scroller >>= \s -> boxPackStart box s PackGrow 0
  containerAdd w box
  return w
  where
    window = do
      w <- windowNew
      set w [windowTitle := "Download Manager", windowResizable := True]
      on w deleteEvent $ do
        liftIO mainQuit
        return False
      return w
    scroller = do
      w <- scrolledWindowNew Nothing Nothing
      vBoxNew False 0 >>= containerAdd w
      return w

makeMenu :: IO MenuBar
makeMenu = do
  m <- menuBarNew
  makeFileButton >>= menuShellAppend m
  makeEditButton >>= menuShellAppend m
  return m

makeEditButton :: IO MenuItem
makeEditButton = do
  edit <- menuItemNewWithLabel "Edit"
  subMenu >>= menuItemSetSubmenu edit
  return edit
  where
    subMenu = do
      sub <- menuNew
      addFile <- menuItemNewWithLabel "New Download"
      menuAttach sub addFile 0 1 0 1
      return sub

makeFileButton :: IO MenuItem
makeFileButton = do
  files <- menuItemNewWithLabel "File"
  subMenu >>= menuItemSetSubmenu files
  return files
  where
    subMenu = do
      sub <- menuNew
      settings <- menuItemNewWithLabel "Settings"
      on settings menuItemActivated $ settingsWindow >>= widgetShowAll
      menuAttach sub settings 0 1 0 1
      return sub
