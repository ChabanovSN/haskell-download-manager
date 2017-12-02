module HDMWindow
  ( makeWindow
  ) where

import Control.Monad.IO.Class
import Graphics.UI.Gtk

makeWindow :: IO Window
makeWindow =
  windowNew >>= \w -> do
    set w [windowTitle := "Download Manager", windowResizable := True]
    vBoxNew False 0 >>= \box -> do
      _ <-
        on w deleteEvent $ do
          liftIO mainQuit
          return False
      let pack a = boxPackStart box a PackNatural 0
      makeMenu >>= pack
      scrolledWindowNew Nothing Nothing >>= pack
      containerAdd w box
    return w

makeMenu :: IO MenuBar
makeMenu =
  menuBarNew >>= \m -> do
    makeFileButton >>= menuShellAppend m
    menuItemNewWithLabel "Edit" >>= menuShellAppend m
    return m

makeFileButton :: IO MenuItem
makeFileButton =
  menuItemNewWithLabel "File" >>= \files -> do
    menuNew >>= \sub -> do
      menuItemSetSubmenu files sub
      menuItemNewWithLabel "Settings" >>= \settings -> do
        _ <- on settings menuItemActivated $ settingsWindow >>= widgetShow
        menuAttach sub settings 0 1 0 1
    return files

settingsWindow :: IO Window
settingsWindow =
  windowNew >>= \w -> do
    set w [windowTitle := "Settings"]
    _ <-
      on w deleteEvent $ do
        liftIO $ putStrLn "Settings Close"
        return False
    return w
