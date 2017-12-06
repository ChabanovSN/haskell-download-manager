module HDMWindow
  ( makeWindow
  ) where

import Control.Monad.IO.Class
import Download
import Graphics.UI.Gtk
import Settings

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
      sWindow <- scrolledWindowNew Nothing Nothing
      boxPackStart box sWindow PackGrow 0
      downloadContainer <- vBoxNew False 0
      containerAdd sWindow downloadContainer
      mapM_
        (\dl -> do
           dl2 <- dl
           boxPackStart downloadContainer dl2 PackNatural 0)
        testShow
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
        _ <- on settings menuItemActivated $ settingsWindow >>= widgetShowAll
        menuAttach sub settings 0 1 0 1
    return files
