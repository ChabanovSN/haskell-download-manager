module HDMWindow
  ( makeWindow
  ) where

import Control.Monad.IO.Class
import Download
import Graphics.UI.Gtk
import Settings

makeWindow :: IO Window
makeWindow =do
  w <-windowNew 
  set w [windowTitle := "Download Manager", windowResizable := True]
  box <- vBoxNew False 0
  _ <-
    on w deleteEvent $ do
      liftIO mainQuit
      return False
  makeMenu >>= \m->boxPackStart box m PackNatural 0
  sWindow <- scrolledWindowNew Nothing Nothing
  boxPackStart box sWindow PackGrow 0
  vBoxNew False 0 >>= containerAdd sWindow 
  -- mapM_
  --   (\dl -> do 
  --      dl2 <- dl
  --      boxPackStart downloadContainer dl2 PackNatural 0)
  --   testShow
  containerAdd w box
  return w

makeMenu :: IO MenuBar
makeMenu =
  menuBarNew >>= \m -> do
    makeFileButton >>= menuShellAppend m
    menuItemNewWithLabel "Edit" >>= menuShellAppend m
    return m

makeFileButton :: IO MenuItem
makeFileButton = do
  files<-menuItemNewWithLabel "File"
  sub<-menuNew
  menuItemSetSubmenu files sub
  settings <-menuItemNewWithLabel "Settings"
  _ <- on settings menuItemActivated $ settingsWindow >>= widgetShowAll
  menuAttach sub settings 0 1 0 1
  return files
