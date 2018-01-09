module HDMWindow
  ( makeWindow
  ) where

import Control.Monad.IO.Class
import DownloadWindow
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
      set
        w
        [ windowTitle := "Download Manager"
        , windowResizable := True
        , windowDestroyWithParent := True
        ]
      on w deleteEvent $ do
        liftIO mainQuit
        return False
      return w
    scroller = do
      w <- scrolledWindowNew Nothing Nothing
      vBoxNew False 0 >>= containerAdd w
      return w

makeMenu :: IO Toolbar
makeMenu = do
  m <- toolbarNew
  let insert i = flip (toolbarInsert m) i
  makeFileButton >>= insert 0
  makeEditButton >>= insert 1
  return m

makeEditButton :: IO ToolItem
makeEditButton =
  toolItemFrom $ makeButton "New Download" $ newDownloadWindow >>= widgetShowAll

makeFileButton :: IO ToolItem
makeFileButton =
  toolItemFrom $ makeButton "Settings" $ settingsWindow >>= widgetShowAll

makeButton :: String -> IO () -> IO Button
makeButton label action = do
  b <- buttonNewWithLabel label
  on b buttonActivated action
  return b

toolItemFrom :: WidgetClass widget => IO widget -> IO ToolItem
toolItemFrom child = do
  item <- toolItemNew
  child >>= containerAdd item
  return item
