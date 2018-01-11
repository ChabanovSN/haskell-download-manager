module HDMWindow
  ( makeWindow
  ) where

import Control.Monad.IO.Class
import Download
import DownloadWindow
import Graphics.UI.Gtk
import Settings

makeWindow :: IO Window
makeWindow = do
  w <- window
  box <- vBoxNew False 0
  let curDL =
        Download
        { name = "THING"
        , link = "THING"
        , location = "THING"
        , size = 100
        , completed = 50
        }
  (s, dlUpdate) <- scroller curDL
  m <- makeMenu (\downloadLink -> dlUpdate $ curDL {name = downloadLink})
  boxPackStart box m PackNatural 0
  boxPackStart box s PackNatural 0
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
    scroller dl = do
      w <- scrolledWindowNew Nothing Nothing
      (dlDisplay, dlUpdater) <- showDownload dl
      containerAdd w dlDisplay
      return (w, dlUpdater)

makeMenu :: (String -> IO a) -> IO Toolbar
makeMenu update = do
  m <- toolbarNew
  let insert = flip (toolbarInsert m) (-1)
  makeSettingsButton >>= insert
  makeDownloadButton update >>= insert
  return m

makeDownloadButton :: (String -> IO a) -> IO ToolItem
makeDownloadButton update =
  toolItemFrom $
  makeButton "New Download" $ newDownloadWindow update >>= widgetShowAll

makeSettingsButton :: IO ToolItem
makeSettingsButton =
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
