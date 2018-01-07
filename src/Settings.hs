module Settings
  ( settingsWindow
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Data.List.Split
import Graphics.UI.Gtk

-- import Graphics.UI.Gtk.General.Enums
-- TODO: Make this component prettier, improve focus handling
newtype HDMSettings = HDMSettings
  { downloadDirectory :: String
  } deriving (Show)

settingsWindow :: IO Window
settingsWindow = do
  w <- windowNew
  state <- getSettings "./settings" >>= newIORef
  set w
    [ windowTitle := "Settings"
    , windowResizable := False
    , windowModal := True
    , windowDestroyWithParent := True
    ]
  -- _ <-
  --   after w deleteEvent $ do
  --     liftIO $ saveSettings state
  --     return False
  vb <- vBoxNew False 16
  downloadDirectoryChooser state >>= \dc -> boxPackStart vb dc PackNatural 8
  buttons <-
    hButtonBoxNew >>= \bBox -> do
      buttonBoxSetLayout bBox ButtonboxEnd
      confirm <- buttonNewWithLabel "Confirm"
      _ <-
        on confirm buttonActivated $ do
          liftIO $ saveSettings state
          return ()
      boxPackEnd bBox confirm PackNatural 16
      return bBox
  boxPackEnd vb buttons PackNatural 8
  containerAdd w vb
  return w

saveSettings :: IORef HDMSettings -> IO ()
saveSettings state = readIORef state >>= (`writeSettingsToFile` "./settings")

downloadDirectoryChooser :: IORef HDMSettings -> IO Table
downloadDirectoryChooser state = do
  t <- tableNew 1 2 False
  let attach col child = tableAttach t child col (col + 1) 0 1 [] [] 16 16
  labelNew (Just "Download directory: ") >>= attach 0
  fb <- fileChooserButtonNew "Download directory" FileChooserActionSelectFolder
  _ <- readIORef state >>= fileChooserSetFilename fb . downloadDirectory
  _ <-
    on fb fileChooserButtonFileSet $
    fileChooserGetFilename fb >>= \fp ->
      case fp of
        Just path -> do
          _ <-
            atomicModifyIORef state $ \_ ->
              let r = HDMSettings path
              in (r, r)
          return ()
        Nothing -> return ()
  attach 1 fb
  return t

getSettings :: String -> IO HDMSettings
getSettings filePath = do
  res <- try (readFile filePath) :: IO (Either IOError String)
  return $
    toHDMSettings
      (case res of
         Left _ -> ""
         Right path -> path)

toHDMSettings :: String -> HDMSettings
toHDMSettings a = HDMSettings {downloadDirectory = extract "dir" params}
  where
    extract _ [] = ""
    extract name groups =
      case filter (\d -> fst d == name) groups of
        [] -> ""
        l -> snd $ head l
    toPair l
      | length l <= 1 = ("", "")
      | otherwise = (head l, l !! 1)
    params = [toPair t | t <- [splitOn "::" a]]

writeSettingsToFile :: HDMSettings -> FilePath -> IO ()
writeSettingsToFile settings filePath =
  writeFile filePath $ "dir::" ++ downloadDirectory settings
