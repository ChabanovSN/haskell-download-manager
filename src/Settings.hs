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
  w <- window
  state <- getState
  vb <- vBoxNew False 16
  downloadDirectoryChooser state >>= \dc -> boxPackStart vb dc PackNatural 8
  buttonsBox state >>= \buttons -> boxPackEnd vb buttons PackNatural 8
  containerAdd w vb
  return w
  where
    window = do
      w <- windowNew
      set
        w
        [ windowTitle := "Settings"
        , windowResizable := False
        , windowModal := True
        , windowDestroyWithParent := True
        ]
      return w
    getState = getSettings "./settings" >>= newIORef
    buttonsBox state = do
      bBox <- hButtonBoxNew
      confirm <- buttonNewWithLabel "Confirm"
      _ <-
        on confirm buttonActivated $ do
          liftIO $ saveSettings state
          return ()
      boxPackEnd bBox confirm PackNatural 16
      return bBox

saveSettings :: IORef HDMSettings -> IO ()
saveSettings state = readIORef state >>= (`writeSettingsToFile` "./settings")

downloadDirectoryChooser :: IORef HDMSettings -> IO Table
downloadDirectoryChooser state = do
  t <- tableNew 1 2 False
  labelNew (Just "Download directory: ") >>= attach t 0
  fileButton >>= attach t 1
  return t
  where
    fileButton = do
      fb <-
        fileChooserButtonNew
          "Download directory"
          FileChooserActionSelectFolder
      readIORef state >>= fileChooserSetFilename fb . downloadDirectory
      on fb fileChooserButtonFileSet $
        fileChooserGetFilename fb >>= \fp -> do
          atomicModifyIORef state $ \cur ->
            case fp of
              Just path ->
                let r = HDMSettings path
                in (r, r)
              _ -> (cur, cur)
          return ()
      return fb
    attach t col child = tableAttach t child col (col + 1) 0 1 [] [] 16 16 

getSettings :: String -> IO HDMSettings
getSettings filePath = do
  res <- try (readFile filePath) :: IO (Either IOError String)
  return $
    toHDMSettings $
    case res of
      Left _ -> ""
      Right path -> path

toHDMSettings :: String -> HDMSettings
toHDMSettings a = HDMSettings {downloadDirectory = extract "dir" params}
  where
    extract _ [] = ""
    extract name groups =
      case filter (\d -> fst d == name) groups of
        [] -> ""
        l -> snd $ head l
    params = map toPair [splitOn "::" a]
    toPair l
      | length l <= 1 = ("", "")
      | otherwise = (head l, l !! 1)

writeSettingsToFile :: HDMSettings -> FilePath -> IO ()
writeSettingsToFile settings filePath =
  writeFile filePath $ "dir::" ++ downloadDirectory settings
