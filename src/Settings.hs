module Settings
  ( settingsWindow
  ) where

import Control.Exception 
import Data.IORef
import Data.List.Split
import Graphics.UI.Gtk
import System.IO.Error

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
  buttonsBox state w >>= \buttons -> boxPackEnd vb buttons PackNatural 8
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
    buttonsBox state w = do
      bBox <- hButtonBoxNew
      confirm <- buttonNewWithLabel "Confirm"
      _ <-
        on confirm buttonActivated $ do
          widgetHide w
          saveSettings state
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
        fileChooserButtonNew "Download directory" FileChooserActionSelectFolder
      readIORef state >>= fileChooserSetFilename fb . downloadDirectory
      on fb fileChooserButtonFileSet $ do
        fp <- fileChooserGetFilename fb
        atomicModifyIORef' state $ \cur ->
          let r =
                case fp of
                  Just path -> HDMSettings path
                  _ -> cur
          in (r, r)
        return ()
      return fb
    attach t col child = tableAttach t child col (col + 1) 0 1 [] [] 16 16

getSettings :: String -> IO HDMSettings
getSettings filePath = do
  res <- try (readFile filePath) :: IO (Either IOError String)
  case res of
    Left err -> do 
      putStrLn $ "Error retrieving settings: " ++ (ioeGetErrorString err)
      return $ toHDMSettings "" 
    Right path -> return $ toHDMSettings path

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
