module Settings
  ( settingsWindow
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Data.List.Split
import Graphics.UI.Gtk

-- TODO: Make this component prettier, improve focus handling
newtype HDMSettings = HDMSettings
  { downloadDirectory :: String
  } deriving (Show)

settingsWindow :: IO Window
settingsWindow =
  windowNew >>= \w -> do
    set
      w
      [ windowTitle := "Settings"
      , windowResizable := False
      , windowModal := True
      , windowDestroyWithParent := True
      ]
    fromFile <- getSettings "./settings"
    state <- newIORef fromFile
    _ <-
      after w deleteEvent $ do
        liftIO $ readIORef state >>= \s -> writeSettingsToFile s "./settings"
        return False
    downloadDirectoryChooser state >>= containerAdd w
    return w

downloadDirectoryChooser :: IORef HDMSettings -> IO HBox
downloadDirectoryChooser state =
  hBoxNew False 0 >>= \b -> do
    let pack a = boxPackStart b a PackNatural 0
    labelNew (Just "Download directory: ") >>= pack
    fileChooserButtonNew "Download directory" FileChooserActionSelectFolder >>= \fb -> do
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
      pack fb
    return b

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
