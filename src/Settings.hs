module Settings
  ( settingsWindow
  ) where

import Data.List.Split
import Graphics.UI.Gtk

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
    downloadDirectoryChooser >>= containerAdd w
    return w

downloadDirectoryChooser :: IO HBox
downloadDirectoryChooser =
  hBoxNew False 0 >>= \b -> do
    let pack a = boxPackStart b a PackNatural 0
    labelNew (Just "Download directory: ") >>= pack
    fileChooserButtonNew "Download directory" FileChooserActionSelectFolder >>= \fb -> do
      _ <-
        getSettings "./settings" >>=
        fileChooserSetFilename fb . downloadDirectory
      _ <-
        on fb fileChooserButtonFileSet $
        fileChooserGetFilename fb >>= \fp ->
          case fp of
            Just path ->
              writeFile "./settings" $ writeSettings $ HDMSettings path
            Nothing -> print "nothing"
      pack fb
    return b

getSettings :: String -> IO HDMSettings
getSettings = fmap toHDMSettings . readFile

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

writeSettings :: HDMSettings -> String
writeSettings s = "dir::" ++ downloadDirectory s ++ "\n"
