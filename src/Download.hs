module Download
  ( showDownload
  , Download(..)
  ) where

import Data.List
import Graphics.UI.Gtk

data Download = Download
  { name :: String
  , link :: String
  , location :: String
  , size :: Integer
  , completed :: Integer
  } deriving (Show)

showDownloads :: [Download] -> IO Grid
showDownloads downloads = gridNew >>= \grid -> addDownloads grid downloads 0

addDownloads :: Grid -> [Download] -> Int -> IO Grid
addDownloads grid [] _ = return grid
addDownloads grid (d:ds) index = do
  p <- progressBarNew
  progressBarSetFraction p progress
  gridAttach grid p 2 index 9 (index + 1)
  labelNew (Just $ name d) >>= \l -> gridAttach grid l 0 index 2 (index + 1)
  labelNew (Just completion) >>= \l -> gridAttach grid l 9 index 10 (index + 1)
  addDownloads grid ds (index + 1)
  where
    completion =
      (sizeToString . completed $ d) ++ "/" ++ (sizeToString . size $ d)
    progress = fromIntegral (completed d) / fromIntegral (size d)

showDownload :: Download -> IO (VBox, Download -> IO ())
showDownload download = do
  box <- vBoxNew True 16
  labels <- hBoxNew False 16
  nameLabel <- labelNew $ Just $ name download
  boxPackStart labels nameLabel PackNatural 8
  completionLabel <- labelNew $ Just $ completion download
  boxPackEnd labels completionLabel PackNatural 8
  boxPackStart box labels PackNatural 0
  pBar <- progressBarNew
  boxPackStart box pBar PackNatural 0
  progressBarSetFraction pBar $ progress download
  hSeparatorNew >>= \sep -> boxPackEnd box sep PackNatural 0
  return
    ( box
    , \newDL -> do
        labelSetText nameLabel $ name newDL
        labelSetText completionLabel $ completion newDL
        progressBarSetFraction pBar $ progress newDL)
  where
    completion dl = 
      sizeToString (completed dl) ++ "/" ++ sizeToString (size dl)
    progress dl = fromIntegral (completed dl) / fromIntegral (size dl)

sizeToString :: Integer -> String
sizeToString s =
  show ((fromIntegral s / fromInteger (fst getSizer)) :: Double) ++ snd getSizer
  where
    getSizer
      | s >= 1000000000 = (1000000000, "GB")
      | s >= 1000000 = (1000000, "MB")
      | s >= 1000 = (1000, "KB")
      | otherwise = (1, "B")
