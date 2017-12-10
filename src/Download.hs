module Download
  ( showDownload
  ) where

import Data.IORef
import Graphics.UI.Gtk

data Download = Download
  { name :: String
  , size :: Integer
  , completed :: Integer
  } deriving (Show)

showDownloads :: [Download] -> IO Grid
showDownloads downloads = gridNew >>= \grid -> addDownloads grid downloads 0

addDownloads :: Grid -> [Download] -> Int -> IO Grid
addDownloads grid [] _ = return grid
addDownloads grid (d:ds) index = do
  progress <- progressBarNew
  progressBarSetFraction progress $
    fromIntegral (completed d) / fromIntegral (size d)
  l <- labelNew (Just $ name d)
  l2 <-
    labelNew
      (Just $ (sizeToString . completed $ d) ++ "/" ++ (sizeToString . size $ d))
  gridAttach grid l 0 index 2 (index + 1)
  gridAttach grid progress 2 index 9 (index + 1)
  gridAttach grid l2 9 index 10 (index + 1)
  addDownloads grid ds (index + 1)

showDownload :: Download -> IO VBox
showDownload download =
  vBoxNew True 16 >>= \box -> do
    hBoxNew False 16 >>= \labels -> do
      labelNew (Just $ name download) >>= \dlName ->
        boxPackStart labels dlName PackNatural 8
      labelNew
        (Just $
         (sizeToString . completed $ download) ++
         "/" ++ (sizeToString . size $ download)) >>= \progress ->
        boxPackEnd labels progress PackNatural 8
      boxPackStart box labels PackNatural 0
    progressBarNew >>= \pBar -> do
      boxPackStart box pBar PackNatural 0
      progressBarSetFraction pBar $
        fromIntegral (completed download) / fromIntegral (size download)
    hSeparatorNew >>= \sep -> boxPackEnd box sep PackNatural 0
    return box

sizeToString :: Integral a => a -> String
sizeToString s =
  show ((fromIntegral s / fromInteger (fst getSizer)) :: Double) ++ snd getSizer
  where
    getSizer
      | s >= 10 ^ 9 = (10 ^ 9, "GB")
      | s >= 10 ^ 6 = (10 ^ 6, "MB")
      | s >= 10 ^ 3 = (10 ^ 3, "KB")
      | otherwise = (1, "B")
