module Download
  ( showDownload
  , testShow
  ) where

import Data.IORef
import Graphics.UI.Gtk

data Download = Download
  { name :: String
  , size :: Integer
  , completed :: Integer
  } deriving (Show)

showDownload :: Download -> IO HBox
showDownload download = do
  progress <- progressBarNew
  progressBarSetFraction progress $
    fromIntegral (completed download) / fromIntegral (size download)
  l <- labelNew (Just $ name download)
  hbox <- hBoxNew False 16
  boxPackStart hbox l PackNatural 0
  boxPackStart hbox progress PackGrow 0
  return hbox

testShow :: [IO HBox]
testShow = do
  let downloads =
        [Download ("Download" ++ show x) 256 (x * 16) | x <- [1 .. 10]]
  -- let outPut = [showDownload dl | dl <- downloads]
  showDownload <$> downloads
