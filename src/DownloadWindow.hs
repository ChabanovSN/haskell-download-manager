{-# LANGUAGE MultiWayIf #-}

module DownloadWindow
  ( streamFromTo
  ) where

import Control.Monad
import qualified Data.ByteString as B
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import System.IO

streamFromTo :: String -> String -> IO Int
streamFromTo url filePath = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest url
  withResponse req manager $ \resp -> do
    let code = statusCode $ responseStatus resp
    putStrLn $ "Status code:" ++ show code
    when (code >= 200 && code < 300) $ streamToFile filePath resp
    return code

streamToFile :: FilePath -> Response BodyReader -> IO ()
streamToFile filePath resp = do
  file <- openFile filePath WriteMode
  loop file 0
  hClose file
  where
    loop file prev = do
      bs <- brRead $ responseBody resp
      let cur = prev + B.length bs
      if | B.null bs -> putStrLn "\nFinished response body"
         | otherwise ->
           do putStrLn $ "\nDownloaded " ++ (show $ cur) ++ " bytes"
              B.hPut file bs
              loop file cur
