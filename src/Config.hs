{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Maybe    (maybe)
import Data.Yaml
import Data.Text     (Text, unpack)
import Control.Monad (void)
import Network.URI
import Types
import Common

instance FromJSON Config where
    parseJSON (Object values) = do
        screenx <- values .: "resolution_x"     
        screeny <- values .: "resolution_y"     
        scoresServer <- values .: "scores_server"  
        return $ Config (screenx, screeny) scoresServer

instance FromJSON URI where
    parseJSON (String rawText) =
        maybe (fail "URL parse error") 
              return (parseURI . unpack $ rawText)

getConfig :: IO (Either ParseException Config)
getConfig = decodeFileEither "config.yaml"


withConfig :: (Config -> IO ()) -> IO ()
withConfig op = do  
    c <- getConfig
    case c of
        Prelude.Left problem -> print problem
        Prelude.Right config -> op config
