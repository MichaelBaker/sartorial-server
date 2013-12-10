{-# LANGUAGE OverloadedStrings #-}

module Server (gameServer, handleCommand) where

import Web.Scotty                 (get, body, text)
import Data.Text.Lazy             (pack)
import Data.ByteString.Lazy.Char8 (unpack)
import Safe                       (readMay)
import Protocol                   (Request (..), Response (..))


gameServer = do
  get "/thing" $ do
    command <- body
    text $ stringToText $ show $ handleCommand command

handleCommand commandText = case readMay $ bytestringToString commandText of
                              Just command -> process command
                              Nothing      -> InvalidRequest

process Thing = Ohai

bytestringToString = unpack
stringToText       = pack
