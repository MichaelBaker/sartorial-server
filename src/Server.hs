{-# LANGUAGE OverloadedStrings #-}

module Server (gameServer, handleCommand, GameState (..)) where

import Web.Scotty                  (get, post, body, text, file)
import Data.Text.Lazy              (pack)
import Data.ByteString.Lazy.Char8  (unpack)
import Safe                        (readMay)
import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Monad.IO.Class      (liftIO)
import Protocol                    (Request (..), Response (..))
import Sartorial.World             (World (), freshWorld, placePlayerInRoom)
import Sartorial.Room              (Room ())
import Sartorial.Player            (Player (..), freshPlayer)

import qualified Data.Map as M

data GameState = GameState { startingRoom :: Room
                           , world        :: World
                           , players      :: M.Map String Player
                           , chats        :: M.Map String [String]
                           }

gameServer gameState = do
  get "/" $ file "vendor/public/index.html"

  post "/command" $ do
    command <- body
    liftIO $ print command
    response <- liftIO $ atomically $ do
      currentState <- readTVar gameState
      let (newGameState, response) = handleCommand currentState $ bytestringToString command
      writeTVar gameState newGameState
      return response
    text $ stringToText $ show response

handleCommand gameState commandText = case readMay commandText of
                                        Just command -> process gameState command
                                        Nothing      -> (gameState, InvalidRequestResponse)

process gameState PlayerListRequest        = (gameState, PlayerListResponse $ M.keys $ players gameState)

process gameState (AddPlayerRequest name)  = (newGameState, SuccessResponse)
  where newGameState = gameState { players = newPlayers, chats = newChats }
        player       = freshPlayer { playerName = name }
        (newPlayers, newChats, response) = case placePlayerInRoom (world gameState) player (startingRoom gameState) of
                                   Just player -> (M.insert name player $ players gameState, M.insert name [] $ chats gameState, SuccessResponse)
                                   Nothing     -> (players gameState, chats gameState, ErrorResponse "Room does not exist")

process gameState (ChatMessageRequest chatPlayer chatMessage) = (newGameState, SuccessResponse)
  where newGameState       = gameState { chats = newChats }
        newChats           = M.map updateMessages $ chats gameState
        updateMessages old = old ++ [newMessage]
        newMessage         = chatPlayer ++ ": " ++ chatMessage

process gameState (ChatUpdatesRequest chatPlayer) = (newGameState, ChatUpdatesResponse messages)
  where messages     = M.findWithDefault [] chatPlayer (chats gameState)
        newGameState = gameState { chats = M.insert chatPlayer [] (chats gameState) }

process gameState _ = (gameState, InvalidRequestResponse)

bytestringToString = unpack
stringToText       = pack
