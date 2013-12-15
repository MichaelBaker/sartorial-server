module Protocol where

data Request = AddPlayerRequest   { addPlayerName :: String }
             | PlayerListRequest
             | ChatMessageRequest { chatPlayer :: String, chatMessage :: String }
             | ChatUpdatesRequest { chatPlayer :: String }
             deriving (Show, Read, Eq)

data Response = SuccessResponse
              | ErrorResponse String
              | PlayerListResponse [String]
              | ChatUpdatesResponse [String]
              | InvalidRequestResponse
              deriving (Show, Read, Eq)
