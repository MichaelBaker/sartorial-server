module Protocol where

data Request = AddPlayerRequest  { addPlayerName :: String }
             | PlayerListRequest
             | ChatMessage       { chatPlayer :: String, chatMessage :: String }
             deriving (Show, Read, Eq)

data Response = SuccessResponse
              | ErrorResponse String
              | PlayerListResponse [String]
              | InvalidRequestResponse
              deriving (Show, Read, Eq)
