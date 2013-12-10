module Protocol where

data Request = Thing
             deriving (Show, Read, Eq)

data Response = Ohai
              | InvalidRequest
              deriving (Show, Read, Eq)

