module Player where

data Player = Player { location :: Maybe Int }

freshPlayer = Player { location = Nothing }
