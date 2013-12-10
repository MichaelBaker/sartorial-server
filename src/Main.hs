import Web.Scotty (scotty)
import Server     (gameServer)

main = scotty 8334 gameServer
