module UI exposing (..)

import Domain exposing (Index, Player)
import Time exposing (Posix)


type Msg
    = GetTime Posix
    | PlayerAction PlayerAction


type PlayerAction
    = DrawCardsToTrade Player
    | SellField Player Index
    | PlantFromHand Player
    | DrawCardsToHand Player
    | KeepFromTrade Player Index
    | PlantFromSide Player Index
    | TradeFromHand Player Index Player
    | Trade Player Index Player
