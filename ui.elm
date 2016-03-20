module UI where

import Domain exposing (..)
import Time exposing (Time)

type Action = DrawCardsToTrade Player
            | SellField Player Index
            | PlantFromHand Player
            | DrawCardsToHand Player
            | KeepFromTrade Player Index
            | PlantFromSide Player Index
            | TradeFromHand Player Index Player
            | Trade Player Index Player
            | GetTime Time
