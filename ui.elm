module UI where

import Domain exposing (..)

type Action = DrawCardsToTrade Player
            | SelectCard Card
            | SellField Player Field
            | PlantFromHand Player
            | DrawCardsToHand Player
            | KeepFromTrade Player Int
            | PlantFromSide Player Int
            | TradeFromHand Player Int Player
            | Trade Player Int Player
