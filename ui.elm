module UI where

import Domain exposing (..)

type Action = DrawCardsToTrade Player
            | SelectCard Card
            | SellField Player Index
            | PlantFromHand Player
            | DrawCardsToHand Player
            | KeepFromTrade Player Index
            | PlantFromSide Player Index
            | TradeFromHand Player Index Player
            | Trade Player Index Player
