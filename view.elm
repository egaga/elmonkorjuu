module View where

import UI as Action   exposing (Action)
import Style           exposing (..)
import Domain          exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Signal          exposing (..)
import Util            exposing (..)
import VirtualDom      exposing (Property)
import Array           exposing (..)

cardContent card buttons =
    List.append buttons [ text card.name, priceMeterView card.cardType ]

viewCard : Card -> Html
viewCard card =
  div [ class "card" ]
      (cardContent card [])

fieldView : Address Action.Action -> Player -> Index -> Field -> Html
fieldView address player index field =
  let
    {amount, card} = field
    cardText = text (card.name ++ " (" ++ (toString amount) ++")")
    sellAmount = Domain.sellPrice amount card.cardType
    sellButton = button [ onClick address (Action.SellField player index) ] [ text ("Sell $" ++ (toString sellAmount)) ]
  in
    div [ class "card" ] [ cardText, sellButton, priceMeterView card.cardType ]

tradeButtonsView address players player cardIndex =
  let
    tradeForPlayer toPlayer =
      button [ class "tradeButton",
               onClick address (Action.Trade player cardIndex toPlayer) ]
             [ text ("to " ++ toPlayer.nick) ]
  in
    List.map tradeForPlayer players

tradeFromHandButtonsView address players player cardIndex =
  let
    tradeForPlayer toPlayer =
      button [ class "tradeButton",
               onClick address (Action.TradeFromHand player cardIndex toPlayer) ]
             [ text ("to " ++ toPlayer.nick) ]
  in
    List.map tradeForPlayer players

plantButton address player =
  button [ class "plantButton",
           onClick address (Action.PlantFromHand player) ]
         [ text ("Plant") ]

plantSideButton address player index =
  button [ class "plantButton",
           onClick address (Action.PlantFromSide player index) ]
         [ text ("Plant") ]

keepButton address player index =
  button [ onClick address (Action.KeepFromTrade player index) ]
         [ text ("Keep") ]

viewTopMostHandCard address players player card =
  let
    cardIndex = 0
    tradeButtons = tradeFromHandButtonsView address players player cardIndex
    buttons = (plantButton address player) :: tradeButtons
  in
    div [ class "card" ] (cardContent card buttons)

amountToPriceView {amount, money} =
  div [ class "priceMapping" ]
      [ div [ class "coins" ]
            [ text (toString money) ],
        div [ class "meterLimit" ]
            [ text (toString amount)] ]

priceMeterView : CardType -> Html
priceMeterView cardType =
  let
    meterList = Domain.priceMeterList cardType
    priceColumns = List.map amountToPriceView (Array.toList meterList)
  in
    div [ class "priceMeter" ] priceColumns

viewHand : Address Action.Action -> List Player -> Player -> List Card -> List Html
viewHand address players player hand =
  let
    topmost = List.head hand
    topmostView = mapMaybeToList (viewTopMostHandCard address players player) topmost
    cardWithTrades cardIndex card =
      div [ class "card" ]
          (cardContent card (tradeFromHandButtonsView address players player cardIndex))

    -- note: indexing contains the first hand so that the rest get correct hand index
    cardsUnderTopView = List.drop 1 (List.indexedMap cardWithTrades hand)
  in
    List.append topmostView cardsUnderTopView

sideView : Address Action.Action -> List Player -> Player -> List Card -> List Html
sideView address players player side =
  let
    sideCardView index card =
      div [ class "card" ] (cardContent card [ plantSideButton address player index ])
  in
    List.indexedMap sideCardView side

fieldsView : Address Action.Action -> Player -> Array Field -> List Html
fieldsView address player fields =
  let fv index field = div [ class "field" ] [fieldView address player index field]
  in List.indexedMap fv (Array.toList fields)

tradeView : Address Action.Action -> List Player -> Player -> List Card -> List Html
tradeView address players player trade =
  let
    tradeCardView index card =
      let
        tradeButtons = tradeButtonsView address players player index
        keepBtn = keepButton address player index
        tradeViewContent = cardContent card (keepBtn :: tradeButtons)
        iv = div [] [ text (toString index) ]
      in
        div [ class "card" ] (iv :: tradeViewContent)
  in
    List.indexedMap tradeCardView trade

playerView : Address Action.Action -> List Player -> Player -> List Html
playerView address players player =
  let
    (o1, o2) = otherElements players (\(i, p)  -> p.nick == player.nick)
    otherPlayers = List.append o1 o2
  in [
    div [ class "player-info" ] [
      text ("Player: " ++ player.nick),
      span [ class "money" ] [ text ("Money: " ++ (toString player.money)) ]
    ],
    button [ onClick address (Action.DrawCardsToTrade player) ] [ text "Draw cards for trade" ],
    button [ onClick address (Action.DrawCardsToHand player) ] [ text "Draw cards to hand" ],
    div [ class "fields-and-hand" ] [
      div [ class "fields" ] (fieldsView address player player.fields),
      div [ class "hand" ] (viewHand address otherPlayers player (Array.toList player.hand))
    ],
    div [ class "trading"] [
      div [ class "trade" ] (tradeView address otherPlayers player (Array.toList player.trade)),
      div [ class "side" ] (sideView address otherPlayers player (Array.toList player.side)) ]
    ]

view : Address Action.Action -> Model -> Html
view address model =
  let
    playerList = Array.toList model.players
    deckView = div [ class "deck" ] (List.map viewCard (Array.toList model.deck))
    discardView = div [ class "discard" ] (List.map viewCard (Array.toList model.discard))
    gameView = stylesheet :: deckView :: discardView :: List.concatMap (playerView address playerList) playerList
  in
    div [ class "game-view" ] gameView
