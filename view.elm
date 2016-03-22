module View (view) where

import UI
import UI as PlayerAction exposing (PlayerAction)
import Style           exposing (..)
import Domain          exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Signal          exposing (..)
import Util            exposing (..)
import Array           exposing (..)
import Time            exposing (..)

type alias Context a = {
  newActionHandler : (Address a -> a -> Attribute) -> PlayerAction -> Attribute
}

type alias EnhancedContext = {
  onClick : PlayerAction -> Attribute
}

enhanced context = {
    onClick = \playerAction -> context.newActionHandler onClick playerAction }

cardContent : Card -> List Html -> List Html
cardContent card buttons =
    List.append buttons [ text card.name, priceMeterView card.cardType ]

viewCard : Card -> Html
viewCard card =
  div [ class "card" ]
      (cardContent card [])

fieldView : EnhancedContext -> Player -> Index -> Field -> Html
fieldView context player index field =
  let
    {amount, card} = field
    cardText = text (card.name ++ " (" ++ (toString amount) ++")")
    sellAmount = Domain.sellPrice amount card.cardType
    sellButton = button [ context.onClick (PlayerAction.SellField player index) ] [ text ("Sell $" ++ (toString sellAmount)) ]
  in
    div [ class "card" ] [ cardText, sellButton, priceMeterView card.cardType ]

tradeButtonsView context players player cardIndex =
  let
    tradeForPlayer toPlayer =
      button [ class "tradeButton",
               context.onClick (PlayerAction.Trade player cardIndex toPlayer) ]
             [ text ("to " ++ toPlayer.nick) ]
  in
    List.map tradeForPlayer players

tradeFromHandButtonsView context players player cardIndex =
  let
    tradeForPlayer toPlayer =
      button [ class "tradeButton",
               context.onClick (PlayerAction.TradeFromHand player cardIndex toPlayer) ]
             [ text ("to " ++ toPlayer.nick) ]
  in
    List.map tradeForPlayer players

plantButton context player =
  button [ class "plantButton",
           context.onClick (PlayerAction.PlantFromHand player) ]
         [ text ("Plant") ]

plantSideButton context player index =
  button [ class "plantButton",
           context.onClick (PlayerAction.PlantFromSide player index) ]
         [ text ("Plant") ]

keepButton context player index =
  button [ context.onClick (PlayerAction.KeepFromTrade player index) ]
         [ text ("Keep") ]

viewTopMostHandCard context players player card =
  let
    cardIndex = 0
    tradeButtons = tradeFromHandButtonsView context players player cardIndex
    buttons = (plantButton context player) :: tradeButtons
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
    priceColumns = Array.map amountToPriceView meterList |> Array.toList
  in
    div [ class "priceMeter" ] priceColumns

viewHand : EnhancedContext -> List Player -> Player -> List Card -> List Html
viewHand context players player hand =
  let
    topmost = List.head hand
    topmostView = mapMaybeToList (viewTopMostHandCard context players player) topmost
    cardWithTrades cardIndex card =
      div [ class "card" ]
          (cardContent card (tradeFromHandButtonsView context players player cardIndex))

    -- note: indexing contains the first hand so that the rest get correct hand index
    cardsUnderTopView = List.drop 1 (List.indexedMap cardWithTrades hand)
  in
    List.append topmostView cardsUnderTopView

sideView : EnhancedContext -> List Player -> Player -> List Card -> List Html
sideView context players player side =
  let
    sideCardView index card =
      div [ class "card" ] (cardContent card [ plantSideButton context player index ])
  in
    List.indexedMap sideCardView side

fieldsView : EnhancedContext -> Player -> Array Field -> List Html
fieldsView context player fields =
  let fv index field = div [ class "field" ] [fieldView context player index field]
  in Array.indexedMap fv fields |> Array.toList

tradeView : EnhancedContext -> List Player -> Player -> List Card -> List Html
tradeView context players player trade =
  let
    tradeCardView index card =
      let
        tradeButtons = tradeButtonsView context players player index
        keepBtn = keepButton context player index
        tradeViewContent = cardContent card (keepBtn :: tradeButtons)
        iv = div [] [ text (toString index) ]
      in
        div [ class "card" ] (iv :: tradeViewContent)
  in
    List.indexedMap tradeCardView trade

playerView : EnhancedContext -> List Player -> Player -> List Html
playerView context players player =
  let
    (o1, o2) = otherElements players (\(i, p)  -> p.nick == player.nick)
    otherPlayers = List.append o1 o2
  in [
    div [ class "player-info" ] [
      text ("Player: " ++ player.nick),
      span [ class "money" ] [ text ("Money: " ++ (toString player.money)) ]
    ],
    button [ context.onClick (PlayerAction.DrawCardsToTrade player) ] [ text "Draw cards for trade" ],
    button [ context.onClick (PlayerAction.DrawCardsToHand player) ] [ text "Draw cards to hand" ],
    div [ class "fields-and-hand" ] [
      div [ class "fields" ] (fieldsView context player player.fields),
      div [ class "hand" ] (viewHand context otherPlayers player (Array.toList player.hand))
    ],
    div [ class "trading"] [
      div [ class "trade" ] (tradeView context otherPlayers player (Array.toList player.trade)),
      div [ class "side" ] (sideView context otherPlayers player (Array.toList player.side)) ]
    ]

timeView : Time -> Html
timeView time =
  let
    timeInSeconds = Time.inSeconds time |> round |> toString
  in
    div [ class "time" ]
        [ text "Play time: ",
          span [ class "seconds" ] [ text <| timeInSeconds ],
          text " seconds." ]

view : Context a -> Model -> Html
view context model =
  doView (enhanced context) model

doView : EnhancedContext -> Model -> Html
doView context model =
  let
    playerList = Array.toList model.players
    deckView = div [ class "deck" ] (List.map viewCard (Array.toList model.deck))
    discardView = div [ class "discard" ] (List.map viewCard (Array.toList model.discard))
    playersView = List.concatMap (playerView context playerList) playerList
    gameView = stylesheet :: timeView model.playTime :: deckView :: discardView :: playersView
  in
    div [ class "game-view" ] gameView
