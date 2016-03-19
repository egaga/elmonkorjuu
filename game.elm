-- TODO
-- Restrict the actions of players (for some of the rules)

-- Design note:
-- Referencing specific card is done using array index.
-- This is vulnarable if multiple simultaneous actions occur.
-- TODO reference cards by their identity.

module Elmonkorjuu where

import Style           exposing (..)
import View            exposing (..)
import Domain          exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Signal          exposing (..)
import StartApp.Simple as StartApp
import Util            exposing (..)
import UI as Action exposing (Action)
import Random exposing (initialSeed)
import Array exposing (..)

-- TODO get seed from mousemovements&time
shuffledDeck = Domain.shuffleDeck Domain.allCards (Random.initialSeed 10)

initialModel : Model
initialModel =
  let
    (deck, players) = startGameWithPlayers shuffledDeck ["Pasi", "Anssi", "Henkka"]
  in
    { players = players,
      deck = deck,
      discard = Array.empty }

updatePlayer : Player -> Array Player -> Array Player
updatePlayer player players =
  updateElement players player (\(i, p)  -> p.nick == player.nick)

updatePlayers : Array Player -> Array Player -> Array Player
updatePlayers players updatedPlayers =
  Array.foldl updatePlayer players updatedPlayers

updateWith : Model -> Player -> Model
updateWith model player =
  { model | players = updatePlayer player model.players }

update : Action -> Model -> Model
update action model =
  case action of
    Action.DrawCardsToTrade playerInput ->
      let
        (deck, player) = Domain.drawCardsToTrade model.deck playerInput
      in
        { model | deck = deck, players = updatePlayer player model.players }
    Action.SelectCard card ->
      Debug.log card.name { model | discard = Array.fromList [ card ] }
    Action.PlantFromHand playerInput ->
      updateWith model (Domain.plantTopmostCard playerInput)
    Action.PlantFromSide playerInput index ->
      updateWith model (Domain.plantFromSide playerInput index)
    Action.SellField playerInput index ->
      case Domain.playerSellsField playerInput index of
        Nothing -> model
        Just ({amount, card}, player) ->
          let
            newDiscard = Array.append model.discard (Array.fromList <| List.repeat amount card)
          in
            { model |
                players = updatePlayer player model.players,
                discard = newDiscard }
    Action.DrawCardsToHand playerInput ->
       let
         (deck, player) = Domain.drawCardsToHand model.deck playerInput
       in { model |
              players = updatePlayer player model.players,
              deck = deck }
    Action.KeepFromTrade playerInput i ->
      updateWith model (Domain.keepFromTrade i playerInput)
    Action.TradeFromHand fromPlayerInput i toPlayerInput ->
      let
        (fromPlayer, toPlayer) = Domain.tradeFromHand fromPlayerInput i toPlayerInput
      in
        { model | players = updatePlayers model.players (Array.fromList [fromPlayer, toPlayer]) }
    Action.Trade fromPlayerInput i toPlayerInput ->
      let
        (fromPlayer, toPlayer) = Domain.trade fromPlayerInput i toPlayerInput
      in
        { model | players = updatePlayers model.players (Array.fromList [fromPlayer, toPlayer]) }

main : Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
      update = update }
