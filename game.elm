-- TODO
-- Restrict the actions of players (for some of the rules)

-- Design note:
-- Referencing specific card is done using array index.
-- This is vulnarable if multiple simultaneous actions occur.
-- TODO reference cards by their identity.

-- Design note:
-- Code (actions, etc) could be split hierarchically so that code consists of components:
-- a component itself would be similar to a small Elm program with update, view and model functions

module Elmonkorjuu where

import Style           exposing (..)
import View            exposing (..)
import Domain          exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Signal          exposing (..)
import StartApp
import Util            exposing (..)
import UI as Action exposing (Action)
import Random exposing (initialSeed)
import Array exposing (..)
import Effects exposing (Effects, Never)
import Time exposing (Time, second)
import Task

-- TODO get seed from mousemovements&time
shuffledDeck = Domain.shuffleDeck Domain.allCards (Random.initialSeed 10)

initialModel : Model
initialModel =
  let
    (deck, players) = startGameWithPlayers shuffledDeck ["Pasi", "Anssi", "Henkka"]
  in
    {
      playTime = 0,
      players = players,
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

noEffect : Model -> (Model, Effects Action)
noEffect model =
  (model, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Action.GetTime newTime ->
      let
        newModel = { model | playTime = newTime }
      in
        (newModel, Effects.tick Action.GetTime)
    Action.DrawCardsToTrade playerInput ->
      let
        (deck, player) = Domain.drawCardsToTrade model.deck playerInput
      in
        noEffect <| { model | deck = deck, players = updatePlayer player model.players }
    Action.PlantFromHand playerInput ->
      noEffect <| updateWith model (Domain.plantTopmostCard playerInput)
    Action.PlantFromSide playerInput index ->
      noEffect <| updateWith model (Domain.plantFromSide playerInput index)
    Action.SellField playerInput index ->
      case Domain.playerSellsField playerInput index of
        Nothing -> noEffect <| model
        Just ({amount, card}, player) ->
          let
            newDiscard = Array.append model.discard (Array.fromList <| List.repeat amount card)
          in
            noEffect <|
              { model |
                players = updatePlayer player model.players,
                discard = newDiscard }
    Action.DrawCardsToHand playerInput ->
       let
         (deck, player) = Domain.drawCardsToHand model.deck playerInput
       in
        noEffect <|
          { model |
              players = updatePlayer player model.players,
              deck = deck }
    Action.KeepFromTrade playerInput i ->
      noEffect <| updateWith model (Domain.keepFromTrade i playerInput)
    Action.TradeFromHand fromPlayerInput i toPlayerInput ->
      let
        (fromPlayer, toPlayer) = Domain.tradeFromHand fromPlayerInput i toPlayerInput
      in
        noEffect <| { model | players = updatePlayers model.players (Array.fromList [fromPlayer, toPlayer]) }
    Action.Trade fromPlayerInput i toPlayerInput ->
      let
        (fromPlayer, toPlayer) = Domain.trade fromPlayerInput i toPlayerInput
      in
        noEffect <| { model | players = updatePlayers model.players (Array.fromList [fromPlayer, toPlayer]) }

init : (Model, Effects Action)
init = (initialModel, Effects.tick Action.GetTime)

app =
  StartApp.start
    { init = init,
      view = view,
      update = update,
      inputs = [] }

main =
  app.html

-- Perform tasks
port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
