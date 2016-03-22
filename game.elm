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

import View as GameView
import Domain          exposing (..)
import Html            exposing (..)
import Signal          exposing (..)
import StartApp
import Util            exposing (..)
import UI as Action exposing (Action)
import UI as PlayerAction exposing (PlayerAction)
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
    Action.PlayerAction playerAction ->
      noEffect <| updatePlayerAction playerAction model

updatePlayerAction : Action.PlayerAction -> Model -> Model
updatePlayerAction action model =
  case action of
    PlayerAction.DrawCardsToTrade playerInput ->
      let
        (deck, player) = Domain.drawCardsToTrade model.deck playerInput
      in
        { model | deck = deck, players = updatePlayer player model.players }
    PlayerAction.PlantFromHand playerInput ->
      updateWith model (Domain.plantTopmostCard playerInput)
    PlayerAction.PlantFromSide playerInput index ->
      updateWith model (Domain.plantFromSide playerInput index)
    PlayerAction.SellField playerInput index ->
      case Domain.playerSellsField playerInput index of
        Nothing ->
          model
        Just ({amount, card}, player) ->
          let
            newDiscard = Array.append model.discard (Array.fromList <| List.repeat amount card)
          in
            { model |
              players = updatePlayer player model.players,
              discard = newDiscard }
    PlayerAction.DrawCardsToHand playerInput ->
      let
        (deck, player) = Domain.drawCardsToHand model.deck playerInput
      in
        { model |
            players = updatePlayer player model.players,
            deck = deck }
    PlayerAction.KeepFromTrade playerInput i ->
      updateWith model (Domain.keepFromTrade i playerInput)
    PlayerAction.TradeFromHand fromPlayerInput i toPlayerInput ->
      let
        (fromPlayer, toPlayer) = Domain.tradeFromHand fromPlayerInput i toPlayerInput
      in
        { model | players = updatePlayers model.players (Array.fromList [fromPlayer, toPlayer]) }
    PlayerAction.Trade fromPlayerInput i toPlayerInput ->
      let
        (fromPlayer, toPlayer) = Domain.trade fromPlayerInput i toPlayerInput
      in
        { model | players = updatePlayers model.players (Array.fromList [fromPlayer, toPlayer]) }

init : (Model, Effects Action)
init = (initialModel, Effects.tick Action.GetTime)

view : Address Action -> Model -> Html
view address model =
  let
    context = {
      newActionHandler = \handler -> \playerAction -> handler address (Action.PlayerAction playerAction)
    }
  in
    GameView.view context model

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
