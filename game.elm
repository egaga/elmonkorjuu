-- TODO
-- Restrict the actions of players (for some of the rules)

-- Design note:
-- Referencing specific card is done using array index.
-- This is vulnarable if multiple simultaneous actions occur.
-- TODO reference cards by their identity.

-- Design note:
-- Code (actions, etc) could be split hierarchically so that code consists of components:
-- a component itself would be similar to a small Elm program with update, view and model functions

module Elmonkorjuu exposing (..)

import View as GameView
import Domain          exposing (..)
import Html            exposing (..)
import Html.App as HtmlApp
import Util            exposing (..)
import UI as Msg exposing (Msg)
import UI as PlayerAction exposing (PlayerAction)
import Random exposing (initialSeed)
import Array exposing (..)
import Time exposing (Time, second)
import Task

-- TODO get seed from mousemovements&time
-- shuffledDeck = Domain.shuffleDeck Domain.allCards (Random.initialSeed 10)
shuffledDeck = Domain.allCards --TODO when mgold/random-sample is upgraded to support elm 0.17

initialModel : Model
initialModel =
  let
    (deck, players) = startGameWithPlayers shuffledDeck ["Pasi", "Anssi", "Henkka"]
  in
    {
      startTime = Nothing,
      currentTime = 0,
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

noEffect : Model -> (Model, Cmd Msg)
noEffect model =
  (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Msg.GetTime newTime ->
      let
        newModel = {
          model |
            startTime = (Just (Maybe.withDefault newTime model.startTime)),
            currentTime = newTime }
      in
        (newModel, Cmd.none)
    Msg.PlayerAction playerAction ->
      noEffect <| updatePlayerAction playerAction model

updatePlayerAction : Msg.PlayerAction -> Model -> Model
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

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)
--init = (initialModel, Action.GetTime)

view : Model -> (Html Msg)
view model =
  GameView.view model

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Msg.GetTime

main =
  HtmlApp.program
    { init = init,
      view = view,
      update = update,
      subscriptions = subscriptions }
