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
import Update exposing (update)
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
      update = Update.update,
      subscriptions = subscriptions }
