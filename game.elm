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
import Domain          exposing (Model, startGameWithPlayers)
import Html            exposing (..)
import Html.App as HtmlApp
import UI as Msg exposing (Msg)
import Random exposing (initialSeed)
import Array exposing (empty)
import Time exposing (Time, second)
import Update exposing (update)

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

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Msg.GetTime

main =
  HtmlApp.program
    { init = (initialModel, Cmd.none),
      view = GameView.view,
      update = Update.update,
      subscriptions = subscriptions }
