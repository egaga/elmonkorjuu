-- TODO
-- Restrict the actions of players (for some of the rules)
-- Design note:
-- Referencing specific card is done using array index.
-- This is vulnarable if multiple simultaneous actions occur.
-- TODO reference cards by their identity.
-- Design note:
-- Code (actions, etc) could be split hierarchically so that code consists of components:
-- a component itself would be similar to a small Elm program with update, view and model functions


module Game exposing (..)

import Array exposing (empty)
import Domain exposing (Model, startGameWithPlayers)
import Html
import Random exposing (initialSeed)
import Time exposing (Time, second)
import UI as Msg exposing (Msg)
import Update exposing (update)
import View as GameView


initialModel : Model
initialModel =
    let
        ( deck, players ) =
            startGameWithPlayers shuffledDeck [ "Pasi", "Anssi", "Henkka" ]
        ( shuffledDeck, seed ) = initDeck
    in
    { startTime = Nothing
    , currentTime = 0
    , players = players
    , deck = deck
    , discard = Array.empty
    }


initDeck = 
    let
        seedValue =
            1525

        -- TODO get seed from mousemovements&time
        shuffledDeckGenerator =
            Domain.shuffleDeck Domain.allCards
    in
    Random.step shuffledDeckGenerator (initialSeed seedValue)


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Msg.GetTime


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = GameView.view
        , update = Update.update
        , subscriptions = subscriptions
        }
