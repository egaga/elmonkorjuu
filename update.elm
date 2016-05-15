module Update exposing (update)

import Domain exposing (..)
import UI as Msg exposing (Msg)
import UI as PlayerAction exposing (PlayerAction)
import Array exposing (..)
import Time exposing (Time, second)
import Util exposing (updateElement)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Msg.GetTime newTime ->
      (updateTime model newTime, Cmd.none)
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

updateTime: Model -> Time -> Model
updateTime model newTime =
  { model |
      startTime = (Just (Maybe.withDefault newTime model.startTime)),
      currentTime = newTime }
