module Domain exposing (..)

import Random exposing (initialSeed)
import Random.Array
import Array exposing (Array)
import Util exposing (asplitByIndex, arrayDrop, arrayReverse)
import Time exposing (Time)

type alias Model = {
  startTime: Maybe Time,
  currentTime: Time,
  players: Array Player,
  deck: Deck,
  discard: Deck }

type CardType = Coffee | Wax | Blue | Chili | Stink | Green | Soy | BlackEyed | Red | Garden | Cocoa

type alias Card = {
  name: String,
  cardType: CardType
}

type alias Deck = Array Card
type alias Index = Int
type alias Money = Int

type alias Player = {
  nick: String,
  hand: Array Card,
  trade: Array Card,
  side: Array Card,
  fields: Array Field,
  money: Money
}

-- Field means planted cards, where List.length cards = amount
type alias Field = {
  amount: Int,
  card: Card
}

type alias PriceMapping = {
  amount: Int,
  money: Money
}

cards = {
  coffee = card "Coffee" Coffee,
  soy = card "Soy" Soy,
  blue = card "Blue" Blue,
  wax = card "Wax" Wax,
  chili = card "Chili" Chili,
  stink = card "Stink" Stink,
  green = card "Green" Green,
  blackEyed = card "Black-eyed" BlackEyed,
  red = card "Red" Red,
  garden = card "Garden" Garden,
  cocoa = card "Cocoa" Cocoa }

allCards : Array Card
allCards =
  createDeck [
    ( 24, cards.coffee ),
    ( 22, cards.wax ),
    ( 20, cards.blue ),
    ( 18, cards.chili ),
    ( 16, cards.stink ),
    ( 14, cards.green ),
    ( 12, cards.soy ),
    ( 10, cards.blackEyed ),
    ( 8, cards.red ),
    ( 6, cards.garden ),
    ( 4, cards.cocoa ) ]

drawToTradeCardsAmount = 2

-- The value of cards given the type of card and amount of cards
-- e.g. if 4 cards of Soy then one gets 2 money
priceMeterList : CardType -> Array PriceMapping
priceMeterList cardType =
  let mappings =
    case cardType of
      Soy -> [(2, 1), (4, 2), (6, 3), (7, 4)]
      Wax -> [(4, 1), (7, 2), (9, 3), (11, 4)]
      Chili -> [(3, 1), (6, 2), (8, 3), (9, 4)]
      Garden -> [(2, 2), (3, 3)]
      Coffee -> [(4, 1), (7, 2), (10, 3), (12, 4)]
      BlackEyed -> [(2, 1), (4, 2), (5, 3), (6, 4)]
      Red -> [(2, 1), (3, 2), (4, 3), (5, 4)]
      Stink -> [(3, 1), (5, 2), (7, 3), (8, 4)]
      Blue -> [(4, 1), (6, 2), (8, 3), (10, 4)]
      Green -> [(3, 1), (5, 2), (6, 3), (7, 4)]
      Cocoa -> [(2, 2), (3, 3), (4, 4)]
    in
      Array.map (\(a, m) -> { amount = a, money = m }) (Array.fromList mappings)

createField : Int -> Card -> Field
createField amount card = {
  amount = amount,
  card = card }

createPlayer : String -> Array Card -> Player
createPlayer nick startingHand =
  {
    nick = nick,
    hand = startingHand,
    trade = Array.empty,
    side = Array.empty,
    fields = Array.empty,
    money = 0 }

startGameWithPlayers : Deck -> List String -> (Deck, Array Player)
startGameWithPlayers startDeck nicks =
  let
    accumulate : String -> (Deck, Array Player) -> (Deck, Array Player)
    accumulate nick (currentDeck, players) =
      let
        (hand, newDeck) = takeCardsFromDeck 5 currentDeck
        newPlayer = createPlayer nick hand
      in
        (newDeck, Array.append (Array.fromList [newPlayer]) players)
  in
    Array.foldl accumulate (startDeck, Array.empty) (Array.fromList nicks)

-- The money player gets when she has <amount> of cards, according to the price mapping
findMeterValue : Array PriceMapping -> Int -> Money
findMeterValue l totalCards =
  let
    over = Array.filter (\{amount, money} -> amount <= totalCards) l
    last = Array.get 0 (arrayReverse over)
  in
    case last of
      Nothing -> 0
      Just {money} -> money

createDeck : List (Int, Card) -> Array Card
createDeck listOfPairs =
  Array.fromList <|
    List.concatMap (\(amount, card) -> List.repeat amount card) listOfPairs

shuffleDeck : Deck -> Random.Generator (Deck)
shuffleDeck deck =
  let
    shuffledDeck = Random.Array.shuffle allCards
  in
    shuffledDeck

card : String -> CardType -> Card
card name cardType = {
  name = name,
  cardType = cardType }

plantTopmostCard : Player -> Player
plantTopmostCard player =
  case Array.get 0 player.hand of
    Nothing ->
      player
    Just first -> {
      player |
        fields = addToFields first player.fields,
        hand = arrayDrop 1 player.hand }

plantFromSide: Player -> Index -> Player
plantFromSide player cardIndex =
  let
    (before, cardAsList, after) = asplitByIndex cardIndex player.side
  in
    case Array.get 0 cardAsList of
      Nothing -> player
      Just elem ->
        { player |
          fields = addToFields elem player.fields,
          side = Array.append before after }

-- Adds card to existing field if same card type; otherwise creates new one.
-- Does not support if same card type has multiple cards.
addToFields : Card -> Array Field -> Array Field
addToFields card array =
    Array.fromList <| listaddToFields card (Array.toList array)

listaddToFields : Card -> List Field -> List Field
listaddToFields card fields =
  let
    (sameFields, differentFields) = List.partition (\field -> field.card.cardType == card.cardType) fields
  in
    case List.head sameFields of
      Nothing ->
        (createField 1 card) :: differentFields
      Just field ->
        (createField (field.amount + 1) card) :: differentFields -- note: if same type has multiple cards, they are lost

drawCardsToHand : Deck -> Player -> (Deck, Player)
drawCardsToHand deck player =
  let
    (newCards, newDeck) = takeCardsFromDeck 3 deck
    newPlayer = { player | hand = Array.append player.hand newCards }
  in
    (newDeck, newPlayer)

drawCardsToTrade : Deck -> Player -> (Deck, Player)
drawCardsToTrade deck player =
  let
    (trade, newDeck) = takeCardsFromDeck drawToTradeCardsAmount deck
    newPlayer = { player | trade = trade }
  in
    (newDeck, newPlayer)

takeCardsFromDeck : Int -> Deck -> (Array Card, Deck)
takeCardsFromDeck amount deck =
  (Array.slice 0 amount deck, arrayDrop amount deck)

takeCardToSide : Player -> Player
takeCardToSide player = player

playerSellsField: Player -> Index -> Maybe (Field, Player)
playerSellsField player index =
  let
    (before, f, after) = asplitByIndex index player.fields
  in
    case Array.get 0 f of
      Nothing ->
        Nothing
      Just field ->
        let
          newPlayer =
            { player |
                money = player.money + sellPrice field.amount field.card.cardType,
                fields = Array.append before after }
        in
          Just (field, newPlayer)

sellPrice: Int -> CardType -> Money
sellPrice amount cardType =
  findMeterValue (priceMeterList cardType) amount

keepFromTrade: Index -> Player -> Player
keepFromTrade index player =
  let
    (before, keptCard, after) = asplitByIndex index player.trade
  in
   { player |
      trade = Array.append before after,
      side = Array.append player.side keptCard }

tradeFromHand : Player -> Index -> Player -> (Player, Player)
tradeFromHand fromPlayerInput index toPlayerInput =
  let
    (fromHand, toSide) = tradeCardFromList fromPlayerInput.hand index toPlayerInput.side
  in
    ( { fromPlayerInput | hand = fromHand },
      { toPlayerInput | side = toSide })

trade : Player -> Index -> Player -> (Player, Player)
trade fromPlayerInput index toPlayerInput =
  let
    (fromSide, toSide) = tradeCardFromList fromPlayerInput.trade index toPlayerInput.side
  in
    ( { fromPlayerInput | trade = fromSide },
      { toPlayerInput | side = toSide })

tradeCardFromList : Array Card -> Index -> Array Card -> (Array Card, Array Card)
tradeCardFromList fromList index toList =
  let
    (before, tradeCard, after) = asplitByIndex index fromList
  in
    case Array.get 0 tradeCard of
      Nothing ->
        (fromList, toList)
      Just card ->
        (Array.append before after, Array.append (Array.fromList [card]) toList)
