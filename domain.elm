module Domain where

import Random exposing (initialSeed)
import Random.Array exposing (..)
import Array exposing (..)
import Dict exposing (..)
import Util exposing (..)

type alias Model = {
    players: List Player,
    deck: Deck,
    discard: Deck }

type CardType = Coffee | Wax | Blue | Chili | Stink | Green | Soy | BlackEyed | Red | Garden | Cocoa

type alias Card = {
  name: String,
  cardType: CardType
}

type alias Index = Int

type alias Deck = List Card

type alias Money = Int

type alias Player = {
  nick: String,
  hand: List Card,
  trade: List Card,
  side: List Card,
  fields: List Field,
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

allCards = createDeck [
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
priceMeterList : CardType -> List PriceMapping
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
      List.map (\mapping -> { amount = fst mapping, money = snd mapping }) mappings

createField : Int -> Card -> Field
createField amount card = {
  amount = amount,
  card = card }

createPlayer : String -> List Card -> Player
createPlayer nick startingHand =
  {
    nick = nick,
    hand = startingHand,
    trade = [],
    side = [],
    fields = [],
    money = 0 }

startGameWithPlayers startDeck nicks =
  let
    accumulate : String -> (Deck, List Player) -> (Deck, List Player)
    accumulate nick (currentDeck, players) =
      let
        (hand, newDeck) = takeCardsFromDeck 5 currentDeck
        newPlayer = createPlayer nick hand
      in
        (newDeck, newPlayer :: players)
  in
    List.foldl accumulate (startDeck, []) nicks

-- The money player gets when she has <amount> of cards, according to the price mapping
findMeterValue : List PriceMapping -> Int -> Money
findMeterValue l totalCards =
  let
    over = List.filter (\{amount, money} -> amount <= totalCards) l
    last = List.head (List.reverse over)
  in
    case last of
      Nothing -> 0
      Just {money} -> money

createDeck : List (Int, Card) -> List Card
createDeck listOfPairs =
  List.concatMap (\(amount, card) -> List.repeat amount card) listOfPairs

shuffleDeck : Deck -> Random.Seed -> Deck
shuffleDeck deck seed =
  let (shuffledDeck, newSeed) = Random.Array.shuffle seed (Array.fromList allCards)
  in Array.toList shuffledDeck

card : String -> CardType -> Card
card name cardType = {
  name = name,
  cardType = cardType }

plantTopmostCard : Player -> Player
plantTopmostCard player =
  case List.head player.hand of
      Nothing -> player
      Just first -> {
        player |
          fields = addToFields first player.fields,
          hand = List.drop 1 player.hand }

plantFromSide: Player -> Int -> Player
plantFromSide player cardIndex =
  let
    (before, cardAsList, after) = splitByIndex cardIndex player.side
  in
    case List.head cardAsList of
      Nothing -> player
      Just elem ->
        { player |
          fields = addToFields elem player.fields,
          side = List.append before after }

-- Adds card to existing field if same card type; otherwise creates new one.
-- Does not support if same card type has multiple cards.
addToFields : Card -> List Field -> List Field
addToFields card fields =
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
    (newCards, deck) = takeCardsFromDeck 3 deck
    newPlayer = { player | hand = List.append player.hand newCards }
  in
    (deck, newPlayer)

drawCardsToTrade : Deck -> Player -> (Deck, Player)
drawCardsToTrade deck player =
  let
    (trade, newDeck) = takeCardsFromDeck drawToTradeCardsAmount deck
    newPlayer = { player | trade = trade }
  in
    (newDeck, newPlayer)

takeCardsFromDeck : Int -> Deck -> (List Card, Deck)
takeCardsFromDeck amount deck =
  (List.take amount deck, List.drop amount deck)

takeCardToSide : Player -> Player
takeCardToSide player = player

playerSellsField: Player -> Index -> Maybe (Field, Player)
playerSellsField player index =
  let
    (before, f, after) = splitByIndex index player.fields
  in
    case List.head f of
      Nothing -> Nothing
      Just field ->
        let
          newPlayer =
            { player |
                money = player.money + sellPrice field.amount field.card.cardType,
                fields = List.append before after }
        in
          Just (field, newPlayer)

sellPrice: Int -> CardType -> Money
sellPrice amount cardType =
  findMeterValue (priceMeterList cardType) amount

keepFromTrade: Int -> Player -> Player
keepFromTrade index player =
  let
    (before, keptCard, after) = splitByIndex index player.trade
  in
   { player |
      trade = List.append before after,
      side = List.append player.side keptCard }

tradeFromHand : Player -> Int -> Player -> (Player, Player)
tradeFromHand fromPlayerInput i toPlayerInput =
  let
    (fromHand, toSide) = commonTrade fromPlayerInput.hand i toPlayerInput.side
  in
    ( { fromPlayerInput | hand = fromHand },
      { toPlayerInput | side = toSide })

trade : Player -> Int -> Player -> (Player, Player)
trade fromPlayerInput i toPlayerInput =
  let
    (fromSide, toSide) = commonTrade fromPlayerInput.trade i toPlayerInput.side
  in
    ( { fromPlayerInput | trade = fromSide },
      { toPlayerInput | side = toSide })

commonTrade : List Card -> Int -> List Card -> (List Card, List Card)
commonTrade fromList i toList =
  let
    (before, tradeCard, after) = splitByIndex i fromList
  in
    case List.head tradeCard of
      Nothing -> (fromList, toList)
      Just card ->
        (List.append before after, card :: toList)
