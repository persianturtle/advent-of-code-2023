let input = Utilities.input

let input2 = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"

exception InvalidInput(string)

type hand = {
  hand: string,
  bid: int,
}

let hands =
  input
  ->Js.String2.split("\n")
  ->Js.Array2.filter(line => line->Js.String2.trim->Js.String2.length > 0)
  ->Js.Array2.map(line => {
    switch line->Js.String2.split(" ") {
    | [hand, bid] => {
        hand,
        bid: bid->Belt.Int.fromString->Belt.Option.getExn,
      }

    | _ => raise(InvalidInput("The input is invalid."))
    }
  })

let mapCardToStrength = card =>
  switch card {
  | "A" => 14
  | "K" => 13
  | "Q" => 12
  | "J" => 11
  | "T" => 10
  | "9" => 9
  | "8" => 8
  | "7" => 7
  | "6" => 6
  | "5" => 5
  | "4" => 4
  | "3" => 3
  | "2" => 2
  | _ => raise(InvalidInput("The card is invalid."))
  }

let mapHandToStrength = hand => {
  let dict = Js.Dict.empty()
  hand
  ->Js.String2.split("")
  ->Js.Array2.forEach(card => {
    dict->Js.Dict.set(card, dict->Js.Dict.get(card)->Belt.Option.getWithDefault(0) + 1)
  })

  switch dict->Js.Dict.entries {
  | [(_, 5)] => 7
  | [(_, 4), _]
  | [_, (_, 4)] => 6
  | [(_, 3), (_, 2)]
  | [(_, 2), (_, 3)] => 5
  | [(_, 3), _, _]
  | [_, (_, 3), _]
  | [_, _, (_, 3)] => 4
  | [(_, 2), (_, 2), _]
  | [_, (_, 2), (_, 2)]
  | [(_, 2), _, (_, 2)] => 3
  | [(_, 2), _, _, _]
  | [_, (_, 2), _, _]
  | [_, _, (_, 2), _]
  | [_, _, _, (_, 2)] => 2
  | _ => 1
  }
}

hands
->Js.Array2.sortInPlaceWith((a, b) => {
  switch mapHandToStrength(a.hand) - mapHandToStrength(b.hand) {
  | 0 =>
    let indexRef = ref(0)
    let resultRef = ref(0)

    while resultRef.contents == 0 && indexRef.contents < 5 {
      switch mapCardToStrength(a.hand->Js.String2.charAt(indexRef.contents)) -
      mapCardToStrength(b.hand->Js.String2.charAt(indexRef.contents)) {
      | 0 => indexRef := indexRef.contents + 1
      | result => resultRef := result
      }
    }

    resultRef.contents
  | result => result
  }
})
->Js.Array2.reducei((winnings, hand, index) => {
  winnings + hand.bid * (index + 1)
}, 0)
->Js.log
