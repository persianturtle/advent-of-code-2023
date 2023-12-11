let input = Utilities.input

let input2 = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"

/* https://www.reddit.com/r/adventofcode/comments/18cr4xr/2023_day_7_better_example_input_not_a_spoiler/ */
let input3 = "2345A 1
Q2KJJ 13
Q2Q2Q 19
T3T3J 17
T3Q33 11
2345J 3
J345A 2
32T3K 5
T55J5 29
KK677 7
KTJJT 34
QQQJA 31
JJJJJ 37
JAAAA 43
AAAAJ 59
AAAAA 61
2AAAA 23
2JJJJ 53
JJJJ2 41
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
  | "T" => 11
  | "9" => 10
  | "8" => 9
  | "7" => 8
  | "6" => 7
  | "5" => 6
  | "4" => 5
  | "3" => 4
  | "2" => 3
  | "J" => 2
  | _ => raise(InvalidInput("The card is invalid."))
  }

let mapHandToStrength = hand => {
  let dict = Js.Dict.empty()
  hand
  ->Js.String2.split("")
  ->Js.Array2.forEach(card => {
    dict->Js.Dict.set(card, dict->Js.Dict.get(card)->Belt.Option.getWithDefault(0) + 1)
  })

  switch dict
  ->Js.Dict.entries
  ->Js.Array2.sortInPlaceWith(((aCard, aCount), (bCard, bCount)) =>
    switch (aCard, bCard) {
    | ("J", "J") => 0
    | ("J", _) => -1
    | (_, "J") => 1
    | _ => bCount - aCount
    }
  ) {
  | [(_, 5)] => 7
  | [("J", _), _] => 7

  | [(_, 4), _] => 6
  | [("J", a), (_, b), _] if a + b == 4 => 6

  | [(_, 3), (_, 2)] => 5
  | [("J", 1), (_, 2), (_, 2)] => 5

  | [(_, 3), _, _] => 4
  | [("J", a), (_, b), _, _] if a + b == 3 => 4

  | [(_, 2), (_, 2), _] => 3

  | [(_, 2), _, _, _] => 2
  | [("J", 1), _, _, _, _] => 2

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
