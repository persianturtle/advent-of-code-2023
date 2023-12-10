let input = Utilities.input

let input2 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

let memo = Js.Dict.empty()

let rec processCard = (cards, card, totalCardsSoFar) => {
  totalCardsSoFar := totalCardsSoFar.contents + 1

  let cardNumber =
    ((card->Js.String2.split("|"))[0]->Js.String2.split(":"))[0]
    ->Js.String2.replaceByRe(%re("/\D/g"), "")
    ->Belt.Int.fromString
    ->Belt.Option.getWithDefault(0)

  switch Js.Dict.get(memo, cardNumber->Js.String2.make) {
  | Some(amountOfWinners) =>
    for index in cardNumber to cardNumber + amountOfWinners - 1 {
      processCard(cards, cards[index], totalCardsSoFar)
    }
  | None =>
    let winningNumbers =
      ((card->Js.String2.split("|"))[0]->Js.String2.split(":"))[1]
      ->Js.String2.split(" ")
      ->Js.Array2.map(string => string->Js.String2.trim)
      ->Js.Array2.filter(string => string->Js.String2.trim->Js.String2.length > 0)

    let numbers =
      (card->Js.String2.split("|"))[1]
      ->Js.String2.split(" ")
      ->Js.Array2.map(string => string->Js.String2.trim)
      ->Js.Array2.filter(string => string->Js.String2.trim->Js.String2.length > 0)

    let amountOfWinners = winningNumbers->Js.Array2.reduce((amountOfWinners, winningNumber) => {
      numbers->Js.Array2.includes(winningNumber) ? amountOfWinners + 1 : amountOfWinners
    }, 0)

    Js.Dict.set(memo, cardNumber->Js.String2.make, amountOfWinners)

    for index in cardNumber to cardNumber + amountOfWinners - 1 {
      processCard(cards, cards[index], totalCardsSoFar)
    }
  }
}

let sum = input => {
  let cards =
    input
    ->Js.String2.split("\n")
    ->Js.Array2.filter(line => line->Js.String2.trim->Js.String2.length > 0)

  let totalCardsSoFar = ref(0)
  cards->Js.Array2.forEach(card => processCard(cards, card, totalCardsSoFar))
  totalCardsSoFar.contents
}

sum(input)->Js.log
