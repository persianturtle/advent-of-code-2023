let input = Utilities.input

let input2 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

type bag = {
  red: int,
  green: int,
  blue: int,
}

let bag = {red: 12, green: 13, blue: 14}

let parse = line => {
  switch line->Js.String2.trim {
  | "" => 0
  | line =>
    let gameNumber =
      (line->Js.String2.split(":"))[0]
      ->Js.String2.replaceByRe(%re("/\D/g"), "")
      ->Belt.Int.fromString
      ->Belt.Option.getWithDefault(0)

    let isPossible =
      (line->Js.String2.split(":"))[1]
      ->Js.String2.split(";")
      ->Js.Array2.reduce((isPossible, game) => {
        let state = Js.Dict.empty()

        ["red", "green", "blue"]->Js.Array2.forEach(color => {
          Js.Dict.set(
            state,
            color,
            game->Js.String2.includes(color)
              ? game
                ->Js.String2.replaceByRe(Js.Re.fromString(`.*?(\\d+)\\s${color}.*`), "$1")
                ->Belt.Int.fromString
                ->Belt.Option.getWithDefault(0)
              : 0,
          )
        })

        isPossible &&
        bag.red >= Js.Dict.get(state, "red")->Belt.Option.getWithDefault(0) &&
        bag.green >= Js.Dict.get(state, "green")->Belt.Option.getWithDefault(0) &&
        bag.blue >= Js.Dict.get(state, "blue")->Belt.Option.getWithDefault(0)
      }, true)

    isPossible ? gameNumber : 0
  }
}

let sum = input => {
  input->Js.String2.split("\n")->Js.Array2.map(parse)->Js.Array2.reduce((sum, n) => sum + n, 0)
}

sum(input)->Js.log
