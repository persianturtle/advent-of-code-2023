let input = Utilities.input

let input2 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

type bag = {
  mutable red: int,
  mutable green: int,
  mutable blue: int,
}

let parse = line => {
  switch line->Js.String2.trim {
  | "" => 0
  | line =>
    let requiredBagSize =
      (line->Js.String2.split(":"))[1]
      ->Js.String2.split(";")
      ->Js.Array2.reduce((requiredBagSize, game) => {
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

        switch (
          Js.Dict.get(state, "red"),
          Js.Dict.get(state, "green"),
          Js.Dict.get(state, "blue"),
        ) {
        | (Some(red), Some(green), Some(blue)) =>
          if red > requiredBagSize.red {
            requiredBagSize.red = red
          }

          if green > requiredBagSize.green {
            requiredBagSize.green = green
          }

          if blue > requiredBagSize.blue {
            requiredBagSize.blue = blue
          }

        | _ => Js.log(`Error parsing game: ${game}`)
        }

        requiredBagSize
      }, {red: 0, green: 0, blue: 0})

    requiredBagSize.red * requiredBagSize.green * requiredBagSize.blue
  }
}

let sum = input => {
  input->Js.String2.split("\n")->Js.Array2.map(parse)->Js.Array2.reduce((sum, n) => sum + n, 0)
}

sum(input)->Js.log
