let input = Utilities.input

let input2 = "Time:      7  15   30
Distance:  9  40  200
"

exception InvalidInput(string)

type race = {time: int, distance: int}

let races = []

let parse = input => {
  let times = []
  let distances = []
  input
  ->Js.String2.split("\n")
  ->Js.Array2.filter(line => line->Js.String2.trim->Js.String2.length > 0)
  ->Js.Array2.forEachi((data, index) => {
    (data->Js.String2.split(":"))[1]
    ->Js.String2.trim
    ->Js.String2.splitByRe(%re("/\s+/"))
    ->Js.Array2.forEach(d => {
      switch d {
      | Some(d) =>
        if index == 0 {
          times->Js.Array2.push(d->Belt.Int.fromString->Belt.Option.getExn)->ignore
        } else {
          distances->Js.Array2.push(d->Belt.Int.fromString->Belt.Option.getExn)->ignore
        }
      | None => raise(InvalidInput("The input is invalid."))
      }
    })
  })

  times->Js.Array2.forEachi((time, index) => {
    races->Js.Array2.push({time, distance: distances[index]})->ignore
  })
}

parse(input)

races
->Js.Array2.map(race => {
  let distances = []

  for t in race.time - 1 downto 1 {
    let speed = race.time - t
    distances->Js.Array2.push(speed * t)->ignore
  }

  distances->Js.Array2.filter(distance => distance > race.distance)->Js.Array2.length
})
->Js.Array2.reduce((result, numberOfWins) => {result * numberOfWins}, 1)
->Js.log
