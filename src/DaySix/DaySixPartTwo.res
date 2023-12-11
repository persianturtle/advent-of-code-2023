let input = Utilities.input

let input2 = "Time:      7  15   30
Distance:  9  40  200
"

exception InvalidInput(string)

type race = {time: int, distance: float}

let race = switch input
->Js.String2.split("\n")
->Js.Array2.filter(line => line->Js.String2.trim->Js.String2.length > 0)
->Js.Array2.map(data => {
  (data->Js.String2.split(":"))[1]
  ->Js.String2.trim
  ->Js.String2.replaceByRe(%re("/\D/g"), "")
  ->Belt.Float.fromString
  ->Belt.Option.getExn
}) {
| [time, distance] => {time: time->Belt.Int.fromFloat, distance}
| _ => raise(InvalidInput("The input was invalid."))
}

let distances = []

for t in race.time - 1 downto 1 {
  let speed = (race.time - t)->Belt.Float.fromInt
  distances->Js.Array2.push(speed *. t->Belt.Float.fromInt)->ignore
}

distances->Js.Array2.filter(distance => distance > race.distance)->Js.Array2.length->Js.log
