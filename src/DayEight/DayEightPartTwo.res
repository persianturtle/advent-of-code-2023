let input = Utilities.input

let input2 = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"

exception InvalidInput(string)

let map = Js.Dict.empty()
let instructions = ref("")
let locations = ref([])
let count = ref(0.0)

input
->Js.String2.split("\n")
->Js.Array2.filter(line => line->Js.String2.trim->Js.String2.length > 0)
->Js.Array2.forEachi((line, index) => {
  if index == 0 {
    instructions := line->Js.String2.trim
  } else {
    switch line
    ->Js.String2.replace(" =", "")
    ->Js.String2.replaceByRe(%re("/[^A-Z ]/g"), "")
    ->Js.String2.split(" ") {
    | [location, left, right] => map->Js.Dict.set(location, (left, right))
    | _ => raise(InvalidInput("The input is invalid."))
    }
  }
})

locations := map->Js.Dict.keys->Js.Array2.filter(location => location->Js.String2.endsWith("A"))

while !(locations.contents->Js.Array2.every(location => location->Js.String2.endsWith("Z"))) {
  instructions.contents
  ->Js.String2.split("")
  ->Js.Array2.forEach(instruction => {
    locations :=
      locations.contents->Js.Array2.map(location => {
        let (left, right) = map->Js.Dict.get(location)->Belt.Option.getExn

        switch instruction {
        | "L" => left
        | "R" => right
        | _ => raise(InvalidInput("The instructions are invalid."))
        }
      })

    count := count.contents +. 1.0
  })
}

Js.log(count.contents)
