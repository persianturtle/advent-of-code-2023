let input = Utilities.input

let input2 = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"

exception InvalidInput(string)

let map = Js.Dict.empty()
let instructions = ref("")
let location = ref("AAA")
let count = ref(0)

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

while location.contents != "ZZZ" {
  instructions.contents
  ->Js.String2.split("")
  ->Js.Array2.forEach(instruction => {
    let (left, right) = map->Js.Dict.get(location.contents)->Belt.Option.getExn

    switch instruction {
    | "L" => location := left
    | "R" => location := right
    | _ => raise(InvalidInput("The instructions are invalid."))
    }

    count := count.contents + 1
  })
}

Js.log(count.contents)
