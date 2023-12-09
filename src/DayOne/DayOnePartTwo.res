let input = Utilities.input

let input2 = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

let input3 = "gsjgklneight6zqfz"

let parse = line => {
  switch line->Js.String2.trim {
  | "" => 0
  | line =>
    let numbers = [
      ("0", "0"),
      ("1", "1"),
      ("2", "2"),
      ("3", "3"),
      ("4", "4"),
      ("5", "5"),
      ("6", "6"),
      ("7", "7"),
      ("8", "8"),
      ("9", "9"),
      ("one", "1"),
      ("two", "2"),
      ("three", "3"),
      ("four", "4"),
      ("five", "5"),
      ("six", "6"),
      ("seven", "7"),
      ("eight", "8"),
      ("nine", "9"),
    ]

    let indexes =
      numbers->Js.Array2.map(((match, number)) => (
        number,
        line->Js.String2.indexOf(match),
        line->Js.String2.lastIndexOf(match),
      ))

    let first = ref((None, 1_000_000))
    let last = ref((None, -1_000_000))

    indexes->Js.Array2.forEach(((number, currentFirstIndex, currentLastIndex)) => {
      let (_, firstIndexSoFar) = first.contents
      let (_, lastIndexSoFar) = last.contents

      if currentFirstIndex > -1 && currentFirstIndex < firstIndexSoFar {
        first := (Some(number), currentFirstIndex)
      }

      if currentLastIndex > lastIndexSoFar {
        last := (Some(number), currentLastIndex)
      }
    })

    switch (first.contents, last.contents) {
    | ((Some(first), _), (Some(last), _)) =>
      `${first}${last}`->Belt.Int.fromString->Belt.Option.getWithDefault(0)

    | _ =>
      Js.log(`Failed to parse line: ${line}`)
      0
    }
  }
}

let sum = input => {
  input->Js.String2.split("\n")->Js.Array2.map(parse)->Js.Array2.reduce((sum, n) => sum + n, 0)
}

sum(input)->Js.log
