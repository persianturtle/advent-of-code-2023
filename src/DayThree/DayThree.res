let input = Utilities.input

let input2 = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

let isValidPartNumber = (lines, (lineIndex, characterIndex)) => {
  [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
  ->Js.Array2.reduce((characters, (lineIndexOffset, characterIndexOffset)) => {
    characters ++
    lines
    ->Belt.Array.get(lineIndex + lineIndexOffset)
    ->Belt.Option.getWithDefault("")
    ->Js.String2.charAt(characterIndex + characterIndexOffset)
  }, "")
  ->Js.String2.replaceByRe(%re("/[0-9.]/g"), "")
  ->Js.String2.length > 0
}

let parse = lines => {
  let validPartNumbers = ref([])
  let state = ref(("", false))

  lines->Js.Array2.forEachi((line, lineIndex) => {
    line
    ->Js.String2.split("")
    ->Js.Array2.forEachi((character, characterIndex) => {
      if Js.Re.test_(%re("/\d/"), character) {
        let (numberSoFar, isNumberValidSoFar) = state.contents
        state :=
          (
            `${numberSoFar}${character}`,
            isNumberValidSoFar || isValidPartNumber(lines, (lineIndex, characterIndex)),
          )

        let (numberSoFar, isNumberValidSoFar) = state.contents
        let nextCharacter =
          lines
          ->Belt.Array.get(lineIndex)
          ->Belt.Option.getWithDefault("")
          ->Js.String2.charAt(characterIndex + 1)

        if !Js.Re.test_(%re("/[0-9]/"), nextCharacter) && isNumberValidSoFar {
          validPartNumbers.contents->Js.Array2.push(numberSoFar)->ignore
        }
      } else {
        state := ("", false)
      }
    })
  })

  validPartNumbers.contents->Js.Array2.map(string =>
    string->Belt.Int.fromString->Belt.Option.getWithDefault(0)
  )
}

let sum = input => {
  parse(
    input->Js.String2.split("\n")->Js.Array2.filter(line => line->Js.String2.length > 0),
  )->Js.Array2.reduce((sum, n) => sum + n, 0)
}

sum(input)->Js.log
