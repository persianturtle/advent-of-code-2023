let input = Utilities.input

let input2 = "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

let getCharacterAtIndex = (lines, (lineIndex, characterIndex)) => {
  lines
  ->Belt.Array.get(lineIndex)
  ->Belt.Option.getWithDefault("")
  ->Js.String2.charAt(characterIndex)
}

let indexesOfNumbersAroundAsterisks = (lines, (lineIndex, characterIndex)) => {
  let indexes = ref([])

  [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]->Js.Array2.forEach(((
    lineIndexOffset,
    characterIndexOffset,
  )) => {
    let character = getCharacterAtIndex(
      lines,
      (lineIndex + lineIndexOffset, characterIndex + characterIndexOffset),
    )

    if Js.Re.test_(%re("/\d/"), character) {
      indexes.contents
      ->Js.Array2.push((lineIndex + lineIndexOffset, characterIndex + characterIndexOffset))
      ->ignore
    }
  })

  indexes.contents->Js.Array2.filteri(((lineIndex, characterIndex), index) => {
    switch indexes.contents->Belt.Array.get(index - 1) {
    | Some((previousLineIndex, previousCharacterIndex)) =>
      !(previousLineIndex === lineIndex && characterIndex - 1 === previousCharacterIndex)
    | None => true
    }
  })
}

let getNumberFromIndex = (lines, (lineIndex, characterIndex)) => {
  let characterIndexRef = ref(characterIndex)
  let number = ref(getCharacterAtIndex(lines, (lineIndex, characterIndexRef.contents)))

  while (
    Js.Re.test_(
      %re("/\d/"),
      getCharacterAtIndex(lines, (lineIndex, characterIndexRef.contents + 1)),
    )
  ) {
    number :=
      `${number.contents}${getCharacterAtIndex(lines, (lineIndex, characterIndexRef.contents + 1))}`
    characterIndexRef := characterIndexRef.contents + 1
  }

  characterIndexRef := characterIndex
  while (
    Js.Re.test_(
      %re("/\d/"),
      getCharacterAtIndex(lines, (lineIndex, characterIndexRef.contents - 1)),
    )
  ) {
    number :=
      `${getCharacterAtIndex(lines, (lineIndex, characterIndexRef.contents - 1))}${number.contents}`
    characterIndexRef := characterIndexRef.contents - 1
  }

  number.contents
}

let parse = lines => {
  let possibleGears = ref([])

  lines->Js.Array2.forEachi((line, lineIndex) => {
    line
    ->Js.String2.split("")
    ->Js.Array2.forEachi((character, characterIndex) => {
      if Js.Re.test_(%re("/\*/"), character) {
        let possibleGear = indexesOfNumbersAroundAsterisks(lines, (lineIndex, characterIndex))

        if possibleGear->Js.Array2.length > 1 {
          possibleGears.contents->Js.Array2.push(possibleGear)->ignore
        }
      }
    })
  })

  possibleGears.contents
  ->Js.Array2.map(indexes =>
    indexes->Js.Array2.map(index => {
      getNumberFromIndex(lines, index)->Belt.Int.fromString->Belt.Option.getWithDefault(1)
    })
  )
  ->Js.Array2.filter(numbers => numbers->Js.Array2.length === 2)
  ->Js.Array2.map(numbers => numbers->Js.Array2.reduce((product, number) => product * number, 1))
}

let sum = input => {
  parse(
    input->Js.String2.split("\n")->Js.Array2.filter(line => line->Js.String2.length > 0),
  )->Js.Array2.reduce((sum, n) => sum + n, 0)
}

sum(input)->Js.log
