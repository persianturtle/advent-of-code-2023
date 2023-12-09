let input = Utilities.input

let input2 = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

let parse = line => {
  let firstNumber = ref(None)
  let lastNumber = ref(None)

  switch line->Js.String2.trim {
  | "" => 0
  | line =>
    line
    ->Js.String2.split("")
    ->Js.Array2.forEach(char => {
      switch char {
      | d
        if d == "0" ||
        d == "1" ||
        d == "2" ||
        d == "3" ||
        d == "4" ||
        d == "5" ||
        d == "6" ||
        d == "7" ||
        d == "8" ||
        d == "9" =>
        if firstNumber.contents->Belt.Option.isNone {
          firstNumber := Some(d)
        }
        lastNumber := Some(d)
      | _ => ()
      }
    })

    switch (firstNumber.contents, lastNumber.contents) {
    | (Some(first), Some(last)) => `${first}${last}`->Belt.Int.fromString->Belt.Option.getExn

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
