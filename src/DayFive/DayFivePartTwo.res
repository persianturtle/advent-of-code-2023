/**
 * Translated from here: https://www.youtube.com/watch?v=NmxHw_bHhGM
 */
let input = Utilities.input

let input2 = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

exception InvalidInput(string)

let seeds = ref([])
let ranges = []

let convertSpaceSeparateNumbersToFloat = string =>
  string
  ->Js.String2.split(" ")
  ->Js.Array2.map(number => number->Belt.Float.fromString->Belt.Option.getWithDefault(0.0))

input
->Js.String2.split("\n\n")
->Js.Array2.forEachi((line, index) => {
  if index == 0 {
    let data = (line->Js.String2.split(": "))[1]->convertSpaceSeparateNumbersToFloat

    data->Js.Array2.forEachi((seed, index) => {
      if mod(index, 2) == 0 {
        seeds.contents->Js.Array2.push((seed, seed +. data[index + 1]))->ignore
      }
    })
  } else {
    ranges
    ->Js.Array2.push(
      (line->Js.String2.split(":\n"))[1]
      ->Js.String2.split("\n")
      ->Js.Array2.filter(line => line->Js.String2.trim->Js.String2.length > 0)
      ->Js.Array2.map(convertSpaceSeparateNumbersToFloat)
      ->Js.Array2.map(map => {
        switch map {
        | [a, b, c] => (a, b, c)
        | _ => raise(InvalidInput("The input was invalid."))
        }
      }),
    )
    ->ignore
  }
})

ranges->Js.Array2.forEach(range => {
  let new = []

  while seeds.contents->Js.Array2.length > 0 {
    let (s, e) = seeds.contents->Js.Array2.pop->Belt.Option.getExn

    let isMatched = ref(false)
    range->Js.Array2.forEach(((a, b, c)) => {
      let os = Js.Math.max_float(s, b)
      let oe = Js.Math.min_float(e, b +. c)

      if os < oe {
        new->Js.Array2.push((os -. b +. a, oe -. b +. a))->ignore
        if os > s {
          seeds.contents->Js.Array2.push((s, os))->ignore
        }
        if e > oe {
          seeds.contents->Js.Array2.push((oe, e))->ignore
        }

        isMatched := true
      }
    })

    if !isMatched.contents {
      new->Js.Array2.push((s, e))->ignore
    }
  }
  seeds := new
})

Js.log(seeds.contents->Js.Array2.map(((start, _)) => start)->Js.Math.minMany_float)
