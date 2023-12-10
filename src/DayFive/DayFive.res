type rec state = {
  mutable seeds: array<float>,
  seedsToSoil: array<(float, float, float)>,
  soilToFertilizer: array<(float, float, float)>,
  fertilizerToWater: array<(float, float, float)>,
  waterToLight: array<(float, float, float)>,
  lightToTemperature: array<(float, float, float)>,
  temperatureToHumidity: array<(float, float, float)>,
  humidityToLocation: array<(float, float, float)>,
}

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

let parseCategory = data => {
  switch data
  ->Js.String2.split(" ")
  ->Js.Array2.map(number => number->Belt.Float.fromString->Belt.Option.getWithDefault(0.0)) {
  | [sourceCategory, destinationCategory, range] => (sourceCategory, destinationCategory, range)
  | _ => (0.0, 0.0, 0.0)
  }
}

let populateState = ((currentField, state), line) => {
  let field =
    line->Js.String2.includes(":")
      ? (line->Js.String2.split(":"))[0]->Js.String2.trim
      : currentField
  let data =
    (line->Js.String2.includes(":") ? (line->Js.String2.split(":"))[1] : line)->Js.String2.trim

  let currentField = switch field {
  | "seeds" =>
    state.seeds =
      data
      ->Js.String2.split(" ")
      ->Js.Array2.filter(string => string->Js.String2.trim->Js.String2.length > 0)
      ->Js.Array2.map(number => number->Belt.Float.fromString->Belt.Option.getWithDefault(0.0))
    "seeds"

  | "seed-to-soil map" =>
    if data->Js.String2.length > 0 {
      state.seedsToSoil->Js.Array2.push(parseCategory(data))->ignore
    }
    "seed-to-soil map"

  | "soil-to-fertilizer map" =>
    if data->Js.String2.length > 0 {
      state.soilToFertilizer->Js.Array2.push(parseCategory(data))->ignore
    }
    "soil-to-fertilizer map"

  | "fertilizer-to-water map" =>
    if data->Js.String2.length > 0 {
      state.fertilizerToWater->Js.Array2.push(parseCategory(data))->ignore
    }
    "fertilizer-to-water map"

  | "water-to-light map" =>
    if data->Js.String2.length > 0 {
      state.waterToLight->Js.Array2.push(parseCategory(data))->ignore
    }
    "water-to-light map"

  | "light-to-temperature map" =>
    if data->Js.String2.length > 0 {
      state.lightToTemperature->Js.Array2.push(parseCategory(data))->ignore
    }
    "light-to-temperature map"

  | "temperature-to-humidity map" =>
    if data->Js.String2.length > 0 {
      state.temperatureToHumidity->Js.Array2.push(parseCategory(data))->ignore
    }
    "temperature-to-humidity map"

  | "humidity-to-location map" =>
    if data->Js.String2.length > 0 {
      state.humidityToLocation->Js.Array2.push(parseCategory(data))->ignore
    }
    "humidity-to-location map"

  | _ => currentField
  }

  (currentField, state)
}

let map = (number, map) => {
  let (number, _) = map->Js.Array2.reduce(((number, isMapped), (destination, source, range)) => {
    if isMapped {
      (number, true)
    } else if number < source || number > source +. range {
      (number, false)
    } else {
      (number +. (destination -. source), true)
    }
  }, (number, false))

  number
}

let calculateLowestLocation = input => {
  let (_, state) =
    input
    ->Js.String2.split("\n")
    ->Js.Array2.filter(line => line->Js.String2.trim->Js.String2.length > 0)
    ->Js.Array2.reduce(
      populateState,
      (
        "",
        {
          seeds: [],
          seedsToSoil: [],
          soilToFertilizer: [],
          fertilizerToWater: [],
          waterToLight: [],
          lightToTemperature: [],
          temperatureToHumidity: [],
          humidityToLocation: [],
        },
      ),
    )

  state.seeds
  ->Js.Array2.map(seed => {
    seed
    ->map(state.seedsToSoil)
    ->map(state.soilToFertilizer)
    ->map(state.fertilizerToWater)
    ->map(state.waterToLight)
    ->map(state.lightToTemperature)
    ->map(state.temperatureToHumidity)
    ->map(state.humidityToLocation)
  })
  ->Js.Math.minMany_float
}

calculateLowestLocation(input)->Js.log
