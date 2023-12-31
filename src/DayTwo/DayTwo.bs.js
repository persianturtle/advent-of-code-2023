// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Utilities from "../Utilities.bs.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function parse(line) {
  var line$1 = line.trim();
  if (line$1 === "") {
    return 0;
  }
  var gameNumber = Belt_Option.getWithDefault(Belt_Int.fromString(Caml_array.get(line$1.split(":"), 0).replace(/\D/g, "")), 0);
  var isPossible = Caml_array.get(line$1.split(":"), 1).split(";").reduce((function (isPossible, game) {
          var state = {};
          [
              "red",
              "green",
              "blue"
            ].forEach(function (color) {
                state[color] = game.includes(color) ? Belt_Option.getWithDefault(Belt_Int.fromString(game.replace(new RegExp(".*?(\\d+)\\s" + color + ".*"), "$1")), 0) : 0;
              });
          if (isPossible && 12 >= Belt_Option.getWithDefault(Js_dict.get(state, "red"), 0) && 13 >= Belt_Option.getWithDefault(Js_dict.get(state, "green"), 0)) {
            return 14 >= Belt_Option.getWithDefault(Js_dict.get(state, "blue"), 0);
          } else {
            return false;
          }
        }), true);
  if (isPossible) {
    return gameNumber;
  } else {
    return 0;
  }
}

function sum(input) {
  return input.split("\n").map(parse).reduce((function (sum, n) {
                return sum + n | 0;
              }), 0);
}

console.log(sum(Utilities.input));

var input = Utilities.input;

var input2 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";

var bag = {
  red: 12,
  green: 13,
  blue: 14
};

export {
  input ,
  input2 ,
  bag ,
  parse ,
  sum ,
}
/*  Not a pure module */
