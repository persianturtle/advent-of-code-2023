// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utilities from "../Utilities.bs.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";

function parse(line) {
  var line$1 = line.trim();
  if (line$1 === "") {
    return 0;
  }
  var winningNumbers = Caml_array.get(Caml_array.get(line$1.split("|"), 0).split(":"), 1).split(" ").map(function (string) {
          return string.trim();
        }).filter(function (string) {
        return string.trim().length > 0;
      });
  var numbers = Caml_array.get(line$1.split("|"), 1).split(" ").map(function (string) {
          return string.trim();
        }).filter(function (string) {
        return string.trim().length > 0;
      });
  var amountOfWinners = winningNumbers.reduce((function (amountOfWinners, winningNumber) {
          if (numbers.includes(winningNumber)) {
            return amountOfWinners + 1 | 0;
          } else {
            return amountOfWinners;
          }
        }), 0);
  return Math.pow(2.0, amountOfWinners - 1 | 0) | 0;
}

function sum(input) {
  return input.split("\n").map(parse).reduce((function (sum, n) {
                return sum + n | 0;
              }), 0);
}

console.log(sum(Utilities.input));

var input = Utilities.input;

var input2 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";

export {
  input ,
  input2 ,
  parse ,
  sum ,
}
/*  Not a pure module */
