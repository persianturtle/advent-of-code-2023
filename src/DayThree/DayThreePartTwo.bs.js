// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Utilities from "../Utilities.bs.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function getCharacterAtIndex(lines, param) {
  return Belt_Option.getWithDefault(Belt_Array.get(lines, param[0]), "").charAt(param[1]);
}

function indexesOfNumbersAroundAsterisks(lines, param) {
  var characterIndex = param[1];
  var lineIndex = param[0];
  var indexes = {
    contents: []
  };
  [
      [
        -1,
        -1
      ],
      [
        -1,
        0
      ],
      [
        -1,
        1
      ],
      [
        0,
        -1
      ],
      [
        0,
        1
      ],
      [
        1,
        -1
      ],
      [
        1,
        0
      ],
      [
        1,
        1
      ]
    ].forEach(function (param) {
        var characterIndexOffset = param[1];
        var lineIndexOffset = param[0];
        var character = getCharacterAtIndex(lines, [
              lineIndex + lineIndexOffset | 0,
              characterIndex + characterIndexOffset | 0
            ]);
        if (/\d/.test(character)) {
          indexes.contents.push([
                lineIndex + lineIndexOffset | 0,
                characterIndex + characterIndexOffset | 0
              ]);
          return ;
        }
        
      });
  return indexes.contents.filter(function (param, index) {
              var match = Belt_Array.get(indexes.contents, index - 1 | 0);
              if (match !== undefined) {
                return !(match[0] === param[0] && (param[1] - 1 | 0) === match[1]);
              } else {
                return true;
              }
            });
}

function getNumberFromIndex(lines, param) {
  var characterIndex = param[1];
  var lineIndex = param[0];
  var characterIndexRef = characterIndex;
  var number = getCharacterAtIndex(lines, [
        lineIndex,
        characterIndexRef
      ]);
  while(/\d/.test(getCharacterAtIndex(lines, [
              lineIndex,
              characterIndexRef + 1 | 0
            ]))) {
    number = "" + number + "" + getCharacterAtIndex(lines, [
          lineIndex,
          characterIndexRef + 1 | 0
        ]) + "";
    characterIndexRef = characterIndexRef + 1 | 0;
  };
  characterIndexRef = characterIndex;
  while(/\d/.test(getCharacterAtIndex(lines, [
              lineIndex,
              characterIndexRef - 1 | 0
            ]))) {
    number = "" + getCharacterAtIndex(lines, [
          lineIndex,
          characterIndexRef - 1 | 0
        ]) + "" + number + "";
    characterIndexRef = characterIndexRef - 1 | 0;
  };
  return number;
}

function parse(lines) {
  var possibleGears = {
    contents: []
  };
  lines.forEach(function (line, lineIndex) {
        line.split("").forEach(function (character, characterIndex) {
              if (!/\*/.test(character)) {
                return ;
              }
              var possibleGear = indexesOfNumbersAroundAsterisks(lines, [
                    lineIndex,
                    characterIndex
                  ]);
              if (possibleGear.length > 1) {
                possibleGears.contents.push(possibleGear);
                return ;
              }
              
            });
      });
  return possibleGears.contents.map(function (indexes) {
                  return indexes.map(function (index) {
                              return Belt_Option.getWithDefault(Belt_Int.fromString(getNumberFromIndex(lines, index)), 1);
                            });
                }).filter(function (numbers) {
                return numbers.length === 2;
              }).map(function (numbers) {
              return numbers.reduce((function (product, number) {
                            return Math.imul(product, number);
                          }), 1);
            });
}

function sum(input) {
  return parse(input.split("\n").filter(function (line) {
                    return line.length > 0;
                  })).reduce((function (sum, n) {
                return sum + n | 0;
              }), 0);
}

console.log(sum(Utilities.input));

var input = Utilities.input;

var input2 = "\n467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..";

export {
  input ,
  input2 ,
  getCharacterAtIndex ,
  indexesOfNumbersAroundAsterisks ,
  getNumberFromIndex ,
  parse ,
  sum ,
}
/*  Not a pure module */