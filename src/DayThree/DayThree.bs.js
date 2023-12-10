// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Utilities from "../Utilities.bs.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function isValidPartNumber(lines, param) {
  var characterIndex = param[1];
  var lineIndex = param[0];
  return [
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
            ].reduce((function (characters, param) {
                  return characters + Belt_Option.getWithDefault(Belt_Array.get(lines, lineIndex + param[0] | 0), "").charAt(characterIndex + param[1] | 0);
                }), "").replace(/[0-9.]/g, "").length > 0;
}

function parse(lines) {
  var validPartNumbers = {
    contents: []
  };
  var state = {
    contents: [
      "",
      false
    ]
  };
  lines.forEach(function (line, lineIndex) {
        line.split("").forEach(function (character, characterIndex) {
              if (!/\d/.test(character)) {
                state.contents = [
                  "",
                  false
                ];
                return ;
              }
              var match = state.contents;
              state.contents = [
                "" + match[0] + "" + character + "",
                match[1] || isValidPartNumber(lines, [
                      lineIndex,
                      characterIndex
                    ])
              ];
              var match$1 = state.contents;
              var nextCharacter = Belt_Option.getWithDefault(Belt_Array.get(lines, lineIndex), "").charAt(characterIndex + 1 | 0);
              if (!/[0-9]/.test(nextCharacter) && match$1[1]) {
                validPartNumbers.contents.push(match$1[0]);
                return ;
              }
              
            });
      });
  return validPartNumbers.contents.map(function (string) {
              return Belt_Option.getWithDefault(Belt_Int.fromString(string), 0);
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

var input2 = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..";

export {
  input ,
  input2 ,
  isValidPartNumber ,
  parse ,
  sum ,
}
/*  Not a pure module */