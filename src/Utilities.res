@module("node:fs")
external readFileSync: (
  ~path: string,
  [
    | #utf8
    | #ascii
  ],
) => string = "readFileSync"
@module external process: 'a = "process"
@module external path: 'a = "path"

let input = readFileSync(
  ~path=`${path["dirname"](. process["argv"]->Belt.Array.getUnsafe(1))}/input.txt`,
  #utf8,
)
