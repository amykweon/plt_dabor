open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Microcparse.program Scanner.token lexbuf in
  let sprogram = Semant.check program in