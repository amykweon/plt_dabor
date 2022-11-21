open Plt_dabor

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program_rule Scanner.token lexbuf in
  (*let sprogram = Semant.check program in*)
  print_endline (Ast.string_of_program program)
