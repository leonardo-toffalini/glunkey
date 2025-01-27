import ast
import gleam/erlang
import gleam/io
import lexer
import parser

pub fn main() {
  let prompt = ">> "

  let assert Ok(line) = erlang.get_line(prompt)
  let tokens = lexer.lex(line)
  io.debug("Tokens:")
  io.debug(tokens)

  let program = parser.parse(tokens)
  io.debug("Program:")
  case program {
    Ok(program) -> ast.print_program(program)
    Error(msg) -> io.print("Error: " <> msg)
  }
  io.debug("AST:")
  let _ = io.debug(program)

  main()
}
