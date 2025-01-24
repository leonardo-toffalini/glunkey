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
  io.debug("AST:")
  let _ = io.debug(program)

  main()
}
