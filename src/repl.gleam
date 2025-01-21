import gleam/erlang
import gleam/io
import lexer

pub fn main() {
  let prompt = ">> "

  let assert Ok(line) = erlang.get_line(prompt)
  let tokens = lexer.lex(line)
  io.debug(tokens)

  main()
}
