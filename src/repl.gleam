import gleam/erlang
import gleam/io
import lexer

pub fn main() {
  let prompt = ">> "
  let assert Ok(line) = erlang.get_line(prompt)
  repl(line)
}

fn repl(input: String) {
  let prompt = ">> "
  // do the repl thing
  let tokens = lexer.lex(input)
  io.debug(tokens)

  // read the next line
  let assert Ok(line) = erlang.get_line(prompt)
  repl(line)
}
