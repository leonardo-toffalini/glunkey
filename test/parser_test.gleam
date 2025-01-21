import parser
import gleam/io
import lexer
import gleeunit

pub fn main() {
  gleeunit.main()
}

pub fn let_stmt_test() {
  let input = "
let x = 5;
let y = 10;
let foobar = 838383;"

  let tokens = lexer.lex(input)
  let program = parser.parse(tokens)
  io.debug("tokens:")
  io.debug(tokens)
  io.debug("program:")
  io.debug(program)
}
