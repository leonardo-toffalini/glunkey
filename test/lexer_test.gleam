import gleeunit
import gleeunit/should
import lexer

pub fn main() {
  gleeunit.main()
}

pub fn lexer_test() {
  let input = "1 + 2"

  let l = lexer.New(input)

  1
  |> should.equal(1)
}
