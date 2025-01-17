import gleam/string
import gleeunit
import lexer
import token

pub fn main() {
  gleeunit.main()
}

fn match_tokens(tokens: List(token.Token), expected: List(token.Token)) {
  case tokens, expected {
    [], [] -> Nil
    [], [_, ..] -> panic as "there are fewer lexed tokens than expected tokens"
    [_, ..], [] -> panic as "there are more lexed tokens than expected tokens"
    [first_token, ..rest_tokens], [first_expected, ..rest_expected]
      if first_token == first_expected
    -> match_tokens(rest_tokens, rest_expected)
    [a, ..], [b, ..] ->
      panic as string.concat([
        "\n\t",
        string.inspect(a),
        "\n\tshould equal \n\t",
        string.inspect(b),
      ])
  }
}

pub fn lexer_test() {
  let input = "1 + 2"
  let expected = [
    token.Token(token.Int, "1"),
    token.Token(token.Plus, "+"),
    token.Token(token.Int, "2"),
  ]

  let tokens = lexer.lex(input)
  match_tokens(tokens, expected)
}
