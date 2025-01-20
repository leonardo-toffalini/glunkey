import gleeunit
import gleeunit/should
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
    [a, ..], [b, ..] -> a |> should.equal(b)
  }
}

pub fn lexer_test() {
  let input = "=+(){},;"
  let expected = [
    token.Token(token.Assign, "="),
    token.Token(token.Plus, "+"),
    token.Token(token.LParen, "("),
    token.Token(token.RParen, ")"),
    token.Token(token.LBrace, "{"),
    token.Token(token.RBrace, "}"),
    token.Token(token.Comma, ","),
    token.Token(token.Semicolon, ";"),
    token.Token(token.EOF, ""),
  ]

  let tokens = lexer.lex(input)
  match_tokens(tokens, expected)

  let input2 =
    "let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5 <= 10 >= 5 == 10 != 5;
if (5 < 10) {
    return true;
} else {
    return false;
}"

  let expected2 = [
    token.Token(token.Let, "let"),
    token.Token(token.Ident, "five"),
    token.Token(token.Assign, "="),
    token.Token(token.Int, "5"),
    token.Token(token.Semicolon, ";"),
    token.Token(token.Let, "let"),
    token.Token(token.Ident, "ten"),
    token.Token(token.Assign, "="),
    token.Token(token.Int, "10"),
    token.Token(token.Semicolon, ";"),
    token.Token(token.Let, "let"),
    token.Token(token.Ident, "add"),
    token.Token(token.Assign, "="),
    token.Token(token.Function, "fn"),
    token.Token(token.LParen, "("),
    token.Token(token.Ident, "x"),
    token.Token(token.Comma, ","),
    token.Token(token.Ident, "y"),
    token.Token(token.RParen, ")"),
    token.Token(token.LBrace, "{"),
    token.Token(token.Ident, "x"),
    token.Token(token.Plus, "+"),
    token.Token(token.Ident, "y"),
    token.Token(token.Semicolon, ";"),
    token.Token(token.RBrace, "}"),
    token.Token(token.Semicolon, ";"),
    token.Token(token.Let, "let"),
    token.Token(token.Ident, "result"),
    token.Token(token.Assign, "="),
    token.Token(token.Ident, "add"),
    token.Token(token.LParen, "("),
    token.Token(token.Ident, "five"),
    token.Token(token.Comma, ","),
    token.Token(token.Ident, "ten"),
    token.Token(token.RParen, ")"),
    token.Token(token.Semicolon, ";"),
    token.Token(token.Bang, "!"),
    token.Token(token.Minus, "-"),
    token.Token(token.Slash, "/"),
    token.Token(token.Star, "*"),
    token.Token(token.Int, "5"),
    token.Token(token.Semicolon, ";"),
    token.Token(token.Int, "5"),
    token.Token(token.Lt, "<"),
    token.Token(token.Int, "10"),
    token.Token(token.Gt, ">"),
    token.Token(token.Int, "5"),
    token.Token(token.Le, "<="),
    token.Token(token.Int, "10"),
    token.Token(token.Ge, ">="),
    token.Token(token.Int, "5"),
    token.Token(token.Eq, "=="),
    token.Token(token.Int, "10"),
    token.Token(token.Neq, "!="),
    token.Token(token.Int, "5"),
    token.Token(token.Semicolon, ";"),
    token.Token(token.If, "if"),
    token.Token(token.LParen, "("),
    token.Token(token.Int, "5"),
    token.Token(token.Lt, "<"),
    token.Token(token.Int, "10"),
    token.Token(token.RParen, ")"),
    token.Token(token.LBrace, "{"),
    token.Token(token.Return, "return"),
    token.Token(token.TRUE, "true"),
    token.Token(token.Semicolon, ";"),
    token.Token(token.RBrace, "}"),
    token.Token(token.Else, "else"),
    token.Token(token.LBrace, "{"),
    token.Token(token.Return, "return"),
    token.Token(token.FALSE, "false"),
    token.Token(token.Semicolon, ";"),
    token.Token(token.RBrace, "}"),
    token.Token(token.EOF, ""),
  ]

  let tokens2 = lexer.lex(input2)
  match_tokens(tokens2, expected2)
}
