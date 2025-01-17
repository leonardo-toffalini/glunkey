import gleam/list
import token

pub fn lex(input: String) -> List(token.Token) {
  do_lex(input, [])
}

fn do_lex(input: String, acc: List(token.Token)) -> List(token.Token) {
  case input {
    "" -> acc |> list.reverse

    // whitespace
    " " <> rest | "\n" <> rest | "\t" <> rest | "\r" <> rest ->
      do_lex(rest, acc)

    // operators
    "+" <> rest -> do_lex(rest, [token.Token(token.Plus, "+"), ..acc])
    "-" <> rest -> do_lex(rest, [token.Token(token.Minus, "-"), ..acc])

    // groupings
    "(" <> rest -> do_lex(rest, [token.Token(token.LParen, "("), ..acc])
    ")" <> rest -> do_lex(rest, [token.Token(token.RParen, ")"), ..acc])
    "[" <> rest -> do_lex(rest, [token.Token(token.LBracket, "["), ..acc])
    "]" <> rest -> do_lex(rest, [token.Token(token.RBracket, "]"), ..acc])
    "{" <> rest -> do_lex(rest, [token.Token(token.LBrace, "{"), ..acc])
    "}" <> rest -> do_lex(rest, [token.Token(token.RBrace, "}"), ..acc])

    // numbers
    // todo: currently only single digit numbers are lexed
    "1" as c <> rest
    | "2" as c <> rest
    | "3" as c <> rest
    | "4" as c <> rest
    | "5" as c <> rest
    | "6" as c <> rest
    | "7" as c <> rest
    | "8" as c <> rest
    | "9" as c <> rest -> do_lex(rest, [token.Token(token.Int, c), ..acc])
    _ -> [token.Token(token.Illegal, "Illegal"), ..acc] |> list.reverse
  }
}
