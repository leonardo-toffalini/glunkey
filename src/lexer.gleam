import gleam/list
import gleam/string
import token

pub fn lex(input: String) -> List(token.Token) {
  do_lex(input, [])
}

fn do_lex(input: String, acc: List(token.Token)) -> List(token.Token) {
  case input {
    "" -> [token.Token(token.EOF, ""), ..acc] |> list.reverse

    // whitespace
    " " <> rest | "\n" <> rest | "\t" <> rest | "\r" <> rest ->
      do_lex(rest, acc)

    // operators
    "==" <> rest -> do_lex(rest, [token.Token(token.Eq, "=="), ..acc])
    "!=" <> rest -> do_lex(rest, [token.Token(token.Neq, "!="), ..acc])
    "<=" <> rest -> do_lex(rest, [token.Token(token.Le, "<="), ..acc])
    ">=" <> rest -> do_lex(rest, [token.Token(token.Ge, ">="), ..acc])
    "<" <> rest -> do_lex(rest, [token.Token(token.Lt, "<"), ..acc])
    ">" <> rest -> do_lex(rest, [token.Token(token.Gt, ">"), ..acc])

    "=" <> rest -> do_lex(rest, [token.Token(token.Assign, "="), ..acc])
    "+" <> rest -> do_lex(rest, [token.Token(token.Plus, "+"), ..acc])
    "-" <> rest -> do_lex(rest, [token.Token(token.Minus, "-"), ..acc])
    "!" <> rest -> do_lex(rest, [token.Token(token.Bang, "!"), ..acc])
    "*" <> rest -> do_lex(rest, [token.Token(token.Star, "*"), ..acc])
    "/" <> rest -> do_lex(rest, [token.Token(token.Slash, "/"), ..acc])

    // delimiters
    "," <> rest -> do_lex(rest, [token.Token(token.Comma, ","), ..acc])
    ";" <> rest -> do_lex(rest, [token.Token(token.Semicolon, ";"), ..acc])

    // groupings
    "(" <> rest -> do_lex(rest, [token.Token(token.LParen, "("), ..acc])
    ")" <> rest -> do_lex(rest, [token.Token(token.RParen, ")"), ..acc])
    "{" <> rest -> do_lex(rest, [token.Token(token.LBrace, "{"), ..acc])
    "}" <> rest -> do_lex(rest, [token.Token(token.RBrace, "}"), ..acc])

    // numbers
    "1" as c <> rest
    | "2" as c <> rest
    | "3" as c <> rest
    | "4" as c <> rest
    | "5" as c <> rest
    | "6" as c <> rest
    | "7" as c <> rest
    | "8" as c <> rest
    | "9" as c <> rest -> {
      let #(lit, r) = lex_number(c <> rest)
      do_lex(r, [token.Token(token.Int, lit), ..acc])
    }

    "a" as c <> rest
    | "b" as c <> rest
    | "c" as c <> rest
    | "d" as c <> rest
    | "e" as c <> rest
    | "f" as c <> rest
    | "g" as c <> rest
    | "h" as c <> rest
    | "i" as c <> rest
    | "j" as c <> rest
    | "k" as c <> rest
    | "l" as c <> rest
    | "m" as c <> rest
    | "n" as c <> rest
    | "o" as c <> rest
    | "p" as c <> rest
    | "q" as c <> rest
    | "r" as c <> rest
    | "s" as c <> rest
    | "t" as c <> rest
    | "u" as c <> rest
    | "v" as c <> rest
    | "w" as c <> rest
    | "x" as c <> rest
    | "y" as c <> rest
    | "z" as c <> rest
    | "_" as c <> rest -> {
      let #(lit, r) = lex_ident(c <> rest)
      let tt = token.look_up_ident(lit)
      do_lex(r, [token.Token(tt, lit), ..acc])
    }

    _ -> [token.Token(token.Illegal, "Illegal"), ..acc] |> list.reverse
  }
}

fn lex_ident(input: String) -> #(String, String) {
  read_while(input, is_letter)
}

fn lex_number(input: String) -> #(String, String) {
  read_while(input, is_number)
}

fn read_while(input: String, predicate: fn(String) -> Bool) -> #(String, String) {
  do_read_while(input, predicate, "")
}

fn do_read_while(
  input: String,
  predicate: fn(String) -> Bool,
  acc: String,
) -> #(String, String) {
  case string.pop_grapheme(input) {
    Error(_) -> #(acc, input)
    Ok(#(hd, tl)) ->
      case predicate(hd) {
        True -> do_read_while(tl, predicate, acc <> hd)
        False -> #(acc, input)
      }
  }
}

fn is_number(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn is_letter(ch: String) -> Bool {
  case ch {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "_" -> True
    _ -> False
  }
}
