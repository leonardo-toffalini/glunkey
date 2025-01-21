import ast
import gleam/list
import gleam/option.{None}
import token

pub type ProgramResult =
  Result(ast.Program, String)

pub type ParseResult =
  Result(#(ast.Statement, List(token.Token)), String)

pub fn parse(tokens: List(token.Token)) -> ProgramResult {
  do_parse(tokens, [])
}

fn do_parse(tokens: List(token.Token), acc: ast.Program) -> ProgramResult {
  case tokens {
    [] -> Error("Empty tokens")
    [token.Token(token.EOF, "")] -> Ok(list.reverse(acc))
    tokens -> {
      case parse_statement(tokens) {
        Ok(#(stmt, rest)) -> do_parse(rest, [stmt, ..acc])
        Error(e) -> Error(e)
      }
    }
  }
}

fn parse_statement(tokens: List(token.Token)) -> ParseResult {
  case tokens {
    [token.Token(token.Let, "let"), ..rest] -> parse_let_statement(rest)
    [first, ..] -> Error("unknown stmt token:" <> first.literal)
    _ -> Error("default branch in parse_statement")
  }
}

fn parse_let_statement(tokens: List(token.Token)) -> ParseResult {
  case tokens {
    [
      token.Token(token.Ident, ident) as cur_tok,
      token.Token(token.Assign, "="),
      ..rest
    ] -> {
      let identifier = ast.Identifier(token: cur_tok, value: ident)
      let value =
        ast.LetStatement(
          token: token.Token(token.Let, "let"),
          name: identifier,
          value: None,
        )

      let assert Ok(rest) = parse_until(rest, token.Token(token.Semicolon, ";"))

      Ok(#(value, rest))
    }
    [] -> Error("Empty tokens in parse let statement")
    _ -> Error("default case")
  }
}

fn parse_until(
  tokens: List(token.Token),
  stop: token.Token,
) -> Result(List(token.Token), String) {
  case tokens {
    [] -> Error("did not find stopper token")
    [first, ..rest] if first.ttype == stop.ttype -> Ok(rest)
    [_, ..rest] -> parse_until(rest, stop)
  }
}
