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
    [token.Token(token.Return, "return"), ..rest] ->
      parse_return_statement(rest)
    [first, ..] -> Error("unknown stmt token:" <> first.literal)
    [] -> Error("expected statement, got: empty token list")
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

      case parse_until(rest, token.Token(token.Semicolon, ";")) {
        Ok(rest) -> Ok(#(value, rest))
        Error(e) -> Error(e)
      }
    }

    // unexpected outcomes
    [token.Token(token.Ident, _), got_tok, ..] ->
      Error(
        "Expected '=' after identifier in let statement, got: "
        <> got_tok.literal,
      )
    [got_tok, ..] ->
      Error("Expected identifier after 'let', got: " <> got_tok.literal)
    [] -> Error("Expected identifier after 'let', got: nothing")
  }
}

fn parse_return_statement(tokens: List(token.Token)) -> ParseResult {
  case tokens {
    _ -> {
      let return_statement =
        ast.ReturnStatement(
          token: token.Token(token.Return, "return"),
          return_value: None,
        )
      case parse_until(tokens, token.Token(token.Semicolon, ";")) {
        Ok(rest) -> Ok(#(return_statement, rest))
        Error(e) -> Error(e)
      }
    }
  }
}

fn parse_until(
  tokens: List(token.Token),
  terminal: token.Token,
) -> Result(List(token.Token), String) {
  case tokens {
    [] -> Error("Did not find terminating token: " <> terminal.literal)
    [first, ..rest] if first.ttype == terminal.ttype -> Ok(rest)
    [_, ..rest] -> parse_until(rest, terminal)
  }
}
