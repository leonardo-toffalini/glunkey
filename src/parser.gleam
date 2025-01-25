import ast
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import token

type Precedence {
  Lowest
  Equals
  LessGreater
  Sum
  Product
  Prefix
  Call
}

type ProgramResult =
  Result(ast.Program, String)

type ParseStmtResult =
  Result(#(ast.Statement, List(token.Token)), String)

type ParseExprResult =
  Result(#(ast.Expression, List(token.Token)), String)

type PrefixParseFn =
  fn(List(token.Token)) -> ParseExprResult

type InfixParseFn =
  fn(ast.Expression, List(token.Token)) -> ParseExprResult

fn get_prefix_parse_fns() -> dict.Dict(token.TokenType, PrefixParseFn) {
  [#(token.Ident, parse_identifier), #(token.Int, parse_integer_literal)]
  |> dict.from_list()
}

fn get_infix_parse_fns() -> dict.Dict(token.TokenType, InfixParseFn) {
  []
  |> dict.from_list()
}

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

fn parse_statement(tokens: List(token.Token)) -> ParseStmtResult {
  case tokens {
    [] -> Error("expected statement, got: empty token list")
    [token.Token(token.Let, "let"), ..rest] -> parse_let_statement(rest)
    [token.Token(token.Return, "return"), ..rest] ->
      parse_return_statement(rest)
    _ -> parse_expression_statement(tokens)
  }
}

fn parse_let_statement(tokens: List(token.Token)) -> ParseStmtResult {
  case tokens {
    [token.Token(token.Ident, ident), token.Token(token.Assign, "="), ..rest] -> {
      let identifier = ast.Identifier(value: ident)
      let value = ast.LetStatement(name: identifier, value: None)

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

fn parse_return_statement(tokens: List(token.Token)) -> ParseStmtResult {
  case tokens {
    _ -> {
      let return_statement = ast.ReturnStatement(return_value: None)
      case parse_until(tokens, token.Token(token.Semicolon, ";")) {
        Ok(rest) -> Ok(#(return_statement, rest))
        Error(e) -> Error(e)
      }
    }
  }
}

fn parse_expression_statement(tokens: List(token.Token)) -> ParseStmtResult {
  case parse_expression(tokens, Lowest) {
    Error(e) -> Error(e)
    Ok(#(expr, rest)) -> {
      case rest {
        [token.Token(token.Semicolon, ";"), ..rest] ->
          Ok(#(ast.ExpressionStatement(Some(expr)), rest))
        _ -> Error("Missing semicolon after expression statement")
      }
    }
  }
}

fn parse_expression(
  tokens: List(token.Token),
  precedence: Precedence,
) -> ParseExprResult {
  case tokens {
    [token.Token(ttype, _), ..] -> {
      case get_prefix_parse_fns() |> dict.get(ttype) {
        Ok(prefix) -> prefix(tokens)
        Error(Nil) ->
          Error(
            "Did not find prefix parse function for token type: "
            <> string.inspect(ttype),
          )
      }
    }
    [] -> Error("Expected expression, got empty token list")
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

fn parse_identifier(tokens: List(token.Token)) -> ParseExprResult {
  case tokens {
    [token.Token(_, literal), ..rest] -> Ok(#(ast.Identifier(literal), rest))
    [] -> Error("Expected identifier in parse_identifier, got empty token list")
  }
}

fn parse_integer_literal(tokens: List(token.Token)) -> ParseExprResult {
  case tokens {
    [token.Token(token.Int, literal), ..rest] -> {
      case int.parse(literal) {
        Ok(value) -> Ok(#(ast.IntegerLiteral(value), rest))
        Error(Nil) -> Error("Could not parse " <> literal <> " as integer")
      }
    }
    [] -> Error("Expected integer literal, got empty token list")
    [token.Token(ttype, _), ..] ->
      Error("Expected integer literal, got: " <> string.inspect(ttype))
  }
}
