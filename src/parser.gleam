import ast
import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import token

// Precedences
const lowest_pre = 0

const equals_pre = 1

const less_greater_pre = 2

const sum_pre = 3

const product_pre = 4

const prefix_pre = 5

const call_pre = 6

type Precedence =
  Int

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
  [
    #(token.Ident, parse_identifier),
    #(token.Int, parse_integer_literal),
    #(token.TRUE, parse_boolean),
    #(token.FALSE, parse_boolean),
    #(token.Bang, parse_prefix_expression),
    #(token.Minus, parse_prefix_expression),
    #(token.LParen, parse_grouped_expression),
    #(token.If, parse_if_expression),
    #(token.Function, parse_function_literal),
  ]
  |> dict.from_list()
}

fn get_infix_parse_fns() -> dict.Dict(token.TokenType, InfixParseFn) {
  [
    #(token.Plus, parse_infix_expression),
    #(token.Minus, parse_infix_expression),
    #(token.Slash, parse_infix_expression),
    #(token.Star, parse_infix_expression),
    #(token.Eq, parse_infix_expression),
    #(token.Neq, parse_infix_expression),
    #(token.Lt, parse_infix_expression),
    #(token.Gt, parse_infix_expression),
    #(token.LParen, parse_call_expression),
  ]
  |> dict.from_list()
}

fn get_precedence_dict() -> dict.Dict(token.TokenType, Precedence) {
  [
    #(token.Eq, equals_pre),
    #(token.Neq, equals_pre),
    #(token.Lt, less_greater_pre),
    #(token.Gt, less_greater_pre),
    #(token.Plus, sum_pre),
    #(token.Minus, sum_pre),
    #(token.Slash, product_pre),
    #(token.Star, product_pre),
    #(token.LParen, call_pre),
  ]
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
      use #(stmt, rest) <- result.try(parse_statement(tokens))
      do_parse(rest, [stmt, ..acc])
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
      use #(val, rest) <- result.try(parse_expression(rest, lowest_pre))
      case rest {
        [token.Token(token.Semicolon, ";"), ..rest] ->
          Ok(#(ast.LetStatement(ast.Identifier(ident), val), rest))
        [token.Token(ttype, _), ..] ->
          Error(
            "Expected semicolon after let statement, got "
            <> string.inspect(ttype),
          )
        [] ->
          Error("Expected semicolon after let statement, got empty token list")
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
  use #(return_value, rest) <- result.try(parse_expression(tokens, lowest_pre))
  case rest {
    [token.Token(token.Semicolon, ";"), ..rest] ->
      Ok(#(ast.ReturnStatement(return_value), rest))
    [token.Token(ttype, _), ..] ->
      Error(
        "Expected semicolon after return statement, got "
        <> string.inspect(ttype),
      )
    [] ->
      Error("Expected semicolon after return statement, got empty token list")
  }
}

fn parse_expression_statement(tokens: List(token.Token)) -> ParseStmtResult {
  use #(expr, rest) <- result.try(parse_expression(tokens, lowest_pre))
  case rest {
    [token.Token(token.Semicolon, ";"), ..rest] ->
      Ok(#(ast.ExpressionStatement(expr), rest))
    _ -> Error("Missing semicolon after expression statement")
  }
}

fn parse_expression(
  tokens: List(token.Token),
  precedence: Precedence,
) -> ParseExprResult {
  case tokens {
    [token.Token(ttype, _), ..] -> {
      use prefix <- result.try(get_prefix_fn(ttype))
      use #(left_exp, rest) <- result.try(prefix(tokens))
      parse_expr_with_precedence(rest, precedence, left_exp)
      // Ok(#(left_exp, rest))
    }
    [] -> Error("Expected expression, got empty token list")
  }
}

fn parse_expr_with_precedence(
  tokens: List(token.Token),
  prec: Precedence,
  expr: ast.Expression,
) -> ParseExprResult {
  case tokens {
    [] -> Error("I dont know what im supposed to do here")
    [token.Token(token.Semicolon, ";"), ..] -> Ok(#(expr, tokens))
    [token.Token(ttype, _), ..rest] -> {
      case prec < get_precedence(ttype) {
        False -> Ok(#(expr, tokens))
        True -> {
          case dict.get(get_infix_parse_fns(), ttype) {
            Ok(infix) -> {
              use #(left_expr, rest) <- result.try(infix(expr, tokens))
              parse_expr_with_precedence(rest, prec, left_expr)
            }
            Error(Nil) -> Ok(#(expr, rest))
          }
        }
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

fn parse_boolean(tokens: List(token.Token)) -> ParseExprResult {
  case tokens {
    [token.Token(token.TRUE, _), ..rest] -> Ok(#(ast.Boolean(True), rest))
    [token.Token(token.FALSE, _), ..rest] -> Ok(#(ast.Boolean(False), rest))
    [token.Token(ttype, _), ..] ->
      Error("Expected boolean, got: " <> string.inspect(ttype))
    [] -> Error("Expected boolean, got empty tokens list")
  }
}

fn parse_prefix_expression(tokens: List(token.Token)) -> ParseExprResult {
  case tokens {
    [token.Token(_, literal), ..rest] -> {
      use #(right, rest) <- result.try(parse_expression(rest, prefix_pre))
      Ok(#(ast.PrefixExpression(literal, right), rest))
    }
    [] -> Error("Expected prefix expression, got empty token list")
  }
}

fn parse_infix_expression(
  left: ast.Expression,
  tokens: List(token.Token),
) -> ParseExprResult {
  case tokens {
    [token.Token(ttype, literal), ..rest] -> {
      let precedence = get_precedence(ttype)
      use #(right, rest) <- result.try(parse_expression(rest, precedence))
      Ok(#(ast.InfixExpression(left, literal, right), rest))
    }
    [] -> Error("Expected infix operator, got empty token list")
  }
}

fn parse_grouped_expression(tokens: List(token.Token)) -> ParseExprResult {
  case tokens {
    [token.Token(token.LParen, "("), ..rest] -> {
      use #(exp, rest) <- result.try(parse_expression(rest, lowest_pre))
      case rest {
        [token.Token(token.RParen, ")"), ..rest] -> Ok(#(exp, rest))
        [token.Token(ttype, _), ..] ->
          Error("Expected RParen, got " <> string.inspect(ttype))
        [] -> Error("Expected RParen, got empty token list")
      }
    }
    [token.Token(ttype, _), ..] ->
      Error("Expected RParen, got " <> string.inspect(ttype))
    [] -> Error("Expected RParen, got empty token list")
  }
}

fn parse_if_expression(tokens: List(token.Token)) -> ParseExprResult {
  case tokens {
    [
      token.Token(token.If, "if"),
      token.Token(token.LParen, "(") as lparen,
      ..rest
    ] -> {
      use #(condition, rest) <- result.try(parse_expression(
        [lparen, ..rest],
        lowest_pre,
      ))
      case rest {
        [token.Token(token.LBrace, "{") as lbrace, ..rest] -> {
          use #(consequence, rest) <- result.try(
            parse_block_statement([lbrace, ..rest]),
          )
          case rest {
            [
              token.Token(token.Else, "else"),
              token.Token(token.LBrace, "{") as lbrace,
              ..rest
            ] -> {
              use #(alternative, rest) <- result.try(
                parse_block_statement([lbrace, ..rest]),
              )
              Ok(#(ast.IfExpression(condition, consequence, alternative), rest))
            }
            [token.Token(ttype, _), ..] ->
              Error("Expected else, got " <> string.inspect(ttype))
            [] -> Error("Expected else, got empty token list")
          }
        }
        [token.Token(ttype, _), ..] ->
          Error("Expected LBrace, got: " <> string.inspect(ttype))
        [] -> Error("Expected LBrace, got empty token list")
      }
    }
    [token.Token(ttype, _), ..] ->
      Error("Expected 'if', got " <> string.inspect(ttype))
    [] -> Error("Expected 'if', got empty token list")
  }
}

fn parse_block_statement(
  tokens: List(token.Token),
) -> Result(#(ast.BlockStatement, List(token.Token)), String) {
  case tokens {
    [token.Token(token.LBrace, "{"), ..rest] ->
      do_parse_block_statement(rest, [])
    [token.Token(ttype, _), ..] ->
      Error("Expected LBrace, got " <> string.inspect(ttype))
    [] -> Error("Expected LBrace, got empty token list")
  }
}

fn parse_function_literal(tokens: List(token.Token)) -> ParseExprResult {
  case tokens {
    [token.Token(token.Function, "fn"), token.Token(token.LParen, "("), ..rest] -> {
      use #(params, rest) <- result.try(parse_function_parameters(rest))
      case rest {
        [token.Token(token.LBrace, "{") as lbrace, ..rest] -> {
          use #(body, rest) <- result.try(
            parse_block_statement([lbrace, ..rest]),
          )
          Ok(#(ast.FunctionLiteral(params, body), rest))
        }
        [token.Token(ttype, _), ..] ->
          Error("Expected LBrace, got: " <> string.inspect(ttype))
        [] -> Error("Expected LBrace, got empty token list")
      }
    }
    [token.Token(ttype, _), ..] ->
      Error("Expected function literal, got " <> string.inspect(ttype))
    [] -> Error("Expected function literal, got empty token list")
  }
}

fn do_parse_block_statement(
  tokens: List(token.Token),
  acc: List(ast.Statement),
) -> Result(#(ast.BlockStatement, List(token.Token)), String) {
  case tokens {
    [token.Token(token.RBrace, "}"), ..rest] -> Ok(#(list.reverse(acc), rest))
    [token.Token(token.EOF, ""), ..] -> Error("Unterminated block statement")
    [] -> Error("Expected block statement, got empty token list")
    _ -> {
      use #(stmt, rest) <- result.try(parse_statement(tokens))
      do_parse_block_statement(rest, [stmt, ..acc])
    }
  }
}

fn parse_function_parameters(
  tokens: List(token.Token),
) -> Result(#(List(ast.Expression), List(token.Token)), String) {
  case tokens {
    [token.Token(token.RParen, ")"), ..rest] -> Ok(#([], rest))
    [token.Token(token.Ident, _), ..] -> do_parse_comma_separated(tokens, [])
    _ -> panic
  }
}

fn do_parse_comma_separated(
  tokens: List(token.Token),
  acc: List(ast.Expression),
) -> Result(#(List(ast.Expression), List(token.Token)), String) {
  case tokens {
    [] -> Error("Unterminated parameter list")
    [token.Token(token.RParen, _), ..rest] -> Ok(#(list.reverse(acc), rest))
    [token.Token(token.Ident, lit), ..rest] ->
      do_parse_comma_separated(rest, [ast.Identifier(lit), ..acc])
    [token.Token(token.Comma, ","), ..rest] ->
      do_parse_comma_separated(rest, acc)
    [token.Token(ttype, _), ..] ->
      Error(
        "Expected comma or identifier or closing parenthesis, got "
        <> string.inspect(ttype),
      )
  }
}

fn parse_call_expression(
  function: ast.Expression,
  tokens: List(token.Token),
) -> ParseExprResult {
  use #(args, rest) <- result.try(parse_call_arguments(tokens))
  Ok(#(ast.CallExpression(function, args), rest))
}

fn parse_call_arguments(
  tokens: List(token.Token),
) -> Result(#(List(ast.Expression), List(token.Token)), String) {
  case tokens {
    [token.Token(token.LParen, "("), ..rest] ->
      do_parse_call_arguments(rest, [])
    [token.Token(ttype, _), ..] ->
      Error("Expected LParen, got " <> string.inspect(ttype))
    [] -> Error("Expected LParen, got empty token list")
  }
}

fn do_parse_call_arguments(
  tokens: List(token.Token),
  acc: List(ast.Expression),
) -> Result(#(List(ast.Expression), List(token.Token)), String) {
  case tokens {
    [token.Token(token.RParen, ")"), ..rest] -> Ok(#(list.reverse(acc), rest))
    [token.Token(token.Comma, ","), ..rest] ->
      do_parse_call_arguments(rest, acc)
    [] -> Error("Unterminated arguments list")
    _ -> {
      use #(arg, rest) <- result.try(parse_expression(tokens, lowest_pre))
      do_parse_call_arguments(rest, [arg, ..acc])
    }
  }
}

fn get_prefix_fn(ttype: token.TokenType) -> Result(PrefixParseFn, String) {
  result.map_error(dict.get(get_prefix_parse_fns(), ttype), fn(_) {
    "Could not find prefix parse function for token of type: "
    <> string.inspect(ttype)
  })
}

fn get_precedence(ttype: token.TokenType) -> Precedence {
  result.unwrap(dict.get(get_precedence_dict(), ttype), lowest_pre)
}
