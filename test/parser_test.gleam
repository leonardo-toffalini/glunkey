import ast
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import lexer
import parser

pub fn main() {
  gleeunit.main()
}

pub fn let_stmt_test() {
  let input =
    "
let x = 5;
let y = 10;
let foobar = 838383;"

  let tokens = lexer.lex(input)
  let program = parser.parse(tokens)
  case program {
    Ok(statements) -> {
      statements
      |> list.length
      |> should.equal(3)

      let expected_identifiers = ["x", "y", "foobar"]
      let statement_identifiers =
        statements
        |> list.map(fn(stmt) {
          case stmt {
            ast.LetStatement(ast.Identifier(value), _) -> value
            _ ->
              panic as "Expected let statement with the name being an identifier"
          }
        })
      statement_identifiers
      |> should.equal(expected_identifiers)
    }
    Error(_) -> should.fail()
  }
}

pub fn return_statement_test() {
  let input = "return 5;"
  let tokens = lexer.lex(input)
  let program = parser.parse(tokens)
  case program {
    Ok(statements) -> {
      statements
      |> list.length
      |> should.equal(1)

      list.first(statements)
      |> should.equal(Ok(ast.ReturnStatement(None)))
    }
    Error(_) -> should.fail()
  }
}

pub fn integer_literal_expression_test() {
  let input = "5;"
  let tokens = lexer.lex(input)
  let program = parser.parse(tokens)

  case program {
    Ok(statements) -> {
      statements
      |> list.length
      |> should.equal(1)

      case list.first(statements) {
        Ok(ast.ExpressionStatement(expr: ast.IntegerLiteral(value: 5))) -> Nil
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn identifier_expression_test() {
  let input = "foobar;"
  let tokens = lexer.lex(input)
  let program = parser.parse(tokens)

  case program {
    Ok(statements) -> {
      statements
      |> list.length
      |> should.equal(1)

      case list.first(statements) {
        Ok(ast.ExpressionStatement(expr: ast.Identifier(value: "foobar"))) ->
          Nil
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn prefix_bang_expression_test() {
  let input = "!5;"
  let tokens = lexer.lex(input)
  let program = parser.parse(tokens)

  case program {
    Ok(statements) -> {
      statements
      |> list.length
      |> should.equal(1)

      case list.first(statements) {
        Ok(ast.ExpressionStatement(expr: ast.PrefixExpression(
          operator: "!",
          right: ast.IntegerLiteral(value: 5),
        ))) -> Nil
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn prefix_minus_expression_test() {
  let input = "-15;"
  let tokens = lexer.lex(input)
  let program = parser.parse(tokens)

  case program {
    Ok(statements) -> {
      statements
      |> list.length
      |> should.equal(1)

      case list.first(statements) {
        Ok(ast.ExpressionStatement(expr: ast.PrefixExpression(
          operator: "-",
          right: ast.IntegerLiteral(value: 15),
        ))) -> Nil
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn multiple_identifier_expressions_test() {
  let input =
    "
    first_ident;
    second_ident;
    third_ident;"
  let tokens = lexer.lex(input)
  let program = parser.parse(tokens)

  case program {
    Ok(statements) -> {
      statements
      |> list.length
      |> should.equal(3)

      let expected_identifiers = ["first_ident", "second_ident", "third_ident"]
      let statement_identifiers =
        statements
        |> list.map(fn(stmt) {
          case stmt {
            ast.ExpressionStatement(expr: ast.Identifier(value: v)) -> v
            _ -> panic as "expected expression statement with identifier"
          }
        })
      statement_identifiers
      |> should.equal(expected_identifiers)
    }
    Error(_) -> should.fail()
  }
}

pub fn infix_expression_test() {
  let test_cases = [
    #("1 + 2;", "+", ast.IntegerLiteral(1), ast.IntegerLiteral(2)),
    #("1 * 2;", "*", ast.IntegerLiteral(1), ast.IntegerLiteral(2)),
    #("1 - 2;", "-", ast.IntegerLiteral(1), ast.IntegerLiteral(2)),
    #("4 / 2;", "/", ast.IntegerLiteral(4), ast.IntegerLiteral(2)),
  ]

  test_cases
  |> list.map(fn(test_case) {
    let #(input, e_operator, e_left, e_right) = test_case
    let tokens = lexer.lex(input)
    let program = parser.parse(tokens)

    case program {
      Ok(statements) -> {
        statements
        |> list.length
        |> should.equal(1)

        case list.first(statements) {
          Ok(ast.ExpressionStatement(expr: ast.Infixexpression(
            left: left,
            operator: operator,
            right: right,
          )))
            if operator == e_operator && left == e_left && right == e_right
          -> Nil
          _ -> should.fail()
        }
      }
      Error(_) -> should.fail()
    }
  })
}

pub fn operator_precedence_test() {
  let input = "1 + 2 * 3;"
  let tokens = lexer.lex(input)
  let program = parser.parse(tokens)

  case program {
    Ok(statements) -> {
      statements
      |> list.length
      |> should.equal(1)

      case list.first(statements) {
        Ok(ast.ExpressionStatement(expr: ast.Infixexpression(
          left: ast.IntegerLiteral(1),
          operator: "+",
          right: ast.Infixexpression(
            left: ast.IntegerLiteral(2),
            operator: "*",
            right: ast.IntegerLiteral(3),
          ),
        ))) -> Nil
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
