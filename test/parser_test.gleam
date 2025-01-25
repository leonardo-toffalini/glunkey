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
            ast.LetStatement(ast.IntegerLiteral(_), _) ->
              panic as "expected identifier in let statement, got integer literal"
            ast.ReturnStatement(_) ->
              panic as "found return statement in let statement test"
            ast.ExpressionStatement(_) ->
              panic as "found expression statement in let statement test"
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
        Ok(ast.ExpressionStatement(expr: Some(ast.IntegerLiteral(value: 5)))) ->
          Nil
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
        Ok(ast.ExpressionStatement(expr: Some(ast.Identifier(value: "foobar")))) ->
          Nil
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
            ast.ExpressionStatement(expr: Some(ast.Identifier(value: v))) -> v
            _ -> panic as "expected expression statement with identifier"
          }
        })
      statement_identifiers
      |> should.equal(expected_identifiers)
    }
    Error(_) -> should.fail()
  }
}
