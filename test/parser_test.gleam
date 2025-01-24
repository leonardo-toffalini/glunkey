import ast
import gleam/list
import gleam/option.{None}
import gleeunit
import gleeunit/should
import lexer
import parser
import token

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
            ast.LetStatement(_, ast.Identifier(_, value), _) -> value
            ast.ReturnStatement(_, _) ->
              panic as "found return statement in let statement test"
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
      |> should.equal(
        Ok(ast.ReturnStatement(token.Token(token.Return, "return"), None)),
      )
    }
    Error(_) -> should.fail()
  }
}
