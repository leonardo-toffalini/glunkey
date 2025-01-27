import gleam/int
import gleam/io
import gleam/option.{type Option, Some}

// NOTE: expr fields are optional as of now because we cannot parse expressions just yet
pub type Statement {
  LetStatement(name: Expression, value: Option(Expression))
  ReturnStatement(return_value: Option(Expression))
  ExpressionStatement(expr: Option(Expression))
}

pub type Expression {
  Identifier(value: String)
  IntegerLiteral(value: Int)
  PrefixExpression(operator: String, right: Expression)
  Infixexpression(left: Expression, operator: String, right: Expression)
}

pub type Program =
  List(Statement)

pub fn print_program(prog: Program) -> Nil {
  case prog {
    [] -> io.print("\n")
    [first, ..rest] -> {
      io.print(statement_to_string(first))
      print_program(rest)
    }
  }
}

pub fn expression_to_string(expr: Expression) -> String {
  case expr {
    Identifier(value) -> "(" <> value <> ")"
    IntegerLiteral(value) -> "(" <> int.to_string(value) <> ")"
    PrefixExpression(operator, right) -> {
      "(" <> operator <> expression_to_string(right) <> ")"
    }
    Infixexpression(left, operator, right) ->
      "("
      <> expression_to_string(left)
      <> operator
      <> expression_to_string(right)
      <> ")"
  }
}

pub fn statement_to_string(stmt: Statement) -> String {
  case stmt {
    LetStatement(name, Some(value)) ->
      "let "
      <> expression_to_string(name)
      <> " = "
      <> expression_to_string(value)
    ReturnStatement(Some(return_value)) ->
      "return " <> expression_to_string(return_value)
    ExpressionStatement(Some(expr)) -> expression_to_string(expr)
    _ -> "Unable to convert this im sorry"
  }
}
