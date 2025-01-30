import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// NOTE: expr fields are optional as of now because we cannot parse expressions just yet
pub type Statement {
  LetStatement(name: Expression, value: Option(Expression))
  ReturnStatement(return_value: Option(Expression))
  ExpressionStatement(expr: Expression)
  BlockStatement(statements: List(Statement))
}

pub type Expression {
  Identifier(value: String)
  IntegerLiteral(value: Int)
  Boolean(value: Bool)
  IfExpression(
    condition: Expression,
    consequence: BlockStatement,
    alternative: Option(BlockStatement),
  )
  FunctionLiteral(parameters: Parameters, body: BlockStatement)
  PrefixExpression(operator: String, right: Expression)
  Infixexpression(left: Expression, operator: String, right: Expression)
}

pub type Program =
  List(Statement)

pub type BlockStatement =
  List(Statement)

pub type Parameters =
  List(Expression)

pub fn print_program(prog: Program) -> Nil {
  case prog {
    [] -> io.print("\n")
    [first, ..rest] -> {
      io.print(statement_to_string(first))
      print_program(rest)
    }
  }
}

fn expression_to_string(expr: Expression) -> String {
  case expr {
    Identifier(value) -> value
    IntegerLiteral(value) -> int.to_string(value)
    Boolean(value) -> string.inspect(value)
    IfExpression(condition, consequence, None) ->
      "if "
      <> expression_to_string(condition)
      <> " { "
      <> stmt_list_to_string(consequence)
      <> " }"
    IfExpression(condition, consequence, Some(alternative)) ->
      "if "
      <> expression_to_string(condition)
      <> " { "
      <> stmt_list_to_string(consequence)
      <> " } else { "
      <> stmt_list_to_string(alternative)
      <> " }"
    FunctionLiteral(params, body) ->
      list.fold(params, "fn (", fn(left, expr) {
        left <> expression_to_string(expr) <> ","
      })
      <> ")"
      <> " { "
      <> stmt_list_to_string(body)
      <> " }"
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

fn statement_to_string(stmt: Statement) -> String {
  case stmt {
    LetStatement(name, Some(value)) ->
      "let "
      <> expression_to_string(name)
      <> " = "
      <> expression_to_string(value)
    ReturnStatement(Some(return_value)) ->
      "return " <> expression_to_string(return_value)
    ExpressionStatement(expr) -> expression_to_string(expr)
    _ -> "Unable to convert this im sorry"
  }
}

fn stmt_list_to_string(stmts: List(Statement)) -> String {
  list.fold(stmts, "", fn(left, stmt) { left <> statement_to_string(stmt) })
}
