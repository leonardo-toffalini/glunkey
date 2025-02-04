import gleam/int
import gleam/io
import gleam/list
import gleam/string

pub type Node {
  StatementNode(Statement)
  ExpressionNode(Expression)
  ProgramNode(Program)
}

pub type Statement {
  LetStatement(name: Expression, value: Expression)
  ReturnStatement(return_value: Expression)
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
    alternative: BlockStatement,
  )
  FunctionLiteral(parameters: List(Expression), body: BlockStatement)
  CallExpression(function: Expression, arguments: List(Expression))
  PrefixExpression(operator: String, right: Expression)
  InfixExpression(left: Expression, operator: String, right: Expression)
}

pub type Program =
  List(Statement)

pub type BlockStatement =
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
    Identifier(value) -> value
    IntegerLiteral(value) -> int.to_string(value)
    Boolean(value) -> string.inspect(value)
    IfExpression(condition, consequence, alternative) ->
      "if "
      <> expression_to_string(condition)
      <> " { "
      <> stmt_list_to_string(consequence)
      <> " } else { "
      <> stmt_list_to_string(alternative)
      <> " }"
    FunctionLiteral(params, body) ->
      "fn ("
      <> list.map(params, expression_to_string) |> string.join(",")
      <> ")"
      <> " { "
      <> stmt_list_to_string(body)
      <> " }"
    CallExpression(function, args) ->
      expression_to_string(function)
      <> "("
      <> list.map(args, expression_to_string) |> string.join(",")
      <> ")"
    PrefixExpression(operator, right) -> {
      "(" <> operator <> expression_to_string(right) <> ")"
    }
    InfixExpression(left, operator, right) ->
      "("
      <> expression_to_string(left)
      <> operator
      <> expression_to_string(right)
      <> ")"
  }
}

fn statement_to_string(stmt: Statement) -> String {
  case stmt {
    LetStatement(name, value) ->
      "let "
      <> expression_to_string(name)
      <> " = "
      <> expression_to_string(value)
    ReturnStatement(return_value) ->
      "return " <> expression_to_string(return_value)
    ExpressionStatement(expr) -> expression_to_string(expr)
    _ -> "Unable to convert this im sorry"
  }
}

pub fn stmt_list_to_string(stmts: List(Statement)) -> String {
  list.fold(stmts, "", fn(left, stmt) { left <> statement_to_string(stmt) })
}
