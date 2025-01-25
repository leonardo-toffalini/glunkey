import gleam/option.{type Option}

// NOTE: expr fields are optional as of now because we cannot parse expressions just yet
pub type Statement {
  LetStatement(name: Expression, value: Option(Expression))
  ReturnStatement(return_value: Option(Expression))
  ExpressionStatement(expr: Option(Expression))
}

pub type Expression {
  Identifier(value: String)
  IntegerLiteral(value: Int)
}

pub type Node {
  Statement(Statement)
  Expression(Expression)
}

pub type Program =
  List(Statement)
