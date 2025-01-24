import gleam/option.{type Option}
import token

// NOTE: value is optional as of now because we cannot parse expressions just yet
pub type Statement {
  LetStatement(token: token.Token, name: Expression, value: Option(Expression))
  ReturnStatement(token: token.Token, return_value: Option(Expression))
}

pub type Expression {
  Identifier(token: token.Token, value: String)
}

pub type Node {
  Statement(Statement)
  Expression(Expression)
}

pub type Program =
  List(Statement)
