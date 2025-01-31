import object
import ast

pub type EvalResult = Result(object.Object, String)

pub fn eval(node: ast.Node) -> EvalResult {
  case node {
    ast.ProgramNode(stmts) -> eval_statements(stmts)

    ast.StatementNode(ast.ExpressionStatement(expr)) -> eval(ast.ExpressionNode(expr))

    ast.ExpressionNode(ast.IntegerLiteral(val)) -> Ok(object.Integer(val))
    _ -> Error("Unable to evaluate this node")
  }
}

pub fn eval_statements(stmts: List(ast.Statement)) -> EvalResult {
  case stmts {
    [] -> Error("AAAAA")
    [stmt] -> eval(ast.StatementNode(stmt))
    [stmt, ..rest] -> {
      let _ = eval(ast.StatementNode(stmt))
      eval_statements(rest)
    }
  }
}


