import ast
import gleam/result
import gleam/string
import object

// const null_obj = object.Null

const true_obj = object.Boolean(True)

const false_obj = object.Boolean(False)

pub type EvalResult =
  Result(object.Object, String)

pub fn eval(node: ast.Node) -> EvalResult {
  case node {
    ast.ProgramNode(stmts) -> eval_statements(stmts)

    ast.StatementNode(ast.ExpressionStatement(expr)) ->
      eval(ast.ExpressionNode(expr))

    ast.ExpressionNode(ast.IntegerLiteral(val)) -> Ok(object.Integer(val))
    ast.ExpressionNode(ast.Boolean(val)) -> native_bool_to_obj(val) |> Ok()
    ast.ExpressionNode(ast.PrefixExpression(op, right)) -> {
      use right <- result.try(eval(ast.ExpressionNode(right)))
      eval_prefix_expression(op, right)
    }
    ast.ExpressionNode(ast.InfixExpression(left, op, right)) -> {
      use left <- result.try(eval(ast.ExpressionNode(left)))
      use right <- result.try(eval(ast.ExpressionNode(right)))
      eval_infix_expression(left, op, right)
    }

    _ -> Error("Unable to evaluate this node")
  }
}

fn eval_statements(stmts: List(ast.Statement)) -> EvalResult {
  case stmts {
    [] -> Error("Expected list of statements, got empty list")
    [stmt] -> eval(ast.StatementNode(stmt))
    [stmt, ..rest] -> {
      let _ = eval(ast.StatementNode(stmt))
      eval_statements(rest)
    }
  }
}

fn eval_prefix_expression(operator: String, right: object.Object) -> EvalResult {
  case operator {
    "!" -> eval_bang_op_expression(right)
    "-" -> eval_minus_op_expression(right)
    op -> Error("Cannot evaluate prefix expression for operator " <> op)
  }
}

fn eval_infix_expression(
  left: object.Object,
  operator: String,
  right: object.Object,
) -> EvalResult {
  case left, right {
    object.Integer(left_val), object.Integer(right_val) ->
      eval_int_infix_expression(left_val, operator, right_val)
    _, _ ->
      Error("Unsupported operand types, only ints are supported as of now")
  }
}

fn eval_int_infix_expression(
  left: Int,
  operator: String,
  right: Int,
) -> EvalResult {
  case operator {
    "+" -> Ok(object.Integer(left + right))
    "-" -> Ok(object.Integer(left - right))
    "*" -> Ok(object.Integer(left * right))
    "/" -> Ok(object.Integer(left / right))
    op -> Error("Unsupported operator for int-int infix expression: " <> op)
  }
}

fn eval_bang_op_expression(right: object.Object) -> EvalResult {
  case right {
    object.Boolean(True) -> Ok(false_obj)
    object.Boolean(False) -> Ok(true_obj)
    object.Null -> Ok(true_obj)
    _ -> Ok(false_obj)
  }
}

fn eval_minus_op_expression(right: object.Object) -> EvalResult {
  case right {
    object.Integer(val) -> Ok(object.Integer(-val))
    obj ->
      Error(
        "No way to evaluate minus prefix expression for " <> string.inspect(obj),
      )
  }
}

fn native_bool_to_obj(b: Bool) -> object.Object {
  case b {
    True -> true_obj
    False -> false_obj
  }
}
