import ast
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import object

const null_obj = object.Null

const true_obj = object.Boolean(True)

const false_obj = object.Boolean(False)

pub type EvalResult =
  Result(object.Object, String)

pub fn eval(node: ast.Node) -> EvalResult {
  case node {
    ast.ProgramNode(stmts) -> eval_statements(stmts)

    // Statements
    ast.StatementNode(ast.ExpressionStatement(expr)) ->
      eval(ast.ExpressionNode(expr))
    ast.StatementNode(ast.BlockStatement(stmts)) -> eval_statements(stmts)

    // Expressions
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
    ast.ExpressionNode(ast.IfExpression(condition, consequence, alternative)) ->
      eval_if_expression(condition, consequence, alternative)

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
    object.Boolean(left_val), object.Boolean(right_val) ->
      eval_bool_infix_expression(left_val, operator, right_val)
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
    "<" -> Ok(native_bool_to_obj(left < right))
    ">" -> Ok(native_bool_to_obj(left > right))
    "==" -> Ok(native_bool_to_obj(left == right))
    "!=" -> Ok(native_bool_to_obj(left != right))
    op -> Error("Unsupported operator for int-int infix expression: " <> op)
  }
}

fn eval_bool_infix_expression(
  left: Bool,
  operator: String,
  right: Bool,
) -> EvalResult {
  case operator {
    "==" -> Ok(native_bool_to_obj(left == right))
    "!=" -> Ok(native_bool_to_obj(left != right))
    op -> Error("Unsupported operator for bool-bool infix expression: " <> op)
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

fn eval_if_expression(
  condition: ast.Expression,
  consequence: ast.BlockStatement,
  alternative: Option(ast.BlockStatement),
) {
  use condition <- result.try(eval(ast.ExpressionNode(condition)))
  case is_truthy(condition), alternative {
    True, _ -> eval_statements(consequence)
    False, Some(alternative) -> eval_statements(alternative)
    False, None -> Ok(null_obj)
    // todo as "do you really want to return a null object here? or make it so every if expression must have an else branch"
  }
}

fn is_truthy(obj: object.Object) {
  case obj {
    object.Boolean(True) -> True
    object.Boolean(False) -> False
    object.Null -> False
    _ -> True
  }
}

fn native_bool_to_obj(b: Bool) -> object.Object {
  case b {
    True -> true_obj
    False -> false_obj
  }
}
