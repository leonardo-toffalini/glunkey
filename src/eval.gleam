import ast
import environment
import gleam/result
import gleam/string
import object

const true_obj = object.Boolean(True)

const false_obj = object.Boolean(False)

pub type EvalResult =
  Result(#(object.Object, environment.Environment), String)

pub fn eval(node: ast.Node, env: environment.Environment) -> EvalResult {
  case node {
    ast.ProgramNode(stmts) -> eval_statements(stmts, env)

    // Statements
    ast.StatementNode(ast.ExpressionStatement(expr)) ->
      eval(ast.ExpressionNode(expr), env)
    ast.StatementNode(ast.BlockStatement(stmts)) -> eval_statements(stmts, env)
    ast.StatementNode(ast.ReturnStatement(_)) ->
      Error("Return statements are not supported by design")
    ast.StatementNode(ast.LetStatement(ast.Identifier(name), val)) -> {
      use #(val, env) <- result.try(eval(ast.ExpressionNode(val), env))
      let env = environment.insert(env, name, val)
      Ok(#(val, env))
    }

    // Expressions
    ast.ExpressionNode(ast.IntegerLiteral(val)) ->
      Ok(#(object.Integer(val), env))
    ast.ExpressionNode(ast.Boolean(val)) -> Ok(#(native_bool_to_obj(val), env))
    ast.ExpressionNode(ast.PrefixExpression(op, right)) -> {
      use #(right, env) <- result.try(eval(ast.ExpressionNode(right), env))
      eval_prefix_expression(op, right, env)
    }
    ast.ExpressionNode(ast.InfixExpression(left, op, right)) -> {
      use #(left, env) <- result.try(eval(ast.ExpressionNode(left), env))
      use #(right, env) <- result.try(eval(ast.ExpressionNode(right), env))
      eval_infix_expression(left, op, right, env)
    }
    ast.ExpressionNode(ast.IfExpression(condition, consequence, alternative)) ->
      eval_if_expression(condition, consequence, alternative, env)
    ast.ExpressionNode(ast.Identifier(name)) -> eval_identifier(name, env)

    _ -> Error("Unable to evaluate this node")
  }
}

fn eval_statements(
  stmts: List(ast.Statement),
  env: environment.Environment,
) -> EvalResult {
  case stmts {
    [] -> Error("Expected list of statements, got empty list")
    [stmt] -> eval(ast.StatementNode(stmt), env)
    [stmt, ..rest] -> {
      let _ = eval(ast.StatementNode(stmt), env)
      eval_statements(rest, env)
    }
  }
}

fn eval_prefix_expression(
  operator: String,
  right: object.Object,
  env: environment.Environment,
) -> EvalResult {
  case operator {
    "!" -> eval_bang_op_expression(right, env)
    "-" -> eval_minus_op_expression(right, env)
    op -> Error("Cannot evaluate prefix expression for operator " <> op)
  }
}

fn eval_infix_expression(
  left: object.Object,
  operator: String,
  right: object.Object,
  env: environment.Environment,
) -> EvalResult {
  case left, right {
    object.Integer(left_val), object.Integer(right_val) ->
      eval_int_infix_expression(left_val, operator, right_val, env)
    object.Boolean(left_val), object.Boolean(right_val) ->
      eval_bool_infix_expression(left_val, operator, right_val, env)
    _, _ ->
      Error("Unsupported operand types, only ints are supported as of now")
  }
}

fn eval_int_infix_expression(
  left: Int,
  operator: String,
  right: Int,
  env: environment.Environment,
) -> EvalResult {
  case operator {
    "+" -> Ok(#(object.Integer(left + right), env))
    "-" -> Ok(#(object.Integer(left - right), env))
    "*" -> Ok(#(object.Integer(left * right), env))
    "/" -> Ok(#(object.Integer(left / right), env))
    "<" -> Ok(#(native_bool_to_obj(left < right), env))
    ">" -> Ok(#(native_bool_to_obj(left > right), env))
    "==" -> Ok(#(native_bool_to_obj(left == right), env))
    "!=" -> Ok(#(native_bool_to_obj(left != right), env))
    op -> Error("Unsupported operator for int-int infix expression: " <> op)
  }
}

fn eval_bool_infix_expression(
  left: Bool,
  operator: String,
  right: Bool,
  env: environment.Environment,
) -> EvalResult {
  case operator {
    "==" -> Ok(#(native_bool_to_obj(left == right), env))
    "!=" -> Ok(#(native_bool_to_obj(left != right), env))
    op -> Error("Unsupported operator for bool-bool infix expression: " <> op)
  }
}

fn eval_bang_op_expression(
  right: object.Object,
  env: environment.Environment,
) -> EvalResult {
  case right {
    object.Boolean(True) -> Ok(#(false_obj, env))
    object.Boolean(False) -> Ok(#(true_obj, env))
    _ -> Ok(#(false_obj, env))
  }
}

fn eval_minus_op_expression(
  right: object.Object,
  env: environment.Environment,
) -> EvalResult {
  case right {
    object.Integer(val) -> Ok(#(object.Integer(-val), env))
    obj ->
      Error(
        "No way to evaluate minus prefix expression for " <> string.inspect(obj),
      )
  }
}

fn eval_if_expression(
  condition: ast.Expression,
  consequence: ast.BlockStatement,
  alternative: ast.BlockStatement,
  env: environment.Environment,
) -> EvalResult {
  use #(condition, env) <- result.try(eval(ast.ExpressionNode(condition), env))
  case is_truthy(condition), alternative {
    True, _ -> eval_statements(consequence, env)
    False, alternative -> eval_statements(alternative, env)
    // todo as "do you really want to return a null object here? or make it so every if expression must have an else branch"
  }
}

fn eval_identifier(name: String, env: environment.Environment) -> EvalResult {
  case environment.get(env, name) {
    Ok(val) -> Ok(#(val, env))
    Error(Nil) -> Error("identifier not found: " <> name)
  }
}

fn is_truthy(obj: object.Object) {
  case obj {
    object.Boolean(True) -> True
    object.Boolean(False) -> False
    _ -> True
  }
}

fn native_bool_to_obj(b: Bool) -> object.Object {
  case b {
    True -> true_obj
    False -> false_obj
  }
}
