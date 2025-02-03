import ast
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/string

pub opaque type Environment {
  Environment(store: Dict(String, Object), outer: Option(Environment))
}

pub fn new(outer: Option(Environment)) -> Environment {
  let s = dict.new()
  Environment(s, outer)
}

pub fn get(env: Environment, key: String) -> Result(Object, Nil) {
  case dict.get(env.store, key), env.outer {
    Ok(obj), _ -> Ok(obj)
    Error(Nil), Some(outer) -> get(outer, key)
    Error(Nil), None -> Error(Nil)
  }
}

pub fn insert(env: Environment, key: String, val: Object) -> Environment {
  let s = dict.insert(env.store, key, val)
  Environment(s, None)
}

pub type Object {
  Integer(value: Int)
  Boolean(value: Bool)
  Function(
    parameters: List(ast.Expression),
    body: ast.BlockStatement,
    env: Environment,
  )
}

pub fn object_to_string(obj: Object) -> String {
  case obj {
    Integer(value) -> string.inspect(value)
    Boolean(value) -> string.inspect(value)
    Function(_, _, _) -> "Function object"
  }
}
