import gleam/dict.{type Dict}
import object.{type Object}

pub opaque type Environment {
  Environment(store: Dict(String, Object))
}

pub fn new() -> Environment {
  let s = dict.new()
  Environment(s)
}

pub fn get(env: Environment, key: String) -> Result(Object, Nil) {
  dict.get(env.store, key)
}

pub fn insert(env: Environment, key: String, val: Object) -> Environment {
  let s = dict.insert(env.store, key, val)
  Environment(s)
}
