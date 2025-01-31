import gleam/string

pub type Object {
  Integer(value: Int)
  Boolean(value: Bool)
  Null
}

pub fn object_to_string(obj: Object) -> String {
  case obj {
    Integer(value) -> string.inspect(value)
    Boolean(value) -> string.inspect(value)
    Null -> "null"
  }
}
