import gleam/dict
import gleam/io
import gleam/result

pub fn main() {
  io.debug(f())
}

fn f() -> Result(Int, String) {
  let d = [#("x", 1), #("y", 2), #("z", 3)] |> dict.from_list()
  use n <- result.try(result.map_error(dict.get(d, "x"), fn(_) { "asd" }))
  Ok(n)
}
