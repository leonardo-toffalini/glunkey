import ast
import eval
import gleam/erlang
import gleam/io
import lexer
import object
import parser

pub fn main() {
  let prompt = ">> "

  let assert Ok(line) = erlang.get_line(prompt)
  let tokens = lexer.lex(line)

  case parser.parse(tokens) {
    Ok(program) ->
      case eval.eval(ast.ProgramNode(program)) {
        Ok(obj) -> object.object_to_string(obj) |> io.println
        Error(e) -> io.println("Error: " <> e)
      }
    Error(e) -> io.println("Error: " <> e)
  }

  main()
}
