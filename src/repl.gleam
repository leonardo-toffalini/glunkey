import ast
import environment
import eval
import gleam/erlang
import gleam/io
import lexer
import object
import parser

fn repl(env: environment.Environment) {
  let prompt = ">> "

  let assert Ok(line) = erlang.get_line(prompt)
  let tokens = lexer.lex(line)

  case parser.parse(tokens) {
    Ok(program) ->
      case eval.eval(ast.ProgramNode(program), env) {
        Ok(#(obj, env)) -> {
          object.object_to_string(obj) |> io.println
          repl(env)
        }
        Error(e) -> {
          io.println("Error: " <> e)
          repl(env)
        }
      }
    Error(e) -> {
      io.println("Error: " <> e)
      repl(env)
    }
  }
}

pub fn main() {
  let env = environment.new()
  repl(env)
}
