import ast
import eval
import gleam/erlang
import gleam/io
import gleam/option.{None}
import lexer
import object.{type Environment, new}
import parser

fn repl(env: Environment) {
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
  let env = new(None)
  repl(env)
}
