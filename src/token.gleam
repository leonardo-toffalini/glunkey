import gleam/dict

pub type TokenType {
  Illegal
  EOF

  Ident
  Int

  // operators
  Assign
  Plus

  // delimiters
  Comma
  Semicolon

  LParen
  RParen
  LBrace
  RBrace

  // keywords
  Function
  Let
}

pub type Token {
  Token(ttype: TokenType, literal: String)
}

pub fn look_up_ident(ident: String) -> TokenType {
  let kw = [#("fn", Function), #("let", Let)]
  let keywords = dict.from_list(kw)
  case dict.get(keywords, ident) {
    Ok(v) -> v
    Error(Nil) -> Ident
  }
}
