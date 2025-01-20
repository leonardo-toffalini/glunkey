import gleam/dict

pub type TokenType {
  Illegal
  EOF

  Ident
  Int

  // operators
  Assign
  Plus
  Minus
  Bang
  Star
  Slash

  Eq
  Neq
  Lt
  Le
  Gt
  Ge

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
  TRUE
  FALSE
  If
  Else
  Return
}

pub type Token {
  Token(ttype: TokenType, literal: String)
}

pub fn look_up_ident(ident: String) -> TokenType {
  let kw = [
    #("fn", Function),
    #("let", Let),
    #("true", TRUE),
    #("false", FALSE),
    #("if", If),
    #("else", Else),
    #("return", Return),
  ]
  let keywords = dict.from_list(kw)
  case dict.get(keywords, ident) {
    Ok(v) -> v
    Error(Nil) -> Ident
  }
}
