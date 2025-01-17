pub type TokenType {
  Illegal
  EOF

  Ident
  Int

  // operators
  Plus
  Minus
  Star
  Slash
  Bang

  Eq
  Neq
  Greater
  GreaterEq
  Less
  LessEq

  Comma
  Semicolon

  LParen
  RParen
  LBrace
  RBrace
}

pub type Token {
  Token(ttype: TokenType, literal: String)
}
