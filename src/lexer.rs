use core::str::{CharIndices, FromStr};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Number(f64),
    Ident(&'a str),
    Comma,
}

pub struct Lexer<'a> {
    raw: &'a str,
    chars: CharIndices<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(str: &'a str) -> Self {
        Self {
            raw: str,
            chars: str.char_indices(),
        }
    }

    pub fn eat(&mut self, f: fn(char) -> bool) -> bool {
        let mut iter = self.chars.clone();
        match iter.next() {
            Some((_, ch)) if f(ch) => {
                self.chars = iter;
                true
            }
            _ => false,
        }
    }

    fn next_inner(&mut self) -> Option<anyhow::Result<Token<'a>>> {
        let (pos, c) = self.chars.next()?;

        let token = match c {
            ' ' | '\t' => {
                while self.eat(|c| c == ' ' || c == '\t') {}
                return self.next_inner();
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            c if c.is_numeric() => {
                let mut end = pos;
                while self.eat(|c| c.is_numeric() || c == '.') {
                    end += 1;
                }
                let str = &self.raw[pos..=end];
                Token::Number(f64::from_str(str).unwrap())
            }
            c if c.is_alphabetic() => {
                let mut end = pos;
                while self.eat(|c| c.is_alphanumeric()) {
                    end += 1;
                }
                let str = &self.raw[pos..=end];
                Token::Ident(str)
            }

            c => return Some(Err(anyhow::anyhow!("unexpected character: {}", c))),
        };

        Some(Ok(token))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = anyhow::Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_inner()
    }
}
