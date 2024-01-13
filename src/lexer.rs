use core::str::{CharIndices, FromStr};

#[derive(Debug)]
pub enum Token<'a> {
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Number(f64),
    Ident(&'a str),
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    raw: &'a str,
    chars: CharIndices<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a str) -> Lexer<'a> {
        Lexer {
            raw: data,
            chars: data.char_indices(),
        }
    }

    fn eat(&mut self, f: fn(char) -> bool) -> bool {
        let mut iter = self.chars.clone();
        match iter.next() {
            Some((_, ch)) if f(ch) => {
                self.chars = iter;
                true
            }
            _ => false,
        }
    }

    fn eat_char(&mut self, ch: char) -> bool {
        let mut iter = self.chars.clone();
        match iter.next() {
            Some((_, ch2)) if ch == ch2 => {
                self.chars = iter;
                true
            }
            _ => false,
        }
    }

    fn read_token(&mut self) -> Option<anyhow::Result<Token<'a>>> {
        let (pos, c) = self.chars.next()?;

        let token = match c {
            '\n' | '\t' | ' ' => {
                while self.eat_char('\n') || self.eat_char('\t') || self.eat_char(' ') {}
                return self.read_token();
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '(' => Token::LParen,
            ')' => Token::RParen,
            c if c.is_digit(10) => {
                let mut end = pos;
                while self.eat(char::is_numeric) || self.eat_char('.') {
                    end += 1;
                }

                let str = &self.raw[pos..=end];
                Token::Number(f64::from_str(str).unwrap())
            }
            c if c.is_alphabetic() => {
                let mut iter = self.chars.clone();
                let mut end = pos;
                while let Some((p, c)) = iter.next() {
                    if !c.is_alphanumeric() {
                        break;
                    }
                    end = p
                }
                self.chars = iter;

                Token::Ident(&self.raw[pos..=end])
            }
            c => return Some(Err(anyhow::anyhow!("invalid character: {}", c))),
        };

        Some(Ok(token))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = anyhow::Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_token()
    }
}
