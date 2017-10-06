module EBNF.Parser.Internal
    ( module Text.Megaparsec
    , Parser
    , whitespace
    , lexeme
    ) where
import           Control.Monad          (void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer  as Lexer
import           Text.Megaparsec.String (Parser)

whitespace :: Parser ()
whitespace = lexeme $ Lexer.space (void spaceChar) line block
  where
    line = lexeme $ Lexer.skipLineComment "//"
    block = lexeme $ Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = Lexer.lexeme whitespace p
