module EBNF.Parser.Terminal
where
import           EBNF.Parser.Char
import           EBNF.Parser.Internal

gapFreeSymbol :: Parser String
gapFreeSymbol = (:[]) <$> terminalCharacter <|> terminalString

terminalString :: Parser String
terminalString
    =   firstQuoteSymbol *> (some firstTerminalCharacter) <* firstQuoteSymbol
    <|> secondQuoteSymbol *> (some secondTerminalCharacter) <* secondQuoteSymbol

gapSeparator :: Parser Char
gapSeparator =   spaceCharacter
             <|> horizontalTabulationCharacter
             <|> newline
             <|> verticalTabulationCharacter
             <|> formFeed

syntax :: Parser [String]
syntax = separators *> some symbols
  where
    separators = many gapSeparator
    symbols = gapFreeSymbol <* separators
