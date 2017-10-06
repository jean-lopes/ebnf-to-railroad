module EBNF.Parser.Char
where
import           EBNF.Parser.Internal

letter :: Parser Char
letter = lowerChar <|> upperChar

digit :: Parser Char
digit = digitChar

concatenateSymbol :: Parser Char
concatenateSymbol = char ','

definingSymbol :: Parser Char
definingSymbol = char '='

definitionSeparatorSymbol :: Parser Char
definitionSeparatorSymbol = char '|'

endGroupSymbol :: Parser Char
endGroupSymbol = char ')'

endOptionSymbol :: Parser Char
endOptionSymbol = char ']'

endRepeatSymbol :: Parser Char
endRepeatSymbol = char '}'

exceptSymbol :: Parser Char
exceptSymbol = char '-'

firstQuoteSymbol :: Parser Char
firstQuoteSymbol = char '\'';

repetitionSymbol :: Parser Char
repetitionSymbol = char '*'

secondQuoteSymbol :: Parser Char
secondQuoteSymbol = char '"'

specialSequenceSymbol :: Parser Char
specialSequenceSymbol = char '?'

startGroupSymbol :: Parser Char
startGroupSymbol = char '('

startOptionSymbol :: Parser Char
startOptionSymbol = char '['

startRepeatSymbol :: Parser Char
startRepeatSymbol = char '{'

terminatorSymbol :: Parser Char
terminatorSymbol = char ';'

otherCharacter :: Parser Char
otherCharacter = oneOf " :+_%@&#$<>\\^`~"

spaceCharacter :: Parser Char
spaceCharacter = char ' '

horizontalTabulationCharacter :: Parser Char
horizontalTabulationCharacter = char '\t'

verticalTabulationCharacter :: Parser Char
verticalTabulationCharacter = char '\x0B'

formFeed :: Parser Char
formFeed = char '\x0C'

terminalCharacter :: Parser Char
terminalCharacter = choice
    [ letter
    , digit
    , concatenateSymbol
    , definingSymbol
    , definitionSeparatorSymbol
    , endGroupSymbol
    , endOptionSymbol
    , endRepeatSymbol
    , exceptSymbol
    , repetitionSymbol
    , specialSequenceSymbol
    , startGroupSymbol
    , startOptionSymbol
    , startRepeatSymbol
    , terminatorSymbol
    , otherCharacter
    ]

firstTerminalCharacter :: Parser Char
firstTerminalCharacter = secondQuoteSymbol <|> terminalCharacter

secondTerminalCharacter :: Parser Char
secondTerminalCharacter = firstQuoteSymbol <|> terminalCharacter
