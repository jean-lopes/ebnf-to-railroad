module Core.EBNF
    ( parseEBNF
    ) where
import           Control.Monad         (void)
import           Core
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lexer
import           Text.Megaparsec.Text  (Parser)

-- | Whitespace consumer
-- |
-- | Also consumers block comments starting with @(*@ and ending with @*)@
whitespace :: Parser ()
whitespace = Lexer.space (void spaceChar) line block
  where
    line = Lexer.skipLineComment "\0"
    block = Lexer.skipBlockComment "(*" "*)"

-- | Consumes whitespace after a successful parse
lexeme :: Parser a -> Parser a
lexeme p = Lexer.lexeme whitespace p

-- | @Parser a@ only succeeds if @Parser b@ have failed
-- consonant = letterChar `excluding` (oneOf "aeiou")
excluding :: Parser a -> Parser b -> Parser a
excluding x y = notFollowedBy y *> x

enclosedBy :: Parser a -> Char -> Parser [a]
enclosedBy p c = delimiter *> some (p `excluding` delimiter) <* delimiter
  where
    delimiter = char c

closed :: Char -> Parser a -> Char -> Parser a
closed o p c = open *> p <* close
  where
    open = lexeme $ char o
    close = lexeme $ char c

nonEmptyList :: Parser a -> Parser (NonEmpty a)
nonEmptyList p = NonEmpty.fromList <$> some p

nonEmptyListSepBy :: Parser a -> Char -> Parser (NonEmpty a)
nonEmptyListSepBy p c = NonEmpty.fromList <$> sepBy1 p sep
  where
    sep = lexeme $ char c

identifier :: Parser Text
identifier = lexeme $ Text.pack <$> text
  where
    text = (:) <$> letterChar <*> many (alphaNumChar <|> char '-')

integer :: Parser Int
integer = lexeme $ read <$> some digitChar

specialSequence' :: Parser String
specialSequence' = printChar `enclosedBy` '?'

terminalString' :: Parser String
terminalString' =   printChar `enclosedBy` '\'' <|> printChar `enclosedBy` '"'

specialSequence :: Parser (Core Text)
specialSequence = Terminal . Text.pack <$> lexeme specialSequence'

terminalString :: Parser (Core Text)
terminalString = Terminal . Text.pack <$> lexeme terminalString'

empty :: Parser (Core Text)
empty = pure Empty

metaIdentifier :: Parser (Core Text)
metaIdentifier = NonTerminal <$> identifier

groupedSequence :: Parser (Core Text)
groupedSequence = Nested <$> closed '(' definitionList ')'

optionalSequence :: Parser (Core Text)
optionalSequence = Optional <$> closed '[' definitionList ']'

repeatedSequence :: Parser (Core Text)
repeatedSequence = ZeroOrMore <$> closed '{' definitionList '}'

terminal :: Parser (Core Text)
terminal = choice
  [ specialSequence
  , terminalString
  , empty
  ]

nonTerminal :: Parser (Core Text)
nonTerminal = choice
  [ repeatedSequence
  , optionalSequence
  , groupedSequence
  , metaIdentifier
  ]

primary :: Parser (Core Text)
primary = nonTerminal <|> terminal

repeatedFactor :: Parser (Core Text)
repeatedFactor = Repeat <$> integer <* asterisk <*> primary
  where
    asterisk = lexeme $ char '*'

factor :: Parser (Core Text)
factor = repeatedFactor <|> primary

excludingTerm :: Parser (Core Text)
excludingTerm = Excluding <$> (metaIdentifier <|> terminalString) <* minus <*> terminalString
  where
    minus = lexeme $ char '-'

term :: Parser (Core Text)
term = try excludingTerm <|> factor

singleDefinition :: Parser (Core Text)
singleDefinition = Sequence <$> nonEmptyListSepBy term ','

definitionList :: Parser (Core Text)
definitionList = Choice <$> nonEmptyListSepBy singleDefinition '|'

syntaxRule :: Parser (Rule Text)
syntaxRule = Rule <$> identifier <* equals <*> definitionList <* terminator
  where
    equals = lexeme $ char '='
    terminator = lexeme $ char ';'

syntax :: Parser (Grammar Text)
syntax = Grammar <$> nonEmptyList syntaxRule

parseEBNF :: Text -> Either String (Grammar Text)
parseEBNF str = case (parse (whitespace *> syntax <* eof) "<input>" str) of
  (Left er) -> Left $ parseErrorPretty er
  (Right x) -> Right x
