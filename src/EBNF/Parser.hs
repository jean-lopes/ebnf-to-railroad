module EBNF.Parser
  ( parseEBNF
  ) where
import           Control.Monad         (void)
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           EBNF.AST
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
identifier = lexeme $ Text.pack <$> name
  where
    name = (:) <$> letterChar <*> many (alphaNumChar <|> char '-')

integer :: Parser Int
integer = lexeme $ read <$> some digitChar

specialSequence' :: Parser String
specialSequence' = printChar `enclosedBy` '?'

terminalString' :: Parser String
terminalString' =   printChar `enclosedBy` '\'' <|> printChar `enclosedBy` '"'

specialSequence :: Parser Terminal
specialSequence = SpecialSequence . Text.pack <$> lexeme specialSequence'

terminalString :: Parser Terminal
terminalString = TerminalString . Text.pack <$> lexeme terminalString'

empty :: Parser Terminal
empty = pure Empty

metaIdentifier :: Parser NonTerminal
metaIdentifier = MetaIdentifier <$> identifier

groupedSequence :: Parser NonTerminal
groupedSequence = GroupedSequence <$> closed '(' definitionList ')'

optionalSequence :: Parser NonTerminal
optionalSequence = OptionalSequence <$> closed '[' definitionList ']'

repeatedSequence :: Parser NonTerminal
repeatedSequence = RepeatedSequence <$> closed '{' definitionList '}'

terminal :: Parser Terminal
terminal = choice
    [ specialSequence
    , terminalString
    , empty
    ]

nonTerminal :: Parser NonTerminal
nonTerminal = choice
    [ repeatedSequence
    , optionalSequence
    , groupedSequence
    , metaIdentifier
    ]

primary :: Parser Primary
primary =   PrimaryNonTerminal <$> nonTerminal
        <|> PrimaryTerminal <$> terminal

repeatedFactor :: Parser Factor
repeatedFactor = RepeatedFactor <$> integer <* asterisk <*> primary
  where
    asterisk = lexeme $ char '*'

factor :: Parser Factor
factor = repeatedFactor <|> Factor <$> primary

excludingTerm :: Parser Term
excludingTerm = ExcludingTerm <$> factor <* minus <*> terminal
  where
    minus = lexeme $ char '-'

term :: Parser Term
term = try excludingTerm <|> Term <$> factor

singleDefinition :: Parser SingleDefinition
singleDefinition = SingleDefinition <$> nonEmptyListSepBy term ','

definitionList :: Parser DefinitionList
definitionList = DefinitionList <$> nonEmptyListSepBy singleDefinition '|'

syntaxRule :: Parser SyntaxRule
syntaxRule = SyntaxRule <$> identifier <* equals <*> definitionList <* terminator
  where
    equals = lexeme $ char '='
    terminator = lexeme $ char ';'

syntax :: Parser Syntax
syntax = Syntax <$> nonEmptyList syntaxRule

parseEBNF :: Text -> Either String Syntax
parseEBNF str = case (parse (whitespace *> syntax <* eof) "<input>" str) of
    (Left er) -> Left $ parseErrorPretty er
    (Right x) -> Right x
