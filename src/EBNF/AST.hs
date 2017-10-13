module EBNF.AST
where
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (Text)

newtype Syntax
    = Syntax (NonEmpty SyntaxRule)
    deriving Show

data SyntaxRule
    = SyntaxRule Text DefinitionList
    deriving Show

newtype DefinitionList
    = DefinitionList (NonEmpty SingleDefinition)
    deriving Show

newtype SingleDefinition
    = SingleDefinition (NonEmpty Term)
    deriving Show

data Term
    = Term Factor
    | ExcludingTerm Factor Terminal
    deriving Show

data Factor
    = Factor Primary
    | RepeatedFactor Int Primary
    deriving Show

data Primary
    = PrimaryTerminal Terminal
    | PrimaryNonTerminal NonTerminal
    deriving Show

data NonTerminal
    = OptionalSequence DefinitionList
    | RepeatedSequence DefinitionList
    | GroupedSequence DefinitionList
    | MetaIdentifier Text
    deriving Show

data Terminal
    = TerminalString Text
    | SpecialSequence Text
    | Empty
    deriving Show
