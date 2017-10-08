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
    = DefinitionList [SingleDefinition]
    deriving Show

newtype SingleDefinition
    = SingleDefinition (NonEmpty Term)
    deriving Show

data Term
    = Term Factor
    | ExcludingTerm Factor Factor
    deriving Show

data Factor
    = Factor Primary
    | RepeatedFactor Integer Primary
    deriving Show

data Primary
    = OptionalSequence DefinitionList
    | RepeatedSequence DefinitionList
    | GroupedSequence DefinitionList
    | MetaIdentifier Text
    | TerminalString Text
    | SpecialSequence Text
    | Empty
    deriving Show

