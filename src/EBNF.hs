{-# LANGUAGE OverloadedStrings #-}
module EBNF
    ( module EBNF.AST
    , module EBNF.Parser
    , Display(view, astToText)
    ) where
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text
import           Data.Tree
import           EBNF.AST
import           EBNF.Parser

class Display a where
    view :: a -> IO ()
    view = Text.putStrLn . Text.pack . drawTree . tree

    astToText :: a -> Text
    astToText = Text.pack . drawTree . tree

    node :: String -> a -> Tree String
    node str x = Node str [ tree x ]

    subNodes :: NonEmpty a -> Forest String
    subNodes = NonEmpty.toList . fmap tree

    tree :: a -> Tree String

instance Display Int where
    tree n = Node (show n) []

instance Display Text where
    tree xs = Node label []
      where
        label = Text.unpack xs

instance Display Terminal where
    tree Empty                = Node "terminal" [ Node "empty" [] ]
    tree (SpecialSequence xs) = Node "terminal" [ node "special-sequence" xs ]
    tree (TerminalString xs)  = Node "primary" [ node "terminal-string" xs ]

instance Display NonTerminal where
    tree (OptionalSequence xs) = Node "non-terminal" [ node "optional-sequence" xs ]
    tree (RepeatedSequence xs) = Node "non-terminal" [ node "repeated-sequence" xs ]
    tree (GroupedSequence xs)  = Node "non-terminal" [ node "grouped-sequence" xs ]
    tree (MetaIdentifier xs)   = Node "non-terminal" [ node "meta-identifier" xs ]

instance Display Primary where
    tree (PrimaryTerminal xs)    = node "primary" xs
    tree (PrimaryNonTerminal xs) = node "primary" xs

instance Display Factor where
    tree (Factor xs)           = Node "factor" [ node "single" xs ]
    tree (RepeatedFactor n xs) = Node "factor" [ Node "repeated" [ tree n, tree xs ] ]

instance Display Term where
    tree (Term xs)             = node "term" xs
    tree (ExcludingTerm xs ys) = Node "term" [ tree xs, node "excluding" ys ]

instance Display SingleDefinition where
    tree (SingleDefinition xs) = Node "single-definition" $ subNodes xs

instance Display DefinitionList where
    tree (DefinitionList xs) = Node "definition-list" $ subNodes xs

instance Display SyntaxRule where
    tree (SyntaxRule xs ys) = Node "syntax-rule" [ tree xs, tree ys ]

instance Display Syntax where
    tree (Syntax xs) = Node "syntax" $ subNodes xs
