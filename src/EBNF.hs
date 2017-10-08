{-# LANGUAGE OverloadedStrings #-}
module EBNF
    ( module EBNF.AST
    , module EBNF.Parser
    , Display(view)
    ) where
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

    node :: String -> a -> Tree String
    node str x = Node str [ tree x ]

    subNodes :: [a] -> Forest String
    subNodes = fmap tree

    tree :: a -> Tree String

instance Display Integer where
    tree n = Node (show n) []

instance Display Text where
    tree xs = Node label []
      where
        label = Text.unpack xs

instance Display Primary where
    tree (OptionalSequence xs) = Node "primary" [ node "optional-sequence" xs ]
    tree (RepeatedSequence xs) = Node "primary" [ node "repeated-sequence" xs ]
    tree (GroupedSequence xs)  = Node "primary" [ node "grouped-sequence" xs ]
    tree (MetaIdentifier xs)   = Node "primary" [ node "meta-identifier" xs ]
    tree (TerminalString xs)   = Node "primary" [ node "terminal-string" xs ]
    tree (SpecialSequence xs)  = Node "primary" [ node "special-sequence" xs ]
    tree Empty                 = Node "primary" [ Node "empty" [] ]

instance Display Factor where
    tree (Factor xs)           = Node "factor" [ node "single" xs ]
    tree (RepeatedFactor n xs) = Node "factor" [ Node "repeated" [ tree n, tree xs ] ]

instance Display Term where
    tree (Term xs)             = node "term" xs
    tree (ExcludingTerm xs ys) = Node "term" [ tree xs, node "excluding" ys ]

instance Display SingleDefinition where
    tree (SingleDefinition xs) = Node "single-definition" $ subNodes $ NonEmpty.toList xs

instance Display DefinitionList where
    tree (DefinitionList xs) = Node "definition-list" $ subNodes xs

instance Display SyntaxRule where
    tree (SyntaxRule xs ys) = Node "syntax-rule" [ tree xs, tree ys ]

instance Display Syntax where
    tree (Syntax xs) = Node "syntax" $ subNodes $ NonEmpty.toList xs
