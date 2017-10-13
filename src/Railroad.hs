{-# LANGUAGE OverloadedStrings #-}
module Railroad
    ( ebnfToRailroad
    , astToRailroad
    ) where
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified EBNF.AST           as AST
import           EBNF.Parser        (parseEBNF)

ebnfToRailroad :: Text -> Either String (NonEmpty (Text, Text))
ebnfToRailroad ebnf = case parseEBNF ebnf of
    (Left e)    -> Left e
    (Right ast) -> Right $ astToRailroad ast

astToRailroad :: AST.Syntax -> NonEmpty (Text, Text)
astToRailroad (AST.Syntax rules) = fmap f rules
  where
    f (AST.SyntaxRule name xs) = (name, diagram . Diagram $ railroad xs)

data Railroad
    = Terminal Text
    | NonTerminal Text
    | Comment Text
    | Sequence [Railroad]
    | Choice (NonEmpty Railroad)
    | Optional Railroad
    | OneOrMore Railroad
    | ZeroOrMore Railroad
    deriving Show

data Diagram
    = Diagram Railroad
    deriving Show

class ToRailroad a where
    railroad :: a -> Railroad

class ToDiagram a where
    diagram :: a -> Text

instance ToRailroad AST.Terminal where
    railroad AST.Empty                = Terminal ""
    railroad (AST.SpecialSequence xs) = Terminal xs
    railroad (AST.TerminalString xs)  = Terminal xs

instance ToRailroad AST.NonTerminal where
    railroad (AST.OptionalSequence xs) = Optional $ railroad xs
    railroad (AST.RepeatedSequence xs) = ZeroOrMore $ railroad xs
    railroad (AST.GroupedSequence xs)  = railroad xs
    railroad (AST.MetaIdentifier xs)   = NonTerminal xs

instance ToRailroad AST.Primary where
    railroad (AST.PrimaryTerminal xs)    = railroad xs
    railroad (AST.PrimaryNonTerminal xs) = railroad xs

instance ToRailroad AST.Factor where
    railroad (AST.Factor primary)           = railroad primary
    railroad (AST.RepeatedFactor n primary) = Sequence
                                            $ fmap railroad
                                            $ NonEmpty.take n
                                            $ NonEmpty.repeat primary

instance ToRailroad AST.Term where
    railroad (AST.Term xs)             = railroad xs
    railroad (AST.ExcludingTerm xs ys) = excludeStr `appendTo` railroad xs
      where
        terminalText AST.Empty                  = ""
        terminalText (AST.SpecialSequence text) = text
        terminalText (AST.TerminalString text)  = text
        excludeStr = " - " <> (quote . terminalText) ys

instance ToRailroad AST.SingleDefinition where
    railroad (AST.SingleDefinition xs) = Sequence
                                       $ map railroad
                                       $ NonEmpty.toList xs

instance ToRailroad AST.DefinitionList where
    railroad (AST.DefinitionList xs) = Choice $ fmap railroad xs

instance ToRailroad AST.SyntaxRule where
    railroad (AST.SyntaxRule _ xs) = railroad xs

commaSepList :: [Railroad] -> Text
commaSepList xs = Text.intercalate ", " $ fmap diagram xs

commaSepList1 :: (NonEmpty Railroad) -> Text
commaSepList1 = commaSepList . NonEmpty.toList

quote :: Text -> Text
quote text = case Text.find (=='\'') text of
    Nothing ->  "'" <> text <> "'"
    _       -> "\"" <> text <> "\""

escapeAndQuote :: Text -> Text
escapeAndQuote = quote . escape

escape :: Text -> Text
escape text = Text.replace "\"" "\\\"" $ Text.replace "'" "\\'" text

appendTo :: Text -> Railroad -> Railroad
appendTo xs (Terminal text)     = Terminal $ text <> xs
appendTo xs (NonTerminal text)  = NonTerminal $ text <> xs
appendTo xs (Comment text)      = Comment $ text <> xs
appendTo xs (Sequence children) = Sequence $ fmap (appendTo xs) children
appendTo xs (Choice children)   = Choice $ fmap (appendTo xs) children
appendTo xs (Optional child)    = Optional $ appendTo xs child
appendTo xs (OneOrMore child)   = OneOrMore $ appendTo xs child
appendTo xs (ZeroOrMore child)  = ZeroOrMore $ appendTo xs child

instance ToDiagram Railroad where
    diagram (Terminal text)     = "Terminal(" <> escapeAndQuote text <> ")"
    diagram (NonTerminal text)  = "NonTerminal(" <> escapeAndQuote text <> ")"
    diagram (Comment text)      = "Comment(" <> escapeAndQuote text <> ")"
    diagram (Sequence children) = "Sequence(" <> commaSepList children <> ")"
    diagram (Choice children)   = "Choice(0, " <> commaSepList1 children <> ")"
    diagram (Optional child)    = "Optional(" <> diagram child <> ", 'skip')"
    diagram (OneOrMore child)   = "OneOrMore(" <> diagram child <> ")"
    diagram (ZeroOrMore child)  = "ZeroOrMore(" <> diagram child <> ", '', 'skip')"

instance ToDiagram Diagram where
    diagram (Diagram xs) = "Diagram(" <> diagram xs <> ").addTo()"
