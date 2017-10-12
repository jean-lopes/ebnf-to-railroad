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
import           EBNF

ebnfToRailroad :: Text -> Either String (NonEmpty (Text, Text))
ebnfToRailroad ebnf = case parseEBNF ebnf of
    (Left e)    -> Left e
    (Right ast) -> Right $ astToRailroad ast

astToRailroad :: Syntax -> NonEmpty (Text, Text)
astToRailroad (Syntax rules) = fmap f rules
  where
    f (SyntaxRule name xs) = (name, diagram . Diagram $ railroad xs)

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

instance ToRailroad Primary where
    railroad (OptionalSequence xs) = Optional $ railroad xs
    railroad (RepeatedSequence xs) = ZeroOrMore $ railroad xs
    railroad (GroupedSequence xs)  = railroad xs
    railroad (MetaIdentifier xs)   = NonTerminal xs
    railroad (TerminalString xs)   = Terminal xs
    railroad (SpecialSequence xs)  = Terminal xs
    railroad Empty                 = Terminal ""

instance ToRailroad Factor where
    railroad (Factor primary)           = railroad primary
    railroad (RepeatedFactor n primary) = Sequence
                                        $ fmap railroad
                                        $ NonEmpty.take n
                                        $ NonEmpty.repeat primary

instance ToRailroad Term where
    railroad (Term xs)             = railroad xs
    railroad (ExcludingTerm xs ys) = Sequence
        [ railroad xs
        , Comment "excluding"
        , railroad ys
        ]

instance ToRailroad SingleDefinition where
    railroad (SingleDefinition xs) = Sequence
                                   $ map railroad
                                   $ NonEmpty.toList xs

instance ToRailroad DefinitionList where
    railroad (DefinitionList xs) = Choice
                                 $ fmap railroad
                                 $ NonEmpty.fromList xs

instance ToRailroad SyntaxRule where
    railroad (SyntaxRule _ xs) = railroad xs

commaSepList :: [Railroad] -> Text
commaSepList xs = Text.intercalate ", " $ fmap diagram xs

commaSepList1 :: (NonEmpty Railroad) -> Text
commaSepList1 = commaSepList . NonEmpty.toList

quote :: Text -> Text
quote text = case Text.find (=='\'') text of
    Nothing -> "'" <> text <> "'"
    _       -> "\"" <> text <> "\""

instance ToDiagram Railroad where
    diagram (Terminal text)     = "Terminal(" <> quote text <> ")"
    diagram (NonTerminal text)  = "NonTerminal(" <> quote text <> ")"
    diagram (Comment text)      = "Comment(" <> quote text <> ")"
    diagram (Sequence children) = "Sequence(" <> commaSepList children <> ")"
    diagram (Choice children)   = "Choice(0, " <> commaSepList1 children <> ")"
    diagram (Optional child)    = "Optional(" <> diagram child <> ", 'skip')"
    diagram (OneOrMore child)   = "OneOrMore(" <> diagram child <> ")"
    diagram (ZeroOrMore child)  = "ZeroOrMore(" <> diagram child <> ", '', 'skip')"

instance ToDiagram Diagram where
    diagram (Diagram xs) = "Diagram(" <> diagram xs <> ").addTo()"
