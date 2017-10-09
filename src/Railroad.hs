{-# LANGUAGE OverloadedStrings #-}
module Railroad
where
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text
import           EBNF

main :: IO ()
main = do
    txt <- Text.readFile "EBNF.ebnf"
    case parseEBNF txt of
        (Left e)    -> putStrLn e
        (Right ast) -> mapM_ saveDiagram $ astToDiagrams ast

saveDiagram :: (Text, Text) -> IO ()
saveDiagram (name, content) = do
    Text.putStrLn name
    Text.putStrLn content
    Text.putStrLn ""
    Text.writeFile ("./railroad/" ++ Text.unpack name) content

astToDiagrams :: Syntax -> NonEmpty (Text, Text)
astToDiagrams (Syntax rules) = fmap f rules
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

newtype Diagrams = NonEmpty Diagram

class ToRailroad a where
    railroad :: a -> Railroad

class ToDiagram a where
    diagram :: a -> Text

class ToDiagram a => ToDiagrams a where
    diagrams :: NonEmpty a -> [Text]
    diagrams xs = NonEmpty.toList $ fmap diagram xs

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
    diagram (Diagram xs) = "Diagram(" <> diagram xs <> ")"
