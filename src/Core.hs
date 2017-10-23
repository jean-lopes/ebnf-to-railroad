{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
module Core where
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as Text

data Core a
    = Empty
    | Terminal a
    | NonTerminal a
    | Repeat Int (Core a)
    | Nested (Core a)
    | Optional (Core a)
    | OneOrMore (Core a)
    | ZeroOrMore (Core a)
    | OneOrMoreSep (Core a) (Core a)
    | ZeroOrMoreSep (Core a) (Core a)
    | Excluding (Core a) (Core a)
    | Sequence (NonEmpty (Core a))
    | Choice (NonEmpty (Core a))
    deriving (Show, Eq, Functor)

data Rule a
    = Rule Text (Core a)
    | AnnotatedRule Text (Rule a)
    deriving (Show, Functor)

newtype Grammar a
    = Grammar (NonEmpty (Rule a))
    deriving (Show, Functor)

toJS :: Core Text -> Text
toJS Empty                     = "''";
toJS (Terminal text)           = escapeAndQuote text
toJS (NonTerminal text)        = "NonTerminal(" <> escapeAndQuote text <> ")"
toJS (Repeat 0 _)              = toJS Empty
toJS (Repeat 1 child)          = toJS child
toJS (Repeat n child)          = toJS $ Sequence $ NonEmpty.fromList $ take n $ repeat child
toJS (Nested child)            = toJS child
toJS (Optional child)          = "Optional(" <> toJS child <> ")"
toJS (OneOrMore child)         = "OneOrMore(" <> toJS child <> ")"
toJS (ZeroOrMore child)        = "ZeroOrMore(" <> toJS child <> ")"
toJS (OneOrMoreSep child sep)  = "OneOrMore(" <> toJS child <> ", " <> toJS sep <> ")"
toJS (ZeroOrMoreSep child sep) = "ZeroOrMore(" <> toJS child <> ", " <> toJS sep <> ")"
toJS (Excluding (NonTerminal ls) (Terminal rs)) = toJS $ NonTerminal $ ls <> " - " <> rs
toJS (Excluding _ _)           = "'Semantic error. Only allowed: <meta-identifier - terminal-string>'"
toJS (Sequence children)       = "Sequence(" <> commaSep children <> ")"
toJS (Choice children)         = "Choice(0, " <> commaSep children <> ")"

commaSep :: NonEmpty (Core Text) -> Text
commaSep xs = Text.intercalate ", " $ NonEmpty.toList $ fmap toJS xs

quote :: Text -> Text
quote text = case Text.find (=='\'') text of
    Nothing ->  "'" <> text <> "'"
    _       -> "\"" <> text <> "\""

escape :: Text -> Text
escape text = Text.replace "\"" "\\\"" $ Text.replace "'" "\\'" text

escapeAndQuote :: Text -> Text
escapeAndQuote = quote . escape

-- grammar :: Grammar Text
-- grammar = Grammar
--     ( Rule {name = "syntax", value = Choice (Sequence (NonTerminal "syntax-rule" :| [ZeroOrMore (Choice (Sequence (NonTerminal "syntax-rule" :| []) :| []))]) :| [])} :|
--     [ Rule {name = "syntax-rule", value = Choice (Sequence (NonTerminal "meta-identifier" :| [Terminal "=",NonTerminal "definitions-list",Terminal ";"]) :| [])}
--     , Rule {name = "definitions-list", value = Choice (Sequence (NonTerminal "single-definition" :| [ZeroOrMore (Choice (Sequence (Terminal "|" :| [NonTerminal "single-definition"]) :| []))]) :| [])}
--     , Rule {name = "single-definition", value = Choice (Sequence (NonTerminal "term" :| [ZeroOrMore (Choice (Sequence (Terminal "," :| [NonTerminal "term"]) :| []))]) :| [])}
--     , Rule {name = "term", value = Choice (Sequence (NonTerminal "factor" :| [Optional (Choice (Sequence (Terminal"-" :| [NonTerminal "terminal"]) :| []))]) :| [])}
--     , Rule {name = "factor", value = Choice (Sequence (Optional (Choice (Sequence (NonTerminal "integer" :| [Terminal "*"]) :| [])) :| [NonTerminal "primary"]) :| [])}
--     , Rule {name = "primary", value = Choice (Sequence (NonTerminal "terminal" :| []) :| [Sequence (NonTerminal "non-terminal" :| [])])}
--     , Rule {name = "non-terminal", value = Choice (Sequence (NonTerminal "optional-sequence" :| []) :| [Sequence (NonTerminal "repeated-sequence" :| []),Sequence (NonTerminal "grouped-sequence" :| []),Sequence (NonTerminal "meta-identifier" :| [])])}
--     , Rule {name = "terminal", value = Choice (Sequence (NonTerminal "special-sequence" :| []) :| [Sequence (NonTerminal "terminal-string" :| []),Sequence (NonTerminal "empty" :| [])])}
--     , Rule {name = "empty", value = Choice (Sequence (Empty :| []) :| [])}
--     , Rule {name = "optional-sequence", value = Choice (Sequence (Terminal "[" :| [NonTerminal "definitions-list",Terminal "]"]):| [])}
--     , Rule {name = "repeated-sequence", value = Choice (Sequence (Terminal "{" :| [NonTerminal "definitions-list",Terminal "}"]) :| [])}
--     , Rule {name = "grouped-sequence", value = Choice (Sequence (Terminal "(" :| [NonTerminal "definitions-list",Terminal ")"]) :| [])}
--     , Rule {name = "terminal-string", value = Choice (Sequence (Terminal "'" :| [Excluding (NonTerminal "character") (Terminal "'"),ZeroOrMore (Choice (Sequence (Excluding (NonTerminal "character") (Terminal "'") :|[]) :| [])),Terminal "'"]) :| [Sequence (Terminal "\"" :| [Excluding (NonTerminal "character") (Terminal "\""),ZeroOrMore (Choice (Sequence (Excluding (NonTerminal "character") (Terminal "\"") :| []) :| [])),Terminal "\""])])}
--     , Rule {name = "meta-identifier", value = Choice (Sequence (NonTerminal "letter" :| [ZeroOrMore(Choice (Sequence (NonTerminal "letter" :| []) :| [Sequence (NonTerminal "decimal-digit" :| [])]))]) :| [])}
--     , Rule {name = "integer", value = Choice (Sequence (NonTerminal "decimal-digit" :| [ZeroOrMore (Choice (Sequence (NonTerminal "decimal-digit" :| []) :| []))]) :| [])}
--     , Rule {name = "special-sequence", value = Choice (Sequence (Terminal "?" :| [ZeroOrMore (Choice (Sequence (Excluding (NonTerminal "character") (Terminal "?") :| []) :| [])),Terminal "?"]) :| [])}
--     , Rule {name = "comment", value = Choice (Sequence (Terminal "(*" :| [ZeroOrMore (Choice (Sequence (NonTerminal "comment-symbol" :| []) :| [])),Terminal "*)"]) :| [])}
--     , Rule {name = "comment-symbol", value = Choice (Sequence (NonTerminal "comment" :| []) :| [Sequence (NonTerminal "terminal-string" :| []),Sequence (NonTerminal "special-sequence" :| []),Sequence (NonTerminal "character" :| [])])}
--     ])

simplify :: Core Text -> Core Text
simplify x = if x == y then x else simplify y
  where
    y = simplify' x

simplify' :: Core Text -> Core Text
simplify' (Excluding (NonTerminal ls) (Terminal rs)) = NonTerminal $ ls <> " - " <> rs
simplify' (Excluding _ _) = Terminal "Semantic error. Only allowed: <meta-identifier - terminal-string>"
simplify' (Choice (x:|[])) = simplify' x
simplify' (Sequence (x:|[])) = simplify' x
simplify' (Sequence (x:|(ZeroOrMore (Sequence (y:|[z]))):[]))
    = let a = simplify' x
          b = simplify' y
          c = simplify' z
      in  if a == c then OneOrMoreSep a b else (Sequence (a:|(ZeroOrMore (Sequence (b:|[c]))):[]))
simplify' (Sequence (x:|((ZeroOrMore y):[])))
    = let a = simplify' x
          b = simplify' y
      in  if a == b then OneOrMore a else (Sequence $ a :| [ZeroOrMore b])
simplify' Empty = Empty
simplify' (Terminal x) = Terminal x
simplify' (NonTerminal x) = NonTerminal x
simplify' (Repeat n child) = Repeat n $ simplify' child
simplify' (Nested child) = Nested $ simplify' child
simplify' (Optional child) = Optional $ simplify' child
simplify' (OneOrMore child) = OneOrMore $ simplify' child
simplify' (ZeroOrMore child) = ZeroOrMore $ simplify' child
simplify' (OneOrMoreSep child sep) = OneOrMoreSep (simplify' child) (simplify' sep)
simplify' (ZeroOrMoreSep child sep) = ZeroOrMoreSep (simplify' child) (simplify' sep)
simplify' (Sequence children) = Sequence $ fmap simplify' children
simplify' (Choice children) = Choice $ fmap simplify' children

simplifyRule :: Rule Text -> Rule Text
simplifyRule (Rule n v)          = Rule n $ simplify v
simplifyRule (AnnotatedRule a v) = AnnotatedRule a $ simplifyRule v

simplifyGrammar ::Grammar Text -> Grammar Text
simplifyGrammar (Grammar rules) = Grammar $ fmap simplifyRule rules

grammarToJS :: Grammar Text -> NonEmpty (Text, Text)
grammarToJS (Grammar rules) = fmap ruleToJS rules

ruleToJS :: Rule Text -> (Text, Text)
ruleToJS (Rule n v)          = (n, "Diagram(" <> toJS v <> ").addTo()")
ruleToJS (AnnotatedRule _ v) = ruleToJS v

-- main :: IO ()
-- main = do
--     let content = foldr (\(n, js) text -> text <> "\n" <> n <> "\n" <> js ) "" $ grammarToJS $ simplifyGrammar grammar
--     Text.writeFile "teste.txt" content

