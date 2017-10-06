module EBNF.Parser
where
import           Control.Monad         (void)
import qualified EBNF.AST              as AST
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lexer
import           Text.Megaparsec.Text  (Parser)

{-
gap free symbol
    = terminal character - (first quote symbol | second quote symbol)
    | terminal string;

terminal string
    = first quote symbol, first terminal character, {first terminal character}, first quote symbol
    | second quote symbol, second terminal character,{second terminal character},second quote symbol;

first terminal character
    = terminal character - first quote symbol;

second terminal character
    = terminal character - second quote symbol;

gap separator
    = space character
    | horizontal tabulation character
    | new line
    | vertical tabulation character
    | form feed;

syntax = {gap separator}, gap free symbol, {gap separator}, {gap free symbol, {gap separator}};
-}
