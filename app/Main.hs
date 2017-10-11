{-# LANGUAGE OverloadedStrings #-}
module Main
where
import           Data.Semigroup      ((<>))
import qualified Data.Text.IO        as Text
import           EBNF
import           Options.Applicative
import           Railroad            ()

data Args = Args
    { input   :: FilePath
    , output  :: FilePath
    , showAst :: Bool
    } deriving Show

args :: Parser Args
args = Args
    <$> strOption
        (  long "input"
        <> short 'i'
        <> metavar "TARGET"
        <> help "EBNF grammar file location." )
    <*> strOption
        (  long "output"
        <> short 'o'
        <> help "Output directory."
        )
    <*> switch
        (  long "ast"
        <> help "Also prints the Abstract Syntax Tree." )

execute :: Args -> IO ()
execute (Args i o a) = do
    ebnf <- Text.readFile i
    case parseEBNF ebnf of
        (Left e) -> putStrLn e
        (Right ast) -> if a
            then Text.writeFile (o ++ "ast.txt") $ astToText ast
            else Text.putStrLn ""

main :: IO ()
main = execute =<< execParser opts
  where
    opts = info (args <**> helper) (fullDesc <> progDesc (unlines description))
    description =
        [ "Generate Railroad diagrams from a EBNF grammar."
        , "Requires 'npm' (https://www.npmjs.com/), and "
        , "the package 'diagrams' (https://www.npmjs.com/package/diagrams)."
        , "'npm' and 'diagrams' must be available in the O.S. PATH variable"
        ]
