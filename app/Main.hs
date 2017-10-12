{-# LANGUAGE OverloadedStrings #-}
module Main
where
import           Data.Semigroup      ((<>))
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           EBNF
import           Html
import           Options.Applicative
import           Railroad
import           System.EasyFile     ((<.>), (</>))
import qualified System.EasyFile     as EasyFile

data Args = Args
    { inputArg   :: FilePath
    , outputArg  :: FilePath
    , showAstArg :: Bool
    } deriving Show

argsParser :: Parser Args
argsParser = Args
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
execute args = do
    input <- EasyFile.canonicalizePath $ inputArg args

    let fileName = EasyFile.dropExtensions $ EasyFile.takeFileName input

    ebnf <- Text.readFile input

    outputDir <- EasyFile.canonicalizePath $ outputArg args

    EasyFile.createDirectoryIfMissing True outputDir

    output <- EasyFile.canonicalizePath $ outputDir </> fileName <.> "html"

    case parseEBNF ebnf of
        (Left e) -> putStrLn e
        (Right ast) -> do
            let htmlFile = Text.pack $ generateHtml fileName $ astToRailroad ast
            Text.putStr "Writting Railroad diagrams: "
            Text.putStrLn $ Text.pack output
            Text.writeFile output htmlFile
            if showAstArg args
                then printAST outputDir ast
                else return ()

printAST :: FilePath -> Syntax -> IO ()
printAST output ast = do
    astFile <- EasyFile.canonicalizePath $ output </> "ast" <.> "txt"
    Text.putStr "Writting Abstract Syntax Tree (AST) file: "
    Text.putStrLn $ Text.pack astFile
    Text.writeFile astFile $ astToText ast

main :: IO ()
main = execute =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc (unlines description))
    description =
        [ "Generate Railroad diagrams from a EBNF grammar."
        , "Requires 'npm' (https://www.npmjs.com/), and "
        , "the package 'diagrams' (https://www.npmjs.com/package/diagrams)."
        , "'npm' and 'diagrams' must be available in the O.S. PATH variable"
        ]
