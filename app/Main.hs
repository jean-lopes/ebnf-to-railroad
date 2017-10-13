{-# LANGUAGE OverloadedStrings #-}
module Main
where
import           Data.Default
import           Data.Semigroup      ((<>))
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           EBNF
import           Html
import           Options.Applicative
import           Railroad
import           System.EasyFile     ((<.>), (</>))
import qualified System.EasyFile     as EasyFile
import           System.Process
import           Twitch

data Args
    = Watch
    | Normal { inputArg :: FilePath, outputArg :: FilePath, showAstArg :: Bool }
    deriving Show

watchParser :: Parser Args
watchParser = flag' Watch
    (  long "watch"
    <> short 'w'
    <> help "Watch changes to any EBNF in current the directory" )

normalParser :: Parser Args
normalParser = Normal
    <$> strOption
        (  long "input"
        <> short 'i'
        <> metavar "TARGET"
        <> help "EBNF grammar file location" )
    <*> option auto
        (  long "output"
        <> short 'o'
        <> help "Output directory"
        <> showDefault
        <> value "."
        <> metavar "DIR" )
    <*> switch
        (  long "ast"
        <> help "Also prints the Abstract Syntax Tree" )

argsParser :: Parser Args
argsParser = watchParser <|> normalParser

execute :: Args -> IO ()
execute Watch = watch
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
            let htmlFile = generateHtml fileName $ astToRailroad ast
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

watch :: IO ()
watch = defaultMainWithOptions def $ do
    "*.ebnf" |> \path -> system $ "ebnf-to-railroad -i " <> path

main :: IO ()
main = execute =<< execParser opts
    where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc desc)
    desc = "Generate Railroad diagrams from a EBNF grammar."
