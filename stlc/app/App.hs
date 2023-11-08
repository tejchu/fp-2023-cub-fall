module App where

-- optparse-applicative
import Options.Applicative
import Parser (parseLambdaTerm)
import TypeCheck
import Data.Text (Text)
import Error

data Transformation
  = TypeCheck
  | Parse

data Action = Action
  { transformation :: Transformation
  , input :: Text
  }

data Args = Args
  { transformationArg :: Transformation
  , inputArg :: Text
  }

actionParser :: Parser Args
actionParser =
  Args <$> parseTransformation
       <*> inputParser

inputParser :: Parser Text
inputParser = strOption
  (  long "input"
  <> short 'i'
  <> metavar "INPUT"
  <> help "String input"
  )

parseTransformation :: Parser Transformation
parseTransformation =
      typeCheckParser
  <|> parserParser

parserParser :: Parser Transformation
parserParser = flag' Parse
  (  long "parse"
  <> short 'p'
  <> help "Parse the term"
  )


typeCheckParser :: Parser Transformation
typeCheckParser = flag' TypeCheck
  (  long "typeCheck"
  <> short 'c'
  <> help "Type check the term"
  )

transform :: Args -> IO Action
transform (Args transformation input) = do
  return $ Action transformation input

printEither :: Show a => Either LambdaError a -> IO ()
printEither (Left err) = do
  putStrLn "Error"
  putStrLn (show err)
printEither (Right x) = do
  putStrLn "Ok"
  print x

runAction :: Args -> IO ()
runAction args = do
  action <- transform args
  case transformation action of
    TypeCheck -> do
      let parsed = parseLambdaTerm (input action)
      case parsed of
        Left err -> putStrLn $ show err
        Right term -> printEither $ typeCheckEmpty term
    Parse ->
      printEither $ parseLambdaTerm (input action)

runApp :: IO ()
runApp = do
    runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "STLC type checker"
      <> header "stlc"
      )
