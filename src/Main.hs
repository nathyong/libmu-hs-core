{-#LANGUAGE NoImplicitPrelude#-}

import Prelude
import System.Environment (getArgs)
import CommandLine
import Parser
import Compile
import LibMu.Builder (pp, checkBuilder)
import LibMu.Execute (runBundle)

data Arg = File
         | Output
         | Check
         | Parse
         | Run

arguments :: [StdArgument Arg]
arguments = [
  Argument "--file" "-f" "bf file to use" True (STRING Nothing) File,
  Argument "--output" "-o" "output file to use" False (STRING Nothing) Output,
  Argument "--check" "-c" "output type check log" False (PRESENT False) Check,
  Argument "--parse" "-p" "Parse File" False (PRESENT False) Parse,
  Argument "--run" "-r" "Run Generated Bundle" False (PRESENT False) Run
  ]

main :: IO ()
main = do
  opts <- getArgs
  let args = parse arguments opts
  case args of
    Left err -> putStrLn $ flag err
    Right [f, o, c, p, r] -> do
      let file = fromMandatorySTRING $ value f
          parse' = fromMandatoryPRESENT $ value p
          check' = fromMandatoryPRESENT $ value c
          run' = fromMandatoryPRESENT $ value r
      parsed <- (runParser parseProgram () file) <$> readFile file
      case parsed of
        Left err -> putStrLn $ show err
        Right parsedProg -> do
          case value o of
            STRING Nothing ->
              if parse' then do
                putStrLn $ show parsedProg
              else
                case compileProgram parsedProg of
                  Left err -> putStrLn $ err
                  Right bs -> 
                    if check' then do
                      putStrLn "Type Check"
                      putStrLn $ unlines $ checkBuilder bs
                      if run' then
                        runBundle $ pp bs
                      else
                        putStrLn $ pp bs                      
                    else
                      if run' then
                        runBundle $ pp bs
                      else
                        putStrLn $ pp bs
            STRING (Just output) -> 
              if parse' then do
                writeFile output $ show parsedProg
              else
                case compileProgram parsedProg of
                  Left err -> putStrLn $ err
                  Right bs -> 
                    if check' then do
                      putStrLn "Type Check"
                      putStrLn $ unlines $ checkBuilder bs
                      writeFile output $ pp bs
                    else writeFile output $ pp bs
            _ -> error "command line parse error. See CommandLine.hs for details"
    Right _ -> error "Argument error, see source code"
