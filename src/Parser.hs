{-#LANGUAGE NoImplicitPrelude#-}

module Parser (
  Token (..),
  Program,
  parseProgram,
  runParser
  ) where

import Prelude (Show)
import Data.Function (($))
import Control.Monad (
  Monad(..),
  void)
import Text.Parsec (
  runParser, char, sepEndBy, many, noneOf,
  (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (optional)

data Token = Increment
           | Decrement
           | PtrInc
           | PtrDec
           | PutChar
           | GetChar
           | Loop Program
             deriving (Show)

type Program = [Token]

parseToken :: Parser Token
parseToken =
  (char '+' >> return Increment) <|>
  (char '-' >> return Decrement) <|>
  (char '>' >> return PtrInc) <|>
  (char '<' >> return PtrDec) <|>
  (char '.' >> return PutChar) <|>
  (char ',' >> return GetChar) <|>
  parseLoop
  
parseLoop :: Parser Token
parseLoop = do
  _ <- char '['
  prog <- parseProgram
  _ <- char ']'
  return $ Loop prog

parseComment :: Parser ()
parseComment = void $ many (noneOf "+-<>.,[]")

parseProgram :: Parser Program
parseProgram = do
  optional parseComment
  sepEndBy parseToken parseComment
