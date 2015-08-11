module Parser where

import Prelude

import Types

import Data.Foldable
import Data.String
import Data.Either
import Data.Maybe
import Data.List ((:))
import qualified Data.Char as C

import Control.Alt
import Control.Apply

import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String

-- Stolen from purescript-parsing
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = do
  c <- anyChar
  if f c then return c
         else fail "Character did not satisfy predicate"

oneOf :: Array Char -> Parser Char
oneOf = satisfy <<< flip elem

char :: Char -> Parser Char
char c = satisfy (== c)

spaces :: Parser _
spaces = many $ try $ oneOf $ toCharArray " \n\r\t"

-- specific IRC stuff
letter :: Parser Char
letter = oneOf $ toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

number :: Parser String
number = foldMap C.toString <$> many1 anyDigit

symbol :: Parser Char
symbol = oneOf $ toCharArray "@%+_\\[]{}^`|"

date :: Parser String
date = foldr (++) "" <$> sepBy1 number (try $ char '-')

time :: Parser String
time = foldr (++) "" <$> sepBy1 number (try $ char ':')

name :: Parser String
name = do
  first <- try letter <|> try symbol
  rest <- many (try letter <|> try anyDigit <|> try symbol <|> try (char '-'))
  return $ foldMap C.toString $ first : rest

message :: Parser String
message = foldMap C.toString <$> many anyChar

testParser :: forall a. (Show a) => String -> Parser a -> String
testParser input parser = case runParser parser input of
                               Left (ParseError err) -> show err
                               Right val -> show val

parseLine :: Parser IRCMessage
parseLine = do
  date *> spaces *> time *> spaces
  nick <- name
  spaces
  msg <- message
  return { name: nick, message: msg }

readLog :: forall a. Parser a -> String -> Maybe a
readLog parser input = either (const Nothing) return $ runParser parser input

readLine = readLog parseLine
readFile = readLog $ endBy parseLine spaces

parsableWith :: (IRCMessage -> Boolean) -> String -> Boolean
parsableWith pred = either (const false) pred <<< runParser parseLine

parsable :: String -> Boolean
parsable line = parsableWith (const true) line

showme :: String -> String
showme input = case readLine input of
                    Just a -> a.name ++ ": " ++ a.message
                    Nothing -> ""
