{-# LANGUAGE InstanceSigs #-}
module Parsix
    ( Parser
    , run
    , symbol
    , keyword
    , oneOf
    , oneOrMore
    , zeroOrMore
    , validChar
    , (|.)
    , (|=)
    , readInt
    , readFloat
    , readDouble
    , optional
    ) where

import Control.Applicative (Alternative, (<|>), empty)

-- TODO: Support error reporting
newtype Parser a = Parser (String -> Maybe (String, a))

run :: Parser a -> String -> Maybe (String, a)
run (Parser parse) input =
    parse input

symbol :: Char -> Parser Char
symbol c =
    let
        check input =
            case input of
                [] ->
                    Nothing
                a : as ->
                    if a == c then
                        Just (as, a)
                    else
                        Nothing
    in
        Parser check

keyword :: String -> Parser String
keyword input = sequenceA $ map symbol input

validChar :: (Char -> Bool) -> Parser Char
validChar isValid =
    let
        check input =
            case input of
                [] ->
                    Nothing
                a : as ->
                    if isValid a then
                        Just (as, a)
                    else
                        Nothing
    in
        Parser check

                Nothing

-- operators inspired by elm

-- TODO
-- idk how to write this type signature, but if I don't write it, it works
-- (|.) :: Parser (a -> b) -> Parser a -> Parser (a -> b) 
(|.) continue throw =
    continue <* throw

(|=) :: Parser (a -> b) -> Parser a -> Parser b
(|=) continue keep =
    continue <*> keep

oneOf :: [ Parser a ] -> Parser a
oneOf parsers = foldl (<|>) empty parsers

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore (Parser parse) =
    Parser $ \input ->
        case parse input of
            Just (rest, parsed) ->
                accumulate (Parser parse) [parsed] rest
            Nothing ->
                Just (input, [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore (Parser parse) =
    Parser $ \input ->
        case parse input of
            Just (rest, parsed) ->
                accumulate (Parser parse) [parsed] rest
            Nothing ->

andThen :: (a -> Parser b) -> Parser a -> Parser b
andThen func (Parser run) =
    let
        check input =
            case run input of
                Just (rest1, parsed) ->
                    case func parsed of
                        Parser toRun ->
                            toRun rest1
                Nothing ->
                    Nothing
    in
        Parser check

optional :: Parser a -> Parser (Maybe a)
optional (Parser parse) =
    let
        check input =
            case parse input of
                Just (rest, parsed) ->
                    Just (rest, Just parsed)
                Nothing ->
                    Just (input, Nothing)
    in
        Parser check

-- only :: Parser a -> Parser a
-- only (Parser parse) =
--     let
--         check input =
--             case parse input of
--                 Nothing ->
--                     Nothing
--                 Just (something, parsed) ->
--                     if null something then
--                         Just (something, parsed)
--                     else
--                         Nothing
--     in
--     Parser check


readInt input = read input :: Integer
readFloat input = read input :: Float
readDouble input = read input :: Double

int :: Parser String -> Parser Integer
int parser = fmap readInt parser

float :: Parser String -> Parser Float
float parser = fmap readFloat parser

double :: Parser String -> Parser Double
double input = fmap readDouble input


accumulate (Parser parse) acc input =
    case parse input of
        Nothing -> Just (input, acc)
        Just (rest, parsed) -> accumulate (Parser parse) (acc ++ [parsed]) rest


instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap aToB (Parser parse) =
        let
            check input = do
                (rest, parsed) <- parse input
                Just (rest, aToB parsed)
        in
            Parser check

instance Applicative Parser where
    pure :: a -> Parser a
    pure value = Parser $ \input -> Just (input, value)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser first) <*> (Parser second) =
        let
            check input = do
                (rest1, aToB) <- first input
                (rest2, a) <- second rest1
                Just (rest2, aToB a)
        in
            Parser check

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \input -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser first) <|> (Parser second) =
        -- we can do this since Maybe is an alternative too
        Parser $ \input -> (first input) <|> (second input)

