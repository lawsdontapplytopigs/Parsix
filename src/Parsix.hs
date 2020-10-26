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
-- TODO: Write the signature as `newtype Parser a = Parser (String -> Maybe (a, String))`
{- A value that given an input string can parse a type of thing `a` from it
-}
newtype Parser a = Parser (String -> Maybe (String, a))

{- Run the parser on some input

    trueParser = keyword "true"

    run trueParser "true"           == Just ("", "true")
    run trueParser "truer"          == Just ("r", "true")
    run trueParser "tru"            == Nothing
    run trueParser "anything else"  == Nothing
-}
run :: Parser a -> String -> Maybe (String, a)
run (Parser parse) input =
    parse input


{- Parse a single character

    run (symbol 'a') "a" == Just ("", "a")
    run (symbol 'a') "abc" == Just ("bc", "a")
-}

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

{- Parse a keyword

    run (keyword "hey") "hey"           == Just ("", "hey")
    run (keyword "he") "hello"          == Just ("llo", "he")
    run (keyword "hello") "he"          == Nothing
    run (keyword "let") "let ok = 10"   == (Just " ok = 10", "let")
-}
keyword :: String -> Parser String
keyword input = sequenceA $ map symbol input

{- Parse a character, if it passes the test

    run (validChar (\c -> c == 'a')) "abc" == Just ("bc", "a")
    run (validChar (\n -> isDigit n)) "27" == Just ("7", "2")

This is useful if you don't want to create an exhaustive list of characters
to check for, but instead, only check for the characters that you know **shouldn't**
be in the input string

for example, a very simple parser for a string in a language would be that
you want to parse a doublequote, followed by any unicode characters
(except for a doublequote), followed by a doublequote.

    stringParser =
        (pure (\str -> str))
            |. symbol '"'
            |= zeroOrMore (validChar (\ch -> ch /= '"'))
            |. symbol '"'

    run stringParser "\"hello world\"" == Just ("", "hello world")
    run stringParser "\"hello world" == Nothing
    run stringParser "\"\"" == Just ("", "")

we could've also created a list of every unicode character except for '"',
and checked for each one, but doing it like this was
1. much easier,
2. clearer,
3. (probably) more performant (, although I haven't checked)
-}
-- TODO: Rename this to `validSymbol`
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


-- COMBINATORS

{- Parse one of a few parsers

    hexDigits = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
    hexDigitsParser = map oneOf hexDigits

    run hexDigitsParser "af" == Just ('f', 'a')
    run hexDigitsParser "hey" == Nothing
    run hexDigitsParser "123" == Just ('23', '1')

    ---------------

    trueOrFalse =
        oneOf
            [ keyword "true"
            , keyword "false"
            ]

    run trueOrFalse "true" == Just ("", "true")
    run trueOrFalse "false" == Just ("", "false")
    run trueOrFalse "fals" == Nothing
    run trueOrFalse "truer" == Just ("r", "true")
-}
oneOf :: [ Parser a ] -> Parser a
oneOf parsers = foldl (<|>) empty parsers

{- Parse zero or more of a certain parser

    digitsParser = zeroOrMore $ map oneOf ['0'..'9']

    run digitsParser "2020" == Just ("2020", "")
    run digitsParser "123abc" == Just ("123", "abc")
    run digitsParser "" == Just ("", "")
    run digitsParser "abc" == Just ("abc", "")
-}
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore (Parser parse) =
    Parser $ \input ->
        case parse input of
            Just (rest, parsed) ->
                accumulate (Parser parse) [parsed] rest
            Nothing ->
                Just (input, [])

{- Parse one or more of a certain parser

    digitsParser = zeroOrMore $ map oneOf ['0'..'9']

    run digitsParser "2020" == Just ("2020", "")
    run digitsParser "123abc" == Just ("123", "abc")
    run digitsParser "" == Nothing
    run digitsParser "abc" == Nothing
-}
oneOrMore :: Parser a -> Parser [a]
oneOrMore (Parser parse) =
    Parser $ \input ->
        case parse input of
            Just (rest, parsed) ->
                accumulate (Parser parse) [parsed] rest
            Nothing ->
                Nothing

-- (operators inspired by elm)
{- Parse things sequentially

It is sometimes very helpful to think of parsing in terms of
"parse this, **followed by** that, **followed by** that, etc"
however even if you do this, in some cases you want to "keep" the result of
parsing, whereas in other cases you want to ignore it
These operators allow you to do this.

As per the elm docs:
"The theory is that (|=) introduces more “visual noise” than (|.), making it
pretty easy to pick out which lines in the pipeline are important."

For example, to parse a 2d point of the form "( 4, 10 )":

    data Point =
        Point
            { x :: Int
            , y :: Int
            }

    parsePoint =
        (pure Point)
            |. symbol '('
            |. zeroOrMore whitespace
            |= int (oneOrMore digit)
            |. zeroOrMore whitespace
            |. symbol ','
            |. zeroOrMore whitespace
            |= int (oneOrMore digit)
            |. zeroOrMore whitespace
            |. symbol ')'

    digit = fmap oneOf ['0'..'9']
    whitespace = fmap oneOf [' ', '\n', '\t', '\r']
-}
-- TODO: idk how to write this type signature, but if I don't write it, it works
-- (|.) :: Parser (a -> b) -> Parser a -> Parser (a -> b)
(|.) continue throw =
    continue <* throw

(|=) :: Parser (a -> b) -> Parser a -> Parser b
(|=) continue keep =
    continue <*> keep

{- Parse something `andThen` parse something else inside of what you just parsed
-- TODO
-}
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

{- Parse something that is optional

Let's say that you know that if the user specifies a size without a unit after it,
by default it's in pixels. Disregarding whether this would be best practice or
not, this is how you'd do something like this:

    data Unit
        = Px
        | Em

    data Size =
        Size
            { number :: Integer
            , unit :: Maybe Unit
            }

    sizeParser =
        (pure Size)
            |= oneOrMore digit
            |= optional unitParser

    unitParser =
        oneOf
            [ keyword "px"
            , keyword "em"
            ]
-}
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

