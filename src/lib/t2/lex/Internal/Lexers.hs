
module Internal.Lexers (
    lexTokens
) where

import Internal.Lexer (Lexer, Token)
import qualified Internal.Lexer as Tok

import qualified Data.Text as Text
import Data.Text (Text)

import Data.Char (isDigit)
import Control.Applicative ((<|>))

lexTokens :: Text -> [Either Text Token]
lexTokens "" = [Right Tok.Eof]
lexTokens text =
    case lexToken text of
        Just (token, rest) -> Right token : (lexTokens . stripSpace) rest
        Nothing            -> [Left text]
    where
        stripSpace = Text.dropWhile (flip elem [' ', '\r', '\n', '\t', '\v'])

lexToken :: Lexer
lexToken text =
    lexBinop text <|>
    lexInteger text <|>
    lexParen text <|>
    lexNewline text

lexBinop :: Lexer
lexBinop text
    | elem firstChar [ "+", "-", "*", "/", "^" ] = Just (Tok.Binop firstChar, rest)
    | otherwise                             = Nothing
    where
        (firstChar, rest) = Text.splitAt 1 text

lexInteger :: Lexer
lexInteger text
    | (not . Text.null) integer = Just (Tok.Integer integer, rest)
    | otherwise                 = Nothing
    where
        (integer, rest) = Text.span isDigit text

lexParen :: Lexer
lexParen text
    | firstChar == "(" = Just (Tok.Paren Tok.Left, rest)
    | firstChar == ")" = Just (Tok.Paren Tok.Right, rest)
    | otherwise        = Nothing
    where
        (firstChar, rest) = Text.splitAt 1 text

lexNewline :: Lexer
lexNewline text
    | (not . Text.null) nls = Just (Tok.Newline, rest)
    | otherwise             = Nothing
    where
        (nls, rest) = Text.span (=='\n') text
