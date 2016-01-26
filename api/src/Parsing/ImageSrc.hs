module Parsing.ImageSrc where

import Control.Monad
import Data.Either
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (char, alphaNum, letter, string)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))

parseImgurPrefix :: Parser ()
parseImgurPrefix = void $ string "http://i.imgur.com/"

parseImgurId :: Parser ()
parseImgurId = void $ many1 alphaNum

parseImgurSuffix :: Parser ()
parseImgurSuffix = void $ char '.' *> many1 letter

parseImgurSrc :: Parser ()
parseImgurSrc = parseImgurPrefix *> parseImgurId *> optional parseImgurSuffix *> eof

checkImgurSrc :: Text -> Either ParseError ()
checkImgurSrc = runParser parseImgurSrc () ""

isImgurSrc :: Text -> Bool
isImgurSrc = isRight . checkImgurSrc
