module CsgoGuideParser where

import Text.Parsec
import Text.Parsec.Char (char, satisfy)
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad

sampleString :: String
sampleString =
  unlines [ "#http://i.imgur.com/BXtCRxZ.jpg"
          , "#public/cache/cache_ct_boost_nade.jpg"
          , "molly vents from long A"
          , "[molotov,vent,long a]"
          , ""
          , "http://i.imgur.com/D1nhbTH.jpg"
          , "smoke and flash from squeaky"
          , "[smoke,flash,squeaky,quad,fence,A]"
          ]

sampleTags :: String
sampleTags = "[molotov,vent,long a]"

data NadeInfo = NadeInfo { _nadeImg :: [String]
                         , _nadeDescription :: String
                         , _nadeTags :: [String]
                         } deriving (Show, Eq)

parseLine :: Parser String
parseLine = manyTill anyChar (try endOfLine)

parseImg :: Parser String
parseImg = char '#' *> parseLine

nadeInfo :: Parser NadeInfo
nadeInfo = do
  img <- many1 parseImg
  (ds, ts) <- manyTill' parseLine (try $ parseTags <* endOfLine)
  return $ NadeInfo img (unlines ds) ts

parseTag :: Parser String
parseTag = many $ noneOf ",]"

parseTags :: Parser [String]
parseTags = inBrackets $ commaSep parseTag

inBrackets :: Parser a -> Parser a
inBrackets p = between (char '[') (char ']') p

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (char ',')

parsei :: Parser a -> String -> Either ParseError a
parsei p = parse p ""

manyTill' :: Parser a -> Parser b -> Parser ([a], b)
manyTill' p end = scan
  where scan = (f <$> end) <|> (g <$> p <*> scan)
        f x = ([], x)
        g y (ys, x)= (y:ys, x)

emptyLine :: Parser ()
emptyLine = void endOfLine

nadeInfos :: Parser [NadeInfo]
nadeInfos = many $ nadeInfo <* many emptyLine
