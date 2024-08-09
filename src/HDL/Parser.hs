module HDL.Parser (parseFolder) where

import Control.Applicative ((<|>))
import Control.Monad (void, (>=>))
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import HDL.Parser.Ast (
  Body (..),
  Chip (..),
  ClockedPins (ClockedPins),
  Conn (..),
  ConnSide (..),
  ConnValue (..),
  Part (Part),
  Pin (..),
 )
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import Text.Megaparsec (MonadParsec (eof), Parsec, between, choice, errorBundlePretty, many, sepBy, try)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

nameP :: Parser Text
nameP = T.pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

number :: Parser Int
number = lexeme L.decimal

comma :: Parser Text
comma = symbol ","

semi :: Parser Text
semi = symbol ";"

-- IN a, b[10]
pinDeclP :: Parser Pin
pinDeclP = do
  pos <- MP.getSourcePos
  name <- nameP
  (Bus name <$> brackets number <*> pure pos) <|> pure (Bit name pos)

-- a=b, a[1]=b[1], a[1..2]=true
connP :: Parser Conn
connP = Conn <$> sideP <*> (symbol "=" *> valueP)
 where
  sideP = do
    pos <- MP.getSourcePos
    name <- nameP
    choice
      [ try . brackets $ Range name <$> number <*> (symbol ".." *> number)
      , try $ Index name <$> brackets number
      , pure $ Id name
      ]
      <*> pure pos
  valueP =
    choice
      [ try $ MP.getSourcePos >>= (string "true" $>) . ConstTrue
      , try $ MP.getSourcePos >>= (string "false" $>) . ConstFalse
      , Side <$> sideP
      ]

-- chipPart(conn, conn...)
partP :: Parser Part
partP = do
  pos <- MP.getSourcePos
  Part <$> nameP <*> parens (connP `sepBy` comma) <*> pure pos

-- /** API documentation: what the chip does. */
-- CHIP ChipName {
--   IN inputPin1, inputPin2, ... ;
--   OUT outputPin1, outputPin2, ... ;
--   PARTS:
--   // Here comes the implementation.
-- }
chipP :: Parser Chip
chipP = between spaceConsumer eof $ do
  void $ string "CHIP"
  pos <- MP.getSourcePos
  name <- nameP
  braces $ Chip name <$> pinP "IN" <*> pinP "OUT" <*> pure [] <*> (partsP <|> builtInP name) <*> pure pos
 where
  pinP tok = string tok *> pinDeclP `sepBy` comma
  partsP = Parts <$> (string "PARTS:" *> partP `sepBy` semi)
  builtInP name = do
    void $ string "BUILTIN"
    pos <- MP.getSourcePos
    BuiltIn name <$> (string name *> semi *> clockedP) <*> pure pos
  clockedP =
    ClockedPins
      <$> choice
        [ string "CLOCKED" *> (Just <$> choice [nameP `sepBy` comma, pure []]) <* semi
        , pure Nothing
        ]

--
parse :: Text -> IO Chip
parse src = case MP.parse chipP "" src of
  Right c -> pure c
  Left e -> fail (errorBundlePretty e)

parseFolder :: String -> IO (Map Text Chip)
parseFolder folder = chipMap
 where
  readText = fmap T.pack . readFile
  chips = listDirectory folder >>= traverse (readText >=> parse) . filter ((== ".hdl") . takeExtension)
  chipMap = foldr (\c acc -> M.insert c.name c acc) M.empty <$> chips