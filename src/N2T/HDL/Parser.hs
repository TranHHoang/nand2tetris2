{-# LANGUAGE QuasiQuotes #-}

module N2T.HDL.Parser (test, loadChip) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import N2T.HDL.Parser.Types (Body (BuiltIn, Parts), Chip (..), Conn (..), ConnSide (..), Part (..), Pin (..))
import PyF (fmt)
import Text.Megaparsec (MonadParsec (try), Parsec, between, choice, errorBundlePretty, many, (<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char (char, digitChar, letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

skipComment :: Parser ()
skipComment = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol skipComment

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipComment

identifier :: Parser Text
identifier = T.pack <$> lexeme (liftA2 (:) (letterChar <|> char '_') (many $ choice [letterChar, digitChar, char '_']))

number :: Parser Int
number = lexeme L.decimal

someLst :: Parser a -> Parser [a]
someLst fa = liftA2 (:) fa (many $ symbol "," *> fa)

-- Declaration: a, b[10]
pinDecl :: Parser Pin
pinDecl = do
  name <- identifier
  (Bus name <$> brackets number) <|> pure (Single name)

-- Connection: a=b, a[1]=b[1], a[1..2]=true
conn :: Parser Conn
conn = liftA2 Conn connSide (symbol "=" *> connSide)
  where
    connSide = do
      name <- identifier
      choice
        [ try $ brackets (liftA2 (Range name) number (symbol ".." *> number)),
          try $ Index name <$> brackets number,
          pure $ Name name
        ]

-- chipPart(conn, conn...)
part :: Parser Part
part = liftA2 Part identifier $ parens (someLst conn)

chip :: Parser Chip
chip = do
  void skipComment
  void $ symbol "CHIP"
  chipName <- identifier
  braces $ do
    inPins <- pinDecls "IN"
    outPins <- pinDecls "OUT"
    body <-
      choice
        [ Parts <$> (symbol "PARTS:" *> many (part <* symbol ";")),
          BuiltIn chipName
            <$> ( symbol "BUILTIN"
                    *> symbol chipName
                    *> symbol ";"
                    *> ((symbol "CLOCKED" *> (Just <$> (someLst identifier <|> pure [])) <* symbol ";") <|> pure Nothing)
                )
        ]
    return $ Chip chipName inPins outPins body
  where
    pinDecls t = symbol t *> someLst pinDecl <* symbol ";"

parse :: Text -> Text -> IO Chip
parse name src = case P.parse chip (T.unpack name) src of
  Right c -> pure c
  Left e -> fail (errorBundlePretty e)

loadChip :: Text -> IO Chip
loadChip name = readFile [fmt|test/hdl/{name}|] >>= parse name . T.pack

test :: IO ()
test = loadChip "test/hdl/Add16.hdl" >>= print
