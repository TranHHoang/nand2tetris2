{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module HDL.Parser.Ast (
  Pin (..),
  ConnSide (..),
  ConnValue (..),
  Conn (..),
  Part (..),
  Chip (..),
  Body (..),
  ClockedPins (..),
  SrcPos,
)
where

import Data.Text (Text)
import Text.Megaparsec qualified as MP

type SrcPos = MP.SourcePos

data Pin
  = Bit {name :: Text, srcPos :: SrcPos}
  | Bus {name :: Text, width :: Int, srcPos :: SrcPos}
  deriving (Show)

data ConnSide
  = Id {name :: Text, srcPos :: SrcPos}
  | Index {name :: Text, index :: Int, srcPos :: SrcPos}
  | Range {name :: Text, begin :: Int, end :: Int, srcPos :: SrcPos}
  deriving (Show)

instance Eq ConnSide where
  (==) (Index _ idx _) (Index _ idx' _) = idx == idx'
  (==) (Range _ a b _) (Range _ a' b' _) = a == a' && b == b'
  (==) (Id{}) (Id{}) = True
  (==) _ _ = False

data ConnValue
  = Side ConnSide
  | ConstTrue SrcPos
  | ConstFalse SrcPos
  deriving (Show)

data Conn = Conn {target :: ConnSide, value :: ConnValue}
  deriving (Show)

data Part = Part {name :: Text, conns :: [Conn], srcPos :: SrcPos}
  deriving (Show)

newtype ClockedPins = ClockedPins (Maybe [Text]) deriving (Show)

data Body
  = Parts [Part]
  | BuiltIn {name :: Text, clockedPins :: ClockedPins, srcPos :: SrcPos}
  deriving (Show)

data Chip = Chip
  { name :: Text
  , inPins :: [Pin]
  , outPins :: [Pin]
  , internalPins :: [Pin]
  , body :: Body
  , srcPos :: SrcPos
  }
  deriving (Show)
