{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module N2T.HDL.Parser.Types (Pin (..), ConnSide (..), Conn (..), Part (..), Body (..), Chip (..)) where

import Data.Text (Text)
import GHC.Generics (Generic)

data Pin
  = Single {name :: Text}
  | Bus {name :: Text, count :: Int}
  deriving (Show, Generic)

data ConnSide
  = Name {name :: Text}
  | Index {name :: Text, index :: Int}
  | Range {name :: Text, from :: Int, to :: Int}
  deriving (Show, Generic)

data Conn = Conn {target :: ConnSide, value :: ConnSide}
  deriving (Show, Generic)

data Part = Part {name :: Text, conns :: [Conn]}
  deriving (Show, Generic)

data Body
  = Parts [Part]
  | BuiltIn Text (Maybe [Text])
  deriving (Show)

data Chip = Chip
  { name :: Text,
    inPins :: [Pin],
    outPins :: [Pin],
    body :: Body
  }
  deriving (Show, Generic)