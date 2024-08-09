module HDL.Analyzer.Types (PinScope (..), PinType (..), PinData (..), SemanticError (..)) where

import Data.Text (Text)
import HDL.Parser.Ast (ConnValue (..), SrcPos)

data PinScope = In | Out | Internal deriving (Show, Eq)

data PinType = TyBit | TyBus Int deriving (Show, Eq)

data PinData = PinData
  { scope :: PinScope
  , type' :: PinType
  , srcPos :: SrcPos
  }
  deriving (Show)

data SemanticError
  = BusWidthTooLarge SrcPos Int
  | InvalidBuiltInChip SrcPos Text
  | InvalidClockedPin SrcPos Text
  | ChipPartNotFound SrcPos Text
  | PinNameNotFound SrcPos Text
  | IncompatibleType PinType PinType
  | InvalidSubscript SrcPos Text
  | InvalidBinding SrcPos PinType PinType
  | InvalidBindingConst SrcPos PinType ConnValue
  | OutOfBound SrcPos Int (Int, Int)
  | InvalidFeedbackLoop SrcPos
  deriving (Show)