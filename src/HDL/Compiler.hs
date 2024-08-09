{-# LANGUAGE TupleSections #-}

module HDL.Compiler () where

import Control.Monad (join)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (State, gets, void)
import Data.Bits (complement, shiftL, (.|.))
import Data.Foldable (for_)
import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as M
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import HDL.Parser.Ast (Chip (..), Conn (..), ConnSide (..), ConnValue (..), Part (..), Pin (..))
import LLVM.AST (Operand)
import LLVM.AST qualified as AST
import LLVM.AST.Type qualified as Type
import LLVM.AST.Typed qualified as Typed
import LLVM.IRBuilder qualified as IR
import LLVM.Prelude (ShortByteString)
import Optics.State.Operators ((%=))

data Env = Env
  { symTable :: Map Text Operand
  , chipSymTable :: Map Text Operand
  , chipTable :: Map Text Chip
  }
  deriving (Show, Generic)

type ModBuilder = IR.ModuleBuilderT (ExceptT String (State Env))
type IRBuilder = IR.IRBuilderT ModBuilder

sByteStr :: Text -> ShortByteString
sByteStr = fromString . T.unpack

note :: Maybe a -> IRBuilder a
note = \case
  Nothing -> throwError "Error while generating IR"
  (Just a) -> pure a

llvmTy :: Pin -> AST.Type
llvmTy = \case
  Bit{} -> Type.i1
  Bus{width} | width <= 8 -> Type.i8
  _ -> Type.i16

alloc :: AST.Type -> Operand -> IRBuilder Operand
alloc ty val = do
  addr <- IR.alloca ty Nothing 0
  IR.store addr 0 val
  pure addr

genPart :: Part -> IRBuilder Operand
genPart part = do
  chip <- note =<< gets (M.lookup part.name . (.chipTable))
  let chipInOuts = chip.inPins ++ chip.outPins

  -- Allocate a variable for each in/out pin
  partSymTable <-
    sequenceA $ foldr (\v -> M.insert v.name $ alloc (llvmTy v) (IR.int16 0)) M.empty chipInOuts

  let (inConns, outConns) = partition (\x -> any ((== x.target.name) . (.name)) chip.inPins) part.conns

  -- Init in pins
  for_ inConns $ \conn -> do
    addr <- note $ M.lookup conn.target.name partSymTable
    ( case conn of
        Conn Id{} ConstTrue{} -> pure $ IR.int16 (-1)
        Conn Id{} ConstFalse{} -> pure $ IR.int16 0
        Conn Index{index} ConstTrue{} -> modify addr index index (IR.int16 1)
        Conn Index{index} ConstFalse{} -> modify addr index index (IR.int16 0)
        Conn Range{begin, end} ConstTrue{} -> modify addr begin end (IR.int16 $ nBitOne (end - begin + 1))
        Conn Range{begin, end} ConstFalse{} -> modify addr begin end (IR.int16 $ complement (nBitOne (end - begin + 1)))
        Conn lhs (Side rhs) -> do
          v <- note =<< gets (M.lookup rhs.name . (.symTable))
          ty <- liftEither =<< Typed.typeOf v
          -- TODO: Keep the original type
          value <- if ty == Type.ptr then IR.load Type.i16 v 0 else pure v
          case (lhs, rhs) of
            (Id{}, Id{}) -> pure value
            (Id{}, Index{index}) -> valueOfRange value index index
            (Id{}, Range{begin, end}) -> valueOfRange value begin end
            (Index{index}, Id{}) -> modify addr index index value
            (Index{index}, Index{index = index'}) -> valueOfRange value index' index' >>= modify addr index index
            (Range{begin, end}, Id{}) -> modify addr begin end value
            (Range{begin, end}, Range{begin = begin', end = end'}) -> valueOfRange value begin' end' >>= modify addr begin end
            _ -> throwError "Error while generating code"
      )
      >>= IR.store addr 0

  -- Call chipPart
  chipOp <- note =<< gets (M.lookup chip.name . (.chipSymTable))
  args <- note $ traverse ((`M.lookup` partSymTable) . (.name)) chipInOuts
  void
    $ IR.call
      (Type.FunctionType Type.VoidType ((llvmTy <$> chip.inPins) ++ (Type.ptr <$ chip.outPins)) False)
      chipOp
      ((,[]) <$> args)

  -- Init out pins

  undefined
 where
  nBitOne :: Int -> Integer
  nBitOne n = iterate ((.|. 1) . (`shiftL` 1)) 1 !! n

  modify n begin end v = do
    let mask = complement $ nBitOne (end - begin + 1) `shiftL` begin
    join (IR.or <$> IR.and n (IR.int16 mask) <*> IR.shl v (IR.int16 $ toInteger begin))

  valueOfRange v begin end = do
    let mask = nBitOne (end - begin + 1)
    IR.ashr v (IR.int16 $ toInteger begin) >>= IR.and (IR.int16 mask)

genBody :: Chip -> [Operand] -> IRBuilder ()
genBody chip args = do
  entry <- IR.named IR.block "entry"
  -- Gen args
  for_ (zip args (chip.inPins ++ chip.outPins)) $ \(op, pin) -> do
    ty <- liftEither =<< Typed.typeOf op
    alloc ty op >>= (#symTable %=) . M.insert pin.name
  -- Gen internal pins
  for_ chip.internalPins $ \pin -> do
    let ty = llvmTy pin
    alloc ty (IR.int16 0) >>= (#symTable %=) . M.insert pin.name
  -- Parts
  undefined

genChip :: Chip -> ModBuilder Operand
genChip chip = do
  fn <- IR.function name params Type.void (genBody chip)
  #chipSymTable %= M.insert chip.name fn
  pure fn
 where
  name = AST.mkName (T.unpack chip.name)
  params = inPins ++ outPins
  inPins = (\p -> (llvmTy p, IR.ParameterName $ sByteStr p.name)) <$> chip.inPins
  outPins = (\p -> (Type.ptr, IR.ParameterName $ sByteStr p.name)) <$> chip.outPins
