module N2T.HDL.Codegen where

import Data.Text qualified as T
import LLVM.AST (mkName)
import LLVM.AST.Type qualified as AST
import LLVM.IRBuilder qualified as IR
import N2T.HDL.Parser.Types (Chip (..), Pin (..))
import Optics.Core ((^.))

genChip :: Chip -> IR.IRBuilderT IR.ModuleBuilder ()
genChip chip = undefined
  where
    name = mkName . T.unpack $ chip ^. #name
    params =
      ( \case
          (Single name) -> (AST.i1, name)
          (Bus name _) -> (AST.ptr, name)
      )
        <$> chip ^. #inPins