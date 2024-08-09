module HDL.Analyzer (check) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, liftIO, throwError, (>=>))
import Control.Monad.State (
  StateT,
  gets,
 )
import Data.Foldable (find, for_, traverse_)
import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import HDL.Analyzer.Types (
  PinData (..),
  PinScope (..),
  PinType (..),
  SemanticError (..),
 )
import HDL.Parser.Ast (
  Body (..),
  Chip (..),
  ClockedPins (..),
  Conn (..),
  ConnSide (..),
  ConnValue (..),
  Part (..),
  Pin (..),
 )
import Optics.State.Operators ((%=), (.=), (<.=))

type SymTable = Map Text PinData

data Env = Env
  { symTable :: SymTable
  , chipMap :: Map Text Chip
  }
  deriving (Show, Generic)

type Env' = ExceptT SemanticError (StateT Env IO)

toPinTy :: Pin -> PinType
toPinTy = \case
  Bit{} -> TyBit
  Bus{..} -> TyBus width

note :: SemanticError -> Maybe a -> Env' a
note = \cases
  e Nothing -> throwError e
  _ (Just a) -> pure a

checkSide :: ConnSide -> PinType -> Env' PinType
checkSide = \cases
  Index{..} TyBit -> throwError $ InvalidSubscript srcPos name
  Range{..} TyBit -> throwError $ InvalidSubscript srcPos name
  Index{..} (TyBus w) | index < 0 || index >= w -> throwError $ OutOfBound srcPos index (0, w)
  Range{..} (TyBus w)
    | begin < 0 || end < 0 || begin >= end || begin >= w || end >= w -> throwError $ OutOfBound srcPos w (begin, end)
  Id{} ty -> pure ty
  Index{} _ -> pure TyBit
  Range{..} _ -> pure $ TyBus (end - begin + 1)

recurCheck :: ([Conn] -> Conn -> Bool) -> Part -> Env' ()
recurCheck fn part = do
  (inPins, chip) <- filterInPins
  for_ inPins (checkSeq chip . (.target))
 where
  filterInPins :: Env' ([Conn], Chip)
  filterInPins = do
    chip <- note (ChipPartNotFound part.srcPos part.name) =<< gets (M.lookup part.name . (.chipMap))
    let (inConns, outConns) = partition (\x -> any ((== x.target.name) . (.name)) chip.inPins) part.conns
    pure (filter (fn outConns) inConns, chip)

checkSeq :: Chip -> ConnSide -> Env' ()
checkSeq = \cases
  Chip{body = Parts parts} connSide -> for_ parts (recurCheck (\_ -> eq connSide.name . (.value)))
  Chip{body = BuiltIn{clockedPins = ClockedPins (Just pins)}} side | side.name `elem` pins -> pure ()
  _ side -> throwError $ InvalidFeedbackLoop side.srcPos
 where
  eq :: Text -> ConnValue -> Bool
  eq = \cases
    name (Side side) -> name == side.name
    _ _ -> False

-- Not a loop: (in=a[1], out=a[2]), (in=a[1], out=a[2..3]), (in=a[1..2], out=a[3]), (in=a[1..2], out=a[3..4])
checkLoop :: Part -> Env' ()
checkLoop = recurCheck (\outConns -> (outConns `hasLoop`) . (.value))
 where
  hasLoop :: [Conn] -> ConnValue -> Bool
  hasLoop cs = \case
    Side side -> not $ all (eq side . (.value)) cs
    _ -> False

  eq = \cases
    Index{index} (Side (Index{index = index'})) -> index == index'
    Index{index} (Side (Range{begin})) -> index < begin
    Range{end} (Side (Index{index})) -> index > end
    Range{end} (Side (Range{end = end'})) -> end < end'
    -- The rest has already been handled in checkPart
    _ _ -> False

checkPart :: Part -> Env' ()
checkPart part = do
  chipPart <- note (ChipPartNotFound part.srcPos part.name) =<< gets (M.lookup part.name . (.chipMap))
  let partInOuts = chipPart.inPins ++ chipPart.outPins
  -- Target pins belong to the chipPart
  let targets = (.target) <$> part.conns
  targetExpectTypes <-
    fmap toPinTy
      <$> traverse (\x -> note (PinNameNotFound x.srcPos x.name) $ find ((== x.name) . (.name)) partInOuts) targets
  targetTypes <- sequenceA $ checkSide <$> targets <*> targetExpectTypes

  let values = (.value) <$> part.conns
  for_ (zip targetTypes values) $ \case
    (TyBit, c@(ConstTrue pos)) -> throwError $ InvalidBindingConst pos TyBit c
    (TyBit, c@(ConstFalse pos)) -> throwError $ InvalidBindingConst pos TyBit c
    (ty, Side side) -> do
      valueTy <- gets $ (fmap (.type')) . M.lookup side.name . (.symTable)
      maybe
        (#symTable %= M.insert side.name (PinData Internal ty side.srcPos))
        (\ty' -> checkSide side ty' *> when (ty /= ty') (throwError $ InvalidBinding side.srcPos ty ty'))
        valueTy
    _ -> pure ()
  checkLoop part

checkBody :: Body -> Env' ()
checkBody = \case
  BuiltIn{..} | name `notElem` ["Nand", "DFF"] -> throwError $ InvalidBuiltInChip srcPos name
  BuiltIn{clockedPins = ClockedPins (Just pins), srcPos} -> do
    symTable <- gets (.symTable)
    maybe
      (pure ())
      (throwError . InvalidClockedPin srcPos)
      $ find (`M.notMember` M.filter (\dat -> dat.scope `elem` [In, Out]) symTable) pins
  Parts parts -> for_ parts checkPart
  _ -> pure ()

checkPins :: Pin -> Env' ()
checkPins = \case
  Bus{..} | width > 16 -> throwError $ BusWidthTooLarge srcPos width
  _ -> pure ()

checkChip :: Chip -> Env' Chip
checkChip chip = do
  traverse_ checkPins chip.inPins
  traverse_ checkPins chip.outPins

  #symTable .= initSymTable
  checkBody chip.body
  internalPins <- gets $ fmap toPin . M.toList . M.filter ((== Internal) . (.scope)) . (.symTable)
  pure chip{internalPins}
 where
  tuples fn scope = (\x -> (x.name, PinData scope (toPinTy x) x.srcPos)) <$> fn chip
  initSymTable = M.fromList $ tuples (.inPins) In ++ tuples (.outPins) Out
  toPin (name, PinData{..}) = case type' of
    TyBit -> Bit name srcPos
    TyBus w -> Bus name w srcPos

check :: IO (Map Text Chip) -> Env' (Map Text Chip)
check = liftIO >=> (#chipMap <.=) >=> traverse checkChip