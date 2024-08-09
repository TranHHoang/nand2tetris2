{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module N2T.HDL.Analyzer () where

import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, liftIO, runExceptT)
import Control.Monad.State (MonadState (get, put), StateT, execStateT, gets, modify)
import Data.Foldable (for_)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Traversable (for)
import GHC.Generics (Generic)
import N2T.HDL.Parser (loadChip)
import N2T.HDL.Parser.Types (Body (..), Chip (..), Conn (..), ConnSide (..), Part (..), Pin (..))
import Optics.Core (view, (%), (^.))
import PyF (fmt)

data PinScope
  = Input
  | Output
  | Internal
  deriving (Eq, Show)

data PinData = PinData
  { scope :: PinScope,
    pin :: Pin
  }
  deriving (Show, Generic)

type SymbolTable = Map Text PinData

type Env = ExceptT Text (StateT SymbolTable IO)

note :: Text -> Maybe a -> Env a
note e Nothing = throwError e
note _ (Just a) = pure a

isSameType :: Pin -> Pin -> Bool
isSameType = \cases
  (Single _) (Single _) -> True
  (Bus _ bc) (Bus _ bc') -> bc == bc'
  _ _ -> False

analyze :: Chip -> Env ()
analyze chip = do
  put =<< liftEither (foldr tryInsert (pure M.empty) $ (PinData Input <$> chip ^. #inPins) ++ (PinData Output <$> chip ^. #outPins))
  symbolTable <- get
  _ <- case chip ^. #body of
    BuiltIn name _ | name `notElem` ["Nand", "DFF"] -> throwError "Only 'Nand' and 'DFF' chips are built-in"
    BuiltIn _ (Just clockedPins) | any (`M.notMember` symbolTable) clockedPins -> throwError "Only input and output pins can be clocked"
    BuiltIn _ _ -> pure ()
    Parts parts ->
      for_ parts $ \(Part chipPartName conns) -> do
        chipPart <- liftIO . loadChip $ chipPartName
        partInOutTable <- liftIO $ M.filter ((`elem` [Input, Output]) . view #scope) <$> execStateT (runExceptT $ analyze chipPart) M.empty
        for_ conns $ \(Conn target value) -> do
          lhs <- note [fmt|'{target ^. #name}' is not an in/out pin of chip '{chipPartName}'|] $ M.lookup (target ^. #name) partInOutTable
          symbolTable <- get
          case M.lookup (value ^. #name) symbolTable of
            Nothing -> modify $ M.insert (value ^. #name) (PinData Internal $ lhs ^. #pin)
            Just rhs ->
              case (target, value) of
                (Name _, Name _)
                  | isSameType (lhs ^. #pin) (rhs ^. #pin) -> pure ()
                  | otherwise -> throwError [fmt|Incompatible type between pin '{target ^. #name}' (chip '{chipPartName}') '' and pin '{value ^. #name}' (chip '{chip ^. #name}')|]
                (_, Index _ index) ->
                  case rhs ^. #pin of
                    Single _ -> throwError ""
                    Bus _ bc
                      | index < 0 || index >= bc -> throwError ""
                      | isSameType (lhs ^. #pin) (Single "") -> pure ()
                      | otherwise -> throwError ""
                (_, Range _ from to) ->
                  case rhs ^. #pin of
                    Single _ -> throwError ""
                    Bus _ bc
                      | from < 0 || from >= bc || to < 0 || to >= bc || from > to -> throwError ""
                      | isSameType (lhs ^. #pin) (Bus "" $ to - from + 1) -> pure ()
                      | otherwise -> throwError ""
      where
        checkType = \cases
          (Name a) (Name b)
            | isSameType (lhs ^. #pin) (rhs ^. #pin) -> pure ()
            | otherwise -> throwError [fmt|Incompatible type between pin '{a}' (chip '{chipPartName}') and pin '{value ^. #name}' (chip '{chip ^. #name}')|]
          (_, Index _ index) ->
            case rhs ^. #pin of
              Single _ -> throwError ""
              Bus _ bc
                | index < 0 || index >= bc -> throwError ""
                | isSameType (lhs ^. #pin) (Single "") -> pure ()
                | otherwise -> throwError ""
          (_, Range _ from to) ->
            case rhs ^. #pin of
              Single _ -> throwError ""
              Bus _ bc
                | from < 0 || from >= bc || to < 0 || to >= bc || from > to -> throwError ""
                | isSameType (lhs ^. #pin) (Bus "" $ to - from + 1) -> pure ()
                | otherwise -> throwError ""

  -- undefined
  -- _ -> undefined
  -- Conn target _
  --   | (target ^. #name) `M.notMember` partInOutTable -> throwError [fmt|'{target ^. #name}' is not an in/out pin of chip '{partName}'|]

  -- do
  -- modify
  --   ( M.insert (value ^. #name) $
  --       PinData
  --         Internal
  --         ( case target of
  --             Range {name, from, to} -> Bus (value ^. #name) (to - from)
  --             _ -> Single (value ^. #name)
  --         )
  --   )
  -- case (target, value) of
  --   (Name _, Name vname) -> modify $ M.insert vname (PinData Internal $ Single vname)
  --   -- (Name _, Index iname) -> modify $ M.insert iname (PinData Internal $ Single iname)
  --   _ -> undefined
  -- undefined
  undefined
  where
    tryInsert :: PinData -> Either Text SymbolTable -> Either Text SymbolTable
    tryInsert d m = do
      m' <- m
      let name = d ^. #pin % #name
      case M.lookup name m' of
        Just _ -> throwError [fmt|Duplicated pin '{name}'|]
        Nothing -> M.insert name d <$> m