-- | Experiment in generating combinator functions with Template Haskell
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
module TH.Pushable where
import qualified Prelude as Prelude
import Prelude.Linear as Linear
import Unsafe.Linear
import GHC.Types
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Syntax as Syntax
import Control.Monad
import Data.Functor
import Data.Traversable
import Data.Foldable

import Linear.Box

derivePushable :: Name -> Q [Dec]
derivePushable tyConName = do
  info <- reify tyConName
  case info of
    TyConI{}   -> derivePlainTy tyConName
    DataConI{} -> dataConIError
    _          -> error (ns ++ "The name must be of a plain type constructor.")
  where
    ns :: String
    ns = "TH.Pushable.derivePushable: "

dataConIError :: a
dataConIError = error "Cannot use a data constructor."

derivePlainTy :: Name -> Q [Dec]
derivePlainTy tyConName = do
  TyConI (DataD _ctx n _bnds _knd cons _dervs) <- reify tyConName
  pushClauses <- mapM (mkClause n) cons
  [d| instance Pushable $(conT tyConName) where
       push = $(return (LamCaseE pushClauses)) |]

mkClause :: Name -> Con -> Q Match
mkClause n c =
  case c of
    NormalC name bTypes -> do
      nams <- forM bTypes (\_ -> newName "match")
      exps <- mapM (distTyField n) (Prelude.zip (Prelude.map snd bTypes) nams)
      let pat = map VarP nams
      let bod = NormalB $ Linear.foldl' AppE (ConE name) exps
      return (Match (ConP 'Box [(ConP name pat)]) (bod) [])
    ForallC _vars _ctx con -> mkClause n con
    GadtC [nam] bTypes (AppT (ConT _) _) ->
      mkClause n (NormalC nam bTypes)
    _ -> error ("mkClause: unhandled " ++ show c)

distTyField :: Name -> (Syntax.Type, Name) -> Q Exp
distTyField n (ty,ename) =
  case ty of
    (AppT (ConT cname) _vname) ->
      if cname Prelude.== n
      then [| push (Box $(varE ename)) |]
      else [| $(varE ename) |]
    ConT _cname -> [| $(varE ename) |]
    VarT _vname -> [| Box $(varE ename) |]
    _ -> error ("distTyField: unhandled case: " ++ show ty)
