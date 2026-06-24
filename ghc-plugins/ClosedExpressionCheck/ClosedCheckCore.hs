-- Copyright (c) 2026 albaDsl
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ClosedExpressionCheck.ClosedCheckCore (corePass) where

import ClosedExpressionCheck.Marker (closedCheck)
import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.Core
  ( Alt (Alt),
    Bind (NonRec, Rec),
    CoreAlt,
    CoreBind,
    CoreExpr,
    Expr (App, Case, Cast, Lam, Let, Tick, Type, Var),
    bindersOfBinds,
    isId,
  )
import GHC.Core.Class (classMethods, classSCSelIds)
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.InstEnv
  ( InstEnvs (..),
    PotentialUnifiers (..),
    instanceSig,
    lookupInstEnv,
  )
import GHC.Core.Opt.OccurAnal (occurAnalyseExpr)
import GHC.Core.Predicate (EqRel (..), Pred (..), classifyPredType, isIPClass)
import GHC.Core.TyCo.Rep (Type)
import GHC.Core.TyCo.Subst (substTys, zipTvSubst)
import GHC.Data.Strict qualified as Strict
import GHC.Plugins
  ( CoreM,
    CoreToDo (CoreDoPluginPass),
    Id,
    IsDoc (vcat),
    IsLine (ftext, hsep, text, (<+>), (<>)),
    IsOutput (empty),
    ModGuts (mg_binds, mg_deps, mg_inst_env, mg_module),
    Outputable (ppr),
    SDoc,
    SrcSpan (RealSrcSpan),
    Var (varType),
    VarSet,
    elemVarSet,
    emptyVarSet,
    errorMsg,
    filterVarSet,
    fsLit,
    getHscEnv,
    hscEPS,
    isEmptyVarSet,
    isGlobalId,
    isStrLitTy,
    liftIO,
    mkModuleSet,
    mkVarSet,
    nest,
    noSrcSpan,
    nonDetEltsUniqSet,
    parens,
    thNameToGhcName,
    throwGhcExceptionIO,
    unionVarSet,
    unitVarSet,
  )
import GHC.Types.Tickish (GenTickish (SourceNote))
import GHC.Types.TyThing (MonadThings (lookupId))
import GHC.Unit.External (ExternalPackageState (eps_inst_env))
import GHC.Unit.Module.Deps (Dependencies (dep_orphs))
import GHC.Utils.Panic (GhcException (..))
import Language.Haskell.TH qualified as TH

data CoreCtx = CoreCtx
  { ccTops :: VarSet,
    ccMarker :: Id,
    ccLoc :: SrcSpan,
    ccEnclosing :: Maybe Id,
    ccClosedLocs :: VarSet,
    ccRecGroup :: VarSet,
    ccErrors :: IORef Int,
    ccInstEnvs :: InstEnvs
  }

corePass :: CoreToDo
corePass = CoreDoPluginPass "ClosedCheck" corePassM

corePassM :: ModGuts -> CoreM ModGuts
corePassM guts = do
  -- pprTraceM "Core pass running on" (ppr (mg_module guts))
  markerId <- lookupCoreId 'ClosedExpressionCheck.Marker.closedCheck
  errRef <- liftIO (newIORef 0)
  hsc <- getHscEnv
  eps <- liftIO (hscEPS hsc)
  let topSet = mkVarSet (bindersOfBinds (mg_binds guts))
      instEnvs =
        InstEnvs
          { ie_global = eps_inst_env eps,
            ie_local = mg_inst_env guts,
            ie_visible = mkModuleSet (mg_module guts : dep_orphs (mg_deps guts))
          }
      ctx0 =
        CoreCtx
          { ccTops = topSet,
            ccMarker = markerId,
            ccLoc = noSrcSpan,
            ccEnclosing = Nothing,
            ccClosedLocs = emptyVarSet,
            ccRecGroup = emptyVarSet,
            ccInstEnvs = instEnvs,
            ccErrors = errRef
          }
  binds' <- mapM (stripBind ctx0) (mg_binds guts)
  n <- liftIO (readIORef errRef)
  when (n > 0) $
    liftIO $
      throwGhcExceptionIO $
        ProgramError $
          "AlbaDsl: " ++ show n ++ " closedness error(s); see messages above."
  pure guts {mg_binds = binds'}

stripAlt :: CoreCtx -> CoreAlt -> CoreM CoreAlt
stripAlt ctx (Alt c xs e) = Alt c xs <$> stripExpr ctx e

reportCoreError :: CoreCtx -> [Var] -> CoreM ()
reportCoreError ctx vars = do
  let msg =
        vcat
          [ text "AlbaDsl: fn/quot/constant body is not a closed expression."
              <+> text "Captured values:",
            nest
              2
              ( vcat
                  [ ppr v
                      <+> text "::"
                      <+> ppr (varType v)
                      <+> parens (describe v)
                  | v <- vars
                  ]
              ),
            case ccEnclosing ctx of
              Just b ->
                text "in the definition of"
                  <+> ppr b
                  GHC.Plugins.<> text "."
              Nothing -> empty
          ]
  errorMsg msg
  liftIO $ modifyIORef' (ccErrors ctx) (+ 1)

describe :: Var -> SDoc
describe v = case classifyPredType (varType v) of
  ClassPred cls [symTy, valTy]
    | isIPClass cls,
      Just sym <- isStrLitTy symTy ->
        text "implicit parameter ?"
          GHC.Plugins.<> ftext sym
          <+> text "::"
          <+> ppr valTy
  ClassPred cls tys ->
    text "constraint" <+> ppr cls <+> hsep (map (parens . ppr) tys)
  EqPred NomEq t1 t2 ->
    text "equality" <+> ppr t1 <+> text "~" <+> ppr t2
  EqPred ReprEq t1 t2 ->
    text "repr. equality" <+> ppr t1 <+> text "~R" <+> ppr t2
  _ -> text "local binding"

checkClosedCore :: CoreCtx -> CoreExpr -> CoreM ()
checkClosedCore ctx arg = do
  let fvs = liveFreeVars arg
      bad = filterVarSet (isRealCapture ctx) fvs
  unless (isEmptyVarSet bad) $ reportCoreError ctx (nonDetEltsUniqSet bad)

liveFreeVars :: CoreExpr -> VarSet
liveFreeVars e = exprFreeVars (occurAnalyseExpr e)

isRealCapture :: CoreCtx -> Var -> Bool
isRealCapture ctx v =
  isId v
    && not (v `elemVarSet` ccTops ctx)
    && not (isGlobalId v)
    && not (v `elemVarSet` ccRecGroup ctx)
    && not (v `elemVarSet` ccClosedLocs ctx)
    && not (isCallStackDict v)
    && not (isEmptyClassDict v)
    && not (isStructurallyClosedDict ctx v)

isStructurallyClosedPred :: CoreCtx -> Type -> Bool
isStructurallyClosedPred ctx p =
  case classifyPredType p of
    ClassPred cls tys ->
      case lookupInstEnv False (ccInstEnvs ctx) cls tys of
        ([(inst, matchTys)], NoUnifiers {}, _)
          | Just concrete <- sequence matchTys ->
              let (tvs, theta, _, _) = instanceSig inst
                  subst = zipTvSubst tvs concrete
                  theta' = substTys subst theta
               in all (isStructurallyClosedPred ctx) theta'
        _ -> False
    _ -> False

isStructurallyClosedDict :: CoreCtx -> Var -> Bool
isStructurallyClosedDict ctx v = isStructurallyClosedPred ctx (varType v)

isEmptyClassDict :: Var -> Bool
isEmptyClassDict v = case classifyPredType (varType v) of
  ClassPred cls _
    | not (isIPClass cls),
      null (classMethods cls),
      null (classSCSelIds cls) ->
        True
  _ -> False

isCallStackDict :: Var -> Bool
isCallStackDict v = case classifyPredType (varType v) of
  ClassPred cls [symTy, _]
    | isIPClass cls,
      Just sym <- isStrLitTy symTy ->
        sym == fsLit "callStack"
  _ -> False

-- Returns True iff the RHS captures nothing (modulo the current context).
isRhsClosed :: CoreCtx -> CoreExpr -> Bool
isRhsClosed ctx rhs =
  isEmptyVarSet (filterVarSet (isRealCapture ctx) (exprFreeVars rhs))

stripBind :: CoreCtx -> CoreBind -> CoreM CoreBind
stripBind ctx (NonRec b rhs) = do
  rhs' <-
    stripExpr
      (ctx {ccEnclosing = Just b, ccRecGroup = emptyVarSet})
      rhs
  pure (NonRec b rhs')
stripBind ctx (Rec prs) = do
  let grp = mkVarSet (map fst prs)
      ctx' = ctx {ccRecGroup = grp}
  prs' <-
    mapM (\(b, e) -> (b,) <$> stripExpr (ctx' {ccEnclosing = Just b}) e) prs
  pure (Rec prs')

stripExpr :: CoreCtx -> CoreExpr -> CoreM CoreExpr
stripExpr ctx = \case
  App (App (Var v) (Type _)) arg
    | v == ccMarker ctx -> do checkClosedCore ctx arg; stripExpr ctx arg
  Tick t@(SourceNote rsp _) e ->
    Tick t <$> stripExpr (ctx {ccLoc = RealSrcSpan rsp Strict.Nothing}) e
  Tick t e -> Tick t <$> stripExpr ctx e
  Let b e ->
    let extra = provenClosed ctx b
        ctxBody = ctx {ccClosedLocs = ccClosedLocs ctx `unionVarSet` extra}
     in Let <$> stripBind ctx b <*> stripExpr ctxBody e
  App f a -> App <$> stripExpr ctx f <*> stripExpr ctx a
  Lam b e -> Lam b <$> stripExpr ctx e
  Case s b t alts ->
    Case <$> stripExpr ctx s <*> pure b <*> pure t <*> mapM (stripAlt ctx) alts
  Cast e co -> flip Cast co <$> stripExpr ctx e
  other -> pure other

provenClosed :: CoreCtx -> CoreBind -> VarSet
provenClosed ctx (NonRec b rhs)
  | isRhsClosed ctx rhs = unitVarSet b
  | otherwise = emptyVarSet
provenClosed ctx (Rec prs) =
  let grp = mkVarSet (map fst prs)
      ctx' = ctx {ccRecGroup = ccRecGroup ctx `unionVarSet` grp}
   in if all (isRhsClosed ctx' . snd) prs then grp else emptyVarSet

lookupCoreId :: TH.Name -> CoreM Id
lookupCoreId thName = do
  mb <- thNameToGhcName thName
  case mb of
    Just n -> lookupId n
    Nothing -> error $ "AlbaDsl Plugin: cannot find " ++ show thName
