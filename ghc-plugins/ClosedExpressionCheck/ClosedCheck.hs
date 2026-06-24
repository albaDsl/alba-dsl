-- Copyright (c) 2026 albaDsl
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ClosedExpressionCheck.ClosedCheck (plugin) where

import ClosedExpressionCheck.ClosedCheckCore (corePass)
import ClosedExpressionCheck.FreeVariables
  ( bindBinders,
    freeLocalsExcept,
    freeLocalsOfBind,
  )
import Control.Monad (unless)
import Data.Generics (everything, everywhereM, listify, mkM, mkQ)
import Data.Monoid (Any (..))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs
  ( GhcRn,
    HasAnnotation (noAnnSrcSpan),
    HsExpr (HsApp, HsVar),
    HsGroup,
    LHsBindLR,
    LHsExpr,
    combineSrcSpansA,
    locA,
    mkModuleName,
    moduleNameString,
    noExtField,
  )
import GHC.Iface.Env (lookupOrig)
import GHC.Plugins
  ( GenLocated (L),
    GenModule (moduleName),
    IsDoc (($$)),
    IsLine (text, (<+>)),
    Name,
    Outputable (ppr),
    PkgQual (NoPkgQual),
    Plugin (installCoreToDos, pluginRecompile, renamedResultAction),
    SrcSpan (..),
    defaultPlugin,
    liftIO,
    mkVarOcc,
    nameModule_maybe,
    nameOccName,
    nest,
    occNameString,
    purePlugin,
  )
import GHC.Tc.Errors.Types (TcRnMessage (..))
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.Monad (addErrAt, failWithTc, getTopEnv)
import GHC.Types.Error
  ( NoDiagnosticOpts (..),
    UnknownDiagnostic (..),
    mkPlainError,
    noHints,
  )
import GHC.Unit.Finder (FindResult (..), findImportedModule)

plugin :: Plugin
plugin =
  defaultPlugin
    { renamedResultAction = sourcePass,
      installCoreToDos = \_opts todos -> pure (corePass : todos),
      pluginRecompile = purePlugin
    }

type Target = (String, String, [Int])

-- - Finds every application of a targeted function.
-- - Runs a scope-aware free-variable check on each designated argument.
-- - Wraps the argument in `Marker.closedCheck` so the Core pass can
--   re-check it after type-class dictionaries have been made explicit.
sourcePass _opts tcg grp = do
  let hasTarget = getAny $ everything (<>) (mkQ mempty spotTarget) grp
      spotTarget :: LHsExpr GhcRn -> Any
      spotTarget le = Any $ case unwrapApp le of
        Just (n, _, _) -> isTarget n
        _ -> False
  if not hasTarget
    then pure (tcg, grp)
    else do
      cc <- lookupClosedCheckName
      let closedLocals = computeClosedLocals grp
      grp' <- everywhereM (mkM (rewriteExpr cc closedLocals)) grp
      pure (tcg, grp')

-- Peel nested HsApp down to its head variable, returning also the full arg
-- list (left-to-right) and the source span of the head.
unwrapApp :: LHsExpr GhcRn -> Maybe (Name, [LHsExpr GhcRn], SrcSpan)
unwrapApp = go []
  where
    go acc (L _ (HsApp _ fun arg)) = go (arg : acc) fun
    go acc (L sp (HsVar _ (L _ n))) = Just (n, acc, locA sp)
    go _ _ = Nothing

isTarget :: Name -> Bool
isTarget n = case nameModule_maybe n of
  Just m ->
    let mn = moduleNameString (moduleName m)
        on = occNameString (nameOccName n)
     in any (\(tm, tn, _) -> tm == mn && tn == on) targets
  Nothing -> False

-- Functions to check.
targets :: [Target]
targets =
  [ ("Alba.Dsl.V1.Bch2026.Lang", "fn", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsA", "quot0", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsA", "quot1", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsA", "quot2", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsA", "quot2_0", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsA", "quot3", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsA", "quot4", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsB", "quot0", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsB", "quot1", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsB", "quot2", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsB", "quot2_0", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsB", "quot3", [0]),
    ("Alba.Dsl.V1.Bch2026.QuotationsB", "quot4", [0]),
    ("Alba.Dsl.V1.Bch2026.Lang", "constant", [0]),
    ("Alba.Dsl.V1.Bch2026.Lang", "runtimeConstant", [0])
  ]

lookupClosedCheckName :: TcM Name
lookupClosedCheckName = do
  hsc <- getTopEnv
  mb <-
    liftIO $
      findImportedModule
        hsc
        (mkModuleName "ClosedExpressionCheck.Marker")
        NoPkgQual
  case mb of
    Found _ m -> lookupOrig m (mkVarOcc "closedCheck")
    _ -> do
      let doc = text "ClosedCheck: cannot locate Marker"
          diag = mkPlainError noHints doc
      failWithTc
        (TcRnUnknownMessage (UnknownDiagnostic (const NoDiagnosticOpts) diag))

-- All local binders whose RHSs transitively reference only External names and
-- each other. These are safe to mention inside a targeted argument even though
-- they are themselves Internal.
computeClosedLocals :: HsGroup GhcRn -> Set Name
computeClosedLocals grp = shrink (Set.fromList (concatMap fst pairs))
  where
    allLBinds :: [LHsBindLR GhcRn GhcRn]
    allLBinds = listify (const True) grp

    pairs :: [([Name], Set Name)]
    pairs = [(bindBinders lb, freeLocalsOfBind lb) | lb <- allLBinds]

    -- Start with every local binder admitted; drop any whose RHS
    -- references something not (yet) admitted.  Converges because the
    -- set shrinks monotonically.
    shrink known =
      let known' =
            Set.fromList
              [n | (ns, fvs) <- pairs, fvs `Set.isSubsetOf` known, n <- ns]
       in if known' == known then known else shrink known'

rewriteExpr :: Name -> Set Name -> LHsExpr GhcRn -> TcM (LHsExpr GhcRn)
rewriteExpr cc closed le =
  case unwrapApp le of
    Just (n, args, _) | isTarget n -> do
      args' <- rewriteTargetArgs cc closed n args
      pure (reapply (justHead le) args')
    _ -> pure le

rewriteTargetArgs ::
  Name -> Set Name -> Name -> [LHsExpr GhcRn] -> TcM [LHsExpr GhcRn]
rewriteTargetArgs cc closed n args =
  let ixs = argIndicesFor n
   in mapM
        ( \(i, a) ->
            if i `elem` ixs
              then checkAndWrap cc closed n i a
              else pure a
        )
        (zip [0 ..] args)

argIndicesFor :: Name -> [Int]
argIndicesFor n = case nameModule_maybe n of
  Just m ->
    let mn = moduleNameString (moduleName m)
        on = occNameString (nameOccName n)
     in concat [ixs | (tm, tn, ixs) <- targets, tm == mn, tn == on]
  Nothing -> []

checkAndWrap ::
  Name -> Set Name -> Name -> Int -> LHsExpr GhcRn -> TcM (LHsExpr GhcRn)
checkAndWrap cc closed fn i arg@(L sp _) = do
  let frees = freeLocalsExcept closed arg
  unless (Set.null frees) $ reportSourceError (locA sp) fn i frees
  pure (wrapClosedCheck cc arg)

justHead :: LHsExpr GhcRn -> LHsExpr GhcRn
justHead (L _ (HsApp _ f _)) = justHead f
justHead h = h

reapply :: LHsExpr GhcRn -> [LHsExpr GhcRn] -> LHsExpr GhcRn
reapply = foldl (\f a -> L (combineLocs f a) (HsApp noExtField f a))
  where
    combineLocs (L sp1 _) (L sp2 _) = combineSrcSpansA sp1 sp2

wrapClosedCheck :: Name -> LHsExpr GhcRn -> LHsExpr GhcRn
wrapClosedCheck cc arg@(L sp _) =
  let nameLoc = noAnnSrcSpan (locA sp)
      headE = L sp (HsVar noExtField (L nameLoc cc))
   in L sp (HsApp noExtField headE arg)

reportSourceError :: SrcSpan -> Name -> Int -> Set Name -> TcM ()
reportSourceError sp fn _ frees = do
  let msg =
        ( text "AlbaDsl: Body of"
            <+> ppr fn
            <+> text "is not a closed expression."
        )
          $$ nest 2 (text "Captured local names:" <+> ppr (Set.toList frees))
          $$ text "An fn/quot/constant body must be a closed expression. It"
          <+> text "can't reference \"Haskell function\" arguments"
          <+> text "directly or indirectly."
      diag = mkPlainError noHints msg
  addErrAt
    sp
    (TcRnUnknownMessage (UnknownDiagnostic (const NoDiagnosticOpts) diag))
