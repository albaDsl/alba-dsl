-- Copyright (c) 2026 albaDsl
{-# LANGUAGE LambdaCase #-}

module ClosedExpressionCheck.FreeVariables
  ( freeLocals,
    freeLocalsExcept,
    freeLocalsOfBind,
    bindBinders,
  )
where

import Data.Foldable qualified as F
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs
  ( ArithSeqInfo (..),
    CollectFlag (CollNoDictBinders),
    GRHS (GRHS),
    GRHSs (GRHSs),
    GhcRn,
    HsBind,
    HsBindLR
      ( FunBind,
        PatBind,
        VarBind,
        fun_id,
        fun_matches,
        pat_lhs,
        pat_rhs,
        var_id,
        var_rhs
      ),
    HsExpr
      ( ArithSeq,
        ExplicitList,
        ExplicitSum,
        ExplicitTuple,
        ExprWithTySig,
        HsApp,
        HsAppType,
        HsCase,
        HsDo,
        HsEmbTy,
        HsGetField,
        HsIPVar,
        HsIf,
        HsLam,
        HsLet,
        HsLit,
        HsMultiIf,
        HsOverLabel,
        HsOverLit,
        HsPar,
        HsPragE,
        HsProjection,
        HsRecSel,
        HsStatic,
        HsTypedBracket,
        HsTypedSplice,
        HsUnboundVar,
        HsUntypedBracket,
        HsUntypedSplice,
        HsVar,
        NegApp,
        OpApp,
        RecordCon,
        RecordUpd,
        SectionL,
        SectionR
      ),
    HsFieldBind (HsFieldBind),
    HsLocalBinds,
    HsLocalBindsLR (EmptyLocalBinds, HsIPBinds, HsValBinds),
    HsRecFields (HsRecFields, rec_flds),
    HsRecordBinds,
    HsTupArg (Present),
    HsValBinds,
    HsValBindsLR (ValBinds, XValBindsLR),
    LGRHS,
    LHsBindLR,
    LHsExpr,
    LMatch,
    LPat,
    LStmt,
    Match (Match, m_grhss, m_pats),
    MatchGroup (MG, mg_alts),
    NHsValBindsLR (NValBinds),
    StmtLR (BindStmt, BodyStmt, LastStmt, LetStmt),
    collectLocalBinders,
    collectPatBinders,
    noExtField,
    noLocA,
    noSyntaxExpr,
  )
import GHC.Types.Name (Name, isInternalName)
import GHC.Types.SrcLoc

freeLocals :: LHsExpr GhcRn -> Set Name
freeLocals = freeLocalsExcept Set.empty

freeLocalsExcept :: Set Name -> LHsExpr GhcRn -> Set Name
freeLocalsExcept exempt e =
  Set.filter isInternalName (fvExpr e) `Set.difference` exempt

freeLocalsOfBind :: LHsBindLR GhcRn GhcRn -> Set Name
freeLocalsOfBind (L _ b) = Set.filter isInternalName (fvBind b)

bindBinders :: LHsBindLR GhcRn GhcRn -> [Name]
bindBinders (L _ b) = case b of
  FunBind {fun_id = L _ n} -> [n]
  PatBind {pat_lhs = p} -> collectPatBinders CollNoDictBinders p
  VarBind {var_id = n} -> [n]
  _ -> []

fvExpr :: LHsExpr GhcRn -> Set Name
fvExpr (L _ e) = case e of
  -- Occurrences
  HsVar _ (L _ n) -> Set.singleton n
  HsUnboundVar {} -> Set.empty
  HsRecSel {} -> Set.empty
  HsOverLabel {} -> Set.empty
  HsIPVar {} -> Set.empty
  HsOverLit {} -> Set.empty
  HsLit {} -> Set.empty
  -- Applicative / operator structure
  HsApp _ f a -> fvExpr f `Set.union` fvExpr a
  HsAppType _ f _ -> fvExpr f
  OpApp _ l op r -> Set.unions [fvExpr l, fvExpr op, fvExpr r]
  NegApp _ x _ -> fvExpr x
  HsPar _ x -> fvExpr x
  SectionL _ a b -> fvExpr a `Set.union` fvExpr b
  SectionR _ a b -> fvExpr a `Set.union` fvExpr b
  -- Aggregates
  ExplicitTuple _ args _ -> Set.unions (map fvTupArg args)
  ExplicitSum _ _ _ x -> fvExpr x
  ExplicitList _ xs -> Set.unions (map fvExpr xs)
  RecordCon _ _ flds -> fvRecFlds flds
  RecordUpd _ x _ -> fvExpr x
  -- Control
  HsIf _ c t el -> Set.unions [fvExpr c, fvExpr t, fvExpr el]
  HsMultiIf _ alts -> Set.unions (map fvGRHS alts)
  -- Annotations / misc
  ExprWithTySig _ x _ -> fvExpr x
  ArithSeq _ _ info -> fvArithSeq info
  HsPragE _ _ x -> fvExpr x
  HsStatic _ x -> fvExpr x
  HsProjection {} -> Set.empty
  HsGetField _ x _ -> fvExpr x
  HsEmbTy _ _ -> Set.empty
  -- Template Haskell / quotations: treat as opaque. Anything referenced
  -- inside a bracket is scope-checked by GHC separately and is not the
  -- target of our "closed expression" notion.
  HsTypedBracket {} -> Set.empty
  HsUntypedBracket {} -> Set.empty
  HsTypedSplice {} -> Set.empty
  HsUntypedSplice {} -> Set.empty
  -- Binding forms: subtract the binders they introduce
  HsLam _ _ mg -> fvMatchGroup mg
  HsCase _ scrut mg -> fvExpr scrut `Set.union` fvMatchGroup mg
  HsLet _ binds body ->
    let (bs, fvBinds) = fvLocalBinds binds
     in (fvBinds `Set.union` fvExpr body) `Set.difference` bs
  HsDo _ _ (L _ stmts) -> fvStmts stmts
  -- Anything else: We err on the side of "no free vars reported".
  _ -> Set.empty

fvMatchGroup :: MatchGroup GhcRn (LHsExpr GhcRn) -> Set Name
fvMatchGroup (MG {mg_alts = L _ ms}) = Set.unions (map fvMatch ms)

fvMatch :: LMatch GhcRn (LHsExpr GhcRn) -> Set Name
fvMatch (L _ (Match {m_pats = pats, m_grhss = grhss})) =
  let patBinders = Set.fromList (concatMap collectPatBinders' pats)
   in fvGRHSs grhss `Set.difference` patBinders

collectPatBinders' :: LPat GhcRn -> [Name]
collectPatBinders' = collectPatBinders CollNoDictBinders

fvGRHSs :: GRHSs GhcRn (LHsExpr GhcRn) -> Set Name
fvGRHSs (GRHSs _ grhss binds) =
  let (bs, fvBinds) = fvLocalBinds binds
      fvAlts = Set.unions (map fvGRHS grhss)
   in (fvAlts `Set.union` fvBinds) `Set.difference` bs

-- Guards bind names visible in `body`, so we model them as a do-block
-- ending with `body`. Scoping is identical.
fvGRHS :: LGRHS GhcRn (LHsExpr GhcRn) -> Set Name
fvGRHS (L _ (GRHS _ guards body)) =
  fvStmts (guards ++ [noLocA (LastStmt noExtField body Nothing noSyntaxExpr)])

-- Returns (names bound, free variables of the RHSs, with mutual recursion
-- accounted for).
fvLocalBinds :: HsLocalBinds GhcRn -> (Set Name, Set Name)
fvLocalBinds lb = case lb of
  HsValBinds _ vbs ->
    let boundHere = Set.fromList (collectLocalBinders CollNoDictBinders lb)
        rhsFVs = fvValBinds vbs
     in (boundHere, rhsFVs `Set.difference` boundHere)
  HsIPBinds {} -> (Set.empty, Set.empty)
  EmptyLocalBinds {} -> (Set.empty, Set.empty)

fvValBinds :: HsValBinds GhcRn -> Set Name
fvValBinds = \case
  XValBindsLR (NValBinds pairs _) ->
    Set.unions [fvBind b | (_, bs) <- pairs, L _ b <- F.toList bs]
  ValBinds _ bs _ ->
    Set.unions [fvBind b | L _ b <- F.toList bs]

fvBind :: HsBind GhcRn -> Set Name
fvBind = \case
  FunBind {fun_matches = mg} ->
    fvMatchGroup mg
  PatBind {pat_lhs = p, pat_rhs = grhss} ->
    fvGRHSs grhss `Set.difference` Set.fromList (collectPatBinders' p)
  VarBind {var_rhs = rhs} ->
    fvExpr rhs
  _ -> Set.empty

fvStmts :: [LStmt GhcRn (LHsExpr GhcRn)] -> Set Name
fvStmts [] = Set.empty
fvStmts (L _ s : rest) = case s of
  LastStmt _ e _ _ -> fvExpr e
  BodyStmt _ e _ _ -> fvExpr e `Set.union` fvStmts rest
  BindStmt _ p e ->
    let bound = Set.fromList (collectPatBinders' p)
     in fvExpr e `Set.union` (fvStmts rest `Set.difference` bound)
  LetStmt _ binds ->
    let (bs, fvBinds) = fvLocalBinds binds
     in fvBinds `Set.union` (fvStmts rest `Set.difference` bs)
  -- Recursive, parallel, and transform statements are rare in closed
  -- expressions. Be lenient rather than risk spurious errors.
  _ -> fvStmts rest

fvTupArg :: HsTupArg GhcRn -> Set Name
fvTupArg = \case
  Present _ e -> fvExpr e
  _ -> Set.empty

fvArithSeq :: ArithSeqInfo GhcRn -> Set Name
fvArithSeq = \case
  From a -> fvExpr a
  FromThen a b -> fvExpr a `Set.union` fvExpr b
  FromTo a b -> fvExpr a `Set.union` fvExpr b
  FromThenTo a b c -> Set.unions [fvExpr a, fvExpr b, fvExpr c]

fvRecFlds :: HsRecordBinds GhcRn -> Set Name
fvRecFlds (HsRecFields {rec_flds = flds}) =
  Set.unions [fvExpr e | L _ (HsFieldBind _ _ e _) <- flds]
