-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Vector
  ( TVector,
    length,
    lengthF,
    null,
    lookup,
    lookupF,
    head,
    headF,
    last,
    lastF,
    init,
    initF,
    tail,
    tailF,
    take,
    drop,
    splitAt,
    splitAtF,
    uncons,
    unconsF,
    unsnoc,
    unsnocF,
    empty,
    singleton,
    replicate,
    replicateF,
    generate,
    generateF,
    iterateN,
    iterateNF,
    cons,
    consF,
    snoc,
    snocF,
    append,
    reverse,
    reverseF,
    map,
    mapF,
    zip,
    zipF,
    zipWith,
    zipWithF,
    unzip,
    unzipF,
    filter,
    filterF,
    foldl,
    foldlF,
  )
where

import Alba.Dsl.V1.Bch2026
  ( FN,
    StackEntry,
    TBool,
    TBytes,
    TLambda,
    TNat,
    begin,
    bytes,
    cast,
    castStack,
    del,
    delCount,
    function,
    invoke1,
    invoke2,
    lambda0,
    lambda1,
    lambda2,
    name,
    nat,
    ns2,
    ns3,
    ns4,
    ns5,
    ns6,
    ns7,
    op0NotEqual,
    op1Add,
    op1SubUnsafe,
    op2Drop,
    op2Dup,
    opCat,
    opDiv,
    opDrop,
    opEqual,
    opFalse,
    opGreaterThan,
    opGreaterThanOrEqual,
    opIf,
    opMul,
    opNotIf,
    opNumEqual,
    opRoll,
    opSize,
    opSplit,
    opTrue,
    opUntil,
    opVerify,
    pick,
    roll,
    un,
    un2,
    un3,
    un4,
    un5,
    un6,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Applicative (liftA2Maybe)
import Alba.Dsl.V1.Bch2026.Contract.Hide
  ( Hide,
    dropHide,
    hide,
    hide2,
    nipHide,
  )
import Alba.Dsl.V1.Bch2026.Contract.Maybe
  ( TMaybe,
    fromMaybe',
    ifJust,
    just,
    maybe,
    nothing,
  )
import Alba.Dsl.V1.Bch2026.Contract.Maybe qualified as Maybe
import Alba.Dsl.V1.Bch2026.Contract.PackFs
  ( PackFs (..),
    TPackFs,
    getPack,
    packFs,
    tcDrop,
    tcPick,
    tcRoll,
    tcSize,
    tcUnpack,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand
  ( dup,
    fromAlt,
    nip,
    rot,
    swap,
    toAlt,
  )
import Alba.Dsl.V1.Bch2026.Contract.Tuple (TTuple, fst, snd, tuple, untuple)
import Alba.Dsl.V1.Bch2026.Contract.TupleFs (TTupleFs, calcPackFs)
import Alba.Dsl.V1.Bch2026.Contract.TupleFs qualified as TFS
import Alba.Dsl.V1.Bch2026.LangArgs (Loop)
import Data.Kind (Type)
import Prelude ()

data TVector (a :: Type)

instance StackEntry (TVector a)

-- ## Length.
length :: forall a s. (PackFs a) => FN (s > TVector a) (s > TNat)
length = v2b # opSize # nip # size @a # opDiv

lengthF :: FN (s > TPackFs a > TVector a) (s > TNat)
lengthF =
  function
    ( begin
        # (ns2 @"packFs" @"vec" # un @"vec")
        # (v2b # opSize # nip # tcSize # opDiv # tcDrop)
    )

null :: FN (s > TVector a) (s > TBool)
null = v2b # bytes [] # opEqual

-- ## Indexing.
lookup :: forall a s. (PackFs a) => FN (s > TVector a > TNat) (s > TMaybe a)
lookup = packFs @a # rot # rot # lookupF

lookupF ::
  (StackEntry a) => FN (s > TPackFs a > TVector a > TNat) (s > TMaybe a)
lookupF =
  function
    ( begin
        # ns3 @"packFs" @"vec" @"cnt"
        # (tcPick # roll @"cnt" # roll @"vec" # splitAtF # nip)
        # (tcRoll # swap # headF)
    )

head :: forall a s. (PackFs a) => FN (s > TVector a) (s > TMaybe a)
head = packFs @a # swap # headF

headF :: (StackEntry a) => FN (s > TPackFs a > TVector a) (s > TMaybe a)
headF =
  function
    ( begin
        # (ns2 @"packFs" @"vec" # tcPick # roll @"vec" # unconsF)
        # (nothing # fstJust # rot # maybe # tcDrop)
    )

-- Save a function slot by factoring out lambda used in more than one place.
fstJust ::
  (StackEntry a, StackEntry b) => FN s (s > TLambda '[TTuple a b] '[TMaybe a])
fstJust = lambda1 (fst # just)

last :: forall a s. (PackFs a) => FN (s > TVector a) (s > TMaybe a)
last = packFs @a # swap # lastF

lastF :: (StackEntry a) => FN (s > TPackFs a > TVector a) (s > TMaybe a)
lastF =
  function
    ( begin
        # (ns2 @"packFs" @"vec" # tcPick # roll @"vec" # unsnocF)
        # (nothing # sndJust # rot # maybe # tcDrop)
    )

sndJust ::
  (StackEntry a, StackEntry b) => FN s (s > TLambda '[TTuple a b] '[TMaybe b])
sndJust = lambda1 (snd # just)

-- ## Slicing.
init :: forall a s. (PackFs a) => FN (s > TVector a) (s > TMaybe (TVector a))
init = packFs @a # swap # initF

initF ::
  (StackEntry a) => FN (s > TPackFs a > TVector a) (s > TMaybe (TVector a))
initF =
  function
    ( begin
        # (ns2 @"packFs" @"vec" # tcPick # roll @"vec" # unsnocF)
        # (nothing # fstJust # rot # maybe # tcDrop)
    )

tail :: forall a s. (PackFs a) => FN (s > TVector a) (s > TMaybe (TVector a))
tail = packFs @a # swap # tailF

tailF ::
  (StackEntry a) =>
  FN (s > TPackFs a > TVector a) (s > TMaybe (TVector a))
tailF =
  function
    ( begin
        # (ns2 @"packFs" @"vec" # tcPick # roll @"vec" # unconsF)
        # (nothing # sndJust # rot # maybe # tcDrop)
    )

take :: (PackFs a) => FN (s > TNat > TVector a) (s > TVector a)
take = splitAt # opDrop

drop :: (PackFs a) => FN (s > TNat > TVector a) (s > TVector a)
drop = splitAt # nip

splitAt ::
  forall a s.
  (PackFs a) =>
  FN (s > TNat > TVector a) (s > TVector a > TVector a)
splitAt = packFs @a # rot # rot # splitAtF

splitAtF :: FN (s > TPackFs a > TNat > TVector a) (s > TVector a > TVector a)
splitAtF =
  function
    ( begin
        # (ns3 @"packFs" @"idx" @"vec" # pick @"idx" # nat 0 # opEqual)
        # opNotIf
          ( begin
              # (tcPick # pick @"vec" # lengthF # pick @"idx" # opGreaterThan)
              # opIf
                (tcRoll # roll @"idx" # roll @"vec" # splitAtUnsafeF)
                (roll @"vec" # empty # delCount @2)
          )
          (empty # roll @"vec" # delCount @2)
    )

splitAtUnsafeF ::
  FN (s > TPackFs a > TNat > TVector a) (s > TVector a > TVector a)
splitAtUnsafeF =
  function
    ( begin
        # ns3 @"packFs" @"idx" @"vec"
        # (roll @"vec" # v2b # roll @"idx" # tcSize # opMul # opSplit)
        # (tcDrop # fixup)
    )
  where
    fixup :: FN (s' > TBytes > TBytes) (s' > TVector a > TVector a)
    fixup = castStack

uncons ::
  forall a s.
  (PackFs a) =>
  FN (s > TVector a) (s > TMaybe (TTuple a (TVector a)))
uncons = packFs @a # swap # unconsF

unconsF ::
  (StackEntry a) =>
  FN (s > TPackFs a > TVector a) (s > TMaybe (TTuple a (TVector a)))
unconsF =
  function
    ( begin
        # (ns2 @"packFs" @"vec" # un @"vec" # dup # null)
        # opNotIf
          ( begin
              # (nat 1 # swap # tcPick # rot # rot # splitAtUnsafeF # swap)
              # (v2b # tcUnpack # swap # tuple # just)
          )
          (opDrop # nothing)
        # tcDrop
    )

unsnoc ::
  forall a s.
  (PackFs a) =>
  FN (s > TVector a) (s > TMaybe (TTuple (TVector a) a))
unsnoc = packFs @a # swap # unsnocF

unsnocF ::
  (StackEntry a) =>
  FN (s > TPackFs a > TVector a) (s > TMaybe (TTuple (TVector a) a))
unsnocF =
  function
    ( begin
        # (ns2 @"packFs" @"vec" # un @"vec" # dup # null)
        # opNotIf
          ( begin
              # (dup # tcPick # swap # lengthF # op1SubUnsafe # swap)
              # (tcPick # rot # rot # splitAtUnsafeF # v2b # tcUnpack # tuple)
              # just
          )
          (opDrop # nothing)
        # tcDrop
    )

-- ## Construction.
empty :: FN s (s > TVector a)
empty = bytes [] # cast

singleton :: (PackFs a) => FN (s > a) (s > TVector a)
singleton = pack # b2v

singletonF :: (StackEntry a) => FN (s > TPackFs a > a) (s > TVector a)
singletonF = function (swap # getPack # invoke1 # cast)

replicate :: forall a s. (PackFs a) => FN (s > TNat > a) (s > TVector a)
replicate = packFs @a # rot # rot # replicateF

replicateF :: (StackEntry a) => FN (s > TPackFs a > TNat > a) (s > TVector a)
replicateF =
  function
    ( begin
        # (ns3 @"packFs" @"cnt" @"val" # pick @"cnt" # op0NotEqual)
        # opIf
          ( begin
              # (roll @"packFs" # roll @"val" # hide # roll @"cnt" # empty)
              # (opUntil loop # nip # nip # nip)
          )
          (delCount @3 # empty)
    )
  where
    loop :: (StackEntry a) => Loop (s > TPackFs a > Hide a > TNat > TVector a)
    loop =
      begin
        # ns4 @"packFs" @"val" @"cnt" @"acc"
        # (roll @"cnt" # op1SubUnsafe # dup # nat 0 # opNumEqual)
        # (tcPick # pick @"val" # roll @"acc" # nipHide # consF)
        # (swap # un2 @"packFs" @"val")

generate ::
  forall a s.
  (PackFs a) =>
  FN (s > TNat > TLambda '[TNat] '[a]) (s > TVector a)
generate = packFs @a # rot # rot # generateF

generateF ::
  forall a s.
  (StackEntry a) =>
  FN (s > TPackFs a > TNat > TLambda '[TNat] '[a]) (s > TVector a)
generateF =
  function
    ( begin
        # (ns3 @"packFs" @"cnt" @"fn" # pick @"cnt" # op0NotEqual)
        # opIf
          ( begin
              # (tcRoll # roll @"fn" # roll @"cnt" # nat 0 # empty)
              # (opUntil loop # nip # nip # nip # nip)
          )
          (delCount @3 # empty)
    )
  where
    loop ::
      (StackEntry a) =>
      Loop (s > TPackFs a > TLambda '[TNat] '[a] > TNat > TNat > TVector a)
    loop =
      begin
        # ns5 @"packFs" @"fn" @"limit" @"cnt" @"acc"
        # (pick @"cnt" # op1Add # dup # pick @"limit" # opGreaterThanOrEqual)
        # (tcPick # roll @"acc" # roll @"cnt" # pick @"fn" # invoke1 # snocF)
        # (swap # un3 @"packFs" @"fn" @"limit")

iterateN ::
  forall a s.
  (PackFs a) =>
  FN (s > TNat > TLambda '[a] '[a] > a) (s > TVector a)
iterateN =
  begin
    # ns3 @"cnt" @"fn" @"val"
    # (packFs @a # roll @"cnt" # roll @"fn" # roll @"val" # iterateNF)

iterateNF ::
  (StackEntry a) =>
  FN (s > TPackFs a > TNat > TLambda '[a] '[a] > a) (s > TVector a)
iterateNF =
  function
    ( begin
        # (ns4 @"packFs" @"cnt" @"fn" @"val" # pick @"cnt" # op0NotEqual)
        # opIf
          ( begin
              # (pick @"packFs" # roll @"fn" # name @"val'" (roll @"val"))
              # (roll @"cnt" # op1SubUnsafe)
              # (roll @"packFs" # pick @"val'" # singletonF # un @"val'")
              # (opUntil loop # nip # nip # nip # nip)
          )
          (delCount @4 # empty)
    )
  where
    loop ::
      (StackEntry a) =>
      Loop (s > TPackFs a > TLambda '[a] '[a] > a > TNat > TVector a)
    loop =
      begin
        # ns5 @"packFs" @"fn" @"val" @"cnt" @"acc"
        # name @"val'" (pick @"fn" # roll @"val" # swap # invoke1)
        # (roll @"cnt" # op1SubUnsafe # dup # nat 0 # opNumEqual)
        # (tcPick # roll @"acc" # pick @"val'" # snocF)
        # (swap # un3 @"packFs" @"fn" @"val'")

-- ## Concatenation.
cons :: forall a s. (PackFs a) => FN (s > a > TVector a) (s > TVector a)
cons = packFs @a # rot # rot # consF

consF :: (StackEntry a) => FN (s > TPackFs a > a > TVector a) (s > TVector a)
consF =
  function
    ( begin
        # (rot # toAlt # swap # fromAlt # getPack # invoke1 # swap # v2b)
        # (opCat # cast)
    )

snoc :: forall a s. (PackFs a) => FN (s > TVector a > a) (s > TVector a)
snoc = packFs @a # rot # rot # snocF

snocF :: (StackEntry a) => FN (s > TPackFs a > TVector a > a) (s > TVector a)
snocF = function (fixup # rot # getPack # invoke1 # opCat # cast)
  where
    fixup :: FN (s > TVector a > a) (s > TBytes > a)
    fixup = castStack

append :: (PackFs a) => FN (s > TVector a > TVector a) (s > TVector a)
append = fixup # opCat # cast
  where
    fixup :: FN (s > TVector a > TVector a) (s > TBytes > TBytes)
    fixup = castStack

-- ## Permutation.
reverse :: forall a s. (PackFs a) => FN (s > TVector a) (s > TVector a)
reverse = packFs @a # swap # reverseF

reverseF :: (StackEntry a) => FN (s > TPackFs a > TVector a) (s > TVector a)
reverseF =
  function
    ( begin
        # (ns2 @"packFs" @"vec" # pick @"packFs")
        # lambda2
          ( begin
              # (swap # untuple # swap # dup # toAlt # rot # rot # consF)
              # (fromAlt # swap # tuple)
          )
        # (roll @"packFs" # empty # tuple # roll @"vec" # foldlF # untuple)
        # nip
    )

-- ## Mapping.
map ::
  forall a b s.
  (PackFs a, PackFs b) =>
  FN (s > TLambda '[a] '[b] > TVector a) (s > TVector b)
map = packFs @a # packFs @b # opRoll @3 # opRoll @3 # mapF

mapF ::
  forall a b s.
  (StackEntry a, StackEntry b) =>
  FN (s > TPackFs a > TPackFs b > TLambda '[a] '[b] > TVector a) (s > TVector b)
mapF =
  function
    ( begin
        # (ns4 @"pfsA" @"pfsB" @"fn" @"vec" # roll @"pfsA" # lambda2 f)
        # (empty # roll @"pfsB" # roll @"fn" # tuple # tuple # roll @"vec")
        # (foldlF # untuple # opDrop)
    )
  where
    f ::
      (StackEntry a, StackEntry b) =>
      FN
        (s > TTuple (TVector b) (TTuple (TPackFs b) (TLambda '[a] '[b])) > a)
        (s > TTuple (TVector b) (TTuple (TPackFs b) (TLambda '[a] '[b])))
    f =
      begin
        # (hide # swap # untuple # untuple # ns4 @"val" @"acc" @"pfs" @"fn")
        # (pick @"pfs" # roll @"acc" # roll @"val" # pick @"fn" # nipHide)
        # (invoke1 # snocF # roll @"pfs" # roll @"fn" # tuple # tuple)

zip ::
  forall a b s.
  (PackFs a, PackFs b) =>
  FN (s > TVector a > TVector b) (s > TVector (TTupleFs a b))
zip = packFs @a # packFs @b # opRoll @3 # opRoll @3 # zipF

zipF ::
  (StackEntry a, StackEntry b) =>
  FN
    (s > TPackFs a > TPackFs b > TVector a > TVector b)
    (s > TVector (TTupleFs a b))
zipF = function (empty # opUntil loop # nip # nip # nip # nip)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop
        ( s
            > TPackFs a
            > TPackFs b
            > TVector a
            > TVector b
            > TVector (TTupleFs a b)
        )
    loop =
      begin
        # ns5 @"pfsA" @"pfsB" @"vecA" @"vecB" @"res"
        # (pick @"pfsA" # pick @"vecA" # unconsF)
        # (pick @"pfsB" # pick @"vecB" # unconsF # op2Dup)
        # (lambdaFst # swap # Maybe.map # swap)
        # (lambdaFst # swap # Maybe.map # swap)
        # (lambda2 tuple # rot # rot # liftA2Maybe)
        # ifJust
          ( begin
              # (del @"vecA" # del @"vecB")
              # (rot # fromJust # snd # rot # fromJust # snd # rot)
              # (pick @"pfsA" # pick @"pfsB" # rot # untuple # TFS.tupleF)
              # (pick @"pfsA" # pick @"pfsB" # calcPackFs)
              # (roll @"res" # rot # snocF)
              # (opFalse # un2 @"pfsA" @"pfsB")
          )
          ( begin
              # (op2Drop # opTrue)
              # un5 @"pfsA" @"pfsB" @"vecA" @"vecB" @"res"
          )

lambdaFst :: (StackEntry a) => FN s (s > TLambda '[TTuple a (TVector a)] '[a])
lambdaFst = lambda1 fst

zipWith ::
  forall a b c s.
  (PackFs a, PackFs b, PackFs c) =>
  FN (s > TLambda '[a, b] '[c] > TVector a > TVector b) (s > TVector c)
zipWith =
  begin
    # (packFs @a # packFs @b # packFs @c)
    # (opRoll @5 # opRoll @5 # opRoll @5 # zipWithF)

type ZipWithFArgs s a b c =
  s
    > TPackFs a
    > TPackFs b
    > TPackFs c
    > TLambda '[a, b] '[c]
    > TVector a
    > TVector b

zipWithF ::
  (StackEntry a, StackEntry b, StackEntry c) =>
  FN (ZipWithFArgs s a b c) (s > TVector c)
zipWithF =
  function
    (empty # opUntil loop # toAlt # op2Drop # op2Drop # op2Drop # fromAlt)
  where
    loop ::
      (StackEntry a, StackEntry b, StackEntry c) =>
      Loop (ZipWithFArgs s a b c > TVector c)
    loop =
      begin
        # ns7 @"pfsA" @"pfsB" @"packFsC" @"fn" @"vecA" @"vecB" @"res"
        # (pick @"pfsA" # pick @"vecA" # unconsF)
        # (pick @"pfsB" # pick @"vecB" # unconsF # op2Dup)
        # (lambdaFst # swap # Maybe.map # swap)
        # (lambdaFst # swap # Maybe.map # swap)
        # (pick @"fn" # rot # rot # liftA2Maybe)
        # ifJust
          ( begin
              # (hide # del @"vecA" # del @"vecB")
              # (rot # fromJust # snd # rot # fromJust # snd # rot)
              # (pick @"packFsC" # roll @"res" # rot # dropHide # snocF)
              # (opFalse # un4 @"pfsA" @"pfsB" @"packFsC" @"fn")
          )
          ( begin
              # (op2Drop # opTrue # un @"pfsA")
              # (un6 @"pfsB" @"packFsC" @"fn" @"vecA" @"vecB" @"res")
          )

unzip ::
  forall a b s.
  (PackFs a, PackFs b) =>
  FN (s > TVector (TTupleFs a b)) (s > TVector a > TVector b)
unzip = packFs @a # packFs @b # op2Dup # calcPackFs # opRoll @3 # unzipF

type UnzipFArgs s a b =
  s
    > TPackFs a
    > TPackFs b
    > TPackFs (TTupleFs a b)
    > TVector (TTupleFs a b)

unzipF ::
  (StackEntry a, StackEntry b) =>
  FN (UnzipFArgs s a b) (s > TVector a > TVector b)
unzipF =
  function
    ( begin
        # (empty # empty # opUntil loop # toAlt # toAlt)
        # (op2Drop # op2Drop # fromAlt # fromAlt)
    )
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (UnzipFArgs s a b > TVector a > TVector b)
    loop =
      begin
        # ns6 @"pfsA" @"pfsB" @"pfsTup" @"vec" @"resA" @"resB"
        # (pick @"pfsTup" # pick @"vec" # unconsF)
        # ifJust
          ( begin
              # del @"vec"
              # (untuple # swap # pick @"pfsA" # pick @"pfsB" # rot)
              # (TFS.untupleF # swap # hide2)
              # (pick @"pfsA" # roll @"resA" # rot # dropHide # snocF # swap)
              # (pick @"pfsB" # roll @"resB" # rot # dropHide # snocF)
              # (opFalse # un3 @"pfsA" @"pfsB" @"pfsTup")
          )
          ( begin
              # opTrue
              # un6 @"pfsA" @"pfsB" @"pfsTup" @"vec" @"resA" @"resB"
          )

-- ## Filtering.
filter ::
  forall a s.
  (PackFs a) =>
  FN (s > TLambda '[a] '[TBool] > TVector a) (s > TVector a)
filter = packFs @a # rot # rot # filterF

filterF ::
  forall a s.
  (StackEntry a) =>
  FN (s > TPackFs a > TLambda '[a] '[TBool] > TVector a) (s > TVector a)
filterF =
  function
    ( begin
        # (ns3 @"packFs" @"fn" @"vec" # pick @"packFs" # lambda2 f)
        # (empty # roll @"packFs" # roll @"fn" # tuple # tuple # roll @"vec")
        # (foldlF # untuple # opDrop)
    )
  where
    f ::
      (StackEntry a) =>
      FN
        ( s
            > TTuple (TVector a) (TTuple (TPackFs a) (TLambda '[a] '[TBool]))
            > a
        )
        (s > TTuple (TVector a) (TTuple (TPackFs a) (TLambda '[a] '[TBool])))
    f =
      begin
        # (hide # swap # untuple # dup # toAlt # untuple)
        # ns4 @"val" @"acc" @"packFs" @"fn"
        # (pick @"val" # roll @"fn" # nipHide # invoke1)
        # opIf
          (pick @"packFs" # roll @"acc" # pick @"val" # dropHide # snocF)
          (roll @"acc")
        # (del @"val" # del @"packFs" # fromAlt # tuple)

-- ## Folding.
foldl ::
  forall a b s.
  (StackEntry a, PackFs b) =>
  FN (s > TLambda '[a, b] '[a] > a > TVector b) (s > a)
foldl =
  begin
    # ns3 @"fn" @"val" @"vec"
    # (packFs @b # roll @"fn" # roll @"val" # hide # roll @"vec" # nipHide)
    # foldlF

foldlF ::
  (StackEntry a, StackEntry b) =>
  FN (s > TPackFs b > TLambda '[a, b] '[a] > a > TVector b) (s > a)
foldlF =
  function (swap # hide # opUntil loop # nip # nip # nip # dropHide)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (s > TPackFs b > TLambda '[a, b] '[a] > TVector b > Hide a)
    loop =
      begin
        # (ns4 @"packFs" @"fn" @"vec" @"acc" # tcPick # pick @"vec" # unconsF)
        # ifJust
          ( begin
              # (del @"vec" # untuple # swap # hide # roll @"acc" # swap)
              # (pick @"fn" # fixup2 # invoke2 # hide # opFalse)
              # un2 @"packFs" @"fn"
          )
          (opTrue # un4 @"packFs" @"fn" @"vec" @"acc")

    fixup2 :: FN (s > Hide a > Hide b > c) (s > a > b > c)
    fixup2 = castStack

-- ## Misc.
-- Used from contexts where it is expected to never fail.
fromJust :: (StackEntry a) => FN (s > TMaybe a) (s > a)
fromJust = err # swap # fromMaybe'
  where
    err = lambda0 (bytes "E0" # opFalse # opVerify # cast)

-- ## Casting.
b2v :: (PackFs a) => FN (s > TBytes) (s > TVector a)
b2v = cast

v2b :: FN (s > TVector a) (s > TBytes)
v2b = cast
