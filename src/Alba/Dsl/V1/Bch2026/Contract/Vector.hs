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
    singletonF,
    replicate,
    replicateF,
    generate,
    generateF,
    iterateN,
    iterateNF,
    unfoldr,
    unfoldrF,
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
    foldr,
    foldrF,
  )
where

import Alba.Dsl.V1.Bch2026
  ( ENV,
    FN,
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
    ifZero,
    invoke1,
    invoke2,
    lambda0,
    lambda1,
    lambda2,
    lambda3,
    lambda4,
    name,
    name2,
    nat,
    ns,
    ns2,
    ns3,
    ns4,
    ns6,
    ns7,
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
    opIf,
    opLessThan,
    opMul,
    opNotIf,
    opRoll,
    opSize,
    opSplit,
    opTrue,
    opUntil,
    opVerify,
    pick,
    pickN,
    roll,
    rollN,
    un,
    un2,
    un3,
    un4,
    un6,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Applicative (liftA2Maybe)
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
    getSize,
    packFs,
    tcDrop,
    tcPick,
    tcRoll,
    tcSize,
    tcUnpack,
  )
import Alba.Dsl.V1.Bch2026.Contract.PartialApplication
  ( apply2,
    apply3,
    apply3_2,
    apply4_2,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (dup, nip, rot, swap)
import Alba.Dsl.V1.Bch2026.Contract.Tuple (TTuple, fst, snd, tuple, untuple)
import Alba.Dsl.V1.Bch2026.Contract.TupleFs (TTupleFs, calcPackFs, tupleF)
import Alba.Dsl.V1.Bch2026.Contract.TupleFs qualified as TFS
import Alba.Dsl.V1.Bch2026.LangArgs (Loop)
import Data.Kind (Type)
import Prelude ()

data TVector (a :: Type)

instance StackEntry (TVector a)

{- ORMOLU_DISABLE -}
type Acc = "acc"
type A = "a"
type B = "b"
type C = "c"
type Cnt = "cnt"
type Fn = "fn"
type Idx = "idx"
type Limit = "limit"
type Pfs = "packFs"
type PfsA = "packFsA"
type PfsB = "packFsB"
type PfsC = "packFsC"
type PfsTup = "packFsTup"
type Res = "res"
type ResA = "resA"
type ResB = "resB"
type Val = "val"
type Val' = "val'"
type Vec = "vec"
type VecA = "vecA"
type VecB = "vecB"
{- ORMOLU_ENABLE -}

-- ## Length.
length :: forall a s. (PackFs a) => FN (s > TVector a) (s > TNat)
length = v2b # opSize # nip # size @a # opDiv

lengthF :: FN (s > TPackFs a > TVector a) (s > TNat)
lengthF =
  function
    ( begin
        # (ns2 Pfs Vec # un Vec)
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
        # (ns3 Pfs Vec Cnt # tcPick # roll Cnt # roll Vec # splitAtF # nip)
        # (tcRoll # swap # headF)
    )

head :: forall a s. (PackFs a) => FN (s > TVector a) (s > TMaybe a)
head = packFs @a # swap # headF

headF :: (StackEntry a) => FN (s > TPackFs a > TVector a) (s > TMaybe a)
headF = function (unconsF # nothing # fstJust # rot # maybe)

-- Save a function slot by factoring out lambda used in more than one place.
fstJust ::
  (StackEntry a, StackEntry b) => FN s (s > TLambda '[TTuple a b] '[TMaybe a])
fstJust = lambda1 (fst # just)

last :: forall a s. (PackFs a) => FN (s > TVector a) (s > TMaybe a)
last = packFs @a # swap # lastF

lastF :: (StackEntry a) => FN (s > TPackFs a > TVector a) (s > TMaybe a)
lastF = function (unsnocF # nothing # sndJust # rot # maybe)

sndJust ::
  (StackEntry a, StackEntry b) => FN s (s > TLambda '[TTuple a b] '[TMaybe b])
sndJust = lambda1 (snd # just)

-- ## Slicing.
init :: forall a s. (PackFs a) => FN (s > TVector a) (s > TMaybe (TVector a))
init = packFs @a # swap # initF

initF ::
  (StackEntry a) => FN (s > TPackFs a > TVector a) (s > TMaybe (TVector a))
initF = function (unsnocF # nothing # fstJust # rot # maybe)

tail :: forall a s. (PackFs a) => FN (s > TVector a) (s > TMaybe (TVector a))
tail = packFs @a # swap # tailF

tailF ::
  (StackEntry a) => FN (s > TPackFs a > TVector a) (s > TMaybe (TVector a))
tailF = function (unconsF # nothing # sndJust # rot # maybe)

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
        # (ns3 Pfs Idx Vec # pick Idx # nat 0 # opEqual)
        # opNotIf
          ( begin
              # (tcPick # pick Vec # lengthF # pick Idx # opGreaterThan)
              # opIf
                (tcRoll # roll Idx # roll Vec # splitAtUnsafeF)
                (roll Vec # empty # delCount 2)
          )
          (empty # roll Vec # delCount 2)
    )

splitAtUnsafeF ::
  FN (s > TPackFs a > TNat > TVector a) (s > TVector a > TVector a)
splitAtUnsafeF = function (v2b # swap # rot # getSize # opMul # opSplit # fixup)
  where
    fixup :: FN (s' > TBytes > TBytes) (s' > TVector a > TVector a)
    fixup = castStack

uncons ::
  forall a s.
  (PackFs a) => FN (s > TVector a) (s > TMaybe (TTuple a (TVector a)))
uncons = packFs @a # swap # unconsF

unconsF ::
  (StackEntry a) =>
  FN (s > TPackFs a > TVector a) (s > TMaybe (TTuple a (TVector a)))
unconsF =
  function
    ( begin
        # (ns2 Pfs Vec # un Vec # dup # null)
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
  (PackFs a) => FN (s > TVector a) (s > TMaybe (TTuple (TVector a) a))
unsnoc = packFs @a # swap # unsnocF

unsnocF ::
  (StackEntry a) =>
  FN (s > TPackFs a > TVector a) (s > TMaybe (TTuple (TVector a) a))
unsnocF =
  function
    ( begin
        # (ns2 Pfs Vec # un Vec # dup # null)
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

replicate :: forall a s. (PackFs a) => ENV (s > TNat > a) (s > TVector a)
replicate = packFs @a # rot # rot # replicateF

replicateF :: (StackEntry a) => ENV (s > TPackFs a > TNat > a) (s > TVector a)
replicateF = function (lambda2 nip # apply2 # generateF)

generate ::
  forall a s.
  (PackFs a) => ENV (s > TNat > TLambda '[TNat] '[a]) (s > TVector a)
generate = packFs @a # rot # rot # generateF

generateF ::
  forall a s.
  (StackEntry a) =>
  ENV (s > TPackFs a > TNat > TLambda '[TNat] '[a]) (s > TVector a)
generateF = function (lambda3 f # apply3_2 # nat 0 # unfoldrF)
  where
    f ::
      (StackEntry a) =>
      FN (s > TNat > TNat > TLambda '[TNat] '[a]) (s > TMaybe (TTuple a TNat))
    f =
      begin
        # (ns3 Cnt Limit Fn # pick Cnt # roll Limit # opLessThan)
        # opIf
          ( begin
              # name A (pick Cnt # roll Fn # invoke1)
              # (roll Cnt # op1Add # un A # tuple # just)
          )
          (delCount 2 # nothing)

iterateN ::
  forall a s.
  (PackFs a) => ENV (s > TNat > TLambda '[a] '[a] > a) (s > TVector a)
iterateN =
  ns3 Cnt Fn Val # (packFs @a # roll Cnt # roll Fn # roll Val # iterateNF)

iterateNF ::
  (StackEntry a) =>
  ENV (s > TPackFs a > TNat > TLambda '[a] '[a] > a) (s > TVector a)
iterateNF = function (swap # lambda2 f # apply2 # rot # rot # tuple # unfoldrF)
  where
    f ::
      (StackEntry a) =>
      FN
        (s > TTuple TNat a > TLambda '[a] '[a])
        (s > TMaybe (TTuple a (TTuple TNat a)))
    f =
      begin
        # (swap # untuple # ns3 Fn Cnt Val # pick Cnt)
        # ifZero
          (delCount 3 # nothing)
          ( begin
              # (name Val' (pick Val) # roll Cnt # op1SubUnsafe # rollN Val)
              # (roll Fn # un Val # invoke1 # tuple # un Val' # tuple # just)
          )

unfoldr ::
  forall a b s.
  (PackFs a, StackEntry b) =>
  FN (s > TLambda '[b] '[TMaybe (TTuple a b)] > b) (s > TVector a)
unfoldr = ns2 Fn Val # packFs @a # roll Fn # roll Val # unfoldrF

unfoldrF ::
  forall a b s.
  (StackEntry a, StackEntry b) =>
  FN (s > TPackFs a > TLambda '[b] '[TMaybe (TTuple a b)] > b) (s > TVector a)
unfoldrF = function (empty # opUntil loop # nip # nip # nip)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (s > TPackFs a > TLambda '[b] '[TMaybe (TTuple a b)] > b > TVector a)
    loop =
      begin
        # ns4 Pfs Fn Val Acc
        # (pickN Val # pick Fn # un Val # invoke1)
        # ifJust
          ( begin
              # (del Val # name2 A B untuple # rollN B # tcPick # roll Acc)
              # (roll A # snocF # un3 Pfs Fn B # opFalse)
          )
          (un4 Pfs Fn Val Acc # opTrue)

-- ## Concatenation.
cons :: forall a s. (PackFs a) => FN (s > a > TVector a) (s > TVector a)
cons = packFs @a # rot # rot # consF

consF :: (StackEntry a) => FN (s > TPackFs a > a > TVector a) (s > TVector a)
consF = function (swap # rot # getPack # invoke1 # swap # v2b # opCat # cast)

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
reverse :: forall a s. (PackFs a) => ENV (s > TVector a) (s > TVector a)
reverse = packFs @a # swap # reverseF

reverseF :: (StackEntry a) => ENV (s > TPackFs a > TVector a) (s > TVector a)
reverseF =
  function
    ( begin
        # (ns2 Pfs Vec # roll Pfs # dup # lambda3 (swap # rot # consF))
        # (apply3 # empty # roll Vec # foldlF)
    )

-- ## Mapping.
map ::
  forall a b s.
  (PackFs a, PackFs b) =>
  ENV (s > TLambda '[a] '[b] > TVector a) (s > TVector b)
map = packFs @a # packFs @b # opRoll 3 # opRoll 3 # mapF

mapF ::
  forall a b s.
  (StackEntry a, StackEntry b) =>
  ENV
    (s > TPackFs a > TPackFs b > TLambda '[a] '[b] > TVector a)
    (s > TVector b)
mapF =
  function
    ( begin
        # (ns4 PfsA PfsB Fn Vec # roll PfsA # roll PfsB # roll Fn # lambda4 f)
        # (apply4_2 # empty # roll Vec # foldlF)
    )
  where
    f ::
      (StackEntry a, StackEntry b) =>
      FN (s > TVector b > a > TPackFs b > TLambda '[a] '[b]) (s > TVector b)
    f =
      begin
        # (ns4 VecB A PfsB Fn # roll PfsB # roll VecB # rollN A # roll Fn)
        # (un A # invoke1 # snocF)

zip ::
  forall a b s.
  (PackFs a, PackFs b) =>
  ENV (s > TVector a > TVector b) (s > TVector (TTupleFs a b))
zip = packFs @a # packFs @b # opRoll 3 # opRoll 3 # zipF

zipF ::
  (StackEntry a, StackEntry b) =>
  ENV
    (s > TPackFs a > TPackFs b > TVector a > TVector b)
    (s > TVector (TTupleFs a b))
zipF =
  function
    ( begin
        # (ns4 PfsA PfsB VecA VecB # pick PfsA # pick PfsB # op2Dup)
        # (calcPackFs # roll PfsA # roll PfsB # lambda4 f # apply4_2)
        # (roll VecA # roll VecB # zipWithF)
    )
  where
    f ::
      (StackEntry a, StackEntry b) =>
      FN (s > a > b > TPackFs a > TPackFs b) (s > TTupleFs a b)
    f =
      begin
        # (ns4 A B PfsA PfsB # roll PfsA # roll PfsB # rollN A # rollN B)
        # (un2 A B # tupleF)

lambdaFst :: (StackEntry a) => FN s (s > TLambda '[TTuple a (TVector a)] '[a])
lambdaFst = lambda1 fst

zipWith ::
  forall a b c s.
  (PackFs a, PackFs b, PackFs c) =>
  FN (s > TLambda '[a, b] '[c] > TVector a > TVector b) (s > TVector c)
zipWith =
  begin
    # (packFs @a # packFs @b # packFs @c)
    # (opRoll 5 # opRoll 5 # opRoll 5 # zipWithF)

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
zipWithF = function (empty # opUntil loop # nip # nip # nip # nip # nip # nip)
  where
    loop ::
      (StackEntry a, StackEntry b, StackEntry c) =>
      Loop (ZipWithFArgs s a b c > TVector c)
    loop =
      begin
        # ns7 PfsA PfsB PfsC Fn VecA VecB Res
        # (pick PfsA # pick VecA # unconsF)
        # (pick PfsB # pick VecB # unconsF # op2Dup)
        # (lambdaFst # swap # Maybe.map # swap)
        # (lambdaFst # swap # Maybe.map # swap)
        # (pick Fn # rot # rot # liftA2Maybe)
        # ifJust
          ( begin
              # (ns C # del VecA # del VecB)
              # (rot # fromJust # snd # rot # fromJust # snd # rot)
              # (pick PfsC # roll Res # rot # un C # snocF)
              # (opFalse # un4 PfsA PfsB PfsC Fn)
          )
          ( begin
              # (op2Drop # opTrue # un PfsA)
              # (un6 PfsB PfsC Fn VecA VecB Res)
          )

unzip ::
  forall a b s.
  (PackFs a, PackFs b) =>
  FN (s > TVector (TTupleFs a b)) (s > TVector a > TVector b)
unzip = packFs @a # packFs @b # op2Dup # calcPackFs # opRoll 3 # unzipF

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
    (empty # empty # opUntil loop # rotDrop # rotDrop # rotDrop # rotDrop)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (UnzipFArgs s a b > TVector a > TVector b)
    loop =
      begin
        # ns6 PfsA PfsB PfsTup Vec ResA ResB
        # (pick PfsTup # pick Vec # unconsF)
        # ifJust
          ( begin
              # del Vec
              # (untuple # swap # pick PfsA # pick PfsB # rot)
              # (TFS.untupleF # swap # ns2 B A)
              # (pick PfsA # roll ResA # rot # un A # snocF # swap)
              # (pick PfsB # roll ResB # rot # un B # snocF)
              # (opFalse # un3 PfsA PfsB PfsTup)
          )
          (opTrue # un6 PfsA PfsB PfsTup Vec ResA ResB)

    rotDrop ::
      (StackEntry a, StackEntry b, StackEntry c) =>
      FN (s > a > b > c) (s > b > c)
    rotDrop = rot # opDrop

-- ## Filtering.
filter ::
  forall a s.
  (PackFs a) =>
  ENV (s > TLambda '[a] '[TBool] > TVector a) (s > TVector a)
filter = packFs @a # rot # rot # filterF

filterF ::
  forall a s.
  (StackEntry a) =>
  ENV (s > TPackFs a > TLambda '[a] '[TBool] > TVector a) (s > TVector a)
filterF =
  function
    ( begin
        # ns3 Pfs Fn Vec
        # (pick Pfs # (roll Pfs # roll Fn # lambda4 f # apply4_2) # empty)
        # (roll Vec # foldlF)
    )
  where
    f ::
      (StackEntry a) =>
      FN (s > TVector a > a > TPackFs a > TLambda '[a] '[TBool]) (s > TVector a)
    f =
      begin
        # (ns4 Acc Val Pfs Fn # pickN Val # roll Fn # un Val # invoke1)
        # opIf (pick Pfs # roll Acc # pick Val # snocF) (roll Acc)
        # (del Val # del Pfs)

-- ## Folding.
foldl ::
  forall a b s.
  (StackEntry b, PackFs a) =>
  FN (s > TLambda '[b, a] '[b] > b > TVector a) (s > b)
foldl =
  ns3 Fn Val Vec # packFs @a # roll Fn # rollN Val # roll Vec # un Val # foldlF

foldlF ::
  (StackEntry a, StackEntry b) =>
  FN (s > TPackFs a > TLambda '[b, a] '[b] > b > TVector a) (s > b)
foldlF = function (swap # opUntil loop # nip # nip # nip)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (s > TPackFs a > TLambda '[b, a] '[b] > TVector a > b)
    loop =
      begin
        # (ns4 Pfs Fn Vec Acc # tcPick # pick Vec # unconsF)
        # ifJust
          ( begin
              # (del Vec # untuple # swap # ns A # rollN Acc # swap)
              # (pick Fn # un2 Acc A # invoke2 # ns Acc # opFalse)
              # un3 Pfs Fn Acc
          )
          (opTrue # un4 Pfs Fn Vec Acc)

foldr ::
  forall a b s.
  (StackEntry b, PackFs a) =>
  FN (s > TLambda '[a, b] '[b] > b > TVector a) (s > b)
foldr =
  ns3 Fn Val Vec # packFs @a # roll Fn # rollN Val # roll Vec # un Val # foldrF

foldrF ::
  (StackEntry a, StackEntry b) =>
  FN (s > TPackFs a > TLambda '[a, b] '[b] > b > TVector a) (s > b)
foldrF = function (swap # opUntil loop # nip # nip # nip)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (s > TPackFs a > TLambda '[a, b] '[b] > TVector a > b)
    loop =
      begin
        # (ns4 Pfs Fn Vec Acc # tcPick # pick Vec # unsnocF)
        # ifJust
          ( begin
              # (del Vec # untuple # ns A # rollN Acc)
              # (pick Fn # un2 A Acc # invoke2 # ns Acc # opFalse)
              # un3 Pfs Fn Acc
          )
          (opTrue # un4 Pfs Fn Vec Acc)

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
