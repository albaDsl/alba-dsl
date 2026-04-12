-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TVector
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
  ( Env,
    Fn,
    StackEntry,
    TBool,
    TBytes,
    TLambda,
    TNat,
    begin,
    bytes,
    cast,
    del,
    delCount,
    fn,
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
    op2Drop,
    op2Dup,
    opCat,
    opDiv,
    opDrop,
    opEqual,
    opFalse,
    opIf,
    opLessThan,
    opNotIf,
    opRoll,
    opSize,
    opSplit,
    opTrue,
    opUntil,
    pick,
    pickN,
    roll,
    rollN,
    un,
    un2,
    un3,
    un4,
    un6,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (..),
    Integral (add1, mul),
    Ord (..),
    PackFs (..),
    TMaybe,
    TPackFs,
    TTuple,
    apply2,
    apply3,
    apply3_2,
    apply4_2,
    blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
    dup,
    errCanNotHappen,
    fromMaybe',
    fst,
    getPack,
    getSize,
    ifJust,
    ifZero,
    just,
    liftA2Maybe,
    maybe,
    nat1SubUnsafe,
    nip,
    nothing,
    rot,
    snd,
    swap,
    tcDrop,
    tcPick,
    tcRoll,
    tcSize,
    tcUnpack,
    tuple,
    untuple,
  )
import Alba.Dsl.V1.Bch2026.Contract.TMaybe qualified as Maybe
import Alba.Dsl.V1.Bch2026.Contract.TTupleFs (TTupleFs, calcPackFs, tupleF)
import Alba.Dsl.V1.Bch2026.Contract.TTupleFs qualified as TFS
import Alba.Dsl.V1.Bch2026.LangArgs (Loop)
import Data.Kind (Type)
import Prelude ()

data TVector (a :: Type)

instance StackEntry (TVector a)

instance (BlobEq a) => BlobEq (TVector a) where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

-- ## Length.
length :: forall a s. (PackFs a) => Fn (s > TVector a) (s > TNat)
length = toRaw . opSize . nip . size @a . opDiv

lengthF :: Fn (s > TPackFs a > TVector a) (s > TNat)
lengthF =
  fn
    ( begin
        . (ns2 #packFs #vec . un #vec)
        . (toRaw . opSize . nip . tcSize . opDiv . tcDrop)
    )

null :: Fn (s > TVector a) (s > TBool)
null = toRaw . bytes [] . opEqual

-- ## Indexing.
lookup :: forall a s. (PackFs a) => Fn (s > TVector a > TNat) (s > TMaybe a)
lookup = packFsRec @a . rot . rot . lookupF

lookupF ::
  (StackEntry a) => Fn (s > TPackFs a > TVector a > TNat) (s > TMaybe a)
lookupF =
  fn
    ( begin
        . (ns3 #packFs #vec #cnt . tcPick . roll #cnt . roll #vec . splitAtF)
        . (nip . tcRoll . swap . headF)
    )

head :: forall a s. (PackFs a) => Fn (s > TVector a) (s > TMaybe a)
head = packFsRec @a . swap . headF

headF :: (StackEntry a) => Fn (s > TPackFs a > TVector a) (s > TMaybe a)
headF = fn (unconsF . nothing . fstJust . rot . maybe)

-- Save a fn slot by factoring out lambda used in more than one place.
fstJust ::
  (StackEntry a, StackEntry b) => Fn s (s > TLambda '[TTuple a b] '[TMaybe a])
fstJust = lambda1 (fst . just)

last :: forall a s. (PackFs a) => Fn (s > TVector a) (s > TMaybe a)
last = packFsRec @a . swap . lastF

lastF :: (StackEntry a) => Fn (s > TPackFs a > TVector a) (s > TMaybe a)
lastF = fn (unsnocF . nothing . sndJust . rot . maybe)

sndJust ::
  (StackEntry a, StackEntry b) => Fn s (s > TLambda '[TTuple a b] '[TMaybe b])
sndJust = lambda1 (snd . just)

-- ## Slicing.
init :: forall a s. (PackFs a) => Fn (s > TVector a) (s > TMaybe (TVector a))
init = packFsRec @a . swap . initF

initF ::
  (StackEntry a) => Fn (s > TPackFs a > TVector a) (s > TMaybe (TVector a))
initF = fn (unsnocF . nothing . fstJust . rot . maybe)

tail :: forall a s. (PackFs a) => Fn (s > TVector a) (s > TMaybe (TVector a))
tail = packFsRec @a . swap . tailF

tailF ::
  (StackEntry a) => Fn (s > TPackFs a > TVector a) (s > TMaybe (TVector a))
tailF = fn (unconsF . nothing . sndJust . rot . maybe)

take :: (PackFs a) => Fn (s > TNat > TVector a) (s > TVector a)
take = splitAt . opDrop

drop :: (PackFs a) => Fn (s > TNat > TVector a) (s > TVector a)
drop = splitAt . nip

splitAt ::
  forall a s.
  (PackFs a) =>
  Fn (s > TNat > TVector a) (s > TVector a > TVector a)
splitAt = packFsRec @a . rot . rot . splitAtF

splitAtF :: Fn (s > TPackFs a > TNat > TVector a) (s > TVector a > TVector a)
splitAtF =
  fn
    ( begin
        . (ns3 #packFs #idx #vec . pick #idx . nat 0 . equal)
        . opNotIf
          ( begin
              . (tcPick . pick #vec . lengthF . pick #idx . greaterThan)
              . opIf
                (tcRoll . roll #idx . roll #vec . splitAtUnsafeF)
                (roll #vec . empty . delCount 2)
          )
          (empty . roll #vec . delCount 2)
    )

splitAtUnsafeF ::
  Fn (s > TPackFs a > TNat > TVector a) (s > TVector a > TVector a)
splitAtUnsafeF = fn (toRaw . swap . rot . getSize . mul . opSplit . fixup)
  where
    -- Optimizer will take care of redundant swaps.
    fixup :: Fn (s' > TBytes > TBytes) (s' > TVector a > TVector a)
    fixup = fromRaw . swap . fromRaw . swap

uncons ::
  forall a s.
  (PackFs a) => Fn (s > TVector a) (s > TMaybe (TTuple a (TVector a)))
uncons = packFsRec @a . swap . unconsF

unconsF ::
  (StackEntry a) =>
  Fn (s > TPackFs a > TVector a) (s > TMaybe (TTuple a (TVector a)))
unconsF =
  fn
    ( begin
        . (ns2 #packFs #vec . un #vec . dup . null)
        . opNotIf
          ( begin
              . (nat 1 . swap . tcPick . rot . rot . splitAtUnsafeF . swap)
              . (toRaw . tcUnpack . swap . tuple . just)
          )
          (opDrop . nothing)
        . tcDrop
    )

unsnoc ::
  forall a s.
  (PackFs a) => Fn (s > TVector a) (s > TMaybe (TTuple (TVector a) a))
unsnoc = packFsRec @a . swap . unsnocF

unsnocF ::
  (StackEntry a) =>
  Fn (s > TPackFs a > TVector a) (s > TMaybe (TTuple (TVector a) a))
unsnocF =
  fn
    ( begin
        . (ns2 #packFs #vec . un #vec . dup . null)
        . opNotIf
          ( begin
              . (dup . tcPick . swap . lengthF . nat1SubUnsafe . swap)
              . (tcPick . rot . rot . splitAtUnsafeF . toRaw . tcUnpack . tuple)
              . just
          )
          (opDrop . nothing)
        . tcDrop
    )

-- ## Construction.
empty :: Fn s (s > TVector a)
empty = bytes [] . fromRaw

singleton :: (PackFs a) => Fn (s > a) (s > TVector a)
singleton = pack . fromRaw

singletonF :: (StackEntry a) => Fn (s > TPackFs a > a) (s > TVector a)
singletonF = fn (swap . getPack . invoke1 . fromRaw)

replicate :: forall a s. (PackFs a) => Env (s > TNat > a) (s > TVector a)
replicate = packFsRec @a . rot . rot . replicateF

replicateF :: (StackEntry a) => Env (s > TPackFs a > TNat > a) (s > TVector a)
replicateF = fn (lambda2 nip . apply2 . generateF)

generate ::
  forall a s.
  (PackFs a) => Env (s > TNat > TLambda '[TNat] '[a]) (s > TVector a)
generate = packFsRec @a . rot . rot . generateF

generateF ::
  forall a s.
  (StackEntry a) =>
  Env (s > TPackFs a > TNat > TLambda '[TNat] '[a]) (s > TVector a)
generateF = fn (lambda3 f . apply3_2 . nat 0 . unfoldrF)
  where
    f ::
      (StackEntry a) =>
      Fn (s > TNat > TNat > TLambda '[TNat] '[a]) (s > TMaybe (TTuple a TNat))
    f =
      begin
        . (ns3 #cnt #limit #f . pick #cnt . roll #limit . opLessThan)
        . opIf
          ( begin
              . name #a (pick #cnt . roll #f . invoke1)
              . (roll #cnt . add1 . un #a . tuple . just)
          )
          (delCount 2 . nothing)

iterateN ::
  forall a s.
  (PackFs a) => Env (s > TNat > TLambda '[a] '[a] > a) (s > TVector a)
iterateN =
  begin
    . ns3 #cnt #f #val
    . (packFsRec @a . roll #cnt . roll #f . roll #val . iterateNF)

iterateNF ::
  (StackEntry a) =>
  Env (s > TPackFs a > TNat > TLambda '[a] '[a] > a) (s > TVector a)
iterateNF = fn (swap . lambda2 f . apply2 . rot . rot . tuple . unfoldrF)
  where
    f ::
      (StackEntry a) =>
      Fn
        (s > TTuple TNat a > TLambda '[a] '[a])
        (s > TMaybe (TTuple a (TTuple TNat a)))
    f =
      begin
        . (swap . untuple . ns3 #f #cnt #val . pick #cnt)
        . ifZero
          (delCount 3 . nothing)
          ( begin
              . (name #val' (pick #val) . roll #cnt . nat1SubUnsafe)
              . (rollN #val . roll #f . un #val . invoke1 . tuple . un #val')
              . (tuple . just)
          )

unfoldr ::
  forall a b s.
  (PackFs a, StackEntry b) =>
  Fn (s > TLambda '[b] '[TMaybe (TTuple a b)] > b) (s > TVector a)
unfoldr = ns2 #f #val . packFsRec @a . roll #f . roll #val . unfoldrF

unfoldrF ::
  forall a b s.
  (StackEntry a, StackEntry b) =>
  Fn (s > TPackFs a > TLambda '[b] '[TMaybe (TTuple a b)] > b) (s > TVector a)
unfoldrF = fn (empty . opUntil loop . nip . nip . nip)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (s > TPackFs a > TLambda '[b] '[TMaybe (TTuple a b)] > b > TVector a)
    loop =
      begin
        . ns4 #packFs #f #val #acc
        . (pickN #val . pick #f . un #val . invoke1)
        . ifJust
          ( begin
              . (del #val . name2 #a #b untuple . rollN #b . tcPick . roll #acc)
              . (roll #a . snocF . un3 #packFs #f #b . opFalse)
          )
          (un4 #packFs #f #val #acc . opTrue)

-- ## Concatenation.
cons :: forall a s. (PackFs a) => Fn (s > a > TVector a) (s > TVector a)
cons = packFsRec @a . rot . rot . consF

consF :: (StackEntry a) => Fn (s > TPackFs a > a > TVector a) (s > TVector a)
consF = fn (swap . rot . getPack . invoke1 . swap . toRaw . opCat . fromRaw)

snoc :: forall a s. (PackFs a) => Fn (s > TVector a > a) (s > TVector a)
snoc = packFsRec @a . rot . rot . snocF

snocF :: (StackEntry a) => Fn (s > TPackFs a > TVector a > a) (s > TVector a)
snocF = fn (fixup . rot . getPack . invoke1 . opCat . fromRaw)
  where
    -- Optimizer will take care of redundant swaps.
    fixup :: (StackEntry a) => Fn (s > TVector a > a) (s > TBytes > a)
    fixup = swap . toRaw . swap

append :: Fn (s > TVector a > TVector a) (s > TVector a)
append = fixup . opCat . fromRaw
  where
    -- Optimizer will take care of redundant swaps.
    fixup :: Fn (s > TVector a > TVector a) (s > TBytes > TBytes)
    fixup = toRaw . swap . toRaw . swap

-- ## Permutation.
reverse :: forall a s. (PackFs a) => Env (s > TVector a) (s > TVector a)
reverse = packFsRec @a . swap . reverseF

reverseF :: (StackEntry a) => Env (s > TPackFs a > TVector a) (s > TVector a)
reverseF =
  fn
    ( begin
        . (ns2 #packFs #vec . roll #packFs . dup . lambda3 (swap . rot . consF))
        . (apply3 . empty . roll #vec . foldlF)
    )

-- ## Mapping.
map ::
  forall a b s.
  (PackFs a, PackFs b) =>
  Env (s > TLambda '[a] '[b] > TVector a) (s > TVector b)
map = packFsRec @a . packFsRec @b . opRoll 3 . opRoll 3 . mapF

mapF ::
  forall a b s.
  (StackEntry a, StackEntry b) =>
  Env
    (s > TPackFs a > TPackFs b > TLambda '[a] '[b] > TVector a)
    (s > TVector b)
mapF =
  fn
    ( begin
        . (ns4 #pfsA #pfsB #f #vec . roll #pfsA . roll #pfsB . roll #f)
        . (lambda4 f . apply4_2 . empty . roll #vec . foldlF)
    )
  where
    f ::
      (StackEntry a, StackEntry b) =>
      Fn (s > TVector b > a > TPackFs b > TLambda '[a] '[b]) (s > TVector b)
    f =
      begin
        . (ns4 #vecB #a #pfsB #f . roll #pfsB . roll #vecB . rollN #a . roll #f)
        . (un #a . invoke1 . snocF)

zip ::
  forall a b s.
  (PackFs a, PackFs b) =>
  Env (s > TVector a > TVector b) (s > TVector (TTupleFs a b))
zip = packFsRec @a . packFsRec @b . opRoll 3 . opRoll 3 . zipF

zipF ::
  (StackEntry a, StackEntry b) =>
  Env
    (s > TPackFs a > TPackFs b > TVector a > TVector b)
    (s > TVector (TTupleFs a b))
zipF =
  fn
    ( begin
        . (ns4 #pfsA #pfsB #vecA #vecB . pick #pfsA . pick #pfsB . op2Dup)
        . (calcPackFs . roll #pfsA . roll #pfsB . lambda4 f . apply4_2)
        . (roll #vecA . roll #vecB . zipWithF)
    )
  where
    f ::
      (StackEntry a, StackEntry b) =>
      Fn (s > a > b > TPackFs a > TPackFs b) (s > TTupleFs a b)
    f =
      begin
        . (ns4 #a #b #pfsA #pfsB . roll #pfsA . roll #pfsB . rollN #a)
        . (rollN #b . un2 #a #b . tupleF)

lambdaFst :: (StackEntry a) => Fn s (s > TLambda '[TTuple a (TVector a)] '[a])
lambdaFst = lambda1 fst

zipWith ::
  forall a b c s.
  (PackFs a, PackFs b, PackFs c) =>
  Fn (s > TLambda '[a, b] '[c] > TVector a > TVector b) (s > TVector c)
zipWith =
  begin
    . (packFsRec @a . packFsRec @b . packFsRec @c)
    . (opRoll 5 . opRoll 5 . opRoll 5 . zipWithF)

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
  Fn (ZipWithFArgs s a b c) (s > TVector c)
zipWithF = fn (empty . opUntil loop . nip . nip . nip . nip . nip . nip)
  where
    loop ::
      (StackEntry a, StackEntry b, StackEntry c) =>
      Loop (ZipWithFArgs s a b c > TVector c)
    loop =
      begin
        . ns7 #pfsA #pfsB #pfsC #f #vecA #vecB #res
        . (pick #pfsA . pick #vecA . unconsF)
        . (pick #pfsB . pick #vecB . unconsF . op2Dup)
        . (lambdaFst . swap . Maybe.map . swap)
        . (lambdaFst . swap . Maybe.map . swap)
        . (pick #f . rot . rot . liftA2Maybe)
        . ifJust
          ( begin
              . (ns #c . del #vecA . del #vecB)
              . (rot . fromJust . snd . rot . fromJust . snd . rot)
              . (pick #pfsC . roll #res . rot . un #c . snocF)
              . (opFalse . un4 #pfsA #pfsB #pfsC #f)
          )
          ( begin
              . (op2Drop . opTrue . un #pfsA)
              . (un6 #pfsB #pfsC #f #vecA #vecB #res)
          )

unzip ::
  forall a b s.
  (PackFs a, PackFs b) =>
  Fn (s > TVector (TTupleFs a b)) (s > TVector a > TVector b)
unzip = packFsRec @a . packFsRec @b . op2Dup . calcPackFs . opRoll 3 . unzipF

type UnzipFArgs s a b =
  s
    > TPackFs a
    > TPackFs b
    > TPackFs (TTupleFs a b)
    > TVector (TTupleFs a b)

unzipF ::
  (StackEntry a, StackEntry b) =>
  Fn (UnzipFArgs s a b) (s > TVector a > TVector b)
unzipF =
  fn
    (empty . empty . opUntil loop . rotDrop . rotDrop . rotDrop . rotDrop)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (UnzipFArgs s a b > TVector a > TVector b)
    loop =
      begin
        . ns6 #pfsA #pfsB #packFsTup #vec #resA #resB
        . (pick #packFsTup . pick #vec . unconsF)
        . ifJust
          ( begin
              . del #vec
              . (untuple . swap . pick #pfsA . pick #pfsB . rot)
              . (TFS.untupleF . swap . ns2 #b #a)
              . (pick #pfsA . roll #resA . rot . un #a . snocF . swap)
              . (pick #pfsB . roll #resB . rot . un #b . snocF)
              . (opFalse . un3 #pfsA #pfsB #packFsTup)
          )
          (opTrue . un6 #pfsA #pfsB #packFsTup #vec #resA #resB)

    rotDrop ::
      (StackEntry a, StackEntry b, StackEntry c) =>
      Fn (s > a > b > c) (s > b > c)
    rotDrop = rot . opDrop

-- ## Filtering.
filter ::
  forall a s.
  (PackFs a) =>
  Env (s > TLambda '[a] '[TBool] > TVector a) (s > TVector a)
filter = packFsRec @a . rot . rot . filterF

filterF ::
  forall a s.
  (StackEntry a) =>
  Env (s > TPackFs a > TLambda '[a] '[TBool] > TVector a) (s > TVector a)
filterF =
  fn
    ( begin
        . ns3 #packFs #f #vec
        . (pick #packFs . (roll #packFs . roll #f . lambda4 f . apply4_2))
        . (empty . roll #vec . foldlF)
    )
  where
    f ::
      (StackEntry a) =>
      Fn (s > TVector a > a > TPackFs a > TLambda '[a] '[TBool]) (s > TVector a)
    f =
      begin
        . (ns4 #acc #val #packFs #f . pickN #val . roll #f . un #val . invoke1)
        . opIf (pick #packFs . roll #acc . pick #val . snocF) (roll #acc)
        . (del #val . del #packFs)

-- ## Folding.
foldl ::
  forall a b s.
  (StackEntry b, PackFs a) =>
  Fn (s > TLambda '[b, a] '[b] > b > TVector a) (s > b)
foldl =
  begin
    . (ns3 #f #val #vec . packFsRec @a . roll #f . rollN #val . roll #vec)
    . (un #val . foldlF)

foldlF ::
  (StackEntry a, StackEntry b) =>
  Fn (s > TPackFs a > TLambda '[b, a] '[b] > b > TVector a) (s > b)
foldlF = fn (swap . opUntil loop . nip . nip . nip)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (s > TPackFs a > TLambda '[b, a] '[b] > TVector a > b)
    loop =
      begin
        . (ns4 #packFs #f #vec #acc . tcPick . pick #vec . unconsF)
        . ifJust
          ( begin
              . (del #vec . untuple . swap . ns #a . rollN #acc . swap)
              . (pick #f . un2 #acc #a . invoke2 . ns #acc . opFalse)
              . un3 #packFs #f #acc
          )
          (opTrue . un4 #packFs #f #vec #acc)

foldr ::
  forall a b s.
  (StackEntry b, PackFs a) =>
  Fn (s > TLambda '[a, b] '[b] > b > TVector a) (s > b)
foldr =
  begin
    . (ns3 #f #val #vec . packFsRec @a . roll #f . rollN #val . roll #vec)
    . (un #val . foldrF)

foldrF ::
  (StackEntry a, StackEntry b) =>
  Fn (s > TPackFs a > TLambda '[a, b] '[b] > b > TVector a) (s > b)
foldrF = fn (swap . opUntil loop . nip . nip . nip)
  where
    loop ::
      (StackEntry a, StackEntry b) =>
      Loop (s > TPackFs a > TLambda '[a, b] '[b] > TVector a > b)
    loop =
      begin
        . (ns4 #packFs #f #vec #acc . tcPick . pick #vec . unsnocF)
        . ifJust
          ( begin
              . (del #vec . untuple . ns #a . rollN #acc)
              . (pick #f . un2 #a #acc . invoke2 . ns #acc . opFalse)
              . un3 #packFs #f #acc
          )
          (opTrue . un4 #packFs #f #vec #acc)

-- ## Misc.
-- Used from contexts where it is expected to never fail.
fromJust :: (StackEntry a) => Fn (s > TMaybe a) (s > a)
fromJust = lambda0 (errCanNotHappen) . swap . fromMaybe'

-- ## Casting.
fromRaw :: Fn (s > TBytes) (s > TVector a)
fromRaw = cast

toRaw :: Fn (s > TVector a) (s > TBytes)
toRaw = cast
