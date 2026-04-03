-- Copyright (c) 2025 albaDsl

module TestLibVector (testLibVector) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Applicative (liftA2Maybe)
import Alba.Dsl.V1.Bch2026.Contract.BlobEq (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.Bytes128
  ( TBytes128,
    bytes128,
    toBytes,
    toBytes128,
  )
import Alba.Dsl.V1.Bch2026.Contract.Int64 (TInt64, int64)
import Alba.Dsl.V1.Bch2026.Contract.Int8 (TInt8, int8)
import Alba.Dsl.V1.Bch2026.Contract.Maybe (TMaybe, fromMaybe', just, nothing)
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (packFsRec))
import Alba.Dsl.V1.Bch2026.Contract.Tuple (fst, snd, tuple)
import Alba.Dsl.V1.Bch2026.Contract.TupleFs qualified as TFS
import Alba.Dsl.V1.Bch2026.Contract.Vector qualified as V
import Numeric.Natural (Natural)
import QuickCheckSupport (BytesSize (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Property, testProperty, (==>))
import TestUtils2026 (evaluateProg, isTrue, isTrue')
import Prelude hiding (drop, fst, snd, sum)

testLibVector :: TestTree
testLibVector =
  testGroup
    "Vectors"
    [ testCase "Length" $ do isTrue (evaluateProg progLength),
      testCase "Indexing" $ do isTrue (evaluateProg progIndexing),
      testCase "Slicing" $ do isTrue (evaluateProg progSlicing),
      testCase "Construction" $ do isTrue (evaluateProg progConstruction),
      testCase "Concatenation" $ do isTrue (evaluateProg progConcatenation),
      testCase "Permutation" $ do isTrue (evaluateProg progPermutation),
      testCase "Mapping" $ do isTrue (evaluateProg progMapping),
      testCase "Zipping" $ do isTrue (evaluateProg progZipping),
      testCase "Filtering" $ do isTrue (evaluateProg progFiltering),
      testProperty "reverse" propReverse,
      testProperty "length" propLength,
      testProperty "lookup" propLookup,
      testProperty "cons snoc append" propConsSnocAppend,
      testProperty "head tail uncons" propHeadTailUncons,
      testProperty "last init unsnoc" propLastInitUnsnoc,
      testProperty "take drop splitAt" propTakeDropSplitAt,
      testProperty "zip unzip" propZipUnzip,
      testProperty "zipWith unzip" propZipWithUnzip,
      testProperty "filter: keep all" propFilterKeepAll,
      testProperty "filter: keep none" propFilterKeepNone,
      testProperty "map composed functions" propMapComposition,
      testProperty "map identity" propMapIdentity,
      testProperty "folding" propFolding
    ]

progLength :: Fn s (s > TBool)
progLength =
  begin
    # (int64Vector # V.length # nat 3 # equalVerify)
    # (int8Vector # V.length # nat 3 # equalVerify)
    # (bytes128Vector # V.length # nat 3 # equalVerify)
    # (int64Vector # V.null # opFalse # equalVerify)
    # (int8Vector # V.null # opFalse # equalVerify)
    # (bytes128Vector # V.null # opFalse # equalVerify)
    # opTrue

progIndexing :: Fn s (s > TBool)
progIndexing =
  begin
    # (int64Vector # V.last # fromJust # int64 2 # equalVerify)
    # (int8Vector # V.last # fromJust # int8 2 # equalVerify)
    # (bytes128Vector # V.last # fromJust # bytes128 b2 # equalVerify)
    # (int64Vector # V.head # fromJust # int64 0 # equalVerify)
    # (int8Vector # V.head # fromJust # int8 0 # equalVerify)
    # (bytes128Vector # V.head # fromJust # bytes128 b0 # equalVerify)
    # (int64Vector # nat 0 # V.lookup # fromJust # int64 0 # equalVerify)
    # (int64Vector # nat 1 # V.lookup # fromJust # int64 1 # equalVerify)
    # (int64Vector # nat 2 # V.lookup # fromJust # int64 2 # equalVerify)
    # (int64Vector # nat 3 # V.lookup # nothing # equalVerify)
    # (int8Vector # nat 0 # V.lookup # fromJust # int8 0 # equalVerify)
    # (int8Vector # nat 1 # V.lookup # fromJust # int8 1 # equalVerify)
    # (int8Vector # nat 2 # V.lookup # fromJust # int8 2 # equalVerify)
    # (int8Vector # nat 3 # V.lookup # nothing # equalVerify)
    # ( begin
          # (bytes128Vector # nat 0 # V.lookup # fromJust # bytes128 b0)
          # equalVerify
      )
    # ( begin
          # (bytes128Vector # nat 1 # V.lookup # fromJust # bytes128 b1)
          # equalVerify
      )
    # ( begin
          # (bytes128Vector # nat 2 # V.lookup # fromJust # bytes128 b2)
          # equalVerify
      )
    # (bytes128Vector # nat 3 # V.lookup # nothing # equalVerify)
    # opTrue

progSlicing :: Fn s (s > TBool)
progSlicing =
  begin
    # (int64Vector # testUncons int64)
    # (int8Vector # testUncons int8)
    # ( begin
          # (int64Vector # V.unsnoc # fromJust # snd)
          # int64 2
          # equalVerify
      )
    # ( begin
          # (int8Vector # V.unsnoc # fromJust # snd)
          # int8 2
          # equalVerify
      )
    # ( begin
          # (bytes128Vector # V.unsnoc # fromJust # snd)
          # bytes128 b2
          # equalVerify
      )
    # ( begin
          # (int64Vector # V.init # fromJust)
          # (int64 0 # int64 1 # V.empty # V.cons # V.cons)
          # equalVerify
      )
    # ( begin
          # (int8Vector # V.init # fromJust)
          # (int8 0 # int8 1 # V.empty # V.cons # V.cons)
          # equalVerify
      )
    # ( begin
          # (bytes128Vector # V.init # fromJust)
          # (bytes128 b0 # bytes128 b1 # V.empty # V.cons # V.cons)
          # equalVerify
      )
    # ( begin
          # (int64Vector # V.tail # fromJust)
          # (int64 1 # int64 2 # V.empty # V.cons # V.cons)
          # equalVerify
      )
    # ( begin
          # (int8Vector # V.tail # fromJust)
          # (int8 1 # int8 2 # V.empty # V.cons # V.cons)
          # equalVerify
      )
    # ( begin
          # (bytes128Vector # V.tail # fromJust)
          # (bytes128 b1 # bytes128 b2 # V.empty # V.cons # V.cons)
          # equalVerify
      )
    # ( begin
          # (nat 2 # int64Vector # V.take)
          # (int64 0 # int64 1 # V.empty # V.cons # V.cons)
          # equalVerify
      )
    # ( begin
          # (nat 2 # int8Vector # V.take)
          # (int8 0 # int8 1 # V.empty # V.cons # V.cons)
          # equalVerify
      )
    # ( begin
          # (nat 2 # bytes128Vector # V.take)
          # (bytes128 b0 # bytes128 b1 # V.empty # V.cons # V.cons)
          # equalVerify
      )
    # ( begin
          # (nat 2 # int64Vector # V.drop)
          # (int64 2 # V.empty # V.cons)
          # equalVerify
      )
    # ( begin
          # (nat 2 # int8Vector # V.drop)
          # (int8 2 # V.empty # V.cons)
          # equalVerify
      )
    # ( begin
          # (nat 2 # bytes128Vector # V.drop)
          # (bytes128 b2 # V.empty # V.cons)
          # equalVerify
      )
    # ( begin
          # (nat 2 # int64Vector # V.splitAt)
          # (opNip # int64 2 # V.singleton)
          # equalVerify
      )
    # ( begin
          # (nat 2 # int8Vector # V.splitAt)
          # (opNip # int8 2 # V.singleton)
          # equalVerify
      )
    # ( begin
          # (nat 2 # bytes128Vector # V.splitAt)
          # (opNip # bytes128 b2 # V.singleton)
          # equalVerify
      )
    # ( begin
          # (nat 10 # int64Vector # V.splitAt)
          # (opDrop # int64Vector # equalVerify)
      )
    # ( begin
          # (nat 10 # int8Vector # V.splitAt)
          # (opDrop # int8Vector # equalVerify)
      )
    # ( begin
          # (nat 10 # bytes128Vector # V.splitAt)
          # (opDrop # bytes128Vector # equalVerify)
      )
    # ( begin
          # (nat 0 # int64Vector # V.splitAt)
          # (opNip # int64Vector # equalVerify)
      )
    # ( begin
          # (nat 0 # int8Vector # V.splitAt)
          # (opNip # int8Vector # equalVerify)
      )
    # ( begin
          # (nat 0 # bytes128Vector # V.splitAt)
          # (opNip # bytes128Vector # equalVerify)
      )
    # opTrue
  where
    testUncons ::
      (BlobEq a, StackNum a, PackFs a) =>
      (forall s'. Integer -> Fn s' (s' > a)) ->
      Fn (s > V.TVector a) s
    testUncons val =
      begin
        # (V.uncons # fromJust # snd)
        # (V.uncons # fromJust # fst # val 1 # equalVerify)

progConstruction :: Fn s (s > TBool)
progConstruction =
  runEnv
    ( begin
        # ( begin
              # (nat 3 # int64 1 # V.replicate)
              # (int64 1 # int64 1 # int64 1 # V.empty)
              # (V.cons # V.cons # V.cons # equalVerify)
          )
        # ( begin
              # (nat 0 # int64 1 # V.replicate)
              # V.empty
              # equalVerify
          )
        # ( begin
              # (nat 3 # lambda1 (op1Add # cast) # V.generate)
              # (int64 1 # int64 2 # int64 3 # V.empty)
              # (V.cons # V.cons # V.cons # equalVerify)
          )
        # ( begin
              # (nat 3 # lambda1 (int8 2 # opMul) # int8 2 # V.iterateN)
              # (int8 2 # int8 4 # int8 8 # V.empty)
              # (V.cons # V.cons # V.cons # equalVerify)
          )
        # opTrue
    )

progConcatenation :: Fn s (s > TBool)
progConcatenation =
  runEnv
    ( begin
        # ( begin
              # (nat 3 # lambda1 (op1Add # cast) # V.generate)
              # (int64 1 # V.empty # V.cons # int64 2 # V.snoc # int64 3)
              # (V.snoc # equalVerify)
          )
        # ( begin
              # ( (nat 3 # lambda1 (op1Add # cast) # V.generate) ::
                    Env s (s > V.TVector TInt64)
                )
              # ( (nat 3 # lambda1 (id # cast) # V.generate) ::
                    Env s (s > V.TVector TInt64)
                )
              # V.append
              # ( begin
                    # (int64 1 # int64 2 # int64 3 # int64 0 # int64 1)
                    # (int64 2 # V.empty # V.cons # V.cons # V.cons # V.cons)
                    # (V.cons # V.cons)
                )
              # equalVerify
          )
        # opTrue
    )

progPermutation :: Fn s (s > TBool)
progPermutation =
  runEnv
    ( begin
        # ( begin
              # (int8Vector # V.reverse)
              # (int8 2 # int8 1 # int8 0 # V.empty # V.cons # V.cons # V.cons)
              # equalVerify
          )
        # ( begin
              # (int64Vector # V.reverse)
              # (int64 2 # int64 1 # int64 0 # V.empty)
              # (V.cons # V.cons # V.cons # equalVerify)
          )
        # ( begin
              # (bytes128Vector # V.reverse)
              # (bytes128 b2 # bytes128 b1 # bytes128 b0 # V.empty)
              # (V.cons # V.cons # V.cons # equalVerify)
          )
        # opTrue
    )

progMapping :: Fn s (s > TBool)
progMapping =
  runEnv
    ( begin
        # ( begin
              # (lambda1 (int8 2 # opMul) # int8Vector # V.map)
              # (int8 0 # int8 2 # int8 4 # V.empty # V.cons # V.cons # V.cons)
              # equalVerify
          )
        # ( begin
              # (lambda1 (int64 2 # opMul) # int64Vector # V.map)
              # (int64 0 # int64 2 # int64 4 # V.empty # V.cons # V.cons)
              # (V.cons # equalVerify)
          )
        # ( begin
              # (lambda1 addExclamation # bytes128Vector # V.map)
              # (bytes128 (b0 <> "!") # bytes128 (b1 <> "!"))
              # (bytes128 (b2 <> "!") # V.empty # V.cons # V.cons # V.cons)
              # equalVerify
          )
        # ( begin
              # (lambda1 int8to64 # int8Vector # V.map)
              # (int64Vector # equalVerify)
          )
        # opTrue
    )
  where
    int8to64 :: Fn (s > TInt8) (s > TInt64)
    int8to64 = cast

progZipping :: Fn s (s > TBool)
progZipping =
  runEnv
    ( begin
        # ( begin
              # (int64Vector # int8Vector # V.zip)
              # (int64 0 # int8 0 # TFS.tuple)
              # (int64 1 # int8 1 # TFS.tuple)
              # (int64 2 # int8 2 # TFS.tuple)
              # (V.empty # V.cons # V.cons # V.cons)
              # equalVerify
          )
        # ( begin
              # (int64Vector # int8Vector # V.zip)
              # (V.unsnoc # fromJust # snd)
              # (int64 2 # int8 2 # TFS.tuple)
              # equalVerify
          )
        # ( begin
              # (int64Vector # bytes128Vector # V.zip)
              # (int64 0 # bytes128 b0 # TFS.tuple)
              # (int64 1 # bytes128 b1 # TFS.tuple)
              # (int64 2 # bytes128 b2 # TFS.tuple)
              # (V.empty # V.cons # V.cons # V.cons)
              # equalVerify
          )
        # ( begin
              # (int64Vector # bytes128Vector # V.zip)
              # (V.unsnoc # fromJust # snd)
              # (int64 2 # bytes128 b2 # TFS.tuple)
              # equalVerify
          )
        # ( begin
              # lambda2
                ( begin
                    # (packFsRec @TInt64 # packFsRec @TInt8 # op2Swap)
                    # TFS.tupleF
                )
              # (int64Vector # int8Vector # V.zipWith)
              # (int64 0 # int8 0 # TFS.tuple)
              # (int64 1 # int8 1 # TFS.tuple)
              # (int64 2 # int8 2 # TFS.tuple)
              # (V.empty # V.cons # V.cons # V.cons)
              # equalVerify
          )
        # ( begin
              # (int64Vector # int8Vector # V.zip)
              # V.unzip
              # (int8Vector # equalVerify)
              # (int64Vector # equalVerify)
          )
        # ( begin
              # lambda2 opAdd
              # int64Vector
              # (opDup # lambda1 (int64 1 # opAdd) # opSwap # V.map)
              # V.zipWith
              # (int64 1 # int64 3 # int64 5 # V.empty)
              # (V.cons # V.cons # V.cons)
              # equalVerify
          )
        # opTrue
    )

progFiltering :: Fn s (s > TBool)
progFiltering =
  runEnv
    ( begin
        # ( begin
              # (nat 10 # lambda1 (op1Add # cast) # V.generate)
              # (lambda1 (int8 3 # int8LessThan) # opSwap # V.filter)
              # (int8 1 # int8 2 # V.empty # V.cons # V.cons)
              # equalVerify
          )
        # opTrue
    )
  where
    int8LessThan :: Fn (s > TInt8 > TInt8) (s > TBool)
    int8LessThan = fixup # opLessThan

    fixup :: Fn (s > TInt8 > TInt8) (s > TInt > TInt)
    fixup = castStack

fromJust :: (StackEntry a) => Fn (s > TMaybe a) (s > a)
fromJust = err # opSwap # fromMaybe'
  where
    err = lambda0 (bytes "E0" # opFalse # opVerify # cast)

-- 'overhead' leaves room for the extra data used in 'foldlF'.
propReverse :: BytesSize -> Bool
propReverse (BytesSize size) =
  let len = max 0 (size - overhead) `div` testVectorElemSize
   in isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' =
      runEnv (testVector len' # opDup # V.reverse # V.reverse # equal)

    overhead :: Integer
    overhead = 10

propLength :: BytesSize -> Bool
propLength (BytesSize size) =
  let len = size `div` testVectorElemSize
   in isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' = runEnv (nat len' # testVector len' # V.length # opNumEqual)

propLookup :: BytesSize -> BytesSize -> Property
propLookup (BytesSize size1) (BytesSize size2) =
  (size2 <= size1) ==>
    let len = size1 `div` testVectorElemSize
        idx = max 0 ((size2 `div` testVectorElemSize) - 1)
     in isTrue' (evaluateProg (prog (fromIntegral len) (fromIntegral idx)))
  where
    prog :: Natural -> Natural -> Fn s (s > TBool)
    prog len' idx' =
      runEnv
        ( begin
            # (testVector len' # nat idx' # V.lookup)
            # (int64 (fromIntegral idx') # just # equal)
        )

propConsSnocAppend :: BytesSize -> Bool
propConsSnocAppend (BytesSize size) =
  let len = max 0 ((size `div` testVectorElemSize) - 2)
   in isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' =
      runEnv
        ( begin
            # name "vec" (testVector len')
            # (int64 1 # pick "vec" # V.cons # int64 2 # V.snoc)
            # (int64 1 # V.singleton # roll "vec" # int64 2 # V.singleton)
            # (V.append # V.append)
            # equal
        )

propHeadTailUncons :: BytesSize -> Property
propHeadTailUncons (BytesSize size) =
  (size >= testVectorElemSize) ==>
    let len = size `div` testVectorElemSize
     in isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' =
      runEnv
        ( begin
            # name "vec" (testVector len')
            # (lambda2 tuple # pick "vec" # V.head)
            # (pick "vec" # V.tail # liftA2Maybe)
            # (roll "vec" # V.uncons # equal)
        )

propLastInitUnsnoc :: BytesSize -> Property
propLastInitUnsnoc (BytesSize size) =
  (size >= testVectorElemSize) ==>
    let len = size `div` testVectorElemSize
     in isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' =
      runEnv
        ( begin
            # name "vec" (testVector len')
            # (lambda2 tuple # pick "vec" # V.init)
            # (pick "vec" # V.last # liftA2Maybe)
            # (roll "vec" # V.unsnoc # equal)
        )

-- 9998 is based on current 10K element limit and leaving room for the
-- tuple size field.
propTakeDropSplitAt :: BytesSize -> BytesSize -> Property
propTakeDropSplitAt (BytesSize size1) (BytesSize size2) =
  (size1 <= 9_998 && size2 <= 9_998) ==>
    let len = size1 `div` testVectorElemSize
        idx = size2 `div` testVectorElemSize
     in isTrue' (evaluateProg (prog (fromIntegral len) (fromIntegral idx)))
  where
    prog :: Natural -> Natural -> Fn s (s > TBool)
    prog len' idx' =
      runEnv
        ( begin
            # name "vec" (testVector len')
            # (nat idx' # pick "vec" # V.take)
            # (nat idx' # pick "vec" # V.drop # tuple)
            # (nat idx' # roll "vec" # V.splitAt # tuple # equal)
        )

-- 4000 is based on current 10K element limit and leaving ample room for the
-- tuple size field.
propZipUnzip :: BytesSize -> BytesSize -> Property
propZipUnzip (BytesSize size1) (BytesSize size2) =
  (size1 <= 4000 && size2 <= 4000) ==>
    let len1 = size1 `div` testVectorElemSize
        len2 = size2 `div` testVectorElemSize
     in isTrue' (evaluateProg (prog (fromIntegral len1) (fromIntegral len2)))
  where
    prog :: Natural -> Natural -> Fn s (s > TBool)
    prog len1' len2' =
      runEnv
        ( begin
            # (name "vec1" (testVector len1') # name "vec2" (testVector len2'))
            # (pick "vec1" # pick "vec2" # V.zip # V.unzip # V.zip)
            # name "minLen" (nat len1' # nat len2' # opMin)
            # (pick "minLen" # roll "vec1" # V.take)
            # (roll "minLen" # roll "vec2" # V.take)
            # (V.zip # equal)
        )

propZipWithUnzip :: BytesSize -> BytesSize -> Property
propZipWithUnzip (BytesSize size1) (BytesSize size2) =
  (size1 <= 4000 && size2 <= 4000) ==>
    let len1 = size1 `div` testVectorElemSize
        len2 = size2 `div` testVectorElemSize
     in isTrue' (evaluateProg (prog (fromIntegral len1) (fromIntegral len2)))
  where
    prog :: Natural -> Natural -> Fn s (s > TBool)
    prog len1' len2' =
      runEnv
        ( begin
            # (name "vec1" (testVector len1') # name "vec2" (testVector len2'))
            # (lambda2 TFS.tuple # opDup)
            # (pick "vec1" # pick "vec2" # V.zipWith # V.unzip # V.zipWith)
            # name "minLen" (nat len1' # nat len2' # opMin)
            # (pick "minLen" # roll "vec1" # V.take)
            # (roll "minLen" # roll "vec2" # V.take)
            # (V.zip # equal)
        )

-- 'overhead' leaves room for the extra data used in 'foldlF'.
propFilterKeepAll :: BytesSize -> Bool
propFilterKeepAll (BytesSize size) =
  let len = max 0 ((size - overhead) `div` testVectorElemSize)
   in isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' =
      runEnv
        ( begin
            # (testVector len' # opDup)
            # (lambda1 (opDrop # opTrue) # opSwap # V.filter # equal)
        )

    overhead :: Integer
    overhead = 10

propFilterKeepNone :: BytesSize -> Bool
propFilterKeepNone (BytesSize size) =
  let len = size `div` testVectorElemSize
   in isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' =
      runEnv
        ( begin
            # (V.empty # testVector len')
            # (lambda1 (opDrop # opFalse) # opSwap # V.filter # equal)
        )

-- 'overhead' leaves room for the extra data used in 'foldlF'.
propMapComposition :: BytesSize -> Bool
propMapComposition (BytesSize size) =
  let len = max 0 ((size - overhead) `div` testVectorElemSize)
   in isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' =
      runEnv
        ( begin
            # name "vec" (testVector len')
            # (lambda1 f # lambda1 g # pick "vec" # V.map # V.map)
            # (lambda1 (g # f) # roll "vec" # V.map)
            # equal
        )

    f :: Fn (s > TInt64) (s > TInt64)
    f = int64 2 # opAdd

    g :: Fn (s > TInt64) (s > TInt64)
    g = int64 2 # opMul

    overhead :: Integer
    overhead = 10

-- 'overhead' leaves room for the extra data used in 'foldlF'.
propMapIdentity :: BytesSize -> Bool
propMapIdentity (BytesSize size) =
  let len = max 0 ((size - overhead) `div` testVectorElemSize)
   in isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' =
      runEnv
        ( begin
            # name "vec" (testVector len')
            # (pick "vec" # lambda1 f # roll "vec" # V.map # equal)
        )

    f :: Fn (s > TInt64) (s > TInt64)
    f = cast

    overhead :: Integer
    overhead = 10

propFolding :: BytesSize -> Bool
propFolding (BytesSize size) =
  isTrue' (evaluateProg (prog (fromIntegral len)))
  where
    prog :: Natural -> Fn s (s > TBool)
    prog len' =
      runEnv
        ( begin
            # (lambda2 opAdd # int64 0 # testVector len' # V.foldl)
            # (int64 sum # equalVerify)
            # (lambda2 opAdd # int64 0 # testVector len' # V.foldr)
            # (int64 sum # equalVerify)
            # opTrue
        )

    len :: Integer
    len = size `div` testVectorElemSize

    sum :: Integer
    sum = let n = len - 1 in n * (n + 1) `div` 2

-- ## Test vectors.
int64Vector :: Fn s (s > V.TVector TInt64)
int64Vector =
  fn
    ( begin
        # (int64 0 # int64 1 # int64 2 # V.empty # V.cons # V.cons # V.cons)
        # (opDup # v2b # opSize # opNip # nat (8 * 3) # equalVerify)
    )

v2b :: Fn (s > V.TVector a) (s > TBytes)
v2b = cast

int8Vector :: Fn s (s > V.TVector TInt8)
int8Vector =
  fn
    ( begin
        # (int8 0 # int8 1 # int8 2 # V.empty # V.cons # V.cons # V.cons)
        # (opDup # v2b # opSize # opNip # nat (1 * 3) # equalVerify)
    )

bytes128Vector :: Fn s (s > V.TVector TBytes128)
bytes128Vector =
  fn
    ( begin
        # (bytes128 b0 # bytes128 b1 # bytes128 b2 # V.empty)
        # (V.cons # V.cons # V.cons)
        # (opDup # v2b # opSize # opNip # nat (130 * 3) # equalVerify)
    )

b0 :: Bytes
b0 = "hello world"

b1 :: Bytes
b1 = "a string that is a bit longer and keeps going"

b2 :: Bytes
b2 = ""

testVector :: Natural -> Env s (s > V.TVector TInt64)
testVector len = nat len # lambda1 cast # V.generate

testVectorElemSize :: Integer
testVectorElemSize = 8

-- ## bytes128Vector ops.
addExclamation :: Fn (s > TBytes128) (s > TBytes128)
addExclamation = toBytes # bytes "!" # opCat # toBytes128
