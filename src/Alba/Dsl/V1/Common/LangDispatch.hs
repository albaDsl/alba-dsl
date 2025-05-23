-- Copyright (c) 2025 albaDsl
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Alba.Dsl.V1.Common.LangDispatch
  ( entry2,
    entry3,
    entry4,
    entry5,
    entry6,
    entry7,
    entry8,
  )
where

import Alba.Dsl.V1.Bch2025.Lang (int)
import Alba.Dsl.V1.Bch2025.LangArgs (argPick, argRoll)
import Alba.Dsl.V1.Bch2025.Ops (opDrop, opIf, opNumEqual, opNumEqualVerify)
import Alba.Dsl.V1.Common.Lang
  ( begin,
    branch1,
    branch2,
    branch3,
    branch4,
    branch5,
    branch6,
    branch7,
    branch8,
    (#),
  )
import Alba.Dsl.V1.Common.ThUtils qualified as TH

entry2 f1 f2 =
  $( TH.foldr
       [|(\(num, f, stack) cont -> fIdxCase num f stack cont)|]
       [|(fIdxLast (pred 2) f2 branch2)|]
       [[|(pred 1, f1, branch1)|]]
   )

entry3 f1 f2 f3 =
  $( TH.foldr
       [|(\(num, f, stack) cont -> fIdxCase num f stack cont)|]
       [|(fIdxLast (pred 3) f3 branch3)|]
       [ [|(pred 1, f1, branch1)|],
         [|(pred 2, f2, branch2)|]
       ]
   )

entry4 f1 f2 f3 f4 =
  $( TH.foldr
       [|(\(num, f, stack) cont -> fIdxCase num f stack cont)|]
       [|(fIdxLast (pred 4) f4 branch4)|]
       [ [|(pred 1, f1, branch1)|],
         [|(pred 2, f2, branch2)|],
         [|(pred 3, f3, branch3)|]
       ]
   )

entry5 f1 f2 f3 f4 f5 =
  $( TH.foldr
       [|(\(num, f, stack) cont -> fIdxCase num f stack cont)|]
       [|(fIdxLast (pred 5) f5 branch5)|]
       [ [|(pred 1, f1, branch1)|],
         [|(pred 2, f2, branch2)|],
         [|(pred 3, f3, branch3)|],
         [|(pred 4, f4, branch4)|]
       ]
   )

entry6 f1 f2 f3 f4 f5 f6 =
  $( TH.foldr
       [|(\(num, f, stack) cont -> fIdxCase num f stack cont)|]
       [|(fIdxLast (pred 6) f6 branch6)|]
       [ [|(pred 1, f1, branch1)|],
         [|(pred 2, f2, branch2)|],
         [|(pred 3, f3, branch3)|],
         [|(pred 4, f4, branch4)|],
         [|(pred 5, f5, branch5)|]
       ]
   )

entry7 f1 f2 f3 f4 f5 f6 f7 =
  $( TH.foldr
       [|(\(num, f, stack) cont -> fIdxCase num f stack cont)|]
       [|(fIdxLast (pred 7) f7 branch7)|]
       [ [|(pred 1, f1, branch1)|],
         [|(pred 2, f2, branch2)|],
         [|(pred 3, f3, branch3)|],
         [|(pred 4, f4, branch4)|],
         [|(pred 5, f5, branch5)|],
         [|(pred 6, f6, branch6)|]
       ]
   )

entry8 f1 f2 f3 f4 f5 f6 f7 f8 =
  $( TH.foldr
       [|(\(num, f, stack) cont -> fIdxCase num f stack cont)|]
       [|(fIdxLast (pred 8) f8 branch8)|]
       [ [|(pred 1, f1, branch1)|],
         [|(pred 2, f2, branch2)|],
         [|(pred 3, f3, branch3)|],
         [|(pred 4, f4, branch4)|],
         [|(pred 5, f5, branch5)|],
         [|(pred 6, f6, branch6)|],
         [|(pred 7, f7, branch7)|]
       ]
   )

fIdxCase num f stack continuation =
  begin
    # (argPick @"_fIdx" # int num # opNumEqual)
    # opIf (argRoll @"_fIdx" # opDrop # stack # f) continuation

fIdxLast num f stack =
  begin
    # (argRoll @"_fIdx" # int num # opNumEqualVerify)
    # stack
    # f
