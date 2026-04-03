-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.LangArgs where

import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.LangArgs (UnNameSeveral)
import Alba.Dsl.V1.Common.Stack (Fn, TBool)
import Prelude hiding (drop)

-- ## Loop with unnamed args.
type Loop args = Fn args (args > TBool)

-- ## Loop with named args.
type Loop1 args =
  forall args'. (UnNameSeveral 1 args ~ args') => Fn args (args' > TBool)

type Loop2 args =
  forall args'. (UnNameSeveral 2 args ~ args') => Fn args (args' > TBool)

type Loop3 args =
  forall args'. (UnNameSeveral 3 args ~ args') => Fn args (args' > TBool)

type Loop4 args =
  forall args'. (UnNameSeveral 4 args ~ args') => Fn args (args' > TBool)

type Loop5 args =
  forall args'. (UnNameSeveral 5 args ~ args') => Fn args (args' > TBool)

type Loop6 args =
  forall args'. (UnNameSeveral 6 args ~ args') => Fn args (args' > TBool)

type Loop7 args =
  forall args'. (UnNameSeveral 7 args ~ args') => Fn args (args' > TBool)

type Loop8 args =
  forall args'. (UnNameSeveral 8 args ~ args') => Fn args (args' > TBool)
