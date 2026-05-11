# AlbaDsl & AlbaVm
Experimental Haskell based tooling for Bitcoin Cash (BCH) contract development
and testing.

## AlbaDsl
AlbaDsl is an *experimental* statically typed functional "Forth" for Bitcoin
Cash contract programming, implemented as a *shallowly* embedded Domain Specific
Language (DSL) in Haskell. It is not a traditional Forth at all. The syntax is
all Haskell, the language features are limited to what is reasonably expressable
on CashVM, and inspiration comes from Haskell and functional programming. But at
the core is postfix notation, a stack for argument passing, and an extensible
collection of words (functions) — like Forth.

AlbaDsl's abstraction mechanisms include macros (expressed using Haskell
functions), functions (words), anonymous functions with partial application, and
parametric polymorphism with type classes (to some degree). Collections of
macros and functions can be grouped using standard Haskell modules and
libraries. Contract complexity is managed by layering abstractions and taking
advantage of the strong typing.

Haskell's type system is used to statically enforce the type of the input and
output stacks of a given AlbaDsl expression. Stack items can be assigned names
(at the type-level) for easy reference. On top of the basic types offered by
CashVM (`TInt`, `TBytes`, `TBool`, etc.) aggregated types such as `TMaybe a`,
`TEither a b`, `TTuple a b`, and `TVector a` are implemented in the base
library. These types are modeled after the corresponding types in Haskell.
Building on this, users can construct their own abstract types. Type information
is removed at compile time and is not present in the final bytecode.

Since an AlbaDsl program is a Haskell program, standard Haskell tooling such as
syntax highlighting, linting, code formatting, and Language Server Protocol
(LSP) are supported out of the box. The Haskell Language Server gives immediate
feedback about type errors such as mismatched stack element types and can be
used to interrogate the type of the stack at a given position in an expression.

## AlbaVm
AlbaVm is a Bitcoin Cash virtual machine written in Haskell. It supports the
CashVm 2026 instruction set. It passes the Libauth 2026 tests and various
additional property based tests. AlbaVm can be used to evaluate arbitrary BCH
byte code, and specifically AlbaDsl programs during development and testing. 

By combining an AlbaDsl program/contract (`p`), the AlbaDsl compiler
(`compile`), and the AlbaVm evaluator (`eval`), one can define a Haskell
function `f = eval (compile p)`. This function can be tested using standard
Haskell unit test frameworks and property based testing such as QuickCheck. This
then serves as testing of `p`.

## Status
This code is *experimental* and work in progress. The AlbaDsl language is not
fleshed out and the syntax and feature set continues to evolve. AlbaVm needs to
be further tested. Example contracts need further validation. There will be
bugs. For licensing, see LICENSE.

## Demo Videos
- Introduction to AlbaDsl & AlbaVm. ([Asciinema](https://asciinema.org/a/yhxjb0JEFGHHjlfe))
  - Covers: Basic expressions, compilation and evaluation, type checking of
    expressions, LSP interaction, parameterized macros, QuickCheck testing,
    naming stack entries, and BigInt arithmetic.
- Functions and constants in AlbaDsl. ([Asciinema](https://asciinema.org/a/olVYRCKvZEaoaz8z))
  - Covers: Defining functions and constants, listing program bytecode, and listing
    the function table.

## Examples
AlbaDsl example code:
- wNAF elliptic curve scalar multiply on secp256k1.
  ([GitHub](https://github.com/albaDsl/alba-dsl/blob/b4b8b02fd1686c1ab068b2318796c8fdccddc673/src/DslDemo/EllipticCurve/JacobianWNaf.hs))
- LZSS decompressor. Uses the CashVM 2026 bit shift operators.
  ([GitHub](https://github.com/albaDsl/alba-dsl/blob/b4b8b02fd1686c1ab068b2318796c8fdccddc673/src/Alba/Dsl/V1/Bch2026/Contract/LzssBit.hs))
  ([BCR](https://bitcoincashresearch.org/t/albadsl-albavm-haskell-based-dsl-and-vm-for-bitcoin-cash-2025-contract-programming/1558/34?u=albadsl))
- Vector library. Vector that can store any type that implements the PackFs type
  class.
  ([GitHub](https://github.com/albaDsl/alba-dsl/blob/b4b8b02fd1686c1ab068b2318796c8fdccddc673/src/Alba/Dsl/V1/Bch2026/Contract/TVector.hs))
  ([BCR](https://bitcoincashresearch.org/t/albadsl-albavm-haskell-based-dsl-and-vm-for-bitcoin-cash-2025-contract-programming/1558/32?u=albadsl))
- MergeSort. Sorts a Vector of elements satisfying the Ord typeclass. Uses
  recursion.
  ([GitHub](https://github.com/albaDsl/alba-dsl/blob/5bf90544fe49e3a0b25b35e4a19c10c2df1965b2/src/DslDemo/MergeSort/MergeSort.hs))
- TurtleVm: a meta-circular evaluator for Bitcoin Cash Script on CashVM 2025.
  ([GitHub](https://github.com/albaDsl/alba-dsl/tree/b4b8b02fd1686c1ab068b2318796c8fdccddc673/src/DslDemo/TurtleVm/Bch2025))
  ([BCR](https://bitcoincashresearch.org/t/turtlevm-a-meta-circular-evaluator-for-bitcoin-cash-script-proof-of-concept/1638?u=albadsl))
- Beginnings of a standard library supporting TTuple, TMaybe, TEither, and
  some base type classes.
  ([GitHub](https://github.com/albaDsl/alba-dsl/tree/b4b8b02fd1686c1ab068b2318796c8fdccddc673/src/Alba/Dsl/V1/Bch2026/Contract))

Example AlbaDsl contracts:
- Last will. Basic contract with three entry points.
  ([GitHub](https://github.com/albaDsl/alba-dsl/tree/b4b8b02fd1686c1ab068b2318796c8fdccddc673/apps/contracts/lastWill))
- Elliptic Curve. Limited secp256k1 scalar multiply with a precomputed wNAF
  table in a BCH 2026 transaction.
  ([GitHub](https://github.com/albaDsl/alba-dsl/tree/b4b8b02fd1686c1ab068b2318796c8fdccddc673/apps/contracts/ellipticCurve))
- MiniTurtleVm Challenge. Meta-circular BCH Script evaluator inside a BCH 2025
  Mainnet transaction.
  ([GitHub](https://github.com/albaDsl/alba-dsl/tree/b4b8b02fd1686c1ab068b2318796c8fdccddc673/apps/contracts/miniTurtleChallenge))
  ([BCR](https://bitcoincashresearch.org/t/turtlevm-a-meta-circular-evaluator-for-bitcoin-cash-script-proof-of-concept/1638?u=albadsl))
- Permutation Challenge. Illustrates shared libraries in read-only inputs on
  BCH 2026.
  ([GitHub](https://github.com/albaDsl/alba-dsl/tree/b4b8b02fd1686c1ab068b2318796c8fdccddc673/apps/contracts/permutationChallenge))
  ([BCR](https://bitcoincashresearch.org/t/albadsl-albavm-haskell-based-dsl-and-vm-for-bitcoin-cash-2025-contract-programming/1558/34?u=albadsl))

Example AlbaVm HTML execution trace with function calls collapsed.
 ([GitHub](https://albadsl.github.io/alba-dsl/log-2-functions.html))

## References
Original announcement: https://albadsl.github.io/alba-dsl/original-announcement.md
