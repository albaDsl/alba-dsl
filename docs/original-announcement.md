# albaDsl & albaVm

Experimental Haskell based tooling for Bitcoin Cash (BCH) contract development
and testing.

## albaDsl

AlbaDsl is a shallowly embedded Domain Specific Language (DSL) for programming
Bitcoin Cash Script (2025) in Haskell. It uses Haskell's type system to
statically enforce the type of the input and output stacks of a given program.
Stack items can be assigned names for easier reference. Haskell functions can be
used as a form of statically typed parameterized macros. Collections of such
macros can be grouped into libraries and complex contracts can be built from
them. Standard Haskell tooling such as syntax highlighting, code formatting, and
Language Server Protocol (LSP) can be used. When using the Haskell Language
Server, it gives immediate feedback about type errors such as mismatched stack
element types.

Bitcoin Cash contracts written in albaDsl can be assembled into transactions and
serialized into byte strings for publishing to the network using, for example,
Bitcoin Cash Node [1].

## albaVm

AlbaVm is a Bitcoin Cash virtual machine written in Haskell. It supports the BCH
2025 instruction set (including tokens, BigInts, and VM limits). It passes the
Libauth [2] 2025 standard and non-standard "success vectors", and part of the
non-standard and invalid "failure vectors" (correct failure reasons yet to be
verified). AlbaVm can be used to evaluate arbitrary BCH byte code, and
specifically albaDsl programs during development and testing.

## Combining albaDsl & albaVm to verify contracts

Given an albaDsl program 'p', the albaDsl compiler 'compile' and the albaVm
evaluator 'eval', it is possible to combine them into a function 'g = eval
(compile p)'. This function can be used to calculate the result of applying 'p'
to arbitrary input stacks. Thus it can be used to write unit tests for the
program. Such tests can make use of automatic property based testing via
Haskell's QuickCheck [8]. Possibly, the LiquidHaskell [9] program verifier can
also be used for verification.

The example contracts in this repository (transferWithTimeout and lastWill)
illustrate how contracts can be built and tested.

## Status

This code is *experimental*. The albaDsl syntax will continue to evolve and
albaVm needs to be further tested. Example contracts need further validation.
There will be bugs. For licensing, see LICENSE.

## Acknowledgments / Notes

For more mature and high-level contract languages for Bitcoin Cash contract
programming, see CashScript [3] and Spedn [4]. Also see CashAssembly [2].

AlbaDsl uses some of the CashScript [3] code optimization rules. 

Bitcoin Cash Node [1] is a continuation of the Satoshi Bitcoin client. The
AlbaDsl project's virtual machine, transactions, and validation, are modeled
after it.

Haskoin [6] is a Haskell library for Bitcoin and Bitcoin Cash development that
this project depends on.

Lorentz [5] is a Haskell EDSL for writing contracts for the Tezos stack based VM
using similar techniques as albaDsl but taken a step further.

For VM verification this project relies on the Libauth [2] transaction level
test suites.

This project depends on libSecp256k1 [7]. The Haskoin provided integration, but
also via a direct integration to the Bitcoin Cash Node fork of libSecp256k1.

Licho Last Will [10] is a covenant contract. An albaDsl interpretation of it is
provided in the contract examples.

## References

[1] Bitcoin Cash Node. https://bitcoincashnode.org/en/

[2] Libauth / CashAssembly. https://github.com/bitauth/libauth

[3] CashScript. https://cashscript.org

[4] Spedn. https://spedn.pl

[5] Morley / Lorentz EDSL. https://gitlab.com/morley-framework/morley

[6] Haskoin. https://github.com/haskoin/haskoin-core

[7] libsecp256k1. https://github.com/bitcoin-core/secp256k1

[8] QuickCheck. https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html

[9] LiquidHaskell. https://ucsd-progsys.github.io/liquidhaskell/

[10] Licho Last Will. https://github.com/KarolTrzeszczkowski/Electron-Cash-Last-Will-Plugin
