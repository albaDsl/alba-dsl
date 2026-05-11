# Installation of GHC and its support tools
The Haskell compiler (GHC) and its support tools can be installed using
[GHCup](https://www.haskell.org/ghcup/):

- GHC compiler: Use the version mentioned at the top of the lts-XX.XX.config
  file inside this repo.
- Cabal build tool: Use the GHCup recommended version.
- Haskell Language Server (HLS): Use the GHCup recommended version.

# Building AlbaDsl & AlbaVm
From the top of this repo, use `make test` to build the project and run the test
suite.

# Trying out the language and contracts
To try the language out see `apps/demo/Demo.hs`, `make repl`, and the
introduction video.

Contracts are wrapped by a shell command that is used to deploy and interact
with the contract. To run a shell command, from the top of this repo use e.g.:

```bash
cabal run lastWill -- -h
cabal run lastWill -- deploy -h
cabal run ellipticCurve -- -h
cabal run permutationChallenge -- -h
```

Some contracts rely on manual publishing of transactions using
[BCHN's](https://bitcoincashnode.org/en/) `sendrawtransaction`. Other contracts
interface directly to BCHN, see `contracts/permutationChallenge/readme.md` for
how to configure this.
