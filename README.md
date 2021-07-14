# haskelchain

Simple implementation of a Blockchain system

[![Haskell CI](https://github.com/roomanidzee/haskelchain/actions/workflows/main.yml/badge.svg)](https://github.com/roomanidzee/haskelchain/actions/workflows/main.yml)

### Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

It is recommended that you follow the instructions on how to install Haskell with Stack on [Haskell's official Downloads page](https://www.haskell.org/downloads/#stack).

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC
(which should be there already if you have Haskell Platform 8.6.5).

## Run

This project has one executable that you can run with

```
stack exec haskelchain-exe
```

During development it is recommended a combination of `build` and `exec`:

```
stack build && stack exec haskelchain-exe
```

Alternatively, you can run

```
stack build file-watch
```

For continuous builds in the background.

## Interpreter

You can run GHCi (GHC interpreter) for the whole project with

```
stack ghci
```

or

```
stack repl
```

During development it might be beneficial to work using an interpreter
for quick reloads, interacting with various parts of code and
trying out things.

Note that you can run executable from GHCi by invoking `main`.

### GHCi commands

There are some useful commands in GHCi that might help
you with the development process:

- `:t` or `:type` — show the type of an expression:

```
>>> :t map (+1)
map (+1) :: Num b => [b] -> [b]
```

- `:l` or `:load` — load file or module:

```
>>> :load src/MyProject.hs
[1 of 1] Compiling MyProject        ( src/MyProject.hs, interpreted )
Ok, one module loaded.
```

- `:set` and `:unset` — turn an option on/off:

```
>>> :set -XOverloadedStrings
>>> :set -Wall -fno-warn-type-defaults
```

## `ghcid`

For faster feedback from the compiler it is recommended to use `ghcid`
(GHCi deamon).

Install `ghcid` with `stack`:

```
stack install ghcid
```

Now you can run `ghcid` with Stack using

```
ghcid -c "stack repl"
```

This will run GHCi with the entire project loaded and will
quickly reload all modules when you modify them to tell you
if you have any errors or warnings.

_Note: you can also run `ghcid` without parameters and it will detect
Stack project, but you'll have to use system-wide `.ghci`.
See [ndmitchell/ghcid#72](https://github.com/ndmitchell/ghcid/issues/72) for more details._

## How to work with blockchain

You can work with by two methods:

- from GHCI:

```haskell
import Mining()
import Types()

let testTransaction = Transaction 300 500 100

let testBlock = Block (V.fromList [testTransaction, testTransaction])

let testGenesisBlock = Block (V.fromList [])

let genesisChain = testGenesisBlock :< Genesis

let newBlock = Block (V.fromList [testTransaction])

let testString = "test_hash"

let testBlockHeader =
     BlockHeader
        { _miner = 1,
          _parentHash = hash (packStr'' testString),
          _nonce = 100,
          _minedAt = unsafePerformIO getPOSIXTime
        }

let testChain = testBlock :< Node testBlockHeader genesisChain

let newChain = addBlock newBlock testBlockHeader testChain

```

- from HTTP API:

```

GET localhost:8081/createChainFileRoute?file_name=genesis_chain1_file 
  return string with creation result

GET localhost:8081/listBalancesRoute?file_name=genesis_chain_file
  return balances from input file chain path

GET localhost:8081/mineBlocksRoute?file_name=genesis_chain_file&account_value=1
  performs mining process and return status string about process

```
