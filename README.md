# purescript-prisma

[![CI](https://github.com/hasanalrasyid/purescript-prisma/workflows/CI/badge.svg?branch=main)](https://github.com/hasanalrasyid/purescript-prisma/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://github.com/hasanalrasyid/purescript-prisma/workflows/CI/badge.svg?branch=main)](https://github.com/hasanalrasyid/purescript-prisma/releases)
[![Maintainer: hasanalrasyid](https://img.shields.io/badge/maintainer-hasanalrasyid-teal.svg)](https://github.com/hasanalrasyid)

A parser for Prisma schema file (schema.prisma)

## Installation

Install `purescript-prisma` by including it into package.dhall. The easiest path would be including clone this repo and set it as addition in the package.dhall.

```console
git clone https://github.com/hasanalrasyid/purescript-prisma
cd working-project
```

Then put this clone in package.dhall inside working-project. For example:

```
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230926/package
s.dhall
        sha256:684d460e9ca2089c89535e9a4ec56b83fb8b927ddd87afa6fa764ffb60bf1488
let additions =
    { prisma = ../purescript-prisma/spago.dhall as Location
    }
in  (upstream // additions)
```

in working-project, you can use it as a library.

```console
spago install prisma
```

## Installation (Future)

Install `purescript-prisma` with [Spago](https://github.com/purescript/spago):

```console
$ spago install prisma
```

## Quick start

Just use the parser as shown in the example inside app.

```purescript
module Main where

import Prisma.Parser as Parser

import Node.Encoding
import Effect (Effect)
import Effect.Console (logShow)
import Node.FS.Sync (readTextFile)
import Prelude (Unit, bind, discard)

main :: Effect Unit
main  = do
  target1 <- readTextFile UTF8 "./prisma/schema.prisma"
  logShow target1
  let res = Parser.runParser target1
  logShow res
```

# Issues

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/hasanalrasyid/purescript-prisma/issues) if you have encountered a bug or problem.

## Contributing

You can contribute to `purescript-prisma` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/hasanalrasyid/purescript-prisma/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
