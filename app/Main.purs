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

