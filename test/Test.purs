module Test where

import Prelude hiding (between)

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log, logShow)

---- StringParser imports
import StringParser (Parser)
import StringParser as StringParser

import Prisma.Parser (parseSchema)

-- ==========================================
-- MAIN EXECUTION
-- ==========================================

main01 :: Effect Unit
main01 = do
  log "### PRISMA PARSER (Generators, Datasources, Enums, Models + Meta) ###"
  doBoth "parseSchema" parseSchema fullInput

-- ==========================================
-- INPUT
-- ==========================================

fullInput :: String
fullInput =
  """
  generator client {
    provider        = "prisma-client-js"
    previewFeatures = ["multiSchema","views"]
  }

  datasource db {
    provider          = "postgresql"
    url               = env("DATABASE_URL")
    shadowDatabaseUrl = env("SHADOW_DATABASE_URL")
    schemas           = ["public"]
  }

  enum RangeWaktu {
    Mingguan //_ @auto @alias(Seminggu,TujuhHari) // this is real one
    Bulanan
    Tahunan
    PlanI   @map("50YEARS")

    @@schema("public") //_ @instanceOf(Semigroup("append a _ = a")) // real comment removed
  }

  model User {
    id        String   @id @default(dbgenerated("gen_random_uuid()")) @db.Uuid //_ @auto // @id @default(auto()) @map("_id") @db.ObjectId @auto
    phone     String   @unique
    password  String
    createdAt DateTime @default(now())
    updatedAt DateTime @default(now()) @updatedAt
    role      Roles    @default(PELANGGAN)
    profile   Profile? @relation(fields: [profileId], references: [id])
    profileId String?  @db.Uuid
    probability Int @default(100) //this probability ranged from [0,10] which 10 equals 100% always shown

    @@unique([phoneNumber, tahunBulan ], name: "uniqueNSInvoiceVoice")
    @@schema("public") //_ @instanceOf(Semigroup("append a _ = a")) // real comment removed
  }
  """

-- ==========================================
-- TEST HELPERS
-- ==========================================

doBoth :: forall a. Show a => String -> Parser a -> String -> Effect Unit
doBoth parserName parser content = do
  log $ "Testing for: " <> parserName
  case StringParser.runParser parser content of
    Left error -> logShow error
    Right result -> log $ show result

