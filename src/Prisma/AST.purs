module Prisma.AST where

import Prelude hiding (between)
--
import Data.Array (fromFoldable)
--
-- ==========================================
-- DATA STRUCTURES (AST)
-- ==========================================

data PrismaValue
  = PVString String
  | PVIdent  String
  | PVNumber String
  | PVArray  (Array PrismaValue)
  | PVEnv    String
  | PVFunc   String (Array PrismaValue)
  | PVKeyVal String PrismaValue

instance showPrismaValue :: Show PrismaValue where
  show (PVString s)    = "PVString " <> s
  show (PVIdent s)     = "PVIdent  " <> s
  show (PVNumber s)    = "PVNumber " <> show s
  show (PVArray a)     = "PVArray  " <> show a
  show (PVEnv s)       = "PVEnv    " <> "env(" <> show s <> ")"
  show (PVFunc n args) = "PVFunc   " <> n <> "(" <> show args <> ")"
  show (PVKeyVal k v)  = "PVKeyVal " <> k <> ": " <> show v

instance eqPrismaValue :: Eq PrismaValue where
  eq (PVString a) (PVString b) = eq a b
  eq (PVIdent a) (PVIdent b) = eq a b
  eq (PVNumber a) (PVNumber b) = eq a b
  eq (PVArray a) (PVArray b) = eq a b
  eq (PVEnv a) (PVEnv b) = eq a b
  eq (PVFunc a x) (PVFunc b y) = eq a b && eq x y
  eq (PVKeyVal a x) (PVKeyVal b y) = eq a b && eq x y
  eq _ _ = false

type Key = String
data Property = Property Key MetaValue

type MetaValue =
  { val :: PrismaValue
  , meta :: Array Property
  }

type Generator =
  { name :: String
  , provider :: String
  , properties :: Array Property
  }

type Datasource =
  { name :: String
  , provider :: String
  , url :: PrismaValue
  , properties :: Array Property
  }

type EnumValue =
  { name :: String
  , attributes :: Array Property -- Added to support @map
  , meta :: Array Property
  }

type Name = String

data EnumDef = EnumDef Name
  { values :: Array EnumValue
  , attributes :: Array Property
  }

type ModelField =
  { name :: String
  , typeName :: String
  , modifier :: String -- "?" or "[]" or ""
  , attributes :: Array Property -- Real attributes like @id
  , meta :: Array Property -- Meta comments //_
  }

data ModelDef = ModelDef Name
  { fields :: Array ModelField
  , attributes :: Array Property -- @@attributes
  }

data SchemaItem
  = ItemGenerator Generator
  | ItemDatasource Datasource
  | ItemEnum EnumDef
  | ItemModel ModelDef
  | ItemView  ModelDef

instance showSchemaItem :: Show SchemaItem where
  show (ItemGenerator g) =
    "\nGenerator {\n" <>
    "  name: " <> g.name <> "\n" <>
    "  provider: " <> g.provider <> "\n" <>
    "  properties: " <> show g.properties <> "\n" <>
    "}"
  show (ItemDatasource d) =
    "\nDatasource {\n" <>
    "  name: " <> d.name <> "\n" <>
    "  provider: " <> d.provider <> "\n" <>
    "  url: " <> show d.url <> "\n" <>
    "  properties: " <> show d.properties <> "\n" <>
    "}"
  show (ItemEnum (EnumDef name e)) =
    "\nEnum " <> name <> " {\n" <>
    "  values: " <> show e.values <> "\n" <>
    "  attributes: " <> show e.attributes <> "\n" <>
    "}"
  show (ItemModel (ModelDef name m)) =
    "\nModel " <> name <> " {\n" <>
    "  fields: " <> show m.fields <> "\n" <>
    "  attributes: " <> show m.attributes <> "\n" <>
    "}"
  show (ItemView (ModelDef name m)) =
    "\nView " <> name <> " {\n" <>
    "  fields: " <> show m.fields <> "\n" <>
    "  attributes: " <> show m.attributes <> "\n" <>
    "}"

instance showProperty :: Show Property where
  show (Property key p) = key <> ": " <> show p.val <> (if (fromFoldable p.meta) == [] then "" else " [Meta: " <> show p.meta <> "]")

instance eqProperty :: Eq Property where
  eq (Property k a) (Property j b) = eq a b && eq k j

