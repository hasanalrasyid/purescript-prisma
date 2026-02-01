module Prisma.AST where

import Prelude

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

data PrismaType
  = PInt
  | PString
  | PBoolean
  | PFloat
  | PDecimal
  | PBigInt
  | PDateTime
  | PJson
  | PBytes
  | PEnum String
  | PMaybe PrismaType
  | PArray PrismaType

typeFromString mod t =
  case mod of
    "[]" -> PArray $ core t
    "?"  -> PMaybe $ core t
    _    -> core t
  where
    core = case _ of
      "Int"       -> PInt
      "String"    -> PString
      "Boolean"   -> PBoolean
      "Float"     -> PFloat
      "Decimal"   -> PDecimal
      "BigInt"    -> PBigInt
      "DateTime"  -> PDateTime
      "Json"      -> PJson
      "Bytes"     -> PBytes
      s           -> PEnum s

instance showPrismaType :: Show PrismaType where
  show PInt       = "PInt"
  show PString    = "PString"
  show PBoolean   = "PBoolean"
  show PFloat     = "PFloat"
  show PDecimal   = "PDecimal"
  show PBigInt    = "PBigInt"
  show PDateTime  = "PDateTime"
  show PJson      = "PJson"
  show PBytes     = "PBytes"
  show (PEnum s)  = "PEnum " <> show s
  show (PArray s) = "PArray " <> show s
  show (PMaybe s) = "PMaybe " <> show s


instance showPrismaValue :: Show PrismaValue where
  show (PVString s)    = "PVString " <> s
  show (PVIdent s)     = "PVIdent  " <> s
  show (PVNumber s)    = "PVNumber " <> show s
  show (PVArray a)     = "PVArray  " <> show a
  show (PVEnv s)       = "PVEnv    " <> "env(" <> show s <> ")"
  show (PVFunc n args) = "PVFunc   " <> n <> "(" <> show args <> ")"
  show (PVKeyVal k v)  = "PVKeyVal " <> k <> ": " <> show v

toString :: PrismaValue -> String
toString = case _ of
  PVString s    -> s
  PVIdent s     -> s
  PVNumber s    -> show s
  PVArray a     -> show a
  PVEnv s       -> "env(" <> show s <> ")"
  PVFunc n args -> n <> "(" <> show args <> ")"
  PVKeyVal k v  -> k <> ": " <> show v


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
data Property = Property Key
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
  , fieldType :: PrismaType
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
    "\nGenerator " <> show g
  show (ItemDatasource d) =
    "\nDatasource " <> show d
  show (ItemEnum (EnumDef name e)) =
    "\nEnum " <> name <> ":" <> show e
  show (ItemModel (ModelDef name m)) =
    "\nModel " <> name <> ":" <> show m
  show (ItemView (ModelDef name m)) =
    "\nView " <> name <> ":" <> show m

instance showProperty :: Show Property where
  show (Property key p) = "Property " <> key <> ": " <> show p

instance eqProperty :: Eq Property where
  eq (Property k a) (Property j b) = eq a b && eq k j

