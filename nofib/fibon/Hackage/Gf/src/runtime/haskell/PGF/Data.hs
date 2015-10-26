module PGF.Data (module PGF.Data, module PGF.Expr, module PGF.Type) where

import PGF.CId
import PGF.Expr hiding (Value, Sig, Env, Tree, eval, apply, value2expr)
import PGF.Type

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.Array.IArray
import Data.Array.Unboxed
import Data.List


-- internal datatypes for PGF

-- | An abstract data type representing multilingual grammar
-- in Portable Grammar Format.
data PGF = PGF {
  gflags    :: Map.Map CId Literal,   -- value of a global flag
  absname   :: CId ,
  abstract  :: Abstr ,
  concretes :: Map.Map CId Concr
  }

data Abstr = Abstr {
  aflags  :: Map.Map CId Literal,                     -- ^ value of a flag
  funs    :: Map.Map CId (Type,Int,Maybe [Equation]), -- ^ type, arrity and definition of function
  cats    :: Map.Map CId ([Hypo],[CId])               -- ^ 1. context of a category
                                                      -- ^ 2. functions of a category. The order in the list is important,
                                                      -- this is the order in which the type singatures are given in the source.
                                                      -- The termination of the exhaustive generation might depend on this.
  }

data Concr = Concr {
  cflags       :: Map.Map CId Literal,                               -- value of a flag
  printnames   :: Map.Map CId String,                                -- printname of a cat or a fun
  cncfuns      :: Array FunId CncFun,
  sequences    :: Array SeqId Sequence,
  productions  :: IntMap.IntMap (Set.Set Production),                -- the original productions loaded from the PGF file
  pproductions :: IntMap.IntMap (Set.Set Production),                -- productions needed for parsing
  lproductions :: Map.Map CId (IntMap.IntMap (Set.Set Production)),  -- productions needed for linearization
  cnccats      :: Map.Map CId CncCat,
  totalCats    :: {-# UNPACK #-} !FId
  }

type FId    = Int
type LIndex = Int
type DotPos = Int
data Symbol
  = SymCat {-# UNPACK #-} !Int {-# UNPACK #-} !LIndex
  | SymLit {-# UNPACK #-} !Int {-# UNPACK #-} !LIndex
  | SymKS [String]
  | SymKP [String] [Alternative]
  deriving (Eq,Ord,Show)
data Production
  = PApply  {-# UNPACK #-} !FunId [FId]
  | PCoerce {-# UNPACK #-} !FId
  | PConst  Expr [String]
  deriving (Eq,Ord,Show)
data CncCat = CncCat {-# UNPACK #-} !FId {-# UNPACK #-} !FId {-# UNPACK #-} !(Array LIndex String)
data CncFun = CncFun CId {-# UNPACK #-} !(UArray LIndex SeqId) deriving (Eq,Ord,Show)
type Sequence = Array DotPos Symbol
type FunId = Int
type SeqId = Int

data Alternative =
   Alt [String] [String]
  deriving (Eq,Ord,Show)

data Term =
   R [Term]
 | P Term Term
 | S [Term]
 | K Tokn
 | V Int
 | C Int
 | F CId
 | FV [Term]
 | W String Term
 | TM String
  deriving (Eq,Ord,Show)

data Tokn =
   KS String
 | KP [String] [Alternative]
  deriving (Eq,Ord,Show)


-- merge two PGFs; fails is differens absnames; priority to second arg

unionPGF :: PGF -> PGF -> PGF
unionPGF one two = case absname one of
  n | n == wildCId     -> two    -- extending empty grammar
    | n == absname two -> one {  -- extending grammar with same abstract
      concretes = Map.union (concretes two) (concretes one)
    }
  _ -> one   -- abstracts don't match ---- print error msg

emptyPGF :: PGF
emptyPGF = PGF {
  gflags    = Map.empty,
  absname   = wildCId,
  abstract  = error "empty grammar, no abstract",
  concretes = Map.empty
  }

-- | This is just a 'CId' with the language name.
-- A language name is the identifier that you write in the 
-- top concrete or abstract module in GF after the 
-- concrete/abstract keyword. Example:
-- 
-- > abstract Lang = ...
-- > concrete LangEng of Lang = ...
type Language     = CId

readLanguage :: String -> Maybe Language
readLanguage = readCId

showLanguage :: Language -> String
showLanguage = showCId

fcatString, fcatInt, fcatFloat, fcatVar :: Int
fcatString = (-1)
fcatInt    = (-2)
fcatFloat  = (-3)
fcatVar    = (-4)

isLiteralFCat :: FId -> Bool
isLiteralFCat = (`elem` [fcatString, fcatInt, fcatFloat, fcatVar])
