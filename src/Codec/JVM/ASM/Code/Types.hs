{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}
module Codec.JVM.ASM.Code.Types where

import Codec.JVM.ASM.Code.CtrlFlow (CtrlFlow)

import Data.Semigroup
import Data.Text (Text)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Prelude

newtype Label = Label Int
  deriving Show

mkLabel :: Int -> Label
mkLabel = Label

newtype Offset = Offset { unOffset :: Int } -- absolute
  deriving (Eq, Num, Show, Ord, Enum, Integral, Real)

newtype StackMapTable = StackMapTable (IntMap CtrlFlow)
  deriving Show

-- Right-biased union
union' :: IntMap a -> IntMap a -> IntMap a
union' = IntMap.unionWith (flip const)

instance Semigroup StackMapTable where
  (<>) (StackMapTable x) (StackMapTable y)
    = StackMapTable $ union' x y

-- TODO: Implement a strict fold for mconcat
instance Monoid StackMapTable where
  mempty = StackMapTable mempty
  mappend = (<>)

insertSMT :: Int -> CtrlFlow -> StackMapTable -> StackMapTable
insertSMT k v (StackMapTable sm) = StackMapTable $ IntMap.insert k v sm

newtype LineNumber = LineNumber Int
  deriving (Show, Eq)

mkLineNumber :: Int -> LineNumber
mkLineNumber = LineNumber

newtype LineNumberTable = LineNumberTable (IntMap LineNumber)
  deriving (Show, Eq)

instance Semigroup LineNumberTable where
  (<>) (LineNumberTable x) (LineNumberTable y)
    = LineNumberTable $ union' x y

instance Monoid LineNumberTable where
  mempty = LineNumberTable mempty
  mappend = (<>)

toListLNT :: LineNumberTable -> [(Offset,LineNumber)]
toListLNT (LineNumberTable m) = map (\(off,ln) -> (Offset off,ln)) $ IntMap.assocs m

insertLNT :: Offset -> LineNumber -> LineNumberTable -> LineNumberTable
insertLNT (Offset off) ln (LineNumberTable lnt) =
  LineNumberTable $ IntMap.insert off ln lnt

newtype LabelTable = LabelTable { unLabelTable :: IntMap Offset }
  deriving Show

instance Semigroup LabelTable where
  (<>) (LabelTable x) (LabelTable y)
    = LabelTable $ union' x y

instance Monoid LabelTable where
  mempty = LabelTable mempty
  mappend = (<>)

toLT :: [(Label, Offset)] -> LabelTable
toLT labels = LabelTable $ IntMap.fromList labels'
  where labels' = map (\(Label l, o) -> (l, o)) labels

unionsLT :: [LabelTable] -> LabelTable
unionsLT = LabelTable
         . foldlStrict union' mempty
         . map unLabelTable

insertLT :: Label -> Offset -> LabelTable -> LabelTable
insertLT (Label l) off (LabelTable lt) = LabelTable $ IntMap.insert l off lt

lookupLT :: Label -> LabelTable -> Offset
lookupLT (Label l) (LabelTable lt) = IntMap.findWithDefault (Offset 0) l lt

isDifferentLT :: Label -> Offset -> LabelTable -> Bool
isDifferentLT (Label l) off (LabelTable lt)
  | Just off' <- IntMap.lookup l lt
  , off == off'
  = False
  | otherwise = True

differenceLT :: LabelTable -> LabelTable -> LabelTable
differenceLT (LabelTable lt1) (LabelTable lt2) = LabelTable $
  IntMap.differenceWith (\a b -> if a /= b then Just a else Nothing) lt1 lt2

sizeLT :: LabelTable -> Int
sizeLT (LabelTable lt) = IntMap.size lt

-- Taken from containers package
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs
{-# INLINE foldlStrict #-}

newtype ExceptionTable = ExceptionTable [(Label, Label, Label, Maybe Text)]

instance Semigroup ExceptionTable where
  (<>) (ExceptionTable x) (ExceptionTable y)
    = ExceptionTable $ x ++ y

instance Monoid ExceptionTable where
  mempty = ExceptionTable mempty
  mappend = (<>)

insertIntoET :: Label -> Label -> Label -> Maybe Text -> ExceptionTable -> ExceptionTable
insertIntoET start end handler constant (ExceptionTable etes) =
  ExceptionTable $ (start, end, handler, constant) : etes

toListET :: ExceptionTable -> [(Label, Label, Label, Maybe Text)]
toListET (ExceptionTable etes) = reverse etes

-- Special means that you don't record the usage of the goto.
-- NotSpecial means you do (typical use case).
data Special = Special | NotSpecial
  deriving Eq

data LastBranch = NoBranch | HasBranch BranchType Offset
  deriving Show

data BranchType = Return | Goto | GotoW
  deriving (Eq, Show)

branchSize :: BranchType -> Offset
branchSize Return = 1
branchSize Goto   = 3
branchSize GotoW  = 5
