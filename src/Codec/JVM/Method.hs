{-# LANGUAGE RecordWildCards #-}
module Codec.JVM.Method where

import Data.Set (Set)
import qualified Data.List as L

import Codec.JVM.Attr
import Codec.JVM.Const
import Codec.JVM.ASM.Code
import Codec.JVM.ConstPool
import Codec.JVM.Internal
import Codec.JVM.Types


-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.6

data MethodInfo = MethodInfo
  { miAccessFlags :: Set AccessFlag
  , miName        :: UName
  , miDescriptor  :: Desc
  , miCode        :: Code
  , miAttributes  :: [Attr] }
  deriving Show

-- TODO: This is very ugly hack
unpackMethodInfo :: MethodInfo -> [Const]
unpackMethodInfo _  = [ CUTF8 $ attrName (ACode a a a a)
                      , CUTF8 $ attrName (AStackMapTable a)
                      , CUTF8 $ attrName (ALineNumberTable a)]
  where a = undefined

putMethodInfo :: String -> ConstPool -> MethodInfo -> Put
putMethodInfo debug cp MethodInfo { miName = UName methodName
                                  , miDescriptor = Desc methodDescriptor
                                  , .. } = do
  putAccessFlags miAccessFlags
  putIx "putMethodInfo[name]" cp $ CUTF8 methodName
  putIx "putMethodInfo[descriptor]" cp $ CUTF8 methodDescriptor
  putI16 . L.length $ attributes
  mapM_ (putAttr ("Method[" ++ show methodName ++ "][" ++ debug ++ "]") Nothing cp) attributes
  where attributes = toAttrs cp miCode ++ miAttributes
