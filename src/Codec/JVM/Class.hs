{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns #-}
module Codec.JVM.Class where

import Data.Binary.Get
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set (Set)
import Data.Word (Word32)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when)

import Codec.JVM.Attr (Attr, putAttr)
import Codec.JVM.Const
import Codec.JVM.ConstPool
import Codec.JVM.Field (FieldInfo, putFieldInfo)
import Codec.JVM.Internal
import Codec.JVM.Method (MethodInfo, putMethodInfo)
import Codec.JVM.Types
import qualified Codec.JVM.ConstPool as CP

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
data ClassFile = ClassFile
  { constants   :: [Const]
  , version     :: Version
  , accessFlags :: Set AccessFlag
  , thisClass   :: IClassName
  , superClass  :: Maybe IClassName
  , interfaces  :: [IClassName]
  , fields      :: [FieldInfo]
  , methods     :: [MethodInfo]
  , attributes  :: Map Text Attr }
  deriving Show

mAGIC :: Word32
mAGIC = 0xCAFEBABE

putClassFile :: ClassFile -> Put
putClassFile ClassFile {..} = do
  putWord32be mAGIC
  putI16 . versionMin $ version
  putI16 . versionMaj $ version
  putI16 . (+) 1 . CP.size $ cp
  putConstPool cp
  putAccessFlags accessFlags
  putIx (classDebug "thisClass") cp . cclass $ thisClass
  putIx (classDebug "superClass") cp . cclass . fromMaybe jlObject $ superClass
  putI16 . L.length $ interfaces
  mapM_ (putIx (classDebug "interface") cp . cclass) interfaces
  putFields
  putMethods
  putI16 . L.length $ attributes
  mapM_ (putAttr (classDebug "attributes") Nothing cp) attributes
  return ()
  where cp         = CP.mkConstPool constants
        classDebug tag = "Class[" ++ tag ++ "][" ++ show thisClass ++ "]"
        putMethods = do
          putI16 . L.length $ methods
          mapM_ (putMethodInfo (classDebug "method") cp) methods
        putFields  = do
          putI16 . L.length $ fields
          mapM_ (putFieldInfo (classDebug "field") cp) fields

getClassName :: Get Text
getClassName = do
  magic <- getWord32be
  when (magic /= mAGIC) $
    fail $ "Invalid .class file MAGIC value: " ++ show magic
  _ <- getWord16be -- minorVersion
  _ <- getWord16be -- majorVersion
  poolSize <- getWord16be
  pool <- getConstPool $ fromIntegral $ poolSize - 1
  _ <- getAccessFlags ATClass -- afs
  classIdx <- getWord16be
  let CClass (IClassName iclsName) = getConstAt classIdx pool
  return iclsName

classFileBS :: ClassFile -> ByteString
classFileBS = toStrict . runPut . putClassFile

classFileCls :: BL.ByteString -> String
classFileCls bs = T.unpack $ runGet getClassName bs

classFileName :: ClassFile -> String
classFileName ClassFile { thisClass = IClassName class_ } = T.unpack class_

superClassName :: ClassFile -> String
superClassName ClassFile { superClass } =
  maybe "java/lang/Object" (T.unpack . unIClassName) superClass
