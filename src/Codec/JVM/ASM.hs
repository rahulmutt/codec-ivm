{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-----------------------------------------------------------------------------
-- | Usage:
--
-- You can assemble a java class:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import Data.Binary.Put (runPut)
-- import Data.Foldable (fold)
-- import qualified Data.ByteString.Lazy as BS
--
-- import Codec.JVM.ASM (mkClassFile, mkMethodDef)
-- import Codec.JVM.ASM.Code
-- import Codec.JVM.Class (ClassFile, putClassFile)
-- import Codec.JVM.Method (AccessFlag(..))
-- import Codec.JVM.Types
--
-- mainClass :: ClassFile
-- mainClass = mkClassFile java8 [] "HelloWorld" Nothing
--   [ mkMethodDef [Public, Static] "main" [arr.obj $ "java/lang/String"] void $ fold
--     [ getstatic systemOut
--     , bipush jint 42
--     , invokevirtual printlnI
--     , vreturn ]
--   ]
--     where
--       systemOut   = mkFieldRef  "java/lang/System"    "out"     (obj "java/io/PrintStream")
--       printlnI    = mkMethodRef "java/io/PrintStream" "println" [prim JInt]                   void
--
-- main :: IO ()
-- main = BS.writeFile "HelloWorld.class" $ runPut . putClassFile $ mainClass
-- @
--
module Codec.JVM.ASM where

import Data.Foldable (fold)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Codec.JVM.ASM.Code (Code, vreturn, invokespecial, dup, gload)
import Codec.JVM.Attr (attrName, innerClassInfo, unpackAttr, Attr(AInnerClasses,ASourceFile))
import Codec.JVM.Class (ClassFile(..))
import Codec.JVM.Const (Const(..))
import Codec.JVM.ConstPool (unpackClassName)
import Codec.JVM.Method (MethodInfo(..), unpackMethodInfo)
import Codec.JVM.Field (FieldInfo(FieldInfo), unpackFieldInfo)
import Codec.JVM.Types

import qualified Codec.JVM.ASM.Code as Code

mkClassFile :: Version
            -> [AccessFlag]
            -> Text         -- class name
            -> Maybe Text   -- superclass, java/lang/Object is nothing
            -> [Text]       -- Interfaces
            -> [FieldDef]
            -> [MethodDef]
            -> ClassFile
mkClassFile v afs tc' sc' is' fds mds = mkClassFileWithAttrs v afs tc' sc' is' fds [] mds
  
mkClassFileWithAttrs :: Version
                     -> [AccessFlag]
                     -> Text         -- class name
                     -> Maybe Text   -- superclass, java/lang/Object is nothing
                     -> [Text]       -- Interfaces
                     -> [FieldDef]
                     -> [Attr]
                     -> [MethodDef]
                     -> ClassFile
mkClassFileWithAttrs v afs tc' sc' is' fds attrs' mds =
  ClassFile cs v (Set.fromList afs) tc sc is fis mis attrs
  where
      is = map IClassName is'
      tc = IClassName tc'
      sc = IClassName <$> sc'
      cs' = ccs ++ mdcs ++ mics ++ fdcs ++ fics where
        ccs = concat $ [unpackClassName tc, unpackClassName $ fromMaybe jlObject sc]
                     ++ map unpackClassName is
        mdcs = mds >>= unpackMethodDef
        mics = mis >>= unpackMethodInfo
        fdcs = fds >>= unpackFieldDef
        fics = fis >>= unpackFieldInfo

      (cs'', innerAttrs) = innerClassInfo cs'
      attrs'' = attrs' ++ innerAttrs
      acs = concatMap unpackAttr attrs''
      attrs =  Map.fromList . map (\attr -> (attrName attr, attr)) $ attrs''
      cs = cs'' ++ cs' ++ acs
      mis = f <$> mds where
        f (MethodDef afs' n' (MethodDesc d) code ats) =
           MethodInfo (Set.fromList afs') n' (Desc d) code ats

      fis = f <$> fds where
        f (FieldDef afs' n' (FieldDesc d)) =
          FieldInfo (Set.fromList afs') n' (Desc d) []

data MethodDef = MethodDef {
  mdAccessFlags :: [AccessFlag],
  mdMethodName  :: UName,
  mdDescriptor  :: MethodDesc,
  mdCode        :: Code,
  mdAttributes  :: [Attr] }
  deriving Show

mkMethodDef :: Text -> [AccessFlag] -> Text -> [FieldType] -> ReturnType -> Code -> MethodDef
mkMethodDef cls afs n fts rt cs = mkMethodDef' afs n (mkMethodDesc fts rt) code
  where code = Code.initCtrlFlow (Static `elem` afs) ((obj cls) : fts) <> cs

mkMethodDef' :: [AccessFlag] -> Text -> MethodDesc -> Code -> MethodDef
mkMethodDef' afs n md c = MethodDef afs (UName n) md c []

-- TODO: Validate attributes to avoid collisions
addAttrsToMethodDef :: [Attr] -> MethodDef -> MethodDef
addAttrsToMethodDef attrs methodDef = methodDef { mdAttributes = mdAttributes methodDef ++ attrs }

unpackMethodDef :: MethodDef -> [Const]
unpackMethodDef (MethodDef _ (UName n') (MethodDesc d) code attrs) =
  [CUTF8 n', CUTF8 d] ++ Code.consts code ++ concatMap unpackAttr attrs

data FieldDef = FieldDef [AccessFlag] UName FieldDesc
  deriving Show

mkFieldDef :: [AccessFlag] -> Text -> FieldType -> FieldDef
mkFieldDef afs n ft = mkFieldDef' afs n (mkFieldDesc ft)

mkFieldDef' :: [AccessFlag] -> Text -> FieldDesc -> FieldDef
mkFieldDef' afs n fd = FieldDef afs (UName n) fd

unpackFieldDef :: FieldDef -> [Const]
unpackFieldDef (FieldDef _ (UName n') (FieldDesc d)) = [CUTF8 n', CUTF8 d]

mkDefaultConstructor :: Text -> Text -> MethodDef
mkDefaultConstructor thisClass superClass =
  mkMethodDef thisClass [Public] "<init>" [] void $ fold
  [ gload thisFt 0,
    invokespecial $ mkMethodRef superClass "<init>" [] void,
    vreturn ]
  where thisFt = obj thisClass

-- This leaves this on the operand stack for the code to consume
mkConstructorDef :: Text -> Text -> [FieldType] -> Code -> MethodDef
mkConstructorDef thisClass superClass args code =
  mkMethodDef thisClass [Public] "<init>" args void $
     gload thisFt 0
  <> dup thisFt
  <> invokespecial (mkMethodRef superClass "<init>" [] void)
  <> code
  <> vreturn
  where thisFt = obj thisClass

addInnerClasses :: [ClassFile] -> ClassFile -> ClassFile
addInnerClasses innerClasses
  outerClass@ClassFile { attributes = outerAttributes
                       , constants = outerConstants }
  = case maybeAttr of
      (newInnerAttr:_)  ->
        outerClass
        { constants = (unpackAttr newInnerAttr) ++ consts ++ outerConstants
        , attributes =
            Map.insertWith mergeInnerClasses
              (attrName newInnerAttr) newInnerAttr outerAttributes }
      _ -> outerClass
  where mergeInnerClasses
          (AInnerClasses newInnerClassMap)
          (AInnerClasses oldInnerClassMap)
          = AInnerClasses $ oldInnerClassMap <> newInnerClassMap
        mergeInnerClasses _ _ = error "Bad inner class attributes"
        (consts, maybeAttr) = innerClassInfo classConsts
        classConsts = map (\ClassFile {..} -> CClass thisClass) innerClasses

mkSourceFileAttr :: Text -> Attr
mkSourceFileAttr = ASourceFile
