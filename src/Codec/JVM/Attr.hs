{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}
module Codec.JVM.Attr where

import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.List (foldl', concat, nub)
import Data.Word(Word8, Word16)

import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T

import Codec.JVM.ASM.Code.CtrlFlow
import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import Codec.JVM.ASM.Code (Code(..))
import Codec.JVM.ASM.Code.Instr (runInstrBCS)
import Codec.JVM.ASM.Code.Types (Offset(..), StackMapTable(..))
import Codec.JVM.Const (Const(..), constTag)
import Codec.JVM.ConstPool (ConstPool, putIx, unpack)
import Codec.JVM.Internal
import Codec.JVM.Types (PrimType(..), FieldType(..), IClassName(..),
                        AccessFlag(..), mkFieldDesc', putAccessFlags)

data Attr
  = ACode
    { maxStack  :: Int
    , maxLocals :: Int
    , code      :: ByteString
    , codeAttrs :: [Attr] }
  | AStackMapTable [(Offset, StackMapFrame)]
  | AInnerClasses InnerClassMap
  | ASignature Text

newtype InnerClassMap = InnerClassMap (Map Text InnerClass)
  deriving (Eq, Show)

innerClassElems :: InnerClassMap -> [InnerClass]
innerClassElems (InnerClassMap m) = Map.elems m

-- Left-biased monoid. Not commutative
instance Monoid InnerClassMap where
  mempty = InnerClassMap mempty
  mappend (InnerClassMap x) (InnerClassMap y) =
    InnerClassMap $ x `Map.union` y

instance Show Attr where
  show (AInnerClasses icm) = "AInnerClasses = " ++ show icm
  show attr = "A" ++ (T.unpack $ attrName attr)

attrName :: Attr -> Text
attrName (ACode _ _ _ _)    = "Code"
attrName (AStackMapTable _) = "StackMapTable"
attrName (AInnerClasses _)  = "InnerClasses"
attrName (ASignature _) = "Signature"

unpackAttr :: Attr -> [Const]
unpackAttr attr@(ACode _ _ _ xs) = (CUTF8 $ attrName attr):(unpackAttr =<< xs)
unpackAttr attr@(ASignature t) = [CUTF8 . attrName $ attr, CUTF8 t]
unpackAttr attr = return . CUTF8 . attrName $ attr

putAttr :: ConstPool -> Attr -> Put
putAttr cp attr = do
  putIx cp $ CUTF8 $ attrName attr
  let xs = runPut $ putAttrBody cp attr
  putI32 . fromIntegral $ LBS.length xs
  putByteString $ LBS.toStrict xs

putAttrBody :: ConstPool -> Attr -> Put
putAttrBody cp (ACode ms ls xs attrs) = do
  putI16 ms
  putI16 ls
  putI32 . fromIntegral $ BS.length xs
  putByteString xs
  putI16 0 -- TODO Exception table
  putI16 $ length attrs
  mapM_ (putAttr cp) attrs
putAttrBody cp (AStackMapTable xs) = do
  putI16 $ length xs
  putStackMapFrames cp xs
putAttrBody cp (AInnerClasses innerClassMap) = do
  putI16 $ length ics
  mapM_ (putInnerClass cp) ics
  where ics = innerClassElems innerClassMap
putAttrBody cp (ASignature t) =
  putIx cp $ CUTF8 t

-- | http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.4
--
-- Offsets are absolute (the delta conversion happen during serialization)

data StackMapFrame
  = SameFrame -- Covers normal & extended
  | SameLocals1StackItem !VerifType -- Covers normal & extended
  | ChopFrame !Int
  | AppendFrame !Word8 ![VerifType]
  | FullFrame ![VerifType] ![VerifType]
  deriving (Eq, Show)

putStackMapFrames :: ConstPool -> [(Offset, StackMapFrame)] -> Put
putStackMapFrames cp xs = (snd $ foldl' f (0, return ()) xs)
  where f (offset, put) (Offset frameOffset, frame)
          = (frameOffset, put *> putFrame frame)
          where delta = frameOffset - (if offset == 0 then 0 else offset + 1)
                putVerifTy = putVerifType cp
                putFrame SameFrame =
                  if delta <= 63
                    then putWord8 $ fromIntegral delta
                    else do
                      putWord8 251
                      putWord16be $ fromIntegral delta
                putFrame (SameLocals1StackItem vt) = do
                  if delta <= 63
                    then putWord8 $ fromIntegral (delta + 64)
                    else do
                      putWord8 247
                      putWord16be $ fromIntegral delta
                  putVerifTy vt
                putFrame (ChopFrame k) = do
                  -- ASSERT (1 <= k <= 3)
                  putWord8 . fromIntegral $ 251 - k
                  putWord16be $ fromIntegral delta
                putFrame (AppendFrame k vts) = do
                  -- ASSERT (1 <= k <= 3)
                  putWord8 $ 251 + k
                  putI16 $ fromIntegral delta
                  traverse_ putVerifTy vts
                putFrame (FullFrame locals stack) = do
                  putWord8 255
                  putI16 $ fromIntegral delta
                  putI16 $ length locals
                  traverse_ putVerifTy locals
                  putI16 $ length stack
                  traverse_ putVerifTy stack

toAttrs :: ConstPool -> Code -> [Attr]
toAttrs cp code = [ACode maxStack' maxLocals' xs attrs]
  where (xs, cf, smt) = runInstrBCS (instr code) cp
        maxLocals' = CF.maxLocals cf
        maxStack' = CF.maxStack cf
        attrs = if null frames then [] else [AStackMapTable frames]
        frames = toStackMapFrames smt

toStackMapFrames :: StackMapTable -> [(Offset, StackMapFrame)]
toStackMapFrames (StackMapTable smt)
  = reverse . fst $ foldl' f ([], c) cfs
  where ((_,c):cfs) = IntMap.toAscList smt
        f (!xs, !cf') (!off, !cf) = ((Offset off, smf):xs, cf)
          where smf = generateStackMapFrame cf' cf

generateStackMapFrame :: CtrlFlow -> CtrlFlow -> StackMapFrame
generateStackMapFrame cf1 cf2
  | sameLocals && sz < 2
  = case sz of
      0 -> SameFrame
      1 -> SameLocals1StackItem stackTop
      _ -> fullFrame
  | otherwise
  = if sz == 0 then
      if lszdiff <= 3  && lszdiff > 0 then
        AppendFrame (fromIntegral lszdiff) $ drop lsz1 clocals2
      else if lszdiff >= -3 && lszdiff < 0 then
        ChopFrame (-lszdiff)
      else fullFrame
    else fullFrame
  where cf1' = mapLocals normaliseLocals cf1
        cf2' = mapLocals normaliseLocals cf2
        (clocals2, cstack2) = compressCtrlFlow cf2'
        (clocals1, _) = compressCtrlFlow cf1'
        fullFrame = FullFrame clocals2 cstack2
        stackTop = last cstack2
        sameLocals = areLocalsSame (locals cf1) (locals cf2)
        lsz1 = length clocals1
        lsz2 = length clocals2
        lszdiff = lsz2 - lsz1
        sz = length cstack2

data InnerClass =
  InnerClass { icInnerClass :: IClassName
             , icOuterClass :: IClassName
             , icInnerName  :: Text
             , icAccessFlags :: [AccessFlag] }
  deriving (Eq, Show)

putInnerClass :: ConstPool -> InnerClass -> Put
putInnerClass cp InnerClass {..} = do
  putIx cp $ CClass icInnerClass
  putIx cp $ CClass icOuterClass
  putIx cp $ CUTF8 icInnerName
  putAccessFlags $ S.fromList icAccessFlags

innerClassInfo :: [Const] -> ([Const], [Attr])
innerClassInfo consts = (nub. concat $ innerConsts, innerClassAttr)
  where
    innerClassAttr = if null innerClasses
                        then []
                        else [ AInnerClasses
                              . InnerClassMap
                              . Map.fromList
                              . map (\ic@InnerClass {..} ->
                                       (icInnerName, ic))
                              $ innerClasses]
    -- TODO: Support generation of private inner classes, not a big priority
    (innerConsts, innerClasses) = unzip $
      mapMaybe (\(CClass icn@(IClassName cn)) ->
                  case T.split (=='$') cn of
                    (outerClass:innerName:_)
                      | T.last innerName /= ';' ->
                        let innerClass =
                              InnerClass { icInnerClass = icn
                                         , icOuterClass = IClassName outerClass
                                         , icInnerName = innerName
                                         , icAccessFlags = [Public, Static] }
                        in Just (unpackInnerClass innerClass, innerClass)
                    _ -> Nothing)
        classConsts
    classConsts = filter (\c -> constTag c == 7) consts

unpackInnerClass :: InnerClass -> [Const]
unpackInnerClass InnerClass {..} =
  (CUTF8 icInnerName) :
    ((unpack $ CClass icOuterClass) ++ (unpack $ CClass icInnerClass))
