{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards, CPP #-}
module Codec.JVM.Attr where

import Data.Maybe (mapMaybe, fromMaybe)
import Data.Semigroup
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.List (foldl', nub)
import Data.Word (Word8)
import Data.Int
import Data.Word
import Data.Char

import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T

import Codec.JVM.ASM.Code.CtrlFlow
import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import Codec.JVM.ASM.Code (Code(..))
import Codec.JVM.ASM.Code.Instr (runInstrBCSLE, ExceptionTableEntry(..))
import Codec.JVM.ASM.Code.Types (Offset(..), StackMapTable(..),
                                 LineNumber(..), LineNumberTable(..),
                                 toListLNT)
import Codec.JVM.Const
import Codec.JVM.ConstPool (ConstPool, putIx, unpack)
import Codec.JVM.Internal
import Codec.JVM.Types (PrimType(..), IClassName(..),
                        AccessFlag(..), mkFieldDesc', putAccessFlags, prim)

import Prelude

type ParameterName = Text

type MParameter = (ParameterName, S.Set AccessFlag)

type ClassName = Text

type InterfaceName = Text

type SuperClassName = Text

data Attr
  = ACode
    { maxStack       :: Int
    , maxLocals      :: Int
    , code           :: ByteString
    , exceptionTable :: [ExceptionTableEntry]
    , codeAttrs      :: [Attr] }
  | AStackMapTable [(Offset, StackMapFrame)]
  | AInnerClasses InnerClassMap
  | ASignature (Signature TypeVariable)
  | AConstantValue Text
  | AMethodParam [MParameter]
  | ALineNumberTable LineNumberTable
  | ASourceFile Text
  | ANormalAnnotations    Visibility [NormalAnnotation]
  | AParameterAnnotations Visibility ParameterAnnotations

------------------------Signatures------------------------------------

type TypeVariable = Text

data Signature a = ClassSig  (ClassSignature a)
                 | MethodSig (MethodSignature a)
                 | FieldSig  (FieldSignature a)
                 deriving Show

-- | JavaTypeSignature
data Parameter a
  = ReferenceParameter (ReferenceParameter a) -- ^ ReferenceTypeSignature
  | PrimitiveParameter PrimType               -- ^ BaseType
  deriving Show

type ObjectType = IClassName

-- | ReferenceTypeSignature
data ReferenceParameter a
  = -- | ClassTypeSignature
    GenericReferenceParameter
      ObjectType                     -- PackageSpecifier & SimpleClassTypeSignature
      [TypeParameter a]              -- TypeArguments
      [ReferenceParameter a]         -- ClassTypeSignatureSuffix
    -- | TypeVariableSignature
  | VariableReferenceParameter a
    -- | ArrayTypeSignature
  | ArrayReferenceParameter    (Parameter a)
  deriving Show

-- | TypeArgument, TypeParameter
data TypeParameter a
  = WildcardTypeParameter (Bound a) -- <?> <? extends A> <? super A>
  | SimpleTypeParameter (ReferenceParameter a)
  deriving Show

data Bound a
  = NotBounded
  | ExtendsBound (ReferenceParameter a)
  | SuperBound   (ReferenceParameter a)
  deriving Show

-- TypeParameters
type TypeVariableDeclarations a = [TypeVariableDeclaration a]

data TypeVariableDeclaration a = TypeVariableDeclaration a [Bound a]
  deriving Show

-- | ** ClassSignature **
data ClassSignature a
  = ClassSignature
      (TypeVariableDeclarations a)  -- TypeParameters
      [ClassParameter a]            -- SuperclassSignature & SuperinterfaceSignature
  deriving Show

type ClassParameter a = ReferenceParameter a

-- | ** MethodSignature **
data MethodSignature a =
  MethodSignature
    (TypeVariableDeclarations a)  -- TypeParameters
    [MethodParameter a]           -- JavaTypeSignature
    (MethodReturn a)              -- Result
    (ThrowsExceptions a)          -- ThrowsSignature
  deriving Show

-- | JavaTypeSignature
type MethodParameter a = Parameter a

-- | Result
type MethodReturn a = Maybe (Parameter a)

-- | ThrowsSignature
type ThrowsExceptions a = [ReferenceParameter a]

-- |  ** FieldSignature **
data FieldSignature a = FieldSignature (FieldParameter a)
  deriving Show

type FieldParameter a = ReferenceParameter a

mconcatMap :: (Monoid m) => (a -> m) -> [a] -> m
mconcatMap f xs = mconcat (map f xs)

generateSignature :: Signature TypeVariable -> Text
generateSignature sig = case sig of
  ClassSig (ClassSignature typeVarDecls classParams) ->
       generateTypeParameters typeVarDecls
    <> generateClassParameters classParams
  MethodSig (MethodSignature typeVarDecls methodParams methodReturn throwExceptions) ->
       generateTypeParameters typeVarDecls
    <> "(" <> mconcatMap generateParameter methodParams <> ")"
    <> maybe "V" generateParameter methodReturn
    <> generateThrowsSignature throwExceptions
  FieldSig (FieldSignature fieldRefParam) ->
       generateReferenceParameter fieldRefParam

generateThrowsSignature :: ThrowsExceptions TypeVariable -> Text
generateThrowsSignature = mconcat . map (T.cons '^' . generateReferenceParameter)

generateClassParameters :: [ReferenceParameter TypeVariable] -> Text
generateClassParameters = mconcat . map generateReferenceParameter

generateTypeParameters :: TypeVariableDeclarations TypeVariable -> Text
generateTypeParameters typeParams
  | length typeParams == 0 = ""
  | otherwise              = "<" <> mconcatMap generateTypeParameter typeParams <> ">"

generateTypeParameter :: TypeVariableDeclaration TypeVariable -> Text
generateTypeParameter (TypeVariableDeclaration identifier bounds)
  = identifier <> mconcatMap generateTypeParameterBound bounds

generateTypeParameterBound :: Bound TypeVariable -> Text
generateTypeParameterBound bounded = ":" <>
  case bounded of
    NotBounded -> ""
    ExtendsBound refParam -> generateReferenceParameter refParam
    SuperBound refParam -> generateReferenceParameter refParam

generateReferenceParameter :: ReferenceParameter TypeVariable -> Text
generateReferenceParameter (GenericReferenceParameter (IClassName className) typeArgs refParams) =
  "L" <> className <> generateTypeArguments typeArgs
      <> mconcatMap (T.cons '.' . generateSimpleClass) refParams <> ";"
  where generateSimpleClass (GenericReferenceParameter (IClassName simpleClassName) tyArgs []) =
          simpleClassName <> generateTypeArguments tyArgs
        generateSimpleClass _ = error "generateReferenceParameter: Not generic."
generateReferenceParameter (ArrayReferenceParameter param) =
  "[" <> generateParameter param
generateReferenceParameter (VariableReferenceParameter identifier) =
  "T" <> identifier <> ";"

generateParameter :: Parameter TypeVariable -> Text
generateParameter (ReferenceParameter refParam) =
  generateReferenceParameter refParam
generateParameter (PrimitiveParameter primType) =
  mkFieldDesc' (prim primType)

generateTypeArguments :: [TypeParameter TypeVariable] -> Text
generateTypeArguments typeParams
  | length typeParams == 0 = ""
  | otherwise              = "<" <> mconcatMap generateTypeArgument typeParams <> ">"

generateTypeArgument :: TypeParameter TypeVariable -> Text
generateTypeArgument (WildcardTypeParameter bound) =
  case bound of
    NotBounded       -> "*"
    ExtendsBound refParam -> "+" <> generateReferenceParameter refParam
    SuperBound refParam   -> "-" <> generateReferenceParameter refParam
generateTypeArgument (SimpleTypeParameter refParam) =
  generateReferenceParameter refParam

---------------------------------------------------------------------------

newtype InnerClassMap = InnerClassMap (Map Text InnerClass)
  deriving (Eq, Show)

innerClassElems :: InnerClassMap -> [InnerClass]
innerClassElems (InnerClassMap m) = Map.elems m

-- Left-biased semigroup. Not commutative
instance Semigroup InnerClassMap where
  (<>) (InnerClassMap x) (InnerClassMap y) =
    InnerClassMap $ x `Map.union` y

instance Monoid InnerClassMap where
  mempty = InnerClassMap mempty
  mappend = (<>)

instance Show Attr where
  show (AInnerClasses icm) = "AInnerClasses = " ++ show icm
  show attr = "A" ++ (T.unpack $ attrName attr)

attrName :: Attr -> Text
attrName (ACode {})      = "Code"
attrName (AStackMapTable {})   = "StackMapTable"
attrName (AInnerClasses {})    = "InnerClasses"
attrName (ASignature {})       = "Signature"
attrName (AConstantValue {})   = "ConstantValue"
attrName (AMethodParam {})     = "MethodParameters"
attrName (ALineNumberTable {}) = "LineNumberTable"
attrName (ASourceFile {})      = "SourceFile"
attrName (ANormalAnnotations vis _)
  = "Runtime" <> T.pack (show vis) <> "Annotations"
attrName (AParameterAnnotations vis _)
  = "Runtime" <> T.pack (show vis) <> "ParameterAnnotations"

unpackAttr :: Attr -> [Const]
unpackAttr attr = CUTF8 (attrName attr) : restAttributes
  where restAttributes =
          case attr of
            ACode { codeAttrs = xs } -> concatMap unpackAttr xs
            ASignature sig -> [CUTF8 $ generateSignature sig]
            ASourceFile f  -> [CUTF8 $ f]
            ANormalAnnotations   _ annots -> concatMap unpackAnnotation annots
            AParameterAnnotations _ (ParameterAnnotations _ annotss) ->
              concatMap (concatMap unpackAnnotation) annotss
            _              -> []

putAttr :: String -> Maybe Int -> ConstPool -> Attr -> Put
putAttr debug mCodeSize cp attr = do
  putIx (debugMsg "name") cp $ CUTF8 $ attrName attr
  let xs = runPut $ putAttrBody (debugMsg "body") mCodeSize cp attr
  putI32 . fromIntegral $ LBS.length xs
  putByteString $ LBS.toStrict xs
  where debugMsg tag = "putAttr[" ++ tag ++ "][" ++ debug ++ "]"

putAttrBody :: String -> Maybe Int -> ConstPool -> Attr -> Put
putAttrBody debug mCodeSize cp attr =
  case attr of
    ACode ms ls xs exceptionTable attrs -> do
      putI16 ms
      putI16 ls
      putI32 . fromIntegral $ BS.length xs
      putByteString xs
      putI16 (length exceptionTable)
      mapM_ (\ExceptionTableEntry {..} -> do
                putI16 eteStartPc
                putI16 eteEndPc
                putI16 eteHandlerPc
                let debugMsg = "putExceptionTable[" ++ debug ++ "]"
                maybe (putI16 0) (putIx debugMsg cp) eteCatchType)
        exceptionTable
      putI16 $ length attrs
      mapM_ (putAttr ("putAttrBody[Code][" ++ debug ++ "]") (Just (BS.length xs)) cp) attrs
    AStackMapTable xs -> do
      let (numFrames, putFrames) =
            putStackMapFrames ("putAttrBody[StackMapTable][" ++ debug ++ "]")
              mCodeSize cp xs
      putI16 numFrames
      putFrames
    AInnerClasses innerClassMap -> do
      let ics = innerClassElems innerClassMap
      putI16 $ length ics
      mapM_ (putInnerClass cp) ics
    ASignature signature ->
      putIx ("putAttrBody[Signature][" ++ debug ++ "]") cp
        $ CUTF8 $ generateSignature signature
    AConstantValue _ -> error "putAttrBody: ConstantValue attribute not implemented!"
    AMethodParam   _ -> error "putAttrBody: MethodParameter attribute not implemented!"
    ALineNumberTable lnt -> do
      let lns = toListLNT lnt
      putI16 $ length lns
      mapM_ (\(Offset pc, LineNumber ln) -> putI16 pc >> putI16 ln) lns
    ASourceFile fileName ->
      putIx ("putAttrBody[SourceFile][" ++ debug ++ "]") cp
        $ CUTF8 $ fileName
    ANormalAnnotations _ annots -> do
      putAnnotations debug cp annots

    AParameterAnnotations _ (ParameterAnnotations numParams annotss) -> do
      putWord8 numParams
      mapM_ (putAnnotations debug cp) annotss

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

putStackMapFrames :: String -> Maybe Int -> ConstPool -> [(Offset, StackMapFrame)] -> (Int, Put)
putStackMapFrames debug mCodeSize cp xs = (numFrames, putFrames)
  where (_nextOffset, numFrames, putFrames) = foldl' f (-1, 0, return ()) xs
        f (offset, !n, put) (Offset frameOffset, frame)
          | frameOffset >= fromMaybe maxBound mCodeSize = (offset, n, put)
          | otherwise = (frameOffset, n + 1, put *> putFrame frame)
          where delta = frameOffset - (if offset == -1 then 0 else offset + 1)
                putVerifTy tag = putVerifType
                  ("StackMapFrame[" ++ tag ++ "][" ++ show frameOffset ++ "]["
                   ++ debug ++ "]") cp
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
                  putVerifTy "SameLocals1StackItem" vt
                putFrame (ChopFrame k) = do
                  -- ASSERT (1 <= k <= 3)
                  putWord8 . fromIntegral $ 251 - k
                  putWord16be $ fromIntegral delta
                putFrame (AppendFrame k vts) = do
                  -- ASSERT (1 <= k <= 3)
                  putWord8 $ 251 + k
                  putI16 $ fromIntegral delta
                  traverse_ (putVerifTy "AppendFrame") vts
                putFrame (FullFrame locals stack) = do
                  putWord8 255
                  putI16 $ fromIntegral delta
                  putI16 $ length locals
                  traverse_
                    (putVerifTy ("FullFrame[locals[" ++ show locals ++ "]]")) locals
                  putI16 $ length stack
                  traverse_ (putVerifTy ("FullFrame[stack[" ++ show stack ++ "]]")) stack

toAttrs :: ConstPool -> Code -> [Attr]
toAttrs cp code = [ACode maxStack' maxLocals' xs etes attrs']
  where (xs, cf, smt, lnt, etes) = runInstrBCSLE (instr code) cp
        maxLocals'               = CF.maxLocals cf
        maxStack'                = CF.maxStack cf
        attrs                    = if null frames then [] else [AStackMapTable frames]
        frames                   = toStackMapFrames smt
        attrs'                   = attrs ++ if lnt == mempty then []
                                      else [ALineNumberTable lnt]

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
  putIx "putInnerClass[innerClass]" cp $ CClass icInnerClass
  putIx "putInnerClass[outerClass]" cp $ CClass icOuterClass
  putIx "putInnerClass[innerName]"  cp $ CUTF8 icInnerName
  putAccessFlags $ S.fromList icAccessFlags

innerClassInfo :: (Text -> Bool) -> [Const] -> ([Const], [Attr])
innerClassInfo ignore consts = (nub. concat $ innerConsts, innerClassAttr)
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
                  if ignore cn
                  then Nothing
                  else
                    case T.break (=='$') cn of
                        (outerClass,innerName')
                          | not (T.null innerName')
                          , let innerName = T.tail innerName'
                          , not (T.null innerName)
                          , T.last innerName /= ';' ->
                              let innerClass =
                                    InnerClass { icInnerClass  = icn
                                               , icOuterClass  = IClassName outerClass
                                               , icInnerName   = innerName
                                               , icAccessFlags = [Public, Static] }
                              in Just (unpackInnerClass innerClass, innerClass)
                        _ -> Nothing)
        classConsts
    classConsts = filter (\c -> constTag c == 7) consts

unpackInnerClass :: InnerClass -> [Const]
unpackInnerClass InnerClass {..} =
  (CUTF8 icInnerName) :
    ((unpack $ CClass icOuterClass) ++ (unpack $ CClass icInnerClass))

data Visibility = Invisible | Visible
  deriving Show

data NormalAnnotation = NormalAnnotation Text [ElementValue]

data ParameterAnnotations = ParameterAnnotations Word8 [[NormalAnnotation]]

data ElementValue =
    BooleanValue    Bool
  | ByteValue       Int8
  | ShortValue      Int16
  | CharValue       Word16
  | IntValue        Int32
  | FloatValue      Float
  | LongValue       Int64
  | DoubleValue     Double
  | StringValue     Text
  | EnumValue       Text Text
  | ClassValue      Text
  | AnnotationValue NormalAnnotation
  | ArrayValue      [ElementValue]

elementTag :: ElementValue -> Char
elementTag val = case val of
  BooleanValue    {} -> 'Z'
  ByteValue       {} -> 'B'
  ShortValue      {} -> 'S'
  CharValue       {} -> 'C'
  IntValue        {} -> 'I'
  FloatValue      {} -> 'F'
  LongValue       {} -> 'J'
  DoubleValue     {} -> 'D'
  StringValue     {} -> 's'
  EnumValue       {} -> 'e'
  ClassValue      {} -> 'c'
  AnnotationValue {} -> '@'
  ArrayValue      {} -> '['

unpackAnnotation :: NormalAnnotation -> [Const]
unpackAnnotation (NormalAnnotation ty vals) =
  CUTF8 ("L" <> ty <> ";") : concatMap unpackElementValue vals

unpackElementValue :: ElementValue -> [Const]
unpackElementValue val =
  case val of
    BooleanValue    b -> intVal $ fromEnum b
    ByteValue       b -> intVal b
    ShortValue      s -> intVal s
    CharValue       c -> intVal c
    IntValue        i -> intVal i
    FloatValue      f -> singVal $ CFloat  f
    LongValue       l -> singVal $ CLong   l
    DoubleValue     d -> singVal $ CDouble d
    StringValue     s -> singVal $ CString s
    EnumValue       ty name -> [CUTF8 ("L" <> ty <> ";"), CUTF8 name]
    ClassValue      name    -> [CUTF8 name]
    AnnotationValue annot   -> unpackAnnotation annot
    ArrayValue      values  -> concatMap unpackElementValue values
  where intVal :: (Integral a) => a -> [Const]
        intVal  = singVal . CInteger . fromIntegral

        singVal = (:[]) . CValue

putAnnotations :: String -> ConstPool -> [NormalAnnotation] -> Put
putAnnotations debug cp annots = do
  putI16 $ length annots
  mapM_ putAnnotation annots
  where putAnnotation annot@(NormalAnnotation ty vals) = do
          putIx ("putAnnotations[" ++ debug ++ "]") cp $
            head $ unpackAnnotation annot
          putI16 $ length vals
          mapM_ putElementValue vals

        putElementValue val = do
          putWord8 $ fromIntegral $ ord $ elementTag val
          let putConst = mapM_ (putIx "putElementValue" cp) $ unpackElementValue val
          case val of
            BooleanValue    {} -> putConst
            ByteValue       {} -> putConst
            ShortValue      {} -> putConst
            CharValue       {} -> putConst
            IntValue        {} -> putConst
            FloatValue      {} -> putConst
            LongValue       {} -> putConst
            DoubleValue     {} -> putConst
            StringValue     {} -> putConst
            EnumValue       {} -> putConst
            ClassValue      {} -> putConst
            AnnotationValue annot  -> putAnnotation annot
            ArrayValue      values -> do
              putI16 $ length values
              mapM_ putElementValue values
