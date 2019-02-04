{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, UnboxedTuples, RecordWildCards, MultiParamTypeClasses, FlexibleContexts, NamedFieldPuns, MagicHash #-}
module Codec.JVM.ASM.Code.Instr where

import Control.Monad.IO.Class
import qualified Control.Monad.Fail as Fail
import Control.Monad.State
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Maybe(fromMaybe)
import Data.List(sortBy)
import Data.Int(Int32)
import Data.Ord(comparing)
import Data.Semigroup

import GHC.Base

import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap

import Codec.JVM.ASM.Code.CtrlFlow (CtrlFlow, Stack, VerifType(..), Stack(..))
import Codec.JVM.ASM.Code.Types
import Codec.JVM.Const (Const(..))
import Codec.JVM.Internal (packI16, packI32)
import Codec.JVM.Opcode (Opcode, opcode)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Types
import qualified Codec.JVM.Types as T

import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

import Prelude

data InstrState =
  InstrState { isByteCode        :: !ByteString
             , isStackMapTable   :: StackMapTable
             , isOffset          :: !Offset
             , isCtrlFlow        :: CtrlFlow
             , isLabelTable      :: LabelTable
             , isLastBranch      :: LastBranch
             , isRunAgain        :: Bool
             , isNextLabel       :: Int
             , isLineNumberTable :: LineNumberTable
             , isExceptionTable  :: ExceptionTable }

newtype InstrM a = InstrM { runInstrM :: ConstPool -> InstrState -> (# a, InstrState #) }

newtype Instr = Instr { unInstr :: InstrM () }

instance Functor InstrM where
  fmap = liftM

instance Applicative InstrM where
  pure x = InstrM $ \_ s -> (# x, s #)
  (<*>) = ap

instance Monad InstrM where
  return = pure
  (InstrM m) >>= f =
    InstrM $ \e s ->
      case m e s of
        (# x, s' #) ->
          case runInstrM (f x) e s' of
            (# x', s'' #) -> (# x', s'' #)

  fail = Fail.fail

instance Fail.MonadFail InstrM where
  fail = error

instance MonadIO InstrM where
  liftIO (IO io) = InstrM $ \_ s ->
    case io realWorld# of
      (# _, a #) -> (# a, s #)

instance MonadState InstrState InstrM where
  get = InstrM $ \_ s -> (# s, s #)
  put s' = InstrM $ \_ _ -> (# (), s' #)

instance MonadReader ConstPool InstrM where
  ask = InstrM $ \e s -> (# e, s #)
  local f (InstrM m) = InstrM $ \e s -> m (f e) s

instance Semigroup Instr where
  (<>) (Instr rws0) (Instr rws1) = Instr $ do
    rws0
    rws1

instance Monoid Instr where
  mempty = Instr $ return ()
  mappend = (<>)

instance Show Instr where
  show _ = "Instructions"

withOffset :: (Int -> Instr) -> Instr
withOffset f = Instr $ do
  InstrState { isOffset = Offset offset } <- get
  unInstr $ f offset

emptyInstrState :: InstrState
emptyInstrState =
  InstrState { isByteCode        = mempty
             , isStackMapTable   = mempty
             , isOffset          = 0
             , isCtrlFlow        = CF.empty
             , isLabelTable      = mempty
             , isLastBranch      = NoBranch
             , isRunAgain        = False
             , isNextLabel       = 1
             , isLineNumberTable = mempty
             , isExceptionTable  = mempty }

getBCS :: InstrState -> (ByteString, CtrlFlow, StackMapTable)
getBCS InstrState{..} = (isByteCode, isCtrlFlow, isStackMapTable)

getBCSL :: InstrState -> (ByteString, CtrlFlow, StackMapTable, LineNumberTable)
getBCSL InstrState{..} = (isByteCode, isCtrlFlow, isStackMapTable, isLineNumberTable)

getBCSLE :: InstrState -> (ByteString, CtrlFlow, StackMapTable, LineNumberTable,
                           [ExceptionTableEntry])
getBCSLE InstrState{..} =
  (isByteCode, isCtrlFlow, isStackMapTable, isLineNumberTable,
   toETEs isExceptionTable isLabelTable)

runInstr :: Instr -> ConstPool -> InstrState
runInstr instr cp = multiPass 0 emptyInstrState { isRunAgain = True }
  where multiPass :: Int -> InstrState -> InstrState
        multiPass n s@InstrState { isRunAgain, isLabelTable = lt }
          | isRunAgain =
            case runInstr' instr cp $ emptyInstrState { isLabelTable = lt } of
              s' -> multiPass (n + 1) s'
          | otherwise = s

runInstrBCS :: Instr -> ConstPool -> (ByteString, CtrlFlow, StackMapTable)
runInstrBCS instr cp = getBCS $ runInstr instr cp

runInstrBCSL :: Instr -> ConstPool -> (ByteString, CtrlFlow, StackMapTable, LineNumberTable)
runInstrBCSL instr cp = getBCSL $ runInstr instr cp

runInstrBCSLE :: Instr -> ConstPool -> (ByteString, CtrlFlow, StackMapTable, LineNumberTable, [ExceptionTableEntry])
runInstrBCSLE instr cp = getBCSLE $ runInstr instr cp

runInstr' :: Instr -> ConstPool -> InstrState -> InstrState
runInstr' (Instr m) e s = case runInstrM m e s of (# _, s' #) -> s'

runInstrBCS' :: Instr -> ConstPool -> InstrState -> (ByteString, CtrlFlow, StackMapTable)
runInstrBCS' instr e s = getBCS $ runInstr' instr e s

runInstrBCSL' :: Instr -> ConstPool -> InstrState ->
                 (ByteString, CtrlFlow, StackMapTable, LineNumberTable)
runInstrBCSL' instr e s = getBCSL $ runInstr' instr e s

recordBranch :: BranchType -> InstrM ()
recordBranch bt = do
  off <- getOffset
  modify' $ \s -> s { isLastBranch = HasBranch bt (Offset off) }

saveLastBranch :: InstrM LastBranch
saveLastBranch = do
  InstrState { isLastBranch = lb } <- get
  modify' $ \s -> s { isLastBranch = NoBranch }
  return lb

resetLastBranch :: LastBranch -> InstrM ()
resetLastBranch lb = modify' $ \s -> s { isLastBranch = lb }

recordLineNumber' :: LineNumber -> InstrM ()
recordLineNumber' ln = do
  off <- getOffset
  modify' $ \s@InstrState { isLineNumberTable = lnt } ->
              s { isLineNumberTable = insertLNT (Offset off) ln lnt }

recordLineNumber :: LineNumber -> Instr
recordLineNumber = Instr . recordLineNumber'

gotoInstr :: Special -> InstrM ()
gotoInstr = gotoInstrSpec OP.goto

gotoWInstr :: Special -> InstrM ()
gotoWInstr = gotoInstrSpec OP.goto_w

gotoInstrSpec :: Opcode -> Special -> InstrM ()
gotoInstrSpec opc special = do
  when (special == NotSpecial) $
    recordBranch (if opc == OP.goto_w then GotoW else Goto)
  op' opc

gotoInstrGen :: Special -> Int -> InstrM ()
gotoInstrGen special offset
  | offset >= 0 && offset <= 3 = return ()
  | outsideGotoRange offset = do
    gotoWInstr special
    writeBytes . packI32 $ offset
  | otherwise = do
    gotoInstr special
    writeBytes . packI16 $ offset

returnInstr :: Opcode -> Instr
returnInstr opc = Instr $ do
  recordBranch Return
  op' opc

modifyStack' :: (Stack -> Stack) -> InstrM ()
modifyStack' f = ctrlFlow' $ CF.mapStack f

modifyStack :: (Stack -> Stack) -> Instr
modifyStack = Instr . modifyStack'

-- TODO:
-- Account for Instr & Instr being empty
-- Account for jumpoffset being > 2^15 - 1
gbranch :: (FieldType -> Stack -> Stack)
        -> FieldType -> Opcode -> Instr -> Instr -> Instr
gbranch f ft oc ok ko = Instr $ do
  [defaultLabel, okLabel] <- mkSystemLabels 2
  jumpOffset <- offsetToLabel okLabel
  lb <- saveLastBranch
  unInstr ifop
  InstrState { isCtrlFlow = cf
             , isLabelTable = lt } <- get
  writeBytes . packI16 $ jumpOffset
  (koCF, koLT, mkoLB) <- withCFState cf lt $ unInstr $
    ko <> condGoto Special defaultLabel
  (okCF, okLT, mokLB) <- withCFState cf lt $ unInstr $
    putLabel okLabel <> ok
  putCtrlFlow' $ CF.merge cf [okCF, koCF]
  mergeLabels [koLT, okLT]
  unInstr $ putLabel defaultLabel
  resetLastBranch $ fromMaybe lb (selectLatestLB mkoLB mokLB)
  where ifop = op oc <> modifyStack (f ft)

bytes :: ByteString -> Instr
bytes = Instr . writeBytes

ix :: Const -> Instr
ix c = Instr $ do
  cp <- ask
  writeBytes . packI16 $ CP.ix $ CP.unsafeIndex "ix" c cp

op :: Opcode -> Instr
op = Instr . op'

op' :: Opcode -> InstrM ()
op' = writeBytes . BS.singleton . opcode

ctrlFlow' :: (CtrlFlow -> CtrlFlow) -> InstrM ()
ctrlFlow' f = modify' $ \s@InstrState { isCtrlFlow = cf }  -> s { isCtrlFlow = f cf }

ctrlFlow :: (CtrlFlow -> CtrlFlow) -> Instr
ctrlFlow = Instr . ctrlFlow'

withStack :: ([VerifType] -> [VerifType]) -> Instr
withStack f = modifyStack (\s -> let stack'     = f (stackVal s)
                                     stackSize' = length stack'
                                 in s { stackVal  = stack'
                                      , stackMax  = max (stackMax s) stackSize'
                                      , stackSize = stackSize' })

initCtrl :: (CtrlFlow -> CtrlFlow) -> Instr
initCtrl f = Instr $ do
  unInstr $ ctrlFlow f
  modify' $ \s@InstrState { isCtrlFlow = cf
                          , isStackMapTable = smt } ->
    s { isStackMapTable = insertSMT (-1) cf smt }
  -- NOTE: The (-1) is done as a special case for when a stack map frame has to
  --       be generated for offset 0.

putCtrlFlow :: CtrlFlow -> Instr
putCtrlFlow = Instr . putCtrlFlow'

putCtrlFlow' :: CtrlFlow -> InstrM ()
putCtrlFlow' = ctrlFlow' . const

withCFState :: CtrlFlow -> LabelTable -> InstrM () -> InstrM (CtrlFlow, LabelTable, Maybe LastBranch)
withCFState cf lt instr = do
  InstrState { isCtrlFlow = cf', isLabelTable = lt' } <- get
  modify' $ \s -> s { isCtrlFlow = cf, isLabelTable = lt }
  instr
  s' <- get
  modify' $ \s -> s { isCtrlFlow = cf', isLabelTable = lt' }
  let mLastBranch
        | ifLastBranch (isOffset s') lb = Just lb
        | otherwise = Nothing
        where lb = isLastBranch s'
  return (isCtrlFlow s', isLabelTable s', mLastBranch)

incOffset :: Int -> Instr
incOffset = Instr . incOffset'

incOffset' :: Int -> InstrM ()
incOffset' i =
  modify' $ \s@InstrState { isOffset = Offset off } ->
              s { isOffset = Offset $ off + i}

write :: ByteString -> StackMapTable -> InstrM ()
write bs smfs = do
  incOffset' $ BS.length bs
  modify' $ \s@InstrState { isByteCode = bs'
                          , isStackMapTable = smfs' } ->
    s { isByteCode = bs' <> bs
      , isStackMapTable = smfs' <> smfs }

writeBytes :: ByteString -> InstrM ()
writeBytes bs = write bs mempty

markStackMapFrame :: Instr
markStackMapFrame = Instr writeStackMapFrame

writeStackMapFrame :: InstrM ()
writeStackMapFrame = do
  modify' $ \s@InstrState { isOffset = Offset offset
                          , isCtrlFlow = cf
                          , isStackMapTable = smt } ->
    s { isStackMapTable = insertSMT offset cf smt }

getOffset :: InstrM Int
getOffset = do
  Offset offset <- gets isOffset
  return offset

addExceptionHandler :: Label -> Label -> Label -> Maybe FieldType -> InstrM ()
addExceptionHandler start end handler mft = do
  let f (Just (ObjectType (IClassName text))) = Just text
      f _ = Nothing
  modify' $ \s@InstrState { isExceptionTable = et } ->
              s { isExceptionTable = insertIntoET start end handler (f mft) et }

type BranchMap = IntMap.IntMap Instr

toInt32AscList :: IntMap.IntMap a -> [(Int, a)]
toInt32AscList = sortBy (comparing ((fromIntegral :: Int -> Int32) . fst)) . IntMap.toAscList

tableswitch :: BranchMap -> Maybe Instr -> Instr
tableswitch = switches OP.tableswitch header
  where header ~(defaultLabel:_:labels) branchMap relOffsetToLabel = do
          debug $ liftIO $ print ("tableswitch", branchMap, low, high)
          writeI32 low
          writeI32 high
          go labels [low..high]
          where go ls@(l:ls') (x:xs)
                  | IntMap.member x branchMap = do
                    relOffsetToLabel l >>= writeI32
                    go ls' xs
                  | otherwise = do
                    relOffsetToLabel defaultLabel >>= writeI32
                    go ls xs
                go _ _ = return ()
                low  = fst . IntMap.findMin $ branchMap
                high = fst . IntMap.findMax $ branchMap

lookupswitch :: BranchMap -> Maybe Instr -> Instr
lookupswitch = switches OP.lookupswitch header
  where header ~(_:_:labels) branchMap relOffsetToLabel = do
          writeI32 (IntMap.size branchMap)
          let keys = map fst $ toInt32AscList branchMap
          forM_ (zip keys labels) $ \(int, l) -> do
            writeI32 int
            relOffsetToLabel l >>= writeI32

switches :: Opcode -> ([Label] -> BranchMap -> (Label -> InstrM Int) -> InstrM ())
         -> BranchMap -> Maybe Instr -> Instr
switches opc f branchMap deflt = Instr $ do
  baseOffset <- getOffset
  lb <- saveLastBranch
  unInstr $ op opc
  modifyStack' $ CF.pop jint
  InstrState { isOffset = offset
             , isCtrlFlow = cf
             , isLabelTable = lt } <- get
  -- Align to 4-byte boundary
  let padding = (4 - (offset `mod` 4)) `mod` 4
      numBranches = IntMap.size branchMap
      branchList = map snd $ toInt32AscList branchMap
  writeBytes . BS.pack . replicate (fromIntegral padding) $ 0
  ls@(defaultLabel:breakLabel:labels) <- mkSystemLabels (1 + 1 + numBranches)
  let relOffsetToLabel = offsetToLabel' (Offset baseOffset)
  relOffsetToLabel defaultLabel >>= writeI32
  f ls branchMap relOffsetToLabel
  let branches = (defaultLabel, fromMaybe mempty deflt)
               : zip labels branchList
  cfsAndLtsAndLbs <- forM branches $ \(l, i) ->
    withCFState cf lt $ unInstr $
      putLabel l <> i <> condGoto Special breakLabel
  let (cfs, lts, mlbs) = unzip3 cfsAndLtsAndLbs
  putCtrlFlow' $ CF.merge cf cfs
  mergeLabels lts
  unInstr $ putLabel breakLabel
  resetLastBranch $ fromMaybe lb (selectLatestLBs mlbs)

lookupLabel :: Label -> InstrM Offset
lookupLabel l = do
  InstrState { isLabelTable = lt } <- get
  return $ lookupLT l lt

mergeLabels :: [LabelTable] -> InstrM ()
mergeLabels tables = do
  debug $ do
    InstrState { isLabelTable = table } <- get
    liftIO $ print ("mergeLabels", map (`differenceLT` table) tables)
  modify' $ \s@InstrState { isLabelTable = table
                          , isRunAgain   = ra } ->
    let diffTables = map (`differenceLT` table) tables
        updates = any (\m -> sizeLT m > 0) diffTables
    in s { isLabelTable = unionsLT (table : diffTables)
         , isRunAgain   = updateRunAgain ra updates }

gotoLabel :: Special -> Label -> Instr
gotoLabel special label = Instr $ offsetToLabel label >>= gotoInstrGen special

condGoto :: Special -> Label -> Instr
condGoto special l = Instr $ do
  InstrState { isLastBranch, isOffset } <- get
  unless (ifLastBranch isOffset isLastBranch) $
    unInstr (gotoLabel special l)

putLabel' :: Label -> Instr
putLabel' l = Instr $ do
  debug $ do
    offset <- getOffset
    liftIO $ print ("putLabel'", l, offset)
  modify' $ \s@InstrState { isLabelTable = lt
                          , isRunAgain = ra
                          , isOffset = off } ->
              s { isLabelTable = insertLT l off lt
                , isRunAgain = updateRunAgain ra (isDifferentLT l off lt) }

putLabel :: Label -> Instr
putLabel l = Instr $ do
  unInstr (putLabel' l)
  writeStackMapFrame

offsetToLabel :: Label -> InstrM Int
offsetToLabel label = do
  offset <- getOffset
  offsetToLabel' (Offset offset) label

offsetToLabel' :: Offset -> Label -> InstrM Int
offsetToLabel' (Offset offset) label = do
  Offset labelOffset <- lookupLabel label
  debug $
    liftIO $ print ("offsetToLabel'", label, labelOffset,
                    offset, labelOffset - offset)
  return $ labelOffset - offset

ifLastBranch :: Offset -> LastBranch -> Bool
ifLastBranch _       NoBranch          = False
ifLastBranch offset (HasBranch bt off) = off == (offset - branchSize bt)

selectLatestLB :: Maybe LastBranch -> Maybe LastBranch -> Maybe LastBranch
selectLatestLB (Just _) b@(Just _) = b
selectLatestLB _        _          = Nothing

selectLatestLBs :: [Maybe LastBranch] -> Maybe LastBranch
selectLatestLBs = foldl1 selectLatestLB

outsideGotoRange :: Int -> Bool
outsideGotoRange offset = offset > 32767 || offset < -32768

mkSystemLabels :: Int -> InstrM [Label]
mkSystemLabels n = do
  s@InstrState { isNextLabel }<- get
  put s { isNextLabel = isNextLabel + n }
  return $ map (\x -> Label (- (isNextLabel + x))) [0..(n - 1)]

writeI16, writeI32 :: Int -> InstrM ()
writeI32 = writeBytes . packI32
writeI16 = writeBytes . packI16

-- For debugging purposes
whenClass :: String -> InstrM () -> InstrM ()
whenClass cls m = do
  InstrState { isCtrlFlow } <- get
  when (CF.getThis (CF.locals isCtrlFlow) == cls) m

debug :: InstrM () -> InstrM ()
debug = const (return ())

updateRunAgain :: Bool -> Bool -> Bool
updateRunAgain = (||)

toETEs :: ExceptionTable -> LabelTable -> [ExceptionTableEntry]
toETEs et lt =
  map (\(start, end, handler, constant) ->
         ExceptionTableEntry { eteStartPc   = unOffset $ lookupLT start   lt
                             , eteEndPc     = unOffset $ lookupLT end     lt
                             , eteHandlerPc = unOffset $ lookupLT handler lt
                             , eteCatchType = fmap (CClass . IClassName) constant })
  $ toListET et

data ExceptionTableEntry
  = ExceptionTableEntry { eteStartPc   :: Int
                        , eteEndPc     :: Int
                        , eteHandlerPc :: Int
                        , eteCatchType :: Maybe Const -- Must be CClass
                        }

tryFinally :: (Instr, Instr, Instr) -> Instr -> Instr -> Instr
tryFinally (storeCode, loadCode, throwCode) tryCode finallyCode = Instr $ do
  [startLabel, endLabel, finallyLabel, defaultLabel] <- mkSystemLabels 4
  lb <- saveLastBranch
  InstrState { isCtrlFlow   = cf
             , isLabelTable = lt } <- get
  (tryCF, tryLT, mtryLB) <- withCFState cf lt $ unInstr $
       putLabel' startLabel
    <> tryCode
    <> putLabel' endLabel
    <> finallyCode
    <> condGoto Special defaultLabel
  (finallyCF, finallyLT, mfinallyLB) <- withCFState cf lt $ unInstr $
       ctrlFlow (CF.mapStack (CF.push jthrowable))
    <> putLabel finallyLabel
    <> storeCode
    <> finallyCode
    <> loadCode
    <> throwCode
  addExceptionHandler startLabel endLabel finallyLabel Nothing
  putCtrlFlow' $ CF.merge cf [tryCF, finallyCF]
  mergeLabels [tryLT, finallyLT]
  unInstr $ putLabel defaultLabel
  resetLastBranch $ fromMaybe lb (selectLatestLB mtryLB mfinallyLB)

synchronized :: (Instr, Instr, Instr, Instr, Instr) -> Instr -> Instr
synchronized (storeCode, loadCode, throwCode, monEnter, monExit) syncCode = Instr $ do
  let tryCode     = syncCode
      finallyCode = monExit
  [startLabel, endLabel, finallyLabel, finallyEndLabel, defaultLabel]
    <- mkSystemLabels 5
  lb <- saveLastBranch
  unInstr $ monEnter
  InstrState { isCtrlFlow   = cf
             , isLabelTable = lt } <- get
  (tryCF, tryLT, mtryLB) <- withCFState cf lt $ unInstr $
       putLabel' startLabel
    <> tryCode
    <> finallyCode
    <> putLabel' endLabel
    <> condGoto Special defaultLabel
  (finallyCF, finallyLT, mfinallyLB) <- withCFState cf lt $ unInstr $
       ctrlFlow (CF.mapStack (CF.push jthrowable))
    <> putLabel finallyLabel
    <> storeCode
    <> finallyCode
    <> putLabel' finallyEndLabel
    <> loadCode
    <> throwCode
  addExceptionHandler startLabel endLabel finallyLabel Nothing
  addExceptionHandler finallyLabel finallyEndLabel finallyLabel Nothing
  putCtrlFlow' $ CF.merge cf [tryCF, finallyCF]
  mergeLabels [tryLT, finallyLT]
  unInstr $ putLabel defaultLabel
  resetLastBranch $ fromMaybe lb (selectLatestLB mtryLB mfinallyLB)

throwable :: Text
throwable = pack "java/lang/Throwable"

jthrowable :: FieldType
jthrowable = T.obj throwable
