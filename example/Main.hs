{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.JVM
import Codec.JVM.ASM.Code.Instr
import Codec.JVM.ConstPool
import Data.Monoid((<>))

import Data.Text (Text)
import qualified Data.ByteString as BS

main :: IO ()
main = BS.writeFile "Test.class" $ classFileBS classFile

mainClass :: Text
mainClass = "Test"

classFile :: ClassFile
classFile = mkClassFileWithAttrs java7 [Public, Super] mainClass Nothing [] [] [srcFile]
  [
    mkMethodDef mainClass [Public, Static] "main" [jarray jstring] void $
        startLabel loop
     <> markStackMap
     <> emitLineNumber (ln 5)  
     <> iconst jint 1
     <> iconst jint 1
     <> iadd
     <> ifeq (goto loop) mempty
     <> vreturn
  ]
  where srcFile = mkSourceFileAttr "Main.hs" 
        loop = mkLabel 1
        ln = mkLineNumber

dumpStackMap :: Code -> IO ()
dumpStackMap (Code consts instr) = do
  putStrLn "Control Flow:"
  print cf
  where cp = mkConstPool consts
        (_, cf, smt) = runInstrBCS instr cp
