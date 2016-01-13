{-#LANGUAGE NoImplicitPrelude#-}

module Compile (
  compileProgram
  ) where

import LibMu.Builder
import LibMu.StdLib
import Parser (Token (..))
import Prelude hiding (EQ)
import Text.Printf

import Control.Monad.Trans.Reader

type BFTree = [Token]


compile :: BFTree -> Builder BuilderState
compile tree = do
  (_:i8:_:i32:i64:_, _:_:_:_:_:_:i32_0:_:_,_)<- loadStdPrelude

  arri8x30K <- putTypeDef "arri8x30K" (Array i8 30000)
  _ <- putGlobal "runner" arri8x30K
  (buffer, iref_i8) <- putGlobal "buffer" i8
  
  uptr_i8 <- putTypeDef "charPtr" (UPtr i8)

  read_sig <- putFuncSig "read.sig" [i32, uptr_i8, i64] [i64]
  write_sig <- putFuncSig "write.sig" [i32, uptr_i8, i64] [i64]

  read_fp <- putTypeDef "read_ptr" (UFuncPtr read_sig)
  write_fp <- putTypeDef "write_ptr" (UFuncPtr write_sig)

  _ <- putConstant "read_address" read_fp readAddr
  _ <- putConstant "write_address" write_fp writeAddr
  
  main_sig <- putFuncSig "main.sig" [] [i32]
  (main_v1, _) <- putFunction "main" "v1" main_sig

  entry <- putBasicBlock "entry" Nothing main_v1
  block0 <- putBasicBlock "block0000" Nothing main_v1
  
  buff_ptr <- createVariable "buff_ptr" uptr_i8
  
  updateBasicBlock entry $ do
    putComminst [buff_ptr] "uvm.native.pin" [] [iref_i8] [] [buffer] Nothing Nothing
    setTermInstBranch block0 [i32_0, buff_ptr]
    
  [index, bPtr] <- updateBasicBlock block0 $
    putParams [i32, uptr_i8]
  
  (ind, _, ret) <- runReaderT (putTokens tree) (index, bPtr, block0)

  updateBasicBlock ret $ do
    putComminst [] "uvm.native.unpin" [] [iref_i8] [] [buffer] Nothing Nothing
    putComminst [] "uvm.thread_exit" [] [] [] [] Nothing Nothing
    
    setTermInstRet [ind]

  lift get

putTokens :: [Token] -> ReaderT (SSAVariable, SSAVariable, Block) Builder (SSAVariable, SSAVariable, Block)
putTokens prog = case prog of
  [] -> ask
  t:ts -> do

    (index, buff_ptr, block) <- ask
    
    [i32, i64, uptr_i8, read_ptr, write_ptr] <- lift $ getTypedefs ["i32", "i64", "charPtr", "read_ptr", "write_ptr"]
    
    i32_1 <- lift $ getConstant "i32_1"
    i32_0 <- lift $ getConstant "i32_0"
    i64_1 <- lift $ getConstant "i64_1"
    i8_0 <- lift $ getConstant "i8_0"
    i8_1 <- lift $ getConstant "i8_1"
    read_address <- lift $ getConstant "read_address"
    write_address <- lift $ getConstant "write_address"

    read_sig <- lift $ getFuncSig "read.sig"
    write_sig <- lift $ getFuncSig "write.sig"
    
    runner <- lift $ getGlobal "runner"

    case t of
      Increment -> do
        lift $ updateBasicBlock block $ do
          putComment "Increment"
          arrElem <- putGetElemIRef False runner index Nothing
          arrOldVal <- putLoad False Nothing arrElem Nothing
          arrNewVal <- putBinOp Add arrOldVal i8_1 Nothing
          putStore False Nothing arrElem arrNewVal Nothing
          
        putTokens ts
      Decrement -> do          
        lift $ updateBasicBlock block $ do
          putComment "Decrement"
          arrElem <- putGetElemIRef False runner index Nothing
          arrOldVal <- putLoad False Nothing arrElem Nothing
          arrNewVal <- putBinOp Sub arrOldVal i8_1 Nothing
          putStore False Nothing arrElem arrNewVal Nothing
          
        putTokens ts
      PtrInc -> do       
        index' <- lift $ updateBasicBlock block $ do
          putComment "Ptr Increment"
          putBinOp Add index i32_1 Nothing

        local (const (index', buff_ptr, block)) (putTokens ts)
      PtrDec -> do
        index' <- lift $ updateBasicBlock block $ do
          putComment "Ptr Decrement"
          putBinOp Sub index i32_1 Nothing

        local (const (index', buff_ptr, block)) (putTokens ts)
      Loop in_prog -> do

        n <- lift $ getVarID
        
        (cond, loop, cont, _, (ind, bPtr)) <- lift $ putWhile [index, buff_ptr] block (\loopBlock contBlock -> do
          [ind, bPtr] <- putParams [i32, uptr_i8]
          putComment (printf "Loop Begin : %d" n)
          arrElem <- putGetElemIRef False runner ind Nothing
          arrVal <- putLoad False Nothing arrElem Nothing
          cmpRes <- putCmpOp EQ arrVal i8_0
          setTermInstBranch2 cmpRes contBlock [ind, bPtr] loopBlock [ind, bPtr])
                                   (\_ -> do
          [ind, bPtr] <- putParams [i32, uptr_i8]
          return (ind, bPtr)
          )

        (loopInd, loopBPtr, loopFin) <- lift $ runReaderT (putTokens in_prog) (ind, bPtr, loop)
        
        lift $ updateBasicBlock loopFin $
          setTermInstBranch cond [loopInd, loopBPtr]
 
        [indCont, bPtrCont] <- lift $ updateBasicBlock cont $
          putParams [i32, uptr_i8]
        
        local (const (indCont, bPtrCont, cont)) (putTokens ts)        

      PutChar -> do
        tmp <- lift $ createVariable "tmp" i64        
        lift $ updateBasicBlock block $ do
          putComment "Put Char"
          arrElem <- putGetElemIRef False runner index Nothing
          arrVal <- putLoad False Nothing arrElem Nothing
          putStore True Nothing buff_ptr arrVal Nothing
          putCCall [tmp] Mu write_ptr write_sig write_address [i32_1, buff_ptr, i64_1] Nothing Nothing
        putTokens ts
        
      GetChar -> do
        arrVal <- lift $ createVariable "arrVal" i64
        
        lift $ updateBasicBlock block $ do
          putComment "Get Char"
          putCCall [arrVal] Mu read_ptr read_sig read_address [i32_0, buff_ptr, i64_1] Nothing Nothing
          arrElem <- putGetElemIRef False runner index Nothing
          buffVal <- putLoad True Nothing buff_ptr Nothing
          
          putStore False Nothing arrElem buffVal Nothing
          
        putTokens ts

compileProgram :: [Token] -> Either Error BuilderState
compileProgram prog = runBuilder (compile prog) emptyBuilderState
