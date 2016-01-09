{-#LANGUAGE NoImplicitPrelude#-}

module Compile (
  compileProgram
  ) where

import LibMu.Builder
import LibMu.StdLib
import Parser (Token (..))
import Prelude hiding (EQ)
import Text.Printf

type BFTree = [Token]


compile :: BFTree -> Builder BuilderState
compile tree = do
  (_:_:_:i32:_, _:_:_:_:_:_:i32_0:_:_,_)<- loadStdPrelude
  
  arri32x30K <- putTypeDef "arri32x30K" (Array i32 30000)
  _ <- putGlobal "runner" arri32x30K

  --addrType <- putTypeDef "AddrType" (MuInt 64)

  getchar_sig <- putFuncSig "getchar.sig" [] [i32]
  putchar_sig <- putFuncSig "putchar.sig" [i32] [i32]

  getchar_fp <- putTypeDef "getchar_ptr" (UFuncPtr getchar_sig)
  putchar_fp <- putTypeDef "putchar_ptr" (UFuncPtr putchar_sig)

  _ <- putConstant "getchar_address" getchar_fp getcharAddr
  _ <- putConstant "putchar_address" putchar_fp putcharAddr

  main_sig <- putFuncSig "main.sig" [] []
  (main_v1, _) <- putFunction "main" "v1" main_sig

  entry <- putBasicBlock "entry" Nothing main_v1
  block0 <- putBasicBlock "block0000" Nothing main_v1

  updateBasicBlock entry $
    setTermInstBranch block0 [i32_0]

  [index] <- updateBasicBlock block0 $
    putParams [i32]
  
  (ind, ret) <- putTokens index block0 tree

  updateBasicBlock ret $ do
    putComminst [] "uvm.thread_exit" [] [] [] [] Nothing Nothing
    setTermInstRet [ind]
  
  lift get

putTokens :: SSAVariable -> Block -> [Token] -> Builder (SSAVariable, Block)
putTokens index block prog = case prog of
  [] -> return (index, block)
  t:ts -> do

    [i32, putchar_ptr, getchar_ptr] <- getTypedefs ["i32", "putchar_ptr", "getchar_ptr"]
    
    i32_1 <- getConstant "i32_1"
    i32_0 <- getConstant "i32_0"

    putchar_address <- getConstant "putchar_address"
    getchar_address <- getConstant "getchar_address"

    putchar_sig <- getFuncSig "putchar.sig"
    getchar_sig <- getFuncSig "getchar.sig"
    
    runner <- getGlobal "runner"
    
    case t of
      Increment -> do
        updateBasicBlock block $ do
          putComment "Increment"
          arrElem <- putGetElemIRef False runner index Nothing
          arrOldVal <- putLoad False Nothing arrElem Nothing
          arrNewVal <- putBinOp Add arrOldVal i32_1 Nothing
          putStore False Nothing arrElem arrNewVal Nothing
          
        putTokens index block ts
      Decrement -> do          
        updateBasicBlock block $ do
          putComment "Decrement"
          arrElem <- putGetElemIRef False runner index Nothing
          arrOldVal <- putLoad False Nothing arrElem Nothing
          arrNewVal <- putBinOp Sub arrOldVal i32_1 Nothing
          putStore False Nothing arrElem arrNewVal Nothing
          
        putTokens index block ts
      PtrInc -> do       
        index' <- updateBasicBlock block $ do
          putComment "Ptr Increment"
          putBinOp Add index i32_1 Nothing
          
        putTokens index' block ts
      PtrDec -> do
        index' <- updateBasicBlock block $ do
          putComment "Ptr Decrement"
          putBinOp Sub index i32_1 Nothing
          
        putTokens index' block ts
      Loop in_prog -> do

        n <- getVarID
        
        (cond, loop, cont, _, ind) <- putWhile [index] block (\loopBlock contBlock -> do
          [ind] <- putParams [i32]
          putComment (printf "Loop Begin : %d" n)
          arrElem <- putGetElemIRef False runner index Nothing
          arrVal <- putLoad False Nothing arrElem Nothing
          cmpRes <- putCmpOp EQ arrVal i32_0
          setTermInstBranch2 cmpRes contBlock [ind] loopBlock [ind])
                                   (\_ -> do
          [ind] <- putParams [i32]
          return ind
          )
        
        (loopInd, loopFin) <- putTokens ind loop in_prog
        
        updateBasicBlock loopFin $
          setTermInstBranch cond [loopInd]
 
        [indCont] <- updateBasicBlock cont $
          putParams [i32]
        
        putTokens indCont cont ts
        
      PutChar -> do
        tmp <- uniqueVariable "tmp" i32
        updateBasicBlock block $ do
          putComment "Put Char"
          arrElem <- putGetElemIRef False runner index Nothing
          arrVal <- putLoad False Nothing arrElem Nothing
          putCCall [tmp] Mu putchar_ptr putchar_sig putchar_address [arrVal] Nothing Nothing
          
        putTokens index block ts
        
      GetChar -> do
        arrVal <- uniqueVariable "arrVal" i32
        
        updateBasicBlock block $ do
          putComment "Get Char"          
          putCCall [arrVal] Mu getchar_ptr getchar_sig getchar_address [] Nothing Nothing
          arrElem <- putGetElemIRef False runner index Nothing
          putStore False Nothing arrElem arrVal Nothing
          
        putTokens index block ts
        
compileProgram :: [Token] -> Either Error BuilderState
compileProgram prog = runBuilder (compile prog) emptyBuilderState
