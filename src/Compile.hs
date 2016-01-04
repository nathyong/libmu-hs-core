{-#LANGUAGE NoImplicitPrelude#-}

module Compile (
  compileProgram
  ) where

import LibMu.Builder
import Parser (Token (..))
import Prelude hiding (EQ)
import Text.Printf

type BFTree = [Token]

compile :: BFTree -> Builder BuilderState
compile tree = do
  --i32 <- putTypeDef "i32" (MuInt 32)
  --i1 <- putTypeDef "i1" (MuInt 1)
  (_:_:_:i32:_, _:_:_:_:_:_:i32_0:i32_1:_,_)<- loadStdPrelude
  
  
  irefi32 <- putTypeDef "irefi32" (IRef i32)
  arri32x3K <- putTypeDef "arri32x3K" (Array i32 30000)
  iref_arri32x3K <- putTypeDef "iref_arri32x3K" (IRef arri32x3K)
  runner <- putGlobal "runner" arri32x3K

  --i32_1 <- putConstant "i32_1" i32 "1"
  --i32_0 <- putConstant "i32_0" i32 "0"

  char <- putTypeDef "char" (MuInt 8)
  addrType <- putTypeDef "AddrType" (MuInt 64)

  getchar_address <- putConstant "getchar_address" addrType "0x400490"
  putchar_address <- putConstant "putchar_address" addrType "0x400460"

  getchar_sig <- putFuncSig "getchar.sig" [] [char]
  putchar_sig <- putFuncSig "putchar.sig" [char] []

  getchar_ptr <- putTypeDef "getchar_ptr" (UFuncPtr getchar_sig)
  putchar_ptr <- putTypeDef "putchar_ptr" (UFuncPtr putchar_sig)

  main_sig <- putFuncSig "main.sig" [] []
  (main_v1, _) <- putFunction "main" "v1" main_sig

  let index = createVariable "index" i32

  entry <- putBasicBlock "entry" [] Nothing main_v1
  block0 <- putBasicBlock "block0000" [index] Nothing main_v1

  withBasicBlock entry $
    setTermInstBranch block0 [i32_0]
  
  _ <- putTokens main_v1 block0 1 tree
  
  lift get

putTokens :: Function -> Block -> Int -> [Token] -> Builder (Block, Int)
putTokens func block count prog = case prog of
  [] -> return (block, count)
  t:ts -> do

    [i32, i1, char, addrType, putchar_ptr, getchar_ptr] <- getTypedefs ["i32", "i1", "char", "AddrType", "putchar_ptr", "getchar_ptr"]
    
    i32_1 <- getConstant "i32_1"
    i32_0 <- getConstant "i32_0"

    putchar_address <- getConstant "putchar_address"
    getchar_address <- getConstant "getchar_address"

    putchar_sig <- getFuncSig "putchar.sig"
    getchar_sig <- getFuncSig "getchar.sig"
    
    irefi32 <- getTypedef "irefi32"
    
    runner <- getGlobal "runner"

    let index = createVariable "index" i32

    nxtBlock <- putBasicBlock (printf "block%.5d" count) [index] Nothing func
    
    case t of
      Increment -> do
        let arrElem = createVariable "arrElem" irefi32
            arrInc = createVariable "arrInc" i32
            
        withBasicBlock block $ 
          putComment "Increment" >>-
          putGetElemIRef arrElem False runner index Nothing >>-
          putAtomicRMW ADD arrInc False SEQ_CST arrElem i32_1 Nothing >>-
          setTermInstBranch nxtBlock [index]

        putTokens func nxtBlock (succ count) ts
      Decrement -> do
        let arrElem = createVariable "arrElem" irefi32
            arrDec = createVariable "arrDec" i32
            
        withBasicBlock block $
          putComment "Decrement" >>-
          putGetElemIRef arrElem False runner index Nothing >>-
          putAtomicRMW SUB arrDec False SEQ_CST arrElem i32_1 Nothing >>-
          setTermInstBranch nxtBlock [index]

        putTokens func nxtBlock (succ count) ts
      PtrInc -> do
        let indInc = createVariable "indInc" i32
        
        withBasicBlock block $
          putComment "Ptr Increment" >>-
          putBinOp Add indInc index i32_1 Nothing >>-
          setTermInstBranch nxtBlock [indInc]

        putTokens func nxtBlock (succ count) ts
      PtrDec -> do
        let indDec = createVariable "indDec" i32
        
        withBasicBlock block $
          putComment "Ptr Decrement" >>-
          putBinOp Sub indDec index i32_1 Nothing >>-
          setTermInstBranch nxtBlock [indDec]

        putTokens func nxtBlock (succ count) ts
      Loop prog -> do
        let arrElem = createVariable "arrElem" irefi32
            arrVal = createVariable "arrVal" i32
            cmpRes = createVariable "cmpRes" i1

        (finBlock, finCount) <- putTokens func nxtBlock (succ count) prog

        withBasicBlock finBlock $
          putComment (printf "Loop Back : %d" (pred count)) >>-
          setTermInstBranch block [index]
        
        contBlock <- putBasicBlock (printf "block%.5d" finCount) [index] Nothing func

        withBasicBlock contBlock $
          putComment (printf "Loop Continue : %d" (pred count))
        
        withBasicBlock block $
          putComment (printf "Loop Begin : %d" (pred count)) >>-
          putGetElemIRef arrElem False runner index Nothing >>- --iref i32
          putLoad arrVal False Nothing arrElem Nothing >>-
          putCmpOp EQ cmpRes arrVal i32_0 >>-
          setTermInstBranch2 cmpRes contBlock [index] nxtBlock [index]

        putTokens func contBlock (succ finCount) ts

      PutChar -> do
        let callee = createVariable "callee" putchar_ptr
            arrElem = createVariable "arrElem" irefi32
            arrVal = createVariable "arrInc" i32
            arrValChar = createVariable "arrValChar" char
            
        withBasicBlock block $
          putComment "Put Char" >>-
          putGetElemIRef arrElem False runner index Nothing >>-
          putLoad arrVal False Nothing arrElem Nothing >>-
          putConvOp TRUNC arrValChar char arrVal Nothing >>-
          putConvOp PTRCAST callee putchar_ptr putchar_address Nothing >>-
          putCCall [] Mu putchar_ptr putchar_sig putchar_address [arrValChar] Nothing Nothing >>-
          setTermInstBranch nxtBlock [index]
          
        putTokens func nxtBlock (succ count) ts
        
      GetChar -> do
        let callee = createVariable "callee" getchar_ptr
            arrElem = createVariable "arrElem" irefi32
            arrVal = createVariable "arrVal" i32
            arrValChar = createVariable "arrValChar" char
            
        withBasicBlock block $
          putComment "Get Char" >>-
          putConvOp PTRCAST callee putchar_ptr putchar_address Nothing >>-
          putCCall [arrValChar] Mu putchar_ptr putchar_sig putchar_address [] Nothing Nothing >>-
          putConvOp ZEXT arrVal i32 arrValChar Nothing >>-
          putGetElemIRef arrElem False runner index Nothing >>-
          putStore False Nothing arrElem arrVal Nothing >>-
          setTermInstBranch nxtBlock [index]
          
        putTokens func nxtBlock (succ count) ts
        
compileProgram :: [Token] -> Either Error BuilderState
compileProgram prog = runBuilder (compile prog) emptyBuilderState
