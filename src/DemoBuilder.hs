{-#LANGUAGE NoImplicitPrelude#-}

module DemoBuilder where

import Prelude hiding (EQ)

import LibMu.Builder

prog :: Builder BuilderState
prog = do
  loadStdPrelude

  [i32, i1] <- getTypedefs ["i32", "i1"]
  [i32_1] <- getConstants ["i32_1"]

  main_sig <- putFuncSig "main.sig" [i32] [i1]
  
  (main, _) <- putFunction "main" "v1" main_sig

  let a = createVariable "a" i32
      cond = createVariable "cond" i1
  entry <- putBasicBlock "entry" [a] Nothing  main

  
  withBasicBlock entry $
    putCmpOp EQ cond a i32_1

  let ret = createVariable "ret" i32
  
  cont <- putIfElse 0 main entry [a] [ret] [ret] [ret] cond (
    putBinOp Add ret a i32_1 Nothing) (
    putBinOp Sub ret a i32_1 Nothing)

  withBasicBlock cont $
    putComment "This is the end!" >>-
    setTermInstRet [ret]

  lift get

fromRight (Right a) = a
