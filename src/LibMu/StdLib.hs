{-#LANGUAGE NoImplicitPrelude, ForeignFunctionInterface#-}

module LibMu.StdLib (
  getcharAddr,
  putcharAddr
              ) where

import Prelude
import Foreign
import Foreign.C.Types
import Text.Printf

foreign import ccall "stdio.h &putchar" putchar :: FunPtr (CInt -> IO Int)
foreign import ccall "stdio.h &getchar" getchar :: FunPtr (IO Int)

getcharAddr :: String
getcharAddr = printf "0x%016x" (fromIntegral (ptrToIntPtr (castFunPtrToPtr getchar)) :: Int)

putcharAddr :: String
putcharAddr = printf "0x%016x" (fromIntegral (ptrToIntPtr (castFunPtrToPtr putchar)) :: Int)
