{-#LANGUAGE NoImplicitPrelude, ForeignFunctionInterface#-}

module LibMu.StdLib (
  getcharAddr,
  putcharAddr,
  readAddr,
  writeAddr
              ) where

import Prelude (IO, String, fromIntegral)
import Foreign
import Foreign.C.Types
import Text.Printf

foreign import ccall "stdio.h &putchar" putchar :: FunPtr (CInt -> IO Word32)
foreign import ccall "stdio.h &getchar" getchar :: FunPtr (IO Word32)

foreign import ccall "unistd.h &read" read :: FunPtr (CInt -> Ptr () -> CSize -> IO Word64)
foreign import ccall "unistd.h &write" write :: FunPtr (CInt -> Ptr () -> CSize -> IO Word64)

getcharAddr :: String
getcharAddr = addressOf getchar

putcharAddr :: String
putcharAddr = addressOf putchar

readAddr :: String
readAddr = addressOf read

writeAddr :: String
writeAddr = addressOf write

addressOf :: FunPtr a -> String
addressOf fn = printf "0x%016x" (fromIntegral (ptrToIntPtr (castFunPtrToPtr fn)) :: Int)
