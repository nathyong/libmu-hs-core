{-#LANGUAGE NoImplicitPrelude, ForeignFunctionInterface#-}
module LibMu.Refimpl where

import Prelude (IO)
import LibMu.MuApi (MuVM, Ptr, Int64_t, CLong(..))

foreign import ccall "refimpl2-start.h mu_refimpl2_new" mu_refimpl2_new :: IO (Ptr MuVM)
foreign import ccall "refimpl2-start.h mu_refimpl2_new_ex" mu_refimpl2_new_ex :: Int64_t -> Int64_t -> Int64_t -> IO (Ptr MuVM)
