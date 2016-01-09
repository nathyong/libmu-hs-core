{-#LANGUAGE NoImplicitPrelude, ForeignFunctionInterface#-}
module LibMu.Refimpl where

import Prelude (IO)
import LibMu.MuApi (MuVM)
import Foreign

foreign import ccall "refimpl2-start.h mu_refimpl2_new" mu_refimpl2_new :: IO (Ptr MuVM)
foreign import ccall "refimpl2-start.h mu_refimpl2_new_ex" mu_refimpl2_new_ex :: Int64 -> Int64 -> Int64 -> IO (Ptr MuVM)
