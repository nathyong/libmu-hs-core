{-#LANGUAGE NoImplicitPrelude#-}

module LibMu.Execute where

import Prelude (IO, String, length, fromIntegral, ($))
import LibMu.MuApi
import LibMu.Refimpl

import Foreign
import Foreign.C.String

runBundle :: String -> IO ()
runBundle file = do
  mvm <- mu_refimpl2_new
  ctx <- call0 mkNewContext new_context mvm
  
  ctxVal <- peek ctx
  withCString file $ \fileStr -> do
    mkLoadBundle (load_bundle ctxVal) ctx fileStr (fromIntegral $ length file)
  withCString "@main" $ \main_name -> do
    main_id <- call1 mkCtxIdOf ctx_id_of ctx main_name
    func <- mkHandleFromFunc (handle_from_func ctxVal) ctx main_id
    stack <- mkNewStack (new_stack ctxVal) ctx func
    _ <- mkNewThread (new_thread ctxVal) ctx stack 1 nullPtr 0 nullPtr
  
    call0 mkExecute execute mvm
  mkCloseContext (close_context ctxVal) ctx
