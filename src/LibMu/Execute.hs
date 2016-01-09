{-#LANGUAGE NoImplicitPrelude#-}

module LibMu.Execute where

import Prelude (IO, String, length, fromIntegral, ($))
import LibMu.MuApi
import LibMu.Refimpl


import Foreign
import Foreign.C.String

{-
MuThreadRefValue thread = ctx->new_thread(ctx, stack, MU_REBIND_PASS_VALUES,
            NULL, 0, NULL);
-}
runBundle :: String -> IO ()
runBundle file = do
  mvm <- mu_refimpl2_new
  mvmVal <- peek mvm
  ctx <- mkNewContext (new_context mvmVal) mvm
  ctxVal <- peek ctx
  withCString file $ \fileStr -> do
    mkLoadBundle (load_bundle ctxVal) ctx fileStr (fromIntegral $ length file)
  withCString "@main" $ \main_name -> do
    main_id <- mkCtxIdOf (ctx_id_of ctxVal) ctx main_name
    func <- mkHandleFromFunc (handle_from_func ctxVal) ctx main_id
    stack <- mkNewStack (new_stack ctxVal) ctx func
    _ <- mkNewThread (new_thread ctxVal) ctx stack 1 nullPtr 0 nullPtr
  
    mkExecute (execute  mvmVal) mvm
  
  mkCloseContext (close_context ctxVal) ctx
