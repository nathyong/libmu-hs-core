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
  ctx <- newContext mvm
  
  withCString file $ \fileStr -> do
    loadBundle ctx fileStr (fromIntegral $ length file)
  withCString "@main" $ \main_name -> do
    main_id <- ctxIdOf ctx main_name
    func <- handleFromFunc ctx (fromIntegral main_id)
    stack <- newStack ctx func
    _ <- newThread ctx stack 1 nullPtr 0 nullPtr
  
    execute mvm
  closeContext ctx
