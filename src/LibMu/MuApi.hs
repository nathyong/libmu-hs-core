{-#LANGUAGE NoImplicitPrelude, DeriveGeneric, ForeignFunctionInterface#-}

module LibMu.MuApi (
  MuVM(..),
  MuCtx(..),

  newContext,

  idOf,

  nameOf,

  setTrapHandler,

  execute,
  
  ctxIdOf,
  ctxNameOf,
  closeContext,
  loadBundle,
  loadHail,
  handleFromSInt8,
  handleFromUInt8,
  handleFromSInt16,
  handleFromUInt16,
  handleFromSInt32,
  handleFromUInt32,
  handleFromSInt64,
  handleFromUInt64,
  handleFromFloat,
  handleFromDouble,
  handleFromPtr,
  handleFromFp,

  handleToSint8,
  handleToUint8,
  handleToSint16,
  handleToUint16,
  handleToSint32,
  handleToUint32,
  handleToSint64,
  handleToUint64,
  handleToFloat,
  handleToDouble,
  handleToPtr,
  handleToFp,
 
  handleFromConst,
  handleFromGlobal,
  handleFromFunc,
  handleFromExpose,

  deleteValue,

  refEq,

  refUlt,

  extractValue,
  insertValue,

  extractElement,
  insertElement,

  newFixed,
  newHybrid,

  refCast,

  getIref,
  getFieldIref,
  getElemIref,
  shiftIref,
  getVarPartIref,
  
  load,
  store,
  cmpxchg,
  atomicrmw,
  fence,

  newStack,
  newThread,
  killStack,

  newCursor,
  newFrame,
  copyCursor,
  closeCursor,

  curFunc,
  curFuncVer,
  curInst,
  dumpKeepalives,

  popFramesTo,
  pushFrame,

  tr64IsFp,
  tr64IsInt,
  tr64isRef,
  tr64ToFp,
  tr64ToInt,
  tr64ToRef,
  tr64ToTag,
  
  tr64FromFp,
  tr64FromInt,
  tr64FromRef,

  enableWatchpoint,
  disableWatchpoint,

  pin,
  unpin,

  expose,
  unexpose,

  Int8_t,
  UInt8_t,
  Int16_t,
  UInt16_t,
  Int32_t,
  UInt32_t,
  Int64_t,
  UInt64_t,

  MuValue,
  MuIntValue,
  MuFloatValue,
  MuDoubleValue,
  MuRefValue,
  MuIRefValue,
  MuStructValue,
  MuArrayValue,
  MuVectorValue,
  MuFuncRefValue,
  MuThreadRefValue,
  MuStackRefValue,
  MuFCRefValue,
  MuTagRef64Value,
  MuUPtrValue,
  MuUFPValue,

  MuID,
  Hs_MuID,
  MuName,
  Hs_MuName,

  MuCPtr,
  MuCFP,

  MuTrapHandlerResult,

  -- Used by new_thread
  MuHowToResume,

  -- Used by MuTrapHandler
  MuValuesFreer,

  -- Signature of the trap handler
  MuTrapHandler,

  MuMemOrd,

  MuAtomicRMWOp,

  MuCallConv
  ) where

import Prelude (IO)

import Foreign
import Foreign.C.String (CString)
import Foreign.C.Types

import GHC.Generics
import Foreign.CStorable

--Stdint.h typedefs
type Int8_t = CChar
type UInt8_t = CUChar
type Int16_t = CShort
type UInt16_t = CUShort
type Int32_t = CInt
type UInt32_t = CUInt
type Int64_t = CLong
type UInt64_t = CULong

type MuValue = Ptr ()              -- Any Mu value
type MuIntValue = Ptr ()           -- int<n>
type MuFloatValue = Ptr ()         -- float
type MuDoubleValue = Ptr ()        -- double
type MuRefValue = Ptr ()           -- ref<T>
type MuIRefValue = Ptr ()          -- iref<T>
type MuStructValue = Ptr ()        -- struct<...>
type MuArrayValue = Ptr ()         -- array<T l>
type MuVectorValue = Ptr ()        -- vector<T l>
type MuFuncRefValue = Ptr ()       -- funcref<sig>
type MuThreadRefValue = Ptr ()     -- threadref
type MuStackRefValue = Ptr ()      -- stackref
type MuFCRefValue = Ptr ()         -- framecursorref
type MuTagRef64Value = Ptr ()      -- tagref64
type MuUPtrValue = Ptr ()          -- uptr
type MuUFPValue = Ptr ()           -- ufuncptr

-- Identifiers and names of Mu
type MuID = CUInt
type Hs_MuID = Int32
type MuName = CString
type Hs_MuName = CString
-- Convenient types for the void* type and the void(*)() type in C
type MuCPtr = Ptr ()
type MuCFP = FunPtr (IO ())

-- Result of a trap handler
type MuTrapHandlerResult = CInt

-- Used by new_thread
type MuHowToResume = CInt

-- Used by MuTrapHandler
type MuValuesFreer = FunPtr (Ptr MuValue -> MuCPtr -> IO ())

-- Signature of the trap handler
type MuTrapHandler = FunPtr (Ptr MuCtx -> MuThreadRefValue ->
                            MuStackRefValue -> CInt -> Ptr MuTrapHandlerResult ->
                            Ptr MuStackRefValue -> Ptr (Ptr MuValue) -> Ptr CInt ->
                            Ptr MuValuesFreer -> Ptr MuCPtr -> Ptr MuRefValue ->
                            MuCPtr -> IO ())

-- Memory orders
type MuMemOrd = CInt

type MuAtomicRMWOp = CInt

type MuCallConv = CInt

data MuVM = MuVM {
  header :: Ptr (),
  new_context :: FunPtr (Ptr MuVM -> IO (Ptr MuCtx)),
  id_of :: FunPtr (Ptr MuVM -> MuName -> IO Hs_MuID),
  name_of :: FunPtr (Ptr MuVM -> MuID -> IO MuName),
  set_trap_handler :: FunPtr (Ptr MuVM -> MuTrapHandler -> MuCPtr -> IO ()),
  mvmExecute :: FunPtr (Ptr MuVM -> IO ())
                 } deriving (Generic)


instance CStorable MuVM

instance Storable MuVM where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek


foreign import ccall "dynamic" mkNewContext :: FunPtr (Ptr MuVM -> IO (Ptr MuCtx)) -> Ptr MuVM -> IO (Ptr MuCtx)
foreign import ccall "dynamic" mkIdOf :: FunPtr (Ptr MuVM -> MuName ->  IO Hs_MuID) -> Ptr MuVM ->  MuName -> IO Hs_MuID
foreign import ccall "dynamic" mkNameOf :: FunPtr (Ptr MuVM -> MuID ->  IO Hs_MuName) -> Ptr MuVM ->  MuID -> IO Hs_MuName
foreign import ccall "dynamic" mkSetTrapHandler :: FunPtr (Ptr MuVM -> MuTrapHandler -> MuCPtr -> IO ()) -> Ptr MuVM -> MuTrapHandler -> MuCPtr -> IO ()
foreign import ccall "dynamic" mkExecute :: FunPtr (Ptr MuVM -> IO ()) -> Ptr MuVM -> IO ()


newContext :: Ptr MuVM -> IO (Ptr MuCtx)
newContext mvm = call0 mkNewContext new_context mvm

idOf :: Ptr MuVM -> MuName -> IO Hs_MuID
idOf = call1 mkIdOf id_of

nameOf :: Ptr MuVM -> MuID -> IO Hs_MuName
nameOf = call1 mkNameOf name_of

setTrapHandler :: Ptr MuVM -> MuTrapHandler -> MuCPtr -> IO ()
setTrapHandler = call2 mkSetTrapHandler set_trap_handler

execute :: Ptr MuVM -> IO ()
execute = call0 mkExecute mvmExecute


data MuCtx = MuCtx {
  ctx_header :: Ptr (),   -- Refer to internal stuff
  
  -- Convert between IDs and names
  ctx_id_of :: FunPtr (Ptr MuCtx -> MuName -> IO Hs_MuID),
  ctx_name_of :: FunPtr (Ptr MuCtx -> MuID -> IO Hs_MuName),

  -- Close the current context, releasing all resources
  close_context :: FunPtr (Ptr MuCtx -> IO ()),

  -- Load bundles and HAIL scripts
  load_bundle :: FunPtr (Ptr MuCtx -> CString -> CInt -> IO ()),
  load_hail :: FunPtr (Ptr MuCtx -> CString -> CInt -> IO ()),

  -- Convert from C values to Mu values  
  handle_from_sint8 :: FunPtr (Ptr MuCtx -> Int8_t -> CInt -> IO MuIntValue),
  handle_from_uint8 :: FunPtr (Ptr MuCtx -> UInt8_t -> CInt -> IO MuIntValue),
  handle_from_sint16 :: FunPtr (Ptr MuCtx -> Int16_t -> CInt -> IO MuIntValue),
  handle_from_uint16 :: FunPtr (Ptr MuCtx -> UInt16_t -> CInt -> IO MuIntValue),
  handle_from_sint32 :: FunPtr (Ptr MuCtx -> Int32_t -> CInt -> IO MuIntValue),
  handle_from_uint32 :: FunPtr (Ptr MuCtx -> UInt32_t -> CInt -> IO MuIntValue),
  handle_from_sint64 :: FunPtr (Ptr MuCtx -> Int64_t -> CInt -> IO MuIntValue),
  handle_from_uint64 :: FunPtr (Ptr MuCtx -> UInt64_t -> CInt -> IO MuIntValue),
  handle_from_float :: FunPtr (Ptr MuCtx -> CFloat -> IO MuFloatValue),
  handle_from_double :: FunPtr (Ptr MuCtx -> CDouble -> IO MuDoubleValue),
  handle_from_ptr :: FunPtr (Ptr MuCtx -> MuID -> MuCPtr -> IO MuUPtrValue),
  handle_from_fp :: FunPtr (Ptr MuCtx -> MuID -> MuCFP -> IO MuUFPValue),
  
  -- Convert from Mu values to C values
  handle_to_sint8 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO Int8_t),
  handle_to_uint8 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO UInt8_t),
  handle_to_sint16 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO Int16_t),
  handle_to_uint16 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO UInt16_t),
  handle_to_sint32 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO Int32_t),
  handle_to_uint32 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO UInt32_t),
  handle_to_sint64 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO Int64_t),
  handle_to_uint64 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO UInt64_t),
  handle_to_float :: FunPtr (Ptr MuCtx -> MuFloatValue -> IO CFloat),
  handle_to_double :: FunPtr (Ptr MuCtx -> MuDoubleValue -> IO CDouble),
  handle_to_ptr :: FunPtr (Ptr MuCtx -> MuUPtrValue -> IO MuCPtr),
  handle_to_fp :: FunPtr (Ptr MuCtx -> MuUFPValue -> IO MuCFP),
  
  -- Make MuValue instances from Mu global SSA variables
  handle_from_const :: FunPtr (Ptr MuCtx -> MuID -> IO MuValue),
  handle_from_global :: FunPtr (Ptr MuCtx -> MuID -> IO MuIRefValue),
  handle_from_func :: FunPtr (Ptr MuCtx -> MuID -> IO MuFuncRefValue),
  handle_from_expose :: FunPtr (Ptr MuCtx -> MuID -> IO MuValue),
  
  -- Delete the value held by the MuCtx, making it unusable, but freeing up
  -- the resource.
  delete_value :: FunPtr (Ptr MuCtx -> MuValue -> IO ()),
  
  -- Compare reference or general reference types.
  -- EQ. Available for ref, iref, funcref, threadref and stackref.
  ref_eq :: FunPtr (Ptr MuCtx -> MuValue -> MuValue -> IO CInt),
  
  -- ULT. Available for iref only.
  ref_ult :: FunPtr (Ptr MuCtx -> MuIRefValue -> MuIRefValue -> IO CInt),
  
  -- Manipulate Mu values of the struct<...> type
  extract_value :: FunPtr (Ptr MuCtx -> MuStructValue -> CInt -> IO MuValue),
  insert_value :: FunPtr (Ptr MuCtx -> MuStructValue -> CInt -> MuValue -> IO MuValue),
  
  -- Manipulate Mu values of the array or vector type
  -- str can be MuArrayValue or MuVectorValue
  extract_element :: FunPtr (Ptr MuCtx -> MuValue -> MuIntValue -> IO MuValue),
  insert_element :: FunPtr (Ptr MuCtx -> MuValue -> MuIntValue -> MuValue -> IO MuValue),
  
  -- Heap allocation
  new_fixed :: FunPtr (Ptr MuCtx -> MuID -> IO MuRefValue),
  new_hybrid :: FunPtr (Ptr MuCtx -> MuID -> MuIntValue -> IO MuRefValue),
  
  -- Change the T or sig in ref<T>, iref<T> or func<sig>
  refcast :: FunPtr (Ptr MuCtx -> MuValue -> MuID -> IO MuValue),
  
  -- Memory addressing
  get_iref :: FunPtr (Ptr MuCtx -> MuRefValue -> IO MuIRefValue),
  get_field_iref :: FunPtr (Ptr MuCtx -> MuIRefValue -> CInt -> IO MuIRefValue),
  get_elem_iref :: FunPtr (Ptr MuCtx -> MuIRefValue -> MuIntValue -> IO MuIRefValue),
  shift_iref :: FunPtr (Ptr MuCtx -> MuIRefValue -> MuIntValue -> IO MuIRefValue),
  get_var_part_iref :: FunPtr (Ptr MuCtx -> MuIRefValue -> IO MuIRefValue),
  
  -- Memory accessing
  load' :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuIRefValue -> IO MuValue),
  store' :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuIRefValue -> MuValue -> IO ()),
  cmpxchg' :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuMemOrd -> CInt -> MuIRefValue -> MuValue -> MuValue -> Ptr CInt -> IO MuValue),
  atomicrmw' :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuAtomicRMWOp -> MuIRefValue -> MuValue -> IO MuValue),
  fence' :: FunPtr (Ptr MuCtx -> MuMemOrd -> IO ()),
  
  -- Thread and stack creation and stack destruction
  new_stack :: FunPtr (Ptr MuCtx -> MuFuncRefValue -> IO MuStackRefValue),
  new_thread :: FunPtr (Ptr MuCtx -> MuStackRefValue -> MuHowToResume -> Ptr MuValue -> CInt -> MuRefValue -> IO MuThreadRefValue),
  kill_stack :: FunPtr (Ptr MuCtx -> MuStackRefValue -> IO ()),
  
  -- Frame cursor operations
  new_cursor :: FunPtr (Ptr MuCtx -> MuStackRefValue -> IO MuFCRefValue),
  new_frame :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO ()),
  copy_cursor :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuFCRefValue),
  close_cursor :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO ()),
  
  -- Stack introspection
  cur_func :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuID),
  cur_func_ver :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuID),
  cur_inst :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuID),
  dump_keepalives :: FunPtr (Ptr MuCtx -> MuFCRefValue -> Ptr MuValue -> IO ()),
  
  -- On-stack replacement
  pop_frames_to :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO ()),
  push_frame :: FunPtr (Ptr MuCtx -> MuStackRefValue -> MuFuncRefValue -> IO ()),
  
  -- 64-bit tagged reference operations
  tr64_is_fp :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO CInt),
  tr64_is_int :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO CInt),
  tr64_is_ref :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO CInt),
  tr64_to_fp :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO MuDoubleValue),
  tr64_to_int :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO MuIntValue),
  tr64_to_ref :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO MuRefValue),
  tr64_to_tag :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO MuIntValue),
  
  tr64_from_fp :: FunPtr (Ptr MuCtx -> MuDoubleValue -> IO MuTagRef64Value),
  tr64_from_int :: FunPtr (Ptr MuCtx -> MuIntValue -> IO MuTagRef64Value),
  tr64_from_ref :: FunPtr (Ptr MuCtx -> MuRefValue -> MuIntValue -> IO MuTagRef64Value),
  
  -- Watchpoint operations
  enable_watchpoint :: FunPtr (Ptr MuCtx -> CInt -> IO ()),
  disable_watchpoint :: FunPtr (Ptr MuCtx -> CInt -> IO ()),
  
  -- Mu memory pinning, usually object pinning
  pin' :: FunPtr (Ptr MuCtx -> MuValue -> IO MuUPtrValue), --loc is either MuRefValue or MuIRefValue
  unpin' :: FunPtr (Ptr MuCtx -> MuValue -> IO ()),      --loc is either MuRefValue or MuIRefValue
  
  -- Expose Mu functions as native callable things, usually function pointers
  expose' :: FunPtr (Ptr MuCtx -> MuFuncRefValue -> MuCallConv -> MuIntValue -> IO MuValue),
  unexpose' :: FunPtr (Ptr MuCtx -> MuCallConv -> MuValue -> IO ())
} deriving (Generic)

instance CStorable MuCtx

instance Storable MuCtx where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek


foreign import ccall "dynamic" mkCtxIdOf :: FunPtr (Ptr MuCtx -> MuName -> IO Hs_MuID) -> Ptr MuCtx -> MuName -> IO Hs_MuID
foreign import ccall "dynamic" mkCtxNameOf :: FunPtr (Ptr MuCtx -> MuID -> IO Hs_MuName) -> Ptr MuCtx -> MuID -> IO Hs_MuName

foreign import ccall "dynamic" mkCloseContext :: FunPtr (Ptr MuCtx -> IO ()) -> Ptr MuCtx -> IO ()

foreign import ccall "dynamic" mkLoadBundle :: FunPtr (Ptr MuCtx -> CString -> CInt -> IO ()) -> Ptr MuCtx -> CString -> CInt -> IO ()
foreign import ccall "dynamic" mkLoadHail :: FunPtr (Ptr MuCtx -> CString -> CInt -> IO ()) -> Ptr MuCtx -> CString -> CInt -> IO ()

foreign import ccall "dynamic" mkHandleFromSint8 :: FunPtr (Ptr MuCtx -> Int8_t -> CInt -> IO MuIntValue) -> Ptr MuCtx -> Int8_t -> CInt -> IO MuIntValue
foreign import ccall "dynamic" mkHandleFromUint8 :: FunPtr (Ptr MuCtx -> UInt8_t -> CInt -> IO MuIntValue) -> Ptr MuCtx -> UInt8_t -> CInt -> IO MuIntValue
foreign import ccall "dynamic" mkHandleFromSint16 :: FunPtr (Ptr MuCtx -> Int16_t -> CInt -> IO MuIntValue) -> Ptr MuCtx -> Int16_t -> CInt -> IO MuIntValue
foreign import ccall "dynamic" mkHandleFromUint16 :: FunPtr (Ptr MuCtx -> UInt16_t -> CInt -> IO MuIntValue) -> Ptr MuCtx -> UInt16_t -> CInt -> IO MuIntValue
foreign import ccall "dynamic" mkHandleFromSint32 :: FunPtr (Ptr MuCtx -> Int32_t -> CInt -> IO MuIntValue) -> Ptr MuCtx -> Int32_t -> CInt -> IO MuIntValue
foreign import ccall "dynamic" mkHandleFromUint32 :: FunPtr (Ptr MuCtx -> UInt32_t -> CInt -> IO MuIntValue) -> Ptr MuCtx -> UInt32_t -> CInt -> IO MuIntValue
foreign import ccall "dynamic" mkHandleFromSint64 :: FunPtr (Ptr MuCtx -> Int64_t -> CInt -> IO MuIntValue) -> Ptr MuCtx -> Int64_t -> CInt -> IO MuIntValue
foreign import ccall "dynamic" mkHandleFromUint64 :: FunPtr (Ptr MuCtx -> UInt64_t -> CInt -> IO MuIntValue) -> Ptr MuCtx -> UInt64_t -> CInt -> IO MuIntValue
foreign import ccall "dynamic" mkHandleFromFloat :: FunPtr (Ptr MuCtx -> CFloat -> IO MuFloatValue) -> Ptr MuCtx -> CFloat -> IO MuFloatValue
foreign import ccall "dynamic" mkHandleFromDouble :: FunPtr (Ptr MuCtx -> CDouble -> IO MuDoubleValue) -> Ptr MuCtx -> CDouble -> IO MuDoubleValue
foreign import ccall "dynamic" mkHandleFromPtr :: FunPtr (Ptr MuCtx -> MuID -> MuCPtr -> IO MuUPtrValue) -> Ptr MuCtx -> MuID -> MuCPtr -> IO MuUPtrValue
foreign import ccall "dynamic" mkHandleFromFp :: FunPtr (Ptr MuCtx -> MuID -> MuCFP -> IO MuUFPValue) -> Ptr MuCtx -> MuID -> MuCFP -> IO MuUFPValue

foreign import ccall "dynamic" mkHandleToSint8 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO Int8_t) -> Ptr MuCtx -> MuIntValue -> IO Int8_t
foreign import ccall "dynamic" mkHandleToUint8 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO UInt8_t) -> Ptr MuCtx -> MuIntValue -> IO UInt8_t
foreign import ccall "dynamic" mkHandleToSint16 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO Int16_t) -> Ptr MuCtx -> MuIntValue -> IO Int16_t
foreign import ccall "dynamic" mkHandleToUint16 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO UInt16_t) -> Ptr MuCtx -> MuIntValue -> IO UInt16_t
foreign import ccall "dynamic" mkHandleToSint32 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO Int32_t) -> Ptr MuCtx -> MuIntValue -> IO Int32_t
foreign import ccall "dynamic" mkHandleToUint32 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO UInt32_t) -> Ptr MuCtx -> MuIntValue -> IO UInt32_t
foreign import ccall "dynamic" mkHandleToSint64 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO Int64_t) -> Ptr MuCtx -> MuIntValue -> IO Int64_t
foreign import ccall "dynamic" mkHandleToUint64 :: FunPtr (Ptr MuCtx -> MuIntValue -> IO UInt64_t) -> Ptr MuCtx -> MuIntValue -> IO UInt64_t
foreign import ccall "dynamic" mkHandleToFloat :: FunPtr (Ptr MuCtx -> MuFloatValue -> IO CFloat) -> Ptr MuCtx -> MuFloatValue -> IO CFloat
foreign import ccall "dynamic" mkHandleToDouble :: FunPtr (Ptr MuCtx -> MuDoubleValue -> IO CDouble) -> Ptr MuCtx -> MuDoubleValue -> IO CDouble
foreign import ccall "dynamic" mkHandleToPtr :: FunPtr (Ptr MuCtx -> MuUPtrValue -> IO MuCPtr) -> Ptr MuCtx -> MuUPtrValue -> IO MuCPtr
foreign import ccall "dynamic" mkHandleToFp :: FunPtr (Ptr MuCtx -> MuUFPValue -> IO MuCFP) -> Ptr MuCtx -> MuUFPValue -> IO MuCFP

foreign import ccall "dynamic" mkHandleFromConst :: FunPtr (Ptr MuCtx -> MuID -> IO MuValue) -> Ptr MuCtx -> MuID -> IO MuValue
foreign import ccall "dynamic" mkHandleFromGlobal :: FunPtr (Ptr MuCtx -> MuID -> IO MuIRefValue) -> Ptr MuCtx -> MuID -> IO MuIRefValue
foreign import ccall "dynamic" mkHandleFromFunc :: FunPtr (Ptr MuCtx -> MuID -> IO MuFuncRefValue) -> Ptr MuCtx -> MuID -> IO MuFuncRefValue
foreign import ccall "dynamic" mkhandleFromExpose :: FunPtr (Ptr MuCtx -> MuID -> IO MuValue) -> Ptr MuCtx -> MuID -> IO MuValue

foreign import ccall "dynamic" mkDeleteValue :: FunPtr (Ptr MuCtx -> MuValue -> IO ()) -> Ptr MuCtx -> MuValue -> IO ()

foreign import ccall "dynamic" mkRefEq :: FunPtr (Ptr MuCtx -> MuValue -> MuValue -> IO CInt) -> Ptr MuCtx -> MuValue -> MuValue -> IO CInt

foreign import ccall "dynamic" mkRefUlt :: FunPtr (Ptr MuCtx -> MuIRefValue -> MuIRefValue -> IO CInt) -> Ptr MuCtx -> MuIRefValue -> MuIRefValue -> IO CInt

foreign import ccall "dynamic" mkExtractValue :: FunPtr (Ptr MuCtx -> MuStructValue -> CInt -> IO MuValue) -> Ptr MuCtx -> MuStructValue -> CInt -> IO MuValue
foreign import ccall "dynamic" mkInsertValue :: FunPtr (Ptr MuCtx -> MuStructValue -> CInt -> MuValue -> IO MuValue) -> Ptr MuCtx -> MuStructValue -> CInt -> MuValue -> IO MuValue

foreign import ccall "dynamic" mkExtractElement :: FunPtr (Ptr MuCtx -> MuValue -> MuIntValue -> IO MuValue) -> Ptr MuCtx -> MuValue -> MuIntValue -> IO MuValue
foreign import ccall "dynamic" mkInsertElement :: FunPtr (Ptr MuCtx -> MuValue -> MuIntValue -> MuValue -> IO MuValue) -> Ptr MuCtx -> MuValue -> MuIntValue -> MuValue -> IO MuValue

foreign import ccall "dynamic" mkNewFixed :: FunPtr (Ptr MuCtx -> MuID -> IO MuRefValue) -> Ptr MuCtx -> MuID -> IO MuRefValue
foreign import ccall "dynamic" mkNewHybrid :: FunPtr (Ptr MuCtx -> MuID -> MuIntValue -> IO MuRefValue) -> Ptr MuCtx -> MuID -> MuIntValue -> IO MuRefValue

foreign import ccall "dynamic" mkRefCast :: FunPtr (Ptr MuCtx -> MuValue -> MuID -> IO MuValue) -> Ptr MuCtx -> MuValue -> MuID -> IO MuValue

foreign import ccall "dynamic" mkGetIref :: FunPtr (Ptr MuCtx -> MuRefValue -> IO MuIRefValue) -> Ptr MuCtx -> MuRefValue -> IO MuIRefValue
foreign import ccall "dynamic" mkGetFieldIref :: FunPtr (Ptr MuCtx -> MuIRefValue -> CInt -> IO MuIRefValue) -> Ptr MuCtx -> MuIRefValue -> CInt -> IO MuIRefValue
foreign import ccall "dynamic" mkGetElemIref :: FunPtr (Ptr MuCtx -> MuIRefValue -> MuIntValue -> IO MuIRefValue) -> Ptr MuCtx -> MuIRefValue -> MuIntValue -> IO MuIRefValue
foreign import ccall "dynamic" mkShiftIref :: FunPtr (Ptr MuCtx -> MuIRefValue -> MuIntValue -> IO MuIRefValue) -> Ptr MuCtx -> MuIRefValue -> MuIntValue -> IO MuIRefValue
foreign import ccall "dynamic" mkGetVarPartIref :: FunPtr (Ptr MuCtx -> MuIRefValue -> IO MuIRefValue) -> Ptr MuCtx -> MuIRefValue -> IO MuIRefValue

foreign import ccall "dynamic" mkLoad :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuIRefValue -> IO MuValue) -> Ptr MuCtx -> MuMemOrd -> MuIRefValue -> IO MuValue
foreign import ccall "dynamic" mkStore :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuIRefValue -> MuValue -> IO ()) -> Ptr MuCtx -> MuMemOrd -> MuIRefValue -> MuValue -> IO ()
foreign import ccall "dynamic" mkCmpxchg :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuMemOrd -> CInt -> MuIRefValue -> MuValue -> MuValue -> Ptr CInt -> IO MuValue) -> Ptr MuCtx -> MuMemOrd -> MuMemOrd -> CInt -> MuIRefValue -> MuValue -> MuValue -> Ptr CInt -> IO MuValue
foreign import ccall "dynamic" mkAtomicrmw :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuAtomicRMWOp -> MuIRefValue -> MuValue -> IO MuValue) -> Ptr MuCtx -> MuMemOrd -> MuAtomicRMWOp -> MuIRefValue -> MuValue -> IO MuValue
foreign import ccall "dynamic" mkFence :: FunPtr (Ptr MuCtx -> MuMemOrd -> IO ()) -> Ptr MuCtx -> MuMemOrd -> IO ()

foreign import ccall "dynamic" mkNewStack :: FunPtr (Ptr MuCtx -> MuFuncRefValue -> IO MuStackRefValue) -> Ptr MuCtx -> MuFuncRefValue -> IO MuStackRefValue
foreign import ccall "dynamic" mkNewThread :: FunPtr (Ptr MuCtx -> MuStackRefValue -> MuHowToResume -> Ptr MuValue -> CInt -> MuRefValue -> IO MuThreadRefValue) -> Ptr MuCtx -> MuStackRefValue -> MuHowToResume -> Ptr MuValue -> CInt -> MuRefValue -> IO MuThreadRefValue
foreign import ccall "dynamic" mkKillStack :: FunPtr (Ptr MuCtx -> MuStackRefValue -> IO ()) -> Ptr MuCtx -> MuStackRefValue -> IO ()

foreign import ccall "dynamic" mkNewCursor :: FunPtr (Ptr MuCtx -> MuStackRefValue -> IO MuFCRefValue) -> Ptr MuCtx -> MuStackRefValue -> IO MuFCRefValue
foreign import ccall "dynamic" mkNewFrame :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO ()) -> Ptr MuCtx -> MuFCRefValue -> IO ()
foreign import ccall "dynamic" mkCopyCursor :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuFCRefValue) -> Ptr MuCtx -> MuFCRefValue -> IO MuFCRefValue
foreign import ccall "dynamic" mkCloseCursor :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO ()) -> Ptr MuCtx -> MuFCRefValue -> IO ()

foreign import ccall "dynamic" mkCurFunc :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuID) -> Ptr MuCtx -> MuFCRefValue -> IO MuID
foreign import ccall "dynamic" mkCurFuncVer :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuID) -> Ptr MuCtx -> MuFCRefValue -> IO MuID
foreign import ccall "dynamic" mkCurInst :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuID) -> Ptr MuCtx -> MuFCRefValue -> IO MuID
foreign import ccall "dynamic" mkDumpKeepalives :: FunPtr (Ptr MuCtx -> MuFCRefValue -> Ptr MuValue -> IO ()) -> Ptr MuCtx -> MuFCRefValue -> Ptr MuValue -> IO ()

foreign import ccall "dynamic" mkPopFramesTo :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO ()) -> Ptr MuCtx -> MuFCRefValue -> IO ()
foreign import ccall "dynamic" mkPushFrame :: FunPtr (Ptr MuCtx -> MuStackRefValue -> MuFuncRefValue -> IO ()) -> Ptr MuCtx -> MuStackRefValue -> MuFuncRefValue -> IO ()

foreign import ccall "dynamic" mkTr64IsFp :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO CInt) -> Ptr MuCtx -> MuTagRef64Value -> IO CInt
foreign import ccall "dynamic" mkTr64IsInt :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO CInt) -> Ptr MuCtx -> MuTagRef64Value -> IO CInt
foreign import ccall "dynamic" mkTr64isRef :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO CInt) -> Ptr MuCtx -> MuTagRef64Value -> IO CInt
foreign import ccall "dynamic" mkTr64ToFp :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO MuDoubleValue) -> Ptr MuCtx -> MuTagRef64Value -> IO MuDoubleValue
foreign import ccall "dynamic" mkTr64ToInt :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO MuIntValue) -> Ptr MuCtx -> MuTagRef64Value -> IO MuIntValue
foreign import ccall "dynamic" mkTr64ToRef :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO MuRefValue) -> Ptr MuCtx -> MuTagRef64Value -> IO MuRefValue
foreign import ccall "dynamic" mkTr64ToTag :: FunPtr (Ptr MuCtx -> MuTagRef64Value -> IO MuIntValue) -> Ptr MuCtx -> MuTagRef64Value -> IO MuIntValue
  
foreign import ccall "dynamic" mkTr64FromFp :: FunPtr (Ptr MuCtx -> MuDoubleValue -> IO MuTagRef64Value) -> Ptr MuCtx -> MuDoubleValue -> IO MuTagRef64Value
foreign import ccall "dynamic" mkTr64FromInt :: FunPtr (Ptr MuCtx -> MuIntValue -> IO MuTagRef64Value) -> Ptr MuCtx -> MuIntValue -> IO MuTagRef64Value
foreign import ccall "dynamic" mkTr64FromRef :: FunPtr (Ptr MuCtx -> MuRefValue -> MuIntValue -> IO MuTagRef64Value) -> Ptr MuCtx -> MuRefValue -> MuIntValue -> IO MuTagRef64Value

foreign import ccall "dynamic" mkEnableWatchpoint :: FunPtr (Ptr MuCtx -> CInt -> IO ()) -> Ptr MuCtx -> CInt -> IO ()
foreign import ccall "dynamic" mkDisableWatchpoint :: FunPtr (Ptr MuCtx -> CInt -> IO ()) -> Ptr MuCtx -> CInt -> IO ()

foreign import ccall "dynamic" mkPin :: FunPtr (Ptr MuCtx -> MuValue -> IO MuUPtrValue) -> Ptr MuCtx -> MuValue -> IO MuUPtrValue
foreign import ccall "dynamic" mkUnpin :: FunPtr (Ptr MuCtx -> MuValue -> IO ()) -> Ptr MuCtx -> MuValue -> IO ()

foreign import ccall "dynamic" mkExpose :: FunPtr (Ptr MuCtx -> MuFuncRefValue -> MuCallConv -> MuIntValue -> IO MuValue) -> Ptr MuCtx -> MuFuncRefValue -> MuCallConv -> MuIntValue -> IO MuValue
foreign import ccall "dynamic" mkUnexpose :: FunPtr (Ptr MuCtx -> MuCallConv -> MuValue -> IO ()) -> Ptr MuCtx -> MuCallConv -> MuValue -> IO ()


ctxIdOf :: Ptr MuCtx -> MuName -> IO Hs_MuID
ctxIdOf = call1 mkCtxIdOf ctx_id_of

ctxNameOf :: Ptr MuCtx -> MuID -> IO Hs_MuName
ctxNameOf = call1 mkCtxNameOf ctx_name_of

closeContext :: Ptr MuCtx -> IO ()
closeContext = call0 mkCloseContext close_context

loadBundle :: Ptr MuCtx -> CString -> CInt -> IO ()
loadBundle = call2 mkLoadBundle load_bundle

loadHail :: Ptr MuCtx -> CString -> CInt -> IO ()
loadHail = call2 mkLoadHail load_hail

handleFromSInt8 :: Ptr MuCtx -> Int8_t -> CInt -> IO MuIntValue
handleFromSInt8 = call2 mkHandleFromSint8 handle_from_sint8
handleFromUInt8 :: Ptr MuCtx -> UInt8_t -> CInt -> IO MuIntValue
handleFromUInt8 = call2 mkHandleFromUint8 handle_from_uint8
handleFromSInt16 :: Ptr MuCtx -> Int16_t -> CInt -> IO MuIntValue
handleFromSInt16 = call2 mkHandleFromSint16 handle_from_sint16
handleFromUInt16 :: Ptr MuCtx -> UInt16_t -> CInt -> IO MuIntValue
handleFromUInt16 = call2 mkHandleFromUint16 handle_from_uint16
handleFromSInt32 :: Ptr MuCtx -> Int32_t -> CInt -> IO MuIntValue
handleFromSInt32 = call2 mkHandleFromSint32 handle_from_sint32
handleFromUInt32 :: Ptr MuCtx -> UInt32_t -> CInt -> IO MuIntValue
handleFromUInt32 = call2 mkHandleFromUint32 handle_from_uint32
handleFromSInt64 :: Ptr MuCtx -> Int64_t -> CInt -> IO MuIntValue
handleFromSInt64 = call2 mkHandleFromSint64 handle_from_sint64
handleFromUInt64 :: Ptr MuCtx -> UInt64_t -> CInt -> IO MuIntValue
handleFromUInt64 = call2 mkHandleFromUint64 handle_from_uint64
handleFromFloat :: Ptr MuCtx -> CFloat -> IO MuFloatValue
handleFromFloat = call1 mkHandleFromFloat handle_from_float
handleFromDouble :: Ptr MuCtx -> CDouble -> IO MuDoubleValue
handleFromDouble = call1 mkHandleFromDouble handle_from_double
handleFromPtr :: Ptr MuCtx -> MuID -> MuCPtr -> IO MuUPtrValue
handleFromPtr = call2 mkHandleFromPtr handle_from_ptr
handleFromFp :: Ptr MuCtx -> MuID -> MuCFP -> IO MuUFPValue
handleFromFp = call2 mkHandleFromFp handle_from_fp

handleToSint8 :: Ptr MuCtx -> MuIntValue -> IO Int8_t
handleToSint8 = call1 mkHandleToSint8 handle_to_sint8
handleToUint8 ::Ptr MuCtx -> MuIntValue -> IO UInt8_t
handleToUint8 = call1 mkHandleToUint8 handle_to_uint8
handleToSint16 :: Ptr MuCtx -> MuIntValue -> IO Int16_t
handleToSint16 = call1 mkHandleToSint16 handle_to_sint16
handleToUint16 :: Ptr MuCtx -> MuIntValue -> IO UInt16_t
handleToUint16 = call1 mkHandleToUint16 handle_to_uint16
handleToSint32 :: Ptr MuCtx -> MuIntValue -> IO Int32_t
handleToSint32 = call1 mkHandleToSint32 handle_to_sint32
handleToUint32 :: Ptr MuCtx -> MuIntValue -> IO UInt32_t
handleToUint32 = call1 mkHandleToUint32 handle_to_uint32
handleToSint64 :: Ptr MuCtx -> MuIntValue -> IO Int64_t
handleToSint64 = call1 mkHandleToSint64 handle_to_sint64
handleToUint64 :: Ptr MuCtx -> MuIntValue -> IO UInt64_t
handleToUint64 = call1 mkHandleToUint64 handle_to_uint64
handleToFloat :: Ptr MuCtx -> MuFloatValue -> IO CFloat
handleToFloat = call1 mkHandleToFloat handle_to_float
handleToDouble :: Ptr MuCtx -> MuDoubleValue -> IO CDouble
handleToDouble = call1 mkHandleToDouble handle_to_double
handleToPtr :: Ptr MuCtx -> MuUPtrValue -> IO MuCPtr
handleToPtr = call1 mkHandleToPtr handle_to_ptr
handleToFp :: Ptr MuCtx -> MuUFPValue -> IO MuCFP
handleToFp = call1 mkHandleToFp handle_to_fp

handleFromConst :: Ptr MuCtx -> MuID -> IO MuValue
handleFromConst = call1 mkHandleFromConst handle_from_const
handleFromGlobal :: Ptr MuCtx -> MuID -> IO MuIRefValue
handleFromGlobal = call1 mkHandleFromGlobal handle_from_global
handleFromFunc :: Ptr MuCtx -> MuID -> IO MuFuncRefValue
handleFromFunc = call1 mkHandleFromFunc handle_from_func
handleFromExpose :: Ptr MuCtx -> MuID -> IO MuValue
handleFromExpose = call1 mkhandleFromExpose handle_from_expose

deleteValue :: Ptr MuCtx -> MuValue -> IO ()
deleteValue = call1 mkDeleteValue delete_value

refEq :: Ptr MuCtx -> MuValue -> MuValue -> IO CInt
refEq = call2 mkRefEq ref_eq

refUlt :: Ptr MuCtx -> MuIRefValue -> MuIRefValue -> IO CInt
refUlt = call2 mkRefUlt ref_ult

extractValue :: Ptr MuCtx -> MuStructValue -> CInt -> IO MuValue
extractValue = call2 mkExtractValue extract_value
insertValue :: Ptr MuCtx -> MuStructValue -> CInt -> MuValue -> IO MuValue
insertValue = call3 mkInsertValue insert_value

extractElement :: Ptr MuCtx -> MuValue -> MuIntValue -> IO MuValue
extractElement = call2 mkExtractElement extract_element
insertElement :: Ptr MuCtx -> MuValue -> MuIntValue -> MuValue -> IO MuValue
insertElement = call3 mkInsertElement insert_element

newFixed :: Ptr MuCtx -> MuID -> IO MuRefValue
newFixed = call1 mkNewFixed new_fixed
newHybrid :: Ptr MuCtx -> MuID -> MuIntValue -> IO MuRefValue
newHybrid = call2 mkNewHybrid new_hybrid

refCast :: Ptr MuCtx -> MuValue -> MuID -> IO MuValue
refCast = call2 mkRefCast refcast

getIref :: Ptr MuCtx -> MuRefValue -> IO MuIRefValue
getIref = call1 mkGetIref get_iref
getFieldIref :: Ptr MuCtx -> MuIRefValue -> CInt -> IO MuIRefValue
getFieldIref = call2 mkGetFieldIref get_field_iref
getElemIref :: Ptr MuCtx -> MuIRefValue -> MuIntValue -> IO MuIRefValue
getElemIref = call2 mkGetElemIref get_elem_iref
shiftIref :: Ptr MuCtx -> MuIRefValue -> MuIntValue -> IO MuIRefValue
shiftIref = call2 mkShiftIref shift_iref
getVarPartIref :: Ptr MuCtx -> MuIRefValue -> IO MuIRefValue
getVarPartIref = call1 mkGetVarPartIref get_var_part_iref

load :: Ptr MuCtx -> MuMemOrd -> MuIRefValue -> IO MuValue
load = call2 mkLoad load'
store :: Ptr MuCtx -> MuMemOrd -> MuIRefValue -> MuValue -> IO ()
store = call3 mkStore store'
cmpxchg :: Ptr MuCtx -> MuMemOrd -> MuMemOrd -> CInt -> MuIRefValue -> MuValue -> MuValue -> Ptr CInt -> IO MuValue
cmpxchg = call7 mkCmpxchg cmpxchg'
atomicrmw :: Ptr MuCtx -> MuMemOrd -> MuAtomicRMWOp -> MuIRefValue -> MuValue -> IO MuValue
atomicrmw = call4 mkAtomicrmw atomicrmw'
fence :: Ptr MuCtx -> MuMemOrd -> IO ()
fence = call1 mkFence fence'

newStack :: Ptr MuCtx -> MuFuncRefValue -> IO MuStackRefValue
newStack = call1 mkNewStack new_stack
newThread :: Ptr MuCtx -> MuStackRefValue -> MuHowToResume -> Ptr MuValue -> CInt -> MuRefValue -> IO MuThreadRefValue
newThread = call5 mkNewThread new_thread
killStack :: Ptr MuCtx -> MuStackRefValue -> IO ()
killStack = call1 mkKillStack kill_stack

newCursor :: Ptr MuCtx -> MuStackRefValue -> IO MuFCRefValue
newCursor = call1 mkNewCursor new_cursor
newFrame :: Ptr MuCtx -> MuFCRefValue -> IO ()
newFrame = call1 mkNewFrame new_frame
copyCursor :: Ptr MuCtx -> MuFCRefValue -> IO MuFCRefValue
copyCursor = call1 mkCopyCursor copy_cursor
closeCursor :: Ptr MuCtx -> MuFCRefValue -> IO ()
closeCursor = call1 mkCloseCursor close_cursor

curFunc :: Ptr MuCtx -> MuFCRefValue -> IO MuID
curFunc = call1 mkCurFunc cur_func
curFuncVer :: Ptr MuCtx -> MuFCRefValue -> IO MuID
curFuncVer = call1 mkCurFuncVer cur_func_ver
curInst :: Ptr MuCtx -> MuFCRefValue -> IO MuID
curInst = call1 mkCurInst cur_inst
dumpKeepalives :: Ptr MuCtx -> MuFCRefValue -> Ptr MuValue -> IO ()
dumpKeepalives = call2 mkDumpKeepalives dump_keepalives

popFramesTo :: Ptr MuCtx -> MuFCRefValue -> IO ()
popFramesTo = call1 mkPopFramesTo pop_frames_to
pushFrame :: Ptr MuCtx -> MuStackRefValue -> MuFuncRefValue -> IO ()
pushFrame = call2 mkPushFrame push_frame

tr64IsFp :: Ptr MuCtx -> MuTagRef64Value -> IO CInt
tr64IsFp = call1 mkTr64IsFp tr64_is_fp
tr64IsInt :: Ptr MuCtx -> MuTagRef64Value -> IO CInt
tr64IsInt = call1 mkTr64IsInt tr64_is_int
tr64isRef :: Ptr MuCtx -> MuTagRef64Value -> IO CInt
tr64isRef = call1 mkTr64isRef tr64_is_ref
tr64ToFp :: Ptr MuCtx -> MuTagRef64Value -> IO MuDoubleValue
tr64ToFp = call1 mkTr64ToFp tr64_to_fp
tr64ToInt :: Ptr MuCtx -> MuTagRef64Value -> IO MuIntValue
tr64ToInt = call1 mkTr64ToInt tr64_to_int
tr64ToRef :: Ptr MuCtx -> MuTagRef64Value -> IO MuRefValue
tr64ToRef = call1 mkTr64ToRef tr64_to_ref
tr64ToTag :: Ptr MuCtx -> MuTagRef64Value -> IO MuIntValue
tr64ToTag = call1 mkTr64ToTag tr64_to_tag
  
tr64FromFp :: Ptr MuCtx -> MuDoubleValue -> IO MuTagRef64Value
tr64FromFp = call1 mkTr64FromFp tr64_from_fp
tr64FromInt :: Ptr MuCtx -> MuIntValue -> IO MuTagRef64Value
tr64FromInt = call1 mkTr64FromInt tr64_from_int
tr64FromRef :: Ptr MuCtx -> MuRefValue -> MuIntValue -> IO MuTagRef64Value
tr64FromRef = call2 mkTr64FromRef tr64_from_ref

enableWatchpoint :: Ptr MuCtx -> CInt -> IO ()
enableWatchpoint = call1 mkEnableWatchpoint enable_watchpoint
disableWatchpoint :: Ptr MuCtx -> CInt -> IO ()
disableWatchpoint = call1 mkDisableWatchpoint disable_watchpoint

pin :: Ptr MuCtx -> MuValue -> IO MuUPtrValue
pin = call1 mkPin pin'
unpin :: Ptr MuCtx -> MuValue -> IO ()
unpin = call1 mkUnpin unpin'

expose :: Ptr MuCtx -> MuFuncRefValue -> MuCallConv -> MuIntValue -> IO MuValue
expose = call3 mkExpose expose'
unexpose :: Ptr MuCtx -> MuCallConv -> MuValue -> IO ()
unexpose = call2 mkUnexpose unexpose'


call0 :: (Storable a) => (FunPtr (Ptr a -> IO b) -> (Ptr a -> IO b)) -> (a -> FunPtr (Ptr a -> IO b)) -> Ptr a -> IO b
call0 mkFn fn ctx = do
  ctxVal <- peek ctx
  mkFn (fn ctxVal) ctx

call1 :: (Storable a) => (FunPtr (Ptr a -> b -> IO c) -> (Ptr a -> b -> IO c)) -> (a -> FunPtr (Ptr a -> b -> IO c)) -> Ptr a -> b -> IO c
call1 mkFn fn ctx arg = do
  ctxVal <- peek ctx
  mkFn (fn ctxVal) ctx arg

call2 :: (Storable a) => (FunPtr (Ptr a -> b -> c -> IO d) -> (Ptr a -> b -> c -> IO d)) -> (a -> FunPtr (Ptr a -> b -> c -> IO d)) -> Ptr a -> b -> c -> IO d
call2 mkFn fn ctx arg arg2 = do
  ctxVal <- peek ctx
  mkFn (fn ctxVal) ctx arg arg2

call3 :: (Storable a) => (FunPtr (Ptr a -> b -> c -> d -> IO e) -> (Ptr a -> b -> c -> d -> IO e)) -> (a -> FunPtr (Ptr a -> b -> c -> d -> IO e)) -> Ptr a -> b -> c -> d -> IO e
call3 mkFn fn ctx arg arg2 arg3 = do
  ctxVal <- peek ctx
  mkFn (fn ctxVal) ctx arg arg2 arg3

call4 :: (Storable a) => (FunPtr (Ptr a -> b -> c -> d -> e -> IO f) -> (Ptr a -> b -> c -> d -> e -> IO f)) -> (a -> FunPtr (Ptr a -> b -> c -> d -> e -> IO f)) -> Ptr a -> b -> c -> d -> e -> IO f
call4 mkFn fn ctx arg arg2 arg3 arg4 = do
  ctxVal <- peek ctx
  mkFn (fn ctxVal) ctx arg arg2 arg3 arg4

call5 :: (Storable a) => (FunPtr (Ptr a -> b -> c -> d -> e -> f -> IO g) -> (Ptr a -> b -> c -> d -> e -> f -> IO g)) -> (a -> FunPtr (Ptr a -> b -> c -> d -> e -> f -> IO g)) -> Ptr a -> b -> c -> d -> e -> f -> IO g
call5 mkFn fn ctx arg arg2 arg3 arg4 arg5 = do
  ctxVal <- peek ctx
  mkFn (fn ctxVal) ctx arg arg2 arg3 arg4 arg5

call7 :: (Storable a) => (FunPtr (Ptr a -> b -> c -> d -> e -> f -> g -> h -> IO i) -> (Ptr a -> b -> c -> d -> e -> f -> g -> h -> IO i)) -> (a -> FunPtr (Ptr a -> b -> c -> d -> e -> f -> g -> h -> IO i)) -> Ptr a -> b -> c -> d -> e -> f -> g -> h -> IO i
call7 mkFn fn ctx arg arg2 arg3 arg4 arg5 arg6 arg7 = do
  ctxVal <- peek ctx
  mkFn (fn ctxVal) ctx arg arg2 arg3 arg4 arg5 arg6 arg7
