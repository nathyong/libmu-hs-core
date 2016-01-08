{-#LANGUAGE NoImplicitPrelude, ForeignFunctionInterface#-}

module Execute where

import Prelude

import Foreign
import Data.Void
import Foreign.C.String
import Foreign.C.Types

--Stdint.h typedefs
type Int8_t = CChar
type UInt8_t = CUChar
type Int16_t = CShort
type UInt16_t = CUShort
type Int32_t = CInt
type UInt32_t = CUInt
type Int64_t = CLong
type UInt64_t = CULong


type MuValue = Ptr Void              -- Any Mu value
type MuIntValue = Ptr Void           -- int<n>
type MuFloatValue = Ptr Void         -- float
type MuDoubleValue = Ptr Void        -- double
type MuRefValue = Ptr Void           -- ref<T>
type MuIRefValue = Ptr Void          -- iref<T>
type MuStructValue = Ptr Void        -- struct<...>
type MuArrayValue = Ptr Void         -- array<T l>
type MuVectorValue = Ptr Void        -- vector<T l>
type MuFuncRefValue = Ptr Void       -- funcref<sig>
type MuThreadRefValue = Ptr Void     -- threadref
type MuStackRefValue = Ptr Void      -- stackref
type MuFCRefValue = Ptr Void         -- framecursorref
type MuTagRef64Value = Ptr Void      -- tagref64
type MuUPtrValue = Ptr Void          -- uptr
type MuUFPValue = Ptr Void           -- ufuncptr

-- Identifiers and names of Mu
type MuID = CUInt
type MuName = CString

-- Convenient types for the void* type and the void(*)() type in C
type MuCPtr = Ptr Void
type MuCFP = FunPtr (IO Void)

-- Result of a trap handler
type MuTrapHandlerResult = CInt

-- Used by new_thread
type MuHowToResume = CInt

-- Used by MuTrapHandler
type MuValuesFreer = FunPtr (Ptr MuValue -> MuCPtr -> IO Void)

-- Signature of the trap handler
type MuTrapHandler = FunPtr (Ptr MuCtx -> MuThreadRefValue ->
                            MuStackRefValue -> CInt -> Ptr MuTrapHandlerResult ->
                            Ptr MuStackRefValue -> Ptr (Ptr MuValue) -> Ptr CInt ->
                            Ptr MuValuesFreer -> Ptr MuCPtr -> Ptr MuRefValue ->
                            MuCPtr -> IO Void)

-- Memory orders
type MuMemOrd = CInt

type MuAtomicRMWOp = CInt

type MuCallConv = CInt

data MuVM = MuVM {
  header :: Ptr Void,
  new_context :: FunPtr (Ptr MuVM -> IO (Ptr MuCtx)),
  id_of :: FunPtr (Ptr MuVM -> MuName -> IO MuID),
  name_of :: FunPtr (Ptr MuVM -> MuID -> IO MuName),
  set_trap_handler :: FunPtr (Ptr MuVM -> MuTrapHandler -> MuCPtr -> IO Void),
  execute :: FunPtr (Ptr MuVM -> IO Void)
                 }

data MuCtx = MuCtx {
  ctx_header :: Ptr Void,   -- Refer to internal stuff
  
  -- Convert between IDs and names
  ctx_id_of :: FunPtr (Ptr MuCtx -> MuName -> IO MuID),
  ctx_name_of :: FunPtr (Ptr MuCtx -> MuID),

  -- Close the current context, releasing all resources
  close_context :: FunPtr (Ptr MuCtx -> IO Void),

  -- Load bundles and HAIL scripts
  load_bundle :: FunPtr (Ptr MuCtx -> CString -> CInt -> IO Void),
  load_hail :: FunPtr (Ptr MuCtx -> CString -> CInt -> IO Void),

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
    handle_from_global :: FunPtr (Ptr MuCtx -> MuID -> MuIRefValue),
    handle_from_func :: FunPtr (Ptr MuCtx -> MuID -> MuFuncRefValue),
    handle_from_expose :: FunPtr (Ptr MuCtx -> MuID -> MuValue),

    -- Delete the value held by the MuCtx, making it unusable, but freeing up
    -- the resource.
    delete_value :: FunPtr (Ptr MuCtx -> MuValue -> IO Void),

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
    insert_elemtn :: FunPtr (Ptr MuCtx -> MuValue -> MuIntValue -> MuValue -> IO MuValue),

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
    load :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuIRefValue -> IO MuValue),
    store :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuIRefValue -> MuValue -> IO Void),
    cmpxchg :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuMemOrd -> CInt -> MuIRefValue -> MuValue -> MuValue -> Ptr CInt -> IO MuValue),
    atomicrmw :: FunPtr (Ptr MuCtx -> MuMemOrd -> MuAtomicRMWOp -> MuIRefValue -> MuValue -> IO MuValue),
    fence :: FunPtr (Ptr MuCtx -> MuMemOrd -> IO Void),

    -- Thread and stack creation and stack destruction
    new_stack :: FunPtr (Ptr MuCtx -> MuFuncRefValue -> IO MuStackRefValue),
    new_thread :: FunPtr (Ptr MuCtx -> MuStackRefValue -> MuHowToResume -> Ptr MuValue -> CInt -> MuRefValue -> IO MuThreadRefValue),
    kill_stack :: FunPtr (Ptr MuCtx -> MuStackRefValue -> IO Void),

    -- Frame cursor operations
    new_cursor :: FunPtr (Ptr MuCtx -> MuStackRefValue -> IO MuFCRefValue),
    new_frame :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO Void),
    copy_cursor :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuFCRefValue),
    close_cursor :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO Void),

    -- Stack introspection
    cur_func :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuID),
    cur_func_ver :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuID),
    cur_inst :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO MuID),
    dump_keepalives :: FunPtr (Ptr MuCtx -> MuFCRefValue -> Ptr MuValue -> IO Void),
    
    -- On-stack replacement
    pop_frames_to :: FunPtr (Ptr MuCtx -> MuFCRefValue -> IO Void),
    push_frame :: FunPtr (Ptr MuCtx -> MuStackRefValue -> MuFuncRefValue -> IO Void),

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
    enable_watchpoint :: FunPtr (Ptr MuCtx -> CInt -> IO Void),
    disable_watchpoint :: FunPtr (Ptr MuCtx -> CInt -> IO Void),

    -- Mu memory pinning, usually object pinning
    MuUPtrValue (*pin  )(MuCtx *ctx, MuValue loc);      // loc is either MuRefValue or MuIRefValue
    void        (*unpin)(MuCtx *ctx, MuValue loc);      // loc is either MuRefValue or MuIRefValue

    // Expose Mu functions as native callable things, usually function pointers
    MuValue     (*expose  )(MuCtx *ctx, MuFuncRefValue func, MuCallConv call_conv, MuIntValue cookie);
    void        (*unexpose)(MuCtx *ctx, MuCallConv call_conv, MuValue value);
                   }
