{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

--An abstract syntax tree for Mu IR
{-
The overall design of the syntax tree is that the tree should contain enough information for MuIR output and Type Checking.
A brief overview of what the tree looks like is given below:

Tree = [Declaration]

Declaration = ConstDecl | Typedef | ... FuncDef ... | Exposedef

FuncDef = Name :: String,
          Parameters :: [SSAVariable],
          instruction :: [Assign],
          terminatingInst :: Expression

Assign = assignee :: SSAVariable,
         expr :: Expression

Expression = BinaryOperation | CompareOperation ... Load ... | GetVarPartIRef


Example Usage:

%c = ADD <@i32> %a %b

can be written thus

i32 :: UvmTypeDef
i32 = UvmTypeDef "i32" (MuInt 32)

a, b, c :: SSAVariable
a = SSAVariable Local "a" i32
b = SSAVariable Local "b" i32
c = SSAVariable Local "c" i32

out :: Assign
out = Assign c (BinaryOperation Add i32 a b Nothing)

putStrLn $ pp out
-}


module LibMu.MuSyntax where 

import Prelude (Eq(..), Ord(..), Show(..), Enum(..), String, Int, Bool, Maybe(..), ($))
import Text.Printf (printf)
import Data.List (intersperse, concat, map)

-- |Represent the Mu Calling convention
data CallConvention = Mu -- ^Mu represents the default calling convention, CCall 
                    | Foreign String -- ^Extra conventions can be invoked using the foreign Convention
                      deriving (Eq, Show)

-- |The scope of all items. Typedefs, Function Declarations, globals and externs produce global variables. While ssa variables are 
data Scope = Local -- ^Local variables (%)
           | Global -- ^Global variables (@)
             deriving (Eq, Show)

-- |Represent the Mu IR native types.
data UvmType = MuInt {intLen :: Int} -- ^Integers of variable sizes (1, 8, 16, 32 & 64 bits)
             | MuFloat  -- ^IEEE single length floating point
             | MuDouble -- ^IEEE double length floating point
             | Ref {refType :: UvmTypeDef} -- ^A reference to a Mu Typedef
             | IRef {irefType :: UvmTypeDef} -- ^An internal refernce to a Mu Typedef
             | WeakRef {weakRefType :: UvmTypeDef} -- ^A weak refernce to a Mu Typedef
             | UPtr {uptrType :: UvmTypeDef} -- ^A pointer to a Mu Typedef
             | Struct {structTypes :: [UvmTypeDef]} -- ^A struct of multiple Mu Types
             | Array {arrayType :: UvmTypeDef, arrayLen :: Int} -- ^An array of a set number of Mu Types
             | Hybrid {hybridTypes :: [UvmTypeDef], hybridType :: UvmTypeDef} -- ^A hybrid with a set 'header' of Mu types, then a variable number of A single Mu Typedef
             | Void
             | ThreadRef
             | StackRef
             | FrameCursorRef
             | TagRef64
             | Vector {vectorType :: UvmTypeDef, vectorLen :: Int} -- ^Vector of multiple Mu types
             | FuncRef {funcRefSig :: FuncSig} -- ^Function ref to a function signature
             | UFuncPtr {ufuncPtrSig ::FuncSig} -- ^Function pointer to a function signature
               deriving (Eq, Ord)

-- |This show instance is used to create "cannonical" types by the builder. That is, the builder can automatically generate an
-- |iref<@i32> type with a standard nameing convention. In this case, iref.i32 = iref<@i32>
instance Show UvmType where
  show uType= case uType of
    MuInt l -> printf "i%d" l
    MuFloat -> "float"
    MuDouble -> "double"
    Ref t -> printf "ref.%s" (uvmTypeDefName t)
    IRef t -> printf "iref.%s" (uvmTypeDefName t)
    WeakRef t -> printf "weakref.%s" (uvmTypeDefName t)
    UPtr t -> printf "uptr.%s" (uvmTypeDefName t)
    Struct ts -> printf "struct.%s" (concat $ intersperse "." $ map uvmTypeDefName ts)
    Array t l -> printf "array.%s.d" (uvmTypeDefName t) l
    Hybrid ts t -> printf "hybrid.%s.%s" (concat $ intersperse "." $ map uvmTypeDefName ts) (uvmTypeDefName t)
    Void -> "void"
    ThreadRef -> "threadref"
    StackRef -> "stackref"
    FrameCursorRef -> "framecursorref"
    TagRef64 -> "tagref64"
    Vector t l -> printf "vector.%s.%d" (uvmTypeDefName t) l
    FuncRef s -> printf "funcref.%s" (funcSigName s)
    UFuncPtr s -> printf "ufuncptr.%s" (funcSigName s)

-- |Represents an SSAVariable, used to hold globals, funcrefs & local varaibles
data SSAVariable = SSAVariable {
  varScope :: Scope,
  varID :: String, 
  varType :: UvmTypeDef
  }
                 deriving (Eq, Show)

-- |Represents a .typedef clause
data UvmTypeDef = UvmTypeDef {
  uvmTypeDefName :: String, -- ^Type alias
  uvmTypeDefType :: UvmType -- ^Type to alias to
  } deriving (Eq, Ord, Show)

-- |Represents a Function Signature,
data FuncSig = FuncSig {
  funcSigName :: String,
  funcSigArgTypes :: [UvmTypeDef],
  funcSigReturnType :: [UvmTypeDef]
  } deriving (Eq, Ord, Show)

-- |Represents an Exception Clause which are appended to certain instructions
data ExceptionClause = ExceptionClause {
  exceptionNor :: DestinationClause, -- ^The destination to go to if normal execution happens
  exceptionExc :: DestinationClause  -- ^The exceptional destination
  } deriving (Eq, Show)

-- |WP Exceptions are used for Watch Points
newtype WPExceptionClause = WPExceptionClause { wpExceptionDest :: DestinationClause }
                            deriving (Eq, Show)

-- |Keep Alive clause used to keep certain variables alive outside their scope
newtype KeepAliveClause = KeepAlive { keepAliveVars :: [SSAVariable] }
                          deriving (Eq, Show)
                                   
-- |Destination clause, the call to move to a basic block e.g. %cont(%a)
data DestinationClause = DestinationClause {
  destClauseDestination :: String,
  destClauseArgList :: [SSAVariable]
  } deriving (Eq, Show)

-- |Flags for Common Instructions and CCall, pp $ Flag "DEFAULT" == "#DEFAULT"
newtype Flag = Flag { flagValue :: String }

-- |Binary Ops as defined by the Mu Spec
-- |Binary Ops all use the same arguments, so these flags represent precisly which binary operation to display
data BinaryOp = Add
              | Sub
              | Mul
              | SDiv
              | SRem
              | UDiv
              | URem
              | Shl
              | LShr
              | AShr
              | And
              | Or
              | Xor
              | FAdd
              | FSub
              | FMul
              | FDiv
              | FRem
                deriving (Eq, Ord, Enum, Show)

-- |Compare Ops as defined by the Mu Spec
-- |See Binary Ops for more detail
data CompareOp = EQ
               | NE
               | SGE
               | SLE
               | SGT
               | SLT
               | UGE
               | UGT
               | ULE
               | ULT
               | FFALSE
               | FTRUE
               | FOEQ
               | FOGT
               | FOGE
               | FOLT
               | FOLE
               | FONE
               | FORD
               | FUEQ
               | FUGT
               | FUGE
               | FULT
               | FULE
               | FUNE
               | FUNO
               deriving (Eq, Ord, Enum, Show)

-- |Convert Ops as defined by the Mu Spec
-- |See Binary Ops for more detail
data ConvertOp = TRUNC
               | ZEXT
               | SEXT
               | FPTRUNC
               | FPEXT
               | FPTOUI
               | FPTOSI
               | UITOFP
               | SITOFP
               | BITCAST
               | REFCAST
               | PTRCAST
               deriving (Eq, Ord, Enum, Show)

-- |Atomic RMW Ops as  defined by the mu spec
-- |See Binary Ops for more detail
data AtomicRMWOp = XCHG
                 | ADD
                 | SUB
                 | AND
                 | NAND
                 | OR
                 | XOR
                 | MAX
                 | MIN
                 | UMAX
                 | UMIN
                 deriving (Eq, Ord, Enum, Show)

-- |Memory Orders ad defined by the Mu Spec
data MemoryOrder = NOT_ATOMIC
                 | RELAXED
                 | CONSUME
                 | ACQUIRE
                 | RELEASE
                 | ACQ_REL
                 | SEQ_CST
                 deriving (Eq, Ord, Enum, Show)

-- |Expressions define all the operations which can be performed on Mu Data. This includes terminal instructions.
data Expression = BinaryOperation {
                    -- |The operation to be performed (Add, sub ...)
                    binOp :: BinaryOp,
                    -- |The type of both parameters (they must be the same type)
                    binType :: UvmTypeDef,
                    -- |The first parameter
                    binV1 :: SSAVariable,
                    -- |The second parameter
                    binV2 :: SSAVariable,
                    -- |The optional exception clause
                    execClause :: Maybe ExceptionClause
                    }
                | CompareOperation {
                    -- |The compare operation, {EQ, SLE ... }
                    cmpOp :: CompareOp,
                    -- |Type of both operands
                    cmpType :: UvmTypeDef,
                    -- |first parameter
                    cmpV1 :: SSAVariable,
                    -- |second parameter
                    cmpV2 :: SSAVariable
                    }
                | ConvertOperation {
                    -- |Operation to be performed {TRUNC, SEXT ...}
                    convOp :: ConvertOp,
                    -- |The source type, the type to be converted from
                    convTypeSrc :: UvmTypeDef,
                    -- |The destination type, the type to be converted to
                    convTypeDest :: UvmTypeDef,
                    -- |Variable to be converted
                    convV :: SSAVariable,
                    -- |optional exception clause
                    convExceptionClause :: Maybe ExceptionClause
                    }
                | AtomicRMWOperation {
                    -- |Bool indicates if Loc is a pointer
                    aRMWIsPtr :: Bool,
                    -- |The memory order for the operation 
                    aRMWMemOrd :: MemoryOrder,
                    -- |The operation to be performed
                    aRMWOp :: AtomicRMWOp,
                    -- |The type of loc
                    aRMWType :: UvmTypeDef,
                    -- |The memory location/address to access. 
                    aRMWLoc :: SSAVariable,
                    -- |The literal to be used
                    aRMWOpnd :: SSAVariable,
                    -- |The optional Exception clause
                    aRMWExecClause :: Maybe ExceptionClause
                    }
                | CmpXchg {
                    -- |Bool indicating if loc is a pointer
                    cmpXchgIsPtr :: Bool,
                    -- |Bool indicating if operation is weak
                    cmpXchgIsWeak :: Bool,
                    -- |Memory order for operation success
                    cmpXchgMemOrdSucc :: MemoryOrder,
                    -- |Memory order for operation failure
                    cmpXchgMemOrdFail :: MemoryOrder,
                    -- |The type of the operation. Must be EQ comparable
                    cmpXchgType :: UvmTypeDef,
                    -- |variable of IRef<T> or UPtr<T>. The memory loc/addr to access
                    cmpXchgLoc :: SSAVariable,
                    -- |Strong Variant represents expected value in memory
                    cmpXchgExpect :: SSAVariable,
                    -- |Strong Variant represents Desired value in memory
                    cmpXchgDesired ::SSAVariable,
                    -- |Optional exception clause
                    cmpXchgExecClause :: Maybe ExceptionClause
                    }
                | Fence {
                    -- |Memory order for fence operation
                    fenceMemOrd :: MemoryOrder
                    }
                | New {
                    -- |Type to allocate from heap
                    newType :: UvmTypeDef,
                    -- |exception clause if operation fails
                    newExecClause :: Maybe ExceptionClause
                    }
                | NewHybrid {
                    -- |Hybrid type to alloate from heap
                    newHybridType :: UvmTypeDef,
                    -- |Length of hybrid type (must be int)
                    newHybridLenType :: UvmTypeDef,
                    -- |Length of hybrid
                    newHybridLen :: SSAVariable,
                    -- |Exception clause if operation fails
                    newHybridExecClause :: Maybe ExceptionClause
                    }
                | Alloca {
                    -- |Type to allocate
                    allocaType :: UvmTypeDef,
                    -- |Exception clause if operation fails
                    allocaExecClause :: Maybe ExceptionClause
                    }
                | AllocaHybrid {
                    -- |Hybrid Type to allocate  
                    allocaHybridType :: UvmTypeDef,
                    -- |Length of hybrid type (must be int)
                    allocaHybridLenType :: UvmTypeDef,
                    -- |Length of hybrid
                    allocaHybridLen :: SSAVariable,
                    -- |Exception clause if operation fails
                    allocaHybridExecClause :: Maybe ExceptionClause
                    }
                | Return {
                    -- |Values to return (if any)
                    returnValues :: [SSAVariable]
                    }
                | Throw {
                    -- |exceptional variable tor throw
                    throwException :: SSAVariable
                    }
                | Call {
                    -- |Signature of Mu Function to call
                    callSignature :: FuncSig,
                    -- |Variable of type funcref<@sig>
                    callCallee :: SSAVariable,
                    -- |Arguments to pass to function
                    callArgList :: [SSAVariable],
                    -- |optional exception clause
                    callExceptionClause :: Maybe ExceptionClause,
                    -- |Optional keep alive clause
                    callKeepAliveClause :: Maybe KeepAliveClause
                    }
                | CCall {
                    -- |Calling convention to follow (Mu or Other)
                    ccallCallConv :: CallConvention,
                    -- |type of callee (ufuncptr<@sig>)
                    ccallType :: UvmTypeDef,
                    -- |Signature of callee
                    ccallSig :: FuncSig,
                    -- |address of function to call. (.const @callee = ufuncptr<@sig> 0xdeadbeef)
                    ccallCallee :: SSAVariable,
                    -- |Arguments to pass to function
                    ccallArgList :: [SSAVariable],
                    -- |optional exception clause
                    ccallExceptionClause :: Maybe ExceptionClause,
                    -- |optional keep alive clause
                    ccallKeepAliveClause :: Maybe KeepAliveClause
                    }
                | TailCall {
                    -- |signature of function to call
                    tailCallSignature :: FuncSig,
                    -- |function to call
                    tailCallCallee :: SSAVariable,
                    -- |arguments to pass
                    tailCallArgList :: [SSAVariable]
                    }
                | Branch1 {
                    -- |BasicBlock to branch to
                    branch1Destination :: DestinationClause
                    }
                | Branch2 {
                    -- |Variable of type int<1>, condition on which to branch
                    branch2Cond :: SSAVariable,
                    -- |Branch if condition is true
                    branch2BranchTrue :: DestinationClause,
                    -- |Branch if condition is false
                    branch2BranchFalse :: DestinationClause
                    }
                | WatchPoint {
                    -- |name of watchpoint
                    watchpointname :: SSAVariable,
                    -- |id of watchpoint
                    watchpointId :: Int,
                    -- |The types of the return values
                    watchpointTypes :: [UvmTypeDef],
                    -- |destination before watchpoint is enabled
                    watchpointdis :: DestinationClause,
                    -- |destination after watchpoint is enabled
                    watchpointena :: DestinationClause,
                    -- |optional exception clause for after watchpoint is enabled
                    watchpointWpExec :: Maybe WPExceptionClause,
                    -- |optional keep alive clause for after watchpoint is enabled
                    watchpointKeepAlive :: Maybe KeepAliveClause
                    }
                | Trap {
                    -- |name of trap
                    trapName :: SSAVariable,
                    -- |the types of the values to return
                    trapTypes :: [UvmTypeDef],
                    -- |optional exception clause
                    trapExceptionClause :: Maybe ExceptionClause,
                    -- |optinoal keep alive clause
                    trapKeepAlive :: Maybe KeepAliveClause
                    }
                | WPBranch {
                    -- |watchpoint id
                    wpBranchId :: Int,
                    -- |destination to jump to if watchpoint is enabled
                    wpBranchDis :: DestinationClause,
                    -- |destination to jump to if watchpoint is disabled
                    wpBranchEna :: DestinationClause
                    }
                | Switch {
                    -- |type of value to switch on
                    switchType :: UvmTypeDef,
                    -- |value to switch on
                    switchOpnd :: SSAVariable,
                    -- |default destination (if all others fail)
                    switchDefault :: DestinationClause,
                    -- |list of (condition, destination) pairs
                    switchBlocks :: [(SSAVariable, DestinationClause)]
                    }
                | SwapStack {
                    -- |variable of type stackref (stack to swap to)
                    swapStackSwapee :: SSAVariable,
                    -- |cur stack clause to use
                    swapStackCurStackClause :: CurStackClause,
                    -- |new stack clause to use
                    swapStackNewStackClause :: NewStackClause,
                    -- |optional exception clause
                    swapStackExecClause :: Maybe ExceptionClause,
                    -- |optional keep alive clause
                    swapStackKeepAliveClause :: Maybe KeepAliveClause
                    }
                | NewThread {
                    -- |variable of type stackref (stack to bind thread to)
                    newThreadStack :: SSAVariable,
                    -- |new stack clause to use
                    newThreadStackClause :: NewStackClause,
                    -- |optional exception clause to use
                    newThreadExceptionClause :: Maybe ExceptionClause
                    }
                | Comminst {
                    -- |global name of common instruction
                    comminstInst :: String,
                    -- |optional flags to pass
                    comminstFlags :: Maybe [Flag],
                    -- |types (if any) of the arguments to pass to comminst
                    comminstTypes ::Maybe [UvmTypeDef],
                    -- |signature (if any) of the comminst
                    comminstSigs :: Maybe [FuncSig],
                    -- |arguments (if any) to pass to the comminst
                    comminstArgs :: Maybe [SSAVariable],
                    -- |optional exception clause
                    comminstExecClause :: Maybe ExceptionClause,
                    -- |optional keep alive clause
                    comminstKeepAliveClause :: Maybe KeepAliveClause
                    }
                | Load {
                    -- |Bool indicating if Loc is a poiner
                    loadIsPtr :: Bool,
                    -- |Optional Memory order (default NOT_ATOMIC)
                    loadMemOrd :: Maybe MemoryOrder,
                    -- |The referant type of loc
                    loadType :: UvmTypeDef,
                    -- |Variable of type IRef or UPtr (the mem location to load from)
                    loadLoc :: SSAVariable,
                    -- |Exception clause if operation fails
                    loadExecClause :: Maybe ExceptionClause
                    }
                | Store {
                    -- |Bool indicating if loc is a pointer
                    storeIsPtr :: Bool,
                    -- |Memory order for operation
                    storeMemOrd :: Maybe MemoryOrder,
                    -- |Type of loc
                    storeType :: UvmTypeDef,
                    -- |variable of IRef or UPtr. Mem loc/addr to store into
                    storeLoc :: SSAVariable,
                    -- |The new value to store
                    storeNewVal :: SSAVariable,
                    -- |Optional exception clause
                    storeExecClause :: Maybe ExceptionClause
                    }
                | ExtractValueS {
                    structExtractType :: UvmTypeDef,
                    structExtractIndex :: Int,
                    structExtractStruct :: SSAVariable,
                    structExtractExecClause :: Maybe ExceptionClause
                    }
                | InsertValueS {
                    structInsertType :: UvmTypeDef,
                    structInsertIndex :: Int,
                    structInsertStruct :: SSAVariable,
                    structInsertNewVal :: SSAVariable,
                    structInsertExecClause :: Maybe ExceptionClause
                    }
                | ExtractValue {   
                    arrExtractType :: UvmTypeDef,     
                    arrExtractIndexType :: UvmTypeDef,
                    arrExtractOpnd :: SSAVariable, 
                    arrExtractIndex :: SSAVariable,
                    arrExtractExecClause :: Maybe ExceptionClause
                    }
                | InsertValue {
                    arrInsertType :: UvmTypeDef,
                    arrInsertIndexType :: UvmTypeDef,
                    arrInsertOpnd :: SSAVariable,
                    arrInsertIndex :: SSAVariable,
                    arrInsertNewVal :: SSAVariable,
                    arrInsertExecClause :: Maybe ExceptionClause
                    }
                | ShuffleVector {
                    arrShuffleV1Type :: UvmTypeDef,
                    arrShuffleV2Type :: UvmTypeDef,
                    arrShuffleV1 :: SSAVariable,
                    arrShuffleV2 :: SSAVariable,
                    arrShuffleMask :: SSAVariable,
                    arrShuffleExecClause :: Maybe ExceptionClause
                    }                  
                | GetIRef {
                    getIRefType :: UvmTypeDef,
                    getIRefOpnd :: SSAVariable,
                    getIRefExecClause :: Maybe ExceptionClause
                    }
                | GetFieldIRef {
                    getFieldIRefPtr :: Bool,
                    getFieldIRefTypeOpnd :: UvmTypeDef,
                    getFieldIRefIndex :: Int,
                    getFieldIRefOpnd :: SSAVariable,
                    getFieldIRefExecClause :: Maybe ExceptionClause
                    }
                | GetElemIRef {
                    getElemIRefPtr :: Bool,
                    getElemIRefTypeOpnd :: UvmTypeDef,
                    getElemIRefTypeIndex :: UvmTypeDef,
                    getElemIRefOpnd :: SSAVariable,
                    getElemIRefIndex :: SSAVariable,
                    getElemIRefExecClause :: Maybe ExceptionClause
                    }
                | ShiftIRef {
                    shiftIRefPtr :: Bool,
                    shiftIRefTypeOpnd :: UvmTypeDef,
                    shiftIRefTypeIndex :: UvmTypeDef,
                    shiftIRefOpnd :: SSAVariable,
                    shiftIRefOffset :: SSAVariable,
                    shiftIRefExecClause :: Maybe ExceptionClause
                    }
                | GetVarPartIRef {
                    getVarPartIRefPtr :: Bool,
                    getVarPartIRefTypeOpnd :: UvmTypeDef,
                    getVarPartIRefOpnd :: SSAVariable,
                    getVarPartIRefExecClause :: Maybe ExceptionClause
                    }
                | Comment {
                    commentVal :: String
                    }
                  
data CurStackClause = RetWith {
                        retWithTypes :: [UvmTypeDef]
                        }
                    | KillOld
                      
data NewStackClause = PassValues {
                        newStackTypes :: [UvmTypeDef],
                        newStackValues :: [SSAVariable]
                        }
                    | ThrowExc {
                        throwExecExceptionClause :: SSAVariable
                        }
                      
data Assign = Assign {
                --Variable to assign the returned value from expression
                assignVarss :: [SSAVariable],
                --Expression to assign to variable
                assignExpr :: Expression
                }
              
newtype Program = Program {unProgram :: [Declaration]}
                  
data Declaration = ConstDecl {
                     constVariable :: SSAVariable,
                     constValue :: String
                     }
                 | Typedef {
                     typeDefType :: UvmTypeDef
                     }
                 | FunctionSignature { 
                     funcSig :: FuncSig
                     }
                 | FunctionDef {
                     funcDefName :: String,
                     funcDefVersion :: String,
                     funcDefSig  :: FuncSig,
                     funcDefBody :: [BasicBlock]
                     }
                 | FunctionDecl {
                     funcDeclName :: String,
                     funcDeclSignature :: FuncSig
                     }
                 | GlobalDef {
                     globalVariable :: SSAVariable,
                     globalType :: UvmTypeDef
                     }
                 | ExposeDef {
                     exposeName :: String,
                     exposeFuncName :: String,
                     exposeCallConv :: CallConvention,
                     exposeCookie :: SSAVariable
                     }
                   
                   
data BasicBlock = BasicBlock {
                    basicBlockName :: String,
                    basicBlockParams :: [SSAVariable],
                    basicBlockExecParam :: Maybe SSAVariable,
                    basicBlockInstructions :: [Assign],
                    basicBlockTerminst :: Expression
                    }
