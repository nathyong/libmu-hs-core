{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

--An abstract syntax tree for Mu IR
--This allows us to generate mu from haskell

module LibMu.MuSyntax where 

import Prelude (Eq(..), Ord(..), Show(..), String, Int, Bool, Maybe(..))


data CallConvention = Mu
                    | Foreign String

data Scope = Local
           | Global
             deriving (Eq)
             
data UvmType = MuInt {intLen :: Int}
             | MuFloat
             | MuDouble
             | Ref {refType :: UvmTypeDef}
             | IRef {irefType :: UvmTypeDef}
             | WeakRef {weakRefType :: UvmTypeDef}
             | UPtr {uptrType :: UvmTypeDef}
             | Struct {structTypes :: [UvmTypeDef]}
             | Array {arrayType :: UvmTypeDef, arrayLen :: Int}
             | Hybrid {hybridTypes :: [UvmTypeDef], hybridType :: UvmTypeDef}
             | Void
             | ThreadRef
             | StackRef
             | FrameCursorRef
             | TagRef64
             | Vector {vectorType :: UvmTypeDef, vectorLen :: Int}
             | FuncRef {funcRefSig :: FuncSig}
             | UFuncPtr {ufuncPtrSig ::FuncSig}
               deriving (Eq, Ord)


data SSAVariable = SSAVariable {
  varScope :: Scope,
  varID :: String,
  varType :: UvmTypeDef
  }

data UvmTypeDef = UvmTypeDef {
  uvmTypeDefName :: String,
  uvmTypeDefType :: UvmType
  } deriving (Eq, Ord)

data FuncSig = FuncSig {
  funcSigName :: String,
  funcSigArgTypes :: [UvmTypeDef],
  funcSigReturnType :: [UvmTypeDef]
  } deriving (Eq, Ord)

data ExceptionClause = ExceptionClause {
  exceptionNor :: DestinationClause,
  exceptionExc :: DestinationClause
  }

data WPExceptionClause = WPExceptionClause {
  wpExceptionDest :: DestinationClause
  }

newtype KeepAliveClause = KeepAlive { keepAliveVars :: [SSAVariable] }

--destination clause as defined in spec %cont(%a)
data DestinationClause = DestinationClause {
  destClauseDestination :: String,
  destClauseArgList :: [SSAVariable]
  }

--destination, the place wich code execution could continue at %cont(<@T> %a)
data Destination = Destination {
  destDestination :: String,
  destArgList :: [SSAVariable],
  destExecHandler :: Maybe SSAVariable
  }

newtype Flag = Flag { flagValue :: String }

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
              deriving (Show)

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
               deriving (Show)

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
               deriving (Show)

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
                 deriving (Show)

data MemoryOrder = NOT_ATOMIC
                 | RELAXED
                 | CONSUME
                 | ACQUIRE
                 | RELEASE
                 | ACQ_REL
                 | SEQ_CST
                 deriving (Show)

data Expression = BinaryOperation {
  --The operation to be performed (Add, sub ...)
  binOp :: BinaryOp,
  --The type of both parameters (they must be the same type)
  binType :: UvmTypeDef,
  --The first parameter
  binV1 :: SSAVariable,
  --The second parameter
  binV2 :: SSAVariable,
  --The optional exception clause
  execClause :: Maybe ExceptionClause
  }
                  --ToDo Comment here
                | CompareOperation {
  cmpOp :: CompareOp,
  cmpType :: UvmTypeDef,
  cmpV1 :: SSAVariable,
  cmpV2 :: SSAVariable
  }
                | ConvertOperation {
  convOp :: ConvertOp,
  convTypeSrc :: UvmTypeDef,
  convTypeDest :: UvmTypeDef,
  convV :: SSAVariable,
  convExceptionClause :: Maybe ExceptionClause
  }
                | AtomicRMWOperation {
  --Bool indicates if Loc is a pointer
  aRMWIsPtr :: Bool,
  --The memory order for the operation 
  aRMWMemOrd :: MemoryOrder,
  --The operation to be performed
  aRMWOp :: AtomicRMWOp,
  --The type of loc
  aRMWType :: UvmTypeDef,
  --The memory location/address to access. 
  aRMWLoc :: SSAVariable,
  --The literal to be used
  aRMWOpnd :: SSAVariable,
  --The optional Exception clause
  aRMWExecClause :: Maybe ExceptionClause
  }
                | CmpXchg {
  --Bool indicating if loc is a pointer
  cmpXchgIsPtr :: Bool,
  --Bool indicating if operation is weak
  cmpXchgIsWeak :: Bool,
  --Memory order for operation success
  cmpXchgMemOrdSucc :: MemoryOrder,
  --Memory order for operation failure
  cmpXchgMemOrdFail :: MemoryOrder,
  --The type of the operation. Must be EQ comparable
  cmpXchgType :: UvmTypeDef,
  --variable of IRef<T> or UPtr<T>. The memory loc/addr to access
  cmpXchgLoc :: SSAVariable,
  --Strong Variant represents expected value in memory
  cmpXchgExpect :: SSAVariable,
  --Strong Variant represents Desired value in memory
  cmpXchgDesired ::SSAVariable,
  --Optional exception clause
  cmpXchgExecClause :: Maybe ExceptionClause
  }
                | Fence {
  --Memory order for fence operation
  fenceMemOrd :: MemoryOrder
  }
                | New {
  --Type to allocate from heap
  newType :: UvmTypeDef,
  --exception clause if operation fails
  newExecClause :: Maybe ExceptionClause
  }
                | NewHybrid {
  --Hybrid type to alloate from heap
  newHybridType :: UvmTypeDef,
  --Length of hybrid type (must be int)
  newHybridLenType :: UvmTypeDef,
  --Length of hybrid
  newHybridLen :: SSAVariable,
  --Exception clause if operation fails
  newHybridExecClause :: Maybe ExceptionClause
  }
                | Alloca {
  --Type to allocate
  allocaType :: UvmTypeDef,
  --Exception clause if operation fails
  allocaExecClause :: Maybe ExceptionClause
  }
                | AllocaHybrid {
  --Hybrid Type to allocate  
  allocaHybridType :: UvmTypeDef,
  --Length of hybrid type (must be int)
  allocaHybridLenType :: UvmTypeDef,
  --Length of hybrid
  allocaHybridLen :: SSAVariable,
  --Exception clause if operation fails
  allocaHybridExecClause :: Maybe ExceptionClause
  }
                  --ToDo Comment here
                | Return {
  returnValues :: [SSAVariable]
  }
                | Throw {
  throwException :: SSAVariable
  }
                | Call {
  callSignature :: FuncSig,
  callCallee :: SSAVariable,
  callArgList :: [SSAVariable],
  callExceptionClause :: Maybe ExceptionClause,
  callKeepAliveClause :: Maybe KeepAliveClause
  }
                | CCall {
  ccallCallConv :: CallConvention,
  ccallType :: UvmTypeDef,
  ccallSig :: FuncSig,
  ccallCallee :: SSAVariable,
  ccallArgList :: [SSAVariable],
  ccallExceptionClause :: Maybe ExceptionClause,
  ccallKeepAliveClause :: Maybe KeepAliveClause
                        }
                | TailCall {
  tailCallSignature :: FuncSig,
  tailCallCallee :: SSAVariable,
  tailCallArgList :: [SSAVariable]
  }
                | Branch1 {
  branch1Destination :: DestinationClause
  }
                | Branch2 {
  branch2Cond :: SSAVariable,
  branch2BranchTrue :: DestinationClause,
  branch2BranchFalse :: DestinationClause
  }
                | WatchPoint {
  watchpointname :: SSAVariable,
  watchpointId :: Int,
  watchpointTypes :: [UvmTypeDef],
  watchpointdis :: DestinationClause,
  watchpointena :: DestinationClause,
  watchpointWpExec :: Maybe WPExceptionClause,
  watchpointKeepAlive :: Maybe KeepAliveClause
  }
                | Trap {
  trapName :: SSAVariable,
  trapTypes :: [UvmTypeDef],
  trapExceptionClause :: Maybe ExceptionClause,
  trapKeepAlive :: Maybe KeepAliveClause
  }
                | WPBranch {
  wpBranchId :: Int,
  wpBranchDis :: DestinationClause,
  wpBranchEna :: DestinationClause
  }
                | Switch {
  switchType :: UvmTypeDef,
  switchOpnd :: SSAVariable,
  switchDefault :: DestinationClause,
  switchBlocks :: [(SSAVariable, DestinationClause)]
  }
                | SwapStack {
  swapStackSwapee :: SSAVariable,
  swapStackCurStackClause :: CurStackClause,
  swapStackNewStackClause :: NewStackClause,
  swapStackExecClause :: Maybe ExceptionClause,
  swapStackKeepAliveClause :: Maybe KeepAliveClause
  }
                | NewThread {
  newThreadStack :: SSAVariable,
  newThreadStackClause :: NewStackClause,
  newThreadExceptionClause :: Maybe ExceptionClause
  }
                | Comminst {
  comminstInst :: String,
  comminstFlags :: Maybe [Flag],
  comminstTypes :: Maybe [UvmTypeDef],
  comminstSigs :: Maybe [FuncSig],
  comminstArgs :: Maybe [SSAVariable],
  comminstExecClause :: Maybe ExceptionClause,
  comminstKeepAliveClause :: Maybe KeepAliveClause
  }
                | Load {
  --Bool indicating if Loc is a poiner
  loadIsPtr :: Bool,
  --Optional Memory order (default NOT_ATOMIC)
  loadMemOrd :: Maybe MemoryOrder,
  --The referant type of loc
  loadType :: UvmTypeDef,
  --Variable of type IRef or UPtr (the mem location to load from)
  loadLoc :: SSAVariable,
  --Exception clause if operation fails
  loadExecClause :: Maybe ExceptionClause
  }
                | Store {
  --Bool indicating if loc is a pointer
  storeIsPtr :: Bool,
  --Memory order for operation
  storeMemOrd :: Maybe MemoryOrder,
  --Type of loc
  storeType :: UvmTypeDef,
  --variable of IRef or UPtr. Mem loc/addr to store into
  storeLoc :: SSAVariable,
  --The new value to store
  storeNewVal :: SSAVariable,
  --Optional exception clause
  storeExecClause :: Maybe ExceptionClause
  }
                  --ToDo Comment Here
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
