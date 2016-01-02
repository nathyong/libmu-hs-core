{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module LibMu.Builder (
  BuilderState(..),
  Builder,
  Function(..),
  Block(..),
  Error,

  runBuilder,
  flatten,
  emptyBuilderState,
  
  getTypedef,
  getTypedefs,
  containsType,

  getConstant,
  getConstants,
  containsConst,

  getFuncSig,
  containsFuncSig,

  getFuncDecl,
  containsFuncDecl,
  
  getGlobal,
  containsGlobal,
  
  getFuncDef,
  containsFuncDef,

  putFuncSig,
  putFunction,
  putTypeDef,
  putGlobal,
  putConstant,
  putFuncDecl,

  createVariable,
  createVariables,

  createExecClause,
  putBasicBlock,
  withBasicBlock,
  (>>-),
  
  putBinOp,
  putConvOp,
  putCmpOp,
  putAtomicRMW,
  putCmpXchg,
  putFence,
  putNew,
  putNewHybrid,
  putAlloca,
  putAllocaHybrid,
  setTermInstRet,
  setTermInstThrow,
  putCall,
  putCCall,
  setTermInstTailCall,
  setTermInstBranch,
  setTermInstBranch2,
  putWatchPoint,
  setTermInstTrap,
  setTermInstWPBranch,
  setTermInstSwitch,
  setTermInstSwapStack,
  putNewThread,
  putComminst,
  putLoad,
  putStore,

  putGetElemIRef,
  
  lift,
  get,

  Log,
  retType,
  checkExpression,
  checkAssign,
  checkAst,
  checkBuilder,

  PrettyPrint (..),

  Scope(..),
  CallConvention(..),
  UvmType(..),
  SSAVariable(..),
  UvmTypeDef(..),
  FuncSig(..),
  ExceptionClause(..),
  BinaryOp(..),
  CompareOp(..),
  ConvertOp(..),
  AtomicRMWOp(..),
  MemoryOrder(..),
  CurStackClause(..),
  NewStackClause(..),
  Program(..)
  ) where

import           Prelude
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except
import           LibMu.PrettyPrint (PrettyPrint(..))
import           LibMu.TypeCheck (
  Log, retType, checkExpression, checkAssign, checkAst)

import qualified Data.Map.Strict as M
import           Text.Printf (printf)

import           LibMu.MuSyntax
import           LibMu.PrettyPrint
import           LibMu.TypeCheck

--Holds Program state for lookup by name
data BuilderState = BuilderState {
  constants :: M.Map String Declaration,
  typedefs :: M.Map String Declaration,
  typedefOrd :: [String], -- hold the order in which typedefs were declared
  funcsigs :: M.Map String Declaration,
  funcdecls :: M.Map String Declaration,
  globals :: M.Map String Declaration,
  exposes :: M.Map String Declaration,
  functionDefs :: M.Map String Declaration,
  functionDefOrd :: [String] -- hold the order in which functions were declared
  }

data Function = Function {fnName :: String, fnVer :: String}
data Block = Block {blockName :: String, blockFn :: Function}

instance PrettyPrint (Either Error BuilderState) where
  ppFormat e = case e of
    Left err -> return err
    Right bs -> ppFormat bs

instance PrettyPrint BuilderState where
  ppFormat = ppFormat . flatten

flatten :: BuilderState -> Program
flatten (BuilderState cons tds tdOrd fs fdecl gl ex fdef fdefOrd) =
    case (,) <$> extractOrdered tds tdOrd <*> extractOrdered fdef fdefOrd of
      Nothing -> error "Failed to order typedefs appropriatly"
      Just (tdLst, fdLst) -> Program $ concat [
        tdLst,
        M.elems gl,
        M.elems cons,
        M.elems fdecl,
        M.elems fs,
        M.elems ex,
        fdLst
        ]

extractOrdered :: (Ord k) => M.Map k v -> [k] -> Maybe [v]
extractOrdered m mask = case mask of
  x:xs -> (:) <$> (M.lookup x m) <*> (extractOrdered m xs)
  [] -> pure []

checkBuilder :: BuilderState -> Log
checkBuilder = checkAst . flatten

emptyBuilderState :: BuilderState
emptyBuilderState = BuilderState M.empty M.empty [] M.empty M.empty M.empty M.empty M.empty []

type Error = String
type Builder = ExceptT Error (State BuilderState)

runBuilder :: Builder a -> BuilderState -> Either Error a
runBuilder b s = evalState (runExceptT b) s

getTypedef :: String -> Builder UvmTypeDef
getTypedef name = do
  pState <- lift get
  case M.lookup name (typedefs pState) of
    Nothing -> throwE $ printf "Failed to find typedef: %s" name
    Just (Typedef tDef) -> return tDef
    _ -> throwE $ printf "Found non typedef with specified id: %s" name

getTypedefs :: [String] -> Builder [UvmTypeDef]
getTypedefs = mapM getTypedef

containsType :: String -> Builder Bool
containsType name = do
  pState <- lift get
  case M.lookup name (typedefs pState) of
    Nothing -> return False
    Just (Typedef _) -> return True
    Just _ -> throwE $ printf "Found non typedef with specified id: %s" name

getConstant :: String -> Builder SSAVariable
getConstant name = do
  pState <- lift get
  case M.lookup name (constants pState) of
    Nothing -> throwE $ printf "Failed to find constant: %s" name
    Just (ConstDecl var _) -> return var
    _ -> throwE $ printf "Found non constant with specified id: %s" name

getConstants :: [String] -> Builder [SSAVariable]
getConstants = mapM getConstant

containsConst :: String -> Builder Bool
containsConst name = do
  pState <- lift get
  case M.lookup name (constants pState) of
    Nothing -> return False
    Just (ConstDecl _ _) -> return True
    Just _ -> throwE $ printf "Found non constant with specified id: %s" name

getFuncSig :: String -> Builder FuncSig
getFuncSig name = do
  pState <- lift get
  case M.lookup name (funcsigs pState) of
   Nothing -> throwE $ printf "Failed to find constant: %s" name
   Just (FunctionSignature sig) -> return sig
   _ -> throwE $ printf "Found non function signature with specified id: %s" name

containsFuncSig :: String -> Builder Bool
containsFuncSig name = do
  pState <- lift get
  case M.lookup name (typedefs pState) of
    Nothing -> return False
    Just (FunctionSignature _) -> return True
    Just _ -> throwE $ printf "Found non function signature with specified id: %s" name


getGlobal :: String -> Builder SSAVariable
getGlobal name = do
  pState <- lift get
  case M.lookup name (globals pState) of
    Nothing -> throwE $ printf "Failed to find global definition: %s" name
    Just (GlobalDef var _) -> return var
    _ -> throwE $ printf "Found non global with specified id: %s" name

containsGlobal :: String -> Builder Bool
containsGlobal name = do
  pState <- lift get
  case M.lookup name (globals pState) of
    Nothing -> return False
    Just (GlobalDef _ _) -> return True
    Just _ -> return False
    

getFuncDecl :: String -> Builder FuncSig
getFuncDecl name = do
  pState <- lift get
  case M.lookup name (funcdecls pState) of
    Nothing -> throwE $ printf "Failed to find funcdecl: %s" name
    Just (FunctionDecl _ sig) -> return sig
    Just _ -> throwE $ printf "Found non funcdecl with specified id: %s" name

containsFuncDecl :: String ->  Builder Bool
containsFuncDecl name = do
  pState <- lift get
  case M.lookup name (globals pState) of
    Nothing -> return False
    Just (FunctionDecl _ _) -> return True
    Just _ -> return False

getFuncDef :: String -> String -> Builder Declaration
getFuncDef name ver = do
  pState <- lift get
  case M.lookup (name ++ ver) (functionDefs pState) of
    Nothing -> throwE $ printf "Failed to find global definition: %s" (name ++ ver)
    Just func@(FunctionDef _ _ _ _) -> return func
    Just _ -> throwE $ printf "Found non function def with specified id: %s" (name ++ ver)

containsFuncDef :: String -> String -> Builder Bool
containsFuncDef name ver = do
  pState <- lift get
  case M.lookup (name ++ ver) (functionDefs pState) of
    Nothing -> return False
    Just (FunctionDef _ _ _ _) -> return True
    Just _ -> return False

putFuncSig :: String -> [UvmTypeDef] -> [UvmTypeDef] -> Builder FuncSig
putFuncSig name args ret = do
  let funcSig = FuncSig name args ret
  lift $ modify (\pState ->
                  pState {
                    funcsigs = M.insert name (FunctionSignature funcSig) (funcsigs pState)
                    })
  return funcSig

putFunction :: String -> String -> FuncSig -> Builder (Function, SSAVariable)
putFunction name ver sig = do
  lift $ modify $ (\pState ->
                    pState {
                      functionDefs = M.insert (name ++ ver) (FunctionDef name ver sig []) (functionDefs pState),
                      functionDefOrd = (functionDefOrd pState) ++ [name ++ ver]
                           })
  let funcRef = SSAVariable Global name (UvmTypeDef (printf "%s_ref" name) (FuncRef sig))
  return $ (Function name ver, funcRef)

putConstant :: String -> UvmTypeDef -> String -> Builder SSAVariable
putConstant name constType val = do
  let constVar = SSAVariable Global name constType
  lift $ modify (\pState ->
                  pState {
                    constants = M.insert name (ConstDecl constVar val) (constants pState)
                    })
  return constVar

putTypeDef :: String -> UvmType -> Builder UvmTypeDef
putTypeDef name uvmType = do
  let tDef = UvmTypeDef name uvmType
  lift $ modify (\pState ->
                  pState {
                    typedefs = M.insert name (Typedef tDef) (typedefs pState),
                    typedefOrd = (typedefOrd pState) ++ [name]
                    })
  return tDef

putFuncDecl :: String -> FuncSig -> Builder ()
putFuncDecl name fSig = 
  lift $ modify (\pState -> pState {funcdecls = M.insert name (FunctionDecl name fSig) (funcdecls pState)
                                   })

putGlobal :: String -> UvmTypeDef -> Builder SSAVariable
putGlobal name dType = do
  let var = SSAVariable Global name (UvmTypeDef ("iref" ++ uvmTypeDefName dType) (IRef dType))
  lift $ modify (\pState ->
                  pState {
                    globals = M.insert name (GlobalDef var dType) (globals pState)
                    })
  return var

createVariable :: String -> UvmTypeDef -> SSAVariable
createVariable name typeVal = SSAVariable Local name typeVal

createVariables :: UvmTypeDef -> [String] -> [SSAVariable]
createVariables t lst = map (flip createVariable t) lst

createExecClause :: Block -> [SSAVariable] -> Block -> [SSAVariable] -> ExceptionClause
createExecClause (Block b1 _) v1 (Block b2 _) v2 = ExceptionClause (DestinationClause b1 v1) (DestinationClause b2 v2)

putBasicBlock :: String -> [SSAVariable] -> Maybe SSAVariable -> Function -> Builder Block
putBasicBlock name vars exec fn@(Function func ver) = do
  (FunctionDef fName fVer fSig fBody) <- getFuncDef func ver
  lift $ modify $ (\pState ->
                    pState {
                      functionDefs = M.insert (fName ++ fVer) (FunctionDef fName fVer fSig (fBody ++ [BasicBlock name vars exec [] (Return [])])) (functionDefs pState)
                           })
  return $ Block name fn

withBasicBlock :: Block -> (BasicBlock -> BasicBlock) -> Builder ()
withBasicBlock (Block name (Function func ver)) prog = do
  (FunctionDef fName fVer fSig bodys) <- getFuncDef func ver
  newBody <- editBlock bodys
  lift $ modify (\pState ->
                  pState {
                    functionDefs = M.insert (fName ++ fVer) (FunctionDef fName fVer fSig newBody) (functionDefs pState)
                         })
    where
      editBlock :: [BasicBlock] -> Builder [BasicBlock]
      editBlock blocks = case blocks of
        x:xs
          | basicBlockName x == name -> return $ prog x:xs
          | otherwise -> (x:) <$> (editBlock xs)
        [] -> throwE $ printf "Failed to find basic block with id %s -> %s -> %s" func ver name

(>>-) :: (a -> b) -> (b -> c) -> a -> c
(>>-) = flip (.)

putBinOp :: BinaryOp -> SSAVariable -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putBinOp op assignee v1@(SSAVariable _ _ opType) v2 exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (BinaryOperation op opType v1 v2 exec)]
  }

putConvOp :: ConvertOp -> SSAVariable -> UvmTypeDef -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putConvOp op assignee dest var exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (ConvertOperation op (varType var) dest var exec)]
  }

putCmpOp :: CompareOp -> SSAVariable -> SSAVariable -> SSAVariable -> BasicBlock -> BasicBlock
putCmpOp op assignee v1@(SSAVariable _ _ opType) v2 block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (CompareOperation op opType v1 v2)]
  }

putAtomicRMW :: AtomicRMWOp -> SSAVariable -> Bool -> MemoryOrder -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putAtomicRMW op assignee ptr memOrd loc opnd exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (AtomicRMWOperation ptr memOrd op (varType opnd) loc opnd exec)]
  }

putCmpXchg :: (SSAVariable, SSAVariable) -> Bool -> Bool -> MemoryOrder -> MemoryOrder -> SSAVariable -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putCmpXchg (ass1, ass2) ptr weak mem1 mem2 loc expec desir exec block = case uvmTypeDefType $ varType loc of
  UPtr t -> block {
    basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [ass1, ass2] (CmpXchg ptr weak mem1 mem2 t loc expec desir exec)]
    }
  IRef t -> block {
    basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [ass1, ass2] (CmpXchg ptr weak mem1 mem2 t loc expec desir exec)]
    }
  _ -> block { --Incorrect types, let it fail else where
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [ass1, ass2] (CmpXchg ptr weak mem1 mem2 (varType loc) loc expec desir exec)]
    }

putFence :: MemoryOrder -> BasicBlock -> BasicBlock
putFence memOrd block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [] (Fence memOrd)]
  }

putNew :: SSAVariable -> UvmTypeDef -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putNew assignee t exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (New t exec)]
  }

putNewHybrid :: SSAVariable -> UvmTypeDef -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putNewHybrid assignee t len exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (NewHybrid t (varType len) len exec)]
                                               }
putAlloca :: SSAVariable -> UvmTypeDef -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putAlloca assignee t exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (Alloca t exec)]
  }

putAllocaHybrid :: SSAVariable -> UvmTypeDef -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putAllocaHybrid assignee t len exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (AllocaHybrid t (varType len) len exec)]
  }

setTermInstRet :: [SSAVariable] -> BasicBlock -> BasicBlock
setTermInstRet rets block = block {
  basicBlockTerminst = Return rets
  }

setTermInstThrow :: SSAVariable -> BasicBlock -> BasicBlock
setTermInstThrow var block = block {
  basicBlockTerminst = Throw var
  }

putCall :: [SSAVariable] -> SSAVariable -> FuncSig -> [SSAVariable] -> Maybe ExceptionClause -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
putCall assignee func sig args exec alive block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign assignee (Call sig func args exec (KeepAlive <$> alive))]
  }

putCCall :: [SSAVariable] ->  CallConvention -> UvmTypeDef -> FuncSig -> SSAVariable -> [SSAVariable] -> Maybe ExceptionClause -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
putCCall assignee callConv t sig callee args exec alive block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign assignee (CCall callConv t sig callee args exec (KeepAlive <$> alive))]
  }

setTermInstTailCall :: FuncSig -> SSAVariable -> [SSAVariable] -> BasicBlock -> BasicBlock
setTermInstTailCall sig callee args block = block {
  basicBlockTerminst = (TailCall sig callee args)
  }

setTermInstBranch :: Block -> [SSAVariable] -> BasicBlock -> BasicBlock
setTermInstBranch (Block dest _) vars block = block {
  basicBlockTerminst = Branch1 $ DestinationClause dest vars
  }

setTermInstBranch2 :: SSAVariable -> Block -> [SSAVariable] -> Block -> [SSAVariable] -> BasicBlock -> BasicBlock
setTermInstBranch2 cond (Block trueBlock _) trueVars (Block falseBlock _) falseVars block = block {
  basicBlockTerminst = Branch2 cond (DestinationClause trueBlock trueVars) (DestinationClause falseBlock falseVars)
  }

putWatchPoint :: [SSAVariable] -> Int -> [UvmTypeDef] -> Block -> [SSAVariable] -> Block -> [SSAVariable] -> Maybe (Block, [SSAVariable]) -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
putWatchPoint assignee wpid ts (Block dis _) disArgs (Block ena _) enaArgs wpexec alive block = block {
    basicBlockInstructions = (basicBlockInstructions block) ++ [Assign assignee (WatchPoint wpid ts (DestinationClause dis disArgs) (DestinationClause ena enaArgs) wp (KeepAlive <$> alive))]
    }
  where
    wp = case wpexec of
      Nothing -> Nothing
      Just (Block wpBlock _, wpVars) -> Just $ WPExceptionClause $ DestinationClause wpBlock wpVars
  
setTermInstTrap :: [UvmTypeDef] -> Maybe ExceptionClause -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
setTermInstTrap ts exec alive block = block {
  basicBlockTerminst = Trap ts exec (KeepAlive <$> alive)
  }

setTermInstWPBranch :: Int -> Block -> [SSAVariable] -> Block -> [SSAVariable] -> BasicBlock -> BasicBlock
setTermInstWPBranch wpid (Block disBlock _) disArgs (Block enaBlock _) enaArgs block = block {
  basicBlockTerminst = WPBranch wpid (DestinationClause disBlock disArgs) (DestinationClause enaBlock enaArgs)
  }

setTermInstSwitch :: SSAVariable -> Block -> [SSAVariable] -> [(SSAVariable, Block, [SSAVariable])] -> BasicBlock -> BasicBlock
setTermInstSwitch cond (Block defBlock _) defArgs blocks block = block {
  basicBlockTerminst = Switch (varType cond) cond (DestinationClause defBlock defArgs) (map toBlocks blocks)
  }
  where
    toBlocks :: (SSAVariable, Block, [SSAVariable]) -> (SSAVariable, DestinationClause)
    toBlocks (cond, (Block block _), args) = (cond, DestinationClause block args)

setTermInstSwapStack :: SSAVariable -> CurStackClause -> NewStackClause -> Maybe ExceptionClause -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
setTermInstSwapStack swappee csClause nsClause exec alive block = block {
  basicBlockTerminst = SwapStack swappee csClause nsClause exec (KeepAlive <$> alive)
  }

putNewThread :: SSAVariable -> SSAVariable -> NewStackClause -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putNewThread assignee stack nsClause exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (NewThread stack nsClause exec)]
  }

putComminst :: [SSAVariable] -> String -> [String] -> [UvmTypeDef] -> [FuncSig] -> [SSAVariable] -> Maybe ExceptionClause -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
putComminst assignee name flags types sigs args exec alive block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign assignee (Comminst name (toMaybe $ map Flag flags) (toMaybe types) (toMaybe sigs) (toMaybe args) exec (KeepAlive <$> alive))]
  }
  where
    toMaybe :: [a] -> Maybe [a]
    toMaybe lst = case lst of
      [] -> Nothing
      _  -> Just lst

putLoad :: SSAVariable -> Bool -> Maybe MemoryOrder -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putLoad assignee ptr memOrd var exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (Load ptr memOrd vType var  exec)] 
  }
  where
    vType :: UvmTypeDef
    vType = case uvmTypeDefType $ varType var of
      IRef t -> t
      UPtr t -> t
      _ -> undefined --varType var --errorful type, but let it fail elsewhere

putStore :: Bool -> Maybe MemoryOrder -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putStore ptr memOrd loc newVal exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [] (Store ptr memOrd locType loc newVal exec)]
  }
  where
    locType :: UvmTypeDef
    locType = case uvmTypeDefType $ varType loc of
      IRef t -> t
      UPtr t -> t
      _ -> varType loc --errorful type, but let it fail elsewhere

--ToDo put Extract Values & Insert Values

--ToDo getIRef

--ToDo getFieldIRef

putGetElemIRef :: SSAVariable -> Bool -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putGetElemIRef assignee ptr opnd index exec block = block {
  basicBlockInstructions = (basicBlockInstructions block) ++ [Assign [assignee] (GetElemIRef ptr opndType (varType index) opnd index exec)]
  }
  where
    opndType :: UvmTypeDef
    opndType = case uvmTypeDefType $ varType opnd of
      IRef t -> t
      UPtr t -> t
      _ -> varType opnd --errorful type, but let it fail elsewhere
