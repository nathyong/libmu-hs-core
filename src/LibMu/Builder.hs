{-# LANGUAGE NoImplicitPrelude, FlexibleInstances#-}

module LibMu.Builder (
  BuilderState,
  Builder,
  Function,
  Block,
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

  putComment,

  putGetElemIRef,

  putIf,
  putIfElse,
  putWhile,
  
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
  Program(..),

  loadStdPrelude,
  loadPrelude
  ) where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.Strict
import           LibMu.PrettyPrint                (PrettyPrint (..))
import           LibMu.TypeCheck                  (Log, checkAssign, checkAst,
                                                   checkExpression, retType)
import           Prelude hiding (EQ)

import qualified Data.Map.Strict                  as M
import           Text.Printf                      (printf)

import           LibMu.MuSyntax
import           LibMu.PrettyPrint
import           LibMu.TypeCheck
import           LibMu.MuPrelude

--Holds Program state for lookup by name
data BuilderState = BuilderState {
  constants      :: M.Map String Declaration,
  typedefs       :: M.Map String Declaration,
  typedefOrd     :: [String], -- hold the order in which typedefs were declared
  funcsigs       :: M.Map String Declaration,
  funcdecls      :: M.Map String Declaration,
  globals        :: M.Map String Declaration,
  exposes        :: M.Map String Declaration,
  functionDefs   :: M.Map String Declaration,
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

loadStdPrelude :: Builder ([UvmTypeDef], [SSAVariable], [FuncSig])
loadStdPrelude = loadPrelude preludeContents
                   
loadPrelude :: MuPrelude -> Builder ([UvmTypeDef], [SSAVariable], [FuncSig])
loadPrelude (types, consts, sigs) = do
  lift $ modify (\pState ->
                  pState {
                    typedefs = M.union (typedefs pState) (M.fromList tPrelude),
                    typedefOrd = (typedefOrd pState) ++ tNames,
                    constants = M.union (constants pState) (M.fromList cPrelude),
                    funcsigs = M.union (funcsigs pState) (M.fromList sPrelude)
                    }
                )
  return (types, map constVariable consts, sigs)
                 where
                   tNames = map uvmTypeDefName types
                   cNames = map (varID . constVariable) consts
                   sNames = map funcSigName sigs

                   tPrelude = zip tNames (map Typedef types)
                   cPrelude = zip cNames consts
                   sPrelude = zip sNames (map FunctionSignature sigs)

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
                      functionDefs = M.insert (fName ++ fVer) (FunctionDef fName fVer fSig ((BasicBlock name vars exec [] (Return [])):fBody)) (functionDefs pState)
                           })
  return $ Block name fn


{-
Basic Block Manipulation functions.
Each function can be used to add an instruction to a given basic block.

withBasicBlock block $
    putBinOp ... >>-
    putCmpOp ... >>-
    setTermInstBranch ...
-}


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
  basicBlockInstructions = (Assign [assignee] (BinaryOperation op opType v1 v2 exec)):(basicBlockInstructions block)
  }

putConvOp :: ConvertOp -> SSAVariable -> UvmTypeDef -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putConvOp op assignee dest var exec block = block {
  basicBlockInstructions = (Assign [assignee] (ConvertOperation op (varType var) dest var exec)):(basicBlockInstructions block)
  }

putCmpOp :: CompareOp -> SSAVariable -> SSAVariable -> SSAVariable -> BasicBlock -> BasicBlock
putCmpOp op assignee v1@(SSAVariable _ _ opType) v2 block = block {
  basicBlockInstructions = (Assign [assignee] (CompareOperation op opType v1 v2)):(basicBlockInstructions block)
  }

putAtomicRMW :: AtomicRMWOp -> SSAVariable -> Bool -> MemoryOrder -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putAtomicRMW op assignee ptr memOrd loc opnd exec block = block {
  basicBlockInstructions = (Assign [assignee] (AtomicRMWOperation ptr memOrd op (varType opnd) loc opnd exec)):(basicBlockInstructions block)
  }

putCmpXchg :: (SSAVariable, SSAVariable) -> Bool -> Bool -> MemoryOrder -> MemoryOrder -> SSAVariable -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putCmpXchg (ass1, ass2) ptr weak mem1 mem2 loc expec desir exec block = case uvmTypeDefType $ varType loc of
  UPtr t -> block {
    basicBlockInstructions = (Assign [ass1, ass2] (CmpXchg ptr weak mem1 mem2 t loc expec desir exec)):(basicBlockInstructions block)
    }
  IRef t -> block {
    basicBlockInstructions = (Assign [ass1, ass2] (CmpXchg ptr weak mem1 mem2 t loc expec desir exec)):(basicBlockInstructions block)
    }
  _ -> block { --Incorrect types, let it fail else where
  basicBlockInstructions = (Assign [ass1, ass2] (CmpXchg ptr weak mem1 mem2 (varType loc) loc expec desir exec)):(basicBlockInstructions block)
    }

putFence :: MemoryOrder -> BasicBlock -> BasicBlock
putFence memOrd block = block {
  basicBlockInstructions = (Assign [] (Fence memOrd)):(basicBlockInstructions block)
  }

putNew :: SSAVariable -> UvmTypeDef -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putNew assignee t exec block = block {
  basicBlockInstructions = (Assign [assignee] (New t exec)):(basicBlockInstructions block)
  }

putNewHybrid :: SSAVariable -> UvmTypeDef -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putNewHybrid assignee t len exec block = block {
  basicBlockInstructions = (Assign [assignee] (NewHybrid t (varType len) len exec)):(basicBlockInstructions block)
                                               }
putAlloca :: SSAVariable -> UvmTypeDef -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putAlloca assignee t exec block = block {
  basicBlockInstructions = (Assign [assignee] (Alloca t exec)):(basicBlockInstructions block)
  }

putAllocaHybrid :: SSAVariable -> UvmTypeDef -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putAllocaHybrid assignee t len exec block = block {
  basicBlockInstructions = (Assign [assignee] (AllocaHybrid t (varType len) len exec)):(basicBlockInstructions block)
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
  basicBlockInstructions = (Assign assignee (Call sig func args exec (KeepAlive <$> alive))):(basicBlockInstructions block)
  }

putCCall :: [SSAVariable] ->  CallConvention -> UvmTypeDef -> FuncSig -> SSAVariable -> [SSAVariable] -> Maybe ExceptionClause -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
putCCall assignee callConv t sig callee args exec alive block = block {
  basicBlockInstructions = (Assign assignee (CCall callConv t sig callee args exec (KeepAlive <$> alive))):(basicBlockInstructions block)
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

putWatchPoint :: [SSAVariable] -> SSAVariable -> Int -> [UvmTypeDef] -> Block -> [SSAVariable] -> Block -> [SSAVariable] -> Maybe (Block, [SSAVariable]) -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
putWatchPoint assignee name wpid ts (Block dis _) disArgs (Block ena _) enaArgs wpexec alive block = block {
    basicBlockInstructions = (Assign assignee (WatchPoint name wpid ts (DestinationClause dis disArgs) (DestinationClause ena enaArgs) wp (KeepAlive <$> alive))):(basicBlockInstructions block)
    }
  where
    wp = case wpexec of
      Nothing -> Nothing
      Just (Block wpBlock _, wpVars) -> Just $ WPExceptionClause $ DestinationClause wpBlock wpVars


putTrap :: [SSAVariable] -> SSAVariable -> [UvmTypeDef] -> Maybe ExceptionClause -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
putTrap assignee name ts exec alive block = block {
  basicBlockInstructions = (Assign assignee (Trap name ts exec (KeepAlive <$> alive))):(basicBlockInstructions block)
  }


setTermInstWatchPoint :: SSAVariable -> Int -> [UvmTypeDef] -> Block -> [SSAVariable] -> Block -> [SSAVariable] -> Maybe (Block, [SSAVariable]) -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
setTermInstWatchPoint name wpid ts (Block dis _) disArgs (Block ena _) enaArgs wpexec alive block = block {
    basicBlockTerminst = WatchPoint name wpid ts (DestinationClause dis disArgs) (DestinationClause ena enaArgs) wp (KeepAlive <$> alive)
    }
  where
    wp = case wpexec of
      Nothing -> Nothing
      Just (Block wpBlock _, wpVars) -> Just $ WPExceptionClause $ DestinationClause wpBlock wpVars


setTermInstTrap :: SSAVariable -> [UvmTypeDef] -> Maybe ExceptionClause -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
setTermInstTrap name ts exec alive block = block {
  basicBlockTerminst = Trap name ts exec (KeepAlive <$> alive)
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
  basicBlockInstructions = (Assign [assignee] (NewThread stack nsClause exec)):(basicBlockInstructions block)
  }

putComminst :: [SSAVariable] -> String -> [String] -> [UvmTypeDef] -> [FuncSig] -> [SSAVariable] -> Maybe ExceptionClause -> Maybe [SSAVariable] -> BasicBlock -> BasicBlock
putComminst assignee name flags types sigs args exec alive block = block {
  basicBlockInstructions = (Assign assignee (Comminst name (toMaybe $ map Flag flags) (toMaybe types) (toMaybe sigs) (toMaybe args) exec (KeepAlive <$> alive))):(basicBlockInstructions block)
  }
  where
    toMaybe :: [a] -> Maybe [a]
    toMaybe lst = case lst of
      [] -> Nothing
      _  -> Just lst

putLoad :: SSAVariable -> Bool -> Maybe MemoryOrder -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putLoad assignee ptr memOrd var exec block = block {
  basicBlockInstructions = (Assign [assignee] (Load ptr memOrd vType var  exec)):(basicBlockInstructions block)
  }
  where
    vType :: UvmTypeDef
    vType = case uvmTypeDefType $ varType var of
      IRef t -> t
      UPtr t -> t
      _ -> undefined --varType var --errorful type, but let it fail elsewhere

putStore :: Bool -> Maybe MemoryOrder -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putStore ptr memOrd loc newVal exec block = block {
  basicBlockInstructions = (Assign [] (Store ptr memOrd locType loc newVal exec)):(basicBlockInstructions block)
  }
  where
    locType :: UvmTypeDef
    locType = case uvmTypeDefType $ varType loc of
      IRef t -> t
      UPtr t -> t
      _ -> varType loc --errorful type, but let it fail elsewhere

putExtractValueS1 :: SSAVariable -> Int -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putExtractValueS1 assignee index opnd exec block = block {
  basicBlockInstructions = (Assign [assignee] (ExtractValueS (varType opnd) index opnd exec)):(basicBlockInstructions block)
  }

putInsertValueS :: SSAVariable -> Int -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putInsertValueS assignee index opnd newVal exec block = block {
  basicBlockInstructions = (Assign [assignee] (InsertValueS (varType opnd) index newVal opnd exec)):(basicBlockInstructions block)
  }

putExtractValue :: SSAVariable -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putExtractValue assignee opnd index exec block = block {
  basicBlockInstructions = (Assign [assignee] (ExtractValue (varType opnd) (varType index) opnd index exec)):(basicBlockInstructions block)
  }

putInsertValue :: SSAVariable -> SSAVariable -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putInsertValue assignee opnd index newVal exec block = block {
  basicBlockInstructions = (Assign [assignee] (InsertValue (varType opnd) (varType index) opnd index newVal exec)):(basicBlockInstructions block)
  }

putShuffleVector :: SSAVariable -> SSAVariable -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putShuffleVector assignee v1 v2 mask exec block = block {
  basicBlockInstructions = (Assign [assignee] (ShuffleVector (varType v1) (varType mask) v1 v2 mask exec)):(basicBlockInstructions block)
  }


putGetIRef :: SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putGetIRef assignee opnd exec block = block {
  basicBlockInstructions = (Assign [assignee] (GetIRef opndType opnd exec)):(basicBlockInstructions block)
  }
  where
    opndType :: UvmTypeDef
    opndType = case uvmTypeDefType $ varType opnd of
      Ref t -> t
      _ -> varType opnd --errorful type, but let it fail elsewhere


putGetFieldIRef :: SSAVariable -> Bool -> Int -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putGetFieldIRef assignee ptr index opnd exec block = block {
  basicBlockInstructions = (Assign [assignee] (GetFieldIRef ptr opndType index opnd exec )):(basicBlockInstructions block)
  }
  where
    opndType :: UvmTypeDef
    opndType = case uvmTypeDefType $ varType opnd of
      IRef t -> t
      UPtr t -> t
      _ -> varType opnd --errorful type, but let it fail elsewhere

putGetElemIRef :: SSAVariable -> Bool -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putGetElemIRef assignee ptr opnd index exec block = block {
  basicBlockInstructions = (Assign [assignee] (GetElemIRef ptr opndType (varType index) opnd index exec)):(basicBlockInstructions block)
  }
  where
    opndType :: UvmTypeDef
    opndType = case uvmTypeDefType $ varType opnd of
      IRef t -> t
      UPtr t -> t
      _ -> varType opnd --errorful type, but let it fail elsewhere


putShiftIRef :: SSAVariable -> Bool -> SSAVariable -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putShiftIRef assignee ptr opnd offset exec block = block {
  basicBlockInstructions = (Assign [assignee] (ShiftIRef ptr opndType (varType offset) opnd offset exec)):(basicBlockInstructions block)
  }
  where
    opndType :: UvmTypeDef
    opndType = case uvmTypeDefType $ varType opnd of
      IRef t -> t
      UPtr t -> t
      _ -> varType opnd --errorful type, but let it fail elsewhere


putGetVarPartIRef :: SSAVariable -> Bool -> SSAVariable -> Maybe ExceptionClause -> BasicBlock -> BasicBlock
putGetVarPartIRef assignee ptr opnd exec block = block {
  basicBlockInstructions = (Assign [assignee] (GetVarPartIRef ptr opndType opnd exec)):(basicBlockInstructions block)
  }
  where
    opndType :: UvmTypeDef
    opndType = case uvmTypeDefType $ varType opnd of
      IRef t -> t
      UPtr t -> t
      _ -> varType opnd --errorful type, but let it fail elsewhere


putComment :: String -> BasicBlock -> BasicBlock
putComment str block = block {
  basicBlockInstructions = (Assign [] (Comment str)):(basicBlockInstructions block)
  }

{-
These Functions build generic program structures.

Overall, each function must take context, in the form of a block representing the "entry" point of the statment.
An int parsed to each function allows the function to generate unique block names to avoid name clashes.

The programmer must pass contexts, those variables passed to each block generated. This documented no a per function basis.

The functions also take a (BasicBlock -> BasicBlock) program(s) which are inserted into appropriate blocks as instructions to perform
various tasks (such as a loop body, or conditional).

Lastly, the functions all return references to each block they produced, in the order that they were produced. This is done to allow "fine tuning"
such as nesting loops & if's  or puting in some more code later on.
-}

{-
putIf: putIf will insert a conditionally evaluated code block into a given entry point.
e.g.

entry():
  %cmp_res = EQ <@i32> %a %b
  //putIf generates everything from here
  branch2 %cmp_res progBlock(%a, %b) contBlock(%a, %b)
progBlock(<@i32> %a, <@i32> %b):
  ...
  ...
  branch contBlock(%c, %d)
contBlock(<@i32> %a, <@i32> %b):
  //to here
  ...

one interesting point to note:
  - the variables parsed to progBlock & contBlock are the same. This is for simplicity, this should not be a problem using the following workaround.

if you wish to pass a different set of variables to progBlock then to contBlock, simply include ALL variables in the context passed to putIf. some variables
will be ignored in either contBlock or progBlock, but this is no problem.


The return values of the progBlock are specified by the second contex passed to putIf.
The SSAVariable passed tells putIf which variable in the entry block is to be used as the conditional boolean.
The last parameter is a program which is inserted into the conditionally executed block.

example usage:
withBasicBlock entry $
  putCmpOp EQ cmp_res a b >>-
  putComment "putIf generates everything from here"

(_, contBlock) <- putIf 2 entry [a, b] [c, d] cmp_res $
  . . . (some program producing variables c & d)

withBasicBlock contBlock $
  putComment "to here" >>-
  ... continue on.

-}

type Context = [SSAVariable]

putIf :: Int -> Block -> Context -> Context -> SSAVariable ->  (BasicBlock -> BasicBlock) -> Builder (Block, Block)
putIf count entry@(Block _ func) ctx rets cond prog = do
  progBlock <- putBasicBlock (printf "progBlock%.5d" count) ctx Nothing func
  contBlock <- putBasicBlock (printf "contBlock%.5d" count) ctx Nothing func

  withBasicBlock entry $
    setTermInstBranch2 cond progBlock ctx contBlock ctx
  
  withBasicBlock progBlock $
    putComment "If True Block" >>-
    prog >>-
    setTermInstBranch contBlock rets

  return (progBlock, contBlock)


{-
putIfElse
-}

putIfElse :: Int -> Block -> Context -> Context -> Context -> Context -> SSAVariable ->  (BasicBlock -> BasicBlock) -> (BasicBlock -> BasicBlock) -> Builder (Block, Block, Block)
putIfElse count entry@(Block _ func) ctx retsTrue retsFalse rets cond trueProg falseProg = do
  trueBlock <- putBasicBlock (printf "trueBlock%.5d" count) ctx Nothing func
  falseBlock <- putBasicBlock (printf "falseBlock%.5d" count) ctx Nothing func
  contBlock <- putBasicBlock (printf "contBlock%.5d" count) rets Nothing func

  withBasicBlock entry $
    setTermInstBranch2 cond trueBlock ctx falseBlock ctx

  withBasicBlock trueBlock $
    putComment "If True Block" >>-
    trueProg >>-
    setTermInstBranch contBlock retsTrue
  
  withBasicBlock falseBlock $
    putComment "If False Block" >>-
    falseProg >>-
    setTermInstBranch contBlock retsFalse

  return (trueBlock, falseBlock, contBlock)

putWhile :: Int -> Block -> Context -> Context -> Context -> SSAVariable -> (BasicBlock -> BasicBlock) -> (BasicBlock -> BasicBlock) -> Builder (Block, Block, Block)
putWhile count entry@(Block _ func) condCtx loopCtx contCtx condVar cond loop = do
  condBlock <- putBasicBlock (printf "condBlock%.5d" count) condCtx Nothing func
  loopBlock <- putBasicBlock (printf "loopBlock%.5d" count) loopCtx Nothing func
  contBlock <- putBasicBlock (printf "contBlock%.5d" count) contCtx Nothing func

  withBasicBlock entry $
    setTermInstBranch condBlock condCtx

  withBasicBlock condBlock $
    putComment "While Condition Block" >>-
    cond >>-
    setTermInstBranch2 condVar loopBlock loopCtx contBlock contCtx

  withBasicBlock loopBlock $
    putComment "While Loop Block" >>-
    loop >>-
    setTermInstBranch condBlock condCtx
  
  return (condBlock, loopBlock, contBlock)

{-
entry:
   branch condBlock[condCtx]
condBlock(condCtx):
   cmp_res = cmp a b
   branch2 cmp_res loopBlock (loopCtx) contBlock (contCtx)
loopBlock (loopCtx):
   ...
   Branch condBlock (condCtx)
contBlock (contCtx):
   ...
-}
