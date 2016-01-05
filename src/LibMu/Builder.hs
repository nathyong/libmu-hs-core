{-#LANGUAGE NoImplicitPrelude, FlexibleInstances#-}

module Builder where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
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
nn  lift $ modify $ (\pState ->
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

putOp :: Expression -> Builder [SSAVariable]
putOp expr = case expr of
  _ -> error "not yet implented"

add,  sub, mul, sdiv, srem, udiv, urem, shl, lshr, ashr, and, or, xor, fadd, fsub,fmul, fdiv, frem  :: Expression
add = BinaryOperation Add undefined undefined undefined Nothing
sub = BinaryOperation Sub undefined undefined undefined Nothing
mul = BinaryOperation Mul undefined undefined undefined Nothing
sdiv = BinaryOperation SDiv undefined undefined undefined Nothing
srem = BinaryOperation SRem undefined undefined undefined Nothing
udiv = BinaryOperation UDiv undefined undefined undefined Nothing
urem = BinaryOperation URem undefined undefined undefined Nothing
shl = BinaryOperation Shl undefined undefined undefined Nothing
lshr = BinaryOperation LShr undefined undefined undefined Nothing
ashr = BinaryOperation AShr undefined undefined undefined Nothing
and = BinaryOperation And undefined undefined undefined Nothing
or = BinaryOperation Or undefined undefined undefined Nothing
xor = BinaryOperation Xor undefined undefined undefined Nothing
fadd = BinaryOperation FAdd undefined undefined undefined Nothing
fsub = BinaryOperation FSub undefined undefined undefined Nothing
fmul = BinaryOperation FMul undefined undefined undefined Nothing
fdiv = BinaryOperation FDiv undefined undefined undefined Nothing
frem = BinaryOperation FRem undefined undefined undefined Nothing


eq, ne, sge, sle, sgt, slt, uge, ugt, ule, ult, ffalse, ftrue, foeq, fogt, foge, folt, fole, fone, ford, fueq, fugt, fuge, fult, fule, fune, funo :: Expression
eq = CompareOperation EQ undefined undefined undefined 
ne = CompareOperation NE undefined undefined undefined 
sge = CompareOperation SGE undefined undefined undefined 
sle = CompareOperation SLE undefined undefined undefined 
sgt = CompareOperation SGT undefined undefined undefined 
slt = CompareOperation SLT undefined undefined undefined 
uge = CompareOperation UGE undefined undefined undefined 
ugt = CompareOperation UGT undefined undefined undefined 
ule = CompareOperation ULE undefined undefined undefined 
ult = CompareOperation ULT undefined undefined undefined 
ffalse = CompareOperation FFALSE undefined undefined undefined 
ftrue = CompareOperation FTRUE undefined undefined undefined 
foeq = CompareOperation FOEQ undefined undefined undefined 
fogt = CompareOperation FOGT undefined undefined undefined 
foge = CompareOperation FOGE undefined undefined undefined 
folt = CompareOperation FOLT undefined undefined undefined 
fole = CompareOperation FOLE undefined undefined undefined 
fone = CompareOperation FONE undefined undefined undefined 
ford = CompareOperation FORD undefined undefined undefined 
fueq = CompareOperation FUEQ undefined undefined undefined 
fugt = CompareOperation FUGT undefined undefined undefined 
fuge = CompareOperation FUGE undefined undefined undefined 
fult = CompareOperation FULT undefined undefined undefined 
fule = CompareOperation FULE undefined undefined undefined 
fune = CompareOperation FUNE undefined undefined undefined 
funo = CompareOperation FUNO undefined undefined undefined 

trunc, zext, sext, fptrunc, fpext, fptoui, fptosi, uitofp, sitofp, bitcast, refcast, ptrcast :: Expression
trunc = ConvertOperation TRUNC undefined undefined undefined Nothing
zext = ConvertOperation ZEXT undefined undefined undefined Nothing
sext = ConvertOperation SEXT undefined undefined undefined Nothing
fptrunc = ConvertOperation FPTRUNC undefined undefined undefined Nothing
fpext = ConvertOperation FPEXT undefined undefined undefined Nothing
fptoui = ConvertOperation FPTOUI undefined undefined undefined Nothing
fptosi = ConvertOperation FPTOSI undefined undefined undefined Nothing
uitofp = ConvertOperation UITOFP undefined undefined undefined Nothing
sitofp = ConvertOperation SITOFP undefined undefined undefined Nothing
bitcast = ConvertOperation BITCAST undefined undefined undefined Nothing
refcast = ConvertOperation REFCAST undefined undefined undefined Nothing
ptrcast = ConvertOperation PTRCAST undefined undefined undefined Nothing

xchgAtomic, addAtomic, subAtomic, andAtomic, nandAtomic, orAtomic, xorAtomic, maxAtomic, minAtomic, umaxAtomic, uminAtomic :: Expression
xchgAtomic = AtomicRMWOperation False SEQ_CST XCHG undefined undefined undefined Nothing
addAtomic = AtomicRMWOperation False SEQ_CST ADD undefined undefined undefined Nothing
subAtomic = AtomicRMWOperation False SEQ_CST SUB undefined undefined undefined Nothing
andAtomic = AtomicRMWOperation False SEQ_CST AND undefined undefined undefined Nothing
nandAtomic = AtomicRMWOperation False SEQ_CST NAND undefined undefined undefined Nothing
orAtomic = AtomicRMWOperation False SEQ_CST OR undefined undefined undefined Nothing
xorAtomic = AtomicRMWOperation False SEQ_CST XOR undefined undefined undefined Nothing
maxAtomic = AtomicRMWOperation False SEQ_CST MAX undefined undefined undefined Nothing
minAtomic = AtomicRMWOperation False SEQ_CST MIN undefined undefined undefined Nothing
umaxAtomic = AtomicRMWOperation False SEQ_CST UMAX undefined undefined undefined Nothing
uminAtomic = AtomicRMWOperation False SEQ_CST UMIN undefined undefined undefined Nothing

cmpXchg, new, newHybrid, alloca, allocaHybrid :: Expression
cmpXchg = CmpXchg False False NOT_ATOMIC NOT_ATOMIC undefined undefined undefined undefined Nothing
new = New undefined Nothing
newHybrid = NewHybrid undefined undefined undefined Nothing
alloca = Alloca undefined Nothing
allocaHybrid = AllocaHybrid undefined undefined undefined Nothing
returnOp = Return []
throw = Throw undefined
call = Call undefined undefined [] Nothing Nothing
ccall = CCall Mu undefined undefined undefined [] Nothing Nothing
tailCall = TailCall undefined undefined []
branch = Branch1 undefined
branch2 = Branch2 undefined undefined undefined
watchPoint = WatchPoint undefined undefined [] undefined undefined Nothing Nothing
trap = Trap undefined [] Nothing Nothing
wpBranch = WPBranch undefined undefined undefined
switch = Switch undefined undefined undefined []
swapStack = SwapStack undefined KillOld undefined Nothing Nothing
newThread = NewThread undefined undefined Nothing
comminst = Comminst undefined Nothing Nothing Nothing Nothing Nothing Nothing
load = Load False Nothing undefined undefined Nothing
store = Store False Nothing undefined undefined undefined Nothing 
extractValueStr = ExtractValueS undefined undefined undefined Nothing
insertValueStr = InsertValueS undefined undefined undefined undefined Nothing
extractValue = ExtractValue undefined undefined undefined undefined Nothing
insertValue = InsertValue undefined undefined undefined undefined undefined Nothing
shuffle = ShuffleVector undefined undefined undefined undefined undefined Nothing
getIRef = GetIRef undefined undefined Nothing
getFieldIRef = GetFieldIRef False undefined undefined undefined Nothing
getElemIRef = GetElemIRef False undefined undefined undefined undefined Nothing
shiftIRef = ShiftIRef False undefined undefined undefined undefined Nothing
getVarPartIRef = GetVarPartIRef False undefined undefined Nothing
comment = Comment undefined

