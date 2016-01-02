{-#LANGUAGE NoImplicitPrelude#-}

module LibMu.TypeCheck (
  Context,         -- String
  Log,             -- [String]
  Typed(..),
  retType,
  checkExpression, -- :: Expression -> Bool
  checkAssign,     -- :: Assign -> Bool
  checkAst         -- :: Program -> Log
       ) where

import Prelude (
  Bool(..), Eq(..), Int, String, Monad(..),
  otherwise, const, undefined,
  not, map, all, and, length, zipWith,
  (&&), (||), (.), (<), (>), ($), (++), (!!))
import LibMu.PrettyPrint (pp)
import LibMu.MuSyntax
import Control.Monad.Trans.Reader (runReader, Reader, ask, local)
import Control.Applicative ((<*>), (<$>))
import Text.Printf (printf)
import System.IO.Unsafe (unsafePerformIO)

class Typed a where
  --Type Equivalent (infix)
  (#=) :: a -> a -> Bool
  (#=) = te
  --Type Equivalent
  te :: a -> a -> Bool
  te = (#=)
  --Type Similar
  ts :: a -> a -> Bool
  isInt :: a -> Bool
  isInt = const False
  isNum :: a -> Bool
  isNum = const False
  isReal :: a -> Bool
  isReal = const False
  isVec :: a -> Bool
  isVec = const False
  isArray :: a -> Bool
  isArray = const False
  isStruct :: a -> Bool
  isStruct = const False
  isHybrid :: a -> Bool
  isHybrid = const False
  isEqComp :: a -> Bool
  isEqComp = const False
  isUltComp :: a -> Bool
  isUltComp = const False
  isRef :: a -> Bool
  isRef = const False
  isIRef :: a -> Bool
  isIRef = const False
  isUPtr :: a -> Bool
  isUPtr = const False
  isWeakRef :: a -> Bool
  isWeakRef = const False
  isFuncRef :: a -> Bool
  isFuncRef = const False
  isUFuncPtr :: a -> Bool
  isUFuncPtr = const False

instance Typed UvmType where
  isInt t = ts t (MuInt undefined)
  isNum t = case t of
    MuInt _ -> True
    MuFloat -> True
    MuDouble-> True
    _ -> False
  isReal t = case t of
    MuFloat -> True
    MuDouble -> True
    _ -> False
  isVec t = ts t (Vector undefined undefined)
  isStruct t = ts t (Struct undefined)
  isHybrid t = ts t (Hybrid undefined undefined)
  isArray t = ts t (Array undefined undefined)
  isEqComp t = case t of
    MuInt _ -> True
    Ref _ -> True
    IRef _ -> True
    FuncRef _ -> True
    StackRef -> True
    ThreadRef -> True
    FrameCursorRef -> True
    UPtr _ -> True
    UFuncPtr _ -> True
    _ -> False
  isUltComp t = case t of
    MuInt _ -> True
    IRef _ -> True
    _ -> False
  isRef t = ts t (Ref undefined)
  isIRef t = ts t (IRef undefined)
  isUPtr t = ts t (UPtr undefined)
  isWeakRef t = ts t (WeakRef undefined)
  isFuncRef t = ts t (FuncRef undefined)
  isUFuncPtr t = ts t (UFuncPtr undefined)
  te a b = case (a, b) of
    (MuInt l1, MuInt l2) -> l1 == l2
    (MuFloat, MuFloat) -> True
    (MuDouble, MuDouble) -> True
    (Ref t1, Ref t2) -> t1 #= t2
    (IRef t1, IRef t2) -> t1 #= t2
    (WeakRef t1, WeakRef t2) -> t1 #= t2
    (UPtr t1, UPtr t2) -> t1 #= t2
    (Struct _, Struct _) -> True
    (Array t1 l1, Array t2 l2) -> t1 #= t2 && (l1 == l2)
    (Hybrid ts1 t1, Hybrid ts2 t2) ->  (ts1 #= ts2) && (t1 #= t2)
    (Void, Void) -> True
    (ThreadRef, ThreadRef) -> True
    (StackRef, StackRef) -> True
    (FrameCursorRef, FrameCursorRef) -> True
    (TagRef64, TagRef64) -> True
    (Vector t1 l1, Vector t2 l2) -> t1 #= t2 && (l1 == l2)
    (FuncRef s1, FuncRef s2) -> s1 #= s2
    (UFuncPtr s1, UFuncPtr s2) -> s1 #= s2
    _ -> False
  ts a b = case (a, b) of
    (MuInt _, MuInt _) -> True
    (MuFloat, MuFloat) -> True
    (MuDouble, MuDouble) -> True
    (Ref _, Ref _) -> True
    (IRef _, IRef _) -> True
    (WeakRef _, WeakRef _) -> True
    (UPtr _, UPtr _) -> True
    (Struct _, Struct _) -> True
    (Array _ _, Array _ _) -> True
    (Hybrid _ _, Hybrid _ _) -> True
    (Void, Void) -> True
    (ThreadRef, ThreadRef) -> True
    (StackRef, StackRef) -> True
    (FrameCursorRef, FrameCursorRef) -> True
    (TagRef64, TagRef64) -> True
    (Vector _ _, Vector _ _) -> True
    (FuncRef _, FuncRef _) -> True
    (UFuncPtr _, UFuncPtr _) -> True
    _ -> False
    
instance Typed FuncSig where
  te (FuncSig id1 xs ret1) (FuncSig id2 ys ret2)
    | id1 == id2 = True
    | (xs #= ys) && (ret1 #= ret2) = True
    | otherwise = False
  ts (FuncSig id1 xs ret1) (FuncSig id2 ys ret2)
    | id1 == id2 = True
    | (ts xs ys) && (ts ret1 ret2) = True
    | otherwise = False

instance (Typed a) => Typed [a] where
  te l1 l2 = (length l1 == length l2) && (and $ zipWith te l1 l2)
  ts l1 l2 = (length l1 == length l2) && (and $ zipWith ts l1 l2)

instance Typed UvmTypeDef where
  isInt = isInt . uvmTypeDefType
  isNum = isNum . uvmTypeDefType
  isReal = isReal . uvmTypeDefType
  isVec = isVec . uvmTypeDefType
  isArray = isArray . uvmTypeDefType
  isStruct = isStruct . uvmTypeDefType
  isHybrid = isHybrid . uvmTypeDefType
  isEqComp = isEqComp . uvmTypeDefType
  isUltComp = isUltComp . uvmTypeDefType
  isRef = isRef . uvmTypeDefType
  isIRef = isIRef . uvmTypeDefType
  isUPtr = isUPtr . uvmTypeDefType
  isWeakRef = isWeakRef . uvmTypeDefType
  isFuncRef = isFuncRef . uvmTypeDefType
  isUFuncPtr = isUFuncPtr . uvmTypeDefType
  te a b
    | (uvmTypeDefName a) == (uvmTypeDefName b) = True
    | (uvmTypeDefType a) #= (uvmTypeDefType b) = True
    | otherwise = False
  ts a b
    | (uvmTypeDefName a) == (uvmTypeDefName b) = True
    | ts (uvmTypeDefType a) (uvmTypeDefType b) = True
    | otherwise = False

instance Typed SSAVariable where
  isInt = isInt . varType
  isNum = isNum . varType
  isReal = isReal . varType
  isVec = isVec . varType
  isHybrid = isVec . varType
  isStruct = isStruct . varType
  isArray = isArray . varType
  isEqComp = isEqComp . varType
  isUltComp = isUltComp . varType
  isRef = isRef . varType
  isIRef = isIRef . varType
  isWeakRef = isWeakRef . varType
  isFuncRef = isFuncRef . varType
  isUFuncPtr = isUFuncPtr . varType
  te a b = (varType a) #= (varType b)
  ts a b = ts (varType a) (varType b)

allSameType :: UvmTypeDef -> [SSAVariable] -> Bool
allSameType t vs = all (\v -> varType v #= t) vs

checkExpression :: Expression -> Bool
checkExpression e = case e of
  BinaryOperation op t1 v1 v2 _p -> allSameType t1 [v1, v2] && (case op of
    Add -> isInt t1 || (isVec t1 && isInt (vectorType t))
    Sub -> isInt t1 || (isVec t1 && isInt (vectorType t))
    Mul -> isInt t1 || (isVec t1 && isInt (vectorType t))
    SDiv -> isInt t1 || (isVec t1 && isInt (vectorType t))
    SRem -> isInt t1 || (isVec t1 && isInt (vectorType t))
    UDiv -> isInt t1 || (isVec t1 && isInt (vectorType t))
    URem -> isInt t1 || (isVec t1 && isInt (vectorType t))
    Shl -> isInt t1 || (isVec t1 && isInt (vectorType t))
    LShr -> isInt t1 || (isVec t1 && isInt (vectorType t))
    AShr -> isInt t1 || (isVec t1 && isInt (vectorType t))
    And -> isInt t1 || (isVec t1 && isInt (vectorType t))
    Or -> isInt t1 || (isVec t1 && isInt (vectorType t))
    Xor -> isInt t1 || (isVec t1 && isInt (vectorType t))
    FAdd -> isReal t1 || (isVec t1 && isReal (vectorType t))
    FSub -> isReal t1 || (isVec t1 && isReal (vectorType t))
    FMul -> isReal t1 || (isVec t1 && isReal (vectorType t))
    FDiv -> isReal t1 || (isVec t1 && isReal (vectorType t))
    FRem -> isReal t1 || (isVec t1 && isReal (vectorType t))
    )
    where
      t :: UvmType
      t = uvmTypeDefType t1
  CompareOperation op t1 v1 v2 -> allSameType t1 [v1, v2] && (case op of
    EQ -> isEqComp t1 || (isVec t1 && (isEqComp $ vectorType t))
    NE -> isEqComp t1 || (isVec t1 && (isEqComp $ vectorType t))
    SGE -> isInt t1 || (isVec t1 && (isEqComp $ vectorType t))
    SGT -> isInt t1 || (isVec t1 && (isEqComp $ vectorType t))
    SLE -> isInt t1 || (isVec t1 && (isEqComp $ vectorType t))
    SLT -> isInt t1 || (isVec t1 && (isEqComp $ vectorType t))
    UGE -> isUltComp t1 || (isVec t1 && (isUltComp $ vectorType t))
    UGT -> isUltComp t1 || (isVec t1 && (isUltComp $ vectorType t))
    ULE -> isUltComp t1 || (isVec t1 && (isUltComp $ vectorType t))
    ULT -> isUltComp t1 || (isVec t1 && (isUltComp $ vectorType t))
    FFALSE -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FTRUE -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FUNO -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FUEQ -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FUNE -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FUGT -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FUGE -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FULT -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FULE -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FORD -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FOEQ -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FONE -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FOGT -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FOGE -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FOLT -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    FOLE -> isReal t1 || (isVec t1 && (isReal $ vectorType t))
    )
    where
      t :: UvmType
      t = uvmTypeDefType t1
  ConvertOperation op t1 t2 v1 _ -> allSameType t1 [v1] && (case op of
    TRUNC -> (isInt t1) && (isInt t2) && (intLen t1Type > intLen t2Type)
    ZEXT -> (isInt t1) && (isInt t2) && (intLen t1Type < intLen t2Type)
    SEXT -> (isInt t1) && (isInt t2) && (intLen t1Type < intLen t2Type)
    FPTRUNC -> (t1Type #= MuDouble) && (t2Type #= MuFloat)
    FPEXT -> (uvmTypeDefType t1 #= MuFloat) && (uvmTypeDefType t2 #= MuDouble)
    FPTOUI -> (isReal t1) && (isInt t2)
    FPTOSI -> (isReal t1) && (isInt t2)
    UITOFP -> (isInt t1) && (isReal t2)
    SITOFP -> (isInt t1) && (isReal t2)
    BITCAST -> (((isInt t1) && (isReal t2)) || ((isReal t1) && (isInt t2))) && ((bitLen t1Type) == (bitLen t2Type))
    REFCAST -> (ts t1 t2) && (isRef t1 || isIRef t1 || isFuncRef t1)
    PTRCAST -> (isUPtr t1 || isUFuncPtr t1 || isInt t1) && (isUPtr t2 || isUFuncPtr t2 || isInt t2) && (not $ ts t1 t2)
    )
    where
      t1Type = uvmTypeDefType t1
      t2Type = uvmTypeDefType t2
      bitLen :: UvmType -> Int
      bitLen t = case t of
        MuInt l -> l
        MuFloat -> 32
        MuDouble -> 64
        _ -> -1
  AtomicRMWOperation _ _ op t1 v1 v2 _ -> ((v1Type #= IRef t1) || (v1Type #= UPtr t1)) && allSameType t1 [v2] && (
    case op of
      XCHG -> True
      ADD -> isInt t1
      SUB -> isInt t1
      AND -> isInt t1
      NAND -> isInt t1
      OR -> isInt t1
      XOR -> isInt t1
      MAX -> isInt t1
      MIN -> isInt t1
      UMAX -> isInt t1
      UMIN -> isInt t1
    )
    where v1Type = uvmTypeDefType $ varType v1
  CmpXchg _ _ _ _ t1 v1 v2 v3 _ ->
    (isEqComp t1) && ((v1Type #= IRef t1) || (v1Type #= UPtr t1)) && allSameType t1 [v2, v3]
    where v1Type = uvmTypeDefType $ varType v1
  Fence _ -> True
  New t1 _ -> not $ isHybrid t1
  NewHybrid t1 t2 v1 _ -> (isHybrid t1) && (isInt t2) && allSameType t2 [v1]
  Alloca t1 _ -> not $ isHybrid t1
  AllocaHybrid t1 t2 v1 _ -> (isHybrid t1) && (isInt t2) && allSameType t2 [v1]
  Return _ -> True
  Throw v1 -> isRef v1
  Call sig v1 vLst _ _ -> ((uvmTypeDefType $ varType v1) #= (FuncRef sig)) && (map varType vLst #= funcSigArgTypes sig)
  CCall _ t1 sig v1 vLst _ _ -> (map varType vLst #= funcSigArgTypes sig) && allSameType t1 [v1]
  TailCall sig v1 vLst -> ((uvmTypeDefType $ varType v1) #= (FuncRef sig)) && (map varType vLst #= funcSigArgTypes sig)
  Branch1 _ -> True
  Branch2 v1 _ _ -> (uvmTypeDefType $ varType v1) #= (MuInt 1)
  WatchPoint _ _ _ _ _ _ -> True
  Trap _ _ _ -> True
  WPBranch _ _ _ -> True
  Switch t1 v1 _ blocks -> allSameType t1 [v1] && (all (\(var, _) -> (varType var) #= t1) blocks)
  SwapStack v1 _ _ _ _ -> ts (uvmTypeDefType $ varType v1) StackRef
  NewThread v1 _ _ -> ts (uvmTypeDefType $ varType v1) StackRef
  Comminst _ _ _ _ _ _ _ -> True
  Load _ _ t1 v1 _ -> ((v1Type #= IRef t1) || (v1Type #= UPtr t1))
    where v1Type = uvmTypeDefType $ varType v1
  Store _ _ t1 v1 v2 _ -> ((v1Type #= IRef t1) || (v1Type #= UPtr t1)) && allSameType t1 [v2]
    where v1Type = uvmTypeDefType $ varType v1
  ExtractValueS t1 _ v1 _ -> isStruct t1 && (varType v1 #= t1)
  InsertValueS t1 _ v1 _ _ -> isStruct t1 && (varType v1 #= t1)
  ExtractValue t1 t2 v1 v2 _ -> ((isArray t1) || isVec t1) && (isInt t2) && (varType v1 #= t1) && (varType v2 #= t2)
  InsertValue t1 t2 v1 v2 _ _ ->  ((isArray t1) || isVec t1) && (isInt t2) && (varType v1 #= t1) && (varType v2 #= t2)
  ShuffleVector t1 t2 v1 v2 v3 _ -> (isVec t1) && (isVec t2) && (isInt (vectorType $ uvmTypeDefType t2)) && allSameType t1 [v1, v2] && (isVec v3) && allSameType t2 [v3]
  GetIRef t1 v1 _ -> ((uvmTypeDefType $ varType v1) #= Ref t1)
  GetFieldIRef _ t1 _ v1 _ -> (isStruct t1 || isHybrid t1) && ((v1Type #= IRef t1) || (v1Type #= UPtr t1))
    where v1Type = uvmTypeDefType $ varType v1
  GetElemIRef _ t1 t2 v1 v2 _ -> (isArray t1) && (isInt t2) && ((v1Type #= IRef t1) || (v1Type #= UPtr t1)) && (varType v2 #= t2) 
    where v1Type = uvmTypeDefType $ varType v1
  ShiftIRef _ t1 t2 v1 v2 _ -> (isInt t2) && ((v1Type #= IRef t1) || (v1Type #= UPtr t1)) && (varType v2 #= t2) 
    where v1Type = uvmTypeDefType $ varType v1
  GetVarPartIRef _ t1 v1 _ -> (isHybrid t1) && ((v1Type #= IRef t1) || (v1Type #= UPtr t1))
    where v1Type = uvmTypeDefType $ varType v1

retType :: Expression -> [UvmType]
retType expr = case expr of
  BinaryOperation _ t _ _ _ -> [uvmTypeDefType t]
  CompareOperation _ t _ _
    | isVec t -> [Vector (UvmTypeDef "cmp" (MuInt 1)) (vectorLen $ uvmTypeDefType t)]
    | otherwise -> [MuInt 1]
  ConvertOperation _ _ t _ _ -> [uvmTypeDefType t]
  AtomicRMWOperation _ _ _ t _ _ _ -> [uvmTypeDefType t]
  CmpXchg _ _ _ _ t _ _ _ _ -> [uvmTypeDefType t, MuInt 1]
  Fence _ -> []
  New t _ -> [Ref t]
  NewHybrid t _ _ _ -> [Ref t]
  Alloca t _ -> [IRef t]
  AllocaHybrid t _ _ _ -> [IRef t]
  Return _ -> []
  Throw _ -> []
  Call sig _ _ _ _ -> map uvmTypeDefType $ funcSigReturnType sig
  TailCall _ _ _ -> []
  CCall _ _ sig _ _ _ _ -> map uvmTypeDefType $ funcSigReturnType sig
  Branch1 _ -> []
  Branch2 _ _ _ -> []
  WatchPoint _ ts _ _ _ _ -> map uvmTypeDefType ts
  Trap ts _ _ -> map uvmTypeDefType ts
  WPBranch _ _ _ -> []
  Switch _ _ _ _ -> []
  SwapStack _ _ _ _ _ -> []
  NewThread _ _ _ -> [ThreadRef]
  Comminst _ _ _ _ _ _ _ -> [] --Unknown
  Load _ _ t _ _ -> [uvmTypeDefType t]
  Store _ _ _ _ _ _ -> []
  ExtractValueS t index _ _ -> [uvmTypeDefType $ (structTypes $ uvmTypeDefType t) !! index]
  InsertValueS t _ _ _ _ -> [uvmTypeDefType t]
  ExtractValue t _ _ _ _ -> case uvmTypeDefType t of
    Array arrType _ -> [uvmTypeDefType arrType]
    Vector vecType _ -> [uvmTypeDefType vecType]
    _ -> []
  InsertValue t  _ _ _ _ _ -> [uvmTypeDefType t]
  ShuffleVector t1 t2 _ _ _ _ -> case (uvmTypeDefType t1, uvmTypeDefType t2) of
    (Vector t1 _, Vector _ len) -> [Vector t1 len]
    _ -> []
  GetIRef t _ _ -> [IRef t]
  GetFieldIRef ptr t index _ _ -> case (ptr, uvmTypeDefType t) of
    (True, Struct ts) -> [UPtr $ ts !! index]
    (False, Struct ts) -> [IRef $ ts !! index]
    (True, Hybrid ts _) -> [UPtr $ ts !! index]
    (False, Hybrid ts _) -> [IRef $ ts !! index]
  GetElemIRef ptr t _ _ _ _ -> case (ptr, uvmTypeDefType t) of
    (True, Array vecType _) -> [UPtr vecType]
    (False, Array vecType _) -> [IRef vecType]
    _ -> []
  ShiftIRef ptr t _ _ _ _
    | ptr -> [UPtr t]
    | otherwise -> [IRef t]
  GetVarPartIRef ptr t _ _ -> case (ptr, uvmTypeDefType t) of
    (True, Hybrid _ hType) -> [UPtr hType]
    (False, Hybrid _ hType) -> [IRef hType]
    _ -> []
  

checkAssign :: Assign -> Bool
checkAssign ass@(Assign vars expr) = case expr of
  Comminst _ _ _ _ _ _ _ -> True
  _ -> (checkExpression expr) && (map (uvmTypeDefType . varType) vars #= retType expr)
  
type Log = [String]
type Context = String

checkBlockList :: [BasicBlock] -> Reader Context Log
checkBlockList blocks = case blocks of
  [] -> return []        
  x:xs -> (++) <$> ((++) <$> (local (++ (printf "%s -> " (basicBlockName x))) $ checkBody $ basicBlockInstructions x) <*> (checkExpr $ basicBlockTerminst x)) <*> (checkBlockList xs)

checkExpr :: Expression -> Reader Context Log
checkExpr expr
  | checkExpression expr = return []
  | otherwise = (\ctx -> [ctx ++ pp expr]) <$> ask

checkBody :: [Assign] -> Reader Context Log
checkBody block = do
  ctx <- ask
  case block of
    [] -> return []
    x:xs
      | checkAssign x -> checkBody xs
      | otherwise -> ((ctx ++ pp x):) <$> (checkBody xs)

checkAst' :: Program -> Reader Context Log
checkAst' (Program decl) = case decl of
    [] -> return []
    x@(FunctionDef _ _ _ _):xs -> do
      (++) <$> (local (++(printf "Type Error Occurred: %s -> %s -> " (funcDefName x) (funcDefVersion x))) (checkBlockList $ funcDefBody x)) <*> (checkAst' (Program xs))
    _:xs -> checkAst' (Program xs)

checkAst :: Program -> Log
checkAst prog = runReader (checkAst' prog) ""
