{-# LANGUAGE NoImplicitPrelude, FlexibleInstances#-}
module LibMu.PrettyPrint ( PrettyPrint(..) ) where

import Prelude (
  Maybe(..), Show(..), String,
  unwords, map, concat, 
  unlines, error, reverse,
  ($), (.), (++))
import Data.Char (toUpper)
import LibMu.MuSyntax
import Text.Printf (printf)
import Control.Applicative ((<$>))
import Control.Monad (Monad(..), mapM)
import Control.Monad.Trans.Reader (runReader, Reader, ask, local)

class PrettyPrint a where 
  ppFormat :: a -> Reader String String
  pp :: a -> String
  pp t = runReader (ppFormat t) ""

instance (PrettyPrint a) => PrettyPrint [a] where
  ppFormat t = do
    ind <- ask
    lst <- (mapM ppFormat t)
    return $ ind ++ (unwords lst)
  
instance PrettyPrint UvmType where
  ppFormat uType = do
    ind <- ask
    return $ ind ++ (
      case uType of
        MuInt len    -> printf "int<%d>" len
        MuFloat      -> "float"
        MuDouble     -> "double"
        Ref t        -> printf "ref<%s>" (pp t)
        IRef t       -> printf "iref<%s>" (pp t)
        WeakRef t    -> printf "weakref<%s>" (pp t)
        UPtr t       -> printf "uptr<%s>" (pp t)
        Struct lst   -> printf "struct<%s>" (unwords $ map pp lst)
        Array t len  -> printf "array<%s %d>" (pp t) len
        Hybrid lst t -> printf "hybrid<%s %s>" (unwords $ map pp lst) (pp t)
        Void         -> "void"
        ThreadRef    -> "threadref"
        StackRef     -> "stackref"
        FrameCursorRef -> "framecursorref"
        TagRef64     -> "tagref64"
        Vector t len -> printf "vector<%s %d>" (pp t) len
        FuncRef sig  -> printf "funcref<%s>" (pp sig)
        UFuncPtr sig -> printf "ufuncptr<%s>" (pp sig))

instance PrettyPrint CallConvention where
  ppFormat conv = do
    ind <- ask
    return $ ind ++ (
      case conv of
        Mu -> "#DEFAULT"
        Foreign s -> '#':s)

instance PrettyPrint SSAVariable where
  ppFormat (SSAVariable scope iD _) = do
    ind <- ask
    case scope of
      Local -> return $ printf "%s%s%s" ind "%" iD
      _ -> return $ printf "%s%s%s" ind "@" iD

instance PrettyPrint UvmTypeDef where
  ppFormat (UvmTypeDef name _) = do
    ind <- ask
    return $ ind ++ ('@':name)

instance PrettyPrint FuncSig where
  ppFormat (FuncSig name _ _) = do
    ind <- ask
    return $ ind ++ ('@':name)

instance PrettyPrint ExceptionClause where
  ppFormat (ExceptionClause nor exc) = do
    ind <- ask
    return $ printf "%sEXC(%s %s)" ind (pp nor) (pp exc)

instance PrettyPrint WPExceptionClause where
  ppFormat (WPExceptionClause dest) = do
    ind <- ask
    return $ printf "%sWPEXC(%s)" ind (pp dest)

instance PrettyPrint KeepAliveClause where
  ppFormat (KeepAlive vars) = do
    ind <- ask
    return $ printf "%sKEEPALIVE(%s)" ind (pp vars)

instance PrettyPrint DestinationClause where
  ppFormat (DestinationClause dest args) = do
    ind <- ask
    return $ printf "%s%s(%s)" ind ('%':dest) (pp args)
      
instance PrettyPrint Flag where
  ppFormat (Flag s) = do
    ind <- ask
    return $ ind ++ ('#':s)

instance PrettyPrint BinaryOp where
  ppFormat op = do
    ind <- ask
    return $ ind ++ (map toUpper $ show op)

instance PrettyPrint ConvertOp where
  ppFormat op = ask >>= (return . (++(show op)))

instance PrettyPrint CompareOp where
  ppFormat op = ask >>= (return . (++(show op)))

instance PrettyPrint AtomicRMWOp where
  ppFormat op = ask >>= (return . (++(show op)))

instance PrettyPrint MemoryOrder where
  ppFormat op = ask >>= (return . (++(show op)))

instance PrettyPrint Expression where
  ppFormat expr = do
    ind <- ask
    return $ ind ++ (
      case expr of
        BinaryOperation op t1 v1 v2 exec -> printf "%s <%s> %s %s%s" (pp op) (pp t1) (pp v1) (pp v2) (printMaybe pp exec)
        CompareOperation op t1 v1 v2 -> printf "%s <%s> %s %s" (pp op) (pp t1) (pp v1) (pp v2)
        ConvertOperation op t1 v1 v2 exec -> printf "%s <%s %s> %s%s" (pp op) (pp t1) (pp v1) (pp v2) (printMaybe pp exec)
        AtomicRMWOperation f1 memOrd op t1 v1 v2 exec ->
          printf "ATOMICRMW%s %s %s <%s> %s %s%s" (if f1 then " PTR" else "") (pp memOrd) (pp op) (pp t1) (pp v1) (pp v2) (printMaybe pp exec)
        CmpXchg f1 f2 memOrd1 memOrd2 t1 v1 v2 v3 exec ->
          printf "CMPXCHG%s%s %s %s <%s> %s %s %s%s" (if f1 then " PTR" else "") (if f2 then " WEAK" else "") (pp memOrd1) (pp memOrd2) (pp t1) (pp v1) (pp v2) (pp v3) (printMaybe pp exec)
        Fence ord -> printf "FENCE %s" (pp ord)
        New t1 exec -> printf "NEW <%s>%s" (pp t1) (printMaybe pp exec)
        NewHybrid t1 t2 v1 exec -> printf "NEWHYBRID <%s %s> %s%s" (pp t1) (pp t2) (pp v1) (printMaybe pp exec)
        Alloca t1 exec -> printf "ALLOCA <%s>%s" (pp t1) (printMaybe pp exec)
        AllocaHybrid t1 t2 v1 exec -> printf "ALLOCAHYBRID <%s %s> %s%s" (pp t1) (pp t2) (pp v1) (printMaybe pp exec)
        Return lst -> printf "RET %s" (printArgList lst)
        Throw v1 -> printf "THROW %s" (pp v1)
        Call s1 v1 lst exec alive -> printf "CALL <%s> %s %s%s%s" (pp s1) (pp v1) (printArgList lst) (printMaybe pp exec) (printMaybe pp alive)
        CCall conv t1 s1 v1 argLst exec alive -> printf "CCALL %s <%s %s> %s %s%s%s" (pp conv) (pp t1) (pp s1) (pp v1) (printArgList argLst) (printMaybe pp exec) (printMaybe pp alive)
        TailCall s1 v1 lst -> printf "TAILCALL <%s> %s %s" (pp s1) (pp v1) (printArgList lst)
        Branch1 dest -> printf "BRANCH %s" (pp dest)
        Branch2 cond d1 d2 -> printf "BRANCH2 %s %s %s" (pp cond) (pp d1) (pp d2)
        WatchPoint name iD lst dis ena exec alive ->
          printf "[%s] WATCHPOINT %d %s %s %s%s%s" (pp name) iD (printTypeList lst) (pp dis) (pp ena) (printMaybe pp exec) (printMaybe pp alive)
        Trap name lst exec alive -> printf "[%s] TRAP %s%s%s" (pp name) (printTypeList lst) (printMaybe pp exec) (printMaybe pp alive)
        WPBranch iD dis ena -> printf "WPBRANCH %d %s %s" iD (pp dis) (pp ena)
        Switch t1 v1 dest blocks -> printf "SWITCH <%s> %s %s {\n%s}" (pp t1) (pp v1) (pp dest) (concat $ runReader (mapM printBlock blocks) (ind ++ "\t"))
        SwapStack v1 csClause nsClause exec alive ->
          printf "SWAPSTACK %s %s %s%s%s" (pp v1) (pp csClause) (pp nsClause) (printMaybe pp exec) (printMaybe pp alive)
        NewThread v1 nsClause exec -> printf "NEWTHREAD %s %s%s" (pp v1) (pp nsClause) (printMaybe pp exec)
        Comminst inst fLst tLst sLst vLst exec alive ->
          printf "COMMINST @%s%s%s%s%s%s%s" inst (printMaybe printFlagList fLst) (printMaybe printTypeList tLst) (printMaybe printSigList sLst) (printMaybe printArgList vLst) (printMaybe pp exec) (printMaybe pp alive)
        Load f1 memOrd t1 v1 exec -> printf "LOAD%s%s <%s> %s%s" (if f1 then " PTR" else "") (printMaybe pp memOrd) (pp t1) (pp v1) (printMaybe pp exec)
        Store f1 memOrd t1 v1 v2 exec -> printf "STORE%s%s <%s> %s %s%s" (if f1 then " PTR" else "") (printMaybe pp memOrd) (pp t1) (pp v1) (pp v2) (printMaybe pp exec)
        ExtractValueS t1 index v1 exec -> printf "EXTRACTVALUE <%s %d> %s%s" (pp t1) index (pp v1) (printMaybe pp exec)
        InsertValueS t1 index v1 v2 exec -> printf "INSERTVALUE <%s %d> %s %s%s" (pp t1) index (pp v1) (pp v2) (printMaybe pp exec)
        ExtractValue t1 t2 v1 v2 exec -> printf "EXTRACTVALUE <%s %s> %s %s%s" (pp t1) (pp t2) (pp v1) (pp v2) (printMaybe pp exec)
        InsertValue t1 t2 v1 v2 v3 exec -> printf "INSERTVALUE <%s %s> %s %s %s%s" (pp t1) (pp t2) (pp v1) (pp v2) (pp v3) (printMaybe pp exec)
        ShuffleVector t1 t2 v1 v2 v3 exec -> printf "SHUFFLEVECTOR <%s %s> %s %s %s%s" (pp t1) (pp t2) (pp v1) (pp v2) (pp v3) (printMaybe pp exec)
        GetIRef t1 v1 exec -> printf "GETIREF <%s> %s%s" (pp t1) (pp v1) (printMaybe pp exec)
        GetFieldIRef f1 t1 index v1 exec -> printf "GETFIELDIREF%s <%s %d> %s%s" (if f1 then " PTR" else "") (pp t1) index (pp v1) (printMaybe pp exec)
        GetElemIRef f1 t1 t2 v1 v2 exec -> printf "GETELEMIREF%s <%s %s> %s %s%s" (if f1 then " PTR" else "") (pp t1) (pp t2) (pp v1) (pp v2) (printMaybe pp exec)
        ShiftIRef f1 t1 t2 v1 v2 exec -> printf "SHIFTIREF%s <%s %s> %s %s%s" (if f1 then " PTR" else "") (pp t1) (pp t2) (pp v1) (pp v2) (printMaybe pp exec)
        GetVarPartIRef f1 t1 v1 exec -> printf "GETVARPARTIREF%s <%s> %s%s" (if f1 then " PTR" else "") (pp t1) (pp v1) (printMaybe pp exec)
        Comment str -> printf "//%s" str
        )
    where
      printTypeList :: [UvmTypeDef] -> String
      printTypeList lst = "<" ++ pp lst++ ">"
      printSigList :: [FuncSig] -> String
      printSigList lst = "<[" ++ (pp lst) ++ "]>"
      printArgList :: [SSAVariable] -> String
      printArgList lst = "(" ++ (pp lst) ++ ")"
      printFlagList :: [Flag] -> String
      printFlagList lst = "[" ++ (unwords $ map pp lst) ++ "]"
      printMaybe :: (a -> String) -> Maybe a -> String
      printMaybe f mVal = case mVal of
        Nothing -> ""
        Just val -> ' ':f val
      printBlock :: (SSAVariable, DestinationClause) -> Reader String String
      printBlock (arg, destClause) = do
        ind <- ask
        return $ printf "%s%s %s\n" ind (pp arg) (pp destClause)

instance PrettyPrint CurStackClause where
  ppFormat clause = do
    ind <- ask
    return $ ind ++ (
      case clause of
        RetWith lst -> printf "RET_WITH <%s>" (pp lst)
        KillOld -> "KILL_OLD"
      )
      
instance PrettyPrint NewStackClause where
  ppFormat clause = do
    ind <- ask
    return $ ind ++ (
      case clause of
        PassValues tLst vLst -> printf "PASS_VALUES <%s> (%s)" (pp tLst) (pp vLst)
        ThrowExc exc -> printf "THROW_EXC %s" (pp exc)
      )
      
instance PrettyPrint Assign where
  ppFormat (Assign vars expr) = do
    ind <- ask
    case vars of
      [] -> return $ ind ++ pp expr
      [var] -> return $ printf "%s%s = %s" ind (pp var) (pp expr)
      _ -> return $ printf "%s%s = %s" ind (printArgList vars) (pp expr)
        where
          printArgList :: [SSAVariable] -> String
          printArgList lst = "(" ++ (pp lst) ++ ")"


instance PrettyPrint Declaration where
  ppFormat decl = do
    ind <- ask
    case decl of
      ConstDecl var@(SSAVariable _ _ dType) val -> return $ printf "%s.const %s <%s> = %s" ind (pp var) (pp dType) val
      Typedef var@(UvmTypeDef _ uType)  -> return $ printf "%s.typedef %s = %s" ind (pp var) (pp uType)
      FunctionSignature var@(FuncSig _ tLst ret) -> return $ printf "%s.funcsig %s = %s -> %s" ind (pp var) (printSig tLst) (printSig ret)
        where
          printSig :: [UvmTypeDef] -> String
          printSig lst = "(" ++ (pp lst) ++ ")"
      FunctionDef name ver sig body -> do
        pBody <- local (++"\t") (mapM ppFormat $ reverse body)
        return $ printf "%s.funcdef @%s VERSION %s <%s> {\n%s\t}" ind name ('%':ver) (pp sig) (unlines pBody)
      FunctionDecl name sig -> return $  printf "%s.funcdecl @%s = <%s>" ind name (pp sig)
      GlobalDef var uType -> return $ printf "%s.global %s <%s>" ind (pp var) (pp uType)
      ExposeDef name fName cconv cookie -> return $ printf "%s.expose @%s = @%s <%s> %s" ind name fName (pp cconv) (pp cookie)

instance PrettyPrint BasicBlock where
  ppFormat (BasicBlock name params exec instructions term) = do
    _ <- error "This happened"
    ind <- ask
    blocks <- local (++"\t") (printBlocks $ reverse instructions)
    termInst <- local (++"\t") (ppFormat term)
    return $ printf "%s%s (%s)%s:\n%s%s" ind ('%':name) (printParams params) (printExec exec) (blocks) (termInst)
    where
      printParams :: [SSAVariable] -> String
      printParams lst = unwords $ map (\p -> printf "<%s> %s" (pp $ varType p) (pp p)) lst
      printBlocks :: [Assign] -> Reader String String
      printBlocks lst = unlines <$> (mapM ppFormat lst)
      printExec :: Maybe SSAVariable -> String
      printExec e = case e of
        Nothing -> ""
        Just exc -> printf " [%s]" (pp exc)

instance PrettyPrint Program where
  ppFormat (Program prog) = unlines <$> mapM ppFormat prog
