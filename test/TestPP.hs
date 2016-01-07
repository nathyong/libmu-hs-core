module TestPP (ppSpec) where

import Test.Hspec
import LibMu.PrettyPrint
import LibMu.MuSyntax

i8, i16, i32, i64, float, double, some_struct, some_array, ref1, ref2, ref3, iref1, iref2,
  iref3 :: UvmTypeDef                                        
i8 = UvmTypeDef "i8" (MuInt 8)
i16 = UvmTypeDef "i16" (MuInt 16)
i32 = UvmTypeDef "i32" (MuInt 32)
i64 = UvmTypeDef "i64" (MuInt 64)
float = UvmTypeDef "float" MuFloat
double = UvmTypeDef "double" MuDouble
some_struct = UvmTypeDef "some_struct" (Struct [i32, i16, i8, double, float])
some_array = UvmTypeDef "some_array" (Array i8 100)
ref1 = UvmTypeDef "ref1" (Ref i32)
ref2 = UvmTypeDef "ref2" (Ref some_struct)
ref3 = UvmTypeDef "ref3" (Ref some_array)
iref1 = UvmTypeDef "iref1" (IRef i32)
iref2 = UvmTypeDef "iref2" (IRef some_struct)
iref3 = UvmTypeDef "iref3" (IRef some_array)

addi32Sig :: FuncSig
addi32Sig = FuncSig "addi32sig" [i32, i32] [i32]

a, b, c, ret, len :: SSAVariable
a = SSAVariable Local "a" i32
b = SSAVariable Local "b" some_struct
c = SSAVariable Local "c" i32
ret = SSAVariable Local "ret" ref2
len = SSAVariable Global "len" i32

exceptionClause :: ExceptionClause
exceptionClause = ExceptionClause (DestinationClause "cont" [a]) (DestinationClause "exec_hldr" [b])

addExpr :: Expression
addExpr = BinaryOperation Add i32 a c Nothing

addBasicBlock :: BasicBlock
addBasicBlock = BasicBlock "entry" [a, c] Nothing [Assign [b] addExpr] (Return [b])

ppSpec :: IO ()
ppSpec = hspec $ do
  describe "pp UvmTypeDef Instance" $ do
    --Test UvmTypeDef instance
    it "correctly displays a type definition" $
      pp (some_struct) `shouldBe` "@some_struct"

  describe "pp FuncSig Instance" $ do
    it "correctly displays a function signature" $
      pp addi32Sig `shouldBe` "@addi32sig"

  describe "pp UvmType Instance" $ do
    it "correctly represents an n bit int value" $
      pp (MuInt 32) `shouldBe` "int<32>"

    it "correctly represents a float value" $
      pp MuFloat `shouldBe` "float"

    it "correctly represents a double value" $
      pp MuDouble `shouldBe` "double"

    it "correctly represents a Ref value of type t" $
      pp (Ref i64) `shouldBe` "ref<@i64>"

    it "correctly represents an IRef value of type t" $
      pp (IRef double) `shouldBe` "iref<@double>"

    it "correctly represents a weakref of a type t" $
      pp (WeakRef float) `shouldBe` "weakref<@float>"

    it "correctly represents a struct value" $
      pp (Struct [i32, float, iref2]) `shouldBe` "struct<@i32 @float @iref2>"

    it "correctly represents an Array of type t and length n" $
      pp (Array some_struct 42) `shouldBe` "array<@some_struct 42>"

    it "correctly represents an Hybrid of types t & ret" $
      pp (Hybrid [i8, iref2, ref1, float] i32) `shouldBe` "hybrid<@i8 @iref2 @ref1 @float @i32>"

    it "correctly represents a void type" $
      pp Void `shouldBe` "void"

    it "correctly represents a ThreadRef type" $
      pp ThreadRef `shouldBe` "threadref"

    it "correctly represents a StackRef type" $
      pp StackRef `shouldBe` "stackref"

    it "correctly represents a FrameCursorRef" $
      pp FrameCursorRef `shouldBe` "framecursorref"

    it "correctly represents a Tagref64" $
      pp TagRef64 `shouldBe` "tagref64"

    it "correctly represents a Vector of type t and length l" $
      pp (Vector i64 42) `shouldBe` "vector<@i64 42>"

    it "correctly represents a FuncRef with a signature s" $
      pp (FuncRef addi32Sig) `shouldBe` "funcref<@addi32sig>"

    it "correctly represents a UFuncRef with a signature s" $
      pp (UFuncPtr addi32Sig) `shouldBe` "ufuncptr<@addi32sig>"

    --Test SSAVariable Instance
    it "correctly represents a local SSAVariable" $
      pp (SSAVariable Local "my_var" iref3) `shouldBe` "%my_var"

    it "correctly representa a global SSAVariable" $
      pp (SSAVariable Global "my_var" i64) `shouldBe` "@my_var"

  describe "pp DestinationClause Instance" $ do
    it "correctly represents a destination clause" $
      pp (DestinationClause "cont" [a, b]) `shouldBe` "%cont(%a %b)" 

  describe "pp Destination Instance" $ do
    it "correctly represents a destination with no exec clause" $
      pp (Destination "cont" [a, b] Nothing) `shouldBe` "cont(<@i32> %a <@some_struct> %b):"

    it "correctly represents a destination with an exec clause" $
      pp (Destination "exec_hldr" [a, b] (Just (SSAVariable Local "exec" i32))) `shouldBe` "%exec_hldr(<@i32> %a <@some_struct> %b) [%exec]:"
  
  describe "pp ExceptionClause Instance" $ do
    it "correctly represents an ExceptionClause" $
      pp exceptionClause `shouldBe` "EXC(%cont(%a) %exec_hldr(%b))"

  describe "pp KeepAliveClause Instance" $ do
    it "correctly represents a keep alive clause" $
      pp (KeepAlive [a, b]) `shouldBe` "KEEPALIVE(%a %b)"

  describe "pp CurStackClause Instance" $ do
    it "correctly represents a CurStackClause (RetWith)" $
      pp (RetWith [i32, float, double]) `shouldBe` "RET_WITH <@i32 @float @double>"

    it "correctly represents a CurStackClause (KillOld)" $
      pp KillOld `shouldBe` "KILL_OLD"

  describe "pp NewStackClause Instance" $ do
    it "correctly represents a NewStackClause (PassValues)" $
      pp (PassValues [i8, iref1, some_array] [a, b, c]) `shouldBe` "PASS_VALUES <@i8 @iref1 @some_array> (%a %b %c)"

    it "correctly represents a NewStackClause (ThrowExc)" $
      pp (ThrowExc c) `shouldBe` "THROW_EXC %c"

  describe "pp Flag Instance" $ do
    it "correctly represents a Flag" $
      pp (Flag "DEFAULT") `shouldBe` "#DEFAULT"
      
  describe  "pp BinaryOp Instance" $ do
    it "correctly represents a binary operator (Add)" $
      pp Add `shouldBe` "ADD"

    it "correctly represents a binary operator (LShr)" $
      pp LShr `shouldBe` "LSHR"
      
  describe "pp CompareOp Instance" $ do
    it "correctly represents a compare operator (SGE)" $
      pp SGE `shouldBe` "SGE"

    it "correctly represents a compare operator (FOLT)" $
      pp FOLT `shouldBe` "FOLT"

  describe "pp ConvertOp Instance" $ do
    it "correctly represents a convert operator (FPTRUNC)" $
      pp FPTRUNC `shouldBe` "FPTRUNC"

    it "correctly represents a convert operator (SEXT)" $
      pp SEXT `shouldBe` "SEXT"

  describe "pp AtomicRMWOp Instance" $ do
    it "correctly represents a atomic-rmw operator (XCHG)" $
      pp XCHG `shouldBe` "XCHG"

    it "correctly represents a atomic-rmw operator (UMAX)" $
      pp UMAX `shouldBe` "UMAX"

  describe "pp MemoryOrder Instance" $ do
    it "correctly represents a memory order (NOT_ATOMIC)" $
      pp NOT_ATOMIC `shouldBe` "NOT_ATOMIC"

    it "correctly represents a memory order (SEQ_CST)" $
      pp SEQ_CST `shouldBe` "SEQ_CST"

  describe "pp Expression Instance" $ do
    --Binary Operations
    it "correctly represents a binaryOperation with no exec clause" $
      pp (BinaryOperation Add i32 a c Nothing) `shouldBe` "ADD <@i32> %a %c"

    it "correctly represents a binaryOperation with an exec clause" $
      pp (BinaryOperation AShr i32 a c (Just exceptionClause)) `shouldBe` "ASHR <@i32> %a %c EXC(%cont(%a) %exec_hldr(%b))"

    --Compare Operations
    it "correctly represents a compareOperation" $
      pp (CompareOperation FFALSE ref1 a ret) `shouldBe` "FFALSE <@ref1> %a %ret"

    --Convert Operations
    it "correctly represents a convertOperation with no exec clause" $
      pp (ConvertOperation FPTRUNC double float b Nothing) `shouldBe` "FPTRUNC <@double @float> %b"

    it "correctly represents a convertOperation with an exec clause" $
      pp (ConvertOperation FPTRUNC double float b (Just exceptionClause)) `shouldBe` "FPTRUNC <@double @float> %b EXC(%cont(%a) %exec_hldr(%b))"

    --AtomicRMW Operation
    it "correctly represents a atomic-rmw operation: no exec, no ptr" $
      pp (AtomicRMWOperation False NOT_ATOMIC XCHG some_struct a b Nothing) `shouldBe` "ATOMICRMW NOT_ATOMIC XCHG <@some_struct> %a %b"

    it "correctly represents a atomic-rmw operation: with exec, no ptr" $
      pp (AtomicRMWOperation False NOT_ATOMIC XCHG some_struct a b (Just exceptionClause)) `shouldBe` "ATOMICRMW NOT_ATOMIC XCHG <@some_struct> %a %b EXC(%cont(%a) %exec_hldr(%b))"

    it "correctly represents a atomic-rmw operation: no exec, with ptr" $
      pp (AtomicRMWOperation True NOT_ATOMIC XCHG some_struct a b Nothing) `shouldBe` "ATOMICRMW PTR NOT_ATOMIC XCHG <@some_struct> %a %b"

    --Cmpxchg operation
    it "correctly represents a cmpxchg operation: no exec, no ptr, no weak" $
      pp (CmpXchg False False NOT_ATOMIC CONSUME ref1 a b c Nothing) `shouldBe` "CMPXCHG NOT_ATOMIC CONSUME <@ref1> %a %b %c" 

    it "correctly represents a cmpxchg operation: no exec, with ptr, no weak" $
      pp (CmpXchg True False NOT_ATOMIC CONSUME ref1 a b c Nothing) `shouldBe` "CMPXCHG PTR NOT_ATOMIC CONSUME <@ref1> %a %b %c"

    it "correctly represents a cmpxchg operation: no exec, no ptr, with weak" $
      pp (CmpXchg False True NOT_ATOMIC CONSUME ref1 a b c Nothing) `shouldBe` "CMPXCHG WEAK NOT_ATOMIC CONSUME <@ref1> %a %b %c"

    it "correctly represents a cmpxchg operation: with exec, no ptr, no weak" $
      pp (CmpXchg False False NOT_ATOMIC CONSUME ref1 a b c (Just exceptionClause)) `shouldBe` "CMPXCHG NOT_ATOMIC CONSUME <@ref1> %a %b %c EXC(%cont(%a) %exec_hldr(%b))" 

    --Fence Operation
    it "correctly represents a fence operation (CONSUME)" $
      pp (Fence CONSUME) `shouldBe` "FENCE CONSUME"

    it "correctly represents a fence operation (NOT_ATOMIC)" $
      pp (Fence NOT_ATOMIC) `shouldBe` "FENCE NOT_ATOMIC"

    --New Operation
    it "correctly represents a new operation with no exec clause" $
      pp (New i8 Nothing) `shouldBe` "NEW <@i8>"

    it "correctly represents a new operation with an exec clause" $
      pp (New some_array (Just exceptionClause)) `shouldBe` "NEW <@some_array> EXC(%cont(%a) %exec_hldr(%b))"

    --NewHybrid operation
    it "correctly represents a NewHybrid operation with no exec clause" $
      pp (NewHybrid ref1 i32 a Nothing) `shouldBe` "NEWHYBRID <@ref1 @i32> %a"

    it "correctly represents a NewHybrid operation with an exec clause" $
      pp (NewHybrid ref1 i32 a (Just exceptionClause)) `shouldBe` "NEWHYBRID <@ref1 @i32> %a EXC(%cont(%a) %exec_hldr(%b))"

    --Alloca Operation
    it "correctly represents an Alloca operation with no exec clause" $
      pp (Alloca some_array Nothing) `shouldBe` "ALLOCA <@some_array>"

    it "correctly represents an Alloca operation with an exec clause" $
      pp (Alloca some_array (Just exceptionClause)) `shouldBe` "ALLOCA <@some_array> EXC(%cont(%a) %exec_hldr(%b))"

    --AllocaHybrid Operation
    it "correctly represents an AllocaHybrid operation with no exec clause" $
      pp (AllocaHybrid i64 ref1 a Nothing) `shouldBe` "ALLOCAHYBRID <@i64 @ref1> %a"

    it "correctly represents an Alloca operation with an exec clause" $
      pp (AllocaHybrid i64 ref1 a (Just exceptionClause)) `shouldBe` "ALLOCAHYBRID <@i64 @ref1> %a EXC(%cont(%a) %exec_hldr(%b))"

    --Return Operation
    it "correctly represents a Return operation" $
      pp (Return [a, b, c]) `shouldBe` "RET (%a %b %c)"

    --Call Operation
    it "correctly represents a Call operation: no exec, no keep alive" $
      pp (Call addi32Sig c [a, b] Nothing Nothing) `shouldBe` "CALL <@addi32sig> %c (%a %b)"

    it "correctly represents a Call operation: with exec, no keep alive" $
      pp (Call addi32Sig c [a, b] (Just exceptionClause) Nothing) `shouldBe` "CALL <@addi32sig> %c (%a %b) EXC(%cont(%a) %exec_hldr(%b))"

    it "correctly represents a Call operation: no exec, with keep alive" $
      pp (Call addi32Sig c [a, b] Nothing (Just (KeepAlive [a, b]))) `shouldBe` "CALL <@addi32sig> %c (%a %b) KEEPALIVE(%a %b)"

    --CCall Operation
    it "correctly represents a CCall operation: no exec, no keep alive" $
      pp (CCall Mu i32 addi32Sig c [a, b] Nothing Nothing) `shouldBe` "CCALL #DEFAULT <@i32 @addi32sig> %c (%a %b)"
    it "correctly represents a CCall operation: with exec, no keep alive" $
      pp (CCall Mu i32 addi32Sig c [a, b] (Just exceptionClause) Nothing) `shouldBe` "CCALL #DEFAULT <@i32 @addi32sig> %c (%a %b) EXC(%cont(%a) %exec_hldr(%b))"
    it "correctly represents a CCall operation: no exec, with keep alive" $
      pp (CCall Mu i32 addi32Sig c [a, b] Nothing (Just (KeepAlive [a, b]))) `shouldBe` "CCALL #DEFAULT <@i32 @addi32sig> %c (%a %b) KEEPALIVE(%a %b)"
    
    --TailCall Operation
    it "correctly represents a TailCall operation" $
      pp (TailCall addi32Sig c [a, b]) `shouldBe` "TAILCALL <@addi32sig> %c (%a %b)"

    --Branch operation
    it "correctly represents a Branch1 operation" $
      pp (Branch1 (DestinationClause "greater" [a])) `shouldBe` "BRANCH %greater(%a)"

    --Branch2 operation
    it "correctly represents a Branch2 operation" $
      pp (Branch2 a (DestinationClause "greater" [a]) (DestinationClause "else" [b, c])) `shouldBe` "BRANCH2 %a %greater(%a) %else(%b %c)"

    --WatchPoint operation
    it "correctly represents a WatchPoint operation: no exec, no alive" $
      pp (WatchPoint a 42 [] (DestinationClause "dis" []) (DestinationClause "ena" []) Nothing Nothing) `shouldBe` "[%a] WATCHPOINT 42 <> %dis() %ena()"

    it "correctly represents a WatchPoint operation: with exec, no alive" $
      pp (WatchPoint a 42 [] (DestinationClause "dis" []) (DestinationClause "ena" []) (Just (WPExceptionClause (DestinationClause "exec" []))) Nothing) `shouldBe` "[%a] WATCHPOINT 42 <> %dis() %ena() WPEXC(%exec())"
    
    it "correctly represents a WatchPoint operation: no exec, with alive" $
      pp (WatchPoint a 42 [] (DestinationClause "dis" []) (DestinationClause "ena" []) Nothing (Just (KeepAlive [a,b]))) `shouldBe` "[%a] WATCHPOINT 42 <> %dis() %ena() KEEPALIVE(%a %b)"

    --Trap Operation
    it "correctly represents a Trap operation: no exec, no alive" $
      pp (Trap a [i32, iref1, some_array] Nothing Nothing) `shouldBe` "[%a] TRAP <@i32 @iref1 @some_array>"

    it "correctly represents a Trap operation: with exec, no alive" $
      pp (Trap a [i32, iref1, some_array] (Just exceptionClause) Nothing) `shouldBe` "[%a] TRAP <@i32 @iref1 @some_array> EXC(%cont(%a) %exec_hldr(%b))"

    it "correctly represents a Trap operation: no exec, with alive" $
      pp (Trap a [i32, iref1, some_array] Nothing (Just (KeepAlive [a,b]))) `shouldBe` "[%a] TRAP <@i32 @iref1 @some_array> KEEPALIVE(%a %b)"

    --WPBranch Operation
    it "correctly represents a WPBranch operation" $
      pp (WPBranch 42 (DestinationClause "cont" []) (DestinationClause "exec" [])) `shouldBe` "WPBRANCH 42 %cont() %exec()"

    --Switch Operation
    it "correctly represents a Switch operation" $
      pp (Switch i32 a (DestinationClause "default" []) [(b, DestinationClause "opt1" []), (c, DestinationClause "opt2" [])]) `shouldBe` "SWITCH <@i32> %a %default() {\n\t%b %opt1()\n\t%c %opt2()\n}"
    
    --SwapStack Operation
    it "correctly represents a SwapStack operation: no exc, no alive" $
      pp (SwapStack a KillOld (ThrowExc ret) Nothing Nothing) `shouldBe` "SWAPSTACK %a KILL_OLD THROW_EXC %ret"

    it "correctly represents a SwapStack operation: with exc, no alive" $
      pp (SwapStack a KillOld (ThrowExc ret) (Just exceptionClause) Nothing) `shouldBe` "SWAPSTACK %a KILL_OLD THROW_EXC %ret EXC(%cont(%a) %exec_hldr(%b))"

    it "correctly represents a SwapStack operation: no exc, with alive" $
      pp (SwapStack a KillOld (ThrowExc ret) Nothing (Just (KeepAlive [a, b]))) `shouldBe` "SWAPSTACK %a KILL_OLD THROW_EXC %ret KEEPALIVE(%a %b)"

    --NewThread Operation
    it "correctly represents a NewThread Operation with no exec clause" $
      pp (NewThread a (ThrowExc ret) Nothing) `shouldBe` "NEWTHREAD %a THROW_EXC %ret"

    it "correctly represents a NewThread Operation with an exec clause" $
      pp (NewThread a (ThrowExc ret) (Just exceptionClause)) `shouldBe` "NEWTHREAD %a THROW_EXC %ret EXC(%cont(%a) %exec_hldr(%b))"

    --Comminst operation
    it "correctly represents a Comminst operation: no flags, no types, no sigs, no args, no exec, no alive" $
      pp (Comminst "@uvm.sayHi" Nothing Nothing Nothing Nothing Nothing Nothing) `shouldBe` "COMMINST @uvm.sayHi"

    it "correctly represents a Comminst operation: with flags, no types, no sigs, no args, no exec, no alive" $
      pp (Comminst "@uvm.sayHi" (Just [Flag "DEFAULT", Flag "STDCALL"]) Nothing Nothing Nothing Nothing Nothing) `shouldBe` "COMMINST @uvm.sayHi [#DEFAULT #STDCALL]"

    it "correctly represents a Comminst operation: no flags, with types, no sigs, no args, no exec, no alive" $
      pp (Comminst "@uvm.sayHi" Nothing (Just [i8, i32, ref3]) Nothing Nothing Nothing Nothing) `shouldBe` "COMMINST @uvm.sayHi <@i8 @i32 @ref3>"

    it "correctly represents a Comminst operation: no flags, no types, with sigs, no args, no exec, no alive" $
      pp (Comminst "@uvm.sayHi" Nothing Nothing (Just [addi32Sig]) Nothing Nothing Nothing) `shouldBe` "COMMINST @uvm.sayHi <[@addi32sig]>"

    it "correctly represents a Comminst operation: no flags, no types, no sigs, with args, no exec, no alive" $
      pp (Comminst "@uvm.sayHi" Nothing Nothing Nothing (Just [a, b, c]) Nothing Nothing) `shouldBe` "COMMINST @uvm.sayHi (%a %b %c)"

    it "correctly represents a Comminst operation: no flags, no types, no sigs, no args, with exec, no alive" $
      pp (Comminst "@uvm.sayHi" Nothing Nothing Nothing Nothing (Just exceptionClause) Nothing) `shouldBe` "COMMINST @uvm.sayHi EXC(%cont(%a) %exec_hldr(%b))"

    it "correctly represents a Comminst operation: no flags, no types, no sigs, no args, no exec, with alive" $
      pp (Comminst "@uvm.sayHi" Nothing Nothing Nothing Nothing Nothing (Just (KeepAlive [a, b]))) `shouldBe` "COMMINST @uvm.sayHi KEEPALIVE(%a %b)"

    --Load operation
    it "correctly represents a Load operation: no ptr, no mem order, no exec" $
      pp (Load False Nothing i16 a Nothing) `shouldBe` "LOAD <@i16> %a"

    it "correctly represents a Load operation: with ptr, no mem order, no exec" $
      pp (Load True Nothing i16 a Nothing) `shouldBe` "LOAD PTR <@i16> %a"

    it "correctly represents a Load operation: no ptr, no mem order, no exec" $
      pp (Load False (Just NOT_ATOMIC) i16 a Nothing) `shouldBe` "LOAD NOT_ATOMIC <@i16> %a"

    it "correctly represents a Load operation: no ptr, no mem order, with exec" $
      pp (Load False Nothing i16 a (Just exceptionClause)) `shouldBe` "LOAD <@i16> %a EXC(%cont(%a) %exec_hldr(%b))"

    --Store Operation
    it "correctly represents a Store operation: no ptr, no mem ord, no exec" $
      pp (Store False Nothing iref2 b a Nothing) `shouldBe` "STORE <@iref2> %b %a"

    it "correctly represents a Store operation: with ptr, no mem ord, no exec" $
      pp (Store True Nothing iref2 b a Nothing) `shouldBe` "STORE PTR <@iref2> %b %a"

    it "correctly represents a Store operation: no ptr, with mem ord, no exec" $
      pp (Store False (Just NOT_ATOMIC) iref2 b a Nothing) `shouldBe` "STORE NOT_ATOMIC <@iref2> %b %a"

    it "correctly represents a Store operation: no ptr, no mem ord, with exec" $
      pp (Store False Nothing iref2 b a (Just exceptionClause)) `shouldBe` "STORE <@iref2> %b %a EXC(%cont(%a) %exec_hldr(%b))"

    --ExtractValue (struct) Operation
    it "correctly represents an ExtractValue operation (Struct). No exec clause" $
      pp (ExtractValueS i16 2 b Nothing) `shouldBe` "EXTRACTVALUE <@i16 2> %b"

    it "correctly represents an ExtractValue operation (Struct). With exec clause" $
      pp (ExtractValueS i16 2 b (Just exceptionClause)) `shouldBe` "EXTRACTVALUE <@i16 2> %b EXC(%cont(%a) %exec_hldr(%b))"

    --InsertValue (Struct) operation
    it "correctly represents an InsertValue operation (Struct). No exec clause" $
      pp (InsertValueS i16 2 a b Nothing) `shouldBe` "INSERTVALUE <@i16 2> %a %b"
      
    it "correctly represents an InsertValue operation (Struct). With exec clause" $
      pp (InsertValueS i16 2 a b (Just exceptionClause)) `shouldBe` "INSERTVALUE <@i16 2> %a %b EXC(%cont(%a) %exec_hldr(%b))"

    --ExtractValue Operation
    it "correctly represents an ExtractValue operation. No exec clause" $
      pp (ExtractValue float i32 b a Nothing) `shouldBe` "EXTRACTVALUE <@float @i32> %b %a"

    it "correctly represents an ExtractValue operation. With exec clause" $
      pp (ExtractValue float i32 b a (Just exceptionClause)) `shouldBe` "EXTRACTVALUE <@float @i32> %b %a EXC(%cont(%a) %exec_hldr(%b))"

    --InsertValue Operation
    it "correctly represents an InsertValue operation. No exec clause" $
      pp (InsertValue float i32 b a c Nothing) `shouldBe` "INSERTVALUE <@float @i32> %b %a %c"
    
    it "correctly represents an InsertValue operation. With exec clause" $
      pp (InsertValue float i32 b a c (Just exceptionClause)) `shouldBe` "INSERTVALUE <@float @i32> %b %a %c EXC(%cont(%a) %exec_hldr(%b))"

    --ShuffleVector Operation
    it "correctly represents a ShuffleVector operation. No exec clause" $
      pp (ShuffleVector double i32 a b c Nothing) `shouldBe` "SHUFFLEVECTOR <@double @i32> %a %b %c"

    it "correctly represents a ShuffleVector operation. With exec clause" $
      pp (ShuffleVector double i32 a b c (Just exceptionClause)) `shouldBe` "SHUFFLEVECTOR <@double @i32> %a %b %c EXC(%cont(%a) %exec_hldr(%b))"

    --GetIRef Operation
    it "correctly represents a GetIRef operation. No exec clause" $
      pp (GetIRef iref1 b Nothing) `shouldBe` "GETIREF <@iref1> %b"

    it "correctly represents a GetIRef operation. With exec clause" $
      pp (GetIRef iref1 b (Just exceptionClause)) `shouldBe` "GETIREF <@iref1> %b EXC(%cont(%a) %exec_hldr(%b))"

    --GetFieldIRef Operation
    it "correctly represents a GetFieldIRef operation: no ptr, no exec" $
      pp (GetFieldIRef False float 42 a Nothing) `shouldBe` "GETFIELDIREF <@float 42> %a"

    it "correctly represents a GetFieldIRef operation: with ptr, no exec" $
      pp (GetFieldIRef True float 42 a Nothing) `shouldBe` "GETFIELDIREF PTR <@float 42> %a"

    it "correctly represents a GetFieldIRef operation: no ptr, with exec" $
      pp (GetFieldIRef False float 42 a (Just exceptionClause)) `shouldBe` "GETFIELDIREF <@float 42> %a EXC(%cont(%a) %exec_hldr(%b))"

    --GetElemIRef
    it "correctly represents a GetElemIRef operation: no ptr, no exec" $
      pp (GetElemIRef False double i8 b a Nothing) `shouldBe` "GETELEMIREF <@double @i8> %b %a"

    it "correctly represents a GetElemIRef operation: with ptr, no exec" $
      pp (GetElemIRef True double i8 b a Nothing) `shouldBe` "GETELEMIREF PTR <@double @i8> %b %a"

    it "correctly represents a GetElemIRef operation:no ptr, no exec" $
      pp (GetElemIRef False double i8 b a (Just exceptionClause)) `shouldBe` "GETELEMIREF <@double @i8> %b %a EXC(%cont(%a) %exec_hldr(%b))"
    
    --ShiftIRef Operation
    it "correctly represents a ShiftIRef operation: no ptr, no exec" $
      pp (ShiftIRef False iref3 i32 b a Nothing) `shouldBe` "SHIFTIREF <@iref3 @i32> %b %a"

    it "correctly represents a ShiftIRef operation: with ptr, no exec" $
      pp (ShiftIRef True iref3 i32 b a Nothing) `shouldBe` "SHIFTIREF PTR <@iref3 @i32> %b %a"

    it "correctly represents a ShiftIRef operation: no ptr, with exec" $
      pp (ShiftIRef False iref3 i32 b a (Just exceptionClause)) `shouldBe` "SHIFTIREF <@iref3 @i32> %b %a EXC(%cont(%a) %exec_hldr(%b))"

    --GetVarPartIRef Operation
    it "correctly represents a GetVarPartIRef operation: no ptr, no exec" $
      pp (GetVarPartIRef False iref2 b Nothing) `shouldBe` "GETVARPARTIREF <@iref2> %b"

    it "correctly represents a GetVarPartIRef operation: with ptr, no exec" $
      pp (GetVarPartIRef True iref2 b Nothing) `shouldBe` "GETVARPARTIREF PTR <@iref2> %b"

    it "correctly represents a GetVarPartIRef operation: no ptr, with exec" $
      pp (GetVarPartIRef False iref2 b (Just exceptionClause)) `shouldBe` "GETVARPARTIREF <@iref2> %b EXC(%cont(%a) %exec_hldr(%b))"

  describe "pp Assign Instance" $ do
    it "correctly represents an Assignment" $
      pp (Assign [b] addExpr) `shouldBe` "%b = ADD <@i32> %a %c"

    it "correctly represents an Assignment with two assignees" $
      pp (Assign [b, a] addExpr) `shouldBe` "(%b %a) = ADD <@i32> %a %c"

  describe "pp BasicBlock Instance" $ do
    it "correctly represents a basic block with no exec clause" $
      pp addBasicBlock `shouldBe` "%entry (<@i32> %a <@i32> %c):\n\t%b = ADD <@i32> %a %c\n\tRET (%b)"

    it "correctly represents a basic block with an exec clause" $
      pp (BasicBlock "entry" [a, c] (Just ret) [Assign [b] addExpr] (Return [b])) `shouldBe` "%entry (<@i32> %a <@i32> %c) [%ret]:\n\t%b = ADD <@i32> %a %c\n\tRET (%b)"
    
  describe "pp Declaration Instance" $ do
    it "correctly represents a ConstDecl" $
      pp (ConstDecl len "42") `shouldBe` ".const @len <@i32> = 42"

    it "correctly represents a typedef" $
      pp (Typedef i32) `shouldBe` ".typedef @i32 = int<32>"

    it "correcctly represents a FunctionSignature" $
      pp (FunctionSignature addi32Sig) `shouldBe` ".funcsig @addi32sig = (@i32 @i32) -> (@i32)"

    it "correctly represents a FunctionDef" $
      pp (FunctionDef "add" "v1" addi32Sig [addBasicBlock]) `shouldBe` ".funcdef @add VERSION %v1 <@addi32sig> {\n\t%entry (<@i32> %a <@i32> %c):\n\t\t%b = ADD <@i32> %a %c\n\t\tRET (%b)\n\t}"

    it "correctly represents a FunctionDecl" $
      pp (FunctionDecl "add" addi32Sig) `shouldBe` ".funcdecl @add = <@addi32sig>"

    it "correctly represents a GlobalDef" $
      pp (GlobalDef len i32) `shouldBe` ".global @len <@i32>"

    it "correctly represents a ExposeDef" $
      pp (ExposeDef "add_native" "add" Mu (SSAVariable Global "i64_0" i64)) `shouldBe` ".expose @add_native = @add <#DEFAULT> @i64_0"
