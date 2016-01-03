{-#LANGUAGE NoImplicitPrelude#-}

import Prelude (
  IO, Maybe(..), Bool(..),
  undefined,
  ($))
import Test.Hspec

import LibMu.PrettyPrint
import LibMu.MuSyntax
import LibMu.TypeCheck -- (checkExpression, checkAssign, checkAst)

i1, i16, i32, i64, float, double, void, irefi32, refi32, reffloat, ireffloat, uptri32, uptrfloat, refvoid, irefvoid, weakrefvoid, sref, uptrvoid, i32x4, floatx4, doublex2, doublex4, funcrefadd, ufuncptradd, hybrid_i32_float, structi32float, iref_structi32float, uptr_structi32float, arrayi32x4, ref_hybrid_i32_float, iref_hybrid_i32_float, uptr_hybrid_i32_float, iref_arrayi32x4, uptr_arrayi32x4 :: UvmTypeDef
i1 = UvmTypeDef "i1" (MuInt 1)
i16 = UvmTypeDef "i16" (MuInt 16)
i32 = UvmTypeDef "i32" (MuInt 32)
i64 = UvmTypeDef "i64" (MuInt 64)
float = UvmTypeDef "float" MuFloat
double = UvmTypeDef "double" MuDouble
void = UvmTypeDef "void" Void
irefi32 = UvmTypeDef "irefi32" (IRef i32)
refi32 = UvmTypeDef "irefi32" (Ref i32)
reffloat = UvmTypeDef "reffloat" (Ref float)
ireffloat = UvmTypeDef "ireffloat" (IRef float)
uptri32 = UvmTypeDef "uptri32" (UPtr i32)
uptrfloat = UvmTypeDef "uptrfloat" (UPtr float)
refvoid = UvmTypeDef "refvoid" (Ref void)
irefvoid = UvmTypeDef "irefvoid" (IRef void)
weakrefvoid = UvmTypeDef "weakrefvoid" (WeakRef void)
sref = UvmTypeDef "sref" StackRef
uptrvoid = UvmTypeDef "uptrvoid" (UPtr void)
i32x4 = UvmTypeDef "i32x4" (Vector i32 4)
floatx4 = UvmTypeDef "floatx4" (Vector float 4)
doublex2 = UvmTypeDef "doublex2" (Vector double 2)
doublex4 = UvmTypeDef "doublex4" (Vector double 4)
funcrefadd = UvmTypeDef "funcrefadd" (FuncRef addi32sig)
ufuncptradd = UvmTypeDef "funcrefadd" (UFuncPtr addi32sig)
hybrid_i32_float = UvmTypeDef "hybrid_i32_float" (Hybrid [i32] float)
structi32float = UvmTypeDef "structi32float" (Struct [i32, float])
arrayi32x4 = UvmTypeDef "arrayi32x4" (Array i32 4)
iref_structi32float = UvmTypeDef "iref_structi32float" (IRef structi32float)
uptr_structi32float = UvmTypeDef "uptr_structi32float" (UPtr structi32float)
ref_hybrid_i32_float = UvmTypeDef "ref_structi32float" (Ref hybrid_i32_float)
iref_hybrid_i32_float = UvmTypeDef "iref_hybrid_i32_float" (IRef hybrid_i32_float)
uptr_hybrid_i32_float = UvmTypeDef "uptr_hybrid_i32_float" (UPtr hybrid_i32_float)
iref_arrayi32x4 = UvmTypeDef "iref_arrayi32x4" (IRef arrayi32x4)
uptr_arrayi32x4 = UvmTypeDef "uptr_arrayi32x4" (UPtr arrayi32x4)

addi32sig, addi64sig :: FuncSig
addi32sig = FuncSig "addi32sig" [i32, i32] [i32]
addi64sig = FuncSig "addi64sig" [i64, i64] [i64]

addSig :: FuncSig
addSig = FuncSig "add" [i32, i32] [i32]

addProg :: Program
addProg = Program [GlobalDef a_i32 i32, FunctionDef "add" "v1" addSig [BasicBlock "entry" [a_i32, a_i32] Nothing [Assign [ret] (BinaryOperation Add i32 a_i64 a_i32 Nothing)] (Return [ret])]]

a_void, b_void, a_refvoid, b_refvoid, a_reffloat, a_ireffloat, a_irefvoid, b_irefvoid, a_weakrefvoid, b_weakrefvoid, a_funcrefadd, a_uptrvoid, a_ufuncptradd, a_irefi32, a_refi32, a_uptri32, a_sref :: SSAVariable
a_void = SSAVariable Global "a_void" void
b_void = SSAVariable Global "b_void" void
a_refvoid = SSAVariable Global "a_refvoid" refvoid
b_refvoid = SSAVariable Global "b_refvoid" refvoid
a_reffloat = SSAVariable Global "a_reffloat" reffloat
a_ireffloat = SSAVariable Global "a_ireffloat" ireffloat
a_irefvoid = SSAVariable Global "a_irefvoid" irefvoid
b_irefvoid = SSAVariable Global "b_irefvoid" irefvoid
a_weakrefvoid = SSAVariable Global "b_void" weakrefvoid
b_weakrefvoid = SSAVariable Global "b_void" weakrefvoid
a_funcrefadd = SSAVariable Global "a_funcrefadd" funcrefadd
a_uptrvoid = SSAVariable Global "a_uptrvoid" uptrvoid
a_uptrfloat = SSAVariable Global "a_uptrfloat" uptrfloat
a_ufuncptradd = SSAVariable Global "a_ufuncptradd" ufuncptradd
a_irefi32 = SSAVariable Global "a_irefi32" irefi32
a_refi32 = SSAVariable Global "a_refi32" refi32
a_uptri32 = SSAVariable Global "a_uptri32" uptri32
a_sref = SSAVariable Global "a_sref" sref

a_i1, a_i16, a_i32, b_i32, a_i64, b_i64, ret :: SSAVariable
a_i1 = SSAVariable Global "a_i1" i1
a_i16 = SSAVariable Global "a_i16" i16
a_i32 = SSAVariable Global "a_i32" i32
b_i32 = SSAVariable Global "b_i32" i32
a_i64 = SSAVariable Global "a_i64" i64
b_i64 = SSAVariable Global "b_i64" i64
ret   = SSAVariable Local "ret" i32

a_float, b_float, a_double, b_double :: SSAVariable
a_float = SSAVariable Global "a_float" float
b_float = SSAVariable Global "b_float" float
a_double = SSAVariable Global "a_double" double
b_double = SSAVariable Global "b_double" double

a_i32x4, b_i32x4, a_floatx4, b_floatx4, a_doublex2, b_doublex2, a_structi32float, a_arrayi32x4, a_uptr_structi32float, a_iref_structi32float, a_iref_hybrid_i32_float, a_uptr_hybrid_i32_float, a_iref_arrayi32x4, a_uptr_arrayi32x4, a_ref_hybrid_i32_float :: SSAVariable
a_i32x4 = SSAVariable Global "a_i32x4" i32x4
b_i32x4 = SSAVariable Global "b_i32x4" i32x4
a_floatx4 = SSAVariable Global "a_floatx4" floatx4
b_floatx4 = SSAVariable Global "b_floatx4" floatx4
a_doublex2 = SSAVariable Global "a_doublex2" doublex2
b_doublex2 = SSAVariable Global "b_doublex2" doublex2
a_doublex4 = SSAVariable Global "a_doublex4" doublex4
a_structi32float = SSAVariable Global "a_structi32float" structi32float
a_arrayi32x4 = SSAVariable Global "a_arrayi32x4" arrayi32x4
a_iref_structi32float = SSAVariable Global "a_iref_structi32float" iref_structi32float
a_uptr_structi32float = SSAVariable Global "a_uptr_structi32float" uptr_structi32float
a_iref_hybrid_i32_float = SSAVariable Global "a_iref_hybrid_i32_float" iref_hybrid_i32_float
a_uptr_hybrid_i32_float = SSAVariable Global "a_uptr_hybrid_i32_float" uptr_hybrid_i32_float
a_uptr_arrayi32x4 = SSAVariable Global "a_uptr_arrayi32x4" uptr_arrayi32x4
a_iref_arrayi32x4 = SSAVariable Global "a_iref_arrayi32x4" iref_arrayi32x4
a_ref_hybrid_i32_float = SSAVariable Global "a_ref_hybrid_i32_float" ref_hybrid_i32_float

main :: IO ()
main = hspec $ do

  describe "checkExpression : BinaryOperation : Add" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation Add i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation Add i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation Add float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation Add doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation Add i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : Sub" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation Sub i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation Sub i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation Sub float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation Sub doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation Sub i32 a_i32 b_i64 Nothing) `shouldBe` False     

  describe "checkExpression : BinaryOperation : Mul" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation Mul i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation Mul i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation Mul float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation Mul doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation Mul i32 a_i32 b_i64 Nothing) `shouldBe` False
      
  describe "checkExpression : BinaryOperation : SDiv" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation SDiv i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation SDiv i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation SDiv float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation SDiv doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation SDiv i32 a_i32 b_i64 Nothing) `shouldBe` False  

  describe "checkExpression : BinaryOperation : SRem" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation SRem i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation SRem i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation SRem float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation SRem doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation SRem i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : UDiv" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation UDiv i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation UDiv i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation UDiv float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation UDiv doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation UDiv i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : URem" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation URem i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation URem i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation URem float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation URem doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation URem i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : Shl" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation Shl i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation Shl i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation Shl float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation Shl doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation Shl i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : LShr" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation LShr i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation LShr i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation LShr float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation LShr doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation LShr i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : AShr" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation AShr i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation AShr i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation AShr float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation AShr doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation AShr i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : And" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation And i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation And i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation And float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation And doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation And i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : Or" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation Or i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation Or i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation Or float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation Or doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation Or i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : Xor" $ do
    it "correctly returns True for i32 arguments" $
      checkExpression (BinaryOperation Xor i32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for vector<i32> arguments" $
      checkExpression (BinaryOperation Xor i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (BinaryOperation Xor float a_float b_float Nothing) `shouldBe` False
    it "correctly returns False for vector<double> arguments" $
      checkExpression (BinaryOperation Xor doublex2 a_doublex2 b_doublex2 Nothing) `shouldBe` False
    it "correctly returns False for int arguments of different sizes" $
      checkExpression (BinaryOperation Xor i32 a_i32 b_i64 Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : FAdd" $ do
    it "correctly returns True for double arguments" $
      checkExpression (BinaryOperation FAdd double a_double b_double Nothing) `shouldBe` True
    it "correctly returns True for vector<float> arguments" $
      checkExpression (BinaryOperation FAdd floatx4 a_floatx4 b_floatx4 Nothing) `shouldBe` True 
    it "correctly returns False for ref<void> arguments" $
      checkExpression (BinaryOperation FAdd refvoid a_refvoid b_refvoid Nothing) `shouldBe` False
    it "correctly returns False for vector<i32> arguments" $
      checkExpression (BinaryOperation FAdd i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (BinaryOperation FAdd float a_float b_double Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : FSub" $ do
    it "correctly returns True for double arguments" $
      checkExpression (BinaryOperation FSub double a_double b_double Nothing) `shouldBe` True
    it "correctly returns True for vector<float> arguments" $
      checkExpression (BinaryOperation FSub floatx4 a_floatx4 b_floatx4 Nothing) `shouldBe` True 
    it "correctly returns False for ref<void> arguments" $
      checkExpression (BinaryOperation FSub refvoid a_refvoid b_refvoid Nothing) `shouldBe` False
    it "correctly returns False for vector<i32> arguments" $
      checkExpression (BinaryOperation FSub i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (BinaryOperation FSub float a_float b_double Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : FMul" $ do
    it "correctly returns True for double arguments" $
      checkExpression (BinaryOperation FMul double a_double b_double Nothing) `shouldBe` True
    it "correctly returns True for vector<float> arguments" $
      checkExpression (BinaryOperation FMul floatx4 a_floatx4 b_floatx4 Nothing) `shouldBe` True 
    it "correctly returns False for ref<void> arguments" $
      checkExpression (BinaryOperation FMul refvoid a_refvoid b_refvoid Nothing) `shouldBe` False
    it "correctly returns False for vector<i32> arguments" $
      checkExpression (BinaryOperation FMul i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (BinaryOperation FMul float a_float b_double Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : FDiv" $ do
    it "correctly returns True for double arguments" $
      checkExpression (BinaryOperation FDiv double a_double b_double Nothing) `shouldBe` True
    it "correctly returns True for vector<float> arguments" $
      checkExpression (BinaryOperation FDiv floatx4 a_floatx4 b_floatx4 Nothing) `shouldBe` True 
    it "correctly returns False for ref<void> arguments" $
      checkExpression (BinaryOperation FDiv refvoid a_refvoid b_refvoid Nothing) `shouldBe` False
    it "correctly returns False for vector<i32> arguments" $
      checkExpression (BinaryOperation FDiv i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (BinaryOperation FDiv float a_float b_double Nothing) `shouldBe` False

  describe "checkExpression : BinaryOperation : FRem" $ do
    it "correctly returns True for double arguments" $
      checkExpression (BinaryOperation FRem double a_double b_double Nothing) `shouldBe` True
    it "correctly returns True for vector<float> arguments" $
      checkExpression (BinaryOperation FRem floatx4 a_floatx4 b_floatx4 Nothing) `shouldBe` True 
    it "correctly returns False for ref<void> arguments" $
      checkExpression (BinaryOperation FRem refvoid a_refvoid b_refvoid Nothing) `shouldBe` False
    it "correctly returns False for vector<i32> arguments" $
      checkExpression (BinaryOperation FRem i32x4 a_i32x4 b_i32x4 Nothing) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (BinaryOperation FRem float a_float b_double Nothing) `shouldBe` False

  describe "checkExpression : CompareOperation : EQ" $ do
    it "correctly returns True for ref<void> arguments" $
      checkExpression (CompareOperation EQ refvoid a_refvoid b_refvoid) `shouldBe` True
    it "correctly returns True for int arguments" $
      checkExpression (CompareOperation EQ i32 a_i32 b_i32) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation EQ weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (CompareOperation EQ refvoid a_refvoid b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation EQ i32x4 a_i32x4 a_i32x4) `shouldBe` True

  describe "checkExpression : CompareOperation : NE" $ do
    it "correctly returns True for ref<void> arguments" $
      checkExpression (CompareOperation NE refvoid a_refvoid b_refvoid) `shouldBe` True
    it "correctly returns True for int arguments" $
      checkExpression (CompareOperation NE i32 a_i32 b_i32) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation NE weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (CompareOperation NE refvoid a_refvoid b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation NE i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : SGE" $ do
    it "correctly returns True for i64 arguments" $
      checkExpression (CompareOperation SGE i64 a_i64 b_i64) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (CompareOperation SGE float a_float b_float) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (CompareOperation SGE i64 a_i64 b_i32) `shouldBe` False
    it "correctly returns False for arguments not of specified type" $
      checkExpression (CompareOperation SGE i64 a_i32 b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation SGE i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : SGT" $ do
    it "correctly returns True for i64 arguments" $
      checkExpression (CompareOperation SGT i64 a_i64 b_i64) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (CompareOperation SGT float a_float b_float) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (CompareOperation SGT i64 a_i64 b_i32) `shouldBe` False
    it "correctly returns False for arguments not of specified type" $
      checkExpression (CompareOperation SGT i64 a_i32 b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation SGT i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : SLE" $ do
    it "correctly returns True for i64 arguments" $
      checkExpression (CompareOperation SLE i64 a_i64 b_i64) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (CompareOperation SLE float a_float b_float) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (CompareOperation SLE i64 a_i64 b_i32) `shouldBe` False
    it "correctly returns False for arguments not of specified type" $
      checkExpression (CompareOperation SLE i64 a_i32 b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation SLE i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : SLT" $ do
    it "correctly returns True for i64 arguments" $
      checkExpression (CompareOperation SLT i64 a_i64 b_i64) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (CompareOperation SLT float a_float b_float) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (CompareOperation SLT i64 a_i64 b_i32) `shouldBe` False
    it "correctly returns False for arguments not of specified type" $
      checkExpression (CompareOperation SLT i64 a_i32 b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation SLT i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : SGE" $ do
    it "correctly returns True for i64 arguments" $
      checkExpression (CompareOperation SGE i64 a_i64 b_i64) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (CompareOperation SGE float a_float b_float) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (CompareOperation SGE i64 a_i64 b_i32) `shouldBe` False
    it "correctly returns False for arguments not of specified type" $
      checkExpression (CompareOperation SGE i64 a_i32 b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation SGE i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : UGE" $ do
    it "correctly returns True for iref<void> arguments" $
      checkExpression (CompareOperation UGE irefvoid a_irefvoid b_irefvoid) `shouldBe` True
    it "correctly return True for int arguments" $
      checkExpression (CompareOperation UGE i32 a_i32 b_i32) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (CompareOperation UGE float a_float b_float) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation UGE i32 a_irefvoid b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation UGE i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : UGT" $ do
    it "correctly returns True for iref<void> arguments" $
      checkExpression (CompareOperation UGT irefvoid a_irefvoid b_irefvoid) `shouldBe` True
    it "correctly return True for int arguments" $
      checkExpression (CompareOperation UGT i32 a_i32 b_i32) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (CompareOperation UGT float a_float b_float) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation UGT i32 a_irefvoid b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation UGT i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : ULE" $ do
    it "correctly returns True for iref<void> arguments" $
      checkExpression (CompareOperation ULE irefvoid a_irefvoid b_irefvoid) `shouldBe` True
    it "correctly return True for int arguments" $
      checkExpression (CompareOperation ULE i32 a_i32 b_i32) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (CompareOperation ULE float a_float b_float) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation ULE i32 a_irefvoid b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation ULE i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : ULT" $ do
    it "correctly returns True for iref<void> arguments" $
      checkExpression (CompareOperation ULT irefvoid a_irefvoid b_irefvoid) `shouldBe` True
    it "correctly return True for int arguments" $
      checkExpression (CompareOperation ULT i32 a_i32 b_i32) `shouldBe` True
    it "correctly returns False for float arguments" $
      checkExpression (CompareOperation ULT float a_float b_float) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation ULT i32 a_irefvoid b_i32) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation ULT i32x4 a_i32x4 a_i32x4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FFALSE" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FFALSE float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FFALSE double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FFALSE weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FFALSE float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<i32 4> arguments" $
      checkExpression (CompareOperation FFALSE floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FTRUE" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FTRUE float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FTRUE double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FTRUE weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FTRUE float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FTRUE floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FUNO" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FUNO float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FUNO double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FUNO weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FUNO float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FUNO floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FUEQ" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FUEQ float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FUEQ double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FUEQ weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FUEQ float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FUEQ floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FUNE" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FUNE float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FUNE double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FUNE weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FUNE float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FUNE floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FUGT" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FUGT float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FUGT double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FUGT weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FUGT float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FUGT floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FUGE" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FUGE float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FUGE double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FUGE weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FUGE float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FUGE floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FULT" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FULT float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FULT double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FULT weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FULT float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FULT floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FULE" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FULE float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FULE double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FULE weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FULE float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FULE floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FORD" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FORD float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FORD double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FORD weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FORD float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FORD floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FOEQ" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FOEQ float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FOEQ double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FOEQ weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FOEQ float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FOEQ floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FONE" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FONE float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FONE double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FONE weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FONE float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FONE floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FOGT" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FOGT float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FOGT double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FOGT weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FOGT float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FOGT floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FOGE" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FOGE float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FOGE double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FOGE weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FOGE float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FOGE floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FOLT" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FOLT float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FOLT double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FOLT weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FOLT float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FOLT floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : CompareOperation : FOLE" $ do
    it "correctly returns True for float arguments" $
      checkExpression (CompareOperation FOLE float a_float b_float) `shouldBe` True
    it "correctly return True for double arguments" $
      checkExpression (CompareOperation FOLE double a_double b_double) `shouldBe` True
    it "correctly returns False for weakref<void> arguments" $
      checkExpression (CompareOperation FOLE weakrefvoid a_weakrefvoid b_weakrefvoid) `shouldBe` False
    it "correctly returns False for arguments not of different types" $
      checkExpression (CompareOperation FOLE float a_double b_double) `shouldBe` False
    it "correctly returns True for vector<float 4> arguments" $
      checkExpression (CompareOperation FOLE floatx4 a_floatx4 a_floatx4) `shouldBe` True
      
  describe "checkExpression : ConvertOperation : TRUNC" $ do
    it "correctly returns True for (i64, i32) arguments" $
      checkExpression (ConvertOperation TRUNC i64 i32 a_i64 Nothing) `shouldBe` True
    it "correctly returns False for (float, i32) arguments" $
      checkExpression (ConvertOperation TRUNC float i32 a_float Nothing) `shouldBe` False      
    it "correctly returns False for (i32, i64) arguments" $
      checkExpression (ConvertOperation TRUNC i32 i64 a_i32 Nothing) `shouldBe` False
    it "correctly returns False for types different from those stated" $
      checkExpression (ConvertOperation TRUNC i64 i32 a_i16 Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : ZEXT" $ do
    it "correctly returns True for (i32, i64) arguments" $
      checkExpression (ConvertOperation ZEXT i32 i64 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for (float, i32) arguments" $
      checkExpression (ConvertOperation ZEXT float i32 a_float Nothing) `shouldBe` False      
    it "correctly returns False for (i32, i64) arguments" $
      checkExpression (ConvertOperation ZEXT i64 i32 a_i64 Nothing) `shouldBe` False
    it "correctly returns False for types different from those stated" $
      checkExpression (ConvertOperation ZEXT i64 i32 a_i16 Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : SEXT" $ do
    it "correctly returns True for (i32, i64) arguments" $
      checkExpression (ConvertOperation SEXT i32 i64 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for (float, i32) arguments" $
      checkExpression (ConvertOperation SEXT float i32 a_float Nothing) `shouldBe` False      
    it "correctly returns False for (i32, i64) arguments" $
      checkExpression (ConvertOperation SEXT i64 i32 a_i64 Nothing) `shouldBe` False
    it "correctly returns False for types different from those stated" $
      checkExpression (ConvertOperation SEXT i64 i32 a_i16 Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : FPTRUNC" $ do
    it "correctly returns True for (double, float) arguments" $
      checkExpression (ConvertOperation FPTRUNC double float a_double Nothing) `shouldBe` True
    it "correctly returns False for (float, i32) arguments" $
      checkExpression (ConvertOperation FPTRUNC float i32 a_float Nothing) `shouldBe` False      
    it "correctly returns False for (float, double) arguments" $
      checkExpression (ConvertOperation FPTRUNC float double a_float Nothing) `shouldBe` False
    it "correctly returns False for types different from those stated" $
      checkExpression (ConvertOperation FPTRUNC double float a_float Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : FPEXT" $ do
    it "correctly returns True for (float, double) arguments" $
      checkExpression (ConvertOperation FPEXT float double a_float Nothing) `shouldBe` True
    it "correctly returns False for (float, i32) arguments" $
      checkExpression (ConvertOperation FPEXT float i32 a_float Nothing) `shouldBe` False      
    it "correctly returns False for (double, float) arguments" $
      checkExpression (ConvertOperation FPEXT double float a_double Nothing) `shouldBe` False
    it "correctly returns False for types different from those stated" $
      checkExpression (ConvertOperation FPEXT float double a_double Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : FPTOUI" $ do
    it "correctly returns True for (float, i32) arguments" $
      checkExpression (ConvertOperation FPTOUI float i32 a_float Nothing) `shouldBe` True
    it "correctly returns False for (i32, float) arguments" $
      checkExpression (ConvertOperation FPTOUI i32 float a_i32 Nothing) `shouldBe` False      
    it "correctly returns False for types different from those stated" $
      checkExpression (ConvertOperation FPTOUI double i32 a_i16 Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : FPTOSI" $ do
    it "correctly returns True for (float, i32) arguments" $
      checkExpression (ConvertOperation FPTOSI float i32 a_float Nothing) `shouldBe` True
    it "correctly returns False for (i32, float) arguments" $
      checkExpression (ConvertOperation FPTOSI i32 float a_i32 Nothing) `shouldBe` False      
    it "correctly returns False for types different from those stated" $
      checkExpression (ConvertOperation FPTOSI double i32 a_i16 Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : UITOFP" $ do
    it "correctly returns True for (i32, float) arguments" $
      checkExpression (ConvertOperation UITOFP i32 float a_i32 Nothing) `shouldBe` True
    it "correctly returns False for (float, i32) arguments" $
      checkExpression (ConvertOperation UITOFP float i32 a_float Nothing) `shouldBe` False      
    it "correctly returns False for types different from those stated" $
      checkExpression (ConvertOperation UITOFP i64 double a_i32 Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : SITOFP" $ do
    it "correctly returns True for (i32, float) arguments" $
      checkExpression (ConvertOperation SITOFP i32 float a_i32 Nothing) `shouldBe` True
    it "correctly returns False for (float, i32) arguments" $
      checkExpression (ConvertOperation SITOFP float i32 a_float Nothing) `shouldBe` False      
    it "correctly returns False for types different from those stated" $
      checkExpression (ConvertOperation SITOFP i64 double a_i32 Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : BITCAST" $ do
    it "correctly returns True for (int32, float) arguments" $
      checkExpression (ConvertOperation BITCAST i32 float a_i32 Nothing) `shouldBe` True
    it "correctly returns False for (int32, double) arguments" $
      checkExpression (ConvertOperation BITCAST i32 double a_i32 Nothing) `shouldBe` False
    it "correctly returns True for (double, i64) arguments" $
      checkExpression (ConvertOperation BITCAST double i64 a_double Nothing) `shouldBe` True
    it "correctly returns True for (double, i16) arguments" $
      checkExpression (ConvertOperation BITCAST double i16 a_double Nothing) `shouldBe` False
    it "correctly returns False for ref<void> arguments" $
      checkExpression (ConvertOperation BITCAST irefvoid irefvoid a_irefvoid Nothing) `shouldBe` False
    it "correctly returns False for arguments different from those stated" $
      checkExpression (ConvertOperation BITCAST double i64 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : REFCAST" $ do
    it "correctly returns True for ref arguments" $
      checkExpression (ConvertOperation REFCAST refvoid refvoid a_refvoid Nothing) `shouldBe` True
    it "correctly returns True for iref arguments" $
      checkExpression (ConvertOperation REFCAST irefvoid irefvoid a_irefvoid Nothing) `shouldBe` True
    it "correctly returns True for funcref arguments" $
      checkExpression (ConvertOperation REFCAST funcrefadd funcrefadd a_funcrefadd Nothing) `shouldBe` True
    it "correctly returns False for int arguments" $
      checkExpression (ConvertOperation REFCAST i32 i32 a_i32 Nothing) `shouldBe` False
    it "correctly returns False for int arguments different from those stated" $
      checkExpression (ConvertOperation REFCAST refvoid refvoid a_irefvoid Nothing) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (ConvertOperation REFCAST refvoid funcrefadd a_funcrefadd Nothing) `shouldBe` False

  describe "checkExpression : ConvertOperation : PTRCAST" $ do
    it "correctly returns True for (uptr<void>, i32) arguments" $
      checkExpression (ConvertOperation PTRCAST uptrvoid i32 a_uptrvoid Nothing) `shouldBe` True
    it "correctly returns True for (i16, ufuncptradd) arguments" $
      checkExpression (ConvertOperation PTRCAST i16 ufuncptradd a_i16 Nothing) `shouldBe` True
    it "correctly returns True for (ufuncptradd, uptr<void>) arguments" $
      checkExpression (ConvertOperation PTRCAST ufuncptradd uptrvoid a_ufuncptradd Nothing) `shouldBe` True
    it "correctly returns False for ref<void> arguments" $
      checkExpression (ConvertOperation PTRCAST refvoid i32 a_refvoid Nothing) `shouldBe` False
    it "correctly returns False for int arguments different from those stated" $
      checkExpression (ConvertOperation PTRCAST i32 uptrvoid a_uptrvoid Nothing) `shouldBe` False
    it "correctly returns False for arguments of the same type" $
      checkExpression (ConvertOperation PTRCAST i32 i32 a_i32 Nothing) `shouldBe` False

  describe "checkExpression : AtomicRMWOperation : ADD" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined ADD i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined ADD i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined ADD void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined ADD i32 a_irefi32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : AtomicRMWOperation : SUB" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined SUB i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined SUB i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined SUB void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined SUB i32 a_irefi32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : AtomicRMWOperation : AND" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined AND i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined AND i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined AND void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined AND i32 a_irefi32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : AtomicRMWOperation : NAND" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined NAND i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined NAND i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined NAND void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined NAND i32 a_irefi32 a_i64 Nothing) `shouldBe` False
  
  describe "checkExpression : AtomicRMWOperation : OR" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined OR i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined OR i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined OR void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined OR i32 a_irefi32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : AtomicRMWOperation : XOR" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined XOR i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined XOR i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined XOR void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined XOR i32 a_irefi32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : AtomicRMWOperation : MAX" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined MAX i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined MAX i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined MAX void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined MAX i32 a_irefi32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : AtomicRMWOperation : MIN" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined MIN i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined MIN i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined MIN void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined MIN i32 a_irefi32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : AtomicRMWOperation : UMAX" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined UMAX i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined UMAX i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined UMAX void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined UMAX i32 a_irefi32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : AtomicRMWOperation : UMIN" $ do
    it "correctly returns True for iref<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined UMIN i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> argument" $
      checkExpression (AtomicRMWOperation False undefined UMIN i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> argument" $
      checkExpression (AtomicRMWOperation False undefined UMIN void a_irefvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for types different to those specified" $
      checkExpression (AtomicRMWOperation False undefined UMIN i32 a_irefi32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : CmpXchg" $ do
    it "correctly returns True for iref<i32> type" $
      checkExpression (CmpXchg False False undefined undefined i32 a_irefi32 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<void> type" $
      checkExpression (CmpXchg False False undefined undefined void a_irefvoid a_void b_void Nothing) `shouldBe` False
    it "correctly returns False for arguments of different types" $
      checkExpression (CmpXchg False False undefined undefined i32 a_irefi32 a_i32 b_i64 Nothing) `shouldBe` False
    it "correctly returns False for arguments not of type specified" $
      checkExpression (CmpXchg False False undefined undefined i32 a_irefi32 a_i64 b_i64 Nothing) `shouldBe` False
    
  describe "checkExpression : New" $ do
    it "correctly returns True for i16 type" $
      checkExpression (New i16 Nothing) `shouldBe` True
    it "correctly returns False for hybrid<i32 float> type" $
      checkExpression (New hybrid_i32_float Nothing) `shouldBe` False

  describe "checkExpression : NewHybrid" $ do
    it "correctly returns True for hybrid<i32 float> int32 type" $
      checkExpression (NewHybrid hybrid_i32_float i32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for void i32 type" $
      checkExpression (NewHybrid void i32 a_i32 Nothing) `shouldBe` False
    it "correctly returns False for hybrid<i32 float> float type" $
      checkExpression (NewHybrid hybrid_i32_float float a_float Nothing) `shouldBe` False
    it "correctly returns False for type different from specified type" $
      checkExpression (NewHybrid hybrid_i32_float i32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : Alloca" $ do
    it "correctly returns true for i32 type" $
      checkExpression (Alloca i32 Nothing) `shouldBe` True
    it "correctly returns False for hybrid<i32 float> type" $
      checkExpression (Alloca hybrid_i32_float Nothing) `shouldBe` False

  describe "checkExpression : AllocaHybrid" $ do
    it "correctly returns True for hybrid<i32 float> i64 type" $
      checkExpression (AllocaHybrid hybrid_i32_float i64 a_i64 Nothing) `shouldBe` True
    it "correctly returns False for i32 i32 type" $
      checkExpression (AllocaHybrid i32 i32 a_i32 Nothing) `shouldBe` False
    it "correctly returns False for hybrid<i32 float> float type" $
      checkExpression (AllocaHybrid hybrid_i32_float float a_float Nothing) `shouldBe` False
    it "correctly returns False for types different from those speficied" $
      checkExpression (AllocaHybrid hybrid_i32_float i32 a_i64 Nothing) `shouldBe` False

  describe "checkExpression : Throw" $ do
    it "correctly returns True for ref<i32>" $
      checkExpression (Throw a_refi32) `shouldBe` True
    it "correctly returns False for i32" $
      checkExpression (Throw a_i32) `shouldBe` False

  describe "checkExpression : Call" $ do
    it "correctly returns True for addi32sig function call" $
      checkExpression (Call addi32sig a_funcrefadd [a_i32, b_i32] Nothing Nothing) `shouldBe` True
    it "correctly returns False for addi32sig function call with i64 parameters" $
      checkExpression (Call addi32sig a_funcrefadd [a_i64, b_i64] Nothing Nothing) `shouldBe` False
    it "correctly returns False for addi64sig function call with ptr to addi32sig" $
      checkExpression (Call addi64sig a_funcrefadd [a_i64, b_i64] Nothing Nothing) `shouldBe` False

  describe "checkExpression : CCall" $ do
    it "correctly returns True for addi32sig function call" $
      checkExpression (CCall Mu i32 addi32sig a_i32 [a_i32, b_i32] Nothing Nothing) `shouldBe` True
    it "correctly returns False for addi32sig function call with i64 parameters" $
      checkExpression (CCall Mu i32 addi32sig a_i32 [a_i64, b_i64] Nothing Nothing) `shouldBe` False
    it "correctly returns False for calle type different from type specified" $
      checkExpression (CCall Mu void addi64sig a_i32 [a_i64, b_i64] Nothing Nothing) `shouldBe` False

  describe "checkExpression : TailCall" $ do
    it "correctly returns True for addi32sig function call" $
      checkExpression (TailCall addi32sig a_funcrefadd [a_i32, b_i32]) `shouldBe` True
    it "correctly returns False for addi32sig function call with i64 parameters" $
      checkExpression (TailCall addi32sig a_funcrefadd [a_i64, b_i64]) `shouldBe` False
    it "correctly returns False for addi64sig function call with ptr to addi32sig" $
      checkExpression (TailCall addi64sig a_funcrefadd [a_i64, b_i64]) `shouldBe` False

  describe "checkExpression : Branch2" $ do
    it "correctly returns True for int<1> condition" $
      checkExpression (Branch2 a_i1 undefined undefined) `shouldBe` True
    it "correctly returns False for int<32> contition" $
      checkExpression (Branch2 a_i32 undefined undefined) `shouldBe` False
    it "correctly returns False for float contition" $
      checkExpression (Branch2 a_float undefined undefined) `shouldBe` False

  describe "checkExpression : Switch" $ do
    it "correctly returns True for i64 opnd with all i64 block types" $
      checkExpression (Switch i64 a_i64 undefined [(a_i64, undefined), (b_i64, undefined)]) `shouldBe` True
    it "correctly returns False for i64 with float opnd" $
      checkExpression (Switch i64 a_float undefined []) `shouldBe` False
    it "correctly returns False for i64 opnt with i32 block types" $
      checkExpression (Switch i64 a_i64 undefined [(a_i32, undefined), (b_i32, undefined)]) `shouldBe` False

  describe "checkExpression : SwapStack" $ do
    it "correctly returns True for stackref type" $
      checkExpression (SwapStack a_sref undefined undefined Nothing Nothing) `shouldBe` True
    it "correctly returns False for iref<void> type" $
      checkExpression (SwapStack a_irefvoid undefined undefined Nothing Nothing) `shouldBe` False

  describe "checkExpression : NewThread" $ do
    it "correctly returns True for stackref type" $
      checkExpression (NewThread a_sref undefined  Nothing) `shouldBe` True
    it "correctly returns False for iref<void> type" $
      checkExpression (NewThread a_irefvoid  undefined  Nothing) `shouldBe` False

  describe "checkExpression : Load" $ do
    it "correctly returns True for iref<i32> type" $
      checkExpression (Load False Nothing i32 a_irefi32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> type" $
      checkExpression (Load False Nothing i32 a_uptri32 Nothing) `shouldBe` True
    it "correctly returns False for ref<void> type" $
      checkExpression (Load False Nothing void a_refvoid Nothing) `shouldBe` False
    it "correctly returns False for different types" $
      checkExpression (Load False Nothing i64 a_irefi32 Nothing) `shouldBe` False

  describe "checkExpression : Store" $ do
    it "correctly returns True for iref<i32> type" $
      checkExpression (Store False Nothing i32 a_irefi32 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<i32> type" $
      checkExpression (Store False Nothing i32 a_uptri32 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for ref<void> type" $
      checkExpression (Store False Nothing void a_refvoid a_void Nothing) `shouldBe` False
    it "correctly returns False for different types" $
      checkExpression (Store False Nothing i64 a_irefi32 a_i32 Nothing) `shouldBe` False

  describe "checkExpression : ExtractValueS" $ do
    it "correctly returns True for struct<i32 float> type" $
      checkExpression (ExtractValueS structi32float undefined a_structi32float Nothing) `shouldBe` True
    it "correctly returns False for vectori32x4 type" $
      checkExpression (ExtractValueS i32x4  undefined a_i32x4 Nothing) `shouldBe` False
    
  describe "checkExpression : InsertValueS" $ do
    it "correctly returns True for struct<i32 float> type" $
      checkExpression (InsertValueS structi32float undefined a_structi32float a_void Nothing) `shouldBe` True
    it "correctly returns False for vectori32x4 type" $
      checkExpression (InsertValueS i32x4  undefined a_i32x4 a_void Nothing) `shouldBe` False

  describe "checkExpression : ExtractElement" $ do
    it "correctly returns True for vectori32x4 type" $
      checkExpression (ExtractValue i32x4 i32 a_i32x4 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for arrayi32x4 type" $
      checkExpression (ExtractValue arrayi32x4 i64 a_arrayi32x4 a_i64 Nothing) `shouldBe` True
    it "correctly returns False for float index type" $
      checkExpression (ExtractValue arrayi32x4 float a_arrayi32x4 a_float Nothing) `shouldBe` False
    it "correctly returns False for index types not matched" $
      checkExpression (ExtractValue arrayi32x4 i64 a_arrayi32x4 a_i32 Nothing) `shouldBe` False
    it "correctly returns False for array types not matched" $
      checkExpression (ExtractValue arrayi32x4 i32 a_i32x4 a_i32 Nothing) `shouldBe` False

  describe "checkExpression : InsertElement" $ do
    it "correctly returns True for vectori32x4 type" $
      checkExpression (InsertValue i32x4 i32 a_i32x4 a_i32 b_i32 Nothing) `shouldBe` True
    it "correctly returns True for arrayi32x4 type" $
      checkExpression (InsertValue arrayi32x4 i64 a_arrayi32x4 a_i64 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for float index type" $
      checkExpression (InsertValue arrayi32x4 float a_arrayi32x4 a_float a_i32 Nothing) `shouldBe` False
    it "correctly returns False for index types not matched" $
      checkExpression (InsertValue arrayi32x4 i64 a_arrayi32x4 a_i32 a_i32 Nothing) `shouldBe` False
    it "correctly returns False for array types not matched" $
      checkExpression (InsertValue arrayi32x4 i32 a_i32x4 a_i32 a_i32 Nothing) `shouldBe` False

  describe "checkExpression : ShuffleVector" $ do
    it "correctly returns True for vector<i32 4> types" $
      checkExpression (ShuffleVector i32x4 i32x4 a_i32x4 b_i32x4 a_i32x4 Nothing) `shouldBe` True
    it "correctly returns False for array<i32 4> types" $
      checkExpression (ShuffleVector arrayi32x4 arrayi32x4 a_arrayi32x4 a_arrayi32x4 a_arrayi32x4 Nothing) `shouldBe` False
    it "correctly returns False fo float mask type" $
      checkExpression (ShuffleVector i32x4 floatx4 a_i32x4 b_i32x4 a_floatx4 Nothing) `shouldBe` False
    it "correctly refutns False for types different from those specified" $
      checkExpression (ShuffleVector i32x4 i32x4 a_floatx4 a_floatx4 a_i32x4 Nothing) `shouldBe` False

  describe "checkExpression : GetIRef" $ do
    it "correctly returns True for ref<i32> type" $
      checkExpression (GetIRef i32 a_refi32 Nothing) `shouldBe` True
    it "correctly returns False for irefvoid type" $
      checkExpression (GetIRef void a_irefvoid Nothing) `shouldBe` False
    it "correctly returns False for type different from type specified" $
      checkExpression (GetIRef i32 a_refvoid Nothing) `shouldBe` False

  describe "checkExpression : GetFieldIRef" $ do
    it "correctly returns True for iref<struct<i32 float>> type" $
      checkExpression (GetFieldIRef False structi32float undefined a_iref_structi32float Nothing) `shouldBe` True
    it "correctly returns True for uptr<struct<i32 float>> type" $
      checkExpression (GetFieldIRef False structi32float undefined a_uptr_structi32float Nothing) `shouldBe` True 
    it "correctly returns True for uptr<hybrid<i32 float>> type" $
      checkExpression (GetFieldIRef False hybrid_i32_float undefined a_uptr_hybrid_i32_float Nothing) `shouldBe` True 
    it "correctly returns True for iref<hybrid<i32 float>> type" $
      checkExpression (GetFieldIRef False hybrid_i32_float undefined a_iref_hybrid_i32_float Nothing) `shouldBe` True

    it "correctly returns False for iref<array<i32 4>> type" $
      checkExpression (GetFieldIRef False arrayi32x4 undefined a_iref_arrayi32x4 Nothing) `shouldBe` False
    it "correctly returns False for arguments different from those specified" $
      checkExpression (GetFieldIRef False hybrid_i32_float undefined a_iref_structi32float Nothing) `shouldBe` False

  describe "checkExpression : GetElemIRef" $ do
    it "correctly returns True for iref<array<i32 4>> type" $
      checkExpression (GetElemIRef False arrayi32x4 i32 a_iref_arrayi32x4 a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<array<i32 4>> type" $
      checkExpression (GetElemIRef False arrayi32x4 i32 a_uptr_arrayi32x4 a_i32 Nothing) `shouldBe` True
    it "correctly returns False for iref<struct<i32 float>> type" $
      checkExpression (GetElemIRef False structi32float i32 a_iref_structi32float a_i32 Nothing) `shouldBe` False
    it "correctly returns False for non int index type" $
      checkExpression (GetElemIRef False arrayi32x4 double a_iref_arrayi32x4 a_double Nothing) `shouldBe` False

  describe "checkExpression : ShiftIRef" $ do
    it "correctly returns True for iref<void> type" $
      checkExpression (ShiftIRef False void i32 a_irefvoid a_i32 Nothing) `shouldBe` True
    it "correctly returns True for uptr<void> type" $
      checkExpression (ShiftIRef False void i32 a_uptrvoid a_i32 Nothing) `shouldBe` True
    it "correctly returns False for non int index type" $
      checkExpression (ShiftIRef False arrayi32x4 double a_iref_arrayi32x4 a_double Nothing) `shouldBe` False

  describe "checkExpression : GetVarPartIRef" $ do
    it "correctly returns True for iref<hybrid<i32 float>> type" $
      checkExpression (GetVarPartIRef False hybrid_i32_float a_iref_hybrid_i32_float Nothing) `shouldBe` True
    it "correctly returns True for uptr<hybrid<i32 float>> type" $
      checkExpression (GetVarPartIRef False hybrid_i32_float a_uptr_hybrid_i32_float Nothing) `shouldBe` True
    it "correctly returns False for iref<void> type" $
      checkExpression (GetVarPartIRef False void a_uptrvoid Nothing) `shouldBe` False

  describe "checkAssign : BinaryOperation" $ do
    it "correctly returns True for correct ret type" $
      checkAssign (Assign [a_i32] (BinaryOperation Add i32 a_i32 b_i32 Nothing)) `shouldBe` True
    it "correctly returns False for incorrect ret type" $
      checkAssign (Assign [a_i32] (BinaryOperation Add i64 a_i64 b_i64 Nothing)) `shouldBe` False

  describe "checkAssign : CompareOperation" $ do
    it "correctly returns True for int<1> ret type" $
      checkAssign (Assign [a_i1] (CompareOperation EQ i32 a_i32 b_i32)) `shouldBe` True
    it "correctly returns False for int<32> ret type" $
      checkAssign (Assign [a_i32] (CompareOperation EQ i32 a_i32 b_i32)) `shouldBe` False
    it "correctly returns True for vector<int<1> 4> types" $
      checkAssign (Assign [SSAVariable Local "res_vec" (UvmTypeDef "res_vec" (Vector i1 4))] (CompareOperation EQ i32x4 a_i32x4 a_i32x4)) `shouldBe` True
    it "correctly returns False for vector<float 4> ret type (with EQ)" $
      checkAssign (Assign [SSAVariable Local "res_vec" (UvmTypeDef "res_vec" (Vector i32 4))] (CompareOperation EQ i32x4 a_i32x4 a_i32x4)) `shouldBe` False

  describe "checkAssign : ConvertOperation" $ do
    it "correctly returns True for i32 ret type (with Trunc)" $
      checkAssign (Assign [a_i32] (ConvertOperation TRUNC i64 i32 a_i64 Nothing)) `shouldBe` True
    it "correctly returns False for float ret type (with Trunc)" $
      checkAssign (Assign [a_float] (ConvertOperation TRUNC i64 i32 a_i64 Nothing)) `shouldBe` False

  describe "checkAssign : AtomicRMWOperation" $ do
    it "correctly returns True for i32 types " $
      checkAssign (Assign [a_i32] (AtomicRMWOperation False undefined ADD i32 a_irefi32 a_i32 Nothing)) `shouldBe` True
    it "correctly returns False for float ret with i32 types" $ 
      checkAssign (Assign [a_float] (AtomicRMWOperation False undefined ADD i32 a_irefi32 a_i32 Nothing)) `shouldBe` False

  describe "checkAssign : CmpXchg" $ do
    it "correctly returns True for (i32 i1) return type" $
      checkAssign (Assign [a_i32, a_i1] (CmpXchg False False undefined undefined i32 a_irefi32 a_i32 b_i32 Nothing)) `shouldBe` True
    it "correctly returns True for (float i1) return type" $
      checkAssign (Assign [a_float, a_i1] (CmpXchg False False undefined undefined i32 a_irefi32 a_i32 b_i32 Nothing)) `shouldBe` False
    it "correctly returns True for (i32 i32) return type" $
      checkAssign (Assign [a_i32, a_i32] (CmpXchg False False undefined undefined i32 a_irefi32 a_i32 b_i32 Nothing)) `shouldBe` False

  describe "checkAssign : New" $ do
    it "correctly returns True for ref<i32> return type" $
      checkAssign (Assign [a_refi32] (New i32 Nothing)) `shouldBe` True
    it "correctly returns False for ref<float> return type (with int argument)" $
      checkAssign (Assign [a_reffloat] (New i32 Nothing)) `shouldBe` False

  describe "checkAssign : NewHybrid" $ do
    it "correctly returns True for ref<hybrid<float, i32>> return type" $
      checkAssign (Assign [a_ref_hybrid_i32_float] (NewHybrid hybrid_i32_float i32 a_i32 Nothing)) `shouldBe` True
    it "correctly returns False for iref<hybrid<float, i32>> return type" $
      checkAssign (Assign [a_iref_hybrid_i32_float] (NewHybrid hybrid_i32_float i32 a_i32 Nothing)) `shouldBe` False
    it "correctly returns False for ref<i32> return type" $
      checkAssign (Assign [a_refi32] (NewHybrid hybrid_i32_float i32 a_i32 Nothing)) `shouldBe` False

  describe "checkAssign : Alloca" $ do
    it "correctly returns True for iref<i32> return type" $
      checkAssign (Assign [a_irefi32] (Alloca i32 Nothing)) `shouldBe` True
    it "correctly returns False for ref<i32> return type" $
      checkAssign (Assign [a_refi32] (Alloca i32 Nothing)) `shouldBe` False
    it "correctly returns False for iref<float> return type" $
      checkAssign (Assign [a_ireffloat] (Alloca i32 Nothing)) `shouldBe` False

  describe "checkAssign : AllocaHybrid" $ do
    it "correctly returns False for ref<hybrid<float, i32>> return type" $
      checkAssign (Assign [a_ref_hybrid_i32_float] (AllocaHybrid hybrid_i32_float i32 a_i32 Nothing)) `shouldBe` False
    it "correctly returns True for iref<hybrid<float, i32>> return type" $
      checkAssign (Assign [a_iref_hybrid_i32_float] (AllocaHybrid hybrid_i32_float i32 a_i32 Nothing)) `shouldBe` True
    it "correctly returns True for ref<i32> return type" $
      checkAssign (Assign [a_refi32] (NewHybrid hybrid_i32_float i32 a_i32 Nothing)) `shouldBe` False

  describe "checkAssign : Call" $ do
    it "correctly returns True for i32 ret type from add call" $
      checkAssign (Assign [a_i32] (Call addi32sig a_funcrefadd [a_i32, b_i32] Nothing Nothing)) `shouldBe` True
    it "correctly returns False for float ret type from add call" $
      checkAssign (Assign [a_float] (Call addi32sig a_funcrefadd [a_i32, b_i32] Nothing Nothing)) `shouldBe` False

  describe "checkAssign : CCall" $ do
    it "correctly returns True for i32 ret type from add call" $
      checkAssign (Assign [a_i32] (CCall Mu i32 addi32sig a_i32 [a_i32, b_i32] Nothing Nothing)) `shouldBe` True
    it "correctly returns False for float ret type from add call" $
      checkAssign (Assign [a_float] (CCall Mu i32 addi32sig a_i32 [a_i32, b_i32] Nothing Nothing)) `shouldBe` False

  describe "checkAssign : WatchPoint" $ do
    it "correctly returns True for type matches" $
      checkAssign (Assign [a_i32, a_float] (WatchPoint undefined 1 [i32, float] undefined undefined Nothing Nothing)) `shouldBe` True
    it "correctly returns False for non type matches" $
      checkAssign (Assign [a_i32, a_float, a_double] (WatchPoint undefined 1 [i32, float] undefined undefined Nothing Nothing)) `shouldBe` False
  
  describe "checkAssign : Trap" $ do
    it "corrctly returns True for type matches" $
      checkAssign (Assign [a_i32, a_float] (Trap undefined [i32, float] undefined undefined)) `shouldBe` True
    it "corrctly returns False for type matches" $
      checkAssign (Assign [a_i32, a_double] (Trap undefined [i32, float] undefined undefined)) `shouldBe` False

  describe "checkAssign : Load" $ do
    it "correctly returns True for matching i32 type" $
      checkAssign (Assign [a_i32] (Load False Nothing i32 a_irefi32 Nothing)) `shouldBe` True
    it "correctly returns False for float ret type with i32 arg type" $
      checkAssign (Assign [a_float] (Load False Nothing i32 a_irefi32 Nothing)) `shouldBe` False

  describe "checkAssign : ExtractValueS" $ do
    it "correctly returns True for correct ret type" $
      checkAssign (Assign [a_i32] (ExtractValueS structi32float 0 a_structi32float Nothing)) `shouldBe` True
    it "correctly returns False for incorrect ret type" $
      checkAssign (Assign [a_float] (ExtractValueS structi32float 0 a_structi32float Nothing)) `shouldBe` False 

  describe "checkAssign : InsertValueS" $ do
    it "correctly returns True for correct ret type" $
      checkAssign (Assign [a_structi32float] (InsertValueS structi32float 1 a_structi32float a_float Nothing)) `shouldBe` True
    it "correctly returns False for incorrect ret type" $
      checkAssign (Assign [a_void] (InsertValueS structi32float 1 a_structi32float a_float Nothing)) `shouldBe` False

  describe "checkAssign : ExtractValue" $ do
    it "corrrectly returns True for vector<i32 4> types" $
      checkAssign (Assign [a_i32] (ExtractValue i32x4 i16 a_i32x4 a_i16 Nothing)) `shouldBe` True
    it "corrrectly returns True for arrray<i32 4> types" $
      checkAssign (Assign [a_i32] (ExtractValue arrayi32x4 i16 a_arrayi32x4 a_i16 Nothing)) `shouldBe` True
    it "corrrectly returns False for incorrect return type" $
      checkAssign (Assign [a_double] (ExtractValue i32x4 i16 a_i32x4 a_i16 Nothing)) `shouldBe` False

  describe "checkAssign : InsertValue" $ do
    it "correctly returns True for matching types" $
      checkAssign (Assign [a_i32x4] (InsertValue i32x4 i16 a_i32x4 a_i16 a_i32 Nothing)) `shouldBe` True
    it "correctly returns False for non matching types" $
      checkAssign (Assign [a_floatx4] (InsertValue i32x4 i16 a_i32x4 a_i16 a_i32 Nothing)) `shouldBe` False

  describe "checkAssign : ShuffleVector" $ do
    it "correctly returns True for matching types" $
      checkAssign (Assign [a_i32x4] (ShuffleVector i32x4 i32x4 a_i32x4 a_i32x4 a_i32x4 Nothing)) `shouldBe` True
    it "correctly returns True for correct length ret type" $
      checkAssign (Assign [a_doublex4] (ShuffleVector doublex2 i32x4 a_doublex2 a_doublex2 a_i32x4 Nothing)) `shouldBe` True
    it "correctly returns False for non matching types" $
      checkAssign (Assign [a_floatx4] (ShuffleVector i32x4 i32x4 a_i32x4 a_i32x4 a_i32x4 Nothing)) `shouldBe` False
    it "correctly returns False for incorrect length ret type" $
      checkAssign (Assign [a_doublex2] (ShuffleVector doublex2 i32x4 a_doublex2 a_doublex2 a_i32x4 Nothing)) `shouldBe` False

  describe "checkAssign : GetIRef" $ do
    it "correctly returns True for iref ret type" $
      checkAssign (Assign [a_irefi32] (GetIRef i32 a_refi32 Nothing)) `shouldBe` True
    it "correctly returns False for ref ret type" $
      checkAssign (Assign [a_refi32] (GetIRef i32 a_refi32 Nothing)) `shouldBe` False
    it "correctly returns False for iref<float> ret type (with i32 arg type)" $
      checkAssign (Assign [a_ireffloat] (GetIRef i32 a_refi32 Nothing)) `shouldBe` False

  describe "checkAssign : GetFieldIRef" $ do
    it "correctly returns True for valid Ptr & Struct args" $
      checkAssign (Assign [a_uptrfloat] (GetFieldIRef True structi32float 1 a_uptr_structi32float Nothing)) `shouldBe` True
    it "correctly returns True for valid Ptr & Hybrid args" $
      checkAssign (Assign [a_uptri32] (GetFieldIRef True hybrid_i32_float 0 a_uptr_hybrid_i32_float Nothing)) `shouldBe` True
    it "correctly returns True for valid iref & struct args" $
      checkAssign (Assign [a_ireffloat] (GetFieldIRef False structi32float 1 a_uptr_structi32float Nothing)) `shouldBe` True
    it "correctly returns True for valid iref & hybrid args" $
      checkAssign (Assign [a_irefi32] (GetFieldIRef False hybrid_i32_float 0 a_uptr_hybrid_i32_float Nothing)) `shouldBe` True
    it "correctly returns False for invalid ptr & struct args" $
      checkAssign (Assign [a_ireffloat] (GetFieldIRef True structi32float 1 a_uptr_structi32float Nothing)) `shouldBe` False
    it "correctly returns False for invalid iref and struct args" $
      checkAssign (Assign [a_uptrfloat] (GetFieldIRef False structi32float 1 a_uptr_structi32float Nothing)) `shouldBe` False

  describe "checkAssign : GetElemIRef" $ do
    it "correctly returns True for valid ptr type" $
      checkAssign (Assign [a_uptri32] (GetElemIRef True arrayi32x4 i16 a_uptr_arrayi32x4 a_i16 Nothing)) `shouldBe` True
    it "correctly returns True for valid iref type" $
      checkAssign (Assign [a_irefi32] (GetElemIRef False arrayi32x4 i16 a_iref_arrayi32x4 a_i16 Nothing)) `shouldBe` True
    it "correctly returns False for invalid iref type" $
      checkAssign (Assign [a_irefi32] (GetElemIRef True arrayi32x4 i16 a_uptr_arrayi32x4 a_i16 Nothing)) `shouldBe` False
    it "correctly returns False for invalid uptr type" $
      checkAssign (Assign [a_uptri32] (GetElemIRef False arrayi32x4 i16 a_uptr_arrayi32x4 a_i16 Nothing)) `shouldBe` False
      
  describe "checkAssign : ShifIRef" $ do
    it "correctly returns True for valid ptr type" $
      checkAssign (Assign [a_uptri32] (ShiftIRef True i32 i16 a_uptri32 a_i16 Nothing)) `shouldBe` True
    it "correctly returns True for valid iref type" $
      checkAssign (Assign [a_irefi32] (ShiftIRef False i32 i16 a_irefi32 a_i16 Nothing)) `shouldBe` True
    it "correctly returns False for invalid ret type" $
      checkAssign (Assign [a_uptri32] (ShiftIRef True float i16 a_uptrfloat a_i16 Nothing)) `shouldBe` False
    it "correctly returns False for valid iref with ptr arg" $
      checkAssign (Assign [a_irefi32] (ShiftIRef True i32 i16 a_uptri32 a_i16 Nothing)) `shouldBe` False

  describe "checkAssign : GetVarPartIRef" $ do
    it "correctly returns True for valid ptr type" $
      checkAssign (Assign [a_uptrfloat] (GetVarPartIRef True hybrid_i32_float a_uptr_hybrid_i32_float Nothing)) `shouldBe` True
    it "correctly returns True for valid iref type" $
      checkAssign (Assign [a_ireffloat] (GetVarPartIRef False hybrid_i32_float a_uptr_hybrid_i32_float Nothing)) `shouldBe` True
    it "correctly returns False for iref ret type with ptr arg" $
      checkAssign (Assign [a_ireffloat] (GetVarPartIRef True hybrid_i32_float a_uptr_hybrid_i32_float Nothing)) `shouldBe` False
    it "correctly returns False invalid ret type" $
      checkAssign (Assign [a_uptri32] (GetVarPartIRef True hybrid_i32_float a_uptr_hybrid_i32_float Nothing)) `shouldBe` False
      
  describe "checkAST" $ do
    it "correctly identifies a type missmatch in an add decleration" $
      checkAst addProg `shouldBe` ["Type Error Occurred: add -> v1 -> entry -> %ret = ADD <@i32> @a_i64 @a_i32"]
