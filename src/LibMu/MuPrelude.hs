{-#LANGUAGE NoImplicitPrelude#-}

module LibMu.MuPrelude where

import LibMu.MuSyntax

type MuPrelude = ([UvmTypeDef], [Declaration], [FuncSig])

preludeContents :: MuPrelude
preludeContents = (
  [i1, i8, i16, i32, i64, float, double, void, threadref, uptr_void],
  [i1_0, i1_1, i8_0, i8_1, i16_0, i16_1, i32_0, i32_1, i64_0, i64_1],
  []
                  )

--types
i1, i8, i16, i32, i64, float, double, threadref, void, uptr_void :: UvmTypeDef
i1 = UvmTypeDef "i1" (MuInt 1)
i8 = UvmTypeDef "i8" (MuInt 8)
i16 = UvmTypeDef "i16" (MuInt 16)
i32 = UvmTypeDef "i32" (MuInt 32)
i64 = UvmTypeDef "i64" (MuInt 64)
float = UvmTypeDef "float" MuFloat
double = UvmTypeDef "double" MuDouble
threadref = UvmTypeDef "threadref" ThreadRef
void = UvmTypeDef "void" Void
uptr_void = UvmTypeDef "uptr.void" (UPtr void)

--constant
i1_0, i1_1, i8_0, i8_1, i16_0, i16_1, i32_0, i32_1, i64_0, i64_1 :: Declaration
i1_0 = ConstDecl (SSAVariable Global "i1_0" i1) "0"
i1_1 = ConstDecl (SSAVariable Global "i1_1" i1) "1"
i8_0 = ConstDecl (SSAVariable Global "i8_0" i8) "0"
i8_1 = ConstDecl (SSAVariable Global "i8_1" i8) "1"
i16_0 = ConstDecl (SSAVariable Global "i16_0" i16) "0"
i16_1 = ConstDecl (SSAVariable Global "i16_1" i16) "1"
i32_0 = ConstDecl (SSAVariable Global "i32_0" i32) "0"
i32_1 = ConstDecl (SSAVariable Global "i32_1" i32) "1"
i64_0 = ConstDecl (SSAVariable Global "i64_0" i64) "0"
i64_1 = ConstDecl (SSAVariable Global "i64_1" i64) "1"

--signatures
--to be implamented
