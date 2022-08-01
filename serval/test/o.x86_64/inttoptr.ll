; ModuleID = 'inttoptr.c'
source_filename = "inttoptr.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@global = dso_local global i32 0, align 4, !dbg !0
@.src = private unnamed_addr constant [11 x i8] c"inttoptr.c\00", align 1
@0 = private unnamed_addr constant { i16, i16, [6 x i8] } { i16 0, i16 11, [6 x i8] c"'int'\00" }
@1 = private unnamed_addr global { { [11 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* } { { [11 x i8]*, i32, i32 } { [11 x i8]* @.src, i32 23, i32 45 }, { i16, i16, [6 x i8] }* @0 }
@pointer = dso_local global i32* null, align 8, !dbg !13
@2 = private unnamed_addr global { { [11 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* } { { [11 x i8]*, i32, i32 } { [11 x i8]* @.src, i32 32, i32 21 }, { i16, i16, [6 x i8] }* @0 }

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind readnone willreturn
define dso_local i64 @getglobal() local_unnamed_addr #0 !dbg !20 {
entry:
  ret i64 ptrtoint (i32* @global to i64), !dbg !23
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind readnone willreturn
define dso_local i64 @add1(i64 %x) local_unnamed_addr #0 !dbg !24 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !28, metadata !DIExpression()), !dbg !29
  %add = add i64 %x, 1, !dbg !30
  ret i64 %add, !dbg !31
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind readnone willreturn
define dso_local i64 @sub1(i64 %x) local_unnamed_addr #0 !dbg !32 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !34, metadata !DIExpression()), !dbg !35
  %sub = add i64 %x, -1, !dbg !36
  ret i64 %sub, !dbg !37
}

; Function Attrs: nounwind
define dso_local i32 @test1() local_unnamed_addr #1 !dbg !38 {
entry:
  call void @llvm.dbg.value(metadata i64 ptrtoint (i32* @global to i64), metadata !42, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata i64 ptrtoint (i32* @global to i64), metadata !43, metadata !DIExpression()), !dbg !44
  %call2 = tail call i64 @sub1(i64 ptrtoint (i32* @global to i64)) #4, !dbg !45
  %call3 = tail call i64 @add1(i64 %call2) #4, !dbg !46
  %0 = inttoptr i64 %call3 to i32*, !dbg !47
  store volatile i32 5, i32* %0, align 4, !dbg !48
  %call4 = tail call i64 @add1(i64 ptrtoint (i32* @global to i64)) #4, !dbg !49
  %call5 = tail call i64 @sub1(i64 %call4) #4, !dbg !50
  %1 = inttoptr i64 %call5 to i32*, !dbg !51
  %2 = load volatile i32, i32* %1, align 4, !dbg !52
  %3 = tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %2, i32 -5), !dbg !53
  %4 = extractvalue { i32, i1 } %3, 1, !dbg !53, !nosanitize !4
  br i1 %4, label %handler.sub_overflow, label %cont, !dbg !53, !prof !54, !nosanitize !4

handler.sub_overflow:                             ; preds = %entry
  %5 = zext i32 %2 to i64, !dbg !53, !nosanitize !4
  tail call void @__ubsan_handle_sub_overflow(i8* bitcast ({ { [11 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* }* @1 to i8*), i64 %5, i64 5) #5, !dbg !53, !nosanitize !4
  br label %cont, !dbg !53, !nosanitize !4

cont:                                             ; preds = %handler.sub_overflow, %entry
  %6 = extractvalue { i32, i1 } %3, 0, !dbg !53, !nosanitize !4
  ret i32 %6, !dbg !55
}

; Function Attrs: uwtable
declare dso_local void @__ubsan_handle_sub_overflow(i8*, i64, i64) local_unnamed_addr #2

; Function Attrs: nounwind
define dso_local i32 @test2() local_unnamed_addr #1 !dbg !56 {
entry:
  store i32 66, i32* @global, align 4, !dbg !57
  store volatile i32* @global, i32** @pointer, align 8, !dbg !58
  %0 = load volatile i32*, i32** @pointer, align 8, !dbg !59
  %1 = load volatile i32, i32* %0, align 4, !dbg !60
  %2 = tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %1, i32 -66), !dbg !61
  %3 = extractvalue { i32, i1 } %2, 1, !dbg !61, !nosanitize !4
  br i1 %3, label %handler.sub_overflow, label %cont, !dbg !61, !prof !54, !nosanitize !4

handler.sub_overflow:                             ; preds = %entry
  %4 = zext i32 %1 to i64, !dbg !61, !nosanitize !4
  tail call void @__ubsan_handle_sub_overflow(i8* bitcast ({ { [11 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* }* @2 to i8*), i64 %4, i64 66) #5, !dbg !61, !nosanitize !4
  br label %cont, !dbg !61, !nosanitize !4

cont:                                             ; preds = %handler.sub_overflow, %entry
  %5 = extractvalue { i32, i1 } %2, 0, !dbg !61, !nosanitize !4
  ret i32 %5, !dbg !62
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #3

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare { i32, i1 } @llvm.sadd.with.overflow.i32(i32, i32) #3

attributes #0 = { mustprogress nofree noinline norecurse nosync nounwind readnone willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #1 = { nounwind "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #2 = { uwtable }
attributes #3 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #4 = { nobuiltin "no-builtins" }
attributes #5 = { nounwind }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!16, !17, !18}
!llvm.ident = !{!19}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "global", scope: !2, file: !3, line: 3, type: !11, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "Homebrew clang version 13.0.1", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, retainedTypes: !5, globals: !12, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "inttoptr.c", directory: "/Users/mkf727/repos/serval/test")
!4 = !{}
!5 = !{!6, !9}
!6 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !7, line: 256, baseType: !8)
!7 = !DIFile(filename: "/usr/local/Cellar/llvm/13.0.1/lib/clang/13.0.1/include/stdint.h", directory: "")
!8 = !DIBasicType(name: "long unsigned int", size: 64, encoding: DW_ATE_unsigned)
!9 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !10, size: 64)
!10 = !DIDerivedType(tag: DW_TAG_volatile_type, baseType: !11)
!11 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!12 = !{!0, !13}
!13 = !DIGlobalVariableExpression(var: !14, expr: !DIExpression())
!14 = distinct !DIGlobalVariable(name: "pointer", scope: !2, file: !3, line: 4, type: !15, isLocal: false, isDefinition: true)
!15 = !DIDerivedType(tag: DW_TAG_volatile_type, baseType: !9)
!16 = !{i32 7, !"Dwarf Version", i32 4}
!17 = !{i32 2, !"Debug Info Version", i32 3}
!18 = !{i32 1, !"wchar_size", i32 4}
!19 = !{!"Homebrew clang version 13.0.1"}
!20 = distinct !DISubprogram(name: "getglobal", scope: !3, file: !3, line: 6, type: !21, scopeLine: 7, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!21 = !DISubroutineType(types: !22)
!22 = !{!6}
!23 = !DILocation(line: 8, column: 5, scope: !20)
!24 = distinct !DISubprogram(name: "add1", scope: !3, file: !3, line: 12, type: !25, scopeLine: 12, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !27)
!25 = !DISubroutineType(types: !26)
!26 = !{!6, !6}
!27 = !{!28}
!28 = !DILocalVariable(name: "x", arg: 1, scope: !24, file: !3, line: 12, type: !6)
!29 = !DILocation(line: 0, scope: !24)
!30 = !DILocation(line: 12, column: 65, scope: !24)
!31 = !DILocation(line: 12, column: 56, scope: !24)
!32 = distinct !DISubprogram(name: "sub1", scope: !3, file: !3, line: 13, type: !25, scopeLine: 13, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !33)
!33 = !{!34}
!34 = !DILocalVariable(name: "x", arg: 1, scope: !32, file: !3, line: 13, type: !6)
!35 = !DILocation(line: 0, scope: !32)
!36 = !DILocation(line: 13, column: 65, scope: !32)
!37 = !DILocation(line: 13, column: 56, scope: !32)
!38 = distinct !DISubprogram(name: "test1", scope: !3, file: !3, line: 15, type: !39, scopeLine: 16, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !41)
!39 = !DISubroutineType(types: !40)
!40 = !{!11}
!41 = !{!42, !43}
!42 = !DILocalVariable(name: "a", scope: !38, file: !3, line: 17, type: !6)
!43 = !DILocalVariable(name: "b", scope: !38, file: !3, line: 17, type: !6)
!44 = !DILocation(line: 0, scope: !38)
!45 = !DILocation(line: 22, column: 28, scope: !38)
!46 = !DILocation(line: 22, column: 23, scope: !38)
!47 = !DILocation(line: 22, column: 6, scope: !38)
!48 = !DILocation(line: 22, column: 38, scope: !38)
!49 = !DILocation(line: 23, column: 35, scope: !38)
!50 = !DILocation(line: 23, column: 30, scope: !38)
!51 = !DILocation(line: 23, column: 13, scope: !38)
!52 = !DILocation(line: 23, column: 12, scope: !38)
!53 = !DILocation(line: 23, column: 45, scope: !38)
!54 = !{!"branch_weights", i32 1, i32 1048575}
!55 = !DILocation(line: 23, column: 5, scope: !38)
!56 = distinct !DISubprogram(name: "test2", scope: !3, file: !3, line: 26, type: !39, scopeLine: 27, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!57 = !DILocation(line: 28, column: 12, scope: !56)
!58 = !DILocation(line: 30, column: 13, scope: !56)
!59 = !DILocation(line: 32, column: 13, scope: !56)
!60 = !DILocation(line: 32, column: 12, scope: !56)
!61 = !DILocation(line: 32, column: 21, scope: !56)
!62 = !DILocation(line: 32, column: 5, scope: !56)
