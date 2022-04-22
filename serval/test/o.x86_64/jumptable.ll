; ModuleID = 'jumptable.c'
source_filename = "jumptable.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@.src = private unnamed_addr constant [12 x i8] c"jumptable.c\00", align 1
@0 = private unnamed_addr constant { i16, i16, [7 x i8] } { i16 0, i16 13, [7 x i8] c"'long'\00" }
@1 = private unnamed_addr global { { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* } { { [12 x i8]*, i32, i32 } { [12 x i8]* @.src, i32 9, i32 29 }, { i16, i16, [7 x i8] }* @0 }
@2 = private unnamed_addr global { { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* } { { [12 x i8]*, i32, i32 } { [12 x i8]* @.src, i32 10, i32 29 }, { i16, i16, [7 x i8] }* @0 }
@3 = private unnamed_addr global { { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* } { { [12 x i8]*, i32, i32 } { [12 x i8]* @.src, i32 11, i32 29 }, { i16, i16, [7 x i8] }* @0 }
@4 = private unnamed_addr global { { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* } { { [12 x i8]*, i32, i32 } { [12 x i8]* @.src, i32 12, i32 29 }, { i16, i16, [7 x i8] }* @0 }
@5 = private unnamed_addr global { { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* } { { [12 x i8]*, i32, i32 } { [12 x i8]* @.src, i32 13, i32 29 }, { i16, i16, [7 x i8] }* @0 }
@6 = private unnamed_addr global { { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* } { { [12 x i8]*, i32, i32 } { [12 x i8]* @.src, i32 14, i32 29 }, { i16, i16, [7 x i8] }* @0 }
@7 = private unnamed_addr global { { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* } { { [12 x i8]*, i32, i32 } { [12 x i8]* @.src, i32 15, i32 29 }, { i16, i16, [7 x i8] }* @0 }
@table = dso_local local_unnamed_addr global [8 x i64 (i64)*] zeroinitializer, align 16, !dbg !0

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define dso_local i64 @add0(i64 returned %x) #0 !dbg !18 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !20, metadata !DIExpression()), !dbg !21
  ret i64 %x, !dbg !22
}

; Function Attrs: mustprogress nofree nosync nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: uwtable
declare dso_local void @__ubsan_handle_add_overflow(i8*, i64, i64) local_unnamed_addr #2

; Function Attrs: nounwind
define dso_local i64 @add1(i64 %x) #3 !dbg !23 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !25, metadata !DIExpression()), !dbg !26
  %0 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %x, i64 1), !dbg !27, !nosanitize !4
  %1 = extractvalue { i64, i1 } %0, 1, !dbg !27, !nosanitize !4
  br i1 %1, label %handler.add_overflow, label %cont, !dbg !27, !prof !28, !nosanitize !4

handler.add_overflow:                             ; preds = %entry
  tail call void @__ubsan_handle_add_overflow(i8* bitcast ({ { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* }* @1 to i8*), i64 %x, i64 1) #7, !dbg !27, !nosanitize !4
  br label %cont, !dbg !27, !nosanitize !4

cont:                                             ; preds = %handler.add_overflow, %entry
  %2 = extractvalue { i64, i1 } %0, 0, !dbg !27, !nosanitize !4
  ret i64 %2, !dbg !29
}

; Function Attrs: nounwind
define dso_local i64 @add2(i64 %x) #3 !dbg !30 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !32, metadata !DIExpression()), !dbg !33
  %0 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %x, i64 2), !dbg !34, !nosanitize !4
  %1 = extractvalue { i64, i1 } %0, 1, !dbg !34, !nosanitize !4
  br i1 %1, label %handler.add_overflow, label %cont, !dbg !34, !prof !28, !nosanitize !4

handler.add_overflow:                             ; preds = %entry
  tail call void @__ubsan_handle_add_overflow(i8* bitcast ({ { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* }* @2 to i8*), i64 %x, i64 2) #7, !dbg !34, !nosanitize !4
  br label %cont, !dbg !34, !nosanitize !4

cont:                                             ; preds = %handler.add_overflow, %entry
  %2 = extractvalue { i64, i1 } %0, 0, !dbg !34, !nosanitize !4
  ret i64 %2, !dbg !35
}

; Function Attrs: nounwind
define dso_local i64 @add3(i64 %x) #3 !dbg !36 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !38, metadata !DIExpression()), !dbg !39
  %0 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %x, i64 3), !dbg !40, !nosanitize !4
  %1 = extractvalue { i64, i1 } %0, 1, !dbg !40, !nosanitize !4
  br i1 %1, label %handler.add_overflow, label %cont, !dbg !40, !prof !28, !nosanitize !4

handler.add_overflow:                             ; preds = %entry
  tail call void @__ubsan_handle_add_overflow(i8* bitcast ({ { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* }* @3 to i8*), i64 %x, i64 3) #7, !dbg !40, !nosanitize !4
  br label %cont, !dbg !40, !nosanitize !4

cont:                                             ; preds = %handler.add_overflow, %entry
  %2 = extractvalue { i64, i1 } %0, 0, !dbg !40, !nosanitize !4
  ret i64 %2, !dbg !41
}

; Function Attrs: nounwind
define dso_local i64 @add4(i64 %x) #3 !dbg !42 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !44, metadata !DIExpression()), !dbg !45
  %0 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %x, i64 4), !dbg !46, !nosanitize !4
  %1 = extractvalue { i64, i1 } %0, 1, !dbg !46, !nosanitize !4
  br i1 %1, label %handler.add_overflow, label %cont, !dbg !46, !prof !28, !nosanitize !4

handler.add_overflow:                             ; preds = %entry
  tail call void @__ubsan_handle_add_overflow(i8* bitcast ({ { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* }* @4 to i8*), i64 %x, i64 4) #7, !dbg !46, !nosanitize !4
  br label %cont, !dbg !46, !nosanitize !4

cont:                                             ; preds = %handler.add_overflow, %entry
  %2 = extractvalue { i64, i1 } %0, 0, !dbg !46, !nosanitize !4
  ret i64 %2, !dbg !47
}

; Function Attrs: nounwind
define dso_local i64 @add5(i64 %x) #3 !dbg !48 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !50, metadata !DIExpression()), !dbg !51
  %0 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %x, i64 5), !dbg !52, !nosanitize !4
  %1 = extractvalue { i64, i1 } %0, 1, !dbg !52, !nosanitize !4
  br i1 %1, label %handler.add_overflow, label %cont, !dbg !52, !prof !28, !nosanitize !4

handler.add_overflow:                             ; preds = %entry
  tail call void @__ubsan_handle_add_overflow(i8* bitcast ({ { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* }* @5 to i8*), i64 %x, i64 5) #7, !dbg !52, !nosanitize !4
  br label %cont, !dbg !52, !nosanitize !4

cont:                                             ; preds = %handler.add_overflow, %entry
  %2 = extractvalue { i64, i1 } %0, 0, !dbg !52, !nosanitize !4
  ret i64 %2, !dbg !53
}

; Function Attrs: nounwind
define dso_local i64 @add6(i64 %x) #3 !dbg !54 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !56, metadata !DIExpression()), !dbg !57
  %0 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %x, i64 6), !dbg !58, !nosanitize !4
  %1 = extractvalue { i64, i1 } %0, 1, !dbg !58, !nosanitize !4
  br i1 %1, label %handler.add_overflow, label %cont, !dbg !58, !prof !28, !nosanitize !4

handler.add_overflow:                             ; preds = %entry
  tail call void @__ubsan_handle_add_overflow(i8* bitcast ({ { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* }* @6 to i8*), i64 %x, i64 6) #7, !dbg !58, !nosanitize !4
  br label %cont, !dbg !58, !nosanitize !4

cont:                                             ; preds = %handler.add_overflow, %entry
  %2 = extractvalue { i64, i1 } %0, 0, !dbg !58, !nosanitize !4
  ret i64 %2, !dbg !59
}

; Function Attrs: nounwind
define dso_local i64 @add7(i64 %x) #3 !dbg !60 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !62, metadata !DIExpression()), !dbg !63
  %0 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %x, i64 7), !dbg !64, !nosanitize !4
  %1 = extractvalue { i64, i1 } %0, 1, !dbg !64, !nosanitize !4
  br i1 %1, label %handler.add_overflow, label %cont, !dbg !64, !prof !28, !nosanitize !4

handler.add_overflow:                             ; preds = %entry
  tail call void @__ubsan_handle_add_overflow(i8* bitcast ({ { [12 x i8]*, i32, i32 }, { i16, i16, [7 x i8] }* }* @7 to i8*), i64 %x, i64 7) #7, !dbg !64, !nosanitize !4
  br label %cont, !dbg !64, !nosanitize !4

cont:                                             ; preds = %handler.add_overflow, %entry
  %2 = extractvalue { i64, i1 } %0, 0, !dbg !64, !nosanitize !4
  ret i64 %2, !dbg !65
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn writeonly
define dso_local void @init_table() local_unnamed_addr #4 !dbg !66 {
entry:
  store i64 (i64)* @add0, i64 (i64)** getelementptr inbounds ([8 x i64 (i64)*], [8 x i64 (i64)*]* @table, i64 0, i64 0), align 16, !dbg !69
  store i64 (i64)* @add1, i64 (i64)** getelementptr inbounds ([8 x i64 (i64)*], [8 x i64 (i64)*]* @table, i64 0, i64 1), align 8, !dbg !70
  store i64 (i64)* @add2, i64 (i64)** getelementptr inbounds ([8 x i64 (i64)*], [8 x i64 (i64)*]* @table, i64 0, i64 2), align 16, !dbg !71
  store i64 (i64)* @add3, i64 (i64)** getelementptr inbounds ([8 x i64 (i64)*], [8 x i64 (i64)*]* @table, i64 0, i64 3), align 8, !dbg !72
  store i64 (i64)* @add4, i64 (i64)** getelementptr inbounds ([8 x i64 (i64)*], [8 x i64 (i64)*]* @table, i64 0, i64 4), align 16, !dbg !73
  store i64 (i64)* @add5, i64 (i64)** getelementptr inbounds ([8 x i64 (i64)*], [8 x i64 (i64)*]* @table, i64 0, i64 5), align 8, !dbg !74
  store i64 (i64)* @add6, i64 (i64)** getelementptr inbounds ([8 x i64 (i64)*], [8 x i64 (i64)*]* @table, i64 0, i64 6), align 16, !dbg !75
  store i64 (i64)* @add7, i64 (i64)** getelementptr inbounds ([8 x i64 (i64)*], [8 x i64 (i64)*]* @table, i64 0, i64 7), align 8, !dbg !76
  ret void, !dbg !77
}

; Function Attrs: naked noinline nounwind
define dso_local void @mret() #5 !dbg !78 {
entry:
  tail call void asm sideeffect "mret", "~{dirflag},~{fpsr},~{flags}"() #7, !dbg !79, !srcloc !80
  unreachable, !dbg !81
}

; Function Attrs: nounwind
define dso_local i64 @call_func(i64 %x, i64 %y) local_unnamed_addr #3 !dbg !82 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !87, metadata !DIExpression()), !dbg !89
  call void @llvm.dbg.value(metadata i64 %y, metadata !88, metadata !DIExpression()), !dbg !89
  %and = and i64 %y, 7, !dbg !90
  call void @llvm.dbg.value(metadata i64 %and, metadata !88, metadata !DIExpression()), !dbg !89
  %arrayidx = getelementptr inbounds [8 x i64 (i64)*], [8 x i64 (i64)*]* @table, i64 0, i64 %and, !dbg !91
  %0 = load i64 (i64)*, i64 (i64)** %arrayidx, align 8, !dbg !91
  %call = tail call i64 %0(i64 %x) #8, !dbg !91
  ret i64 %call, !dbg !92
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #6

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #1 = { mustprogress nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { uwtable }
attributes #3 = { nounwind "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #4 = { mustprogress nofree norecurse nosync nounwind willreturn writeonly "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #5 = { naked noinline nounwind "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #6 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #7 = { nounwind }
attributes #8 = { nobuiltin nounwind "no-builtins" }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!14, !15, !16}
!llvm.ident = !{!17}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "table", scope: !2, file: !3, line: 6, type: !6, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "Homebrew clang version 13.0.1", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "jumptable.c", directory: "/Users/mkf727/repos/serval/test")
!4 = !{}
!5 = !{!0}
!6 = !DICompositeType(tag: DW_TAG_array_type, baseType: !7, size: 512, elements: !12)
!7 = !DIDerivedType(tag: DW_TAG_typedef, name: "function", file: !3, line: 4, baseType: !8)
!8 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !9, size: 64)
!9 = !DISubroutineType(types: !10)
!10 = !{!11, !11}
!11 = !DIBasicType(name: "long int", size: 64, encoding: DW_ATE_signed)
!12 = !{!13}
!13 = !DISubrange(count: 8)
!14 = !{i32 7, !"Dwarf Version", i32 4}
!15 = !{i32 2, !"Debug Info Version", i32 3}
!16 = !{i32 1, !"wchar_size", i32 4}
!17 = !{!"Homebrew clang version 13.0.1"}
!18 = distinct !DISubprogram(name: "add0", scope: !3, file: !3, line: 8, type: !9, scopeLine: 8, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !19)
!19 = !{!20}
!20 = !DILocalVariable(name: "x", arg: 1, scope: !18, file: !3, line: 8, type: !11)
!21 = !DILocation(line: 0, scope: !18)
!22 = !DILocation(line: 8, column: 20, scope: !18)
!23 = distinct !DISubprogram(name: "add1", scope: !3, file: !3, line: 9, type: !9, scopeLine: 9, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !24)
!24 = !{!25}
!25 = !DILocalVariable(name: "x", arg: 1, scope: !23, file: !3, line: 9, type: !11)
!26 = !DILocation(line: 0, scope: !23)
!27 = !DILocation(line: 9, column: 29, scope: !23)
!28 = !{!"branch_weights", i32 1, i32 1048575}
!29 = !DILocation(line: 9, column: 20, scope: !23)
!30 = distinct !DISubprogram(name: "add2", scope: !3, file: !3, line: 10, type: !9, scopeLine: 10, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !31)
!31 = !{!32}
!32 = !DILocalVariable(name: "x", arg: 1, scope: !30, file: !3, line: 10, type: !11)
!33 = !DILocation(line: 0, scope: !30)
!34 = !DILocation(line: 10, column: 29, scope: !30)
!35 = !DILocation(line: 10, column: 20, scope: !30)
!36 = distinct !DISubprogram(name: "add3", scope: !3, file: !3, line: 11, type: !9, scopeLine: 11, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !37)
!37 = !{!38}
!38 = !DILocalVariable(name: "x", arg: 1, scope: !36, file: !3, line: 11, type: !11)
!39 = !DILocation(line: 0, scope: !36)
!40 = !DILocation(line: 11, column: 29, scope: !36)
!41 = !DILocation(line: 11, column: 20, scope: !36)
!42 = distinct !DISubprogram(name: "add4", scope: !3, file: !3, line: 12, type: !9, scopeLine: 12, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !43)
!43 = !{!44}
!44 = !DILocalVariable(name: "x", arg: 1, scope: !42, file: !3, line: 12, type: !11)
!45 = !DILocation(line: 0, scope: !42)
!46 = !DILocation(line: 12, column: 29, scope: !42)
!47 = !DILocation(line: 12, column: 20, scope: !42)
!48 = distinct !DISubprogram(name: "add5", scope: !3, file: !3, line: 13, type: !9, scopeLine: 13, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !49)
!49 = !{!50}
!50 = !DILocalVariable(name: "x", arg: 1, scope: !48, file: !3, line: 13, type: !11)
!51 = !DILocation(line: 0, scope: !48)
!52 = !DILocation(line: 13, column: 29, scope: !48)
!53 = !DILocation(line: 13, column: 20, scope: !48)
!54 = distinct !DISubprogram(name: "add6", scope: !3, file: !3, line: 14, type: !9, scopeLine: 14, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !55)
!55 = !{!56}
!56 = !DILocalVariable(name: "x", arg: 1, scope: !54, file: !3, line: 14, type: !11)
!57 = !DILocation(line: 0, scope: !54)
!58 = !DILocation(line: 14, column: 29, scope: !54)
!59 = !DILocation(line: 14, column: 20, scope: !54)
!60 = distinct !DISubprogram(name: "add7", scope: !3, file: !3, line: 15, type: !9, scopeLine: 15, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !61)
!61 = !{!62}
!62 = !DILocalVariable(name: "x", arg: 1, scope: !60, file: !3, line: 15, type: !11)
!63 = !DILocation(line: 0, scope: !60)
!64 = !DILocation(line: 15, column: 29, scope: !60)
!65 = !DILocation(line: 15, column: 20, scope: !60)
!66 = distinct !DISubprogram(name: "init_table", scope: !3, file: !3, line: 18, type: !67, scopeLine: 19, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!67 = !DISubroutineType(types: !68)
!68 = !{null}
!69 = !DILocation(line: 20, column: 18, scope: !66)
!70 = !DILocation(line: 21, column: 18, scope: !66)
!71 = !DILocation(line: 22, column: 18, scope: !66)
!72 = !DILocation(line: 23, column: 18, scope: !66)
!73 = !DILocation(line: 24, column: 18, scope: !66)
!74 = !DILocation(line: 25, column: 18, scope: !66)
!75 = !DILocation(line: 26, column: 18, scope: !66)
!76 = !DILocation(line: 27, column: 18, scope: !66)
!77 = !DILocation(line: 28, column: 1, scope: !66)
!78 = distinct !DISubprogram(name: "mret", scope: !3, file: !3, line: 30, type: !67, scopeLine: 31, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!79 = !DILocation(line: 32, column: 9, scope: !78)
!80 = !{i64 632}
!81 = !DILocation(line: 33, column: 1, scope: !78)
!82 = distinct !DISubprogram(name: "call_func", scope: !3, file: !3, line: 35, type: !83, scopeLine: 35, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !86)
!83 = !DISubroutineType(types: !84)
!84 = !{!11, !11, !85}
!85 = !DIBasicType(name: "long unsigned int", size: 64, encoding: DW_ATE_unsigned)
!86 = !{!87, !88}
!87 = !DILocalVariable(name: "x", arg: 1, scope: !82, file: !3, line: 35, type: !11)
!88 = !DILocalVariable(name: "y", arg: 2, scope: !82, file: !3, line: 35, type: !85)
!89 = !DILocation(line: 0, scope: !82)
!90 = !DILocation(line: 36, column: 15, scope: !82)
!91 = !DILocation(line: 37, column: 16, scope: !82)
!92 = !DILocation(line: 37, column: 9, scope: !82)
