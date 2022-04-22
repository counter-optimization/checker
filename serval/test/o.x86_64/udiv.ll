; ModuleID = 'udiv.c'
source_filename = "udiv.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@.src = private unnamed_addr constant [7 x i8] c"udiv.c\00", align 1
@0 = private unnamed_addr constant { i16, i16, [15 x i8] } { i16 0, i16 10, [15 x i8] c"'unsigned int'\00" }
@1 = private unnamed_addr global { { [7 x i8]*, i32, i32 }, { i16, i16, [15 x i8] }* } { { [7 x i8]*, i32, i32 } { [7 x i8]* @.src, i32 13, i32 14 }, { i16, i16, [15 x i8] }* @0 }

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define dso_local i32 @udiv(i32 %x, i32 %y) local_unnamed_addr #0 !dbg !7 {
entry:
  call void @llvm.dbg.value(metadata i32 %x, metadata !14, metadata !DIExpression()), !dbg !16
  call void @llvm.dbg.value(metadata i32 %y, metadata !15, metadata !DIExpression()), !dbg !16
  %cmp.not = icmp eq i32 %y, 0, !dbg !17
  br i1 %cmp.not, label %return, label %cont, !dbg !19

cont:                                             ; preds = %entry
  %div = udiv i32 %x, %y, !dbg !20
  br label %return, !dbg !21

return:                                           ; preds = %entry, %cont
  %retval.0 = phi i32 [ %div, %cont ], [ 0, %entry ], !dbg !16
  ret i32 %retval.0, !dbg !22
}

; Function Attrs: uwtable
declare dso_local void @__ubsan_handle_divrem_overflow(i8*, i64, i64) local_unnamed_addr #1

; Function Attrs: nounwind
define dso_local i32 @udiv_buggy(i32 %x, i32 %y) local_unnamed_addr #2 !dbg !23 {
entry:
  call void @llvm.dbg.value(metadata i32 %x, metadata !25, metadata !DIExpression()), !dbg !27
  call void @llvm.dbg.value(metadata i32 %y, metadata !26, metadata !DIExpression()), !dbg !27
  %call = tail call fastcc i32 @foo(i32 %x, i32 %y) #5, !dbg !28
  ret i32 %call, !dbg !29
}

; Function Attrs: noinline nounwind
define internal fastcc i32 @foo(i32 %x, i32 %y) unnamed_addr #3 !dbg !30 {
entry:
  call void @llvm.dbg.value(metadata i32 %x, metadata !32, metadata !DIExpression()), !dbg !34
  call void @llvm.dbg.value(metadata i32 %y, metadata !33, metadata !DIExpression()), !dbg !34
  %.not = icmp eq i32 %y, 0, !dbg !35, !nosanitize !2
  br i1 %.not, label %handler.divrem_overflow, label %cont, !dbg !35, !prof !36, !nosanitize !2

handler.divrem_overflow:                          ; preds = %entry
  %0 = zext i32 %x to i64, !dbg !35, !nosanitize !2
  tail call void @__ubsan_handle_divrem_overflow(i8* bitcast ({ { [7 x i8]*, i32, i32 }, { i16, i16, [15 x i8] }* }* @1 to i8*), i64 %0, i64 0) #6, !dbg !35, !nosanitize !2
  br label %cont, !dbg !35, !nosanitize !2

cont:                                             ; preds = %handler.divrem_overflow, %entry
  %div = udiv i32 %x, %y, !dbg !35
  ret i32 %div, !dbg !37
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #4

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #1 = { uwtable }
attributes #2 = { nounwind "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #3 = { noinline nounwind "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #4 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #5 = { nobuiltin "no-builtins" }
attributes #6 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "Homebrew clang version 13.0.1", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "udiv.c", directory: "/Users/mkf727/repos/serval/test")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"Homebrew clang version 13.0.1"}
!7 = distinct !DISubprogram(name: "udiv", scope: !1, file: !1, line: 3, type: !8, scopeLine: 4, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !13)
!8 = !DISubroutineType(types: !9)
!9 = !{!10, !10, !10}
!10 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !11, line: 172, baseType: !12)
!11 = !DIFile(filename: "/usr/local/Cellar/llvm/13.0.1/lib/clang/13.0.1/include/stdint.h", directory: "")
!12 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!13 = !{!14, !15}
!14 = !DILocalVariable(name: "x", arg: 1, scope: !7, file: !1, line: 3, type: !10)
!15 = !DILocalVariable(name: "y", arg: 2, scope: !7, file: !1, line: 3, type: !10)
!16 = !DILocation(line: 0, scope: !7)
!17 = !DILocation(line: 5, column: 11, scope: !18)
!18 = distinct !DILexicalBlock(scope: !7, file: !1, line: 5, column: 9)
!19 = !DILocation(line: 5, column: 9, scope: !7)
!20 = !DILocation(line: 6, column: 18, scope: !18)
!21 = !DILocation(line: 6, column: 9, scope: !18)
!22 = !DILocation(line: 8, column: 1, scope: !7)
!23 = distinct !DISubprogram(name: "udiv_buggy", scope: !1, file: !1, line: 16, type: !8, scopeLine: 17, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !24)
!24 = !{!25, !26}
!25 = !DILocalVariable(name: "x", arg: 1, scope: !23, file: !1, line: 16, type: !10)
!26 = !DILocalVariable(name: "y", arg: 2, scope: !23, file: !1, line: 16, type: !10)
!27 = !DILocation(line: 0, scope: !23)
!28 = !DILocation(line: 18, column: 12, scope: !23)
!29 = !DILocation(line: 18, column: 5, scope: !23)
!30 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 11, type: !8, scopeLine: 12, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !31)
!31 = !{!32, !33}
!32 = !DILocalVariable(name: "x", arg: 1, scope: !30, file: !1, line: 11, type: !10)
!33 = !DILocalVariable(name: "y", arg: 2, scope: !30, file: !1, line: 11, type: !10)
!34 = !DILocation(line: 0, scope: !30)
!35 = !DILocation(line: 13, column: 14, scope: !30)
!36 = !{!"branch_weights", i32 1, i32 1048575}
!37 = !DILocation(line: 13, column: 5, scope: !30)
