; ModuleID = 'global.c'
source_filename = "global.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@val = dso_local local_unnamed_addr global i64 0, align 8, !dbg !0
@vals = dso_local local_unnamed_addr global [4096 x i32] zeroinitializer, align 16, !dbg !6

; Function Attrs: mustprogress nofree norecurse nosync nounwind readonly willreturn
define dso_local i64 @get_value() local_unnamed_addr #0 !dbg !20 {
entry:
  %0 = load i64, i64* @val, align 8, !dbg !23
  ret i64 %0, !dbg !24
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readonly willreturn
define dso_local i32 @get_value_i(i64 %i) local_unnamed_addr #0 !dbg !25 {
entry:
  call void @llvm.dbg.value(metadata i64 %i, metadata !31, metadata !DIExpression()), !dbg !32
  %cmp = icmp ult i64 %i, 4096, !dbg !33
  br i1 %cmp, label %cond.true, label %cond.end, !dbg !34

cond.true:                                        ; preds = %entry
  %arrayidx = getelementptr inbounds [4096 x i32], [4096 x i32]* @vals, i64 0, i64 %i, !dbg !35
  %0 = load i32, i32* %arrayidx, align 4, !dbg !35
  br label %cond.end, !dbg !34

cond.end:                                         ; preds = %entry, %cond.true
  %cond = phi i32 [ %0, %cond.true ], [ -1, %entry ], !dbg !34
  ret i32 %cond, !dbg !36
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn writeonly
define dso_local void @set_value(i64 %x) local_unnamed_addr #1 !dbg !37 {
entry:
  call void @llvm.dbg.value(metadata i64 %x, metadata !41, metadata !DIExpression()), !dbg !42
  store i64 %x, i64* @val, align 8, !dbg !43
  ret void, !dbg !44
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn writeonly
define dso_local void @set_value_i(i64 %i, i32 %x) local_unnamed_addr #1 !dbg !45 {
entry:
  call void @llvm.dbg.value(metadata i64 %i, metadata !49, metadata !DIExpression()), !dbg !51
  call void @llvm.dbg.value(metadata i32 %x, metadata !50, metadata !DIExpression()), !dbg !51
  %cmp = icmp ult i64 %i, 4096, !dbg !52
  br i1 %cmp, label %if.then, label %if.end, !dbg !54

if.then:                                          ; preds = %entry
  %arrayidx = getelementptr inbounds [4096 x i32], [4096 x i32]* @vals, i64 0, i64 %i, !dbg !55
  store i32 %x, i32* %arrayidx, align 4, !dbg !56
  br label %if.end, !dbg !55

if.end:                                           ; preds = %if.then, %entry
  ret void, !dbg !57
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #2

attributes #0 = { mustprogress nofree norecurse nosync nounwind readonly willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #1 = { mustprogress nofree norecurse nosync nounwind willreturn writeonly "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #2 = { nofree nosync nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!16, !17, !18}
!llvm.ident = !{!19}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "val", scope: !2, file: !3, line: 6, type: !14, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "Homebrew clang version 13.0.1", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "global.c", directory: "/Users/mkf727/repos/serval/test")
!4 = !{}
!5 = !{!0, !6}
!6 = !DIGlobalVariableExpression(var: !7, expr: !DIExpression())
!7 = distinct !DIGlobalVariable(name: "vals", scope: !2, file: !3, line: 7, type: !8, isLocal: false, isDefinition: true)
!8 = !DICompositeType(tag: DW_TAG_array_type, baseType: !9, size: 131072, elements: !12)
!9 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !10, line: 172, baseType: !11)
!10 = !DIFile(filename: "/usr/local/Cellar/llvm/13.0.1/lib/clang/13.0.1/include/stdint.h", directory: "")
!11 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!12 = !{!13}
!13 = !DISubrange(count: 4096)
!14 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !10, line: 98, baseType: !15)
!15 = !DIBasicType(name: "long unsigned int", size: 64, encoding: DW_ATE_unsigned)
!16 = !{i32 7, !"Dwarf Version", i32 4}
!17 = !{i32 2, !"Debug Info Version", i32 3}
!18 = !{i32 1, !"wchar_size", i32 4}
!19 = !{!"Homebrew clang version 13.0.1"}
!20 = distinct !DISubprogram(name: "get_value", scope: !3, file: !3, line: 9, type: !21, scopeLine: 10, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!21 = !DISubroutineType(types: !22)
!22 = !{!14}
!23 = !DILocation(line: 11, column: 12, scope: !20)
!24 = !DILocation(line: 11, column: 5, scope: !20)
!25 = distinct !DISubprogram(name: "get_value_i", scope: !3, file: !3, line: 14, type: !26, scopeLine: 15, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !30)
!26 = !DISubroutineType(types: !27)
!27 = !{!9, !28}
!28 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", file: !29, line: 46, baseType: !15)
!29 = !DIFile(filename: "/usr/local/Cellar/llvm/13.0.1/lib/clang/13.0.1/include/stddef.h", directory: "")
!30 = !{!31}
!31 = !DILocalVariable(name: "i", arg: 1, scope: !25, file: !3, line: 14, type: !28)
!32 = !DILocation(line: 0, scope: !25)
!33 = !DILocation(line: 16, column: 15, scope: !25)
!34 = !DILocation(line: 16, column: 12, scope: !25)
!35 = !DILocation(line: 16, column: 22, scope: !25)
!36 = !DILocation(line: 16, column: 5, scope: !25)
!37 = distinct !DISubprogram(name: "set_value", scope: !3, file: !3, line: 19, type: !38, scopeLine: 20, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !40)
!38 = !DISubroutineType(types: !39)
!39 = !{null, !14}
!40 = !{!41}
!41 = !DILocalVariable(name: "x", arg: 1, scope: !37, file: !3, line: 19, type: !14)
!42 = !DILocation(line: 0, scope: !37)
!43 = !DILocation(line: 21, column: 9, scope: !37)
!44 = !DILocation(line: 22, column: 1, scope: !37)
!45 = distinct !DISubprogram(name: "set_value_i", scope: !3, file: !3, line: 24, type: !46, scopeLine: 25, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !48)
!46 = !DISubroutineType(types: !47)
!47 = !{null, !28, !9}
!48 = !{!49, !50}
!49 = !DILocalVariable(name: "i", arg: 1, scope: !45, file: !3, line: 24, type: !28)
!50 = !DILocalVariable(name: "x", arg: 2, scope: !45, file: !3, line: 24, type: !9)
!51 = !DILocation(line: 0, scope: !45)
!52 = !DILocation(line: 26, column: 11, scope: !53)
!53 = distinct !DILexicalBlock(scope: !45, file: !3, line: 26, column: 9)
!54 = !DILocation(line: 26, column: 9, scope: !45)
!55 = !DILocation(line: 27, column: 9, scope: !53)
!56 = !DILocation(line: 27, column: 17, scope: !53)
!57 = !DILocation(line: 28, column: 1, scope: !45)
