; ModuleID = 'array.c'
source_filename = "array.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.A = type { i32, i32 }

@as = dso_local local_unnamed_addr global [4 x %struct.A] zeroinitializer, align 16, !dbg !0
@arr = dso_local local_unnamed_addr global [4 x i32] zeroinitializer, align 16, !dbg !6

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn writeonly
define dso_local i32 @test(i32 %x) local_unnamed_addr #0 !dbg !23 {
entry:
  call void @llvm.dbg.value(metadata i32 %x, metadata !27, metadata !DIExpression()), !dbg !29
  %rem = and i32 %x, 3, !dbg !30
  call void @llvm.dbg.value(metadata i32 %x, metadata !28, metadata !DIExpression(DW_OP_constu, 3, DW_OP_and, DW_OP_stack_value)), !dbg !29
  %idxprom = zext i32 %rem to i64, !dbg !31
  %y = getelementptr inbounds [4 x %struct.A], [4 x %struct.A]* @as, i64 0, i64 %idxprom, i32 1, !dbg !32
  store i32 9, i32* %y, align 4, !dbg !33
  %add = add i32 %x, 1, !dbg !34
  %rem6 = and i32 %add, 3, !dbg !35
  %idxprom7 = zext i32 %rem6 to i64, !dbg !36
  %arrayidx8 = getelementptr inbounds [4 x i32], [4 x i32]* @arr, i64 0, i64 %idxprom7, !dbg !36
  store i32 9, i32* %arrayidx8, align 4, !dbg !37
  ret i32 9, !dbg !38
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn writeonly "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!19, !20, !21}
!llvm.ident = !{!22}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "as", scope: !2, file: !3, line: 3, type: !14, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "Homebrew clang version 13.0.1", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "array.c", directory: "/Users/mkf727/repos/serval/test")
!4 = !{}
!5 = !{!0, !6}
!6 = !DIGlobalVariableExpression(var: !7, expr: !DIExpression())
!7 = distinct !DIGlobalVariable(name: "arr", scope: !2, file: !3, line: 4, type: !8, isLocal: false, isDefinition: true)
!8 = !DICompositeType(tag: DW_TAG_array_type, baseType: !9, size: 128, elements: !12)
!9 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !10, line: 172, baseType: !11)
!10 = !DIFile(filename: "/usr/local/Cellar/llvm/13.0.1/lib/clang/13.0.1/include/stdint.h", directory: "")
!11 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!12 = !{!13}
!13 = !DISubrange(count: 4)
!14 = !DICompositeType(tag: DW_TAG_array_type, baseType: !15, size: 256, elements: !12)
!15 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !3, line: 3, size: 64, elements: !16)
!16 = !{!17, !18}
!17 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !15, file: !3, line: 3, baseType: !9, size: 32)
!18 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !15, file: !3, line: 3, baseType: !9, size: 32, offset: 32)
!19 = !{i32 7, !"Dwarf Version", i32 4}
!20 = !{i32 2, !"Debug Info Version", i32 3}
!21 = !{i32 1, !"wchar_size", i32 4}
!22 = !{!"Homebrew clang version 13.0.1"}
!23 = distinct !DISubprogram(name: "test", scope: !3, file: !3, line: 6, type: !24, scopeLine: 7, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !26)
!24 = !DISubroutineType(types: !25)
!25 = !{!9, !9}
!26 = !{!27, !28}
!27 = !DILocalVariable(name: "x", arg: 1, scope: !23, file: !3, line: 6, type: !9)
!28 = !DILocalVariable(name: "idx", scope: !23, file: !3, line: 8, type: !9)
!29 = !DILocation(line: 0, scope: !23)
!30 = !DILocation(line: 8, column: 22, scope: !23)
!31 = !DILocation(line: 10, column: 5, scope: !23)
!32 = !DILocation(line: 10, column: 13, scope: !23)
!33 = !DILocation(line: 10, column: 15, scope: !23)
!34 = !DILocation(line: 11, column: 14, scope: !23)
!35 = !DILocation(line: 11, column: 19, scope: !23)
!36 = !DILocation(line: 11, column: 5, scope: !23)
!37 = !DILocation(line: 11, column: 24, scope: !23)
!38 = !DILocation(line: 12, column: 5, scope: !23)
