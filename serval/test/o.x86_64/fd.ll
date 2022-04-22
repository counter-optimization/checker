; ModuleID = 'fd.c'
source_filename = "fd.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.proc = type { [16 x i64] }
%struct.file = type { i64 }

@procs = dso_local local_unnamed_addr global [64 x %struct.proc] zeroinitializer, align 16, !dbg !0
@current = dso_local local_unnamed_addr global i64 0, align 8, !dbg !17
@files = dso_local local_unnamed_addr global [128 x %struct.file] zeroinitializer, align 16, !dbg !6

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define dso_local i32 @close(i64 %fd) local_unnamed_addr #0 !dbg !32 {
entry:
  call void @llvm.dbg.value(metadata i64 %fd, metadata !38, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata !DIArgList([64 x %struct.proc]* @procs, i64 undef), metadata !39, metadata !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_arg, 1, DW_OP_constu, 128, DW_OP_mul, DW_OP_plus, DW_OP_stack_value)), !dbg !44
  %0 = icmp ugt i64 %fd, 15, !dbg !45
  br i1 %0, label %cleanup, label %if.end, !dbg !45

if.end:                                           ; preds = %entry
  %1 = load i64, i64* @current, align 8, !dbg !47
  call void @llvm.dbg.value(metadata !DIArgList([64 x %struct.proc]* @procs, i64 %1), metadata !39, metadata !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_arg, 1, DW_OP_constu, 128, DW_OP_mul, DW_OP_plus, DW_OP_stack_value)), !dbg !44
  call void @llvm.dbg.value(metadata !DIArgList([64 x %struct.proc]* @procs, i64 %1), metadata !39, metadata !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_arg, 1, DW_OP_constu, 128, DW_OP_mul, DW_OP_plus, DW_OP_stack_value)), !dbg !44
  %arrayidx2 = getelementptr inbounds [64 x %struct.proc], [64 x %struct.proc]* @procs, i64 0, i64 %1, i32 0, i64 %fd, !dbg !48
  %2 = load i64, i64* %arrayidx2, align 8, !dbg !48
  call void @llvm.dbg.value(metadata i64 %2, metadata !41, metadata !DIExpression()), !dbg !44
  %cmp3 = icmp ugt i64 %2, 127, !dbg !49
  br i1 %cmp3, label %cleanup, label %if.end5, !dbg !51

if.end5:                                          ; preds = %if.end
  call void @llvm.dbg.value(metadata !DIArgList([128 x %struct.file]* @files, i64 %2), metadata !42, metadata !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_arg, 1, DW_OP_constu, 8, DW_OP_mul, DW_OP_plus, DW_OP_stack_value)), !dbg !44
  %refcount = getelementptr inbounds [128 x %struct.file], [128 x %struct.file]* @files, i64 0, i64 %2, i32 0, !dbg !52
  %3 = load i64, i64* %refcount, align 8, !dbg !53
  %dec = add i64 %3, -1, !dbg !53
  store i64 %dec, i64* %refcount, align 8, !dbg !53
  br label %cleanup, !dbg !54

cleanup:                                          ; preds = %if.end, %entry, %if.end5
  %retval.0 = phi i32 [ 0, %if.end5 ], [ -1, %entry ], [ -1, %if.end ], !dbg !44
  ret i32 %retval.0, !dbg !55
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!28, !29, !30}
!llvm.ident = !{!31}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "procs", scope: !2, file: !3, line: 12, type: !19, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "Homebrew clang version 13.0.1", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "fd.c", directory: "/Users/mkf727/repos/serval/test")
!4 = !{}
!5 = !{!0, !6, !17}
!6 = !DIGlobalVariableExpression(var: !7, expr: !DIExpression())
!7 = distinct !DIGlobalVariable(name: "files", scope: !2, file: !3, line: 13, type: !8, isLocal: false, isDefinition: true)
!8 = !DICompositeType(tag: DW_TAG_array_type, baseType: !9, size: 8192, elements: !15)
!9 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "file", file: !3, line: 8, size: 64, elements: !10)
!10 = !{!11}
!11 = !DIDerivedType(tag: DW_TAG_member, name: "refcount", scope: !9, file: !3, line: 9, baseType: !12, size: 64)
!12 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !13, line: 98, baseType: !14)
!13 = !DIFile(filename: "/usr/local/Cellar/llvm/13.0.1/lib/clang/13.0.1/include/stdint.h", directory: "")
!14 = !DIBasicType(name: "long unsigned int", size: 64, encoding: DW_ATE_unsigned)
!15 = !{!16}
!16 = !DISubrange(count: 128)
!17 = !DIGlobalVariableExpression(var: !18, expr: !DIExpression())
!18 = distinct !DIGlobalVariable(name: "current", scope: !2, file: !3, line: 14, type: !12, isLocal: false, isDefinition: true)
!19 = !DICompositeType(tag: DW_TAG_array_type, baseType: !20, size: 65536, elements: !26)
!20 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "proc", file: !3, line: 4, size: 1024, elements: !21)
!21 = !{!22}
!22 = !DIDerivedType(tag: DW_TAG_member, name: "fds", scope: !20, file: !3, line: 5, baseType: !23, size: 1024)
!23 = !DICompositeType(tag: DW_TAG_array_type, baseType: !12, size: 1024, elements: !24)
!24 = !{!25}
!25 = !DISubrange(count: 16)
!26 = !{!27}
!27 = !DISubrange(count: 64)
!28 = !{i32 7, !"Dwarf Version", i32 4}
!29 = !{i32 2, !"Debug Info Version", i32 3}
!30 = !{i32 1, !"wchar_size", i32 4}
!31 = !{!"Homebrew clang version 13.0.1"}
!32 = distinct !DISubprogram(name: "close", scope: !3, file: !3, line: 16, type: !33, scopeLine: 17, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !37)
!33 = !DISubroutineType(types: !34)
!34 = !{!35, !36}
!35 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!36 = !DIBasicType(name: "long int", size: 64, encoding: DW_ATE_signed)
!37 = !{!38, !39, !41, !42}
!38 = !DILocalVariable(name: "fd", arg: 1, scope: !32, file: !3, line: 16, type: !36)
!39 = !DILocalVariable(name: "proc", scope: !32, file: !3, line: 18, type: !40)
!40 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !20, size: 64)
!41 = !DILocalVariable(name: "fileid", scope: !32, file: !3, line: 19, type: !12)
!42 = !DILocalVariable(name: "file", scope: !32, file: !3, line: 20, type: !43)
!43 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !9, size: 64)
!44 = !DILocation(line: 0, scope: !32)
!45 = !DILocation(line: 23, column: 16, scope: !46)
!46 = distinct !DILexicalBlock(scope: !32, file: !3, line: 23, column: 9)
!47 = !DILocation(line: 22, column: 19, scope: !32)
!48 = !DILocation(line: 25, column: 14, scope: !32)
!49 = !DILocation(line: 26, column: 16, scope: !50)
!50 = distinct !DILexicalBlock(scope: !32, file: !3, line: 26, column: 9)
!51 = !DILocation(line: 26, column: 9, scope: !32)
!52 = !DILocation(line: 29, column: 13, scope: !32)
!53 = !DILocation(line: 29, column: 5, scope: !32)
!54 = !DILocation(line: 30, column: 5, scope: !32)
!55 = !DILocation(line: 31, column: 1, scope: !32)
