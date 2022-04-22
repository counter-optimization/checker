; ModuleID = 'bext.c'
source_filename = "bext.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@.src = private unnamed_addr constant [7 x i8] c"bext.c\00", align 1
@0 = private unnamed_addr constant { i16, i16, [35 x i8] } { i16 0, i16 10, [35 x i8] c"'uint_xlen_t' (aka 'unsigned int')\00" }
@1 = private unnamed_addr constant { i16, i16, [6 x i8] } { i16 0, i16 11, [6 x i8] c"'int'\00" }
@2 = private unnamed_addr global { { [7 x i8]*, i32, i32 }, { i16, i16, [35 x i8] }*, { i16, i16, [6 x i8] }* } { { [7 x i8]*, i32, i32 } { [7 x i8]* @.src, i32 13, i32 37 }, { i16, i16, [35 x i8] }* @0, { i16, i16, [6 x i8] }* @1 }
@3 = private unnamed_addr global { { [7 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* } { { [7 x i8]*, i32, i32 } { [7 x i8]* @.src, i32 14, i32 14 }, { i16, i16, [6 x i8] }* @1 }

; Function Attrs: nounwind
define dso_local i32 @bext(i32 %rs1, i32 %rs2) local_unnamed_addr #0 !dbg !12 {
entry:
  call void @llvm.dbg.value(metadata i32 %rs1, metadata !16, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.value(metadata i32 %rs2, metadata !17, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.value(metadata i32 0, metadata !18, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.value(metadata i32 0, metadata !19, metadata !DIExpression()), !dbg !24
  call void @llvm.dbg.value(metadata i32 0, metadata !22, metadata !DIExpression()), !dbg !24
  br label %cont, !dbg !25

for.cond.cleanup:                                 ; preds = %for.inc
  ret i32 %r.2, !dbg !26

cont:                                             ; preds = %entry, %for.inc
  %j.024 = phi i32 [ 0, %entry ], [ %j.1, %for.inc ]
  %i.023 = phi i32 [ 0, %entry ], [ %9, %for.inc ]
  %r.022 = phi i32 [ 0, %entry ], [ %r.2, %for.inc ]
  call void @llvm.dbg.value(metadata i32 %j.024, metadata !22, metadata !DIExpression()), !dbg !24
  call void @llvm.dbg.value(metadata i32 %i.023, metadata !19, metadata !DIExpression()), !dbg !24
  call void @llvm.dbg.value(metadata i32 %r.022, metadata !18, metadata !DIExpression()), !dbg !23
  %0 = shl nuw i32 1, %i.023, !dbg !27
  %1 = and i32 %0, %rs2, !dbg !27
  %tobool.not = icmp eq i32 %1, 0, !dbg !27
  br i1 %tobool.not, label %for.inc, label %cont2, !dbg !30

cont2:                                            ; preds = %cont
  %2 = and i32 %0, %rs1, !dbg !31
  %tobool5.not = icmp eq i32 %2, 0, !dbg !31
  br i1 %tobool5.not, label %if.end, label %if.then6, !dbg !34

if.then6:                                         ; preds = %cont2
  %3 = icmp ult i32 %j.024, 32, !dbg !35
  br i1 %3, label %cont8, label %handler.shift_out_of_bounds7, !dbg !35, !prof !36, !nosanitize !2

handler.shift_out_of_bounds7:                     ; preds = %if.then6
  %4 = zext i32 %j.024 to i64, !dbg !35, !nosanitize !2
  tail call void @__ubsan_handle_shift_out_of_bounds(i8* bitcast ({ { [7 x i8]*, i32, i32 }, { i16, i16, [35 x i8] }*, { i16, i16, [6 x i8] }* }* @2 to i8*), i64 1, i64 %4) #4, !dbg !35, !nosanitize !2
  br label %cont8, !dbg !35, !nosanitize !2

cont8:                                            ; preds = %handler.shift_out_of_bounds7, %if.then6
  %shl = shl nuw i32 1, %j.024, !dbg !35
  %or = or i32 %shl, %r.022, !dbg !37
  call void @llvm.dbg.value(metadata i32 %or, metadata !18, metadata !DIExpression()), !dbg !23
  br label %if.end, !dbg !38

if.end:                                           ; preds = %cont8, %cont2
  %r.1 = phi i32 [ %or, %cont8 ], [ %r.022, %cont2 ], !dbg !23
  call void @llvm.dbg.value(metadata i32 %r.1, metadata !18, metadata !DIExpression()), !dbg !23
  %5 = tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %j.024, i32 1), !dbg !39, !nosanitize !2
  %6 = extractvalue { i32, i1 } %5, 0, !dbg !39, !nosanitize !2
  %7 = extractvalue { i32, i1 } %5, 1, !dbg !39, !nosanitize !2
  br i1 %7, label %handler.add_overflow, label %for.inc, !dbg !39, !prof !40, !nosanitize !2

handler.add_overflow:                             ; preds = %if.end
  %8 = zext i32 %j.024 to i64, !dbg !39, !nosanitize !2
  tail call void @__ubsan_handle_add_overflow(i8* bitcast ({ { [7 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* }* @3 to i8*), i64 %8, i64 1) #4, !dbg !39, !nosanitize !2
  br label %for.inc, !dbg !39, !nosanitize !2

for.inc:                                          ; preds = %if.end, %handler.add_overflow, %cont
  %r.2 = phi i32 [ %r.022, %cont ], [ %r.1, %handler.add_overflow ], [ %r.1, %if.end ], !dbg !23
  %j.1 = phi i32 [ %j.024, %cont ], [ %6, %handler.add_overflow ], [ %6, %if.end ], !dbg !24
  call void @llvm.dbg.value(metadata i32 %j.1, metadata !22, metadata !DIExpression()), !dbg !24
  call void @llvm.dbg.value(metadata i32 %r.2, metadata !18, metadata !DIExpression()), !dbg !23
  %9 = add nuw nsw i32 %i.023, 1, !dbg !41
  call void @llvm.dbg.value(metadata i32 %9, metadata !19, metadata !DIExpression()), !dbg !24
  %exitcond.not = icmp eq i32 %9, 32, !dbg !42
  br i1 %exitcond.not, label %for.cond.cleanup, label %cont, !dbg !25, !llvm.loop !43
}

; Function Attrs: uwtable
declare dso_local void @__ubsan_handle_shift_out_of_bounds(i8*, i64, i64) local_unnamed_addr #1

; Function Attrs: mustprogress nofree nosync nounwind readnone speculatable willreturn
declare { i32, i1 } @llvm.sadd.with.overflow.i32(i32, i32) #2

; Function Attrs: uwtable
declare dso_local void @__ubsan_handle_add_overflow(i8*, i64, i64) local_unnamed_addr #1

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #3

attributes #0 = { nounwind "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #1 = { uwtable }
attributes #2 = { mustprogress nofree nosync nounwind readnone speculatable willreturn }
attributes #3 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #4 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!8, !9, !10}
!llvm.ident = !{!11}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "Homebrew clang version 13.0.1", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, retainedTypes: !3, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "bext.c", directory: "/Users/mkf727/repos/serval/test")
!2 = !{}
!3 = !{!4}
!4 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_xlen_t", file: !1, line: 3, baseType: !5)
!5 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !6, line: 172, baseType: !7)
!6 = !DIFile(filename: "/usr/local/Cellar/llvm/13.0.1/lib/clang/13.0.1/include/stdint.h", directory: "")
!7 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!8 = !{i32 7, !"Dwarf Version", i32 4}
!9 = !{i32 2, !"Debug Info Version", i32 3}
!10 = !{i32 1, !"wchar_size", i32 4}
!11 = !{!"Homebrew clang version 13.0.1"}
!12 = distinct !DISubprogram(name: "bext", scope: !1, file: !1, line: 7, type: !13, scopeLine: 8, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !15)
!13 = !DISubroutineType(types: !14)
!14 = !{!4, !4, !4}
!15 = !{!16, !17, !18, !19, !22}
!16 = !DILocalVariable(name: "rs1", arg: 1, scope: !12, file: !1, line: 7, type: !4)
!17 = !DILocalVariable(name: "rs2", arg: 2, scope: !12, file: !1, line: 7, type: !4)
!18 = !DILocalVariable(name: "r", scope: !12, file: !1, line: 9, type: !4)
!19 = !DILocalVariable(name: "i", scope: !20, file: !1, line: 10, type: !21)
!20 = distinct !DILexicalBlock(scope: !12, file: !1, line: 10, column: 5)
!21 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!22 = !DILocalVariable(name: "j", scope: !20, file: !1, line: 10, type: !21)
!23 = !DILocation(line: 0, scope: !12)
!24 = !DILocation(line: 0, scope: !20)
!25 = !DILocation(line: 10, column: 5, scope: !20)
!26 = !DILocation(line: 16, column: 5, scope: !12)
!27 = !DILocation(line: 11, column: 24, scope: !28)
!28 = distinct !DILexicalBlock(scope: !29, file: !1, line: 11, column: 13)
!29 = distinct !DILexicalBlock(scope: !20, file: !1, line: 10, column: 5)
!30 = !DILocation(line: 11, column: 13, scope: !29)
!31 = !DILocation(line: 12, column: 28, scope: !32)
!32 = distinct !DILexicalBlock(scope: !33, file: !1, line: 12, column: 17)
!33 = distinct !DILexicalBlock(scope: !28, file: !1, line: 11, column: 29)
!34 = !DILocation(line: 12, column: 17, scope: !33)
!35 = !DILocation(line: 13, column: 37, scope: !32)
!36 = !{!"branch_weights", i32 1048575, i32 1}
!37 = !DILocation(line: 13, column: 19, scope: !32)
!38 = !DILocation(line: 13, column: 17, scope: !32)
!39 = !DILocation(line: 14, column: 14, scope: !33)
!40 = !{!"branch_weights", i32 1, i32 1048575}
!41 = !DILocation(line: 10, column: 39, scope: !29)
!42 = !DILocation(line: 10, column: 30, scope: !29)
!43 = distinct !{!43, !25, !44, !45}
!44 = !DILocation(line: 15, column: 9, scope: !20)
!45 = !{!"llvm.loop.mustprogress"}
