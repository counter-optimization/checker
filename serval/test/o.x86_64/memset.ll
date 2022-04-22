; ModuleID = 'memset.c'
source_filename = "memset.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.A = type { i32, %struct.B }
%struct.B = type { i32, i32 }

@buffer = dso_local global [10 x i8] zeroinitializer, align 1, !dbg !0
@a = dso_local global [10 x %struct.A] zeroinitializer, align 16, !dbg !6
@pages = dso_local global [10 x [4096 x i8]] zeroinitializer, align 16, !dbg !20

; Function Attrs: naked noinline nounwind
define dso_local void @mret() #0 !dbg !33 {
entry:
  tail call void asm sideeffect "mret", "~{dirflag},~{fpsr},~{flags}"() #4, !dbg !36, !srcloc !37
  unreachable, !dbg !38
}

; Function Attrs: nofree noinline norecurse nosync nounwind writeonly
define dso_local i8* @memset(i8* returned %p, i32 %c, i64 %len) local_unnamed_addr #1 !dbg !39 {
entry:
  call void @llvm.dbg.value(metadata i8* %p, metadata !47, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i32 %c, metadata !48, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i64 %len, metadata !49, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i8* %p, metadata !50, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i64 0, metadata !53, metadata !DIExpression()), !dbg !54
  %conv = trunc i32 %c to i8
  call void @llvm.dbg.value(metadata i64 0, metadata !53, metadata !DIExpression()), !dbg !54
  %cmp6.not = icmp eq i64 %len, 0, !dbg !55
  br i1 %cmp6.not, label %for.end, label %for.body.preheader, !dbg !58

for.body.preheader:                               ; preds = %entry
  %0 = add i64 %len, -1, !dbg !58
  %xtraiter = and i64 %len, 7, !dbg !58
  %1 = icmp ult i64 %0, 7, !dbg !58
  br i1 %1, label %for.end.loopexit.unr-lcssa, label %for.body.preheader.new, !dbg !58

for.body.preheader.new:                           ; preds = %for.body.preheader
  %unroll_iter = and i64 %len, -8, !dbg !58
  br label %for.body, !dbg !58

for.body:                                         ; preds = %for.body, %for.body.preheader.new
  %i.07 = phi i64 [ 0, %for.body.preheader.new ], [ %inc.7, %for.body ]
  %niter = phi i64 [ %unroll_iter, %for.body.preheader.new ], [ %niter.nsub.7, %for.body ]
  call void @llvm.dbg.value(metadata i64 %i.07, metadata !53, metadata !DIExpression()), !dbg !54
  %arrayidx = getelementptr inbounds i8, i8* %p, i64 %i.07, !dbg !59
  store i8 %conv, i8* %arrayidx, align 1, !dbg !60
  %inc = or i64 %i.07, 1, !dbg !61
  call void @llvm.dbg.value(metadata i64 %inc, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i64 %inc, metadata !53, metadata !DIExpression()), !dbg !54
  %arrayidx.1 = getelementptr inbounds i8, i8* %p, i64 %inc, !dbg !59
  store i8 %conv, i8* %arrayidx.1, align 1, !dbg !60
  %inc.1 = or i64 %i.07, 2, !dbg !61
  call void @llvm.dbg.value(metadata i64 %inc.1, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i64 %inc.1, metadata !53, metadata !DIExpression()), !dbg !54
  %arrayidx.2 = getelementptr inbounds i8, i8* %p, i64 %inc.1, !dbg !59
  store i8 %conv, i8* %arrayidx.2, align 1, !dbg !60
  %inc.2 = or i64 %i.07, 3, !dbg !61
  call void @llvm.dbg.value(metadata i64 %inc.2, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i64 %inc.2, metadata !53, metadata !DIExpression()), !dbg !54
  %arrayidx.3 = getelementptr inbounds i8, i8* %p, i64 %inc.2, !dbg !59
  store i8 %conv, i8* %arrayidx.3, align 1, !dbg !60
  %inc.3 = or i64 %i.07, 4, !dbg !61
  call void @llvm.dbg.value(metadata i64 %inc.3, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i64 %inc.3, metadata !53, metadata !DIExpression()), !dbg !54
  %arrayidx.4 = getelementptr inbounds i8, i8* %p, i64 %inc.3, !dbg !59
  store i8 %conv, i8* %arrayidx.4, align 1, !dbg !60
  %inc.4 = or i64 %i.07, 5, !dbg !61
  call void @llvm.dbg.value(metadata i64 %inc.4, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i64 %inc.4, metadata !53, metadata !DIExpression()), !dbg !54
  %arrayidx.5 = getelementptr inbounds i8, i8* %p, i64 %inc.4, !dbg !59
  store i8 %conv, i8* %arrayidx.5, align 1, !dbg !60
  %inc.5 = or i64 %i.07, 6, !dbg !61
  call void @llvm.dbg.value(metadata i64 %inc.5, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i64 %inc.5, metadata !53, metadata !DIExpression()), !dbg !54
  %arrayidx.6 = getelementptr inbounds i8, i8* %p, i64 %inc.5, !dbg !59
  store i8 %conv, i8* %arrayidx.6, align 1, !dbg !60
  %inc.6 = or i64 %i.07, 7, !dbg !61
  call void @llvm.dbg.value(metadata i64 %inc.6, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i64 %inc.6, metadata !53, metadata !DIExpression()), !dbg !54
  %arrayidx.7 = getelementptr inbounds i8, i8* %p, i64 %inc.6, !dbg !59
  store i8 %conv, i8* %arrayidx.7, align 1, !dbg !60
  %inc.7 = add nuw i64 %i.07, 8, !dbg !61
  call void @llvm.dbg.value(metadata i64 %inc.7, metadata !53, metadata !DIExpression()), !dbg !54
  %niter.nsub.7 = add i64 %niter, -8, !dbg !58
  %niter.ncmp.7 = icmp eq i64 %niter.nsub.7, 0, !dbg !58
  br i1 %niter.ncmp.7, label %for.end.loopexit.unr-lcssa, label %for.body, !dbg !58, !llvm.loop !62

for.end.loopexit.unr-lcssa:                       ; preds = %for.body, %for.body.preheader
  %i.07.unr = phi i64 [ 0, %for.body.preheader ], [ %inc.7, %for.body ]
  %lcmp.mod.not = icmp eq i64 %xtraiter, 0, !dbg !58
  br i1 %lcmp.mod.not, label %for.end, label %for.body.epil, !dbg !58

for.body.epil:                                    ; preds = %for.end.loopexit.unr-lcssa, %for.body.epil
  %i.07.epil = phi i64 [ %inc.epil, %for.body.epil ], [ %i.07.unr, %for.end.loopexit.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %for.body.epil ], [ %xtraiter, %for.end.loopexit.unr-lcssa ]
  call void @llvm.dbg.value(metadata i64 %i.07.epil, metadata !53, metadata !DIExpression()), !dbg !54
  %arrayidx.epil = getelementptr inbounds i8, i8* %p, i64 %i.07.epil, !dbg !59
  store i8 %conv, i8* %arrayidx.epil, align 1, !dbg !60
  %inc.epil = add nuw i64 %i.07.epil, 1, !dbg !61
  call void @llvm.dbg.value(metadata i64 %inc.epil, metadata !53, metadata !DIExpression()), !dbg !54
  %epil.iter.sub = add i64 %epil.iter, -1, !dbg !58
  %epil.iter.cmp.not = icmp eq i64 %epil.iter.sub, 0, !dbg !58
  br i1 %epil.iter.cmp.not, label %for.end, label %for.body.epil, !dbg !58, !llvm.loop !65

for.end:                                          ; preds = %for.end.loopexit.unr-lcssa, %for.body.epil, %entry
  ret i8* %p, !dbg !67
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_byte_buffer() local_unnamed_addr #2 !dbg !68 {
entry:
  %call = tail call i8* @memset(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @buffer, i64 0, i64 0), i32 0, i64 10) #5, !dbg !69
  ret void, !dbg !70
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_a0() local_unnamed_addr #2 !dbg !71 {
entry:
  %call = tail call i8* @memset(i8* bitcast ([10 x %struct.A]* @a to i8*), i32 0, i64 12) #5, !dbg !72
  ret void, !dbg !73
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_ai(i64 %i) local_unnamed_addr #2 !dbg !74 {
entry:
  call void @llvm.dbg.value(metadata i64 %i, metadata !78, metadata !DIExpression()), !dbg !79
  %cmp = icmp ult i64 %i, 10, !dbg !80
  br i1 %cmp, label %if.then, label %if.end, !dbg !82

if.then:                                          ; preds = %entry
  %arrayidx = getelementptr inbounds [10 x %struct.A], [10 x %struct.A]* @a, i64 0, i64 %i, !dbg !83
  %0 = bitcast %struct.A* %arrayidx to i8*, !dbg !84
  %call = tail call i8* @memset(i8* nonnull %0, i32 0, i64 12) #5, !dbg !85
  br label %if.end, !dbg !85

if.end:                                           ; preds = %if.then, %entry
  ret void, !dbg !86
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_an() local_unnamed_addr #2 !dbg !87 {
entry:
  %call = tail call i8* @memset(i8* bitcast ([10 x %struct.A]* @a to i8*), i32 0, i64 120) #5, !dbg !88
  ret void, !dbg !89
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_b(i64 %i) local_unnamed_addr #2 !dbg !90 {
entry:
  call void @llvm.dbg.value(metadata i64 %i, metadata !92, metadata !DIExpression()), !dbg !93
  %cmp = icmp ult i64 %i, 10, !dbg !94
  br i1 %cmp, label %if.then, label %if.end, !dbg !96

if.then:                                          ; preds = %entry
  %b = getelementptr inbounds [10 x %struct.A], [10 x %struct.A]* @a, i64 0, i64 %i, i32 1, !dbg !97
  %0 = bitcast %struct.B* %b to i8*, !dbg !98
  %call = tail call i8* @memset(i8* nonnull %0, i32 0, i64 8) #5, !dbg !99
  br label %if.end, !dbg !99

if.end:                                           ; preds = %if.then, %entry
  ret void, !dbg !100
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_b_z(i64 %i) local_unnamed_addr #2 !dbg !101 {
entry:
  call void @llvm.dbg.value(metadata i64 %i, metadata !103, metadata !DIExpression()), !dbg !104
  %cmp = icmp ult i64 %i, 10, !dbg !105
  br i1 %cmp, label %if.then, label %if.end, !dbg !107

if.then:                                          ; preds = %entry
  %z = getelementptr inbounds [10 x %struct.A], [10 x %struct.A]* @a, i64 0, i64 %i, i32 1, i32 1, !dbg !108
  %0 = bitcast i32* %z to i8*, !dbg !109
  %call = tail call i8* @memset(i8* nonnull %0, i32 0, i64 4) #5, !dbg !110
  br label %if.end, !dbg !110

if.end:                                           ; preds = %if.then, %entry
  ret void, !dbg !111
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_pages(i64 %lower, i64 %upper) local_unnamed_addr #2 !dbg !112 {
entry:
  call void @llvm.dbg.value(metadata i64 %lower, metadata !116, metadata !DIExpression()), !dbg !118
  call void @llvm.dbg.value(metadata i64 %upper, metadata !117, metadata !DIExpression()), !dbg !118
  %cmp = icmp uge i64 %upper, %lower, !dbg !119
  %cmp1 = icmp ult i64 %upper, 11
  %or.cond = and i1 %cmp, %cmp1, !dbg !121
  br i1 %or.cond, label %if.then, label %if.end, !dbg !121

if.then:                                          ; preds = %entry
  %0 = getelementptr inbounds [10 x [4096 x i8]], [10 x [4096 x i8]]* @pages, i64 0, i64 %lower, i64 0, !dbg !122
  %sub = sub i64 %upper, %lower, !dbg !123
  %mul = shl i64 %sub, 12, !dbg !124
  %call = tail call i8* @memset(i8* nonnull %0, i32 0, i64 %mul) #5, !dbg !125
  br label %if.end, !dbg !125

if.end:                                           ; preds = %if.then, %entry
  ret void, !dbg !126
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_buggy_too_large_a() local_unnamed_addr #2 !dbg !127 {
entry:
  %call = tail call i8* @memset(i8* bitcast ([10 x %struct.A]* @a to i8*), i32 0, i64 132) #5, !dbg !128
  ret void, !dbg !129
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_buggy_too_large_b() local_unnamed_addr #2 !dbg !130 {
entry:
  %call = tail call i8* @memset(i8* bitcast (i32* getelementptr inbounds ([10 x %struct.A], [10 x %struct.A]* @a, i64 0, i64 0, i32 1, i32 1) to i8*), i32 0, i64 8) #5, !dbg !131
  ret void, !dbg !132
}

; Function Attrs: nofree norecurse nosync nounwind writeonly
define dso_local void @test_buggy_out_of_bounds(i64 %i) local_unnamed_addr #2 !dbg !133 {
entry:
  call void @llvm.dbg.value(metadata i64 %i, metadata !135, metadata !DIExpression()), !dbg !136
  %arrayidx = getelementptr inbounds [10 x %struct.A], [10 x %struct.A]* @a, i64 0, i64 %i, !dbg !137
  %0 = bitcast %struct.A* %arrayidx to i8*, !dbg !138
  %call = tail call i8* @memset(i8* nonnull %0, i32 0, i64 12) #5, !dbg !139
  ret void, !dbg !140
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #3

attributes #0 = { naked noinline nounwind "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #1 = { nofree noinline norecurse nosync nounwind writeonly "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #2 = { nofree norecurse nosync nounwind writeonly "frame-pointer"="none" "min-legal-vector-width"="0" "no-builtins" "no-jump-tables"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+x87,-aes,-avx,-avx2,-avx512bf16,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vp2intersect,-avx512vpopcntdq,-avxvnni,-f16c,-fma,-fma4,-gfni,-kl,-pclmul,-sha,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-sse4a,-ssse3,-vaes,-vpclmulqdq,-widekl,-xop" "tune-cpu"="generic" }
attributes #3 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #4 = { nounwind }
attributes #5 = { nobuiltin "no-builtins" }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!29, !30, !31}
!llvm.ident = !{!32}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "buffer", scope: !2, file: !3, line: 16, type: !28, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "Homebrew clang version 13.0.1", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "memset.c", directory: "/Users/mkf727/repos/serval/test")
!4 = !{}
!5 = !{!6, !20, !0}
!6 = !DIGlobalVariableExpression(var: !7, expr: !DIExpression())
!7 = distinct !DIGlobalVariable(name: "a", scope: !2, file: !3, line: 13, type: !8, isLocal: false, isDefinition: true)
!8 = !DICompositeType(tag: DW_TAG_array_type, baseType: !9, size: 960, elements: !18)
!9 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !3, line: 6, size: 96, elements: !10)
!10 = !{!11, !13}
!11 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !9, file: !3, line: 7, baseType: !12, size: 32)
!12 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!13 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !9, file: !3, line: 10, baseType: !14, size: 64, offset: 32)
!14 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "B", file: !3, line: 8, size: 64, elements: !15)
!15 = !{!16, !17}
!16 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !14, file: !3, line: 9, baseType: !12, size: 32)
!17 = !DIDerivedType(tag: DW_TAG_member, name: "z", scope: !14, file: !3, line: 9, baseType: !12, size: 32, offset: 32)
!18 = !{!19}
!19 = !DISubrange(count: 10)
!20 = !DIGlobalVariableExpression(var: !21, expr: !DIExpression())
!21 = distinct !DIGlobalVariable(name: "pages", scope: !2, file: !3, line: 15, type: !22, isLocal: false, isDefinition: true)
!22 = !DICompositeType(tag: DW_TAG_array_type, baseType: !23, size: 327680, elements: !26)
!23 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !24, line: 226, baseType: !25)
!24 = !DIFile(filename: "/usr/local/Cellar/llvm/13.0.1/lib/clang/13.0.1/include/stdint.h", directory: "")
!25 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!26 = !{!19, !27}
!27 = !DISubrange(count: 4096)
!28 = !DICompositeType(tag: DW_TAG_array_type, baseType: !23, size: 80, elements: !18)
!29 = !{i32 7, !"Dwarf Version", i32 4}
!30 = !{i32 2, !"Debug Info Version", i32 3}
!31 = !{i32 1, !"wchar_size", i32 4}
!32 = !{!"Homebrew clang version 13.0.1"}
!33 = distinct !DISubprogram(name: "mret", scope: !3, file: !3, line: 19, type: !34, scopeLine: 20, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!34 = !DISubroutineType(types: !35)
!35 = !{null}
!36 = !DILocation(line: 21, column: 5, scope: !33)
!37 = !{i64 243}
!38 = !DILocation(line: 22, column: 1, scope: !33)
!39 = distinct !DISubprogram(name: "memset", scope: !3, file: !3, line: 25, type: !40, scopeLine: 26, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !46)
!40 = !DISubroutineType(types: !41)
!41 = !{!42, !42, !12, !43}
!42 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", file: !44, line: 46, baseType: !45)
!44 = !DIFile(filename: "/usr/local/Cellar/llvm/13.0.1/lib/clang/13.0.1/include/stddef.h", directory: "")
!45 = !DIBasicType(name: "long unsigned int", size: 64, encoding: DW_ATE_unsigned)
!46 = !{!47, !48, !49, !50, !53}
!47 = !DILocalVariable(name: "p", arg: 1, scope: !39, file: !3, line: 25, type: !42)
!48 = !DILocalVariable(name: "c", arg: 2, scope: !39, file: !3, line: 25, type: !12)
!49 = !DILocalVariable(name: "len", arg: 3, scope: !39, file: !3, line: 25, type: !43)
!50 = !DILocalVariable(name: "s", scope: !39, file: !3, line: 27, type: !51)
!51 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !52, size: 64)
!52 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!53 = !DILocalVariable(name: "i", scope: !39, file: !3, line: 28, type: !43)
!54 = !DILocation(line: 0, scope: !39)
!55 = !DILocation(line: 30, column: 19, scope: !56)
!56 = distinct !DILexicalBlock(scope: !57, file: !3, line: 30, column: 5)
!57 = distinct !DILexicalBlock(scope: !39, file: !3, line: 30, column: 5)
!58 = !DILocation(line: 30, column: 5, scope: !57)
!59 = !DILocation(line: 31, column: 9, scope: !56)
!60 = !DILocation(line: 31, column: 14, scope: !56)
!61 = !DILocation(line: 30, column: 26, scope: !56)
!62 = distinct !{!62, !58, !63, !64}
!63 = !DILocation(line: 31, column: 16, scope: !57)
!64 = !{!"llvm.loop.mustprogress"}
!65 = distinct !{!65, !66}
!66 = !{!"llvm.loop.unroll.disable"}
!67 = !DILocation(line: 33, column: 5, scope: !39)
!68 = distinct !DISubprogram(name: "test_byte_buffer", scope: !3, file: !3, line: 36, type: !34, scopeLine: 37, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!69 = !DILocation(line: 38, column: 5, scope: !68)
!70 = !DILocation(line: 39, column: 1, scope: !68)
!71 = distinct !DISubprogram(name: "test_a0", scope: !3, file: !3, line: 41, type: !34, scopeLine: 42, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!72 = !DILocation(line: 43, column: 5, scope: !71)
!73 = !DILocation(line: 44, column: 1, scope: !71)
!74 = distinct !DISubprogram(name: "test_ai", scope: !3, file: !3, line: 46, type: !75, scopeLine: 47, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !77)
!75 = !DISubroutineType(types: !76)
!76 = !{null, !43}
!77 = !{!78}
!78 = !DILocalVariable(name: "i", arg: 1, scope: !74, file: !3, line: 46, type: !43)
!79 = !DILocation(line: 0, scope: !74)
!80 = !DILocation(line: 48, column: 11, scope: !81)
!81 = distinct !DILexicalBlock(scope: !74, file: !3, line: 48, column: 9)
!82 = !DILocation(line: 48, column: 9, scope: !74)
!83 = !DILocation(line: 49, column: 17, scope: !81)
!84 = !DILocation(line: 49, column: 16, scope: !81)
!85 = !DILocation(line: 49, column: 9, scope: !81)
!86 = !DILocation(line: 50, column: 1, scope: !74)
!87 = distinct !DISubprogram(name: "test_an", scope: !3, file: !3, line: 52, type: !34, scopeLine: 53, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!88 = !DILocation(line: 54, column: 5, scope: !87)
!89 = !DILocation(line: 55, column: 1, scope: !87)
!90 = distinct !DISubprogram(name: "test_b", scope: !3, file: !3, line: 57, type: !75, scopeLine: 58, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !91)
!91 = !{!92}
!92 = !DILocalVariable(name: "i", arg: 1, scope: !90, file: !3, line: 57, type: !43)
!93 = !DILocation(line: 0, scope: !90)
!94 = !DILocation(line: 59, column: 11, scope: !95)
!95 = distinct !DILexicalBlock(scope: !90, file: !3, line: 59, column: 9)
!96 = !DILocation(line: 59, column: 9, scope: !90)
!97 = !DILocation(line: 60, column: 22, scope: !95)
!98 = !DILocation(line: 60, column: 16, scope: !95)
!99 = !DILocation(line: 60, column: 9, scope: !95)
!100 = !DILocation(line: 61, column: 1, scope: !90)
!101 = distinct !DISubprogram(name: "test_b_z", scope: !3, file: !3, line: 63, type: !75, scopeLine: 64, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !102)
!102 = !{!103}
!103 = !DILocalVariable(name: "i", arg: 1, scope: !101, file: !3, line: 63, type: !43)
!104 = !DILocation(line: 0, scope: !101)
!105 = !DILocation(line: 65, column: 11, scope: !106)
!106 = distinct !DILexicalBlock(scope: !101, file: !3, line: 65, column: 9)
!107 = !DILocation(line: 65, column: 9, scope: !101)
!108 = !DILocation(line: 66, column: 24, scope: !106)
!109 = !DILocation(line: 66, column: 16, scope: !106)
!110 = !DILocation(line: 66, column: 9, scope: !106)
!111 = !DILocation(line: 67, column: 1, scope: !101)
!112 = distinct !DISubprogram(name: "test_pages", scope: !3, file: !3, line: 69, type: !113, scopeLine: 70, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !115)
!113 = !DISubroutineType(types: !114)
!114 = !{null, !43, !43}
!115 = !{!116, !117}
!116 = !DILocalVariable(name: "lower", arg: 1, scope: !112, file: !3, line: 69, type: !43)
!117 = !DILocalVariable(name: "upper", arg: 2, scope: !112, file: !3, line: 69, type: !43)
!118 = !DILocation(line: 0, scope: !112)
!119 = !DILocation(line: 71, column: 15, scope: !120)
!120 = distinct !DILexicalBlock(scope: !112, file: !3, line: 71, column: 9)
!121 = !DILocation(line: 71, column: 24, scope: !120)
!122 = !DILocation(line: 72, column: 16, scope: !120)
!123 = !DILocation(line: 72, column: 41, scope: !120)
!124 = !DILocation(line: 72, column: 50, scope: !120)
!125 = !DILocation(line: 72, column: 9, scope: !120)
!126 = !DILocation(line: 73, column: 1, scope: !112)
!127 = distinct !DISubprogram(name: "test_buggy_too_large_a", scope: !3, file: !3, line: 75, type: !34, scopeLine: 76, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!128 = !DILocation(line: 77, column: 5, scope: !127)
!129 = !DILocation(line: 78, column: 1, scope: !127)
!130 = distinct !DISubprogram(name: "test_buggy_too_large_b", scope: !3, file: !3, line: 80, type: !34, scopeLine: 81, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!131 = !DILocation(line: 82, column: 5, scope: !130)
!132 = !DILocation(line: 83, column: 1, scope: !130)
!133 = distinct !DISubprogram(name: "test_buggy_out_of_bounds", scope: !3, file: !3, line: 85, type: !75, scopeLine: 86, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !134)
!134 = !{!135}
!135 = !DILocalVariable(name: "i", arg: 1, scope: !133, file: !3, line: 85, type: !43)
!136 = !DILocation(line: 0, scope: !133)
!137 = !DILocation(line: 87, column: 13, scope: !133)
!138 = !DILocation(line: 87, column: 12, scope: !133)
!139 = !DILocation(line: 87, column: 5, scope: !133)
!140 = !DILocation(line: 88, column: 1, scope: !133)
