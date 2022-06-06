	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movl	$0, -4(%rbp)
	movl	%edi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$-1, -20(%rbp)
	movw	$0, -22(%rbp)
	movb	$-1, -23(%rbp)
	movq	$0, -32(%rbp)
	movl	-20(%rbp), %ecx
	movw	-22(%rbp), %dx
	movb	-23(%rbp), %sil
	## InlineAsm Start
	xorq	%rax, %rax
	movl	%ecx, %eax
	movw	%dx, %ax
	movb	%sil, %al
	movq	%rax, %rcx

	## InlineAsm End
	movq	%rcx, -40(%rbp)                 ## 8-byte Spill
	movq	-40(%rbp), %rax                 ## 8-byte Reload
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rsi
	leaq	L_.str(%rip), %rdi
	movb	$0, %al
	callq	_printf
	xorl	%eax, %eax
	addq	$48, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"result is: %#016llx\n"

.subsections_via_symbols
