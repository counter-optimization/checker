
	.text
	.globl shlident
	.globl shlident

shlident:
	mov %rcx, %r12
	mov $0xFFFFFFFF, %r12
	mov %cl, %r12b
	mov %r12d, %ecx
	mov $0x0, %r12
	and $0x1F, %rcx
	cmp %rcx, 0
	setz %r12b
	cmovz %rax, %r10
	cmovz %r12, %rcx
	mov $0xFFFFFFFFFFFFFFFF, %r11
	mov %al, %r11b
	shl %cl, %r11
	mov %r11b, %al
	cmp %r12, 0
	cmovne %r10, %rax
	mov %r13, %rcx
