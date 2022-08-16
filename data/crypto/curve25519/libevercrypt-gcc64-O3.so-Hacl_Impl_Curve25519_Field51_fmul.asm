
data/crypto//curve25519/libevercrypt-gcc64-O3.so:	file format elf64-x86-64


Disassembly of section .text:

0000000000041910 <Hacl_Impl_Curve25519_Field51_fmul>:
   41910: f3 0f 1e fa                  	endbr64
   41914: 41 57                        	pushq	%r15
   41916: 41 56                        	pushq	%r14
   41918: 41 55                        	pushq	%r13
   4191a: 41 54                        	pushq	%r12
   4191c: 55                           	pushq	%rbp
   4191d: 53                           	pushq	%rbx
   4191e: 4c 8b 3e                     	movq	(%rsi), %r15
   41921: 4c 8b 76 08                  	movq	8(%rsi), %r14
   41925: 48 8b 6e 20                  	movq	32(%rsi), %rbp
   41929: 4c 8b 6e 10                  	movq	16(%rsi), %r13
   4192d: 4c 8b 66 18                  	movq	24(%rsi), %r12
   41931: 48 8b 02                     	movq	(%rdx), %rax
   41934: 48 8b 72 10                  	movq	16(%rdx), %rsi
   41938: 48 8b 5a 18                  	movq	24(%rdx), %rbx
   4193c: 48 8b 4a 08                  	movq	8(%rdx), %rcx
   41940: 4c 89 64 24 c0               	movq	%r12, -64(%rsp)
   41945: 48 89 44 24 b0               	movq	%rax, -80(%rsp)
   4194a: 48 8b 52 20                  	movq	32(%rdx), %rdx
   4194e: 48 8d 04 f6                  	leaq	(%rsi,%rsi,8), %rax
   41952: 4c 8d 0c 46                  	leaq	(%rsi,%rax,2), %r9
   41956: 48 8d 04 db                  	leaq	(%rbx,%rbx,8), %rax
   4195a: 4c 89 6c 24 b8               	movq	%r13, -72(%rsp)
   4195f: 4c 8d 04 43                  	leaq	(%rbx,%rax,2), %r8
   41963: 48 8d 04 d2                  	leaq	(%rdx,%rdx,8), %rax
   41967: 48 89 54 24 d0               	movq	%rdx, -48(%rsp)
   4196c: 48 8d 14 42                  	leaq	(%rdx,%rax,2), %rdx
   41970: 48 89 d0                     	movq	%rdx, %rax
   41973: 48 89 54 24 c8               	movq	%rdx, -56(%rsp)
   41978: 49 f7 e6                     	mulq	%r14
   4197b: 49 89 c2                     	movq	%rax, %r10
   4197e: 48 8b 44 24 b0               	movq	-80(%rsp), %rax
   41983: 49 89 d3                     	movq	%rdx, %r11
   41986: 49 f7 e7                     	mulq	%r15
   41989: 49 01 c2                     	addq	%rax, %r10
   4198c: 4c 89 e8                     	movq	%r13, %rax
   4198f: 49 11 d3                     	adcq	%rdx, %r11
   41992: 49 f7 e0                     	mulq	%r8
   41995: 49 01 c2                     	addq	%rax, %r10
   41998: 4c 89 e0                     	movq	%r12, %rax
   4199b: 49 11 d3                     	adcq	%rdx, %r11
   4199e: 49 f7 e1                     	mulq	%r9
   419a1: 49 01 c2                     	addq	%rax, %r10
   419a4: 48 8d 04 c9                  	leaq	(%rcx,%rcx,8), %rax
   419a8: 48 8d 04 41                  	leaq	(%rcx,%rax,2), %rax
   419ac: 49 11 d3                     	adcq	%rdx, %r11
   419af: 48 f7 e5                     	mulq	%rbp
   419b2: 49 01 c2                     	addq	%rax, %r10
   419b5: 48 8b 44 24 b0               	movq	-80(%rsp), %rax
   419ba: 49 11 d3                     	adcq	%rdx, %r11
   419bd: 4d 89 d4                     	movq	%r10, %r12
   419c0: 49 f7 e6                     	mulq	%r14
   419c3: 4d 89 dd                     	movq	%r11, %r13
   419c6: 4c 89 64 24 d8               	movq	%r12, -40(%rsp)
   419cb: 4c 89 6c 24 e0               	movq	%r13, -32(%rsp)
   419d0: 49 89 c2                     	movq	%rax, %r10
   419d3: 4c 89 f8                     	movq	%r15, %rax
   419d6: 49 89 d3                     	movq	%rdx, %r11
   419d9: 48 f7 e1                     	mulq	%rcx
   419dc: 49 01 c2                     	addq	%rax, %r10
   419df: 48 8b 44 24 c8               	movq	-56(%rsp), %rax
   419e4: 49 11 d3                     	adcq	%rdx, %r11
   419e7: 48 f7 64 24 b8               	mulq	-72(%rsp)
   419ec: 49 01 c2                     	addq	%rax, %r10
   419ef: 48 8b 44 24 c0               	movq	-64(%rsp), %rax
   419f4: 49 11 d3                     	adcq	%rdx, %r11
   419f7: 49 f7 e0                     	mulq	%r8
   419fa: 49 01 c2                     	addq	%rax, %r10
   419fd: 4c 89 c8                     	movq	%r9, %rax
   41a00: 49 11 d3                     	adcq	%rdx, %r11
   41a03: 48 f7 e5                     	mulq	%rbp
   41a06: 4c 01 d0                     	addq	%r10, %rax
   41a09: 4c 11 da                     	adcq	%r11, %rdx
   41a0c: 4d 0f ac ec 33               	shrdq	$51, %r13, %r12
   41a11: 45 31 db                     	xorl	%r11d, %r11d
   41a14: 4c 01 e0                     	addq	%r12, %rax
   41a17: 4c 11 da                     	adcq	%r11, %rdx
   41a1a: 49 89 c4                     	movq	%rax, %r12
   41a1d: 48 89 c8                     	movq	%rcx, %rax
   41a20: 49 89 d5                     	movq	%rdx, %r13
   41a23: 49 f7 e6                     	mulq	%r14
   41a26: 49 89 c2                     	movq	%rax, %r10
   41a29: 4c 89 f8                     	movq	%r15, %rax
   41a2c: 49 89 d3                     	movq	%rdx, %r11
   41a2f: 48 f7 e6                     	mulq	%rsi
   41a32: 49 01 c2                     	addq	%rax, %r10
   41a35: 48 8b 44 24 b0               	movq	-80(%rsp), %rax
   41a3a: 49 11 d3                     	adcq	%rdx, %r11
   41a3d: 48 f7 64 24 b8               	mulq	-72(%rsp)
   41a42: 49 01 c2                     	addq	%rax, %r10
   41a45: 48 8b 44 24 c8               	movq	-56(%rsp), %rax
   41a4a: 49 11 d3                     	adcq	%rdx, %r11
   41a4d: 48 f7 64 24 c0               	mulq	-64(%rsp)
   41a52: 4c 89 64 24 e8               	movq	%r12, -24(%rsp)
   41a57: 4c 89 6c 24 f0               	movq	%r13, -16(%rsp)
   41a5c: 49 01 c2                     	addq	%rax, %r10
   41a5f: 4c 89 c0                     	movq	%r8, %rax
   41a62: 4d 89 e0                     	movq	%r12, %r8
   41a65: 49 11 d3                     	adcq	%rdx, %r11
   41a68: 48 f7 e5                     	mulq	%rbp
   41a6b: 4c 01 d0                     	addq	%r10, %rax
   41a6e: 4c 11 da                     	adcq	%r11, %rdx
   41a71: 4d 0f ac e8 33               	shrdq	$51, %r13, %r8
   41a76: 45 31 ed                     	xorl	%r13d, %r13d
   41a79: 4d 89 c4                     	movq	%r8, %r12
   41a7c: 49 01 c4                     	addq	%rax, %r12
   41a7f: 48 89 f0                     	movq	%rsi, %rax
   41a82: 49 11 d5                     	adcq	%rdx, %r13
   41a85: 49 f7 e6                     	mulq	%r14
   41a88: 4d 89 e2                     	movq	%r12, %r10
   41a8b: 49 89 c0                     	movq	%rax, %r8
   41a8e: 4c 89 f8                     	movq	%r15, %rax
   41a91: 49 89 d1                     	movq	%rdx, %r9
   41a94: 48 f7 e3                     	mulq	%rbx
   41a97: 49 01 c0                     	addq	%rax, %r8
   41a9a: 48 8b 44 24 b8               	movq	-72(%rsp), %rax
   41a9f: 49 11 d1                     	adcq	%rdx, %r9
   41aa2: 48 f7 e1                     	mulq	%rcx
   41aa5: 49 01 c0                     	addq	%rax, %r8
   41aa8: 48 8b 44 24 b0               	movq	-80(%rsp), %rax
   41aad: 49 11 d1                     	adcq	%rdx, %r9
   41ab0: 48 f7 64 24 c0               	mulq	-64(%rsp)
   41ab5: 49 01 c0                     	addq	%rax, %r8
   41ab8: 48 8b 44 24 c8               	movq	-56(%rsp), %rax
   41abd: 49 11 d1                     	adcq	%rdx, %r9
   41ac0: 48 f7 e5                     	mulq	%rbp
   41ac3: 4c 01 c0                     	addq	%r8, %rax
   41ac6: 4c 11 ca                     	adcq	%r9, %rdx
   41ac9: 4d 0f ac ea 33               	shrdq	$51, %r13, %r10
   41ace: 45 31 c9                     	xorl	%r9d, %r9d
   41ad1: 4d 89 d0                     	movq	%r10, %r8
   41ad4: 49 01 c0                     	addq	%rax, %r8
   41ad7: 48 89 d8                     	movq	%rbx, %rax
   41ada: 49 11 d1                     	adcq	%rdx, %r9
   41add: 49 f7 e6                     	mulq	%r14
   41ae0: 49 89 c2                     	movq	%rax, %r10
   41ae3: 48 8b 44 24 d0               	movq	-48(%rsp), %rax
   41ae8: 49 89 d3                     	movq	%rdx, %r11
   41aeb: 4d 89 d6                     	movq	%r10, %r14
   41aee: 4c 8b 54 24 d8               	movq	-40(%rsp), %r10
   41af3: 49 f7 e7                     	mulq	%r15
   41af6: 4d 89 df                     	movq	%r11, %r15
   41af9: 49 01 c6                     	addq	%rax, %r14
   41afc: 48 8b 44 24 b8               	movq	-72(%rsp), %rax
   41b01: 49 11 d7                     	adcq	%rdx, %r15
   41b04: 48 f7 e6                     	mulq	%rsi
   41b07: 49 01 c6                     	addq	%rax, %r14
   41b0a: 48 8b 44 24 c0               	movq	-64(%rsp), %rax
   41b0f: 49 11 d7                     	adcq	%rdx, %r15
   41b12: 48 f7 e1                     	mulq	%rcx
   41b15: 4c 89 c1                     	movq	%r8, %rcx
   41b18: 49 01 c6                     	addq	%rax, %r14
   41b1b: 48 8b 44 24 b0               	movq	-80(%rsp), %rax
   41b20: 49 11 d7                     	adcq	%rdx, %r15
   41b23: 48 f7 e5                     	mulq	%rbp
   41b26: 49 01 c6                     	addq	%rax, %r14
   41b29: 49 11 d7                     	adcq	%rdx, %r15
   41b2c: 4c 0f ac c9 33               	shrdq	$51, %r9, %rcx
   41b31: 31 d2                        	xorl	%edx, %edx
   41b33: 49 01 ce                     	addq	%rcx, %r14
   41b36: 49 11 d7                     	adcq	%rdx, %r15
   41b39: 4c 89 f0                     	movq	%r14, %rax
   41b3c: 4c 0f ac f8 33               	shrdq	$51, %r15, %rax
   41b41: 48 8d 14 c0                  	leaq	(%rax,%rax,8), %rdx
   41b45: 48 8d 0c 50                  	leaq	(%rax,%rdx,2), %rcx
   41b49: 48 ba ff ff ff ff ff ff 07 00	movabsq	$2251799813685247, %rdx
   41b53: 49 21 d2                     	andq	%rdx, %r10
   41b56: 49 21 d6                     	andq	%rdx, %r14
   41b59: 49 21 d0                     	andq	%rdx, %r8
   41b5c: 4a 8d 04 11                  	leaq	(%rcx,%r10), %rax
   41b60: 4c 8b 54 24 e8               	movq	-24(%rsp), %r10
   41b65: 5b                           	popq	%rbx
   41b66: 4c 89 77 20                  	movq	%r14, 32(%rdi)
   41b6a: 48 89 c1                     	movq	%rax, %rcx
   41b6d: 48 c1 e8 33                  	shrq	$51, %rax
   41b71: 5d                           	popq	%rbp
   41b72: 4c 89 47 18                  	movq	%r8, 24(%rdi)
   41b76: 49 21 d2                     	andq	%rdx, %r10
   41b79: 48 21 d1                     	andq	%rdx, %rcx
   41b7c: 49 01 c2                     	addq	%rax, %r10
   41b7f: 48 89 0f                     	movq	%rcx, (%rdi)
   41b82: 4c 89 57 08                  	movq	%r10, 8(%rdi)
   41b86: 4d 89 e2                     	movq	%r12, %r10
   41b89: 41 5c                        	popq	%r12
   41b8b: 49 21 d2                     	andq	%rdx, %r10
   41b8e: 41 5d                        	popq	%r13
   41b90: 41 5e                        	popq	%r14
   41b92: 4c 89 57 10                  	movq	%r10, 16(%rdi)
   41b96: 41 5f                        	popq	%r15
   41b98: c3                           	retq
   41b99: 0f 1f 80 00 00 00 00         	nopl	(%rax)
