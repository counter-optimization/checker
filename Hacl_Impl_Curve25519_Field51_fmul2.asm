
data/crypto/curve25519/Hacl_Curve25519_51_O0.o:	file format elf64-x86-64

Disassembly of section .text:

0000000000004010 <Hacl_Impl_Curve25519_Field51_fmul2>:
    4010: 55                           	push	rbp
    4011: 48 89 e5                     	mov	rbp, rsp
    4014: 48 81 ec 50 1a 00 00         	sub	rsp, 6736
    401b: 48 89 7d f8                  	mov	qword ptr [rbp - 8], rdi
    401f: 48 89 75 f0                  	mov	qword ptr [rbp - 16], rsi
    4023: 48 89 55 e8                  	mov	qword ptr [rbp - 24], rdx
    4027: 48 89 4d e0                  	mov	qword ptr [rbp - 32], rcx
    402b: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    402f: 48 8b 00                     	mov	rax, qword ptr [rax]
    4032: 48 89 45 d8                  	mov	qword ptr [rbp - 40], rax
    4036: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    403a: 48 8b 40 08                  	mov	rax, qword ptr [rax + 8]
    403e: 48 89 45 d0                  	mov	qword ptr [rbp - 48], rax
    4042: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    4046: 48 8b 40 10                  	mov	rax, qword ptr [rax + 16]
    404a: 48 89 45 c8                  	mov	qword ptr [rbp - 56], rax
    404e: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    4052: 48 8b 40 18                  	mov	rax, qword ptr [rax + 24]
    4056: 48 89 45 c0                  	mov	qword ptr [rbp - 64], rax
    405a: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    405e: 48 8b 40 20                  	mov	rax, qword ptr [rax + 32]
    4062: 48 89 45 b8                  	mov	qword ptr [rbp - 72], rax
    4066: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    406a: 48 8b 00                     	mov	rax, qword ptr [rax]
    406d: 48 89 45 b0                  	mov	qword ptr [rbp - 80], rax
    4071: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    4075: 48 8b 40 08                  	mov	rax, qword ptr [rax + 8]
    4079: 48 89 45 a8                  	mov	qword ptr [rbp - 88], rax
    407d: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    4081: 48 8b 40 10                  	mov	rax, qword ptr [rax + 16]
    4085: 48 89 45 a0                  	mov	qword ptr [rbp - 96], rax
    4089: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    408d: 48 8b 40 18                  	mov	rax, qword ptr [rax + 24]
    4091: 48 89 45 98                  	mov	qword ptr [rbp - 104], rax
    4095: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    4099: 48 8b 40 20                  	mov	rax, qword ptr [rax + 32]
    409d: 48 89 45 90                  	mov	qword ptr [rbp - 112], rax
    40a1: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    40a5: 48 8b 40 28                  	mov	rax, qword ptr [rax + 40]
    40a9: 48 89 45 88                  	mov	qword ptr [rbp - 120], rax
    40ad: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    40b1: 48 8b 40 30                  	mov	rax, qword ptr [rax + 48]
    40b5: 48 89 45 80                  	mov	qword ptr [rbp - 128], rax
    40b9: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    40bd: 48 8b 40 38                  	mov	rax, qword ptr [rax + 56]
    40c1: 48 89 85 78 ff ff ff         	mov	qword ptr [rbp - 136], rax
    40c8: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    40cc: 48 8b 40 40                  	mov	rax, qword ptr [rax + 64]
    40d0: 48 89 85 70 ff ff ff         	mov	qword ptr [rbp - 144], rax
    40d7: 48 8b 45 f0                  	mov	rax, qword ptr [rbp - 16]
    40db: 48 8b 40 48                  	mov	rax, qword ptr [rax + 72]
    40df: 48 89 85 68 ff ff ff         	mov	qword ptr [rbp - 152], rax
    40e6: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    40ea: 48 8b 40 28                  	mov	rax, qword ptr [rax + 40]
    40ee: 48 89 85 60 ff ff ff         	mov	qword ptr [rbp - 160], rax
    40f5: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    40f9: 48 8b 40 30                  	mov	rax, qword ptr [rax + 48]
    40fd: 48 89 85 58 ff ff ff         	mov	qword ptr [rbp - 168], rax
    4104: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    4108: 48 8b 40 38                  	mov	rax, qword ptr [rax + 56]
    410c: 48 89 85 50 ff ff ff         	mov	qword ptr [rbp - 176], rax
    4113: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    4117: 48 8b 40 40                  	mov	rax, qword ptr [rax + 64]
    411b: 48 89 85 48 ff ff ff         	mov	qword ptr [rbp - 184], rax
    4122: 48 8b 45 e8                  	mov	rax, qword ptr [rbp - 24]
    4126: 48 8b 40 48                  	mov	rax, qword ptr [rax + 72]
    412a: 48 89 85 40 ff ff ff         	mov	qword ptr [rbp - 192], rax
    4131: 48 8b 45 a8                  	mov	rax, qword ptr [rbp - 88]
    4135: 48 8d 0c c0                  	lea	rcx, [rax + 8*rax]
    4139: 48 8d 04 48                  	lea	rax, [rax + 2*rcx]
    413d: 48 89 85 38 ff ff ff         	mov	qword ptr [rbp - 200], rax
    4144: 48 8b 45 a0                  	mov	rax, qword ptr [rbp - 96]
    4148: 48 8d 0c c0                  	lea	rcx, [rax + 8*rax]
    414c: 48 8d 04 48                  	lea	rax, [rax + 2*rcx]
    4150: 48 89 85 30 ff ff ff         	mov	qword ptr [rbp - 208], rax
    4157: 48 8b 45 98                  	mov	rax, qword ptr [rbp - 104]
    415b: 48 8d 0c c0                  	lea	rcx, [rax + 8*rax]
    415f: 48 8d 04 48                  	lea	rax, [rax + 2*rcx]
    4163: 48 89 85 28 ff ff ff         	mov	qword ptr [rbp - 216], rax
    416a: 48 8b 45 90                  	mov	rax, qword ptr [rbp - 112]
    416e: 48 8d 0c c0                  	lea	rcx, [rax + 8*rax]
    4172: 48 8d 04 48                  	lea	rax, [rax + 2*rcx]
    4176: 48 89 85 20 ff ff ff         	mov	qword ptr [rbp - 224], rax
    417d: 48 8b 85 58 ff ff ff         	mov	rax, qword ptr [rbp - 168]
    4184: 48 8d 0c c0                  	lea	rcx, [rax + 8*rax]
    4188: 48 8d 04 48                  	lea	rax, [rax + 2*rcx]
    418c: 48 89 85 18 ff ff ff         	mov	qword ptr [rbp - 232], rax
    4193: 48 8b 85 50 ff ff ff         	mov	rax, qword ptr [rbp - 176]
    419a: 48 8d 0c c0                  	lea	rcx, [rax + 8*rax]
    419e: 48 8d 04 48                  	lea	rax, [rax + 2*rcx]
    41a2: 48 89 85 10 ff ff ff         	mov	qword ptr [rbp - 240], rax
    41a9: 48 8b 85 48 ff ff ff         	mov	rax, qword ptr [rbp - 184]
    41b0: 48 8d 0c c0                  	lea	rcx, [rax + 8*rax]
    41b4: 48 8d 04 48                  	lea	rax, [rax + 2*rcx]
    41b8: 48 89 85 08 ff ff ff         	mov	qword ptr [rbp - 248], rax
    41bf: 48 8b 85 40 ff ff ff         	mov	rax, qword ptr [rbp - 192]
    41c6: 48 8d 0c c0                  	lea	rcx, [rax + 8*rax]
    41ca: 48 8d 04 48                  	lea	rax, [rax + 2*rcx]
    41ce: 48 89 85 00 ff ff ff         	mov	qword ptr [rbp - 256], rax
    41d5: 48 8b 7d d8                  	mov	rdi, qword ptr [rbp - 40]
    41d9: 48 8b 75 b0                  	mov	rsi, qword ptr [rbp - 80]
    41dd: e8 be f6 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    41e2: 48 89 85 e0 fe ff ff         	mov	qword ptr [rbp - 288], rax
    41e9: 48 89 95 e8 fe ff ff         	mov	qword ptr [rbp - 280], rdx
    41f0: 48 8b 85 e0 fe ff ff         	mov	rax, qword ptr [rbp - 288]
    41f7: 48 8b 8d e8 fe ff ff         	mov	rcx, qword ptr [rbp - 280]
    41fe: 48 89 8d f8 fe ff ff         	mov	qword ptr [rbp - 264], rcx
    4205: 48 89 85 f0 fe ff ff         	mov	qword ptr [rbp - 272], rax
    420c: 48 8b 7d d8                  	mov	rdi, qword ptr [rbp - 40]
    4210: 48 8b 75 a8                  	mov	rsi, qword ptr [rbp - 88]
    4214: e8 87 f6 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4219: 48 89 85 c0 fe ff ff         	mov	qword ptr [rbp - 320], rax
    4220: 48 89 95 c8 fe ff ff         	mov	qword ptr [rbp - 312], rdx
    4227: 48 8b 85 c0 fe ff ff         	mov	rax, qword ptr [rbp - 320]
    422e: 48 8b 8d c8 fe ff ff         	mov	rcx, qword ptr [rbp - 312]
    4235: 48 89 8d d8 fe ff ff         	mov	qword ptr [rbp - 296], rcx
    423c: 48 89 85 d0 fe ff ff         	mov	qword ptr [rbp - 304], rax
    4243: 48 8b 7d d8                  	mov	rdi, qword ptr [rbp - 40]
    4247: 48 8b 75 a0                  	mov	rsi, qword ptr [rbp - 96]
    424b: e8 50 f6 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4250: 48 89 85 a0 fe ff ff         	mov	qword ptr [rbp - 352], rax
    4257: 48 89 95 a8 fe ff ff         	mov	qword ptr [rbp - 344], rdx
    425e: 48 8b 85 a0 fe ff ff         	mov	rax, qword ptr [rbp - 352]
    4265: 48 8b 8d a8 fe ff ff         	mov	rcx, qword ptr [rbp - 344]
    426c: 48 89 8d b8 fe ff ff         	mov	qword ptr [rbp - 328], rcx
    4273: 48 89 85 b0 fe ff ff         	mov	qword ptr [rbp - 336], rax
    427a: 48 8b 7d d8                  	mov	rdi, qword ptr [rbp - 40]
    427e: 48 8b 75 98                  	mov	rsi, qword ptr [rbp - 104]
    4282: e8 19 f6 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4287: 48 89 85 80 fe ff ff         	mov	qword ptr [rbp - 384], rax
    428e: 48 89 95 88 fe ff ff         	mov	qword ptr [rbp - 376], rdx
    4295: 48 8b 85 80 fe ff ff         	mov	rax, qword ptr [rbp - 384]
    429c: 48 8b 8d 88 fe ff ff         	mov	rcx, qword ptr [rbp - 376]
    42a3: 48 89 8d 98 fe ff ff         	mov	qword ptr [rbp - 360], rcx
    42aa: 48 89 85 90 fe ff ff         	mov	qword ptr [rbp - 368], rax
    42b1: 48 8b 7d d8                  	mov	rdi, qword ptr [rbp - 40]
    42b5: 48 8b 75 90                  	mov	rsi, qword ptr [rbp - 112]
    42b9: e8 e2 f5 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    42be: 48 89 85 60 fe ff ff         	mov	qword ptr [rbp - 416], rax
    42c5: 48 89 95 68 fe ff ff         	mov	qword ptr [rbp - 408], rdx
    42cc: 48 8b 85 60 fe ff ff         	mov	rax, qword ptr [rbp - 416]
    42d3: 48 8b 8d 68 fe ff ff         	mov	rcx, qword ptr [rbp - 408]
    42da: 48 89 8d 78 fe ff ff         	mov	qword ptr [rbp - 392], rcx
    42e1: 48 89 85 70 fe ff ff         	mov	qword ptr [rbp - 400], rax
    42e8: 48 8b 85 f0 fe ff ff         	mov	rax, qword ptr [rbp - 272]
    42ef: 48 89 85 c0 e5 ff ff         	mov	qword ptr [rbp - 6720], rax
    42f6: 48 8b 85 f8 fe ff ff         	mov	rax, qword ptr [rbp - 264]
    42fd: 48 89 85 b8 e5 ff ff         	mov	qword ptr [rbp - 6728], rax
    4304: 48 8b 7d d0                  	mov	rdi, qword ptr [rbp - 48]
    4308: 48 8b b5 20 ff ff ff         	mov	rsi, qword ptr [rbp - 224]
    430f: e8 8c f5 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4314: 48 8b b5 b8 e5 ff ff         	mov	rsi, qword ptr [rbp - 6728]
    431b: 48 89 c1                     	mov	rcx, rax
    431e: 48 89 d0                     	mov	rax, rdx
    4321: 48 8b 95 c0 e5 ff ff         	mov	rdx, qword ptr [rbp - 6720]
    4328: 48 89 8d 40 fe ff ff         	mov	qword ptr [rbp - 448], rcx
    432f: 48 89 85 48 fe ff ff         	mov	qword ptr [rbp - 440], rax
    4336: 48 8b 85 40 fe ff ff         	mov	rax, qword ptr [rbp - 448]
    433d: 48 8b 8d 48 fe ff ff         	mov	rcx, qword ptr [rbp - 440]
    4344: 48 89 b5 38 fe ff ff         	mov	qword ptr [rbp - 456], rsi
    434b: 48 89 95 30 fe ff ff         	mov	qword ptr [rbp - 464], rdx
    4352: 48 8b bd 30 fe ff ff         	mov	rdi, qword ptr [rbp - 464]
    4359: 48 8b b5 38 fe ff ff         	mov	rsi, qword ptr [rbp - 456]
    4360: 48 89 8d 28 fe ff ff         	mov	qword ptr [rbp - 472], rcx
    4367: 48 89 85 20 fe ff ff         	mov	qword ptr [rbp - 480], rax
    436e: 48 8b 95 20 fe ff ff         	mov	rdx, qword ptr [rbp - 480]
    4375: 48 8b 8d 28 fe ff ff         	mov	rcx, qword ptr [rbp - 472]
    437c: e8 bf f4 ff ff               	call	0x3840 <FStar_UInt128_add>
    4381: 48 89 85 10 fe ff ff         	mov	qword ptr [rbp - 496], rax
    4388: 48 89 95 18 fe ff ff         	mov	qword ptr [rbp - 488], rdx
    438f: 48 8b 85 10 fe ff ff         	mov	rax, qword ptr [rbp - 496]
    4396: 48 8b 8d 18 fe ff ff         	mov	rcx, qword ptr [rbp - 488]
    439d: 48 89 8d 58 fe ff ff         	mov	qword ptr [rbp - 424], rcx
    43a4: 48 89 85 50 fe ff ff         	mov	qword ptr [rbp - 432], rax
    43ab: 48 8b 85 d0 fe ff ff         	mov	rax, qword ptr [rbp - 304]
    43b2: 48 89 85 d0 e5 ff ff         	mov	qword ptr [rbp - 6704], rax
    43b9: 48 8b 85 d8 fe ff ff         	mov	rax, qword ptr [rbp - 296]
    43c0: 48 89 85 c8 e5 ff ff         	mov	qword ptr [rbp - 6712], rax
    43c7: 48 8b 7d d0                  	mov	rdi, qword ptr [rbp - 48]
    43cb: 48 8b 75 b0                  	mov	rsi, qword ptr [rbp - 80]
    43cf: e8 cc f4 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    43d4: 48 8b b5 c8 e5 ff ff         	mov	rsi, qword ptr [rbp - 6712]
    43db: 48 89 c1                     	mov	rcx, rax
    43de: 48 89 d0                     	mov	rax, rdx
    43e1: 48 8b 95 d0 e5 ff ff         	mov	rdx, qword ptr [rbp - 6704]
    43e8: 48 89 8d f0 fd ff ff         	mov	qword ptr [rbp - 528], rcx
    43ef: 48 89 85 f8 fd ff ff         	mov	qword ptr [rbp - 520], rax
    43f6: 48 8b 85 f0 fd ff ff         	mov	rax, qword ptr [rbp - 528]
    43fd: 48 8b 8d f8 fd ff ff         	mov	rcx, qword ptr [rbp - 520]
    4404: 48 89 b5 e8 fd ff ff         	mov	qword ptr [rbp - 536], rsi
    440b: 48 89 95 e0 fd ff ff         	mov	qword ptr [rbp - 544], rdx
    4412: 48 8b bd e0 fd ff ff         	mov	rdi, qword ptr [rbp - 544]
    4419: 48 8b b5 e8 fd ff ff         	mov	rsi, qword ptr [rbp - 536]
    4420: 48 89 8d d8 fd ff ff         	mov	qword ptr [rbp - 552], rcx
    4427: 48 89 85 d0 fd ff ff         	mov	qword ptr [rbp - 560], rax
    442e: 48 8b 95 d0 fd ff ff         	mov	rdx, qword ptr [rbp - 560]
    4435: 48 8b 8d d8 fd ff ff         	mov	rcx, qword ptr [rbp - 552]
    443c: e8 ff f3 ff ff               	call	0x3840 <FStar_UInt128_add>
    4441: 48 89 85 c0 fd ff ff         	mov	qword ptr [rbp - 576], rax
    4448: 48 89 95 c8 fd ff ff         	mov	qword ptr [rbp - 568], rdx
    444f: 48 8b 85 c0 fd ff ff         	mov	rax, qword ptr [rbp - 576]
    4456: 48 8b 8d c8 fd ff ff         	mov	rcx, qword ptr [rbp - 568]
    445d: 48 89 8d 08 fe ff ff         	mov	qword ptr [rbp - 504], rcx
    4464: 48 89 85 00 fe ff ff         	mov	qword ptr [rbp - 512], rax
    446b: 48 8b 85 b0 fe ff ff         	mov	rax, qword ptr [rbp - 336]
    4472: 48 89 85 e0 e5 ff ff         	mov	qword ptr [rbp - 6688], rax
    4479: 48 8b 85 b8 fe ff ff         	mov	rax, qword ptr [rbp - 328]
    4480: 48 89 85 d8 e5 ff ff         	mov	qword ptr [rbp - 6696], rax
    4487: 48 8b 7d d0                  	mov	rdi, qword ptr [rbp - 48]
    448b: 48 8b 75 a8                  	mov	rsi, qword ptr [rbp - 88]
    448f: e8 0c f4 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4494: 48 8b b5 d8 e5 ff ff         	mov	rsi, qword ptr [rbp - 6696]
    449b: 48 89 c1                     	mov	rcx, rax
    449e: 48 89 d0                     	mov	rax, rdx
    44a1: 48 8b 95 e0 e5 ff ff         	mov	rdx, qword ptr [rbp - 6688]
    44a8: 48 89 8d a0 fd ff ff         	mov	qword ptr [rbp - 608], rcx
    44af: 48 89 85 a8 fd ff ff         	mov	qword ptr [rbp - 600], rax
    44b6: 48 8b 85 a0 fd ff ff         	mov	rax, qword ptr [rbp - 608]
    44bd: 48 8b 8d a8 fd ff ff         	mov	rcx, qword ptr [rbp - 600]
    44c4: 48 89 b5 98 fd ff ff         	mov	qword ptr [rbp - 616], rsi
    44cb: 48 89 95 90 fd ff ff         	mov	qword ptr [rbp - 624], rdx
    44d2: 48 8b bd 90 fd ff ff         	mov	rdi, qword ptr [rbp - 624]
    44d9: 48 8b b5 98 fd ff ff         	mov	rsi, qword ptr [rbp - 616]
    44e0: 48 89 8d 88 fd ff ff         	mov	qword ptr [rbp - 632], rcx
    44e7: 48 89 85 80 fd ff ff         	mov	qword ptr [rbp - 640], rax
    44ee: 48 8b 95 80 fd ff ff         	mov	rdx, qword ptr [rbp - 640]
    44f5: 48 8b 8d 88 fd ff ff         	mov	rcx, qword ptr [rbp - 632]
    44fc: e8 3f f3 ff ff               	call	0x3840 <FStar_UInt128_add>
    4501: 48 89 85 70 fd ff ff         	mov	qword ptr [rbp - 656], rax
    4508: 48 89 95 78 fd ff ff         	mov	qword ptr [rbp - 648], rdx
    450f: 48 8b 85 70 fd ff ff         	mov	rax, qword ptr [rbp - 656]
    4516: 48 8b 8d 78 fd ff ff         	mov	rcx, qword ptr [rbp - 648]
    451d: 48 89 8d b8 fd ff ff         	mov	qword ptr [rbp - 584], rcx
    4524: 48 89 85 b0 fd ff ff         	mov	qword ptr [rbp - 592], rax
    452b: 48 8b 85 90 fe ff ff         	mov	rax, qword ptr [rbp - 368]
    4532: 48 89 85 f0 e5 ff ff         	mov	qword ptr [rbp - 6672], rax
    4539: 48 8b 85 98 fe ff ff         	mov	rax, qword ptr [rbp - 360]
    4540: 48 89 85 e8 e5 ff ff         	mov	qword ptr [rbp - 6680], rax
    4547: 48 8b 7d d0                  	mov	rdi, qword ptr [rbp - 48]
    454b: 48 8b 75 a0                  	mov	rsi, qword ptr [rbp - 96]
    454f: e8 4c f3 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4554: 48 8b b5 e8 e5 ff ff         	mov	rsi, qword ptr [rbp - 6680]
    455b: 48 89 c1                     	mov	rcx, rax
    455e: 48 89 d0                     	mov	rax, rdx
    4561: 48 8b 95 f0 e5 ff ff         	mov	rdx, qword ptr [rbp - 6672]
    4568: 48 89 8d 50 fd ff ff         	mov	qword ptr [rbp - 688], rcx
    456f: 48 89 85 58 fd ff ff         	mov	qword ptr [rbp - 680], rax
    4576: 48 8b 85 50 fd ff ff         	mov	rax, qword ptr [rbp - 688]
    457d: 48 8b 8d 58 fd ff ff         	mov	rcx, qword ptr [rbp - 680]
    4584: 48 89 b5 48 fd ff ff         	mov	qword ptr [rbp - 696], rsi
    458b: 48 89 95 40 fd ff ff         	mov	qword ptr [rbp - 704], rdx
    4592: 48 8b bd 40 fd ff ff         	mov	rdi, qword ptr [rbp - 704]
    4599: 48 8b b5 48 fd ff ff         	mov	rsi, qword ptr [rbp - 696]
    45a0: 48 89 8d 38 fd ff ff         	mov	qword ptr [rbp - 712], rcx
    45a7: 48 89 85 30 fd ff ff         	mov	qword ptr [rbp - 720], rax
    45ae: 48 8b 95 30 fd ff ff         	mov	rdx, qword ptr [rbp - 720]
    45b5: 48 8b 8d 38 fd ff ff         	mov	rcx, qword ptr [rbp - 712]
    45bc: e8 7f f2 ff ff               	call	0x3840 <FStar_UInt128_add>
    45c1: 48 89 85 20 fd ff ff         	mov	qword ptr [rbp - 736], rax
    45c8: 48 89 95 28 fd ff ff         	mov	qword ptr [rbp - 728], rdx
    45cf: 48 8b 85 20 fd ff ff         	mov	rax, qword ptr [rbp - 736]
    45d6: 48 8b 8d 28 fd ff ff         	mov	rcx, qword ptr [rbp - 728]
    45dd: 48 89 8d 68 fd ff ff         	mov	qword ptr [rbp - 664], rcx
    45e4: 48 89 85 60 fd ff ff         	mov	qword ptr [rbp - 672], rax
    45eb: 48 8b 85 70 fe ff ff         	mov	rax, qword ptr [rbp - 400]
    45f2: 48 89 85 00 e6 ff ff         	mov	qword ptr [rbp - 6656], rax
    45f9: 48 8b 85 78 fe ff ff         	mov	rax, qword ptr [rbp - 392]
    4600: 48 89 85 f8 e5 ff ff         	mov	qword ptr [rbp - 6664], rax
    4607: 48 8b 7d d0                  	mov	rdi, qword ptr [rbp - 48]
    460b: 48 8b 75 98                  	mov	rsi, qword ptr [rbp - 104]
    460f: e8 8c f2 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4614: 48 8b b5 f8 e5 ff ff         	mov	rsi, qword ptr [rbp - 6664]
    461b: 48 89 c1                     	mov	rcx, rax
    461e: 48 89 d0                     	mov	rax, rdx
    4621: 48 8b 95 00 e6 ff ff         	mov	rdx, qword ptr [rbp - 6656]
    4628: 48 89 8d 00 fd ff ff         	mov	qword ptr [rbp - 768], rcx
    462f: 48 89 85 08 fd ff ff         	mov	qword ptr [rbp - 760], rax
    4636: 48 8b 85 00 fd ff ff         	mov	rax, qword ptr [rbp - 768]
    463d: 48 8b 8d 08 fd ff ff         	mov	rcx, qword ptr [rbp - 760]
    4644: 48 89 b5 f8 fc ff ff         	mov	qword ptr [rbp - 776], rsi
    464b: 48 89 95 f0 fc ff ff         	mov	qword ptr [rbp - 784], rdx
    4652: 48 8b bd f0 fc ff ff         	mov	rdi, qword ptr [rbp - 784]
    4659: 48 8b b5 f8 fc ff ff         	mov	rsi, qword ptr [rbp - 776]
    4660: 48 89 8d e8 fc ff ff         	mov	qword ptr [rbp - 792], rcx
    4667: 48 89 85 e0 fc ff ff         	mov	qword ptr [rbp - 800], rax
    466e: 48 8b 95 e0 fc ff ff         	mov	rdx, qword ptr [rbp - 800]
    4675: 48 8b 8d e8 fc ff ff         	mov	rcx, qword ptr [rbp - 792]
    467c: e8 bf f1 ff ff               	call	0x3840 <FStar_UInt128_add>
    4681: 48 89 85 d0 fc ff ff         	mov	qword ptr [rbp - 816], rax
    4688: 48 89 95 d8 fc ff ff         	mov	qword ptr [rbp - 808], rdx
    468f: 48 8b 85 d0 fc ff ff         	mov	rax, qword ptr [rbp - 816]
    4696: 48 8b 8d d8 fc ff ff         	mov	rcx, qword ptr [rbp - 808]
    469d: 48 89 8d 18 fd ff ff         	mov	qword ptr [rbp - 744], rcx
    46a4: 48 89 85 10 fd ff ff         	mov	qword ptr [rbp - 752], rax
    46ab: 48 8b 85 50 fe ff ff         	mov	rax, qword ptr [rbp - 432]
    46b2: 48 89 85 10 e6 ff ff         	mov	qword ptr [rbp - 6640], rax
    46b9: 48 8b 85 58 fe ff ff         	mov	rax, qword ptr [rbp - 424]
    46c0: 48 89 85 08 e6 ff ff         	mov	qword ptr [rbp - 6648], rax
    46c7: 48 8b 7d c8                  	mov	rdi, qword ptr [rbp - 56]
    46cb: 48 8b b5 28 ff ff ff         	mov	rsi, qword ptr [rbp - 216]
    46d2: e8 c9 f1 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    46d7: 48 8b b5 08 e6 ff ff         	mov	rsi, qword ptr [rbp - 6648]
    46de: 48 89 c1                     	mov	rcx, rax
    46e1: 48 89 d0                     	mov	rax, rdx
    46e4: 48 8b 95 10 e6 ff ff         	mov	rdx, qword ptr [rbp - 6640]
    46eb: 48 89 8d b0 fc ff ff         	mov	qword ptr [rbp - 848], rcx
    46f2: 48 89 85 b8 fc ff ff         	mov	qword ptr [rbp - 840], rax
    46f9: 48 8b 85 b0 fc ff ff         	mov	rax, qword ptr [rbp - 848]
    4700: 48 8b 8d b8 fc ff ff         	mov	rcx, qword ptr [rbp - 840]
    4707: 48 89 b5 a8 fc ff ff         	mov	qword ptr [rbp - 856], rsi
    470e: 48 89 95 a0 fc ff ff         	mov	qword ptr [rbp - 864], rdx
    4715: 48 8b bd a0 fc ff ff         	mov	rdi, qword ptr [rbp - 864]
    471c: 48 8b b5 a8 fc ff ff         	mov	rsi, qword ptr [rbp - 856]
    4723: 48 89 8d 98 fc ff ff         	mov	qword ptr [rbp - 872], rcx
    472a: 48 89 85 90 fc ff ff         	mov	qword ptr [rbp - 880], rax
    4731: 48 8b 95 90 fc ff ff         	mov	rdx, qword ptr [rbp - 880]
    4738: 48 8b 8d 98 fc ff ff         	mov	rcx, qword ptr [rbp - 872]
    473f: e8 fc f0 ff ff               	call	0x3840 <FStar_UInt128_add>
    4744: 48 89 85 80 fc ff ff         	mov	qword ptr [rbp - 896], rax
    474b: 48 89 95 88 fc ff ff         	mov	qword ptr [rbp - 888], rdx
    4752: 48 8b 85 80 fc ff ff         	mov	rax, qword ptr [rbp - 896]
    4759: 48 8b 8d 88 fc ff ff         	mov	rcx, qword ptr [rbp - 888]
    4760: 48 89 8d c8 fc ff ff         	mov	qword ptr [rbp - 824], rcx
    4767: 48 89 85 c0 fc ff ff         	mov	qword ptr [rbp - 832], rax
    476e: 48 8b 85 00 fe ff ff         	mov	rax, qword ptr [rbp - 512]
    4775: 48 89 85 20 e6 ff ff         	mov	qword ptr [rbp - 6624], rax
    477c: 48 8b 85 08 fe ff ff         	mov	rax, qword ptr [rbp - 504]
    4783: 48 89 85 18 e6 ff ff         	mov	qword ptr [rbp - 6632], rax
    478a: 48 8b 7d c8                  	mov	rdi, qword ptr [rbp - 56]
    478e: 48 8b b5 20 ff ff ff         	mov	rsi, qword ptr [rbp - 224]
    4795: e8 06 f1 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    479a: 48 8b b5 18 e6 ff ff         	mov	rsi, qword ptr [rbp - 6632]
    47a1: 48 89 c1                     	mov	rcx, rax
    47a4: 48 89 d0                     	mov	rax, rdx
    47a7: 48 8b 95 20 e6 ff ff         	mov	rdx, qword ptr [rbp - 6624]
    47ae: 48 89 8d 60 fc ff ff         	mov	qword ptr [rbp - 928], rcx
    47b5: 48 89 85 68 fc ff ff         	mov	qword ptr [rbp - 920], rax
    47bc: 48 8b 85 60 fc ff ff         	mov	rax, qword ptr [rbp - 928]
    47c3: 48 8b 8d 68 fc ff ff         	mov	rcx, qword ptr [rbp - 920]
    47ca: 48 89 b5 58 fc ff ff         	mov	qword ptr [rbp - 936], rsi
    47d1: 48 89 95 50 fc ff ff         	mov	qword ptr [rbp - 944], rdx
    47d8: 48 8b bd 50 fc ff ff         	mov	rdi, qword ptr [rbp - 944]
    47df: 48 8b b5 58 fc ff ff         	mov	rsi, qword ptr [rbp - 936]
    47e6: 48 89 8d 48 fc ff ff         	mov	qword ptr [rbp - 952], rcx
    47ed: 48 89 85 40 fc ff ff         	mov	qword ptr [rbp - 960], rax
    47f4: 48 8b 95 40 fc ff ff         	mov	rdx, qword ptr [rbp - 960]
    47fb: 48 8b 8d 48 fc ff ff         	mov	rcx, qword ptr [rbp - 952]
    4802: e8 39 f0 ff ff               	call	0x3840 <FStar_UInt128_add>
    4807: 48 89 85 30 fc ff ff         	mov	qword ptr [rbp - 976], rax
    480e: 48 89 95 38 fc ff ff         	mov	qword ptr [rbp - 968], rdx
    4815: 48 8b 85 30 fc ff ff         	mov	rax, qword ptr [rbp - 976]
    481c: 48 8b 8d 38 fc ff ff         	mov	rcx, qword ptr [rbp - 968]
    4823: 48 89 8d 78 fc ff ff         	mov	qword ptr [rbp - 904], rcx
    482a: 48 89 85 70 fc ff ff         	mov	qword ptr [rbp - 912], rax
    4831: 48 8b 85 b0 fd ff ff         	mov	rax, qword ptr [rbp - 592]
    4838: 48 89 85 30 e6 ff ff         	mov	qword ptr [rbp - 6608], rax
    483f: 48 8b 85 b8 fd ff ff         	mov	rax, qword ptr [rbp - 584]
    4846: 48 89 85 28 e6 ff ff         	mov	qword ptr [rbp - 6616], rax
    484d: 48 8b 7d c8                  	mov	rdi, qword ptr [rbp - 56]
    4851: 48 8b 75 b0                  	mov	rsi, qword ptr [rbp - 80]
    4855: e8 46 f0 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    485a: 48 8b b5 28 e6 ff ff         	mov	rsi, qword ptr [rbp - 6616]
    4861: 48 89 c1                     	mov	rcx, rax
    4864: 48 89 d0                     	mov	rax, rdx
    4867: 48 8b 95 30 e6 ff ff         	mov	rdx, qword ptr [rbp - 6608]
    486e: 48 89 8d 10 fc ff ff         	mov	qword ptr [rbp - 1008], rcx
    4875: 48 89 85 18 fc ff ff         	mov	qword ptr [rbp - 1000], rax
    487c: 48 8b 85 10 fc ff ff         	mov	rax, qword ptr [rbp - 1008]
    4883: 48 8b 8d 18 fc ff ff         	mov	rcx, qword ptr [rbp - 1000]
    488a: 48 89 b5 08 fc ff ff         	mov	qword ptr [rbp - 1016], rsi
    4891: 48 89 95 00 fc ff ff         	mov	qword ptr [rbp - 1024], rdx
    4898: 48 8b bd 00 fc ff ff         	mov	rdi, qword ptr [rbp - 1024]
    489f: 48 8b b5 08 fc ff ff         	mov	rsi, qword ptr [rbp - 1016]
    48a6: 48 89 8d f8 fb ff ff         	mov	qword ptr [rbp - 1032], rcx
    48ad: 48 89 85 f0 fb ff ff         	mov	qword ptr [rbp - 1040], rax
    48b4: 48 8b 95 f0 fb ff ff         	mov	rdx, qword ptr [rbp - 1040]
    48bb: 48 8b 8d f8 fb ff ff         	mov	rcx, qword ptr [rbp - 1032]
    48c2: e8 79 ef ff ff               	call	0x3840 <FStar_UInt128_add>
    48c7: 48 89 85 e0 fb ff ff         	mov	qword ptr [rbp - 1056], rax
    48ce: 48 89 95 e8 fb ff ff         	mov	qword ptr [rbp - 1048], rdx
    48d5: 48 8b 85 e0 fb ff ff         	mov	rax, qword ptr [rbp - 1056]
    48dc: 48 8b 8d e8 fb ff ff         	mov	rcx, qword ptr [rbp - 1048]
    48e3: 48 89 8d 28 fc ff ff         	mov	qword ptr [rbp - 984], rcx
    48ea: 48 89 85 20 fc ff ff         	mov	qword ptr [rbp - 992], rax
    48f1: 48 8b 85 60 fd ff ff         	mov	rax, qword ptr [rbp - 672]
    48f8: 48 89 85 40 e6 ff ff         	mov	qword ptr [rbp - 6592], rax
    48ff: 48 8b 85 68 fd ff ff         	mov	rax, qword ptr [rbp - 664]
    4906: 48 89 85 38 e6 ff ff         	mov	qword ptr [rbp - 6600], rax
    490d: 48 8b 7d c8                  	mov	rdi, qword ptr [rbp - 56]
    4911: 48 8b 75 a8                  	mov	rsi, qword ptr [rbp - 88]
    4915: e8 86 ef ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    491a: 48 8b b5 38 e6 ff ff         	mov	rsi, qword ptr [rbp - 6600]
    4921: 48 89 c1                     	mov	rcx, rax
    4924: 48 89 d0                     	mov	rax, rdx
    4927: 48 8b 95 40 e6 ff ff         	mov	rdx, qword ptr [rbp - 6592]
    492e: 48 89 8d c0 fb ff ff         	mov	qword ptr [rbp - 1088], rcx
    4935: 48 89 85 c8 fb ff ff         	mov	qword ptr [rbp - 1080], rax
    493c: 48 8b 85 c0 fb ff ff         	mov	rax, qword ptr [rbp - 1088]
    4943: 48 8b 8d c8 fb ff ff         	mov	rcx, qword ptr [rbp - 1080]
    494a: 48 89 b5 b8 fb ff ff         	mov	qword ptr [rbp - 1096], rsi
    4951: 48 89 95 b0 fb ff ff         	mov	qword ptr [rbp - 1104], rdx
    4958: 48 8b bd b0 fb ff ff         	mov	rdi, qword ptr [rbp - 1104]
    495f: 48 8b b5 b8 fb ff ff         	mov	rsi, qword ptr [rbp - 1096]
    4966: 48 89 8d a8 fb ff ff         	mov	qword ptr [rbp - 1112], rcx
    496d: 48 89 85 a0 fb ff ff         	mov	qword ptr [rbp - 1120], rax
    4974: 48 8b 95 a0 fb ff ff         	mov	rdx, qword ptr [rbp - 1120]
    497b: 48 8b 8d a8 fb ff ff         	mov	rcx, qword ptr [rbp - 1112]
    4982: e8 b9 ee ff ff               	call	0x3840 <FStar_UInt128_add>
    4987: 48 89 85 90 fb ff ff         	mov	qword ptr [rbp - 1136], rax
    498e: 48 89 95 98 fb ff ff         	mov	qword ptr [rbp - 1128], rdx
    4995: 48 8b 85 90 fb ff ff         	mov	rax, qword ptr [rbp - 1136]
    499c: 48 8b 8d 98 fb ff ff         	mov	rcx, qword ptr [rbp - 1128]
    49a3: 48 89 8d d8 fb ff ff         	mov	qword ptr [rbp - 1064], rcx
    49aa: 48 89 85 d0 fb ff ff         	mov	qword ptr [rbp - 1072], rax
    49b1: 48 8b 85 10 fd ff ff         	mov	rax, qword ptr [rbp - 752]
    49b8: 48 89 85 50 e6 ff ff         	mov	qword ptr [rbp - 6576], rax
    49bf: 48 8b 85 18 fd ff ff         	mov	rax, qword ptr [rbp - 744]
    49c6: 48 89 85 48 e6 ff ff         	mov	qword ptr [rbp - 6584], rax
    49cd: 48 8b 7d c8                  	mov	rdi, qword ptr [rbp - 56]
    49d1: 48 8b 75 a0                  	mov	rsi, qword ptr [rbp - 96]
    49d5: e8 c6 ee ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    49da: 48 8b b5 48 e6 ff ff         	mov	rsi, qword ptr [rbp - 6584]
    49e1: 48 89 c1                     	mov	rcx, rax
    49e4: 48 89 d0                     	mov	rax, rdx
    49e7: 48 8b 95 50 e6 ff ff         	mov	rdx, qword ptr [rbp - 6576]
    49ee: 48 89 8d 70 fb ff ff         	mov	qword ptr [rbp - 1168], rcx
    49f5: 48 89 85 78 fb ff ff         	mov	qword ptr [rbp - 1160], rax
    49fc: 48 8b 85 70 fb ff ff         	mov	rax, qword ptr [rbp - 1168]
    4a03: 48 8b 8d 78 fb ff ff         	mov	rcx, qword ptr [rbp - 1160]
    4a0a: 48 89 b5 68 fb ff ff         	mov	qword ptr [rbp - 1176], rsi
    4a11: 48 89 95 60 fb ff ff         	mov	qword ptr [rbp - 1184], rdx
    4a18: 48 8b bd 60 fb ff ff         	mov	rdi, qword ptr [rbp - 1184]
    4a1f: 48 8b b5 68 fb ff ff         	mov	rsi, qword ptr [rbp - 1176]
    4a26: 48 89 8d 58 fb ff ff         	mov	qword ptr [rbp - 1192], rcx
    4a2d: 48 89 85 50 fb ff ff         	mov	qword ptr [rbp - 1200], rax
    4a34: 48 8b 95 50 fb ff ff         	mov	rdx, qword ptr [rbp - 1200]
    4a3b: 48 8b 8d 58 fb ff ff         	mov	rcx, qword ptr [rbp - 1192]
    4a42: e8 f9 ed ff ff               	call	0x3840 <FStar_UInt128_add>
    4a47: 48 89 85 40 fb ff ff         	mov	qword ptr [rbp - 1216], rax
    4a4e: 48 89 95 48 fb ff ff         	mov	qword ptr [rbp - 1208], rdx
    4a55: 48 8b 85 40 fb ff ff         	mov	rax, qword ptr [rbp - 1216]
    4a5c: 48 8b 8d 48 fb ff ff         	mov	rcx, qword ptr [rbp - 1208]
    4a63: 48 89 8d 88 fb ff ff         	mov	qword ptr [rbp - 1144], rcx
    4a6a: 48 89 85 80 fb ff ff         	mov	qword ptr [rbp - 1152], rax
    4a71: 48 8b 85 c0 fc ff ff         	mov	rax, qword ptr [rbp - 832]
    4a78: 48 89 85 60 e6 ff ff         	mov	qword ptr [rbp - 6560], rax
    4a7f: 48 8b 85 c8 fc ff ff         	mov	rax, qword ptr [rbp - 824]
    4a86: 48 89 85 58 e6 ff ff         	mov	qword ptr [rbp - 6568], rax
    4a8d: 48 8b 7d c0                  	mov	rdi, qword ptr [rbp - 64]
    4a91: 48 8b b5 30 ff ff ff         	mov	rsi, qword ptr [rbp - 208]
    4a98: e8 03 ee ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4a9d: 48 8b b5 58 e6 ff ff         	mov	rsi, qword ptr [rbp - 6568]
    4aa4: 48 89 c1                     	mov	rcx, rax
    4aa7: 48 89 d0                     	mov	rax, rdx
    4aaa: 48 8b 95 60 e6 ff ff         	mov	rdx, qword ptr [rbp - 6560]
    4ab1: 48 89 8d 20 fb ff ff         	mov	qword ptr [rbp - 1248], rcx
    4ab8: 48 89 85 28 fb ff ff         	mov	qword ptr [rbp - 1240], rax
    4abf: 48 8b 85 20 fb ff ff         	mov	rax, qword ptr [rbp - 1248]
    4ac6: 48 8b 8d 28 fb ff ff         	mov	rcx, qword ptr [rbp - 1240]
    4acd: 48 89 b5 18 fb ff ff         	mov	qword ptr [rbp - 1256], rsi
    4ad4: 48 89 95 10 fb ff ff         	mov	qword ptr [rbp - 1264], rdx
    4adb: 48 8b bd 10 fb ff ff         	mov	rdi, qword ptr [rbp - 1264]
    4ae2: 48 8b b5 18 fb ff ff         	mov	rsi, qword ptr [rbp - 1256]
    4ae9: 48 89 8d 08 fb ff ff         	mov	qword ptr [rbp - 1272], rcx
    4af0: 48 89 85 00 fb ff ff         	mov	qword ptr [rbp - 1280], rax
    4af7: 48 8b 95 00 fb ff ff         	mov	rdx, qword ptr [rbp - 1280]
    4afe: 48 8b 8d 08 fb ff ff         	mov	rcx, qword ptr [rbp - 1272]
    4b05: e8 36 ed ff ff               	call	0x3840 <FStar_UInt128_add>
    4b0a: 48 89 85 f0 fa ff ff         	mov	qword ptr [rbp - 1296], rax
    4b11: 48 89 95 f8 fa ff ff         	mov	qword ptr [rbp - 1288], rdx
    4b18: 48 8b 85 f0 fa ff ff         	mov	rax, qword ptr [rbp - 1296]
    4b1f: 48 8b 8d f8 fa ff ff         	mov	rcx, qword ptr [rbp - 1288]
    4b26: 48 89 8d 38 fb ff ff         	mov	qword ptr [rbp - 1224], rcx
    4b2d: 48 89 85 30 fb ff ff         	mov	qword ptr [rbp - 1232], rax
    4b34: 48 8b 85 70 fc ff ff         	mov	rax, qword ptr [rbp - 912]
    4b3b: 48 89 85 70 e6 ff ff         	mov	qword ptr [rbp - 6544], rax
    4b42: 48 8b 85 78 fc ff ff         	mov	rax, qword ptr [rbp - 904]
    4b49: 48 89 85 68 e6 ff ff         	mov	qword ptr [rbp - 6552], rax
    4b50: 48 8b 7d c0                  	mov	rdi, qword ptr [rbp - 64]
    4b54: 48 8b b5 28 ff ff ff         	mov	rsi, qword ptr [rbp - 216]
    4b5b: e8 40 ed ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4b60: 48 8b b5 68 e6 ff ff         	mov	rsi, qword ptr [rbp - 6552]
    4b67: 48 89 c1                     	mov	rcx, rax
    4b6a: 48 89 d0                     	mov	rax, rdx
    4b6d: 48 8b 95 70 e6 ff ff         	mov	rdx, qword ptr [rbp - 6544]
    4b74: 48 89 8d d0 fa ff ff         	mov	qword ptr [rbp - 1328], rcx
    4b7b: 48 89 85 d8 fa ff ff         	mov	qword ptr [rbp - 1320], rax
    4b82: 48 8b 85 d0 fa ff ff         	mov	rax, qword ptr [rbp - 1328]
    4b89: 48 8b 8d d8 fa ff ff         	mov	rcx, qword ptr [rbp - 1320]
    4b90: 48 89 b5 c8 fa ff ff         	mov	qword ptr [rbp - 1336], rsi
    4b97: 48 89 95 c0 fa ff ff         	mov	qword ptr [rbp - 1344], rdx
    4b9e: 48 8b bd c0 fa ff ff         	mov	rdi, qword ptr [rbp - 1344]
    4ba5: 48 8b b5 c8 fa ff ff         	mov	rsi, qword ptr [rbp - 1336]
    4bac: 48 89 8d b8 fa ff ff         	mov	qword ptr [rbp - 1352], rcx
    4bb3: 48 89 85 b0 fa ff ff         	mov	qword ptr [rbp - 1360], rax
    4bba: 48 8b 95 b0 fa ff ff         	mov	rdx, qword ptr [rbp - 1360]
    4bc1: 48 8b 8d b8 fa ff ff         	mov	rcx, qword ptr [rbp - 1352]
    4bc8: e8 73 ec ff ff               	call	0x3840 <FStar_UInt128_add>
    4bcd: 48 89 85 a0 fa ff ff         	mov	qword ptr [rbp - 1376], rax
    4bd4: 48 89 95 a8 fa ff ff         	mov	qword ptr [rbp - 1368], rdx
    4bdb: 48 8b 85 a0 fa ff ff         	mov	rax, qword ptr [rbp - 1376]
    4be2: 48 8b 8d a8 fa ff ff         	mov	rcx, qword ptr [rbp - 1368]
    4be9: 48 89 8d e8 fa ff ff         	mov	qword ptr [rbp - 1304], rcx
    4bf0: 48 89 85 e0 fa ff ff         	mov	qword ptr [rbp - 1312], rax
    4bf7: 48 8b 85 20 fc ff ff         	mov	rax, qword ptr [rbp - 992]
    4bfe: 48 89 85 80 e6 ff ff         	mov	qword ptr [rbp - 6528], rax
    4c05: 48 8b 85 28 fc ff ff         	mov	rax, qword ptr [rbp - 984]
    4c0c: 48 89 85 78 e6 ff ff         	mov	qword ptr [rbp - 6536], rax
    4c13: 48 8b 7d c0                  	mov	rdi, qword ptr [rbp - 64]
    4c17: 48 8b b5 20 ff ff ff         	mov	rsi, qword ptr [rbp - 224]
    4c1e: e8 7d ec ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4c23: 48 8b b5 78 e6 ff ff         	mov	rsi, qword ptr [rbp - 6536]
    4c2a: 48 89 c1                     	mov	rcx, rax
    4c2d: 48 89 d0                     	mov	rax, rdx
    4c30: 48 8b 95 80 e6 ff ff         	mov	rdx, qword ptr [rbp - 6528]
    4c37: 48 89 8d 80 fa ff ff         	mov	qword ptr [rbp - 1408], rcx
    4c3e: 48 89 85 88 fa ff ff         	mov	qword ptr [rbp - 1400], rax
    4c45: 48 8b 85 80 fa ff ff         	mov	rax, qword ptr [rbp - 1408]
    4c4c: 48 8b 8d 88 fa ff ff         	mov	rcx, qword ptr [rbp - 1400]
    4c53: 48 89 b5 78 fa ff ff         	mov	qword ptr [rbp - 1416], rsi
    4c5a: 48 89 95 70 fa ff ff         	mov	qword ptr [rbp - 1424], rdx
    4c61: 48 8b bd 70 fa ff ff         	mov	rdi, qword ptr [rbp - 1424]
    4c68: 48 8b b5 78 fa ff ff         	mov	rsi, qword ptr [rbp - 1416]
    4c6f: 48 89 8d 68 fa ff ff         	mov	qword ptr [rbp - 1432], rcx
    4c76: 48 89 85 60 fa ff ff         	mov	qword ptr [rbp - 1440], rax
    4c7d: 48 8b 95 60 fa ff ff         	mov	rdx, qword ptr [rbp - 1440]
    4c84: 48 8b 8d 68 fa ff ff         	mov	rcx, qword ptr [rbp - 1432]
    4c8b: e8 b0 eb ff ff               	call	0x3840 <FStar_UInt128_add>
    4c90: 48 89 85 50 fa ff ff         	mov	qword ptr [rbp - 1456], rax
    4c97: 48 89 95 58 fa ff ff         	mov	qword ptr [rbp - 1448], rdx
    4c9e: 48 8b 85 50 fa ff ff         	mov	rax, qword ptr [rbp - 1456]
    4ca5: 48 8b 8d 58 fa ff ff         	mov	rcx, qword ptr [rbp - 1448]
    4cac: 48 89 8d 98 fa ff ff         	mov	qword ptr [rbp - 1384], rcx
    4cb3: 48 89 85 90 fa ff ff         	mov	qword ptr [rbp - 1392], rax
    4cba: 48 8b 85 d0 fb ff ff         	mov	rax, qword ptr [rbp - 1072]
    4cc1: 48 89 85 90 e6 ff ff         	mov	qword ptr [rbp - 6512], rax
    4cc8: 48 8b 85 d8 fb ff ff         	mov	rax, qword ptr [rbp - 1064]
    4ccf: 48 89 85 88 e6 ff ff         	mov	qword ptr [rbp - 6520], rax
    4cd6: 48 8b 7d c0                  	mov	rdi, qword ptr [rbp - 64]
    4cda: 48 8b 75 b0                  	mov	rsi, qword ptr [rbp - 80]
    4cde: e8 bd eb ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4ce3: 48 8b b5 88 e6 ff ff         	mov	rsi, qword ptr [rbp - 6520]
    4cea: 48 89 c1                     	mov	rcx, rax
    4ced: 48 89 d0                     	mov	rax, rdx
    4cf0: 48 8b 95 90 e6 ff ff         	mov	rdx, qword ptr [rbp - 6512]
    4cf7: 48 89 8d 30 fa ff ff         	mov	qword ptr [rbp - 1488], rcx
    4cfe: 48 89 85 38 fa ff ff         	mov	qword ptr [rbp - 1480], rax
    4d05: 48 8b 85 30 fa ff ff         	mov	rax, qword ptr [rbp - 1488]
    4d0c: 48 8b 8d 38 fa ff ff         	mov	rcx, qword ptr [rbp - 1480]
    4d13: 48 89 b5 28 fa ff ff         	mov	qword ptr [rbp - 1496], rsi
    4d1a: 48 89 95 20 fa ff ff         	mov	qword ptr [rbp - 1504], rdx
    4d21: 48 8b bd 20 fa ff ff         	mov	rdi, qword ptr [rbp - 1504]
    4d28: 48 8b b5 28 fa ff ff         	mov	rsi, qword ptr [rbp - 1496]
    4d2f: 48 89 8d 18 fa ff ff         	mov	qword ptr [rbp - 1512], rcx
    4d36: 48 89 85 10 fa ff ff         	mov	qword ptr [rbp - 1520], rax
    4d3d: 48 8b 95 10 fa ff ff         	mov	rdx, qword ptr [rbp - 1520]
    4d44: 48 8b 8d 18 fa ff ff         	mov	rcx, qword ptr [rbp - 1512]
    4d4b: e8 f0 ea ff ff               	call	0x3840 <FStar_UInt128_add>
    4d50: 48 89 85 00 fa ff ff         	mov	qword ptr [rbp - 1536], rax
    4d57: 48 89 95 08 fa ff ff         	mov	qword ptr [rbp - 1528], rdx
    4d5e: 48 8b 85 00 fa ff ff         	mov	rax, qword ptr [rbp - 1536]
    4d65: 48 8b 8d 08 fa ff ff         	mov	rcx, qword ptr [rbp - 1528]
    4d6c: 48 89 8d 48 fa ff ff         	mov	qword ptr [rbp - 1464], rcx
    4d73: 48 89 85 40 fa ff ff         	mov	qword ptr [rbp - 1472], rax
    4d7a: 48 8b 85 80 fb ff ff         	mov	rax, qword ptr [rbp - 1152]
    4d81: 48 89 85 a0 e6 ff ff         	mov	qword ptr [rbp - 6496], rax
    4d88: 48 8b 85 88 fb ff ff         	mov	rax, qword ptr [rbp - 1144]
    4d8f: 48 89 85 98 e6 ff ff         	mov	qword ptr [rbp - 6504], rax
    4d96: 48 8b 7d c0                  	mov	rdi, qword ptr [rbp - 64]
    4d9a: 48 8b 75 a8                  	mov	rsi, qword ptr [rbp - 88]
    4d9e: e8 fd ea ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4da3: 48 8b b5 98 e6 ff ff         	mov	rsi, qword ptr [rbp - 6504]
    4daa: 48 89 c1                     	mov	rcx, rax
    4dad: 48 89 d0                     	mov	rax, rdx
    4db0: 48 8b 95 a0 e6 ff ff         	mov	rdx, qword ptr [rbp - 6496]
    4db7: 48 89 8d e0 f9 ff ff         	mov	qword ptr [rbp - 1568], rcx
    4dbe: 48 89 85 e8 f9 ff ff         	mov	qword ptr [rbp - 1560], rax
    4dc5: 48 8b 85 e0 f9 ff ff         	mov	rax, qword ptr [rbp - 1568]
    4dcc: 48 8b 8d e8 f9 ff ff         	mov	rcx, qword ptr [rbp - 1560]
    4dd3: 48 89 b5 d8 f9 ff ff         	mov	qword ptr [rbp - 1576], rsi
    4dda: 48 89 95 d0 f9 ff ff         	mov	qword ptr [rbp - 1584], rdx
    4de1: 48 8b bd d0 f9 ff ff         	mov	rdi, qword ptr [rbp - 1584]
    4de8: 48 8b b5 d8 f9 ff ff         	mov	rsi, qword ptr [rbp - 1576]
    4def: 48 89 8d c8 f9 ff ff         	mov	qword ptr [rbp - 1592], rcx
    4df6: 48 89 85 c0 f9 ff ff         	mov	qword ptr [rbp - 1600], rax
    4dfd: 48 8b 95 c0 f9 ff ff         	mov	rdx, qword ptr [rbp - 1600]
    4e04: 48 8b 8d c8 f9 ff ff         	mov	rcx, qword ptr [rbp - 1592]
    4e0b: e8 30 ea ff ff               	call	0x3840 <FStar_UInt128_add>
    4e10: 48 89 85 b0 f9 ff ff         	mov	qword ptr [rbp - 1616], rax
    4e17: 48 89 95 b8 f9 ff ff         	mov	qword ptr [rbp - 1608], rdx
    4e1e: 48 8b 85 b0 f9 ff ff         	mov	rax, qword ptr [rbp - 1616]
    4e25: 48 8b 8d b8 f9 ff ff         	mov	rcx, qword ptr [rbp - 1608]
    4e2c: 48 89 8d f8 f9 ff ff         	mov	qword ptr [rbp - 1544], rcx
    4e33: 48 89 85 f0 f9 ff ff         	mov	qword ptr [rbp - 1552], rax
    4e3a: 48 8b 85 30 fb ff ff         	mov	rax, qword ptr [rbp - 1232]
    4e41: 48 89 85 b0 e6 ff ff         	mov	qword ptr [rbp - 6480], rax
    4e48: 48 8b 85 38 fb ff ff         	mov	rax, qword ptr [rbp - 1224]
    4e4f: 48 89 85 a8 e6 ff ff         	mov	qword ptr [rbp - 6488], rax
    4e56: 48 8b 7d b8                  	mov	rdi, qword ptr [rbp - 72]
    4e5a: 48 8b b5 38 ff ff ff         	mov	rsi, qword ptr [rbp - 200]
    4e61: e8 3a ea ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4e66: 48 8b b5 a8 e6 ff ff         	mov	rsi, qword ptr [rbp - 6488]
    4e6d: 48 89 c1                     	mov	rcx, rax
    4e70: 48 89 d0                     	mov	rax, rdx
    4e73: 48 8b 95 b0 e6 ff ff         	mov	rdx, qword ptr [rbp - 6480]
    4e7a: 48 89 8d 90 f9 ff ff         	mov	qword ptr [rbp - 1648], rcx
    4e81: 48 89 85 98 f9 ff ff         	mov	qword ptr [rbp - 1640], rax
    4e88: 48 8b 85 90 f9 ff ff         	mov	rax, qword ptr [rbp - 1648]
    4e8f: 48 8b 8d 98 f9 ff ff         	mov	rcx, qword ptr [rbp - 1640]
    4e96: 48 89 b5 88 f9 ff ff         	mov	qword ptr [rbp - 1656], rsi
    4e9d: 48 89 95 80 f9 ff ff         	mov	qword ptr [rbp - 1664], rdx
    4ea4: 48 8b bd 80 f9 ff ff         	mov	rdi, qword ptr [rbp - 1664]
    4eab: 48 8b b5 88 f9 ff ff         	mov	rsi, qword ptr [rbp - 1656]
    4eb2: 48 89 8d 78 f9 ff ff         	mov	qword ptr [rbp - 1672], rcx
    4eb9: 48 89 85 70 f9 ff ff         	mov	qword ptr [rbp - 1680], rax
    4ec0: 48 8b 95 70 f9 ff ff         	mov	rdx, qword ptr [rbp - 1680]
    4ec7: 48 8b 8d 78 f9 ff ff         	mov	rcx, qword ptr [rbp - 1672]
    4ece: e8 6d e9 ff ff               	call	0x3840 <FStar_UInt128_add>
    4ed3: 48 89 85 60 f9 ff ff         	mov	qword ptr [rbp - 1696], rax
    4eda: 48 89 95 68 f9 ff ff         	mov	qword ptr [rbp - 1688], rdx
    4ee1: 48 8b 85 60 f9 ff ff         	mov	rax, qword ptr [rbp - 1696]
    4ee8: 48 8b 8d 68 f9 ff ff         	mov	rcx, qword ptr [rbp - 1688]
    4eef: 48 89 8d a8 f9 ff ff         	mov	qword ptr [rbp - 1624], rcx
    4ef6: 48 89 85 a0 f9 ff ff         	mov	qword ptr [rbp - 1632], rax
    4efd: 48 8b 85 e0 fa ff ff         	mov	rax, qword ptr [rbp - 1312]
    4f04: 48 89 85 c0 e6 ff ff         	mov	qword ptr [rbp - 6464], rax
    4f0b: 48 8b 85 e8 fa ff ff         	mov	rax, qword ptr [rbp - 1304]
    4f12: 48 89 85 b8 e6 ff ff         	mov	qword ptr [rbp - 6472], rax
    4f19: 48 8b 7d b8                  	mov	rdi, qword ptr [rbp - 72]
    4f1d: 48 8b b5 30 ff ff ff         	mov	rsi, qword ptr [rbp - 208]
    4f24: e8 77 e9 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4f29: 48 8b b5 b8 e6 ff ff         	mov	rsi, qword ptr [rbp - 6472]
    4f30: 48 89 c1                     	mov	rcx, rax
    4f33: 48 89 d0                     	mov	rax, rdx
    4f36: 48 8b 95 c0 e6 ff ff         	mov	rdx, qword ptr [rbp - 6464]
    4f3d: 48 89 8d 40 f9 ff ff         	mov	qword ptr [rbp - 1728], rcx
    4f44: 48 89 85 48 f9 ff ff         	mov	qword ptr [rbp - 1720], rax
    4f4b: 48 8b 85 40 f9 ff ff         	mov	rax, qword ptr [rbp - 1728]
    4f52: 48 8b 8d 48 f9 ff ff         	mov	rcx, qword ptr [rbp - 1720]
    4f59: 48 89 b5 38 f9 ff ff         	mov	qword ptr [rbp - 1736], rsi
    4f60: 48 89 95 30 f9 ff ff         	mov	qword ptr [rbp - 1744], rdx
    4f67: 48 8b bd 30 f9 ff ff         	mov	rdi, qword ptr [rbp - 1744]
    4f6e: 48 8b b5 38 f9 ff ff         	mov	rsi, qword ptr [rbp - 1736]
    4f75: 48 89 8d 28 f9 ff ff         	mov	qword ptr [rbp - 1752], rcx
    4f7c: 48 89 85 20 f9 ff ff         	mov	qword ptr [rbp - 1760], rax
    4f83: 48 8b 95 20 f9 ff ff         	mov	rdx, qword ptr [rbp - 1760]
    4f8a: 48 8b 8d 28 f9 ff ff         	mov	rcx, qword ptr [rbp - 1752]
    4f91: e8 aa e8 ff ff               	call	0x3840 <FStar_UInt128_add>
    4f96: 48 89 85 10 f9 ff ff         	mov	qword ptr [rbp - 1776], rax
    4f9d: 48 89 95 18 f9 ff ff         	mov	qword ptr [rbp - 1768], rdx
    4fa4: 48 8b 85 10 f9 ff ff         	mov	rax, qword ptr [rbp - 1776]
    4fab: 48 8b 8d 18 f9 ff ff         	mov	rcx, qword ptr [rbp - 1768]
    4fb2: 48 89 8d 58 f9 ff ff         	mov	qword ptr [rbp - 1704], rcx
    4fb9: 48 89 85 50 f9 ff ff         	mov	qword ptr [rbp - 1712], rax
    4fc0: 48 8b 85 90 fa ff ff         	mov	rax, qword ptr [rbp - 1392]
    4fc7: 48 89 85 d0 e6 ff ff         	mov	qword ptr [rbp - 6448], rax
    4fce: 48 8b 85 98 fa ff ff         	mov	rax, qword ptr [rbp - 1384]
    4fd5: 48 89 85 c8 e6 ff ff         	mov	qword ptr [rbp - 6456], rax
    4fdc: 48 8b 7d b8                  	mov	rdi, qword ptr [rbp - 72]
    4fe0: 48 8b b5 28 ff ff ff         	mov	rsi, qword ptr [rbp - 216]
    4fe7: e8 b4 e8 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    4fec: 48 8b b5 c8 e6 ff ff         	mov	rsi, qword ptr [rbp - 6456]
    4ff3: 48 89 c1                     	mov	rcx, rax
    4ff6: 48 89 d0                     	mov	rax, rdx
    4ff9: 48 8b 95 d0 e6 ff ff         	mov	rdx, qword ptr [rbp - 6448]
    5000: 48 89 8d f0 f8 ff ff         	mov	qword ptr [rbp - 1808], rcx
    5007: 48 89 85 f8 f8 ff ff         	mov	qword ptr [rbp - 1800], rax
    500e: 48 8b 85 f0 f8 ff ff         	mov	rax, qword ptr [rbp - 1808]
    5015: 48 8b 8d f8 f8 ff ff         	mov	rcx, qword ptr [rbp - 1800]
    501c: 48 89 b5 e8 f8 ff ff         	mov	qword ptr [rbp - 1816], rsi
    5023: 48 89 95 e0 f8 ff ff         	mov	qword ptr [rbp - 1824], rdx
    502a: 48 8b bd e0 f8 ff ff         	mov	rdi, qword ptr [rbp - 1824]
    5031: 48 8b b5 e8 f8 ff ff         	mov	rsi, qword ptr [rbp - 1816]
    5038: 48 89 8d d8 f8 ff ff         	mov	qword ptr [rbp - 1832], rcx
    503f: 48 89 85 d0 f8 ff ff         	mov	qword ptr [rbp - 1840], rax
    5046: 48 8b 95 d0 f8 ff ff         	mov	rdx, qword ptr [rbp - 1840]
    504d: 48 8b 8d d8 f8 ff ff         	mov	rcx, qword ptr [rbp - 1832]
    5054: e8 e7 e7 ff ff               	call	0x3840 <FStar_UInt128_add>
    5059: 48 89 85 c0 f8 ff ff         	mov	qword ptr [rbp - 1856], rax
    5060: 48 89 95 c8 f8 ff ff         	mov	qword ptr [rbp - 1848], rdx
    5067: 48 8b 85 c0 f8 ff ff         	mov	rax, qword ptr [rbp - 1856]
    506e: 48 8b 8d c8 f8 ff ff         	mov	rcx, qword ptr [rbp - 1848]
    5075: 48 89 8d 08 f9 ff ff         	mov	qword ptr [rbp - 1784], rcx
    507c: 48 89 85 00 f9 ff ff         	mov	qword ptr [rbp - 1792], rax
    5083: 48 8b 85 40 fa ff ff         	mov	rax, qword ptr [rbp - 1472]
    508a: 48 89 85 e0 e6 ff ff         	mov	qword ptr [rbp - 6432], rax
    5091: 48 8b 85 48 fa ff ff         	mov	rax, qword ptr [rbp - 1464]
    5098: 48 89 85 d8 e6 ff ff         	mov	qword ptr [rbp - 6440], rax
    509f: 48 8b 7d b8                  	mov	rdi, qword ptr [rbp - 72]
    50a3: 48 8b b5 20 ff ff ff         	mov	rsi, qword ptr [rbp - 224]
    50aa: e8 f1 e7 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    50af: 48 8b b5 d8 e6 ff ff         	mov	rsi, qword ptr [rbp - 6440]
    50b6: 48 89 c1                     	mov	rcx, rax
    50b9: 48 89 d0                     	mov	rax, rdx
    50bc: 48 8b 95 e0 e6 ff ff         	mov	rdx, qword ptr [rbp - 6432]
    50c3: 48 89 8d a0 f8 ff ff         	mov	qword ptr [rbp - 1888], rcx
    50ca: 48 89 85 a8 f8 ff ff         	mov	qword ptr [rbp - 1880], rax
    50d1: 48 8b 85 a0 f8 ff ff         	mov	rax, qword ptr [rbp - 1888]
    50d8: 48 8b 8d a8 f8 ff ff         	mov	rcx, qword ptr [rbp - 1880]
    50df: 48 89 b5 98 f8 ff ff         	mov	qword ptr [rbp - 1896], rsi
    50e6: 48 89 95 90 f8 ff ff         	mov	qword ptr [rbp - 1904], rdx
    50ed: 48 8b bd 90 f8 ff ff         	mov	rdi, qword ptr [rbp - 1904]
    50f4: 48 8b b5 98 f8 ff ff         	mov	rsi, qword ptr [rbp - 1896]
    50fb: 48 89 8d 88 f8 ff ff         	mov	qword ptr [rbp - 1912], rcx
    5102: 48 89 85 80 f8 ff ff         	mov	qword ptr [rbp - 1920], rax
    5109: 48 8b 95 80 f8 ff ff         	mov	rdx, qword ptr [rbp - 1920]
    5110: 48 8b 8d 88 f8 ff ff         	mov	rcx, qword ptr [rbp - 1912]
    5117: e8 24 e7 ff ff               	call	0x3840 <FStar_UInt128_add>
    511c: 48 89 85 70 f8 ff ff         	mov	qword ptr [rbp - 1936], rax
    5123: 48 89 95 78 f8 ff ff         	mov	qword ptr [rbp - 1928], rdx
    512a: 48 8b 85 70 f8 ff ff         	mov	rax, qword ptr [rbp - 1936]
    5131: 48 8b 8d 78 f8 ff ff         	mov	rcx, qword ptr [rbp - 1928]
    5138: 48 89 8d b8 f8 ff ff         	mov	qword ptr [rbp - 1864], rcx
    513f: 48 89 85 b0 f8 ff ff         	mov	qword ptr [rbp - 1872], rax
    5146: 48 8b 85 f0 f9 ff ff         	mov	rax, qword ptr [rbp - 1552]
    514d: 48 89 85 f0 e6 ff ff         	mov	qword ptr [rbp - 6416], rax
    5154: 48 8b 85 f8 f9 ff ff         	mov	rax, qword ptr [rbp - 1544]
    515b: 48 89 85 e8 e6 ff ff         	mov	qword ptr [rbp - 6424], rax
    5162: 48 8b 7d b8                  	mov	rdi, qword ptr [rbp - 72]
    5166: 48 8b 75 b0                  	mov	rsi, qword ptr [rbp - 80]
    516a: e8 31 e7 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    516f: 48 8b b5 e8 e6 ff ff         	mov	rsi, qword ptr [rbp - 6424]
    5176: 48 89 c1                     	mov	rcx, rax
    5179: 48 89 d0                     	mov	rax, rdx
    517c: 48 8b 95 f0 e6 ff ff         	mov	rdx, qword ptr [rbp - 6416]
    5183: 48 89 8d 50 f8 ff ff         	mov	qword ptr [rbp - 1968], rcx
    518a: 48 89 85 58 f8 ff ff         	mov	qword ptr [rbp - 1960], rax
    5191: 48 8b 85 50 f8 ff ff         	mov	rax, qword ptr [rbp - 1968]
    5198: 48 8b 8d 58 f8 ff ff         	mov	rcx, qword ptr [rbp - 1960]
    519f: 48 89 b5 48 f8 ff ff         	mov	qword ptr [rbp - 1976], rsi
    51a6: 48 89 95 40 f8 ff ff         	mov	qword ptr [rbp - 1984], rdx
    51ad: 48 8b bd 40 f8 ff ff         	mov	rdi, qword ptr [rbp - 1984]
    51b4: 48 8b b5 48 f8 ff ff         	mov	rsi, qword ptr [rbp - 1976]
    51bb: 48 89 8d 38 f8 ff ff         	mov	qword ptr [rbp - 1992], rcx
    51c2: 48 89 85 30 f8 ff ff         	mov	qword ptr [rbp - 2000], rax
    51c9: 48 8b 95 30 f8 ff ff         	mov	rdx, qword ptr [rbp - 2000]
    51d0: 48 8b 8d 38 f8 ff ff         	mov	rcx, qword ptr [rbp - 1992]
    51d7: e8 64 e6 ff ff               	call	0x3840 <FStar_UInt128_add>
    51dc: 48 89 85 20 f8 ff ff         	mov	qword ptr [rbp - 2016], rax
    51e3: 48 89 95 28 f8 ff ff         	mov	qword ptr [rbp - 2008], rdx
    51ea: 48 8b 85 20 f8 ff ff         	mov	rax, qword ptr [rbp - 2016]
    51f1: 48 8b 8d 28 f8 ff ff         	mov	rcx, qword ptr [rbp - 2008]
    51f8: 48 89 8d 68 f8 ff ff         	mov	qword ptr [rbp - 1944], rcx
    51ff: 48 89 85 60 f8 ff ff         	mov	qword ptr [rbp - 1952], rax
    5206: 48 8b 85 a0 f9 ff ff         	mov	rax, qword ptr [rbp - 1632]
    520d: 48 8b 8d a8 f9 ff ff         	mov	rcx, qword ptr [rbp - 1624]
    5214: 48 89 8d 18 f8 ff ff         	mov	qword ptr [rbp - 2024], rcx
    521b: 48 89 85 10 f8 ff ff         	mov	qword ptr [rbp - 2032], rax
    5222: 48 8b 85 50 f9 ff ff         	mov	rax, qword ptr [rbp - 1712]
    5229: 48 8b 8d 58 f9 ff ff         	mov	rcx, qword ptr [rbp - 1704]
    5230: 48 89 8d 08 f8 ff ff         	mov	qword ptr [rbp - 2040], rcx
    5237: 48 89 85 00 f8 ff ff         	mov	qword ptr [rbp - 2048], rax
    523e: 48 8b 85 00 f9 ff ff         	mov	rax, qword ptr [rbp - 1792]
    5245: 48 8b 8d 08 f9 ff ff         	mov	rcx, qword ptr [rbp - 1784]
    524c: 48 89 8d f8 f7 ff ff         	mov	qword ptr [rbp - 2056], rcx
    5253: 48 89 85 f0 f7 ff ff         	mov	qword ptr [rbp - 2064], rax
    525a: 48 8b 85 b0 f8 ff ff         	mov	rax, qword ptr [rbp - 1872]
    5261: 48 8b 8d b8 f8 ff ff         	mov	rcx, qword ptr [rbp - 1864]
    5268: 48 89 8d e8 f7 ff ff         	mov	qword ptr [rbp - 2072], rcx
    526f: 48 89 85 e0 f7 ff ff         	mov	qword ptr [rbp - 2080], rax
    5276: 48 8b 85 60 f8 ff ff         	mov	rax, qword ptr [rbp - 1952]
    527d: 48 8b 8d 68 f8 ff ff         	mov	rcx, qword ptr [rbp - 1944]
    5284: 48 89 8d d8 f7 ff ff         	mov	qword ptr [rbp - 2088], rcx
    528b: 48 89 85 d0 f7 ff ff         	mov	qword ptr [rbp - 2096], rax
    5292: 48 8b 7d 88                  	mov	rdi, qword ptr [rbp - 120]
    5296: 48 8b b5 60 ff ff ff         	mov	rsi, qword ptr [rbp - 160]
    529d: e8 fe e5 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    52a2: 48 89 85 b0 f7 ff ff         	mov	qword ptr [rbp - 2128], rax
    52a9: 48 89 95 b8 f7 ff ff         	mov	qword ptr [rbp - 2120], rdx
    52b0: 48 8b 85 b0 f7 ff ff         	mov	rax, qword ptr [rbp - 2128]
    52b7: 48 8b 8d b8 f7 ff ff         	mov	rcx, qword ptr [rbp - 2120]
    52be: 48 89 8d c8 f7 ff ff         	mov	qword ptr [rbp - 2104], rcx
    52c5: 48 89 85 c0 f7 ff ff         	mov	qword ptr [rbp - 2112], rax
    52cc: 48 8b 7d 88                  	mov	rdi, qword ptr [rbp - 120]
    52d0: 48 8b b5 58 ff ff ff         	mov	rsi, qword ptr [rbp - 168]
    52d7: e8 c4 e5 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    52dc: 48 89 85 90 f7 ff ff         	mov	qword ptr [rbp - 2160], rax
    52e3: 48 89 95 98 f7 ff ff         	mov	qword ptr [rbp - 2152], rdx
    52ea: 48 8b 85 90 f7 ff ff         	mov	rax, qword ptr [rbp - 2160]
    52f1: 48 8b 8d 98 f7 ff ff         	mov	rcx, qword ptr [rbp - 2152]
    52f8: 48 89 8d a8 f7 ff ff         	mov	qword ptr [rbp - 2136], rcx
    52ff: 48 89 85 a0 f7 ff ff         	mov	qword ptr [rbp - 2144], rax
    5306: 48 8b 7d 88                  	mov	rdi, qword ptr [rbp - 120]
    530a: 48 8b b5 50 ff ff ff         	mov	rsi, qword ptr [rbp - 176]
    5311: e8 8a e5 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5316: 48 89 85 70 f7 ff ff         	mov	qword ptr [rbp - 2192], rax
    531d: 48 89 95 78 f7 ff ff         	mov	qword ptr [rbp - 2184], rdx
    5324: 48 8b 85 70 f7 ff ff         	mov	rax, qword ptr [rbp - 2192]
    532b: 48 8b 8d 78 f7 ff ff         	mov	rcx, qword ptr [rbp - 2184]
    5332: 48 89 8d 88 f7 ff ff         	mov	qword ptr [rbp - 2168], rcx
    5339: 48 89 85 80 f7 ff ff         	mov	qword ptr [rbp - 2176], rax
    5340: 48 8b 7d 88                  	mov	rdi, qword ptr [rbp - 120]
    5344: 48 8b b5 48 ff ff ff         	mov	rsi, qword ptr [rbp - 184]
    534b: e8 50 e5 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5350: 48 89 85 50 f7 ff ff         	mov	qword ptr [rbp - 2224], rax
    5357: 48 89 95 58 f7 ff ff         	mov	qword ptr [rbp - 2216], rdx
    535e: 48 8b 85 50 f7 ff ff         	mov	rax, qword ptr [rbp - 2224]
    5365: 48 8b 8d 58 f7 ff ff         	mov	rcx, qword ptr [rbp - 2216]
    536c: 48 89 8d 68 f7 ff ff         	mov	qword ptr [rbp - 2200], rcx
    5373: 48 89 85 60 f7 ff ff         	mov	qword ptr [rbp - 2208], rax
    537a: 48 8b 7d 88                  	mov	rdi, qword ptr [rbp - 120]
    537e: 48 8b b5 40 ff ff ff         	mov	rsi, qword ptr [rbp - 192]
    5385: e8 16 e5 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    538a: 48 89 85 30 f7 ff ff         	mov	qword ptr [rbp - 2256], rax
    5391: 48 89 95 38 f7 ff ff         	mov	qword ptr [rbp - 2248], rdx
    5398: 48 8b 85 30 f7 ff ff         	mov	rax, qword ptr [rbp - 2256]
    539f: 48 8b 8d 38 f7 ff ff         	mov	rcx, qword ptr [rbp - 2248]
    53a6: 48 89 8d 48 f7 ff ff         	mov	qword ptr [rbp - 2232], rcx
    53ad: 48 89 85 40 f7 ff ff         	mov	qword ptr [rbp - 2240], rax
    53b4: 48 8b 85 c0 f7 ff ff         	mov	rax, qword ptr [rbp - 2112]
    53bb: 48 89 85 00 e7 ff ff         	mov	qword ptr [rbp - 6400], rax
    53c2: 48 8b 85 c8 f7 ff ff         	mov	rax, qword ptr [rbp - 2104]
    53c9: 48 89 85 f8 e6 ff ff         	mov	qword ptr [rbp - 6408], rax
    53d0: 48 8b 7d 80                  	mov	rdi, qword ptr [rbp - 128]
    53d4: 48 8b b5 00 ff ff ff         	mov	rsi, qword ptr [rbp - 256]
    53db: e8 c0 e4 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    53e0: 48 8b b5 f8 e6 ff ff         	mov	rsi, qword ptr [rbp - 6408]
    53e7: 48 89 c1                     	mov	rcx, rax
    53ea: 48 89 d0                     	mov	rax, rdx
    53ed: 48 8b 95 00 e7 ff ff         	mov	rdx, qword ptr [rbp - 6400]
    53f4: 48 89 8d 10 f7 ff ff         	mov	qword ptr [rbp - 2288], rcx
    53fb: 48 89 85 18 f7 ff ff         	mov	qword ptr [rbp - 2280], rax
    5402: 48 8b 85 10 f7 ff ff         	mov	rax, qword ptr [rbp - 2288]
    5409: 48 8b 8d 18 f7 ff ff         	mov	rcx, qword ptr [rbp - 2280]
    5410: 48 89 b5 08 f7 ff ff         	mov	qword ptr [rbp - 2296], rsi
    5417: 48 89 95 00 f7 ff ff         	mov	qword ptr [rbp - 2304], rdx
    541e: 48 8b bd 00 f7 ff ff         	mov	rdi, qword ptr [rbp - 2304]
    5425: 48 8b b5 08 f7 ff ff         	mov	rsi, qword ptr [rbp - 2296]
    542c: 48 89 8d f8 f6 ff ff         	mov	qword ptr [rbp - 2312], rcx
    5433: 48 89 85 f0 f6 ff ff         	mov	qword ptr [rbp - 2320], rax
    543a: 48 8b 95 f0 f6 ff ff         	mov	rdx, qword ptr [rbp - 2320]
    5441: 48 8b 8d f8 f6 ff ff         	mov	rcx, qword ptr [rbp - 2312]
    5448: e8 f3 e3 ff ff               	call	0x3840 <FStar_UInt128_add>
    544d: 48 89 85 e0 f6 ff ff         	mov	qword ptr [rbp - 2336], rax
    5454: 48 89 95 e8 f6 ff ff         	mov	qword ptr [rbp - 2328], rdx
    545b: 48 8b 85 e0 f6 ff ff         	mov	rax, qword ptr [rbp - 2336]
    5462: 48 8b 8d e8 f6 ff ff         	mov	rcx, qword ptr [rbp - 2328]
    5469: 48 89 8d 28 f7 ff ff         	mov	qword ptr [rbp - 2264], rcx
    5470: 48 89 85 20 f7 ff ff         	mov	qword ptr [rbp - 2272], rax
    5477: 48 8b 85 a0 f7 ff ff         	mov	rax, qword ptr [rbp - 2144]
    547e: 48 89 85 10 e7 ff ff         	mov	qword ptr [rbp - 6384], rax
    5485: 48 8b 85 a8 f7 ff ff         	mov	rax, qword ptr [rbp - 2136]
    548c: 48 89 85 08 e7 ff ff         	mov	qword ptr [rbp - 6392], rax
    5493: 48 8b 7d 80                  	mov	rdi, qword ptr [rbp - 128]
    5497: 48 8b b5 60 ff ff ff         	mov	rsi, qword ptr [rbp - 160]
    549e: e8 fd e3 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    54a3: 48 8b b5 08 e7 ff ff         	mov	rsi, qword ptr [rbp - 6392]
    54aa: 48 89 c1                     	mov	rcx, rax
    54ad: 48 89 d0                     	mov	rax, rdx
    54b0: 48 8b 95 10 e7 ff ff         	mov	rdx, qword ptr [rbp - 6384]
    54b7: 48 89 8d c0 f6 ff ff         	mov	qword ptr [rbp - 2368], rcx
    54be: 48 89 85 c8 f6 ff ff         	mov	qword ptr [rbp - 2360], rax
    54c5: 48 8b 85 c0 f6 ff ff         	mov	rax, qword ptr [rbp - 2368]
    54cc: 48 8b 8d c8 f6 ff ff         	mov	rcx, qword ptr [rbp - 2360]
    54d3: 48 89 b5 b8 f6 ff ff         	mov	qword ptr [rbp - 2376], rsi
    54da: 48 89 95 b0 f6 ff ff         	mov	qword ptr [rbp - 2384], rdx
    54e1: 48 8b bd b0 f6 ff ff         	mov	rdi, qword ptr [rbp - 2384]
    54e8: 48 8b b5 b8 f6 ff ff         	mov	rsi, qword ptr [rbp - 2376]
    54ef: 48 89 8d a8 f6 ff ff         	mov	qword ptr [rbp - 2392], rcx
    54f6: 48 89 85 a0 f6 ff ff         	mov	qword ptr [rbp - 2400], rax
    54fd: 48 8b 95 a0 f6 ff ff         	mov	rdx, qword ptr [rbp - 2400]
    5504: 48 8b 8d a8 f6 ff ff         	mov	rcx, qword ptr [rbp - 2392]
    550b: e8 30 e3 ff ff               	call	0x3840 <FStar_UInt128_add>
    5510: 48 89 85 90 f6 ff ff         	mov	qword ptr [rbp - 2416], rax
    5517: 48 89 95 98 f6 ff ff         	mov	qword ptr [rbp - 2408], rdx
    551e: 48 8b 85 90 f6 ff ff         	mov	rax, qword ptr [rbp - 2416]
    5525: 48 8b 8d 98 f6 ff ff         	mov	rcx, qword ptr [rbp - 2408]
    552c: 48 89 8d d8 f6 ff ff         	mov	qword ptr [rbp - 2344], rcx
    5533: 48 89 85 d0 f6 ff ff         	mov	qword ptr [rbp - 2352], rax
    553a: 48 8b 85 80 f7 ff ff         	mov	rax, qword ptr [rbp - 2176]
    5541: 48 89 85 20 e7 ff ff         	mov	qword ptr [rbp - 6368], rax
    5548: 48 8b 85 88 f7 ff ff         	mov	rax, qword ptr [rbp - 2168]
    554f: 48 89 85 18 e7 ff ff         	mov	qword ptr [rbp - 6376], rax
    5556: 48 8b 7d 80                  	mov	rdi, qword ptr [rbp - 128]
    555a: 48 8b b5 58 ff ff ff         	mov	rsi, qword ptr [rbp - 168]
    5561: e8 3a e3 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5566: 48 8b b5 18 e7 ff ff         	mov	rsi, qword ptr [rbp - 6376]
    556d: 48 89 c1                     	mov	rcx, rax
    5570: 48 89 d0                     	mov	rax, rdx
    5573: 48 8b 95 20 e7 ff ff         	mov	rdx, qword ptr [rbp - 6368]
    557a: 48 89 8d 70 f6 ff ff         	mov	qword ptr [rbp - 2448], rcx
    5581: 48 89 85 78 f6 ff ff         	mov	qword ptr [rbp - 2440], rax
    5588: 48 8b 85 70 f6 ff ff         	mov	rax, qword ptr [rbp - 2448]
    558f: 48 8b 8d 78 f6 ff ff         	mov	rcx, qword ptr [rbp - 2440]
    5596: 48 89 b5 68 f6 ff ff         	mov	qword ptr [rbp - 2456], rsi
    559d: 48 89 95 60 f6 ff ff         	mov	qword ptr [rbp - 2464], rdx
    55a4: 48 8b bd 60 f6 ff ff         	mov	rdi, qword ptr [rbp - 2464]
    55ab: 48 8b b5 68 f6 ff ff         	mov	rsi, qword ptr [rbp - 2456]
    55b2: 48 89 8d 58 f6 ff ff         	mov	qword ptr [rbp - 2472], rcx
    55b9: 48 89 85 50 f6 ff ff         	mov	qword ptr [rbp - 2480], rax
    55c0: 48 8b 95 50 f6 ff ff         	mov	rdx, qword ptr [rbp - 2480]
    55c7: 48 8b 8d 58 f6 ff ff         	mov	rcx, qword ptr [rbp - 2472]
    55ce: e8 6d e2 ff ff               	call	0x3840 <FStar_UInt128_add>
    55d3: 48 89 85 40 f6 ff ff         	mov	qword ptr [rbp - 2496], rax
    55da: 48 89 95 48 f6 ff ff         	mov	qword ptr [rbp - 2488], rdx
    55e1: 48 8b 85 40 f6 ff ff         	mov	rax, qword ptr [rbp - 2496]
    55e8: 48 8b 8d 48 f6 ff ff         	mov	rcx, qword ptr [rbp - 2488]
    55ef: 48 89 8d 88 f6 ff ff         	mov	qword ptr [rbp - 2424], rcx
    55f6: 48 89 85 80 f6 ff ff         	mov	qword ptr [rbp - 2432], rax
    55fd: 48 8b 85 60 f7 ff ff         	mov	rax, qword ptr [rbp - 2208]
    5604: 48 89 85 30 e7 ff ff         	mov	qword ptr [rbp - 6352], rax
    560b: 48 8b 85 68 f7 ff ff         	mov	rax, qword ptr [rbp - 2200]
    5612: 48 89 85 28 e7 ff ff         	mov	qword ptr [rbp - 6360], rax
    5619: 48 8b 7d 80                  	mov	rdi, qword ptr [rbp - 128]
    561d: 48 8b b5 50 ff ff ff         	mov	rsi, qword ptr [rbp - 176]
    5624: e8 77 e2 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5629: 48 8b b5 28 e7 ff ff         	mov	rsi, qword ptr [rbp - 6360]
    5630: 48 89 c1                     	mov	rcx, rax
    5633: 48 89 d0                     	mov	rax, rdx
    5636: 48 8b 95 30 e7 ff ff         	mov	rdx, qword ptr [rbp - 6352]
    563d: 48 89 8d 20 f6 ff ff         	mov	qword ptr [rbp - 2528], rcx
    5644: 48 89 85 28 f6 ff ff         	mov	qword ptr [rbp - 2520], rax
    564b: 48 8b 85 20 f6 ff ff         	mov	rax, qword ptr [rbp - 2528]
    5652: 48 8b 8d 28 f6 ff ff         	mov	rcx, qword ptr [rbp - 2520]
    5659: 48 89 b5 18 f6 ff ff         	mov	qword ptr [rbp - 2536], rsi
    5660: 48 89 95 10 f6 ff ff         	mov	qword ptr [rbp - 2544], rdx
    5667: 48 8b bd 10 f6 ff ff         	mov	rdi, qword ptr [rbp - 2544]
    566e: 48 8b b5 18 f6 ff ff         	mov	rsi, qword ptr [rbp - 2536]
    5675: 48 89 8d 08 f6 ff ff         	mov	qword ptr [rbp - 2552], rcx
    567c: 48 89 85 00 f6 ff ff         	mov	qword ptr [rbp - 2560], rax
    5683: 48 8b 95 00 f6 ff ff         	mov	rdx, qword ptr [rbp - 2560]
    568a: 48 8b 8d 08 f6 ff ff         	mov	rcx, qword ptr [rbp - 2552]
    5691: e8 aa e1 ff ff               	call	0x3840 <FStar_UInt128_add>
    5696: 48 89 85 f0 f5 ff ff         	mov	qword ptr [rbp - 2576], rax
    569d: 48 89 95 f8 f5 ff ff         	mov	qword ptr [rbp - 2568], rdx
    56a4: 48 8b 85 f0 f5 ff ff         	mov	rax, qword ptr [rbp - 2576]
    56ab: 48 8b 8d f8 f5 ff ff         	mov	rcx, qword ptr [rbp - 2568]
    56b2: 48 89 8d 38 f6 ff ff         	mov	qword ptr [rbp - 2504], rcx
    56b9: 48 89 85 30 f6 ff ff         	mov	qword ptr [rbp - 2512], rax
    56c0: 48 8b 85 40 f7 ff ff         	mov	rax, qword ptr [rbp - 2240]
    56c7: 48 89 85 40 e7 ff ff         	mov	qword ptr [rbp - 6336], rax
    56ce: 48 8b 85 48 f7 ff ff         	mov	rax, qword ptr [rbp - 2232]
    56d5: 48 89 85 38 e7 ff ff         	mov	qword ptr [rbp - 6344], rax
    56dc: 48 8b 7d 80                  	mov	rdi, qword ptr [rbp - 128]
    56e0: 48 8b b5 48 ff ff ff         	mov	rsi, qword ptr [rbp - 184]
    56e7: e8 b4 e1 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    56ec: 48 8b b5 38 e7 ff ff         	mov	rsi, qword ptr [rbp - 6344]
    56f3: 48 89 c1                     	mov	rcx, rax
    56f6: 48 89 d0                     	mov	rax, rdx
    56f9: 48 8b 95 40 e7 ff ff         	mov	rdx, qword ptr [rbp - 6336]
    5700: 48 89 8d d0 f5 ff ff         	mov	qword ptr [rbp - 2608], rcx
    5707: 48 89 85 d8 f5 ff ff         	mov	qword ptr [rbp - 2600], rax
    570e: 48 8b 85 d0 f5 ff ff         	mov	rax, qword ptr [rbp - 2608]
    5715: 48 8b 8d d8 f5 ff ff         	mov	rcx, qword ptr [rbp - 2600]
    571c: 48 89 b5 c8 f5 ff ff         	mov	qword ptr [rbp - 2616], rsi
    5723: 48 89 95 c0 f5 ff ff         	mov	qword ptr [rbp - 2624], rdx
    572a: 48 8b bd c0 f5 ff ff         	mov	rdi, qword ptr [rbp - 2624]
    5731: 48 8b b5 c8 f5 ff ff         	mov	rsi, qword ptr [rbp - 2616]
    5738: 48 89 8d b8 f5 ff ff         	mov	qword ptr [rbp - 2632], rcx
    573f: 48 89 85 b0 f5 ff ff         	mov	qword ptr [rbp - 2640], rax
    5746: 48 8b 95 b0 f5 ff ff         	mov	rdx, qword ptr [rbp - 2640]
    574d: 48 8b 8d b8 f5 ff ff         	mov	rcx, qword ptr [rbp - 2632]
    5754: e8 e7 e0 ff ff               	call	0x3840 <FStar_UInt128_add>
    5759: 48 89 85 a0 f5 ff ff         	mov	qword ptr [rbp - 2656], rax
    5760: 48 89 95 a8 f5 ff ff         	mov	qword ptr [rbp - 2648], rdx
    5767: 48 8b 85 a0 f5 ff ff         	mov	rax, qword ptr [rbp - 2656]
    576e: 48 8b 8d a8 f5 ff ff         	mov	rcx, qword ptr [rbp - 2648]
    5775: 48 89 8d e8 f5 ff ff         	mov	qword ptr [rbp - 2584], rcx
    577c: 48 89 85 e0 f5 ff ff         	mov	qword ptr [rbp - 2592], rax
    5783: 48 8b 85 20 f7 ff ff         	mov	rax, qword ptr [rbp - 2272]
    578a: 48 89 85 50 e7 ff ff         	mov	qword ptr [rbp - 6320], rax
    5791: 48 8b 85 28 f7 ff ff         	mov	rax, qword ptr [rbp - 2264]
    5798: 48 89 85 48 e7 ff ff         	mov	qword ptr [rbp - 6328], rax
    579f: 48 8b bd 78 ff ff ff         	mov	rdi, qword ptr [rbp - 136]
    57a6: 48 8b b5 08 ff ff ff         	mov	rsi, qword ptr [rbp - 248]
    57ad: e8 ee e0 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    57b2: 48 8b b5 48 e7 ff ff         	mov	rsi, qword ptr [rbp - 6328]
    57b9: 48 89 c1                     	mov	rcx, rax
    57bc: 48 89 d0                     	mov	rax, rdx
    57bf: 48 8b 95 50 e7 ff ff         	mov	rdx, qword ptr [rbp - 6320]
    57c6: 48 89 8d 80 f5 ff ff         	mov	qword ptr [rbp - 2688], rcx
    57cd: 48 89 85 88 f5 ff ff         	mov	qword ptr [rbp - 2680], rax
    57d4: 48 8b 85 80 f5 ff ff         	mov	rax, qword ptr [rbp - 2688]
    57db: 48 8b 8d 88 f5 ff ff         	mov	rcx, qword ptr [rbp - 2680]
    57e2: 48 89 b5 78 f5 ff ff         	mov	qword ptr [rbp - 2696], rsi
    57e9: 48 89 95 70 f5 ff ff         	mov	qword ptr [rbp - 2704], rdx
    57f0: 48 8b bd 70 f5 ff ff         	mov	rdi, qword ptr [rbp - 2704]
    57f7: 48 8b b5 78 f5 ff ff         	mov	rsi, qword ptr [rbp - 2696]
    57fe: 48 89 8d 68 f5 ff ff         	mov	qword ptr [rbp - 2712], rcx
    5805: 48 89 85 60 f5 ff ff         	mov	qword ptr [rbp - 2720], rax
    580c: 48 8b 95 60 f5 ff ff         	mov	rdx, qword ptr [rbp - 2720]
    5813: 48 8b 8d 68 f5 ff ff         	mov	rcx, qword ptr [rbp - 2712]
    581a: e8 21 e0 ff ff               	call	0x3840 <FStar_UInt128_add>
    581f: 48 89 85 50 f5 ff ff         	mov	qword ptr [rbp - 2736], rax
    5826: 48 89 95 58 f5 ff ff         	mov	qword ptr [rbp - 2728], rdx
    582d: 48 8b 85 50 f5 ff ff         	mov	rax, qword ptr [rbp - 2736]
    5834: 48 8b 8d 58 f5 ff ff         	mov	rcx, qword ptr [rbp - 2728]
    583b: 48 89 8d 98 f5 ff ff         	mov	qword ptr [rbp - 2664], rcx
    5842: 48 89 85 90 f5 ff ff         	mov	qword ptr [rbp - 2672], rax
    5849: 48 8b 85 d0 f6 ff ff         	mov	rax, qword ptr [rbp - 2352]
    5850: 48 89 85 60 e7 ff ff         	mov	qword ptr [rbp - 6304], rax
    5857: 48 8b 85 d8 f6 ff ff         	mov	rax, qword ptr [rbp - 2344]
    585e: 48 89 85 58 e7 ff ff         	mov	qword ptr [rbp - 6312], rax
    5865: 48 8b bd 78 ff ff ff         	mov	rdi, qword ptr [rbp - 136]
    586c: 48 8b b5 00 ff ff ff         	mov	rsi, qword ptr [rbp - 256]
    5873: e8 28 e0 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5878: 48 8b b5 58 e7 ff ff         	mov	rsi, qword ptr [rbp - 6312]
    587f: 48 89 c1                     	mov	rcx, rax
    5882: 48 89 d0                     	mov	rax, rdx
    5885: 48 8b 95 60 e7 ff ff         	mov	rdx, qword ptr [rbp - 6304]
    588c: 48 89 8d 30 f5 ff ff         	mov	qword ptr [rbp - 2768], rcx
    5893: 48 89 85 38 f5 ff ff         	mov	qword ptr [rbp - 2760], rax
    589a: 48 8b 85 30 f5 ff ff         	mov	rax, qword ptr [rbp - 2768]
    58a1: 48 8b 8d 38 f5 ff ff         	mov	rcx, qword ptr [rbp - 2760]
    58a8: 48 89 b5 28 f5 ff ff         	mov	qword ptr [rbp - 2776], rsi
    58af: 48 89 95 20 f5 ff ff         	mov	qword ptr [rbp - 2784], rdx
    58b6: 48 8b bd 20 f5 ff ff         	mov	rdi, qword ptr [rbp - 2784]
    58bd: 48 8b b5 28 f5 ff ff         	mov	rsi, qword ptr [rbp - 2776]
    58c4: 48 89 8d 18 f5 ff ff         	mov	qword ptr [rbp - 2792], rcx
    58cb: 48 89 85 10 f5 ff ff         	mov	qword ptr [rbp - 2800], rax
    58d2: 48 8b 95 10 f5 ff ff         	mov	rdx, qword ptr [rbp - 2800]
    58d9: 48 8b 8d 18 f5 ff ff         	mov	rcx, qword ptr [rbp - 2792]
    58e0: e8 5b df ff ff               	call	0x3840 <FStar_UInt128_add>
    58e5: 48 89 85 00 f5 ff ff         	mov	qword ptr [rbp - 2816], rax
    58ec: 48 89 95 08 f5 ff ff         	mov	qword ptr [rbp - 2808], rdx
    58f3: 48 8b 85 00 f5 ff ff         	mov	rax, qword ptr [rbp - 2816]
    58fa: 48 8b 8d 08 f5 ff ff         	mov	rcx, qword ptr [rbp - 2808]
    5901: 48 89 8d 48 f5 ff ff         	mov	qword ptr [rbp - 2744], rcx
    5908: 48 89 85 40 f5 ff ff         	mov	qword ptr [rbp - 2752], rax
    590f: 48 8b 85 80 f6 ff ff         	mov	rax, qword ptr [rbp - 2432]
    5916: 48 89 85 70 e7 ff ff         	mov	qword ptr [rbp - 6288], rax
    591d: 48 8b 85 88 f6 ff ff         	mov	rax, qword ptr [rbp - 2424]
    5924: 48 89 85 68 e7 ff ff         	mov	qword ptr [rbp - 6296], rax
    592b: 48 8b bd 78 ff ff ff         	mov	rdi, qword ptr [rbp - 136]
    5932: 48 8b b5 60 ff ff ff         	mov	rsi, qword ptr [rbp - 160]
    5939: e8 62 df ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    593e: 48 8b b5 68 e7 ff ff         	mov	rsi, qword ptr [rbp - 6296]
    5945: 48 89 c1                     	mov	rcx, rax
    5948: 48 89 d0                     	mov	rax, rdx
    594b: 48 8b 95 70 e7 ff ff         	mov	rdx, qword ptr [rbp - 6288]
    5952: 48 89 8d e0 f4 ff ff         	mov	qword ptr [rbp - 2848], rcx
    5959: 48 89 85 e8 f4 ff ff         	mov	qword ptr [rbp - 2840], rax
    5960: 48 8b 85 e0 f4 ff ff         	mov	rax, qword ptr [rbp - 2848]
    5967: 48 8b 8d e8 f4 ff ff         	mov	rcx, qword ptr [rbp - 2840]
    596e: 48 89 b5 d8 f4 ff ff         	mov	qword ptr [rbp - 2856], rsi
    5975: 48 89 95 d0 f4 ff ff         	mov	qword ptr [rbp - 2864], rdx
    597c: 48 8b bd d0 f4 ff ff         	mov	rdi, qword ptr [rbp - 2864]
    5983: 48 8b b5 d8 f4 ff ff         	mov	rsi, qword ptr [rbp - 2856]
    598a: 48 89 8d c8 f4 ff ff         	mov	qword ptr [rbp - 2872], rcx
    5991: 48 89 85 c0 f4 ff ff         	mov	qword ptr [rbp - 2880], rax
    5998: 48 8b 95 c0 f4 ff ff         	mov	rdx, qword ptr [rbp - 2880]
    599f: 48 8b 8d c8 f4 ff ff         	mov	rcx, qword ptr [rbp - 2872]
    59a6: e8 95 de ff ff               	call	0x3840 <FStar_UInt128_add>
    59ab: 48 89 85 b0 f4 ff ff         	mov	qword ptr [rbp - 2896], rax
    59b2: 48 89 95 b8 f4 ff ff         	mov	qword ptr [rbp - 2888], rdx
    59b9: 48 8b 85 b0 f4 ff ff         	mov	rax, qword ptr [rbp - 2896]
    59c0: 48 8b 8d b8 f4 ff ff         	mov	rcx, qword ptr [rbp - 2888]
    59c7: 48 89 8d f8 f4 ff ff         	mov	qword ptr [rbp - 2824], rcx
    59ce: 48 89 85 f0 f4 ff ff         	mov	qword ptr [rbp - 2832], rax
    59d5: 48 8b 85 30 f6 ff ff         	mov	rax, qword ptr [rbp - 2512]
    59dc: 48 89 85 80 e7 ff ff         	mov	qword ptr [rbp - 6272], rax
    59e3: 48 8b 85 38 f6 ff ff         	mov	rax, qword ptr [rbp - 2504]
    59ea: 48 89 85 78 e7 ff ff         	mov	qword ptr [rbp - 6280], rax
    59f1: 48 8b bd 78 ff ff ff         	mov	rdi, qword ptr [rbp - 136]
    59f8: 48 8b b5 58 ff ff ff         	mov	rsi, qword ptr [rbp - 168]
    59ff: e8 9c de ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5a04: 48 8b b5 78 e7 ff ff         	mov	rsi, qword ptr [rbp - 6280]
    5a0b: 48 89 c1                     	mov	rcx, rax
    5a0e: 48 89 d0                     	mov	rax, rdx
    5a11: 48 8b 95 80 e7 ff ff         	mov	rdx, qword ptr [rbp - 6272]
    5a18: 48 89 8d 90 f4 ff ff         	mov	qword ptr [rbp - 2928], rcx
    5a1f: 48 89 85 98 f4 ff ff         	mov	qword ptr [rbp - 2920], rax
    5a26: 48 8b 85 90 f4 ff ff         	mov	rax, qword ptr [rbp - 2928]
    5a2d: 48 8b 8d 98 f4 ff ff         	mov	rcx, qword ptr [rbp - 2920]
    5a34: 48 89 b5 88 f4 ff ff         	mov	qword ptr [rbp - 2936], rsi
    5a3b: 48 89 95 80 f4 ff ff         	mov	qword ptr [rbp - 2944], rdx
    5a42: 48 8b bd 80 f4 ff ff         	mov	rdi, qword ptr [rbp - 2944]
    5a49: 48 8b b5 88 f4 ff ff         	mov	rsi, qword ptr [rbp - 2936]
    5a50: 48 89 8d 78 f4 ff ff         	mov	qword ptr [rbp - 2952], rcx
    5a57: 48 89 85 70 f4 ff ff         	mov	qword ptr [rbp - 2960], rax
    5a5e: 48 8b 95 70 f4 ff ff         	mov	rdx, qword ptr [rbp - 2960]
    5a65: 48 8b 8d 78 f4 ff ff         	mov	rcx, qword ptr [rbp - 2952]
    5a6c: e8 cf dd ff ff               	call	0x3840 <FStar_UInt128_add>
    5a71: 48 89 85 60 f4 ff ff         	mov	qword ptr [rbp - 2976], rax
    5a78: 48 89 95 68 f4 ff ff         	mov	qword ptr [rbp - 2968], rdx
    5a7f: 48 8b 85 60 f4 ff ff         	mov	rax, qword ptr [rbp - 2976]
    5a86: 48 8b 8d 68 f4 ff ff         	mov	rcx, qword ptr [rbp - 2968]
    5a8d: 48 89 8d a8 f4 ff ff         	mov	qword ptr [rbp - 2904], rcx
    5a94: 48 89 85 a0 f4 ff ff         	mov	qword ptr [rbp - 2912], rax
    5a9b: 48 8b 85 e0 f5 ff ff         	mov	rax, qword ptr [rbp - 2592]
    5aa2: 48 89 85 90 e7 ff ff         	mov	qword ptr [rbp - 6256], rax
    5aa9: 48 8b 85 e8 f5 ff ff         	mov	rax, qword ptr [rbp - 2584]
    5ab0: 48 89 85 88 e7 ff ff         	mov	qword ptr [rbp - 6264], rax
    5ab7: 48 8b bd 78 ff ff ff         	mov	rdi, qword ptr [rbp - 136]
    5abe: 48 8b b5 50 ff ff ff         	mov	rsi, qword ptr [rbp - 176]
    5ac5: e8 d6 dd ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5aca: 48 8b b5 88 e7 ff ff         	mov	rsi, qword ptr [rbp - 6264]
    5ad1: 48 89 c1                     	mov	rcx, rax
    5ad4: 48 89 d0                     	mov	rax, rdx
    5ad7: 48 8b 95 90 e7 ff ff         	mov	rdx, qword ptr [rbp - 6256]
    5ade: 48 89 8d 40 f4 ff ff         	mov	qword ptr [rbp - 3008], rcx
    5ae5: 48 89 85 48 f4 ff ff         	mov	qword ptr [rbp - 3000], rax
    5aec: 48 8b 85 40 f4 ff ff         	mov	rax, qword ptr [rbp - 3008]
    5af3: 48 8b 8d 48 f4 ff ff         	mov	rcx, qword ptr [rbp - 3000]
    5afa: 48 89 b5 38 f4 ff ff         	mov	qword ptr [rbp - 3016], rsi
    5b01: 48 89 95 30 f4 ff ff         	mov	qword ptr [rbp - 3024], rdx
    5b08: 48 8b bd 30 f4 ff ff         	mov	rdi, qword ptr [rbp - 3024]
    5b0f: 48 8b b5 38 f4 ff ff         	mov	rsi, qword ptr [rbp - 3016]
    5b16: 48 89 8d 28 f4 ff ff         	mov	qword ptr [rbp - 3032], rcx
    5b1d: 48 89 85 20 f4 ff ff         	mov	qword ptr [rbp - 3040], rax
    5b24: 48 8b 95 20 f4 ff ff         	mov	rdx, qword ptr [rbp - 3040]
    5b2b: 48 8b 8d 28 f4 ff ff         	mov	rcx, qword ptr [rbp - 3032]
    5b32: e8 09 dd ff ff               	call	0x3840 <FStar_UInt128_add>
    5b37: 48 89 85 10 f4 ff ff         	mov	qword ptr [rbp - 3056], rax
    5b3e: 48 89 95 18 f4 ff ff         	mov	qword ptr [rbp - 3048], rdx
    5b45: 48 8b 85 10 f4 ff ff         	mov	rax, qword ptr [rbp - 3056]
    5b4c: 48 8b 8d 18 f4 ff ff         	mov	rcx, qword ptr [rbp - 3048]
    5b53: 48 89 8d 58 f4 ff ff         	mov	qword ptr [rbp - 2984], rcx
    5b5a: 48 89 85 50 f4 ff ff         	mov	qword ptr [rbp - 2992], rax
    5b61: 48 8b 85 90 f5 ff ff         	mov	rax, qword ptr [rbp - 2672]
    5b68: 48 89 85 a0 e7 ff ff         	mov	qword ptr [rbp - 6240], rax
    5b6f: 48 8b 85 98 f5 ff ff         	mov	rax, qword ptr [rbp - 2664]
    5b76: 48 89 85 98 e7 ff ff         	mov	qword ptr [rbp - 6248], rax
    5b7d: 48 8b bd 70 ff ff ff         	mov	rdi, qword ptr [rbp - 144]
    5b84: 48 8b b5 10 ff ff ff         	mov	rsi, qword ptr [rbp - 240]
    5b8b: e8 10 dd ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5b90: 48 8b b5 98 e7 ff ff         	mov	rsi, qword ptr [rbp - 6248]
    5b97: 48 89 c1                     	mov	rcx, rax
    5b9a: 48 89 d0                     	mov	rax, rdx
    5b9d: 48 8b 95 a0 e7 ff ff         	mov	rdx, qword ptr [rbp - 6240]
    5ba4: 48 89 8d f0 f3 ff ff         	mov	qword ptr [rbp - 3088], rcx
    5bab: 48 89 85 f8 f3 ff ff         	mov	qword ptr [rbp - 3080], rax
    5bb2: 48 8b 85 f0 f3 ff ff         	mov	rax, qword ptr [rbp - 3088]
    5bb9: 48 8b 8d f8 f3 ff ff         	mov	rcx, qword ptr [rbp - 3080]
    5bc0: 48 89 b5 e8 f3 ff ff         	mov	qword ptr [rbp - 3096], rsi
    5bc7: 48 89 95 e0 f3 ff ff         	mov	qword ptr [rbp - 3104], rdx
    5bce: 48 8b bd e0 f3 ff ff         	mov	rdi, qword ptr [rbp - 3104]
    5bd5: 48 8b b5 e8 f3 ff ff         	mov	rsi, qword ptr [rbp - 3096]
    5bdc: 48 89 8d d8 f3 ff ff         	mov	qword ptr [rbp - 3112], rcx
    5be3: 48 89 85 d0 f3 ff ff         	mov	qword ptr [rbp - 3120], rax
    5bea: 48 8b 95 d0 f3 ff ff         	mov	rdx, qword ptr [rbp - 3120]
    5bf1: 48 8b 8d d8 f3 ff ff         	mov	rcx, qword ptr [rbp - 3112]
    5bf8: e8 43 dc ff ff               	call	0x3840 <FStar_UInt128_add>
    5bfd: 48 89 85 c0 f3 ff ff         	mov	qword ptr [rbp - 3136], rax
    5c04: 48 89 95 c8 f3 ff ff         	mov	qword ptr [rbp - 3128], rdx
    5c0b: 48 8b 85 c0 f3 ff ff         	mov	rax, qword ptr [rbp - 3136]
    5c12: 48 8b 8d c8 f3 ff ff         	mov	rcx, qword ptr [rbp - 3128]
    5c19: 48 89 8d 08 f4 ff ff         	mov	qword ptr [rbp - 3064], rcx
    5c20: 48 89 85 00 f4 ff ff         	mov	qword ptr [rbp - 3072], rax
    5c27: 48 8b 85 40 f5 ff ff         	mov	rax, qword ptr [rbp - 2752]
    5c2e: 48 89 85 b0 e7 ff ff         	mov	qword ptr [rbp - 6224], rax
    5c35: 48 8b 85 48 f5 ff ff         	mov	rax, qword ptr [rbp - 2744]
    5c3c: 48 89 85 a8 e7 ff ff         	mov	qword ptr [rbp - 6232], rax
    5c43: 48 8b bd 70 ff ff ff         	mov	rdi, qword ptr [rbp - 144]
    5c4a: 48 8b b5 08 ff ff ff         	mov	rsi, qword ptr [rbp - 248]
    5c51: e8 4a dc ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5c56: 48 8b b5 a8 e7 ff ff         	mov	rsi, qword ptr [rbp - 6232]
    5c5d: 48 89 c1                     	mov	rcx, rax
    5c60: 48 89 d0                     	mov	rax, rdx
    5c63: 48 8b 95 b0 e7 ff ff         	mov	rdx, qword ptr [rbp - 6224]
    5c6a: 48 89 8d a0 f3 ff ff         	mov	qword ptr [rbp - 3168], rcx
    5c71: 48 89 85 a8 f3 ff ff         	mov	qword ptr [rbp - 3160], rax
    5c78: 48 8b 85 a0 f3 ff ff         	mov	rax, qword ptr [rbp - 3168]
    5c7f: 48 8b 8d a8 f3 ff ff         	mov	rcx, qword ptr [rbp - 3160]
    5c86: 48 89 b5 98 f3 ff ff         	mov	qword ptr [rbp - 3176], rsi
    5c8d: 48 89 95 90 f3 ff ff         	mov	qword ptr [rbp - 3184], rdx
    5c94: 48 8b bd 90 f3 ff ff         	mov	rdi, qword ptr [rbp - 3184]
    5c9b: 48 8b b5 98 f3 ff ff         	mov	rsi, qword ptr [rbp - 3176]
    5ca2: 48 89 8d 88 f3 ff ff         	mov	qword ptr [rbp - 3192], rcx
    5ca9: 48 89 85 80 f3 ff ff         	mov	qword ptr [rbp - 3200], rax
    5cb0: 48 8b 95 80 f3 ff ff         	mov	rdx, qword ptr [rbp - 3200]
    5cb7: 48 8b 8d 88 f3 ff ff         	mov	rcx, qword ptr [rbp - 3192]
    5cbe: e8 7d db ff ff               	call	0x3840 <FStar_UInt128_add>
    5cc3: 48 89 85 70 f3 ff ff         	mov	qword ptr [rbp - 3216], rax
    5cca: 48 89 95 78 f3 ff ff         	mov	qword ptr [rbp - 3208], rdx
    5cd1: 48 8b 85 70 f3 ff ff         	mov	rax, qword ptr [rbp - 3216]
    5cd8: 48 8b 8d 78 f3 ff ff         	mov	rcx, qword ptr [rbp - 3208]
    5cdf: 48 89 8d b8 f3 ff ff         	mov	qword ptr [rbp - 3144], rcx
    5ce6: 48 89 85 b0 f3 ff ff         	mov	qword ptr [rbp - 3152], rax
    5ced: 48 8b 85 f0 f4 ff ff         	mov	rax, qword ptr [rbp - 2832]
    5cf4: 48 89 85 c0 e7 ff ff         	mov	qword ptr [rbp - 6208], rax
    5cfb: 48 8b 85 f8 f4 ff ff         	mov	rax, qword ptr [rbp - 2824]
    5d02: 48 89 85 b8 e7 ff ff         	mov	qword ptr [rbp - 6216], rax
    5d09: 48 8b bd 70 ff ff ff         	mov	rdi, qword ptr [rbp - 144]
    5d10: 48 8b b5 00 ff ff ff         	mov	rsi, qword ptr [rbp - 256]
    5d17: e8 84 db ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5d1c: 48 8b b5 b8 e7 ff ff         	mov	rsi, qword ptr [rbp - 6216]
    5d23: 48 89 c1                     	mov	rcx, rax
    5d26: 48 89 d0                     	mov	rax, rdx
    5d29: 48 8b 95 c0 e7 ff ff         	mov	rdx, qword ptr [rbp - 6208]
    5d30: 48 89 8d 50 f3 ff ff         	mov	qword ptr [rbp - 3248], rcx
    5d37: 48 89 85 58 f3 ff ff         	mov	qword ptr [rbp - 3240], rax
    5d3e: 48 8b 85 50 f3 ff ff         	mov	rax, qword ptr [rbp - 3248]
    5d45: 48 8b 8d 58 f3 ff ff         	mov	rcx, qword ptr [rbp - 3240]
    5d4c: 48 89 b5 48 f3 ff ff         	mov	qword ptr [rbp - 3256], rsi
    5d53: 48 89 95 40 f3 ff ff         	mov	qword ptr [rbp - 3264], rdx
    5d5a: 48 8b bd 40 f3 ff ff         	mov	rdi, qword ptr [rbp - 3264]
    5d61: 48 8b b5 48 f3 ff ff         	mov	rsi, qword ptr [rbp - 3256]
    5d68: 48 89 8d 38 f3 ff ff         	mov	qword ptr [rbp - 3272], rcx
    5d6f: 48 89 85 30 f3 ff ff         	mov	qword ptr [rbp - 3280], rax
    5d76: 48 8b 95 30 f3 ff ff         	mov	rdx, qword ptr [rbp - 3280]
    5d7d: 48 8b 8d 38 f3 ff ff         	mov	rcx, qword ptr [rbp - 3272]
    5d84: e8 b7 da ff ff               	call	0x3840 <FStar_UInt128_add>
    5d89: 48 89 85 20 f3 ff ff         	mov	qword ptr [rbp - 3296], rax
    5d90: 48 89 95 28 f3 ff ff         	mov	qword ptr [rbp - 3288], rdx
    5d97: 48 8b 85 20 f3 ff ff         	mov	rax, qword ptr [rbp - 3296]
    5d9e: 48 8b 8d 28 f3 ff ff         	mov	rcx, qword ptr [rbp - 3288]
    5da5: 48 89 8d 68 f3 ff ff         	mov	qword ptr [rbp - 3224], rcx
    5dac: 48 89 85 60 f3 ff ff         	mov	qword ptr [rbp - 3232], rax
    5db3: 48 8b 85 a0 f4 ff ff         	mov	rax, qword ptr [rbp - 2912]
    5dba: 48 89 85 d0 e7 ff ff         	mov	qword ptr [rbp - 6192], rax
    5dc1: 48 8b 85 a8 f4 ff ff         	mov	rax, qword ptr [rbp - 2904]
    5dc8: 48 89 85 c8 e7 ff ff         	mov	qword ptr [rbp - 6200], rax
    5dcf: 48 8b bd 70 ff ff ff         	mov	rdi, qword ptr [rbp - 144]
    5dd6: 48 8b b5 60 ff ff ff         	mov	rsi, qword ptr [rbp - 160]
    5ddd: e8 be da ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5de2: 48 8b b5 c8 e7 ff ff         	mov	rsi, qword ptr [rbp - 6200]
    5de9: 48 89 c1                     	mov	rcx, rax
    5dec: 48 89 d0                     	mov	rax, rdx
    5def: 48 8b 95 d0 e7 ff ff         	mov	rdx, qword ptr [rbp - 6192]
    5df6: 48 89 8d 00 f3 ff ff         	mov	qword ptr [rbp - 3328], rcx
    5dfd: 48 89 85 08 f3 ff ff         	mov	qword ptr [rbp - 3320], rax
    5e04: 48 8b 85 00 f3 ff ff         	mov	rax, qword ptr [rbp - 3328]
    5e0b: 48 8b 8d 08 f3 ff ff         	mov	rcx, qword ptr [rbp - 3320]
    5e12: 48 89 b5 f8 f2 ff ff         	mov	qword ptr [rbp - 3336], rsi
    5e19: 48 89 95 f0 f2 ff ff         	mov	qword ptr [rbp - 3344], rdx
    5e20: 48 8b bd f0 f2 ff ff         	mov	rdi, qword ptr [rbp - 3344]
    5e27: 48 8b b5 f8 f2 ff ff         	mov	rsi, qword ptr [rbp - 3336]
    5e2e: 48 89 8d e8 f2 ff ff         	mov	qword ptr [rbp - 3352], rcx
    5e35: 48 89 85 e0 f2 ff ff         	mov	qword ptr [rbp - 3360], rax
    5e3c: 48 8b 95 e0 f2 ff ff         	mov	rdx, qword ptr [rbp - 3360]
    5e43: 48 8b 8d e8 f2 ff ff         	mov	rcx, qword ptr [rbp - 3352]
    5e4a: e8 f1 d9 ff ff               	call	0x3840 <FStar_UInt128_add>
    5e4f: 48 89 85 d0 f2 ff ff         	mov	qword ptr [rbp - 3376], rax
    5e56: 48 89 95 d8 f2 ff ff         	mov	qword ptr [rbp - 3368], rdx
    5e5d: 48 8b 85 d0 f2 ff ff         	mov	rax, qword ptr [rbp - 3376]
    5e64: 48 8b 8d d8 f2 ff ff         	mov	rcx, qword ptr [rbp - 3368]
    5e6b: 48 89 8d 18 f3 ff ff         	mov	qword ptr [rbp - 3304], rcx
    5e72: 48 89 85 10 f3 ff ff         	mov	qword ptr [rbp - 3312], rax
    5e79: 48 8b 85 50 f4 ff ff         	mov	rax, qword ptr [rbp - 2992]
    5e80: 48 89 85 e0 e7 ff ff         	mov	qword ptr [rbp - 6176], rax
    5e87: 48 8b 85 58 f4 ff ff         	mov	rax, qword ptr [rbp - 2984]
    5e8e: 48 89 85 d8 e7 ff ff         	mov	qword ptr [rbp - 6184], rax
    5e95: 48 8b bd 70 ff ff ff         	mov	rdi, qword ptr [rbp - 144]
    5e9c: 48 8b b5 58 ff ff ff         	mov	rsi, qword ptr [rbp - 168]
    5ea3: e8 f8 d9 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5ea8: 48 8b b5 d8 e7 ff ff         	mov	rsi, qword ptr [rbp - 6184]
    5eaf: 48 89 c1                     	mov	rcx, rax
    5eb2: 48 89 d0                     	mov	rax, rdx
    5eb5: 48 8b 95 e0 e7 ff ff         	mov	rdx, qword ptr [rbp - 6176]
    5ebc: 48 89 8d b0 f2 ff ff         	mov	qword ptr [rbp - 3408], rcx
    5ec3: 48 89 85 b8 f2 ff ff         	mov	qword ptr [rbp - 3400], rax
    5eca: 48 8b 85 b0 f2 ff ff         	mov	rax, qword ptr [rbp - 3408]
    5ed1: 48 8b 8d b8 f2 ff ff         	mov	rcx, qword ptr [rbp - 3400]
    5ed8: 48 89 b5 a8 f2 ff ff         	mov	qword ptr [rbp - 3416], rsi
    5edf: 48 89 95 a0 f2 ff ff         	mov	qword ptr [rbp - 3424], rdx
    5ee6: 48 8b bd a0 f2 ff ff         	mov	rdi, qword ptr [rbp - 3424]
    5eed: 48 8b b5 a8 f2 ff ff         	mov	rsi, qword ptr [rbp - 3416]
    5ef4: 48 89 8d 98 f2 ff ff         	mov	qword ptr [rbp - 3432], rcx
    5efb: 48 89 85 90 f2 ff ff         	mov	qword ptr [rbp - 3440], rax
    5f02: 48 8b 95 90 f2 ff ff         	mov	rdx, qword ptr [rbp - 3440]
    5f09: 48 8b 8d 98 f2 ff ff         	mov	rcx, qword ptr [rbp - 3432]
    5f10: e8 2b d9 ff ff               	call	0x3840 <FStar_UInt128_add>
    5f15: 48 89 85 80 f2 ff ff         	mov	qword ptr [rbp - 3456], rax
    5f1c: 48 89 95 88 f2 ff ff         	mov	qword ptr [rbp - 3448], rdx
    5f23: 48 8b 85 80 f2 ff ff         	mov	rax, qword ptr [rbp - 3456]
    5f2a: 48 8b 8d 88 f2 ff ff         	mov	rcx, qword ptr [rbp - 3448]
    5f31: 48 89 8d c8 f2 ff ff         	mov	qword ptr [rbp - 3384], rcx
    5f38: 48 89 85 c0 f2 ff ff         	mov	qword ptr [rbp - 3392], rax
    5f3f: 48 8b 85 00 f4 ff ff         	mov	rax, qword ptr [rbp - 3072]
    5f46: 48 89 85 f0 e7 ff ff         	mov	qword ptr [rbp - 6160], rax
    5f4d: 48 8b 85 08 f4 ff ff         	mov	rax, qword ptr [rbp - 3064]
    5f54: 48 89 85 e8 e7 ff ff         	mov	qword ptr [rbp - 6168], rax
    5f5b: 48 8b bd 68 ff ff ff         	mov	rdi, qword ptr [rbp - 152]
    5f62: 48 8b b5 18 ff ff ff         	mov	rsi, qword ptr [rbp - 232]
    5f69: e8 32 d9 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    5f6e: 48 8b b5 e8 e7 ff ff         	mov	rsi, qword ptr [rbp - 6168]
    5f75: 48 89 c1                     	mov	rcx, rax
    5f78: 48 89 d0                     	mov	rax, rdx
    5f7b: 48 8b 95 f0 e7 ff ff         	mov	rdx, qword ptr [rbp - 6160]
    5f82: 48 89 8d 60 f2 ff ff         	mov	qword ptr [rbp - 3488], rcx
    5f89: 48 89 85 68 f2 ff ff         	mov	qword ptr [rbp - 3480], rax
    5f90: 48 8b 85 60 f2 ff ff         	mov	rax, qword ptr [rbp - 3488]
    5f97: 48 8b 8d 68 f2 ff ff         	mov	rcx, qword ptr [rbp - 3480]
    5f9e: 48 89 b5 58 f2 ff ff         	mov	qword ptr [rbp - 3496], rsi
    5fa5: 48 89 95 50 f2 ff ff         	mov	qword ptr [rbp - 3504], rdx
    5fac: 48 8b bd 50 f2 ff ff         	mov	rdi, qword ptr [rbp - 3504]
    5fb3: 48 8b b5 58 f2 ff ff         	mov	rsi, qword ptr [rbp - 3496]
    5fba: 48 89 8d 48 f2 ff ff         	mov	qword ptr [rbp - 3512], rcx
    5fc1: 48 89 85 40 f2 ff ff         	mov	qword ptr [rbp - 3520], rax
    5fc8: 48 8b 95 40 f2 ff ff         	mov	rdx, qword ptr [rbp - 3520]
    5fcf: 48 8b 8d 48 f2 ff ff         	mov	rcx, qword ptr [rbp - 3512]
    5fd6: e8 65 d8 ff ff               	call	0x3840 <FStar_UInt128_add>
    5fdb: 48 89 85 30 f2 ff ff         	mov	qword ptr [rbp - 3536], rax
    5fe2: 48 89 95 38 f2 ff ff         	mov	qword ptr [rbp - 3528], rdx
    5fe9: 48 8b 85 30 f2 ff ff         	mov	rax, qword ptr [rbp - 3536]
    5ff0: 48 8b 8d 38 f2 ff ff         	mov	rcx, qword ptr [rbp - 3528]
    5ff7: 48 89 8d 78 f2 ff ff         	mov	qword ptr [rbp - 3464], rcx
    5ffe: 48 89 85 70 f2 ff ff         	mov	qword ptr [rbp - 3472], rax
    6005: 48 8b 85 b0 f3 ff ff         	mov	rax, qword ptr [rbp - 3152]
    600c: 48 89 85 00 e8 ff ff         	mov	qword ptr [rbp - 6144], rax
    6013: 48 8b 85 b8 f3 ff ff         	mov	rax, qword ptr [rbp - 3144]
    601a: 48 89 85 f8 e7 ff ff         	mov	qword ptr [rbp - 6152], rax
    6021: 48 8b bd 68 ff ff ff         	mov	rdi, qword ptr [rbp - 152]
    6028: 48 8b b5 10 ff ff ff         	mov	rsi, qword ptr [rbp - 240]
    602f: e8 6c d8 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    6034: 48 8b b5 f8 e7 ff ff         	mov	rsi, qword ptr [rbp - 6152]
    603b: 48 89 c1                     	mov	rcx, rax
    603e: 48 89 d0                     	mov	rax, rdx
    6041: 48 8b 95 00 e8 ff ff         	mov	rdx, qword ptr [rbp - 6144]
    6048: 48 89 8d 10 f2 ff ff         	mov	qword ptr [rbp - 3568], rcx
    604f: 48 89 85 18 f2 ff ff         	mov	qword ptr [rbp - 3560], rax
    6056: 48 8b 85 10 f2 ff ff         	mov	rax, qword ptr [rbp - 3568]
    605d: 48 8b 8d 18 f2 ff ff         	mov	rcx, qword ptr [rbp - 3560]
    6064: 48 89 b5 08 f2 ff ff         	mov	qword ptr [rbp - 3576], rsi
    606b: 48 89 95 00 f2 ff ff         	mov	qword ptr [rbp - 3584], rdx
    6072: 48 8b bd 00 f2 ff ff         	mov	rdi, qword ptr [rbp - 3584]
    6079: 48 8b b5 08 f2 ff ff         	mov	rsi, qword ptr [rbp - 3576]
    6080: 48 89 8d f8 f1 ff ff         	mov	qword ptr [rbp - 3592], rcx
    6087: 48 89 85 f0 f1 ff ff         	mov	qword ptr [rbp - 3600], rax
    608e: 48 8b 95 f0 f1 ff ff         	mov	rdx, qword ptr [rbp - 3600]
    6095: 48 8b 8d f8 f1 ff ff         	mov	rcx, qword ptr [rbp - 3592]
    609c: e8 9f d7 ff ff               	call	0x3840 <FStar_UInt128_add>
    60a1: 48 89 85 e0 f1 ff ff         	mov	qword ptr [rbp - 3616], rax
    60a8: 48 89 95 e8 f1 ff ff         	mov	qword ptr [rbp - 3608], rdx
    60af: 48 8b 85 e0 f1 ff ff         	mov	rax, qword ptr [rbp - 3616]
    60b6: 48 8b 8d e8 f1 ff ff         	mov	rcx, qword ptr [rbp - 3608]
    60bd: 48 89 8d 28 f2 ff ff         	mov	qword ptr [rbp - 3544], rcx
    60c4: 48 89 85 20 f2 ff ff         	mov	qword ptr [rbp - 3552], rax
    60cb: 48 8b 85 60 f3 ff ff         	mov	rax, qword ptr [rbp - 3232]
    60d2: 48 89 85 10 e8 ff ff         	mov	qword ptr [rbp - 6128], rax
    60d9: 48 8b 85 68 f3 ff ff         	mov	rax, qword ptr [rbp - 3224]
    60e0: 48 89 85 08 e8 ff ff         	mov	qword ptr [rbp - 6136], rax
    60e7: 48 8b bd 68 ff ff ff         	mov	rdi, qword ptr [rbp - 152]
    60ee: 48 8b b5 08 ff ff ff         	mov	rsi, qword ptr [rbp - 248]
    60f5: e8 a6 d7 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    60fa: 48 8b b5 08 e8 ff ff         	mov	rsi, qword ptr [rbp - 6136]
    6101: 48 89 c1                     	mov	rcx, rax
    6104: 48 89 d0                     	mov	rax, rdx
    6107: 48 8b 95 10 e8 ff ff         	mov	rdx, qword ptr [rbp - 6128]
    610e: 48 89 8d c0 f1 ff ff         	mov	qword ptr [rbp - 3648], rcx
    6115: 48 89 85 c8 f1 ff ff         	mov	qword ptr [rbp - 3640], rax
    611c: 48 8b 85 c0 f1 ff ff         	mov	rax, qword ptr [rbp - 3648]
    6123: 48 8b 8d c8 f1 ff ff         	mov	rcx, qword ptr [rbp - 3640]
    612a: 48 89 b5 b8 f1 ff ff         	mov	qword ptr [rbp - 3656], rsi
    6131: 48 89 95 b0 f1 ff ff         	mov	qword ptr [rbp - 3664], rdx
    6138: 48 8b bd b0 f1 ff ff         	mov	rdi, qword ptr [rbp - 3664]
    613f: 48 8b b5 b8 f1 ff ff         	mov	rsi, qword ptr [rbp - 3656]
    6146: 48 89 8d a8 f1 ff ff         	mov	qword ptr [rbp - 3672], rcx
    614d: 48 89 85 a0 f1 ff ff         	mov	qword ptr [rbp - 3680], rax
    6154: 48 8b 95 a0 f1 ff ff         	mov	rdx, qword ptr [rbp - 3680]
    615b: 48 8b 8d a8 f1 ff ff         	mov	rcx, qword ptr [rbp - 3672]
    6162: e8 d9 d6 ff ff               	call	0x3840 <FStar_UInt128_add>
    6167: 48 89 85 90 f1 ff ff         	mov	qword ptr [rbp - 3696], rax
    616e: 48 89 95 98 f1 ff ff         	mov	qword ptr [rbp - 3688], rdx
    6175: 48 8b 85 90 f1 ff ff         	mov	rax, qword ptr [rbp - 3696]
    617c: 48 8b 8d 98 f1 ff ff         	mov	rcx, qword ptr [rbp - 3688]
    6183: 48 89 8d d8 f1 ff ff         	mov	qword ptr [rbp - 3624], rcx
    618a: 48 89 85 d0 f1 ff ff         	mov	qword ptr [rbp - 3632], rax
    6191: 48 8b 85 10 f3 ff ff         	mov	rax, qword ptr [rbp - 3312]
    6198: 48 89 85 20 e8 ff ff         	mov	qword ptr [rbp - 6112], rax
    619f: 48 8b 85 18 f3 ff ff         	mov	rax, qword ptr [rbp - 3304]
    61a6: 48 89 85 18 e8 ff ff         	mov	qword ptr [rbp - 6120], rax
    61ad: 48 8b bd 68 ff ff ff         	mov	rdi, qword ptr [rbp - 152]
    61b4: 48 8b b5 00 ff ff ff         	mov	rsi, qword ptr [rbp - 256]
    61bb: e8 e0 d6 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    61c0: 48 8b b5 18 e8 ff ff         	mov	rsi, qword ptr [rbp - 6120]
    61c7: 48 89 c1                     	mov	rcx, rax
    61ca: 48 89 d0                     	mov	rax, rdx
    61cd: 48 8b 95 20 e8 ff ff         	mov	rdx, qword ptr [rbp - 6112]
    61d4: 48 89 8d 70 f1 ff ff         	mov	qword ptr [rbp - 3728], rcx
    61db: 48 89 85 78 f1 ff ff         	mov	qword ptr [rbp - 3720], rax
    61e2: 48 8b 85 70 f1 ff ff         	mov	rax, qword ptr [rbp - 3728]
    61e9: 48 8b 8d 78 f1 ff ff         	mov	rcx, qword ptr [rbp - 3720]
    61f0: 48 89 b5 68 f1 ff ff         	mov	qword ptr [rbp - 3736], rsi
    61f7: 48 89 95 60 f1 ff ff         	mov	qword ptr [rbp - 3744], rdx
    61fe: 48 8b bd 60 f1 ff ff         	mov	rdi, qword ptr [rbp - 3744]
    6205: 48 8b b5 68 f1 ff ff         	mov	rsi, qword ptr [rbp - 3736]
    620c: 48 89 8d 58 f1 ff ff         	mov	qword ptr [rbp - 3752], rcx
    6213: 48 89 85 50 f1 ff ff         	mov	qword ptr [rbp - 3760], rax
    621a: 48 8b 95 50 f1 ff ff         	mov	rdx, qword ptr [rbp - 3760]
    6221: 48 8b 8d 58 f1 ff ff         	mov	rcx, qword ptr [rbp - 3752]
    6228: e8 13 d6 ff ff               	call	0x3840 <FStar_UInt128_add>
    622d: 48 89 85 40 f1 ff ff         	mov	qword ptr [rbp - 3776], rax
    6234: 48 89 95 48 f1 ff ff         	mov	qword ptr [rbp - 3768], rdx
    623b: 48 8b 85 40 f1 ff ff         	mov	rax, qword ptr [rbp - 3776]
    6242: 48 8b 8d 48 f1 ff ff         	mov	rcx, qword ptr [rbp - 3768]
    6249: 48 89 8d 88 f1 ff ff         	mov	qword ptr [rbp - 3704], rcx
    6250: 48 89 85 80 f1 ff ff         	mov	qword ptr [rbp - 3712], rax
    6257: 48 8b 85 c0 f2 ff ff         	mov	rax, qword ptr [rbp - 3392]
    625e: 48 89 85 30 e8 ff ff         	mov	qword ptr [rbp - 6096], rax
    6265: 48 8b 85 c8 f2 ff ff         	mov	rax, qword ptr [rbp - 3384]
    626c: 48 89 85 28 e8 ff ff         	mov	qword ptr [rbp - 6104], rax
    6273: 48 8b bd 68 ff ff ff         	mov	rdi, qword ptr [rbp - 152]
    627a: 48 8b b5 60 ff ff ff         	mov	rsi, qword ptr [rbp - 160]
    6281: e8 1a d6 ff ff               	call	0x38a0 <FStar_UInt128_mul_wide>
    6286: 48 8b b5 28 e8 ff ff         	mov	rsi, qword ptr [rbp - 6104]
    628d: 48 89 c1                     	mov	rcx, rax
    6290: 48 89 d0                     	mov	rax, rdx
    6293: 48 8b 95 30 e8 ff ff         	mov	rdx, qword ptr [rbp - 6096]
    629a: 48 89 8d 20 f1 ff ff         	mov	qword ptr [rbp - 3808], rcx
    62a1: 48 89 85 28 f1 ff ff         	mov	qword ptr [rbp - 3800], rax
    62a8: 48 8b 85 20 f1 ff ff         	mov	rax, qword ptr [rbp - 3808]
    62af: 48 8b 8d 28 f1 ff ff         	mov	rcx, qword ptr [rbp - 3800]
    62b6: 48 89 b5 18 f1 ff ff         	mov	qword ptr [rbp - 3816], rsi
    62bd: 48 89 95 10 f1 ff ff         	mov	qword ptr [rbp - 3824], rdx
    62c4: 48 8b bd 10 f1 ff ff         	mov	rdi, qword ptr [rbp - 3824]
    62cb: 48 8b b5 18 f1 ff ff         	mov	rsi, qword ptr [rbp - 3816]
    62d2: 48 89 8d 08 f1 ff ff         	mov	qword ptr [rbp - 3832], rcx
    62d9: 48 89 85 00 f1 ff ff         	mov	qword ptr [rbp - 3840], rax
    62e0: 48 8b 95 00 f1 ff ff         	mov	rdx, qword ptr [rbp - 3840]
    62e7: 48 8b 8d 08 f1 ff ff         	mov	rcx, qword ptr [rbp - 3832]
    62ee: e8 4d d5 ff ff               	call	0x3840 <FStar_UInt128_add>
    62f3: 48 89 85 f0 f0 ff ff         	mov	qword ptr [rbp - 3856], rax
    62fa: 48 89 95 f8 f0 ff ff         	mov	qword ptr [rbp - 3848], rdx
    6301: 48 8b 85 f0 f0 ff ff         	mov	rax, qword ptr [rbp - 3856]
    6308: 48 8b 8d f8 f0 ff ff         	mov	rcx, qword ptr [rbp - 3848]
    630f: 48 89 8d 38 f1 ff ff         	mov	qword ptr [rbp - 3784], rcx
    6316: 48 89 85 30 f1 ff ff         	mov	qword ptr [rbp - 3792], rax
    631d: 48 8b 85 70 f2 ff ff         	mov	rax, qword ptr [rbp - 3472]
    6324: 48 8b 8d 78 f2 ff ff         	mov	rcx, qword ptr [rbp - 3464]
    632b: 48 89 8d e8 f0 ff ff         	mov	qword ptr [rbp - 3864], rcx
    6332: 48 89 85 e0 f0 ff ff         	mov	qword ptr [rbp - 3872], rax
    6339: 48 8b 85 20 f2 ff ff         	mov	rax, qword ptr [rbp - 3552]
    6340: 48 8b 8d 28 f2 ff ff         	mov	rcx, qword ptr [rbp - 3544]
    6347: 48 89 8d d8 f0 ff ff         	mov	qword ptr [rbp - 3880], rcx
    634e: 48 89 85 d0 f0 ff ff         	mov	qword ptr [rbp - 3888], rax
    6355: 48 8b 85 d0 f1 ff ff         	mov	rax, qword ptr [rbp - 3632]
    635c: 48 8b 8d d8 f1 ff ff         	mov	rcx, qword ptr [rbp - 3624]
    6363: 48 89 8d c8 f0 ff ff         	mov	qword ptr [rbp - 3896], rcx
    636a: 48 89 85 c0 f0 ff ff         	mov	qword ptr [rbp - 3904], rax
    6371: 48 8b 85 80 f1 ff ff         	mov	rax, qword ptr [rbp - 3712]
    6378: 48 8b 8d 88 f1 ff ff         	mov	rcx, qword ptr [rbp - 3704]
    637f: 48 89 8d b8 f0 ff ff         	mov	qword ptr [rbp - 3912], rcx
    6386: 48 89 85 b0 f0 ff ff         	mov	qword ptr [rbp - 3920], rax
    638d: 48 8b 85 30 f1 ff ff         	mov	rax, qword ptr [rbp - 3792]
    6394: 48 8b 8d 38 f1 ff ff         	mov	rcx, qword ptr [rbp - 3784]
    639b: 48 89 8d a8 f0 ff ff         	mov	qword ptr [rbp - 3928], rcx
    63a2: 48 89 85 a0 f0 ff ff         	mov	qword ptr [rbp - 3936], rax
    63a9: 48 8b 85 10 f8 ff ff         	mov	rax, qword ptr [rbp - 2032]
    63b0: 48 89 85 40 e8 ff ff         	mov	qword ptr [rbp - 6080], rax
    63b7: 48 8b 85 18 f8 ff ff         	mov	rax, qword ptr [rbp - 2024]
    63be: 48 89 85 38 e8 ff ff         	mov	qword ptr [rbp - 6088], rax
    63c5: 31 c0                        	xor	eax, eax
    63c7: 89 c7                        	mov	edi, eax
    63c9: 48 89 bd 88 e8 ff ff         	mov	qword ptr [rbp - 6008], rdi
    63d0: e8 fb d4 ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    63d5: 48 8b b5 38 e8 ff ff         	mov	rsi, qword ptr [rbp - 6088]
    63dc: 48 89 c1                     	mov	rcx, rax
    63df: 48 89 d0                     	mov	rax, rdx
    63e2: 48 8b 95 40 e8 ff ff         	mov	rdx, qword ptr [rbp - 6080]
    63e9: 48 89 8d 80 f0 ff ff         	mov	qword ptr [rbp - 3968], rcx
    63f0: 48 89 85 88 f0 ff ff         	mov	qword ptr [rbp - 3960], rax
    63f7: 48 8b 85 80 f0 ff ff         	mov	rax, qword ptr [rbp - 3968]
    63fe: 48 8b 8d 88 f0 ff ff         	mov	rcx, qword ptr [rbp - 3960]
    6405: 48 89 b5 78 f0 ff ff         	mov	qword ptr [rbp - 3976], rsi
    640c: 48 89 95 70 f0 ff ff         	mov	qword ptr [rbp - 3984], rdx
    6413: 48 8b bd 70 f0 ff ff         	mov	rdi, qword ptr [rbp - 3984]
    641a: 48 8b b5 78 f0 ff ff         	mov	rsi, qword ptr [rbp - 3976]
    6421: 48 89 8d 68 f0 ff ff         	mov	qword ptr [rbp - 3992], rcx
    6428: 48 89 85 60 f0 ff ff         	mov	qword ptr [rbp - 4000], rax
    642f: 48 8b 95 60 f0 ff ff         	mov	rdx, qword ptr [rbp - 4000]
    6436: 48 8b 8d 68 f0 ff ff         	mov	rcx, qword ptr [rbp - 3992]
    643d: e8 fe d3 ff ff               	call	0x3840 <FStar_UInt128_add>
    6442: 48 89 85 50 f0 ff ff         	mov	qword ptr [rbp - 4016], rax
    6449: 48 89 95 58 f0 ff ff         	mov	qword ptr [rbp - 4008], rdx
    6450: 48 8b 85 50 f0 ff ff         	mov	rax, qword ptr [rbp - 4016]
    6457: 48 8b 8d 58 f0 ff ff         	mov	rcx, qword ptr [rbp - 4008]
    645e: 48 89 8d 98 f0 ff ff         	mov	qword ptr [rbp - 3944], rcx
    6465: 48 89 85 90 f0 ff ff         	mov	qword ptr [rbp - 3952], rax
    646c: 48 8b 85 90 f0 ff ff         	mov	rax, qword ptr [rbp - 3952]
    6473: 48 8b 8d 98 f0 ff ff         	mov	rcx, qword ptr [rbp - 3944]
    647a: 48 89 8d 38 f0 ff ff         	mov	qword ptr [rbp - 4040], rcx
    6481: 48 89 85 30 f0 ff ff         	mov	qword ptr [rbp - 4048], rax
    6488: 48 8b bd 30 f0 ff ff         	mov	rdi, qword ptr [rbp - 4048]
    648f: 48 8b b5 38 f0 ff ff         	mov	rsi, qword ptr [rbp - 4040]
    6496: e8 65 d4 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    649b: 48 b9 ff ff ff ff ff ff 07 00	movabs	rcx, 2251799813685247
    64a5: 48 89 8d e0 e8 ff ff         	mov	qword ptr [rbp - 5920], rcx
    64ac: 48 21 c8                     	and	rax, rcx
    64af: 48 89 85 48 f0 ff ff         	mov	qword ptr [rbp - 4024], rax
    64b6: 48 8b 85 90 f0 ff ff         	mov	rax, qword ptr [rbp - 3952]
    64bd: 48 8b 8d 98 f0 ff ff         	mov	rcx, qword ptr [rbp - 3944]
    64c4: 48 89 8d 18 f0 ff ff         	mov	qword ptr [rbp - 4072], rcx
    64cb: 48 89 85 10 f0 ff ff         	mov	qword ptr [rbp - 4080], rax
    64d2: 48 8b bd 10 f0 ff ff         	mov	rdi, qword ptr [rbp - 4080]
    64d9: 48 8b b5 18 f0 ff ff         	mov	rsi, qword ptr [rbp - 4072]
    64e0: ba 33 00 00 00               	mov	edx, 51
    64e5: 89 95 ec e8 ff ff            	mov	dword ptr [rbp - 5908], edx
    64eb: e8 40 d4 ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    64f0: 48 89 85 00 f0 ff ff         	mov	qword ptr [rbp - 4096], rax
    64f7: 48 89 95 08 f0 ff ff         	mov	qword ptr [rbp - 4088], rdx
    64fe: 48 8b 85 00 f0 ff ff         	mov	rax, qword ptr [rbp - 4096]
    6505: 48 8b 8d 08 f0 ff ff         	mov	rcx, qword ptr [rbp - 4088]
    650c: 48 89 8d f8 ef ff ff         	mov	qword ptr [rbp - 4104], rcx
    6513: 48 89 85 f0 ef ff ff         	mov	qword ptr [rbp - 4112], rax
    651a: 48 8b bd f0 ef ff ff         	mov	rdi, qword ptr [rbp - 4112]
    6521: 48 8b b5 f8 ef ff ff         	mov	rsi, qword ptr [rbp - 4104]
    6528: e8 d3 d3 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    652d: 48 89 85 28 f0 ff ff         	mov	qword ptr [rbp - 4056], rax
    6534: 48 8b 85 00 f8 ff ff         	mov	rax, qword ptr [rbp - 2048]
    653b: 48 89 85 50 e8 ff ff         	mov	qword ptr [rbp - 6064], rax
    6542: 48 8b 85 08 f8 ff ff         	mov	rax, qword ptr [rbp - 2040]
    6549: 48 89 85 48 e8 ff ff         	mov	qword ptr [rbp - 6072], rax
    6550: 48 8b bd 28 f0 ff ff         	mov	rdi, qword ptr [rbp - 4056]
    6557: e8 74 d3 ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    655c: 48 8b b5 48 e8 ff ff         	mov	rsi, qword ptr [rbp - 6072]
    6563: 48 89 c1                     	mov	rcx, rax
    6566: 48 89 d0                     	mov	rax, rdx
    6569: 48 8b 95 50 e8 ff ff         	mov	rdx, qword ptr [rbp - 6064]
    6570: 48 89 8d d0 ef ff ff         	mov	qword ptr [rbp - 4144], rcx
    6577: 48 89 85 d8 ef ff ff         	mov	qword ptr [rbp - 4136], rax
    657e: 48 8b 85 d0 ef ff ff         	mov	rax, qword ptr [rbp - 4144]
    6585: 48 8b 8d d8 ef ff ff         	mov	rcx, qword ptr [rbp - 4136]
    658c: 48 89 b5 c8 ef ff ff         	mov	qword ptr [rbp - 4152], rsi
    6593: 48 89 95 c0 ef ff ff         	mov	qword ptr [rbp - 4160], rdx
    659a: 48 8b bd c0 ef ff ff         	mov	rdi, qword ptr [rbp - 4160]
    65a1: 48 8b b5 c8 ef ff ff         	mov	rsi, qword ptr [rbp - 4152]
    65a8: 48 89 8d b8 ef ff ff         	mov	qword ptr [rbp - 4168], rcx
    65af: 48 89 85 b0 ef ff ff         	mov	qword ptr [rbp - 4176], rax
    65b6: 48 8b 95 b0 ef ff ff         	mov	rdx, qword ptr [rbp - 4176]
    65bd: 48 8b 8d b8 ef ff ff         	mov	rcx, qword ptr [rbp - 4168]
    65c4: e8 77 d2 ff ff               	call	0x3840 <FStar_UInt128_add>
    65c9: 48 89 85 a0 ef ff ff         	mov	qword ptr [rbp - 4192], rax
    65d0: 48 89 95 a8 ef ff ff         	mov	qword ptr [rbp - 4184], rdx
    65d7: 48 8b 85 a0 ef ff ff         	mov	rax, qword ptr [rbp - 4192]
    65de: 48 8b 8d a8 ef ff ff         	mov	rcx, qword ptr [rbp - 4184]
    65e5: 48 89 8d e8 ef ff ff         	mov	qword ptr [rbp - 4120], rcx
    65ec: 48 89 85 e0 ef ff ff         	mov	qword ptr [rbp - 4128], rax
    65f3: 48 8b 85 e0 ef ff ff         	mov	rax, qword ptr [rbp - 4128]
    65fa: 48 8b 8d e8 ef ff ff         	mov	rcx, qword ptr [rbp - 4120]
    6601: 48 89 8d 88 ef ff ff         	mov	qword ptr [rbp - 4216], rcx
    6608: 48 89 85 80 ef ff ff         	mov	qword ptr [rbp - 4224], rax
    660f: 48 8b bd 80 ef ff ff         	mov	rdi, qword ptr [rbp - 4224]
    6616: 48 8b b5 88 ef ff ff         	mov	rsi, qword ptr [rbp - 4216]
    661d: e8 de d2 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6622: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    6629: 8b 95 ec e8 ff ff            	mov	edx, dword ptr [rbp - 5908]
    662f: 48 21 c8                     	and	rax, rcx
    6632: 48 89 85 98 ef ff ff         	mov	qword ptr [rbp - 4200], rax
    6639: 48 8b 85 e0 ef ff ff         	mov	rax, qword ptr [rbp - 4128]
    6640: 48 8b 8d e8 ef ff ff         	mov	rcx, qword ptr [rbp - 4120]
    6647: 48 89 8d 68 ef ff ff         	mov	qword ptr [rbp - 4248], rcx
    664e: 48 89 85 60 ef ff ff         	mov	qword ptr [rbp - 4256], rax
    6655: 48 8b bd 60 ef ff ff         	mov	rdi, qword ptr [rbp - 4256]
    665c: 48 8b b5 68 ef ff ff         	mov	rsi, qword ptr [rbp - 4248]
    6663: e8 c8 d2 ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    6668: 48 89 85 50 ef ff ff         	mov	qword ptr [rbp - 4272], rax
    666f: 48 89 95 58 ef ff ff         	mov	qword ptr [rbp - 4264], rdx
    6676: 48 8b 85 50 ef ff ff         	mov	rax, qword ptr [rbp - 4272]
    667d: 48 8b 8d 58 ef ff ff         	mov	rcx, qword ptr [rbp - 4264]
    6684: 48 89 8d 48 ef ff ff         	mov	qword ptr [rbp - 4280], rcx
    668b: 48 89 85 40 ef ff ff         	mov	qword ptr [rbp - 4288], rax
    6692: 48 8b bd 40 ef ff ff         	mov	rdi, qword ptr [rbp - 4288]
    6699: 48 8b b5 48 ef ff ff         	mov	rsi, qword ptr [rbp - 4280]
    66a0: e8 5b d2 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    66a5: 48 89 85 78 ef ff ff         	mov	qword ptr [rbp - 4232], rax
    66ac: 48 8b 85 f0 f7 ff ff         	mov	rax, qword ptr [rbp - 2064]
    66b3: 48 89 85 60 e8 ff ff         	mov	qword ptr [rbp - 6048], rax
    66ba: 48 8b 85 f8 f7 ff ff         	mov	rax, qword ptr [rbp - 2056]
    66c1: 48 89 85 58 e8 ff ff         	mov	qword ptr [rbp - 6056], rax
    66c8: 48 8b bd 78 ef ff ff         	mov	rdi, qword ptr [rbp - 4232]
    66cf: e8 fc d1 ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    66d4: 48 8b b5 58 e8 ff ff         	mov	rsi, qword ptr [rbp - 6056]
    66db: 48 89 c1                     	mov	rcx, rax
    66de: 48 89 d0                     	mov	rax, rdx
    66e1: 48 8b 95 60 e8 ff ff         	mov	rdx, qword ptr [rbp - 6048]
    66e8: 48 89 8d 20 ef ff ff         	mov	qword ptr [rbp - 4320], rcx
    66ef: 48 89 85 28 ef ff ff         	mov	qword ptr [rbp - 4312], rax
    66f6: 48 8b 85 20 ef ff ff         	mov	rax, qword ptr [rbp - 4320]
    66fd: 48 8b 8d 28 ef ff ff         	mov	rcx, qword ptr [rbp - 4312]
    6704: 48 89 b5 18 ef ff ff         	mov	qword ptr [rbp - 4328], rsi
    670b: 48 89 95 10 ef ff ff         	mov	qword ptr [rbp - 4336], rdx
    6712: 48 8b bd 10 ef ff ff         	mov	rdi, qword ptr [rbp - 4336]
    6719: 48 8b b5 18 ef ff ff         	mov	rsi, qword ptr [rbp - 4328]
    6720: 48 89 8d 08 ef ff ff         	mov	qword ptr [rbp - 4344], rcx
    6727: 48 89 85 00 ef ff ff         	mov	qword ptr [rbp - 4352], rax
    672e: 48 8b 95 00 ef ff ff         	mov	rdx, qword ptr [rbp - 4352]
    6735: 48 8b 8d 08 ef ff ff         	mov	rcx, qword ptr [rbp - 4344]
    673c: e8 ff d0 ff ff               	call	0x3840 <FStar_UInt128_add>
    6741: 48 89 85 f0 ee ff ff         	mov	qword ptr [rbp - 4368], rax
    6748: 48 89 95 f8 ee ff ff         	mov	qword ptr [rbp - 4360], rdx
    674f: 48 8b 85 f0 ee ff ff         	mov	rax, qword ptr [rbp - 4368]
    6756: 48 8b 8d f8 ee ff ff         	mov	rcx, qword ptr [rbp - 4360]
    675d: 48 89 8d 38 ef ff ff         	mov	qword ptr [rbp - 4296], rcx
    6764: 48 89 85 30 ef ff ff         	mov	qword ptr [rbp - 4304], rax
    676b: 48 8b 85 30 ef ff ff         	mov	rax, qword ptr [rbp - 4304]
    6772: 48 8b 8d 38 ef ff ff         	mov	rcx, qword ptr [rbp - 4296]
    6779: 48 89 8d d8 ee ff ff         	mov	qword ptr [rbp - 4392], rcx
    6780: 48 89 85 d0 ee ff ff         	mov	qword ptr [rbp - 4400], rax
    6787: 48 8b bd d0 ee ff ff         	mov	rdi, qword ptr [rbp - 4400]
    678e: 48 8b b5 d8 ee ff ff         	mov	rsi, qword ptr [rbp - 4392]
    6795: e8 66 d1 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    679a: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    67a1: 8b 95 ec e8 ff ff            	mov	edx, dword ptr [rbp - 5908]
    67a7: 48 21 c8                     	and	rax, rcx
    67aa: 48 89 85 e8 ee ff ff         	mov	qword ptr [rbp - 4376], rax
    67b1: 48 8b 85 30 ef ff ff         	mov	rax, qword ptr [rbp - 4304]
    67b8: 48 8b 8d 38 ef ff ff         	mov	rcx, qword ptr [rbp - 4296]
    67bf: 48 89 8d b8 ee ff ff         	mov	qword ptr [rbp - 4424], rcx
    67c6: 48 89 85 b0 ee ff ff         	mov	qword ptr [rbp - 4432], rax
    67cd: 48 8b bd b0 ee ff ff         	mov	rdi, qword ptr [rbp - 4432]
    67d4: 48 8b b5 b8 ee ff ff         	mov	rsi, qword ptr [rbp - 4424]
    67db: e8 50 d1 ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    67e0: 48 89 85 a0 ee ff ff         	mov	qword ptr [rbp - 4448], rax
    67e7: 48 89 95 a8 ee ff ff         	mov	qword ptr [rbp - 4440], rdx
    67ee: 48 8b 85 a0 ee ff ff         	mov	rax, qword ptr [rbp - 4448]
    67f5: 48 8b 8d a8 ee ff ff         	mov	rcx, qword ptr [rbp - 4440]
    67fc: 48 89 8d 98 ee ff ff         	mov	qword ptr [rbp - 4456], rcx
    6803: 48 89 85 90 ee ff ff         	mov	qword ptr [rbp - 4464], rax
    680a: 48 8b bd 90 ee ff ff         	mov	rdi, qword ptr [rbp - 4464]
    6811: 48 8b b5 98 ee ff ff         	mov	rsi, qword ptr [rbp - 4456]
    6818: e8 e3 d0 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    681d: 48 89 85 c8 ee ff ff         	mov	qword ptr [rbp - 4408], rax
    6824: 48 8b 85 e0 f7 ff ff         	mov	rax, qword ptr [rbp - 2080]
    682b: 48 89 85 70 e8 ff ff         	mov	qword ptr [rbp - 6032], rax
    6832: 48 8b 85 e8 f7 ff ff         	mov	rax, qword ptr [rbp - 2072]
    6839: 48 89 85 68 e8 ff ff         	mov	qword ptr [rbp - 6040], rax
    6840: 48 8b bd c8 ee ff ff         	mov	rdi, qword ptr [rbp - 4408]
    6847: e8 84 d0 ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    684c: 48 8b b5 68 e8 ff ff         	mov	rsi, qword ptr [rbp - 6040]
    6853: 48 89 c1                     	mov	rcx, rax
    6856: 48 89 d0                     	mov	rax, rdx
    6859: 48 8b 95 70 e8 ff ff         	mov	rdx, qword ptr [rbp - 6032]
    6860: 48 89 8d 70 ee ff ff         	mov	qword ptr [rbp - 4496], rcx
    6867: 48 89 85 78 ee ff ff         	mov	qword ptr [rbp - 4488], rax
    686e: 48 8b 85 70 ee ff ff         	mov	rax, qword ptr [rbp - 4496]
    6875: 48 8b 8d 78 ee ff ff         	mov	rcx, qword ptr [rbp - 4488]
    687c: 48 89 b5 68 ee ff ff         	mov	qword ptr [rbp - 4504], rsi
    6883: 48 89 95 60 ee ff ff         	mov	qword ptr [rbp - 4512], rdx
    688a: 48 8b bd 60 ee ff ff         	mov	rdi, qword ptr [rbp - 4512]
    6891: 48 8b b5 68 ee ff ff         	mov	rsi, qword ptr [rbp - 4504]
    6898: 48 89 8d 58 ee ff ff         	mov	qword ptr [rbp - 4520], rcx
    689f: 48 89 85 50 ee ff ff         	mov	qword ptr [rbp - 4528], rax
    68a6: 48 8b 95 50 ee ff ff         	mov	rdx, qword ptr [rbp - 4528]
    68ad: 48 8b 8d 58 ee ff ff         	mov	rcx, qword ptr [rbp - 4520]
    68b4: e8 87 cf ff ff               	call	0x3840 <FStar_UInt128_add>
    68b9: 48 89 85 40 ee ff ff         	mov	qword ptr [rbp - 4544], rax
    68c0: 48 89 95 48 ee ff ff         	mov	qword ptr [rbp - 4536], rdx
    68c7: 48 8b 85 40 ee ff ff         	mov	rax, qword ptr [rbp - 4544]
    68ce: 48 8b 8d 48 ee ff ff         	mov	rcx, qword ptr [rbp - 4536]
    68d5: 48 89 8d 88 ee ff ff         	mov	qword ptr [rbp - 4472], rcx
    68dc: 48 89 85 80 ee ff ff         	mov	qword ptr [rbp - 4480], rax
    68e3: 48 8b 85 80 ee ff ff         	mov	rax, qword ptr [rbp - 4480]
    68ea: 48 8b 8d 88 ee ff ff         	mov	rcx, qword ptr [rbp - 4472]
    68f1: 48 89 8d 28 ee ff ff         	mov	qword ptr [rbp - 4568], rcx
    68f8: 48 89 85 20 ee ff ff         	mov	qword ptr [rbp - 4576], rax
    68ff: 48 8b bd 20 ee ff ff         	mov	rdi, qword ptr [rbp - 4576]
    6906: 48 8b b5 28 ee ff ff         	mov	rsi, qword ptr [rbp - 4568]
    690d: e8 ee cf ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6912: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    6919: 8b 95 ec e8 ff ff            	mov	edx, dword ptr [rbp - 5908]
    691f: 48 21 c8                     	and	rax, rcx
    6922: 48 89 85 38 ee ff ff         	mov	qword ptr [rbp - 4552], rax
    6929: 48 8b 85 80 ee ff ff         	mov	rax, qword ptr [rbp - 4480]
    6930: 48 8b 8d 88 ee ff ff         	mov	rcx, qword ptr [rbp - 4472]
    6937: 48 89 8d 08 ee ff ff         	mov	qword ptr [rbp - 4600], rcx
    693e: 48 89 85 00 ee ff ff         	mov	qword ptr [rbp - 4608], rax
    6945: 48 8b bd 00 ee ff ff         	mov	rdi, qword ptr [rbp - 4608]
    694c: 48 8b b5 08 ee ff ff         	mov	rsi, qword ptr [rbp - 4600]
    6953: e8 d8 cf ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    6958: 48 89 85 f0 ed ff ff         	mov	qword ptr [rbp - 4624], rax
    695f: 48 89 95 f8 ed ff ff         	mov	qword ptr [rbp - 4616], rdx
    6966: 48 8b 85 f0 ed ff ff         	mov	rax, qword ptr [rbp - 4624]
    696d: 48 8b 8d f8 ed ff ff         	mov	rcx, qword ptr [rbp - 4616]
    6974: 48 89 8d e8 ed ff ff         	mov	qword ptr [rbp - 4632], rcx
    697b: 48 89 85 e0 ed ff ff         	mov	qword ptr [rbp - 4640], rax
    6982: 48 8b bd e0 ed ff ff         	mov	rdi, qword ptr [rbp - 4640]
    6989: 48 8b b5 e8 ed ff ff         	mov	rsi, qword ptr [rbp - 4632]
    6990: e8 6b cf ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6995: 48 89 85 18 ee ff ff         	mov	qword ptr [rbp - 4584], rax
    699c: 48 8b 85 d0 f7 ff ff         	mov	rax, qword ptr [rbp - 2096]
    69a3: 48 89 85 80 e8 ff ff         	mov	qword ptr [rbp - 6016], rax
    69aa: 48 8b 85 d8 f7 ff ff         	mov	rax, qword ptr [rbp - 2088]
    69b1: 48 89 85 78 e8 ff ff         	mov	qword ptr [rbp - 6024], rax
    69b8: 48 8b bd 18 ee ff ff         	mov	rdi, qword ptr [rbp - 4584]
    69bf: e8 0c cf ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    69c4: 48 8b b5 78 e8 ff ff         	mov	rsi, qword ptr [rbp - 6024]
    69cb: 48 89 c1                     	mov	rcx, rax
    69ce: 48 89 d0                     	mov	rax, rdx
    69d1: 48 8b 95 80 e8 ff ff         	mov	rdx, qword ptr [rbp - 6016]
    69d8: 48 89 8d c0 ed ff ff         	mov	qword ptr [rbp - 4672], rcx
    69df: 48 89 85 c8 ed ff ff         	mov	qword ptr [rbp - 4664], rax
    69e6: 48 8b 85 c0 ed ff ff         	mov	rax, qword ptr [rbp - 4672]
    69ed: 48 8b 8d c8 ed ff ff         	mov	rcx, qword ptr [rbp - 4664]
    69f4: 48 89 b5 b8 ed ff ff         	mov	qword ptr [rbp - 4680], rsi
    69fb: 48 89 95 b0 ed ff ff         	mov	qword ptr [rbp - 4688], rdx
    6a02: 48 8b bd b0 ed ff ff         	mov	rdi, qword ptr [rbp - 4688]
    6a09: 48 8b b5 b8 ed ff ff         	mov	rsi, qword ptr [rbp - 4680]
    6a10: 48 89 8d a8 ed ff ff         	mov	qword ptr [rbp - 4696], rcx
    6a17: 48 89 85 a0 ed ff ff         	mov	qword ptr [rbp - 4704], rax
    6a1e: 48 8b 95 a0 ed ff ff         	mov	rdx, qword ptr [rbp - 4704]
    6a25: 48 8b 8d a8 ed ff ff         	mov	rcx, qword ptr [rbp - 4696]
    6a2c: e8 0f ce ff ff               	call	0x3840 <FStar_UInt128_add>
    6a31: 48 89 85 90 ed ff ff         	mov	qword ptr [rbp - 4720], rax
    6a38: 48 89 95 98 ed ff ff         	mov	qword ptr [rbp - 4712], rdx
    6a3f: 48 8b 85 90 ed ff ff         	mov	rax, qword ptr [rbp - 4720]
    6a46: 48 8b 8d 98 ed ff ff         	mov	rcx, qword ptr [rbp - 4712]
    6a4d: 48 89 8d d8 ed ff ff         	mov	qword ptr [rbp - 4648], rcx
    6a54: 48 89 85 d0 ed ff ff         	mov	qword ptr [rbp - 4656], rax
    6a5b: 48 8b 85 d0 ed ff ff         	mov	rax, qword ptr [rbp - 4656]
    6a62: 48 8b 8d d8 ed ff ff         	mov	rcx, qword ptr [rbp - 4648]
    6a69: 48 89 8d 78 ed ff ff         	mov	qword ptr [rbp - 4744], rcx
    6a70: 48 89 85 70 ed ff ff         	mov	qword ptr [rbp - 4752], rax
    6a77: 48 8b bd 70 ed ff ff         	mov	rdi, qword ptr [rbp - 4752]
    6a7e: 48 8b b5 78 ed ff ff         	mov	rsi, qword ptr [rbp - 4744]
    6a85: e8 76 ce ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6a8a: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    6a91: 8b 95 ec e8 ff ff            	mov	edx, dword ptr [rbp - 5908]
    6a97: 48 21 c8                     	and	rax, rcx
    6a9a: 48 89 85 88 ed ff ff         	mov	qword ptr [rbp - 4728], rax
    6aa1: 48 8b 85 d0 ed ff ff         	mov	rax, qword ptr [rbp - 4656]
    6aa8: 48 8b 8d d8 ed ff ff         	mov	rcx, qword ptr [rbp - 4648]
    6aaf: 48 89 8d 58 ed ff ff         	mov	qword ptr [rbp - 4776], rcx
    6ab6: 48 89 85 50 ed ff ff         	mov	qword ptr [rbp - 4784], rax
    6abd: 48 8b bd 50 ed ff ff         	mov	rdi, qword ptr [rbp - 4784]
    6ac4: 48 8b b5 58 ed ff ff         	mov	rsi, qword ptr [rbp - 4776]
    6acb: e8 60 ce ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    6ad0: 48 89 85 40 ed ff ff         	mov	qword ptr [rbp - 4800], rax
    6ad7: 48 89 95 48 ed ff ff         	mov	qword ptr [rbp - 4792], rdx
    6ade: 48 8b 85 40 ed ff ff         	mov	rax, qword ptr [rbp - 4800]
    6ae5: 48 8b 8d 48 ed ff ff         	mov	rcx, qword ptr [rbp - 4792]
    6aec: 48 89 8d 38 ed ff ff         	mov	qword ptr [rbp - 4808], rcx
    6af3: 48 89 85 30 ed ff ff         	mov	qword ptr [rbp - 4816], rax
    6afa: 48 8b bd 30 ed ff ff         	mov	rdi, qword ptr [rbp - 4816]
    6b01: 48 8b b5 38 ed ff ff         	mov	rsi, qword ptr [rbp - 4808]
    6b08: e8 f3 cd ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6b0d: 48 8b bd 88 e8 ff ff         	mov	rdi, qword ptr [rbp - 6008]
    6b14: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    6b1b: 48 89 85 68 ed ff ff         	mov	qword ptr [rbp - 4760], rax
    6b22: 48 8b 85 48 f0 ff ff         	mov	rax, qword ptr [rbp - 4024]
    6b29: 48 8b 95 68 ed ff ff         	mov	rdx, qword ptr [rbp - 4760]
    6b30: 48 8d 34 d2                  	lea	rsi, [rdx + 8*rdx]
    6b34: 48 8d 14 72                  	lea	rdx, [rdx + 2*rsi]
    6b38: 48 01 d0                     	add	rax, rdx
    6b3b: 48 89 85 28 ed ff ff         	mov	qword ptr [rbp - 4824], rax
    6b42: 48 8b 85 28 ed ff ff         	mov	rax, qword ptr [rbp - 4824]
    6b49: 48 21 c8                     	and	rax, rcx
    6b4c: 48 89 85 20 ed ff ff         	mov	qword ptr [rbp - 4832], rax
    6b53: 48 8b 85 28 ed ff ff         	mov	rax, qword ptr [rbp - 4824]
    6b5a: 48 c1 e8 33                  	shr	rax, 51
    6b5e: 48 89 85 18 ed ff ff         	mov	qword ptr [rbp - 4840], rax
    6b65: 48 8b 85 20 ed ff ff         	mov	rax, qword ptr [rbp - 4832]
    6b6c: 48 89 85 10 ed ff ff         	mov	qword ptr [rbp - 4848], rax
    6b73: 48 8b 85 98 ef ff ff         	mov	rax, qword ptr [rbp - 4200]
    6b7a: 48 8b 8d 18 ed ff ff         	mov	rcx, qword ptr [rbp - 4840]
    6b81: 48 01 c8                     	add	rax, rcx
    6b84: 48 89 85 08 ed ff ff         	mov	qword ptr [rbp - 4856], rax
    6b8b: 48 8b 85 e8 ee ff ff         	mov	rax, qword ptr [rbp - 4376]
    6b92: 48 89 85 00 ed ff ff         	mov	qword ptr [rbp - 4864], rax
    6b99: 48 8b 85 38 ee ff ff         	mov	rax, qword ptr [rbp - 4552]
    6ba0: 48 89 85 f8 ec ff ff         	mov	qword ptr [rbp - 4872], rax
    6ba7: 48 8b 85 88 ed ff ff         	mov	rax, qword ptr [rbp - 4728]
    6bae: 48 89 85 f0 ec ff ff         	mov	qword ptr [rbp - 4880], rax
    6bb5: 48 8b 85 e0 f0 ff ff         	mov	rax, qword ptr [rbp - 3872]
    6bbc: 48 89 85 98 e8 ff ff         	mov	qword ptr [rbp - 5992], rax
    6bc3: 48 8b 85 e8 f0 ff ff         	mov	rax, qword ptr [rbp - 3864]
    6bca: 48 89 85 90 e8 ff ff         	mov	qword ptr [rbp - 6000], rax
    6bd1: e8 fa cc ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    6bd6: 48 8b b5 90 e8 ff ff         	mov	rsi, qword ptr [rbp - 6000]
    6bdd: 48 89 c1                     	mov	rcx, rax
    6be0: 48 89 d0                     	mov	rax, rdx
    6be3: 48 8b 95 98 e8 ff ff         	mov	rdx, qword ptr [rbp - 5992]
    6bea: 48 89 8d d0 ec ff ff         	mov	qword ptr [rbp - 4912], rcx
    6bf1: 48 89 85 d8 ec ff ff         	mov	qword ptr [rbp - 4904], rax
    6bf8: 48 8b 85 d0 ec ff ff         	mov	rax, qword ptr [rbp - 4912]
    6bff: 48 8b 8d d8 ec ff ff         	mov	rcx, qword ptr [rbp - 4904]
    6c06: 48 89 b5 c8 ec ff ff         	mov	qword ptr [rbp - 4920], rsi
    6c0d: 48 89 95 c0 ec ff ff         	mov	qword ptr [rbp - 4928], rdx
    6c14: 48 8b bd c0 ec ff ff         	mov	rdi, qword ptr [rbp - 4928]
    6c1b: 48 8b b5 c8 ec ff ff         	mov	rsi, qword ptr [rbp - 4920]
    6c22: 48 89 8d b8 ec ff ff         	mov	qword ptr [rbp - 4936], rcx
    6c29: 48 89 85 b0 ec ff ff         	mov	qword ptr [rbp - 4944], rax
    6c30: 48 8b 95 b0 ec ff ff         	mov	rdx, qword ptr [rbp - 4944]
    6c37: 48 8b 8d b8 ec ff ff         	mov	rcx, qword ptr [rbp - 4936]
    6c3e: e8 fd cb ff ff               	call	0x3840 <FStar_UInt128_add>
    6c43: 48 89 85 a0 ec ff ff         	mov	qword ptr [rbp - 4960], rax
    6c4a: 48 89 95 a8 ec ff ff         	mov	qword ptr [rbp - 4952], rdx
    6c51: 48 8b 85 a0 ec ff ff         	mov	rax, qword ptr [rbp - 4960]
    6c58: 48 8b 8d a8 ec ff ff         	mov	rcx, qword ptr [rbp - 4952]
    6c5f: 48 89 8d e8 ec ff ff         	mov	qword ptr [rbp - 4888], rcx
    6c66: 48 89 85 e0 ec ff ff         	mov	qword ptr [rbp - 4896], rax
    6c6d: 48 8b 85 e0 ec ff ff         	mov	rax, qword ptr [rbp - 4896]
    6c74: 48 8b 8d e8 ec ff ff         	mov	rcx, qword ptr [rbp - 4888]
    6c7b: 48 89 8d 88 ec ff ff         	mov	qword ptr [rbp - 4984], rcx
    6c82: 48 89 85 80 ec ff ff         	mov	qword ptr [rbp - 4992], rax
    6c89: 48 8b bd 80 ec ff ff         	mov	rdi, qword ptr [rbp - 4992]
    6c90: 48 8b b5 88 ec ff ff         	mov	rsi, qword ptr [rbp - 4984]
    6c97: e8 64 cc ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6c9c: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    6ca3: 8b 95 ec e8 ff ff            	mov	edx, dword ptr [rbp - 5908]
    6ca9: 48 21 c8                     	and	rax, rcx
    6cac: 48 89 85 98 ec ff ff         	mov	qword ptr [rbp - 4968], rax
    6cb3: 48 8b 85 e0 ec ff ff         	mov	rax, qword ptr [rbp - 4896]
    6cba: 48 8b 8d e8 ec ff ff         	mov	rcx, qword ptr [rbp - 4888]
    6cc1: 48 89 8d 68 ec ff ff         	mov	qword ptr [rbp - 5016], rcx
    6cc8: 48 89 85 60 ec ff ff         	mov	qword ptr [rbp - 5024], rax
    6ccf: 48 8b bd 60 ec ff ff         	mov	rdi, qword ptr [rbp - 5024]
    6cd6: 48 8b b5 68 ec ff ff         	mov	rsi, qword ptr [rbp - 5016]
    6cdd: e8 4e cc ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    6ce2: 48 89 85 50 ec ff ff         	mov	qword ptr [rbp - 5040], rax
    6ce9: 48 89 95 58 ec ff ff         	mov	qword ptr [rbp - 5032], rdx
    6cf0: 48 8b 85 50 ec ff ff         	mov	rax, qword ptr [rbp - 5040]
    6cf7: 48 8b 8d 58 ec ff ff         	mov	rcx, qword ptr [rbp - 5032]
    6cfe: 48 89 8d 48 ec ff ff         	mov	qword ptr [rbp - 5048], rcx
    6d05: 48 89 85 40 ec ff ff         	mov	qword ptr [rbp - 5056], rax
    6d0c: 48 8b bd 40 ec ff ff         	mov	rdi, qword ptr [rbp - 5056]
    6d13: 48 8b b5 48 ec ff ff         	mov	rsi, qword ptr [rbp - 5048]
    6d1a: e8 e1 cb ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6d1f: 48 89 85 78 ec ff ff         	mov	qword ptr [rbp - 5000], rax
    6d26: 48 8b 85 d0 f0 ff ff         	mov	rax, qword ptr [rbp - 3888]
    6d2d: 48 89 85 a8 e8 ff ff         	mov	qword ptr [rbp - 5976], rax
    6d34: 48 8b 85 d8 f0 ff ff         	mov	rax, qword ptr [rbp - 3880]
    6d3b: 48 89 85 a0 e8 ff ff         	mov	qword ptr [rbp - 5984], rax
    6d42: 48 8b bd 78 ec ff ff         	mov	rdi, qword ptr [rbp - 5000]
    6d49: e8 82 cb ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    6d4e: 48 8b b5 a0 e8 ff ff         	mov	rsi, qword ptr [rbp - 5984]
    6d55: 48 89 c1                     	mov	rcx, rax
    6d58: 48 89 d0                     	mov	rax, rdx
    6d5b: 48 8b 95 a8 e8 ff ff         	mov	rdx, qword ptr [rbp - 5976]
    6d62: 48 89 8d 20 ec ff ff         	mov	qword ptr [rbp - 5088], rcx
    6d69: 48 89 85 28 ec ff ff         	mov	qword ptr [rbp - 5080], rax
    6d70: 48 8b 85 20 ec ff ff         	mov	rax, qword ptr [rbp - 5088]
    6d77: 48 8b 8d 28 ec ff ff         	mov	rcx, qword ptr [rbp - 5080]
    6d7e: 48 89 b5 18 ec ff ff         	mov	qword ptr [rbp - 5096], rsi
    6d85: 48 89 95 10 ec ff ff         	mov	qword ptr [rbp - 5104], rdx
    6d8c: 48 8b bd 10 ec ff ff         	mov	rdi, qword ptr [rbp - 5104]
    6d93: 48 8b b5 18 ec ff ff         	mov	rsi, qword ptr [rbp - 5096]
    6d9a: 48 89 8d 08 ec ff ff         	mov	qword ptr [rbp - 5112], rcx
    6da1: 48 89 85 00 ec ff ff         	mov	qword ptr [rbp - 5120], rax
    6da8: 48 8b 95 00 ec ff ff         	mov	rdx, qword ptr [rbp - 5120]
    6daf: 48 8b 8d 08 ec ff ff         	mov	rcx, qword ptr [rbp - 5112]
    6db6: e8 85 ca ff ff               	call	0x3840 <FStar_UInt128_add>
    6dbb: 48 89 85 f0 eb ff ff         	mov	qword ptr [rbp - 5136], rax
    6dc2: 48 89 95 f8 eb ff ff         	mov	qword ptr [rbp - 5128], rdx
    6dc9: 48 8b 85 f0 eb ff ff         	mov	rax, qword ptr [rbp - 5136]
    6dd0: 48 8b 8d f8 eb ff ff         	mov	rcx, qword ptr [rbp - 5128]
    6dd7: 48 89 8d 38 ec ff ff         	mov	qword ptr [rbp - 5064], rcx
    6dde: 48 89 85 30 ec ff ff         	mov	qword ptr [rbp - 5072], rax
    6de5: 48 8b 85 30 ec ff ff         	mov	rax, qword ptr [rbp - 5072]
    6dec: 48 8b 8d 38 ec ff ff         	mov	rcx, qword ptr [rbp - 5064]
    6df3: 48 89 8d d8 eb ff ff         	mov	qword ptr [rbp - 5160], rcx
    6dfa: 48 89 85 d0 eb ff ff         	mov	qword ptr [rbp - 5168], rax
    6e01: 48 8b bd d0 eb ff ff         	mov	rdi, qword ptr [rbp - 5168]
    6e08: 48 8b b5 d8 eb ff ff         	mov	rsi, qword ptr [rbp - 5160]
    6e0f: e8 ec ca ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6e14: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    6e1b: 8b 95 ec e8 ff ff            	mov	edx, dword ptr [rbp - 5908]
    6e21: 48 21 c8                     	and	rax, rcx
    6e24: 48 89 85 e8 eb ff ff         	mov	qword ptr [rbp - 5144], rax
    6e2b: 48 8b 85 30 ec ff ff         	mov	rax, qword ptr [rbp - 5072]
    6e32: 48 8b 8d 38 ec ff ff         	mov	rcx, qword ptr [rbp - 5064]
    6e39: 48 89 8d b8 eb ff ff         	mov	qword ptr [rbp - 5192], rcx
    6e40: 48 89 85 b0 eb ff ff         	mov	qword ptr [rbp - 5200], rax
    6e47: 48 8b bd b0 eb ff ff         	mov	rdi, qword ptr [rbp - 5200]
    6e4e: 48 8b b5 b8 eb ff ff         	mov	rsi, qword ptr [rbp - 5192]
    6e55: e8 d6 ca ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    6e5a: 48 89 85 a0 eb ff ff         	mov	qword ptr [rbp - 5216], rax
    6e61: 48 89 95 a8 eb ff ff         	mov	qword ptr [rbp - 5208], rdx
    6e68: 48 8b 85 a0 eb ff ff         	mov	rax, qword ptr [rbp - 5216]
    6e6f: 48 8b 8d a8 eb ff ff         	mov	rcx, qword ptr [rbp - 5208]
    6e76: 48 89 8d 98 eb ff ff         	mov	qword ptr [rbp - 5224], rcx
    6e7d: 48 89 85 90 eb ff ff         	mov	qword ptr [rbp - 5232], rax
    6e84: 48 8b bd 90 eb ff ff         	mov	rdi, qword ptr [rbp - 5232]
    6e8b: 48 8b b5 98 eb ff ff         	mov	rsi, qword ptr [rbp - 5224]
    6e92: e8 69 ca ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6e97: 48 89 85 c8 eb ff ff         	mov	qword ptr [rbp - 5176], rax
    6e9e: 48 8b 85 c0 f0 ff ff         	mov	rax, qword ptr [rbp - 3904]
    6ea5: 48 89 85 b8 e8 ff ff         	mov	qword ptr [rbp - 5960], rax
    6eac: 48 8b 85 c8 f0 ff ff         	mov	rax, qword ptr [rbp - 3896]
    6eb3: 48 89 85 b0 e8 ff ff         	mov	qword ptr [rbp - 5968], rax
    6eba: 48 8b bd c8 eb ff ff         	mov	rdi, qword ptr [rbp - 5176]
    6ec1: e8 0a ca ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    6ec6: 48 8b b5 b0 e8 ff ff         	mov	rsi, qword ptr [rbp - 5968]
    6ecd: 48 89 c1                     	mov	rcx, rax
    6ed0: 48 89 d0                     	mov	rax, rdx
    6ed3: 48 8b 95 b8 e8 ff ff         	mov	rdx, qword ptr [rbp - 5960]
    6eda: 48 89 8d 70 eb ff ff         	mov	qword ptr [rbp - 5264], rcx
    6ee1: 48 89 85 78 eb ff ff         	mov	qword ptr [rbp - 5256], rax
    6ee8: 48 8b 85 70 eb ff ff         	mov	rax, qword ptr [rbp - 5264]
    6eef: 48 8b 8d 78 eb ff ff         	mov	rcx, qword ptr [rbp - 5256]
    6ef6: 48 89 b5 68 eb ff ff         	mov	qword ptr [rbp - 5272], rsi
    6efd: 48 89 95 60 eb ff ff         	mov	qword ptr [rbp - 5280], rdx
    6f04: 48 8b bd 60 eb ff ff         	mov	rdi, qword ptr [rbp - 5280]
    6f0b: 48 8b b5 68 eb ff ff         	mov	rsi, qword ptr [rbp - 5272]
    6f12: 48 89 8d 58 eb ff ff         	mov	qword ptr [rbp - 5288], rcx
    6f19: 48 89 85 50 eb ff ff         	mov	qword ptr [rbp - 5296], rax
    6f20: 48 8b 95 50 eb ff ff         	mov	rdx, qword ptr [rbp - 5296]
    6f27: 48 8b 8d 58 eb ff ff         	mov	rcx, qword ptr [rbp - 5288]
    6f2e: e8 0d c9 ff ff               	call	0x3840 <FStar_UInt128_add>
    6f33: 48 89 85 40 eb ff ff         	mov	qword ptr [rbp - 5312], rax
    6f3a: 48 89 95 48 eb ff ff         	mov	qword ptr [rbp - 5304], rdx
    6f41: 48 8b 85 40 eb ff ff         	mov	rax, qword ptr [rbp - 5312]
    6f48: 48 8b 8d 48 eb ff ff         	mov	rcx, qword ptr [rbp - 5304]
    6f4f: 48 89 8d 88 eb ff ff         	mov	qword ptr [rbp - 5240], rcx
    6f56: 48 89 85 80 eb ff ff         	mov	qword ptr [rbp - 5248], rax
    6f5d: 48 8b 85 80 eb ff ff         	mov	rax, qword ptr [rbp - 5248]
    6f64: 48 8b 8d 88 eb ff ff         	mov	rcx, qword ptr [rbp - 5240]
    6f6b: 48 89 8d 28 eb ff ff         	mov	qword ptr [rbp - 5336], rcx
    6f72: 48 89 85 20 eb ff ff         	mov	qword ptr [rbp - 5344], rax
    6f79: 48 8b bd 20 eb ff ff         	mov	rdi, qword ptr [rbp - 5344]
    6f80: 48 8b b5 28 eb ff ff         	mov	rsi, qword ptr [rbp - 5336]
    6f87: e8 74 c9 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    6f8c: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    6f93: 8b 95 ec e8 ff ff            	mov	edx, dword ptr [rbp - 5908]
    6f99: 48 21 c8                     	and	rax, rcx
    6f9c: 48 89 85 38 eb ff ff         	mov	qword ptr [rbp - 5320], rax
    6fa3: 48 8b 85 80 eb ff ff         	mov	rax, qword ptr [rbp - 5248]
    6faa: 48 8b 8d 88 eb ff ff         	mov	rcx, qword ptr [rbp - 5240]
    6fb1: 48 89 8d 08 eb ff ff         	mov	qword ptr [rbp - 5368], rcx
    6fb8: 48 89 85 00 eb ff ff         	mov	qword ptr [rbp - 5376], rax
    6fbf: 48 8b bd 00 eb ff ff         	mov	rdi, qword ptr [rbp - 5376]
    6fc6: 48 8b b5 08 eb ff ff         	mov	rsi, qword ptr [rbp - 5368]
    6fcd: e8 5e c9 ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    6fd2: 48 89 85 f0 ea ff ff         	mov	qword ptr [rbp - 5392], rax
    6fd9: 48 89 95 f8 ea ff ff         	mov	qword ptr [rbp - 5384], rdx
    6fe0: 48 8b 85 f0 ea ff ff         	mov	rax, qword ptr [rbp - 5392]
    6fe7: 48 8b 8d f8 ea ff ff         	mov	rcx, qword ptr [rbp - 5384]
    6fee: 48 89 8d e8 ea ff ff         	mov	qword ptr [rbp - 5400], rcx
    6ff5: 48 89 85 e0 ea ff ff         	mov	qword ptr [rbp - 5408], rax
    6ffc: 48 8b bd e0 ea ff ff         	mov	rdi, qword ptr [rbp - 5408]
    7003: 48 8b b5 e8 ea ff ff         	mov	rsi, qword ptr [rbp - 5400]
    700a: e8 f1 c8 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    700f: 48 89 85 18 eb ff ff         	mov	qword ptr [rbp - 5352], rax
    7016: 48 8b 85 b0 f0 ff ff         	mov	rax, qword ptr [rbp - 3920]
    701d: 48 89 85 c8 e8 ff ff         	mov	qword ptr [rbp - 5944], rax
    7024: 48 8b 85 b8 f0 ff ff         	mov	rax, qword ptr [rbp - 3912]
    702b: 48 89 85 c0 e8 ff ff         	mov	qword ptr [rbp - 5952], rax
    7032: 48 8b bd 18 eb ff ff         	mov	rdi, qword ptr [rbp - 5352]
    7039: e8 92 c8 ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    703e: 48 8b b5 c0 e8 ff ff         	mov	rsi, qword ptr [rbp - 5952]
    7045: 48 89 c1                     	mov	rcx, rax
    7048: 48 89 d0                     	mov	rax, rdx
    704b: 48 8b 95 c8 e8 ff ff         	mov	rdx, qword ptr [rbp - 5944]
    7052: 48 89 8d c0 ea ff ff         	mov	qword ptr [rbp - 5440], rcx
    7059: 48 89 85 c8 ea ff ff         	mov	qword ptr [rbp - 5432], rax
    7060: 48 8b 85 c0 ea ff ff         	mov	rax, qword ptr [rbp - 5440]
    7067: 48 8b 8d c8 ea ff ff         	mov	rcx, qword ptr [rbp - 5432]
    706e: 48 89 b5 b8 ea ff ff         	mov	qword ptr [rbp - 5448], rsi
    7075: 48 89 95 b0 ea ff ff         	mov	qword ptr [rbp - 5456], rdx
    707c: 48 8b bd b0 ea ff ff         	mov	rdi, qword ptr [rbp - 5456]
    7083: 48 8b b5 b8 ea ff ff         	mov	rsi, qword ptr [rbp - 5448]
    708a: 48 89 8d a8 ea ff ff         	mov	qword ptr [rbp - 5464], rcx
    7091: 48 89 85 a0 ea ff ff         	mov	qword ptr [rbp - 5472], rax
    7098: 48 8b 95 a0 ea ff ff         	mov	rdx, qword ptr [rbp - 5472]
    709f: 48 8b 8d a8 ea ff ff         	mov	rcx, qword ptr [rbp - 5464]
    70a6: e8 95 c7 ff ff               	call	0x3840 <FStar_UInt128_add>
    70ab: 48 89 85 90 ea ff ff         	mov	qword ptr [rbp - 5488], rax
    70b2: 48 89 95 98 ea ff ff         	mov	qword ptr [rbp - 5480], rdx
    70b9: 48 8b 85 90 ea ff ff         	mov	rax, qword ptr [rbp - 5488]
    70c0: 48 8b 8d 98 ea ff ff         	mov	rcx, qword ptr [rbp - 5480]
    70c7: 48 89 8d d8 ea ff ff         	mov	qword ptr [rbp - 5416], rcx
    70ce: 48 89 85 d0 ea ff ff         	mov	qword ptr [rbp - 5424], rax
    70d5: 48 8b 85 d0 ea ff ff         	mov	rax, qword ptr [rbp - 5424]
    70dc: 48 8b 8d d8 ea ff ff         	mov	rcx, qword ptr [rbp - 5416]
    70e3: 48 89 8d 78 ea ff ff         	mov	qword ptr [rbp - 5512], rcx
    70ea: 48 89 85 70 ea ff ff         	mov	qword ptr [rbp - 5520], rax
    70f1: 48 8b bd 70 ea ff ff         	mov	rdi, qword ptr [rbp - 5520]
    70f8: 48 8b b5 78 ea ff ff         	mov	rsi, qword ptr [rbp - 5512]
    70ff: e8 fc c7 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    7104: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    710b: 8b 95 ec e8 ff ff            	mov	edx, dword ptr [rbp - 5908]
    7111: 48 21 c8                     	and	rax, rcx
    7114: 48 89 85 88 ea ff ff         	mov	qword ptr [rbp - 5496], rax
    711b: 48 8b 85 d0 ea ff ff         	mov	rax, qword ptr [rbp - 5424]
    7122: 48 8b 8d d8 ea ff ff         	mov	rcx, qword ptr [rbp - 5416]
    7129: 48 89 8d 58 ea ff ff         	mov	qword ptr [rbp - 5544], rcx
    7130: 48 89 85 50 ea ff ff         	mov	qword ptr [rbp - 5552], rax
    7137: 48 8b bd 50 ea ff ff         	mov	rdi, qword ptr [rbp - 5552]
    713e: 48 8b b5 58 ea ff ff         	mov	rsi, qword ptr [rbp - 5544]
    7145: e8 e6 c7 ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    714a: 48 89 85 40 ea ff ff         	mov	qword ptr [rbp - 5568], rax
    7151: 48 89 95 48 ea ff ff         	mov	qword ptr [rbp - 5560], rdx
    7158: 48 8b 85 40 ea ff ff         	mov	rax, qword ptr [rbp - 5568]
    715f: 48 8b 8d 48 ea ff ff         	mov	rcx, qword ptr [rbp - 5560]
    7166: 48 89 8d 38 ea ff ff         	mov	qword ptr [rbp - 5576], rcx
    716d: 48 89 85 30 ea ff ff         	mov	qword ptr [rbp - 5584], rax
    7174: 48 8b bd 30 ea ff ff         	mov	rdi, qword ptr [rbp - 5584]
    717b: 48 8b b5 38 ea ff ff         	mov	rsi, qword ptr [rbp - 5576]
    7182: e8 79 c7 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    7187: 48 89 85 68 ea ff ff         	mov	qword ptr [rbp - 5528], rax
    718e: 48 8b 85 a0 f0 ff ff         	mov	rax, qword ptr [rbp - 3936]
    7195: 48 89 85 d8 e8 ff ff         	mov	qword ptr [rbp - 5928], rax
    719c: 48 8b 85 a8 f0 ff ff         	mov	rax, qword ptr [rbp - 3928]
    71a3: 48 89 85 d0 e8 ff ff         	mov	qword ptr [rbp - 5936], rax
    71aa: 48 8b bd 68 ea ff ff         	mov	rdi, qword ptr [rbp - 5528]
    71b1: e8 1a c7 ff ff               	call	0x38d0 <FStar_UInt128_uint64_to_uint128>
    71b6: 48 8b b5 d0 e8 ff ff         	mov	rsi, qword ptr [rbp - 5936]
    71bd: 48 89 c1                     	mov	rcx, rax
    71c0: 48 89 d0                     	mov	rax, rdx
    71c3: 48 8b 95 d8 e8 ff ff         	mov	rdx, qword ptr [rbp - 5928]
    71ca: 48 89 8d 10 ea ff ff         	mov	qword ptr [rbp - 5616], rcx
    71d1: 48 89 85 18 ea ff ff         	mov	qword ptr [rbp - 5608], rax
    71d8: 48 8b 85 10 ea ff ff         	mov	rax, qword ptr [rbp - 5616]
    71df: 48 8b 8d 18 ea ff ff         	mov	rcx, qword ptr [rbp - 5608]
    71e6: 48 89 b5 08 ea ff ff         	mov	qword ptr [rbp - 5624], rsi
    71ed: 48 89 95 00 ea ff ff         	mov	qword ptr [rbp - 5632], rdx
    71f4: 48 8b bd 00 ea ff ff         	mov	rdi, qword ptr [rbp - 5632]
    71fb: 48 8b b5 08 ea ff ff         	mov	rsi, qword ptr [rbp - 5624]
    7202: 48 89 8d f8 e9 ff ff         	mov	qword ptr [rbp - 5640], rcx
    7209: 48 89 85 f0 e9 ff ff         	mov	qword ptr [rbp - 5648], rax
    7210: 48 8b 95 f0 e9 ff ff         	mov	rdx, qword ptr [rbp - 5648]
    7217: 48 8b 8d f8 e9 ff ff         	mov	rcx, qword ptr [rbp - 5640]
    721e: e8 1d c6 ff ff               	call	0x3840 <FStar_UInt128_add>
    7223: 48 89 85 e0 e9 ff ff         	mov	qword ptr [rbp - 5664], rax
    722a: 48 89 95 e8 e9 ff ff         	mov	qword ptr [rbp - 5656], rdx
    7231: 48 8b 85 e0 e9 ff ff         	mov	rax, qword ptr [rbp - 5664]
    7238: 48 8b 8d e8 e9 ff ff         	mov	rcx, qword ptr [rbp - 5656]
    723f: 48 89 8d 28 ea ff ff         	mov	qword ptr [rbp - 5592], rcx
    7246: 48 89 85 20 ea ff ff         	mov	qword ptr [rbp - 5600], rax
    724d: 48 8b 85 20 ea ff ff         	mov	rax, qword ptr [rbp - 5600]
    7254: 48 8b 8d 28 ea ff ff         	mov	rcx, qword ptr [rbp - 5592]
    725b: 48 89 8d c8 e9 ff ff         	mov	qword ptr [rbp - 5688], rcx
    7262: 48 89 85 c0 e9 ff ff         	mov	qword ptr [rbp - 5696], rax
    7269: 48 8b bd c0 e9 ff ff         	mov	rdi, qword ptr [rbp - 5696]
    7270: 48 8b b5 c8 e9 ff ff         	mov	rsi, qword ptr [rbp - 5688]
    7277: e8 84 c6 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    727c: 48 8b 8d e0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5920]
    7283: 8b 95 ec e8 ff ff            	mov	edx, dword ptr [rbp - 5908]
    7289: 48 21 c8                     	and	rax, rcx
    728c: 48 89 85 d8 e9 ff ff         	mov	qword ptr [rbp - 5672], rax
    7293: 48 8b 85 20 ea ff ff         	mov	rax, qword ptr [rbp - 5600]
    729a: 48 8b 8d 28 ea ff ff         	mov	rcx, qword ptr [rbp - 5592]
    72a1: 48 89 8d a8 e9 ff ff         	mov	qword ptr [rbp - 5720], rcx
    72a8: 48 89 85 a0 e9 ff ff         	mov	qword ptr [rbp - 5728], rax
    72af: 48 8b bd a0 e9 ff ff         	mov	rdi, qword ptr [rbp - 5728]
    72b6: 48 8b b5 a8 e9 ff ff         	mov	rsi, qword ptr [rbp - 5720]
    72bd: e8 6e c6 ff ff               	call	0x3930 <FStar_UInt128_shift_right>
    72c2: 48 89 85 90 e9 ff ff         	mov	qword ptr [rbp - 5744], rax
    72c9: 48 89 95 98 e9 ff ff         	mov	qword ptr [rbp - 5736], rdx
    72d0: 48 8b 85 90 e9 ff ff         	mov	rax, qword ptr [rbp - 5744]
    72d7: 48 8b 8d 98 e9 ff ff         	mov	rcx, qword ptr [rbp - 5736]
    72de: 48 89 8d 88 e9 ff ff         	mov	qword ptr [rbp - 5752], rcx
    72e5: 48 89 85 80 e9 ff ff         	mov	qword ptr [rbp - 5760], rax
    72ec: 48 8b bd 80 e9 ff ff         	mov	rdi, qword ptr [rbp - 5760]
    72f3: 48 8b b5 88 e9 ff ff         	mov	rsi, qword ptr [rbp - 5752]
    72fa: e8 01 c6 ff ff               	call	0x3900 <FStar_UInt128_uint128_to_uint64>
    72ff: 48 89 85 b8 e9 ff ff         	mov	qword ptr [rbp - 5704], rax
    7306: 48 8b 85 98 ec ff ff         	mov	rax, qword ptr [rbp - 4968]
    730d: 48 6b 8d b8 e9 ff ff 13      	imul	rcx, qword ptr [rbp - 5704], 19
    7315: 48 01 c8                     	add	rax, rcx
    7318: 48 89 85 78 e9 ff ff         	mov	qword ptr [rbp - 5768], rax
    731f: 48 b8 ff ff ff ff ff ff 07 00	movabs	rax, 2251799813685247
    7329: 48 23 85 78 e9 ff ff         	and	rax, qword ptr [rbp - 5768]
    7330: 48 89 85 70 e9 ff ff         	mov	qword ptr [rbp - 5776], rax
    7337: 48 8b 85 78 e9 ff ff         	mov	rax, qword ptr [rbp - 5768]
    733e: 48 c1 e8 33                  	shr	rax, 51
    7342: 48 89 85 68 e9 ff ff         	mov	qword ptr [rbp - 5784], rax
    7349: 48 8b 85 70 e9 ff ff         	mov	rax, qword ptr [rbp - 5776]
    7350: 48 89 85 60 e9 ff ff         	mov	qword ptr [rbp - 5792], rax
    7357: 48 8b 85 e8 eb ff ff         	mov	rax, qword ptr [rbp - 5144]
    735e: 48 03 85 68 e9 ff ff         	add	rax, qword ptr [rbp - 5784]
    7365: 48 89 85 58 e9 ff ff         	mov	qword ptr [rbp - 5800], rax
    736c: 48 8b 85 38 eb ff ff         	mov	rax, qword ptr [rbp - 5320]
    7373: 48 89 85 50 e9 ff ff         	mov	qword ptr [rbp - 5808], rax
    737a: 48 8b 85 88 ea ff ff         	mov	rax, qword ptr [rbp - 5496]
    7381: 48 89 85 48 e9 ff ff         	mov	qword ptr [rbp - 5816], rax
    7388: 48 8b 85 d8 e9 ff ff         	mov	rax, qword ptr [rbp - 5672]
    738f: 48 89 85 40 e9 ff ff         	mov	qword ptr [rbp - 5824], rax
    7396: 48 8b 85 10 ed ff ff         	mov	rax, qword ptr [rbp - 4848]
    739d: 48 89 85 38 e9 ff ff         	mov	qword ptr [rbp - 5832], rax
    73a4: 48 8b 85 08 ed ff ff         	mov	rax, qword ptr [rbp - 4856]
    73ab: 48 89 85 30 e9 ff ff         	mov	qword ptr [rbp - 5840], rax
    73b2: 48 8b 85 00 ed ff ff         	mov	rax, qword ptr [rbp - 4864]
    73b9: 48 89 85 28 e9 ff ff         	mov	qword ptr [rbp - 5848], rax
    73c0: 48 8b 85 f8 ec ff ff         	mov	rax, qword ptr [rbp - 4872]
    73c7: 48 89 85 20 e9 ff ff         	mov	qword ptr [rbp - 5856], rax
    73ce: 48 8b 85 f0 ec ff ff         	mov	rax, qword ptr [rbp - 4880]
    73d5: 48 89 85 18 e9 ff ff         	mov	qword ptr [rbp - 5864], rax
    73dc: 48 8b 85 60 e9 ff ff         	mov	rax, qword ptr [rbp - 5792]
    73e3: 48 89 85 10 e9 ff ff         	mov	qword ptr [rbp - 5872], rax
    73ea: 48 8b 85 58 e9 ff ff         	mov	rax, qword ptr [rbp - 5800]
    73f1: 48 89 85 08 e9 ff ff         	mov	qword ptr [rbp - 5880], rax
    73f8: 48 8b 85 50 e9 ff ff         	mov	rax, qword ptr [rbp - 5808]
    73ff: 48 89 85 00 e9 ff ff         	mov	qword ptr [rbp - 5888], rax
    7406: 48 8b 85 48 e9 ff ff         	mov	rax, qword ptr [rbp - 5816]
    740d: 48 89 85 f8 e8 ff ff         	mov	qword ptr [rbp - 5896], rax
    7414: 48 8b 85 40 e9 ff ff         	mov	rax, qword ptr [rbp - 5824]
    741b: 48 89 85 f0 e8 ff ff         	mov	qword ptr [rbp - 5904], rax
    7422: 48 8b 8d 38 e9 ff ff         	mov	rcx, qword ptr [rbp - 5832]
    7429: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    742d: 48 89 08                     	mov	qword ptr [rax], rcx
    7430: 48 8b 8d 30 e9 ff ff         	mov	rcx, qword ptr [rbp - 5840]
    7437: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    743b: 48 89 48 08                  	mov	qword ptr [rax + 8], rcx
    743f: 48 8b 8d 28 e9 ff ff         	mov	rcx, qword ptr [rbp - 5848]
    7446: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    744a: 48 89 48 10                  	mov	qword ptr [rax + 16], rcx
    744e: 48 8b 8d 20 e9 ff ff         	mov	rcx, qword ptr [rbp - 5856]
    7455: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    7459: 48 89 48 18                  	mov	qword ptr [rax + 24], rcx
    745d: 48 8b 8d 18 e9 ff ff         	mov	rcx, qword ptr [rbp - 5864]
    7464: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    7468: 48 89 48 20                  	mov	qword ptr [rax + 32], rcx
    746c: 48 8b 8d 10 e9 ff ff         	mov	rcx, qword ptr [rbp - 5872]
    7473: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    7477: 48 89 48 28                  	mov	qword ptr [rax + 40], rcx
    747b: 48 8b 8d 08 e9 ff ff         	mov	rcx, qword ptr [rbp - 5880]
    7482: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    7486: 48 89 48 30                  	mov	qword ptr [rax + 48], rcx
    748a: 48 8b 8d 00 e9 ff ff         	mov	rcx, qword ptr [rbp - 5888]
    7491: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    7495: 48 89 48 38                  	mov	qword ptr [rax + 56], rcx
    7499: 48 8b 8d f8 e8 ff ff         	mov	rcx, qword ptr [rbp - 5896]
    74a0: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    74a4: 48 89 48 40                  	mov	qword ptr [rax + 64], rcx
    74a8: 48 8b 8d f0 e8 ff ff         	mov	rcx, qword ptr [rbp - 5904]
    74af: 48 8b 45 f8                  	mov	rax, qword ptr [rbp - 8]
    74b3: 48 89 48 48                  	mov	qword ptr [rax + 72], rcx
    74b7: 48 81 c4 50 1a 00 00         	add	rsp, 6736
    74be: 5d                           	pop	rbp
    74bf: c3                           	ret
