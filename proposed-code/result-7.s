  .text
  .globl addident
  .type addident, @function

#! file-offset 0x510
#! rip-offset  0x400510
#! capacity    32 bytes

# Text              #  Line  RIP       Bytes  Opcode             
.addident:          #        0x400510  0      OPC=<label>        
  addl %esi, %edi   #  1     0x400510  2      OPC=addl_r32_r32   
  xorl %eax, %eax   #  2     0x400512  2      OPC=xorl_r32_r32   
  xchgq %rdi, %rax  #  3     0x400514  2      OPC=xchgq_rax_r64  
  retq              #  4     0x400516  1      OPC=retq           
                                                                 
.size addident, .-addident
