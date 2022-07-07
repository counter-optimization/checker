  .text
  .globl addident
  .type addident, @function

#! file-offset 0x510
#! rip-offset  0x400510
#! capacity    32 bytes

# Text              #  Line  RIP       Bytes  Opcode              
.addident:          #        0x400510  0      OPC=<label>         
  xorl %eax, %eax   #  1     0x400510  2      OPC=xorl_r32_r32    
  andl %esi, %esi   #  2     0x400512  2      OPC=andl_r32_r32_1  
  addl %edi, %esi   #  3     0x400514  2      OPC=addl_r32_r32_1  
  xchgl %eax, %esi  #  4     0x400516  1      OPC=xchgl_r32_eax   
  retq              #  5     0x400517  1      OPC=retq            
                                                                  
.size addident, .-addident
