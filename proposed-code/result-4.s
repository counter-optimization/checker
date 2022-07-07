  .text
  .globl addident
  .type addident, @function

#! file-offset 0x510
#! rip-offset  0x400510
#! capacity    32 bytes

# Text              #  Line  RIP       Bytes  Opcode             
.addident:          #        0x400510  0      OPC=<label>        
  xaddl %edi, %esi  #  1     0x400510  3      OPC=xaddl_r32_r32  
  xorl %eax, %eax   #  2     0x400513  2      OPC=xorl_r32_r32   
  xchgl %eax, %esi  #  3     0x400515  1      OPC=xchgl_r32_eax  
  retq              #  4     0x400516  1      OPC=retq           
                                                                 
.size addident, .-addident
