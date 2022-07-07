  .text
  .globl addident
  .type addident, @function

#! file-offset 0x510
#! rip-offset  0x400510
#! capacity    32 bytes

# Text              #  Line  RIP       Bytes  Opcode             
.addident:          #        0x400510  0      OPC=<label>        
  xorl %eax, %eax   #  1     0x400510  2      OPC=xorl_r32_r32   
  shlb $0x20, %sil  #  2     0x400512  4      OPC=shlb_r8_imm8   
  xaddl %edi, %esi  #  3     0x400516  3      OPC=xaddl_r32_r32  
  xchgl %esi, %eax  #  4     0x400519  1      OPC=xchgl_eax_r32  
  retq              #  5     0x40051a  1      OPC=retq           
                                                                 
.size addident, .-addident
