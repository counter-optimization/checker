  .text
  .globl addident
  .type addident, @function

#! file-offset 0x510
#! rip-offset  0x400510
#! capacity    32 bytes

# Text              #  Line  RIP       Bytes  Opcode             
.addident:          #        0x400510  0      OPC=<label>        
  xchgb %sil, %sil  #  1     0x400510  3      OPC=xchgb_r8_r8_1  
  addl %edi, %esi   #  2     0x400513  2      OPC=addl_r32_r32   
  xorl %eax, %eax   #  3     0x400515  2      OPC=xorl_r32_r32   
  cltd              #  4     0x400517  1      OPC=cltd           
  xchgw %di, %ax    #  5     0x400518  2      OPC=xchgw_ax_r16   
  xchgl %esi, %eax  #  6     0x40051a  1      OPC=xchgl_eax_r32  
  retq              #  7     0x40051b  1      OPC=retq           
                                                                 
.size addident, .-addident
