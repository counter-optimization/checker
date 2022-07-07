  .text
  .globl addident
  .type addident, @function

#! file-offset 0x510
#! rip-offset  0x400510
#! capacity    32 bytes

# Text               #  Line  RIP       Bytes  Opcode              
.addident:           #        0x400510  0      OPC=<label>         
  movb $0x7, %dl     #  1     0x400510  3      OPC=movb_r8_imm8_1  
  movl %esi, %esi    #  2     0x400513  2      OPC=movl_r32_r32    
  xorl %eax, %eax    #  3     0x400515  2      OPC=xorl_r32_r32    
  cmovcl %edi, %esi  #  4     0x400517  3      OPC=cmovcl_r32_r32  
  xaddl %edi, %esi   #  5     0x40051a  3      OPC=xaddl_r32_r32   
  xchgl %esi, %eax   #  6     0x40051d  1      OPC=xchgl_eax_r32   
  retq               #  7     0x40051e  1      OPC=retq            
                                                                   
.size addident, .-addident
