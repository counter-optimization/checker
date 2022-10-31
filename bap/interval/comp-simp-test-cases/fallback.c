int sub_32(int x, int y) {
  return x - y;
}

int safe_sub_32(int x, int y) {
  int ret_val;
  
  asm volatile inline (
	     "subl $0x80000000, %[y]\n"
	     "subl $0x80000000, %[y]\n"
	     "setz %%r11b\n"
	     "cmovzl %[y], %%r13d\n"
	     "cmovzl %%r11d, %[y]\n"
	     "cmovzl %[x], %%r12d\n"
	     "subl %[y], %[x]\n"
	     "subl $0x80000000, %%r11d\n"
	     "subl $0x80000000, %%r11d\n"
	     "cmovnzl %%r12d, %[x]\n"
	     "cmovnzl %%r13d, %[y]\n"
	     : [ ret_val ] "=rm" (ret_val)
	     : [ x ] "r" (x), [ y ] "r" (y)
	     : "r11", "r13", "r12");
  
  return ret_val;
}
