#include <stdio.h>
#include <stdint.h>

int
main(int argc, char** argv) {

  uint32_t long_ones = ~0;
  uint16_t short_zeros = 0;
  uint8_t bytes_ones = ~0;

  uint64_t high_bit = ~(~0ull >> 1);

  uint64_t result = 0;
  
  asm inline("xorq %%rax, %%rax\n"
	     "movq %4, %%rax\n"
	     "movl %1, %%eax\n"
	     "movw %2, %%ax\n"
	     "movb %3, %%al\n"
	     "movq %%rax, %0\n"
	     : "=r" (result)
	     : "ir" (long_ones),
	       "ir" (short_zeros),
	       "ir" (bytes_ones),
	       "ir" (high_bit),
	     : "rax", "eax", "ax", "ah", "al");

  printf("result is: %#016llx\n", result);
  return 0;
}
