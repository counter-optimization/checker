#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

uint64_t fix_convert_from_int64(int64_t i);

int
main(int argc, char** argv) 
{
	assert(argc == 2);	
	int64_t input = atoi(argv[1]);
	uint64_t result = fix_convert_from_int64(input);
	printf("%llu\n", result);
	return result;
}
