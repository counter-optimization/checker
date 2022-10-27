
CFLAGS = -O0 --target=x86_64-unknown-linux
CC = clang

test_c_files = $(wildcard *.c)
test_obj_files = $(test_c_files:.c=.o)

.PHONY = all clean

all: $(test_obj_files)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm *.o
