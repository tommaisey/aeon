current_dir = $(shell pwd)

all:
	make clean
	cc -dynamiclib -o libtimeout.dylib timeout.c

clean:
	rm -f libtimeout.dylib