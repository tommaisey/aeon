current_dir = $(shell pwd)

all:
	make clean
	(cd timeout; make)
	(cd sc3;  make)

clean:
	(cd timeout; make clean)
	(cd sc3; make clean)