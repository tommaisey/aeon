current_dir = $(shell pwd)

all:
	make clean
	(cd libs/third-party; make)

clean:
	(cd libs/third-party; make clean)