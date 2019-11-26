current_dir = $(shell pwd)

all:
	make clean
	cp mk-r6rs/mk-r6rs.sls mk-r6rs.sls
	(cd rhs/mk;  make libdir=$(current_dir))
	(cd sosc/mk; make libdir=$(current_dir))
	(cd rsc3/mk; make libdir=$(current_dir))
	rm mk-r6rs.sls

clean:
	rm -f sosc.sls rhs.sls rsc3.sls