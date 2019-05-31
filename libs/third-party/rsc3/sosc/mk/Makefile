libdir=$(HOME)/Code/

all:
	make clean
	scheme --libdirs $(libdir)  --script mk.scm $(libdir)

clean:
	rm -f $(libdir)/sosc.*.sls
