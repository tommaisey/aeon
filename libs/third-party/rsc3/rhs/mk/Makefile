libdir=$(HOME)/Code/scheme-libs/

all:
	make clean
	scheme --libdirs $(libdir) --script mk.scm $(libdir)

clean:
	rm -f $(libdir)/rhs.sl?

