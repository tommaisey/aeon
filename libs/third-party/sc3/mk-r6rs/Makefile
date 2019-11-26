D=$(HOME)/opt/lib/r6rs

install:
	mkdir -p $(D)
	cp -f mk-r6rs.sls $(D)

push-sp:
	darcs push -a rd@slavepianos.org:sw/mk-r6rs

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/mk-r6rs
