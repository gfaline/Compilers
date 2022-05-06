CC := /usr/bin/cc

default.rto:
	${CC} -c -o default.rto default.c
sensor_linux.rto:
	${CC} -c -o sensor_linux.rto sensor_linux.c

.PHONY:runtime
runtime: default.rto sensor_linux.rto

.PHONY:clean
clean:
	rm -f default.rto sensor_linux.rto
