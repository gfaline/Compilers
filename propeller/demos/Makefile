binding.out: binding.pr
	../prc.sh binding.pr

sensor.out: sensor.pr
	../prc.sh -r sensor_linux sensor.pr

.PHONY:demos
demos: binding.out sensor.out

.PHONY:clean
clean:
	rm -f binding.out sensor.out
