#include <stdio.h>
#include <unistd.h>

extern int init();

void(*boundf)(int,int);
int oldt = 0;

int object_new_Sensor()
{
	return 0;
}

int object_prop_bind_Sensor_temperature(int oid, void(*f)(int,int))
{
	if (oid == 0)
		boundf = f;
}

int main()
{
	int ret = init();
	while (1)
	{
		FILE *f = fopen("/sys/class/thermal/thermal_zone0/temp", "r");
		int t;
		fscanf(f, "%d", &t);
		boundf(oldt, t);
		oldt = t;
		fclose(f);
		sleep(1);
	}
	return ret;
}
