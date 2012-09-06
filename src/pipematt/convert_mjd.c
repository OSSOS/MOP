#include <stdio.h>
#include <math.h>
#define IGREG 2299161

main(argc, argv)
     int argc;
     char **argv;
{
  
  long jd;
  double mjd, fd;
  int mm, dd, yyyy;
  void caldat(long julian, int *mm, int *dd, int *yyyy);

  if(argc != 2) {
    printf("args: mjd\n");
    exit(-1);
  }

  sscanf(argv[1], "%lf", &mjd);

  fd = mjd - (int)mjd;

  jd = (long)floor(mjd + 2400001.0);
  caldat(jd, &mm, &dd, &yyyy);
  fd += (double)dd;
  printf("%4d ", yyyy);
  if(mm<10){
    printf("0%1d ", mm);
  }else{
    printf("%2d ", mm);
  }
  if(fd<10){
    printf("0%6.5lf", fd);
  }else{
    printf("%7.5lf", fd);
  }
}


void caldat(long julian, int *mm, int *id, int *iyyy)
{
	long ja,jalpha,jb,jc,jd,je;

	if (julian >= IGREG) {
		jalpha=(long)(((double) (julian-1867216)-0.25)/36524.25);
		ja=julian+1+jalpha-(long) (0.25*jalpha);
	} else if (julian < 0) {
		ja=julian+36525*(1-julian/36525);
	} else
		ja=julian;
	jb=ja+1524;
	jc=(long)(6680.0+((double) (jb-2439870)-122.1)/365.25);
	jd=(long)(365*jc+(0.25*jc));
	je=(long)((jb-jd)/30.6001);
	*id=jb-jd-(long) (30.6001*je);
	*mm=je-1;
	if (*mm > 12) *mm -= 12;
	*iyyy=jc-4715;
	if (*mm > 2) --(*iyyy);
	if (*iyyy <= 0) --(*iyyy);
	if (julian < 0) iyyy -= 100*(1-julian/36525);
}
#undef IGREG
