#include <stdio.h>
#include <math.h>

#define MAXLINE 200
#define SKIP 0

main(argc, argv)
     int argc;
     char **argv;
{
  
  char line[1000];
  double x, y, flux, flux_max, area, elong;
  double fwhm, c_thresh, c, p_min, satur, obs_cons;
  double i;

  if(argc != 2) {
    printf("args: satur\n");
    exit(-1);
  }

  sscanf(argv[1], "%lf", &satur);

  for(i=0; i<SKIP; i++){
    fgets(line, MAXLINE, stdin);
  }

  /* satur is the flux_max level above which detections are considered saturated */

  while(scanf("%lf %lf %lf %lf %lf %lf", &x, &y, &flux, &area, &flux_max, &elong) != EOF){
    if(flux_max<satur){
      printf("%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf\n", x, y, flux, area, flux_max, elong);
    }
  }
}
