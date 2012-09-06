#include <stdio.h>
#include <math.h>

#define MAXLINE 200
#define LARGE 1e100
#define MAX_STARS 15000

main(argc, argv)
     int argc;
     char **argv;
{
  
  int n_files, i, j, k, l, m, num, nbr;
  double x, y, flux, flux_max, area;
  double elong, mag;
  int n_sources;
  double distance, flux_ratio, elong_max, area_min;
  double flux_min, satur;

  if(argc != 2) {
    printf("args: satur num\n");
    exit(-1);
  }

  sscanf(argv[1], "%lf", &distance);
  sscanf(argv[2], "%lf", &satur);
  sscanf(argv[3], "%lf", &elong_max);
  sscanf(argv[4], "%lf", &area_min);
  sscanf(argv[5], "%d", &num);
  
  flux_min = 1.0;

  j=0;
  while(scanf("%lf %lf %lf %lf %lf %lf", 
	       &x, &y, &flux, &area, &flux_max, &elong) != EOF &&
	j < num){
    if(flux_max < satur){
      j++;
      if(flux > 0.0){
	mag = -2.5*log10(flux);
      }else{
	mag = -2.5*log10(flux_min);
      }
      printf("%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf%10.2lf\n",
	     x, y, flux, area, flux_max, elong, mag);

    }
  }
}



