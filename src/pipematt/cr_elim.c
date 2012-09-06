#include <stdio.h>
#include <math.h>

#define MAXLINE 200
#define SKIP 6
#define MAXELONG 9.99

main(argc, argv)
     int argc;
     char **argv;
{
  
  char line[1000];
  double x, y, flux, flux_max, area, elong;
  double fwhm, c_thresh, c, p_min, satur, obs_cons;
  double i;

  if(argc != 4) {
    printf("args: obs_cons fwhm p_min\n");
    exit(-1);
  }

  sscanf(argv[1], "%lf", &obs_cons);
  sscanf(argv[2], "%lf", &fwhm);
  sscanf(argv[3], "%lf", &p_min);

  for(i=0; i<SKIP; i++){
    fgets(line, MAXLINE, stdin);
  }

  /* obs_cons is the threshold for 5 pixel seeing */
  /* fwhm is measured in pixels */
  /* p_min is the value of flux_max below which all detections are accepted */

  c_thresh = obs_cons*(5.0*5.0/(fwhm*fwhm));
  
  while(scanf("%lf %lf %lf %lf %lf %lf", &x, &y, &flux, &area, &flux_max, &elong) != EOF){
    c = flux_max / flux;
    if(((c>=0.0 && c<=c_thresh && flux>0.0 && elong<MAXELONG) || flux_max<p_min)){
      printf("%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf\n", x, y, flux, area, flux_max, elong);
    }
  }
}
