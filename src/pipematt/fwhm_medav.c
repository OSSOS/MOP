#include <stdio.h>
#include <math.h>

#define MAXLINE 200
#define LARGE 1e100
#define MAX_STARS 15000
#define FRAC 0.6

main(argc, argv)
     int argc;
     char **argv;
{
  
  /* This program requires input sorted on   */
  /* the fwhm field.                         */

  char line[5][1000];
  int n_files, i, j, k, l, m, num, nbr;
  int flag[MAX_STARS];
  char file_name[100];
  FILE *file;
  double x[MAX_STARS], y[MAX_STARS];
  double flux[MAX_STARS], flux_max[MAX_STARS], area[MAX_STARS];
  double elong[MAX_STARS], mag[MAX_STARS], fwhm[MAX_STARS];
  int n_sources, id[MAX_STARS];
  int match[MAX_STARS];
  double x_tmp, y_tmp, mag_tmp;
  double dx, dy, d2, distance, flux_ratio, elong_max;
  double flux_min, satur, fwhm_av, fwhm_med, fwhm_s2, fwhm_sd;

  if(argc != 1) {
    printf("args: \n");
    exit(-1);
  }

  /* sscanf(argv[1], "%lf", &distance); */

  fwhm_av = 0.0;
  fwhm_s2 = 0.0;
  j=0;
  while(scanf("%lf %lf %lf %lf %lf", 
	       &x[j], &y[j], &mag[j], &fwhm[j], &id[j]) != EOF){
    fwhm_av += fwhm[j];
    fwhm_s2 += fwhm[j]*fwhm[j];
    j++;
  }
  n_sources = j;

  fwhm_av /= n_sources;

  /* fwhm_sd = fwhm_s2/n_sources - fwhm_av*fwhm_av; */
  fwhm_sd = sqrt(fwhm_s2/n_sources - fwhm_av*fwhm_av);

  j = n_sources/2;

  if(n_sources % 2){
    fwhm_med = (fwhm[j] + fwhm[j + 1])/2.0;
  }else{
    fwhm_med = fwhm[j];
  }

  if(fwhm_sd > FRAC*fwhm_med){
    printf("%lf\n", -1.0);
  }else{
    printf("%lf\n", fwhm_med);
  }
}



