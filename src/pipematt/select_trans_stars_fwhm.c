#include <stdio.h>
#include <math.h>

#define MAXLINE 200
#define LARGE 1e100
#define MAX_STARS 15000

main(argc, argv)
     int argc;
     char **argv;
{
  
  /* This is the same program as select_trans_stars.c,  */
  /* but this one will accept a FWHM field   */
  /* from the sextractor results.            */

  char line[5][1000];
  int n_files, i, j, k, l, m, num, nbr;
  int flag[MAX_STARS];
  char file_name[100];
  FILE *file;
  double x[MAX_STARS], y[MAX_STARS];
  double flux[MAX_STARS], flux_max[MAX_STARS], area[MAX_STARS];
  double elong[MAX_STARS], mag[MAX_STARS], fwhm[MAX_STARS];
  int n_sources;
  int match[MAX_STARS];
  double x_tmp, y_tmp, mag_tmp;
  double dx, dy, d2, distance, flux_ratio, elong_max, area_min;
  double flux_min, satur;

  if(argc != 7) {
    printf("args: distance satur elong_max area_min num file_name\n");
    exit(-1);
  }

  sscanf(argv[1], "%lf", &distance);
  sscanf(argv[2], "%lf", &satur);
  sscanf(argv[3], "%lf", &elong_max);
  sscanf(argv[4], "%lf", &area_min);
  sscanf(argv[5], "%d", &num);
  sscanf(argv[6], "%s", file_name);

  file = fopen(file_name, "r");
  
  flux_min = 1.0;

  j=0;
  while(fscanf(file, "%lf %lf %lf %lf %lf %lf %lf", 
	       &x[j], &y[j], &flux[j], &area[j], &flux_max[j], &elong[j], &fwhm[j]) != EOF){

    if(flux_max[j] < satur && elong[j] < elong_max && area[j] >= area_min){
      flag[j] = 0;
    }else{
      flag[j] = 1;
    }

    j++;
  }
  n_sources = j;

  nbr = 0;

  for(k=0; k<n_sources && nbr < num; k++){
    for(l=0; l<k; l++){
      
      dx = x[k]-x[l];
      dy = y[k]-y[l];
      d2 = dx*dx + dy*dy;

      if(d2 < distance*distance){
	flag[l] = 1;
	if(flag[k] == 0){
	  flag[k] = 1;
	  nbr--;
	}
      }
    }
    
    if(flag[k] == 0 && 
       flux_max[k] < satur &&
       elong[k] < elong_max &&
       area[k] >= area_min){
      nbr++;
    }
  }

  nbr = 0;
  for(k=0; k<n_sources && nbr < num; k++){
    if(flag[k] == 0){
      nbr++;
      
      if(flux[k] > 0.0){
	mag[k] = -2.5*log10(flux[k]);
      }else{
	mag[k] = -2.5*log10(flux_min);
      }

      printf("%8.2lf%8.2lf%10.2lf%6.2lf %6d\n",
	     x[k], y[k], mag[k], fwhm[k], k);
    }
  }
}



