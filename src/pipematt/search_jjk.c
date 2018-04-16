#define PI 3.14159265358979323846
#define TWOPI 6.283185307179586476925287
#define MAX_IDX 1000
#define SKIP 7
#define MAXLINE 200

#include <stdio.h>
#include <math.h>


double min(double a, double b)
{
  if ( a < b ) return a;
  return b;
}

double max(double a, double b)
{
  if ( a > b ) return a; 
  return b;
}

double median3(double a, double b, double c)
{
  double x, y;
  x = min(min(a, b), c);
  y = max(max(a, b), c);
  return a + b + c - x - y;
}


char   *help[] = {
    "search_idx: searches for linearly moving objects files that contain ",
    "         x,y,mag groups of sources.                                  ",
    "args: thresh rate_min rate_max angle_center width plate_scale max_flux_ratio minimum_median_flux max_area_ratio file_0 t_file_0 fwhm_file_0 file_1 t_file_1 fwhm_file_1 ...                ",
    0
};

void
print_help(void)
{
  int	i;
  for (i = 0; help[i] != 0; i++)
    fprintf (stderr, "%s\n", help[i]);
  exit (1);
}

main(argc, argv)
     int argc;
     char **argv;
{
  
  char line[1000];
  int comment;
  int source_count;
  int n_files, i, j, k, l, m, n;
  int m_min, m_max;
  int n_min, n_max;
  char file_name[5][100];
  FILE *file[5];
  double tobs[5];
  double x[5][15000], y[5][15000], flux[5][15000], flux_max[5][15000], area[5][15000];
  double x0[5][15000], y0[5][15000], elong[5][15000], fwhm[3];
  int n_sources[5];
  int idx[5][MAX_IDX];
  double bin_size = 10.0;
  double xp, yp;
  double dx, dy, dx3p, dy3p, thresh, dist_min, dist_max, rate;
  double rate_min, rate_max;
  double angle, angle_center, width, dangle, dang_tmp;
  double plate_scale;
  double d_ij_2, d_ik_2, d_jk_2, d3p_2;
  double t0, delta_t;
  double principal_value();
  double max_flux_ratio, minimum_median_flux, min_area;

  if(argc == 1) {
    print_help();
    exit(-1);
  }

  sscanf(argv[1], "%lf", &thresh);
  sscanf(argv[2], "%lf", &rate_min);
  sscanf(argv[3], "%lf", &rate_max);
  sscanf(argv[4], "%lf", &angle_center);
  sscanf(argv[5], "%lf", &width);
  sscanf(argv[6], "%lf", &plate_scale);
  sscanf(argv[7], "%lf", &max_flux_ratio);
  sscanf(argv[8], "%lf", &minimum_median_flux);
  sccanf(argv[9], "%lf", &min_area);

  /* Will compute maximum distance based on time and rate,
     assuming the rate is in arcsec/hour, the times are 
     modified julian dates, and the plate scale is
     in arcseconds.
  */

  n_files = (argc-9)/3;

  for(i=0; i<n_files; i++){
    sscanf(argv[3*i+10], "%s", file_name[i]);
    file[i] = fopen(file_name[i], "r");
    for(j=0; j<SKIP; j++){
      fgets(line, MAXLINE, file[i]);
    }
    sscanf(argv[3*i+11], "%lf", &tobs[i]);
    sscanf(argv[3*i+12], "%lf", &fwhm[i]);
    /*printf("%lf\n", tobs[i]);*/
  }

  for(i=0; i<n_files; i++){
    k=0;
    j=0;
    idx[i][0] = 0;
    while(fscanf(file[i], "%lf %lf %lf %lf %lf %lf %lf %lf", 
		 &x0[i][j], &y0[i][j], &x[i][j], &y[i][j], 
		 &flux[i][j], &area[i][j], &flux_max[i][j], &elong[i][j]) != EOF){

	/*
      printf("%08.2lf %08.2lf %08.2lf %08.2lf %013.2lf %09.1lf %010.2lf %06.2lf\n",
	     x0[i][j], y0[i][j], x[i][j], y[i][j], flux[i][j], area[i][j], flux_max[i][j], elong[i][j]);
	*/
      

      m = x[i][j]/bin_size;
      if(m>k){
	for(l=m; l>k; l--){
	  idx[i][l] = j;
	}
	k=m;
      }
      j++;
    }
    k++;
    for(m=k; m<MAX_IDX; m++){
      idx[i][m] = j;
    }
    n_sources[i] = j;
  }

	/*
  for(i=0; i<n_files; i++){
    for(j=0; j<MAX_IDX; j++){
      printf("%d %d %d\n", i, j, idx[i][j]);
    }
  }
	*/

  t0 = tobs[0];

  for(i=0; i<n_files; i++){
    tobs[i] -= t0;   /* Do everything in terms of deltas. */
    tobs[i] *= 24.0; /* Convert to hours.                 */
  }

  i = 0;
  k = 1;
  j = 2;

  delta_t = tobs[j]-tobs[i];
  dist_min = fabs(delta_t)*rate_min/plate_scale;
  dist_max = fabs(delta_t)*rate_max/plate_scale;
  angle_center *= PI/180.0;
  width *= PI/180.0;

  /*printf("%lf %lf %lf %lf %lf %lf\n", angle_center, width, dist_min, dist_max, thresh, delta_t);*/
  source_count = 0;
  for(l=0; l<n_sources[i]; l++){

    m_min = (x[i][l]-dist_max)/bin_size;
    if(m_min<0) m_min = 0;
    m_max = (x[i][l]+dist_max)/bin_size + 1;

    for(m=idx[j][m_min]; m<idx[j][m_max]; m++){

      dx = x[j][m]-x[i][l];
      dy = y[j][m]-y[i][l];
      d_ij_2 = dx*dx + dy*dy;
      angle = atan2(dy,dx);

      /* This assumes that angle_center is -PI to PI at this point. */
      
      dangle = principal_value(fabs(angle-angle_center));

      if(dangle>PI){ /* take complement */
	dangle = TWOPI - dangle;
      }

      if(d_ij_2 >= dist_min*dist_min &&
	 d_ij_2 < dist_max*dist_max &&
	 dangle <= width
	 ){

	/* extrapolated position at kth time */

	xp = x[i][l] + dx*(tobs[k]-tobs[i])/(tobs[j]-tobs[i]);
	yp = y[i][l] + dy*(tobs[k]-tobs[i])/(tobs[j]-tobs[i]);

	n_min = (xp-thresh)/bin_size;
	if(n_min<0) n_min = 0;
	n_max = (xp+thresh)/bin_size + 1;

	/*for(n=0; n<n_sources[k]; n++){*/
        comment = 1;
	for(n=idx[k][n_min]; n<idx[k][n_max]; n++){
	  
	  /* distance between k source and predicted position */
	  dx3p = xp-x[k][n];
	  dy3p = yp-y[k][n];
	  d3p_2 = dx3p*dx3p + dy3p*dy3p;

	  if(
	     d3p_2 < thresh*thresh
	     ){
	    /* check if interstellar conditions are met */
	    /* x pixel locations differ by more than 2 pixels*/
	    if ( fabs(x0[i][l] - x0[j][m]) < 2 ) {
	      comment = 0;
	    }

	    /* median flux value > 1000 */

	    if ( median3(flux[i][l], flux[k][n], flux[j][m] ) < minimum_median_flux ) {
	      comment = 0;
	    }

	    /* The ratio between the maximum and minimum flux values should be less than 3. */

	    if ( max(max(flux[i][l], flux[k][n]), flux[j][m]) > max_flux_ratio*min(min(flux[i][l], flux[k][n]), flux[j][m]) ) {
	      comment = 0 ;
	    }

	    /* The size of the candidate for each image should be greater than the pi/4*s^2, where s is FWHM of the the CCD it's on. */

	    if ( area[i][l] < min_area || area[k][n] < min_area || area[j][m] < min_area ) {
	      comment = 0;
	    }
	    if ( comment != 1 ) { 
	      continue;
	    }
	    source_count++;
	    printf("\n");
	    printf("%8.2lf%8.2lf%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf\n",
		   x0[i][l], y0[i][l], x[i][l], y[i][l], flux[i][l], area[i][l], flux_max[i][l], elong[i][l]);
	    printf("%8.2lf%8.2lf%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf\n",
		   x0[k][n], y0[k][n], x[k][n], y[k][n], flux[k][n], area[k][n], flux_max[k][n], elong[k][n]);
	    printf("%8.2lf%8.2lf%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf\n",
		   x0[j][m], y0[j][m], x[j][m], y[j][m], flux[j][m], area[j][m], flux_max[j][m], elong[j][m]);

	  }
	}
      }
    }
  }
}

double principal_value_0(double theta)
{
  theta -= 2.0*PI*floor(theta/(2.0*PI));
  if(theta < PI)
    return(theta);
  else 
    return(theta - 2.0*PI);
}

double principal_value(double theta)
{
  theta -= 2.0*PI*floor(theta/(2.0*PI));
  return(theta);
}

