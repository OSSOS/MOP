#include <stdio.h>
#include <math.h>

#define MAX_IDX 1000
#define MAXLINE 200
#define LARGE 1e100
#define MAX_STARS 15000

main(argc, argv)
     int argc;
     char **argv;
{
  
  char line[5][1000];
  int n_files, i, j, k, l, m;
  int m_min, m_max;
  int flag[MAX_STARS], tmp_flag;
  char file_name[5][100];
  char bright_file_name[5][100];
  FILE *file[5];
  FILE *bright_file[5];
  double x[5][MAX_STARS], y[5][MAX_STARS], x0[5][MAX_STARS], y0[5][MAX_STARS];
  double flux[5][MAX_STARS], flux_max[5][MAX_STARS], area[5][MAX_STARS], mag[5][MAX_STARS];
  double elong[5][MAX_STARS];
  int n_sources[5];
  int idx[5][MAX_IDX];
  int match[5][MAX_STARS], num, nbr;
  double bin_size = 10.0;
  double x_tmp, y_tmp, mag_tmp;
  double dx, dy, d2, d2min, seeing, flux_ratio, fr_min, fr_max, elong_max;

  if(argc == 1) {
    printf("args: seeing fr_max elong_max num file_0 bright_file_0 file_1 bright_file_1 ...\n");
    exit(-1);
  }

  sscanf(argv[1], "%lf", &seeing);
  sscanf(argv[2], "%lf", &fr_max);
  sscanf(argv[3], "%lf", &elong_max);
  sscanf(argv[4], "%d", &num);


  n_files = (argc-4)/2;

  for(i=0; i<n_files; i++){
    sscanf(argv[2*i+5], "%s", file_name[i]);
    file[i] = fopen(file_name[i], "r");
    sscanf(argv[2*i+6], "%s", bright_file_name[i]);
    bright_file[i] = fopen(bright_file_name[i], "a");
  }

  for(i=0; i<n_files; i++){
    j = 0;
    k=0;
    idx[i][0] = 0;
    while(fscanf(file[i], "%lf %lf %lf %lf %lf %lf %lf %lf", 
		 &x0[i][j], &y0[i][j], &x[i][j], &y[i][j], 
		 &flux[i][j], &area[i][j], &flux_max[i][j], &elong[i][j]) != EOF){
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

  i=0;
  fr_min = 1.0/fr_max;
  nbr = 0;

  for(k=0; k<n_sources[i] && nbr < num; k++){

    match[i][k] = k;

    m_min = (x[i][k]-seeing)/bin_size;
    if(m_min<0) m_min = 0;
    m_max = (x[i][k]+seeing)/bin_size + 1;

    for(j=i+1; j<n_files; j++){

      d2min = LARGE;
      match[j][k] = -1;

      for(l=idx[j][m_min]; l<idx[j][m_max]; l++){

	dx = x[i][k]-x[j][l];
	dy = y[i][k]-y[j][l];
	d2 = dx*dx + dy*dy;
	flux_ratio = flux[j][l]/flux[i][k];

	if(
	   d2 < d2min && 
	   d2 < seeing*seeing &&
	   flux_ratio < fr_max &&
 	   flux_ratio > fr_min &&
	   elong[i][k] < elong_max &&
	   elong[j][l] < elong_max
	   ){

	  match[j][k] = l;
	  d2min = d2;
	  
	}

      }
    }

    flag[k] = 1;
    for(j=0; j<n_files; j++){
      m = match[j][k];
      if(m == -1){
	flag[k] = 0;
      }
    }

    if(flag[k] == 1){
      nbr++;
      for(j=0; j<n_files; j++){
	m = match[j][k];
	fprintf(bright_file[j], "%8.2lf%8.2lf%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf\n",
		x0[j][m], y0[j][m], x[j][m], y[j][m], flux[j][m], area[j][m], flux_max[j][m], elong[j][m]);
      }
    }
    
  }

}



