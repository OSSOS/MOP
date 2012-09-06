#include <stdio.h>
#include <math.h>

#define MAX_IDX 1000
#define MAXLINE 200

main(argc, argv)
     int argc;
     char **argv;
{
  
  char line[5][1000];
  int n_files, i, j, k, l, m;
  int m_min, m_max;
  int flag[15000], tmp_flag;
  char file_name[5][100];
  FILE *file[5];
  double x[5][15000], y[5][15000], x0[5][15000], y0[5][15000];
  double flux[5][15000], flux_max[5][15000], area[5][15000], mag[5][15000];
  double elong[5][15000];
  int n_sources[5];
  int idx[5][MAX_IDX];
  double bin_size = 10.0;
  double x_tmp, y_tmp, mag_tmp;
  double dx, dy, d2, seeing;

  if(argc == 1) {
    printf("args: seeing file_0 file_1 ...\n");
    exit(-1);
  }

  sscanf(argv[1], "%lf", &seeing);

  n_files = argc-2;

  for(i=0; i<n_files; i++){
    sscanf(argv[i+2], "%s", file_name[i]);
    file[i] = fopen(file_name[i], "r");
  }

  /*
  for(i=0; i<n_files; i++){
    fgets(line[i],MAXLINE,file[i]);
  }
  */

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

  /*
  for(i=0; i<n_files; i++){
    for(j=0; j<MAX_IDX; j++){
      printf("%d %d %d\n", i, j, idx[i][j]);
    }
  }
  */

  i=0;
  /*printf("%s", line[i]);*/
  for(k=0; k<n_sources[i]; k++){

    flag[k] = 1;
    tmp_flag = 1;

    m_min = (x[i][k]-seeing)/bin_size;
    if(m_min<0) m_min = 0;
    m_max = (x[i][k]+seeing)/bin_size + 1;

    for(j=i+1; j<n_files; j++){

      for(l=idx[j][m_min]; l<idx[j][m_max]; l++){

	dx = x[i][k]-x[j][l];
	dy = y[i][k]-y[j][l];
	d2 = dx*dx + dy*dy;

	if(d2 < seeing*seeing){
	  tmp_flag++;
	}

      }
    }
    if(tmp_flag < n_files-1){
      printf("%8.2lf%8.2lf%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf\n",
	     x0[i][k], y0[i][k], x[i][k], y[i][k], flux[i][k], area[i][k], flux_max[i][k], elong[i][k]);
    }
  }
}



