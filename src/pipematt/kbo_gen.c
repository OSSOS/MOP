#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979323846

main(argc, argv)
     int argc;
     char **argv;
{

  int i;
  int n, n_min, n_max;
  double x_min, x_max, dx, x;
  double y_min, y_max, dy, y;
  double r_min, r_max, dr, r;
  double a_min, a_mean, a_width, da, a;
  double m_min, m_max, dm, m;
  double pixscale;
  double ran2();
  long sd;

  if(argc != 15) {
    printf("args: x_min x_max y_min y_max r_min r_max a_mean a_width m_min m_max n_min n_max sd pixscale\n");
    exit(-1);
  }

  sscanf(argv[1], "%lf", &x_min);
  sscanf(argv[2], "%lf", &x_max);
  sscanf(argv[3], "%lf", &y_min);
  sscanf(argv[4], "%lf", &y_max);
  sscanf(argv[5], "%lf", &r_min);
  sscanf(argv[6], "%lf", &r_max);
  sscanf(argv[7], "%lf", &a_mean);
  sscanf(argv[8], "%lf", &a_width);
  sscanf(argv[9], "%lf", &m_min);
  sscanf(argv[10],"%lf", &m_max);
  sscanf(argv[11], "%d", &n_min);
  sscanf(argv[12], "%d", &n_max);
  sscanf(argv[13], "%d", &sd);
  sscanf(argv[14], "%lf", &pixscale);

  /* force the seed, sd, to be negative */

  sd = -labs(sd);

  /* determine the number of trials */

  n = (int)(n_min + (n_max-n_min)*ran2(&sd));

  /* determine deltas */

  dx = x_max - x_min;
  dy = y_max - y_min;
  dr = r_max - r_min;
  da = 2.0*a_width;
  a_min = a_mean - a_width;
  dm = m_max - m_min;

  /* print header */

  printf("#        x          y        mag   pix rate      angle  ''/h rate     id\n");

  /* loop to get n trials */

  i=0;
  
  while(i<n){

    /* determine values */
    
    x = x_min + dx*ran2(&sd);
    y = y_min + dy*ran2(&sd);
    r = r_min + dr*ran2(&sd);
    a = a_min + da*ran2(&sd);
    m = m_min + dm*ran2(&sd);

    /* print values */

    printf("%10.2lf %10.2lf %10.2lf %10.2lf %10.2lf %10.2lf %6d\n",
	   x, y, m, r/pixscale, a, r, i);
    i++;
  }
}



