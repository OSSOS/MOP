/* short program to generate statistics for a file */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void print_help(void);
double find_ave(float *x, int num);
double find_stnddev(float *x, double ave, int num);

#define MAXIN 100000
#define skipline(inptr) while ((getc(inptr)!='\n') && !(feof(inptr)))

void print_help(void) {
  printf("Usage: infile < stats column# \n");
  printf("   input is file of numbers\n");
  printf("   output is mean, stndev\n");
  printf("column# is column want stats of, count from 1\n");
  exit(0);
}

main (int argc, char *argv[]) {

  int i, icol;
  float x[MAXIN];
  double mean, stdev;
  int num;

  if (argc<2) 
    print_help();
  
  icol = atoi(argv[1]);

  /* read in data */
  num = 0;
  while (num<MAXIN) {
    if (feof(stdin))
      break;
    for (i=1; i<icol; i++) 
      scanf("%*f");
    scanf("%f", &x[num]);
    if (feof(stdin)) 
      break;
    skipline(stdin);
    num++;
  }

  /* for (i=0; i<num; i++)
     printf("%f\n", x[i]); */

  if (num == MAXIN)
    printf("Warning: input could be greater than array can handle %d\n", num);
  
  mean = find_ave(x, num);
  stdev = find_stnddev(x, mean, num);

  printf("%f %f %d\n", mean, stdev, num);
  return;

}

double find_ave(float *x, int num) {
  
  float *xptr;
  double ave;
  int i;

  ave = 0;
  xptr = x;
  for (i=0; i<num; i++) {
    ave += *xptr; 
    xptr++;
  }
  ave = ave/(float) num;

  return(ave);
}

double find_stnddev(float *x, double ave, int num) {
  
  float *xptr;
  double stnddev;
  int i;
  double eps = 1e-20;

  stnddev = 0;
  xptr = x;
  for (i=0; i<num; i++) {
    stnddev += *xptr**xptr;
    xptr++;
  }

  stnddev = (stnddev/num  - ave*ave);
  if (stnddev < eps)
    stnddev = 0;
  else
    stnddev = sqrt(stnddev);
  
  return(stnddev);

}
  




