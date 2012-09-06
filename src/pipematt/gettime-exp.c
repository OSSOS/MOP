#include <stdio.h>
#include <math.h>

main(argc, argv)
     int argc;
     char **argv;
{
  
  double mjd, expt;

  if(argc != 1) {
    printf("args: \n");
    exit(-1);
  }

  while(scanf("%lf %lf", &mjd, &expt) != EOF){
    printf("%.7lf", mjd+expt/(2.0*24.0*60.0*60.0));
  }
}

