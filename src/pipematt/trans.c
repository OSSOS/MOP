#define PI 3.14159265358979323846
#define TWOPI 6.283185307179586476925287
#define MAXLINE 200

#include <stdio.h>
#include <math.h>

void transf(double a, double b, double c, double d, double e, double f, 
	      double *x, double *y);

char   *help[] = {
    "trans: transforms a list of x,y,mag coordinates according to a ",
    "         supplied linear transformation.                       ",
    "args: trans_file                                               ",
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
  char trans_file_name[100];
  FILE *trans_file;
  double x0, y0, x, y, flux, flux_max, area, mag, elong;
  double a, b, c, d, e, f;


  if(argc == 1) {
    print_help();
    exit(-1);
  }

  sscanf(argv[1], "%s", trans_file_name);

  trans_file = fopen(trans_file_name, "r");
  
  if(trans_file == NULL){
    printf("No such file.\n");
    exit(-1);
  }

  fscanf(trans_file, "%lf %lf %lf %lf %lf %lf", &a, &b, &c, &d, &e, &f);

  /*
  fgets(line,MAXLINE,stdin);
  printf("%s",line);
  */
  while(scanf("%lf %lf %lf %lf %lf %lf", &x, &y, &flux, &area, &flux_max, &elong) != EOF){
    x0 = x; 
    y0 = y;
    transf(a, b, c, d, e, f, &x, &y);
    printf("%8.2lf%8.2lf%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf\n", x0, y0, x, y, flux, area, flux_max, elong);
  }
}

void transf(double a, double b, double c, double d, double e, double f, 
	      double *x, double *y)
{
  double xp, yp;

  xp = a + b*(*x) + c*(*y);
  yp = d + e*(*x) + f*(*y);

  *x = xp;
  *y = yp;
  return;
}

double principal_value(double theta)
{
  theta -= 2.0*PI*floor(theta/(2.0*PI));
  return(theta);
}

