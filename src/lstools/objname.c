#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main (int argc, char *argv[]) 
{
  /* translate chip # into proper letter for LS survey */

  int nchip;
  char schip[0];
  
  if (argc<2) {
    printf("Give me the chip number you want translated\n");
    return(0);
  }  
  nchip = atoi(argv[1]);
  if (nchip<0||nchip>36) printf("chip out of bounds, come on now\n");
  if (nchip < 10) strcpy(schip,argv[1]);
  if (nchip==10) strcpy(schip,"A");
  if (nchip==11) strcpy(schip,"B");
  if (nchip==12) strcpy(schip,"C");
  if (nchip==13) strcpy(schip,"D");
  if (nchip==14) strcpy(schip,"E");
  if (nchip==15) strcpy(schip,"F");
  if (nchip==16) strcpy(schip,"G");
  if (nchip==17) strcpy(schip,"H");
  if (nchip==18) strcpy(schip,"I");
  if (nchip==19) strcpy(schip,"J");
  if (nchip==20) strcpy(schip,"K");
  if (nchip==21) strcpy(schip,"L");
  if (nchip==22) strcpy(schip,"M");
  if (nchip==23) strcpy(schip,"N");
  if (nchip==24) strcpy(schip,"O");
  if (nchip==25) strcpy(schip,"P");
  if (nchip==26) strcpy(schip,"Q");
  if (nchip==27) strcpy(schip,"R");
  if (nchip==28) strcpy(schip,"S");
  if (nchip==29) strcpy(schip,"T");
  if (nchip==30) strcpy(schip,"U");
  if (nchip==31) strcpy(schip,"V");
  if (nchip==32) strcpy(schip,"W");
  if (nchip==33) strcpy(schip,"X");
  if (nchip==34) strcpy(schip,"Y");
  if (nchip==35) strcpy(schip,"Z");
  
  printf("Translated chip #%d to %s\n", nchip, schip);

  printf("So objname is l|o|t|r|s FIELDNAME a|b|c %s\n",schip);
  
  return(1);
}
