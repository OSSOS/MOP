#include "procmega.h"

/* bias left side coords x1, x2, y1, y2 */
int BL[4] = {2, 29, 1, 4612};
/* bias right side coords */
int BR[4] = {2085, 2112, 1, 4612};
/* data left side coords */
int DL[4] = {33, 1056, 1, 4612};
/* data right side coords */
int DR[4] = {1057, 2080, 1, 4612};
/* total data array size */
int DA[4] = {1, 2047, 1, 4612};

void print_help();
float **getpixmem(int ysize, int xsize);
int get_imagedata_over(float **dataleft, float **dataright, float **biasleft, 
		       float **biasright, int *BL, int *BR, int *DL, int *DR, 
		       char *infilechip) ;
int get_fullimage(float **data, int *DA, char *infilename);
int write_image(float **data, int *DA, char *infilechip, int nchip, int flipchips, float avebiasleft, float avebiasright);
int write_procimage(float **data, int *DA, char *infilename, int usebias, int useflat, char *biasname, char *flatname);
int write_ushortimage(float **data, int *DA, char *infilename);

void print_help() {
  printf("Usage: procmega imagename [-o] [-p] [-u] [-c #] [-b biasname] [-f flatname] [-s]\n");
  printf(" Yea, you have to write the imagename with .fits included\n");
  printf(" If you are running this to remove overscan and trim image, and turn entire\n");
  printf("  mosaic into single chips, then use -o (probably RAW images only).\n");
  printf(" The -p option is for processed images; it will trim and separate processed images\n");
  printf("   without subtracting the overscan\n");
  printf(" Adding -u will flip chips 1-18 in Y  as well, to make everything have the same\n");
  printf("  orientation - N up and E left. Everything flips E/W\n");
  printf(" Additional option - full imagename, with -c chipnumber (0-based numbering)\n");
  printf("   will pull out one chip only\n");
  printf(" In all above cases the output files will be named FILENAME_CHIPID\n"); 
  printf("\n");
  printf(" Then -b and -f will bias subtract and flatfield, respectively.\n");
  printf("  This part is intended to be used on single chips - so don't give it mosaics\n");
  printf("  The modified file is written back to the original filename in this case\n");
  printf("  There's some bug right now that crashes output if you do both at once - \n");
  printf("  so do one of -b OR -f at a time for now.\n");
  printf("  (obviously you can't do -o and -b or -f at the same time - I'd have to figure\n");
  printf("   something smarter out for the input filenames)\n");
  printf("\n");
  printf(" -s option is to be used last. It will take the REAL image and change\n");
  printf("   it into a USHORT image. This option must be used alone.\n");
  exit(-1);
}

int main (int argc, char *argv[]) {
  
  int i, j, iarg; 
  char infilename[80], infilechip[80];
  char biasname[80], flatname[80];
  char outfilename[80];
  int dooverscan=0;
  int usebias=0;
  int useflat=0;
  int flipchips=0;
  int onechiponly = 0;
  int onechip = 0;
  int shorten = 0;
  int processed = 0;
  /* pointers to data and bias(overscan) arrays */
  float **biasleft, **biasright, **dataleft, **dataright;
  float **image, **image2, **biasimage, **flatfield;
  int ii, jj, nchip;
  float *dataptr, *dataptr2, *dataptr3;
  float avebiasleft, avebiasright; 
  int blxsize, blysize, brxsize, brysize;
  int dlxsize, dlysize, drxsize, drysize;
  int daxsize, daysize;

  float eps = 1.0e-10;
  float BADPIX=-10000;

  /* arrays supposed to be stored and accessed in y then x like fits format*/
  /* main program just gets data - doesn't actually know anything about fits files */
  /* the subroutines handle getting the data and writing the data - all the fitsio stuff */

  /* get input options */
  if (argc<2) print_help();
  /* first variable should be file name */
  strcpy(infilename, argv[1]); 
  /* process options from command line */
  for (iarg=2; iarg<argc; iarg++) {
    if (argv[iarg][0]=='-' && atof(argv[iarg])==0) {
      /* specifiying an option not a negative number */
      if (strcasecmp(argv[iarg]+1, "o")==0) {
	/* do overscan */
	dooverscan=1;
      }
      else if (strcasecmp(argv[iarg]+1, "c")==0) {
	onechip = atoi(argv[++iarg]);
	onechiponly = 1;
      }
      else if (strcasecmp(argv[iarg]+1, "b")==0) {
	/* use a bias subtraction frame */
	strcpy(biasname, argv[++iarg]);
	usebias=1;
      }
      else if (strcasecmp(argv[iarg]+1, "f")==0) {
	/* use a flat field division frame */
	strcpy(flatname, argv[++iarg]);
	useflat=1;
      }
      else if (strcasecmp(argv[iarg]+1, "u")==0) {
	/* flip chips to all N/S same */
	flipchips=1;
      }
      else if (strcasecmp(argv[iarg]+1, "s")==0) {
	/* Shorten image to USHORT */
	shorten=1;
      }
      else if (strcasecmp(argv[iarg]+1, "p")==0) {
	/* Elixir-processed image, split, flip + trim */
	processed=1;
	dooverscan = 1; /* just to get the flags right.. basically the same thing */
      }
      else {
	fprintf(stdout, "Unknown option %s\n", argv[iarg]);
	print_help();
      }
    }
  }

  if (dooverscan || usebias || useflat || flipchips || processed) shorten = 0;

  fprintf(stdout, "Read options for %s\n", infilename);
  if (dooverscan) fprintf(stdout, "Reading, removing and trimming overscan\n");
  if (processed) fprintf(stdout, "Trimming and splitting processed image\n");
  if (usebias) fprintf(stdout, "Got the bias option\n");
  if (useflat) fprintf(stdout, "Got the flat option\n");
  if (flipchips) fprintf(stdout, "Got the flip chips option\n");
  if (onechiponly) fprintf(stdout, "Got the one chip only option\n");
  if (shorten) fprintf(stdout, "Got the shorten option\n");

  /* NOTE - need to add 1 to each of xsize/ysize because counting 1 too -
     i.e., if DA[1] = 2 and DA[0] = 1 (a 2xX array), then need xsize=2 not 1 */
  daxsize = DA[1]-DA[0]+1;
  daysize = DA[3]-DA[2]+1;
  fprintf(stdout, "Entire data matrix %d %d size\n", daxsize, daysize);
  /* allocate memory for image and extra image */
  image = getpixmem(daysize, daxsize);
  image2 = getpixmem(daysize, daxsize);
  /* initialize these arrays */
  for (jj=0; jj<daysize; jj++) {
    dataptr = &image[jj][0];
    dataptr2 = &image[jj][0];
    for (ii=0; ii<daxsize; ii++) {
      *dataptr=0;
      *dataptr2 = 0;
      dataptr++;
      dataptr2++;
    }
  }
  
  /* now go to work */

  /* for each chip */ 
  for (nchip=STARTMEGA; nchip<=MAXCHIPS; nchip++) {
    /* set up the name of this chip */
    sprintf(infilechip, "%s[%d]", infilename, (nchip+1));
    if (!onechiponly)
      fprintf(stdout, "Working on chip %s\n", infilechip);
    
    if (dooverscan) {
      /* allocate overscan (==bias left and right) arrays */
      /* if this is the first time through, need to allocate storage */
      if (nchip==STARTMEGA) {
	dlxsize = DL[1] - DL[0] +1 ;
	dlysize = DL[3] - DL[2] +1 ;
	dataleft = getpixmem(dlysize, dlxsize);
	/*      fprintf(stdout, "dataleft matrix %d %d sizes\n", dlxsize, dlysize);  */
	drxsize = DR[1] - DR[0] + 1;
	drysize = DR[3] - DR[2] + 1;
	dataright = getpixmem(drysize, drxsize);
	/*      fprintf(stdout, "dataright matrix %d %d sizes\n", drxsize, drysize);  */
	blxsize = BL[1] - BL[0] + 1;
	blysize = BL[3] - BL[2] + 1;
	biasleft = getpixmem(blysize, blxsize);
	/*      fprintf(stdout, "biasleft matrix %d %d sizes\n", blxsize, blysize); */
	brxsize = BR[1] - BR[0] + 1;
	brysize = BR[3] - BR[2] + 1;
	biasright = getpixmem(brysize, brxsize);
	/*      fprintf(stdout, "biasright matrix %d %d sizes\n", brxsize, brysize);  */
      }
      /* and initialize arrays -  do separately in case of variable sizes */
      for (jj=0; jj<blysize; jj++) {
	dataptr = &biasleft[jj][0];
	for (ii=0; ii<blxsize; ii++) {
	  *dataptr = 0;
	  dataptr++;
	}
      }
      for (jj=0; jj<dlysize; jj++) {
	dataptr = &dataleft[jj][0];
	for (ii=0; ii<dlxsize; ii++) {
	  *dataptr = 0;
	  dataptr++;
	}
      }
      for (jj=0; jj<brysize; jj++) {
	dataptr=&biasright[jj][0];
	for (ii=0; ii<brxsize; ii++) {
	  *dataptr = 0 ;
	  dataptr++;
	}
      }
      for (jj=0; jj<drysize; jj++) {
	dataptr = &dataright[jj][0];
	for (ii=0; ii<drxsize; ii++) { 
	  *dataptr = 0;
	  dataptr++;
	}
      }
      
      /* get data for this chip */
      if (onechiponly) { /*if only one chip, skip to that one */
	nchip = onechip;
	sprintf(infilechip, "%s[%d]", infilename, (onechip+1));
	fprintf(stdout, "Working on chip %s\n", infilechip);
      }
      get_imagedata_over(dataleft, dataright, biasleft, biasright, 
			 BL, BR, DL, DR, infilechip);
      fprintf(stdout, "Done reading %s\n", infilechip); 
      
      /* find average values for overscan fit */
      avebiasleft = 0.0;
      for (jj=0; jj<blysize; jj++) {
	dataptr = &biasleft[jj][0];
	for (ii=0; ii<blxsize; ii++) {
	  avebiasleft += *dataptr;
	  dataptr++;
	}
      }
      avebiasleft = avebiasleft / (float) (blxsize) / (float) (blysize);
      fprintf(stdout, "Average value for left overscan: %g\n", avebiasleft);
      avebiasright = 0.0;
      for (jj=0; jj<brysize; jj++) {
	dataptr = &biasright[jj][0];
	for (ii=0; ii<brxsize; ii++) {
	  avebiasright += *dataptr;
	  dataptr++;
	}
      }
      avebiasright = avebiasright / (float) (brxsize) / (float) (brysize);
      fprintf(stdout, "Average value for right overscan: %g\n", avebiasright);
      
      if (processed) {
	avebiasleft = 0;
	avebiasright = 0;
	fprintf(stdout, "Processed image, so not subtracting overscan\n");
      }
      
      /* overscan correction simply constant for now */
      /* overscan subtract from data */
      for (jj=0; jj<dlysize; jj++) {
	dataptr = &dataleft[jj][0];
	for (ii=0; ii<dlxsize; ii++) {
	  *dataptr = *dataptr - avebiasleft;
	  dataptr++;
	}
      }
      for (jj=0; jj<drysize; jj++) {
	dataptr = &dataright[jj][0];
	for (ii=0; ii<drxsize; ii++) {
	  *dataptr = *dataptr - avebiasright;
	  dataptr++;
	}
      }
      /* write image data to one data array */
      fprintf(stdout, "Creating merged/trimmed image array for chip %d\n", nchip);
      for (jj=0; jj<dlysize; jj++) {
	dataptr = &image[jj][0];
	dataptr2 = &dataleft[jj][0];
	for (ii=0; ii<dlxsize; ii++) {
	  *dataptr = *dataptr2; 
	  dataptr++;
	  dataptr2++;
	}
      }
      for (jj=0; jj<drysize; jj++) {
	dataptr = &image[jj][dlxsize];
	dataptr2 = &dataright[jj][0];
	for (ii=0; ii<drxsize; ii++) {
	  *dataptr = *dataptr2; 
	  /* if (ii<drxsize/2) *dataptr=1;
	     else *dataptr=0; */
	  dataptr++;
	  dataptr2++;
	}
      }
    }
    
    /* can now write out data if simply want to merge amps */
    
    /* can flip image N/S if chip 0(1) through 17(18) */
    /* also have to flip these chips 1-18  E/W */
    if (flipchips && nchip<=HALFMEGA) {
      /* north south flip requires writing columns upside down */
      fprintf(stdout, "Flipping chip %d in %s\n", nchip, infilechip);
      /* make copy of flipped  up/down  left/right chip from
	 image (dataptr) into image2 (dataptr2) */
      for (jj=0;jj<daysize;jj++) {
	dataptr=&image[jj][0];
	dataptr2=&image2[(daysize-jj-1)][daxsize-1];
	for (ii=0; ii<daxsize; ii++) {
	  *dataptr2 = *dataptr;
	  dataptr2--;
	  dataptr++;
	}
      }
      /* now copy dataptr2 (image2) back into image (dataptr) */
      for (jj=0; jj<daysize; jj++) {
	dataptr=&image[jj][0];
	dataptr2=&image2[jj][0];
	for (ii=0; ii<daxsize; ii++){
	  *dataptr = *dataptr2;
	  dataptr2++;
	  dataptr++;
	}
      }
    }
    
    if (!dooverscan) {
      /* not doing overscan subtraction, but still need to read
	 the image */
      /* so only want to run through this process once, because 
	 we'll assume that these are single-chip images now */
      /* this also assumes that name given is full name of image, no
	 numbers additional necessary. Also, image is trimmed size */
      nchip=MAXCHIPS;
      strcpy(infilechip, infilename);
      if (get_fullimage(image, DA, infilechip)!=0) {
	/* then read image failed - exit */
	fprintf(stdout, "Error - can't open %s\n", infilechip);
	return(-1);
      }
    }
    
    /* can bias subtract */
    /* bias subtraction gets structure to compensate for overscan being "flat" */
    if (usebias) {
      if (get_fullimage(image2, DA, biasname)!=0) {
	fprintf(stdout, "Can't open bias image %s\n", biasname);
	return(-1);
      }
      /* subtract bias image from image */
      for (jj=0; jj<daysize; jj++) {
	dataptr=&image[jj][0];
	dataptr2=&image2[jj][0];
	for (ii=0; ii<daxsize; ii++) {
	  *dataptr = *dataptr - *dataptr2;
	  dataptr2++;
	  dataptr++;
	}
      }
      fprintf(stdout, "%s: Subtracted bias %s\n", infilechip, biasname);
    }
    
    /* goofy error - does this fix it ? */
    /*    if (usebias && useflat) {
	  write_procimage(image, DA, infilechip, 0, 0);
	  get_fullimage(image, DA, infilechip);
	  } */
    /* no it didn't */
    
    /* can flat field divide if given option here */
    if (useflat) {
      if (get_fullimage(image2, DA, flatname)!=0) {
	fprintf(stdout, "Can't open flatfield image %s\n", flatname);
	return(-1);
      }
      /* divide by flat field image */
      for (jj=0; jj<daysize; jj++) {
	dataptr=&image[jj][0];
	dataptr2=&image2[jj][0];
	for (ii=0; ii<daxsize; ii++) {
	  /* don't want to divide by zero or bad columns */
	  if (*dataptr2 < eps)
	    *dataptr = BADPIX;
	  *dataptr = *dataptr / *dataptr2;
	  dataptr2++;
	  dataptr++;
	}
      }
      fprintf(stdout, "%s: Flatfielded with %s\n", infilechip, flatname);
    }
    
    printf("infilechip is \n", infilechip);

    /* write data */
    if (dooverscan) {
      write_image(image, DA, infilechip, nchip, flipchips, avebiasleft, avebiasright);
    }
    else if (shorten) {
      write_ushortimage(image, DA, infilechip);
    }
    else {
      write_procimage(image, DA, infilechip, usebias, useflat, biasname, flatname);
    }

    if (onechiponly)
      nchip = MAXCHIPS; /* this stops us from going round the loop again */

  }
  
  return(0);
  
}
