#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <float.h>


#include <fitsio.h>
#include <fitsio2.h>
#include <longnam.h>

/* take pointing from command line and find location in megacam image ..
   finds chip # (0 based) and approximate X/Y on that chip.
   Give it the RA/DEC of the object you want to find, THEN
   Can either give the image name OR the RA/DEC of the field center
*/

void print_help();

void print_help() {
  printf("chip_locator RA (hrs min sec) DEC (deg min sec) of object\n");
  printf("  then option of [-n imagename]\n");
  printf("  or [-r RA (hrs min sec) DEC (deg min sec)] of Megacam field\n");
  printf("Returns the chip number of the object, and approximate X and Y\n");
  exit(-1);
}

int main (int argc, char *argv[]) {

  int iarg;
  char imagename[80], keyname[80];
  int fitserr = 0;
  fitsfile *infptr;
  int rah_obj, ram_obj, decd_obj, decm_obj, decsign_obj;
  float ras_obj, decs_obj;
  int rah_image, ram_image, decd_image, decm_image, decsign_image;
  float ras_image, decs_image;
  float ra_obj, dec_obj;
  float ra_image, dec_image;
  float xoffset, yoffset;
  int chipnum, row, frow, chipx, chipy;
  float tempx, tempy;
  int done;
  int gap = 0;

  /* megacam chip properties */
  /* North is up (on all chips) - East is left (on all chips) ...
     that is, this assumes they have been flipped with procmega */
  float pixscale = .187; /* arcseconds per pixel */
  int chip_xsize = 2048; 
  int chip_ysize = 4612;
  int smgap = 70;  /* small gap size */
  int lggap = 425; /* large gap size */
  int halftotalx = (int) ((9*chip_xsize + 8*smgap)/2.0);
  int halftotaly = (int) ((4*chip_ysize + 2*lggap + smgap)/2.0);
  /*  printf("x %d y %d\n", halftotalx, halftotaly); */

  float deg_per_hrs = 360.0/24.0;
  float rad_per_deg = 3.14159 / 180.0;
  float arcsec_per_deg = 3600.0;

  /* read in arguments */
  if (argc<8) 
    print_help();
  
  /* read RA/DEC of POINTING */
  iarg = 1;
  rah_obj = atoi(argv[iarg++]);
  ram_obj = atoi(argv[iarg++]);
  ras_obj = atof(argv[iarg++]);
  ra_obj = rah_obj + ram_obj/60.0 + ras_obj/3600.0;
  decsign_obj = 1;
  decd_obj = atoi(argv[iarg++]);
  decm_obj = atoi(argv[iarg++]);
  decs_obj = atof(argv[iarg++]);
  if (decd_obj < 0) { /* dec less than zero */
    decd_obj = - decd_obj;
    decsign_obj = -1;
  }
  /* or special case w/ dec = -0 X X */
  if (decd_obj == 0 && argv[iarg-3][0] == '-') 
    decsign_obj = -1;
  dec_obj = decsign_obj * (decd_obj + decm_obj/60.0 + decs_obj/3600.0);
  printf("RA and DEC of OBJECT are %g (hrs) %g (deg)\n", ra_obj, dec_obj);
  
  /* get FIELD pointing */
  if (argv[iarg][0] == '-' && (atof(argv[iarg])==0)) { 
    /* specifying type of field pointing spec */
    if (strcasecmp(argv[iarg]+1, "n")==0) { /* giving image name */
      iarg++;
      strcpy(imagename, argv[iarg]);
      fits_open_file(&infptr, imagename, READONLY, &fitserr);
      if (infptr==NULL) printf("Couldn't open image %s\n", imagename);
      strcpy(keyname, "RA_DEG");
      fits_read_key(infptr, TFLOAT, keyname, &ra_image, NULL, &fitserr);
      ra_image = ra_image/deg_per_hrs; /* now have RA in hours */
      strcpy(keyname, "DEC_DEG");
      fits_read_key(infptr, TFLOAT, keyname, &dec_image, NULL, &fitserr);
      printf("RA and DEC of IMAGE %s are %g (hrs) %g (deg)\n", 
	     imagename, ra_image, dec_image);
      if (fitserr!=0) {
	printf("Some error in reading RA/DEC of image\n");
	exit(-1);
      }
    }
    else if (strcasecmp(argv[iarg]+1, "r")==0) { /* giving RA/DEC */
      iarg++;
      rah_image = atoi(argv[iarg++]);
      ram_image = atoi(argv[iarg++]);
      ras_image = atof(argv[iarg++]);
      ra_image = rah_image + ram_image/60.0 + ras_image/3600.0;
      decsign_image = 1;
      decd_image = atoi(argv[iarg++]);
      decm_image = atoi(argv[iarg++]);
      decs_image = atof(argv[iarg++]);
      if (decd_image < 0) { /* dec less than zero */
	decd_image = - decd_image;
	decsign_image = -1;
      }
      /* or special case w/ dec = -0 X X */
      if (decd_image == 0 && argv[iarg-3][0] == '-') 
	decsign_image = -1;
      dec_image = decsign_image * 
	(decd_image + decm_image/60.0 + decs_image/3600.0);
      printf("RA and DEC of IMAGE are %g (hrs) %g (deg)\n", 
	   ra_image, dec_image);
    }
    else 
      print_help();
  }
  else print_help();

  /* could easily add check for FLIPPED keyword */
  
  /* now have object RA/DEC and image RA/DEC */
  yoffset = (dec_obj - dec_image) * arcsec_per_deg / pixscale;
  xoffset = (ra_image - ra_obj)*deg_per_hrs*arcsec_per_deg
    * cos(dec_obj*rad_per_deg) / pixscale;
  /* notice opposite sign on xoffset b/c RA incr to left but X incr to right */
  printf("Total pixel X offset from center is %f; Total pixel Y offset is %f\n", 
	 xoffset, yoffset);
  
  
  tempx = xoffset + halftotalx; /* correct X to left side = 0 column */
  done = 0;
  if (tempx < 0) {
    printf("Off field by about %d pix to East (left - chip0 side)\n",
	   (int)(-tempx));
    chipx = (int)(tempx);
    chipnum=-1;
    done = 1;
  }
  if (xoffset > halftotalx) {
    printf("Off field by about %d pix to West (right - chip8 side)\n",
	   (int) (xoffset-halftotalx));
    chipx = (int)(xoffset);
    done =1;
    chipnum=-1;
  }
  chipnum = 0;
  if (tempx<=chip_xsize) {
    chipx = (int) (tempx);
    done=1;
  }
  while (!done) {
    tempx = tempx - chip_xsize - smgap;
    chipnum++;
    if (tempx <= chip_xsize && tempx>=0) {
      /* we're in this chip */
      chipx = (int) (tempx); 
      done = 1;
    }
    else if (tempx<0) {
      /* or we're in the gap */
      chipx = (int)(tempx + smgap);
      printf("In chip gap between %d and %d columns, by about %d pix fm %d\n",
	     chipnum-1, chipnum, chipx, chipnum-1);
      done=1;
      gap = 1;
    }
    /* or we're going to go back around to the next chip */
  }
  
  tempy = yoffset + halftotaly; /* correct Y to bottom level */
  done = 0;
  if (tempy < 0) {
    printf("Off field by about %d pix to South (bottom -row27 side)\n",
	   (int)(-tempy));
    chipy = (int)(tempy);
    done = 1;
    frow = -1;
  }
  if (yoffset > halftotaly) {
    printf("Off field by about %d pix to North (top -row0)\n",
	   (int)(yoffset-halftotaly));
    chipy = (int)(yoffset);
    done=1;
    frow = -1;
  }
  row = 3;
  if (tempy<=chip_ysize && !done) {
    chipy = (int)(tempy);
    done = 1;
    frow = row;
  }
  tempy = tempy - chip_ysize - lggap;
  row = 2;
  if (tempy<=chip_ysize && tempy>=0) {
    chipy = (int)(tempy);
    done=1;
    frow = row;
  }
  else if (tempy<0 && !done) {
    chipy = (int)(tempy+lggap);
    printf("In chip gap between %d and %d rows, by about %d pix above %d\n",
	   row+1, row, chipy, row+1);
    done=1;
    gap =1;
    frow = row;
  }
  tempy = tempy-chip_ysize - smgap;
  row=1;
  if (tempy<=chip_ysize && tempy>=0) {
    chipy = (int)(tempy);
    done=1;
    frow = row;
  }
  else if (tempy<0 && !done) {
    chipy = (int)(tempy+smgap);
    printf("In chip gap between %d and %d rows, by about %d pix above %d\n",
	   row+1, row, chipy, row+1);
    done=1;
    gap = 1;
    frow = row;
  }
  tempy = tempy - chip_ysize - lggap;
  row=0;
  if (tempy<=chip_ysize && tempy>=0) {
    chipy = (int) (tempy);
    done=1;
    frow = row;
  }
  else if (tempy<0 && !done) {
    chipy = (int) (tempy+lggap);
    printf("In chip gap between %d and %d rows, by about %d pix above %d\n",
	   row+1, row, chipy, row+1);
    done=1;
    frow = row;
    gap = 1;
  }

  chipnum = chipnum + 9 * (frow);
  if (!gap) 
    printf("Object located in approx. chip %d, x %d y %d\n", chipnum, chipx, chipy);
  else 
    printf("Object located in gap, but approximately chip %d, x %d y %d\n",
	   chipnum, chipx, chipy);
  
  return(0);
}
