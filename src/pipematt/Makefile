#
# 
# Makefile for the c routines Holman's version of the search code.
# M. Holman July 2001
# 
#
CC    = gcc				# the C compiler
COPTS = -O				# options for the C compiler
LIBS  = -lm

all: add_mag add_mag_fwhm bright convert_mjd cr_elim cr_elim_fwhm fwhm_medav gettime-exp kbo_gen non_stationary satur search select_trans_stars select_trans_stars_fwhm trans

add_mag: add_mag.c 
	$(CC) add_mag.c -o add_mag $(LIBS) $(COPTS)

add_mag_fwhm: add_mag_fwhm.c 
	$(CC) add_mag_fwhm.c -o add_mag_fwhm $(LIBS) $(COPTS)

bright: bright.c 
	$(CC) bright.c -o bright $(LIBS) $(COPTS)

convert_mjd: convert_mjd.c 
	$(CC) convert_mjd.c -o convert_mjd $(LIBS) $(COPTS)

cr_elim: cr_elim.c 
	$(CC) cr_elim.c -o cr_elim $(LIBS) $(COPTS)

cr_elim_fwhm: cr_elim_fwhm.c 
	$(CC) cr_elim_fwhm.c -o cr_elim_fwhm $(LIBS) $(COPTS)

fwhm_medav: fwhm_medav.c 
	$(CC) fwhm_medav.c -o fwhm_medav $(LIBS) $(COPTS)

gettime-exp: gettime-exp.c 
	$(CC) gettime-exp.c -o gettime-exp $(LIBS) $(COPTS)

kbo_gen: kbo_gen.c 
	$(CC) kbo_gen.c ran2.c -o kbo_gen $(LIBS) $(COPTS)

non_stationary: non_stationary.c 
	$(CC) non_stationary.c -o non_stationary $(LIBS) $(COPTS)

satur: satur.c 
	$(CC) satur.c -o satur $(LIBS) $(COPTS)

search: search.c 
	$(CC) search.c -o search $(LIBS) $(COPTS)

search_jjk: search_jjk.c 
	$(CC) search_jjk.c -o search_jjk $(LIBS) $(COPTS)

select_trans_stars: select_trans_stars.c 
	$(CC) select_trans_stars.c -o select_trans_stars $(LIBS) $(COPTS)

select_trans_stars_fwhm: select_trans_stars_fwhm.c 
	$(CC) select_trans_stars_fwhm.c -o select_trans_stars_fwhm $(LIBS) $(COPTS)

trans: $(ALL) trans.c 
	$(CC) trans.c -o trans $(LIBS) $(COPTS)

clean:
	/bin/rm -f add_mag add_mag_fwhm bright convert_mjd convert_radec gettime-exp kbo_gen non_stationary satur search select_trans_stars select_trans_stars_fwhm trans cr_elim cr_elim_fwhm fwhm_medav
