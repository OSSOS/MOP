#sample makefile


CC = gcc
CFLAGS = -O3 -I$(INCLUDEDIR)

#LIBS = -L/Users/lynne/Unix/cfitsio -lcfitsio -lm
LIBS = -L$(LIBDIRA) -lcfitsio -lm  #(for regular unix machines)
#LIBS = -L/home/salish/1/kavelaar/lib/LINUX -lcfitsio -lm  #(for regular unix machines)

PROGRAMS = procmega chip_locator cstats objname chpix

install: $(PROGRAMS)
	install $(PROGRAMS) $(BINDIRA)
	
clean:
	rm $(PROGRAMS)
	rm *.o

procmega: procmega.o procmegafunc.o 
	$(CC) $(CFLAGS) procmega.o procmegafunc.o  $(LIBS) -o procmega
	strip procmega

procmega.o : procmega.c procmega.h
	$(CC) $(CFLAGS) -c procmega.c

chip_locator: chip_locator.c
	$(CC) $(CFLAGS) chip_locator.c $(LIBS) -o chip_locator
	strip chip_locator

cstats: cstats.c
	$(CC) $(CFLAGS) -lm cstats.c -o cstats
	strip cstats

objname: objname.c
	$(CC) $(CFLAGS) -lm objname.c -o objname
	strip objname

chpix: chpix.c
	$(CC) $(CFLAGS) chpix.c  $(LIBS) -o chpix
	strip chpix
