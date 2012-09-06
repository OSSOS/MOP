DEST = .
FC=g77
FFLAGS	= -O
CFLAGS	= -O

OBJ	= corner.o eatit.o cio.o saveit.o square.o readq.o whereis.o

produce_starlist:	$(OBJ)
	ar vr $(DEST)/libstarlist.a $(OBJ)

