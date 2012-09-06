	subroutine readq (canal, long, chaine)

	integer*4
     1	  canal,
     1	  i,
     1	  long

	character
     1	  chaine*(*)

	long = len(chaine)
	do 1001 i = 1, long
	    chaine(i:i) = char(0)
 1001	continue

   10	format (a)
	read (canal, 10) chaine

 1002	continue
	if (chaine(long:long).eq.char(0)
     1		.or. chaine(long:long).eq.' ') then
	    long = long - 1
	    if (long.eq.0) goto 1003
	    goto 1002
	end if
 1003	continue

	return
	end