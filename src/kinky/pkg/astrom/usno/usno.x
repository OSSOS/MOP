# Copyright(c) 2000 JJ Kavelaars :-)

include	<config.h>
include	<mach.h>
include	<fset.h>
include	<fio.h>
include <ctype.h>


define 	SZ_BUF		2048

procedure strstr(sin,pat,sout)

char	sin[ARB], sout[ARB], pat[ARB] 

int	loop,start,last,current,strlen(), strmatch()

begin 

	start = strmatch(sin,pat)	
	last = strlen(sin)
	call printf ("start: %d last: %d\n");
	call pargi (start)
	call pargi (last)
	if ( start < 1 ) {
	   sout[1]=EOS 
	   return 
	}
	call printf("\n-->%s<---\n");
	call pargstr(sin)
	current = 1
	do loop = start, last {
	   sout[current] = sin[loop] 
	   current = current+1 
	}
	sout[current] = EOS 
	return 
end

task usno = t_usno
procedure t_usno()

bool	done
real	ra, dec, fov, lastch, clgetr()
char	hostname[SZ_LINE], buf[SZ_BUF], outline[SZ_BUF] 
char	netpath[SZ_LINE], ch, catfile[SZ_FNAME]
int	i, strlen(), getc(), stat, open()
int	ndopen(), read(), fd, getline(), nchars, fout

begin
	ra = clgetr("RA");
	dec = clgetr("DEC");
	fov = clgetr("FOV");
	call clgstr ("catalog",catfile, SZ_FNAME)
	# Get the USNO cataloge server address
	call clgstr ("server", hostname, SZ_LINE)
	iferr (fout=open(catfile,NEW_FILE,TEXT_FILE) ) {
	   call printf("Error opening new file: %s\n")
	   call pargstr(catfile)
	   return
	}


	# Connect to HTTP server (default port 80) on the given host.
	call fprintf(STDERR,"connecting\r");
	call sprintf (netpath, SZ_LINE, "inet:80:%s:text")
	    call pargstr (hostname)
	iferr (fd = ndopen (netpath, READ_WRITE)) {
	    call printf ("cannot access host: %s\n")
	    call pargstr(hostname)
	    return
	}
	call fprintf(STDERR,"connection established\r");

	# Send the get-url request to the server.
	call fprintf (fd, "GET /usnoget.php?ra=%f&dec=%f&rad=%f HTTP/1.0\n\n")
	    call pargr (ra)
	    call pargr (dec)
	    call pargr (fov)
	call flush (fd)
	call fprintf(STDERR,"request made, waiting for reply\n");
	call flush (STDERR)


	# Read and print the given URL.  The returned text consists of the
	# HTTP protocol header, a blank line, then the document text.
	# Since this is a debug routine we output the protocol header as
	# well as the document, but a real program would probably strip
	# the header since it is not part of the document data.

	repeat {
	    stat = getc(fd, ch)
	    if ( ch == '\n') {
		stat = getc(fd,ch)
		stat = getc(fd,ch)
		if ( ch == '\n' ) {
		# two in a row.. header now done
		  done =true;
		}
   	    }
	} until (done || stat==EOF)

	repeat {
	    nchars = read (fd, buf,SZ_BUF)
	    if ( nchars != EOF ) {
	        buf[nchars+1] = '\0'
  	        call putline (fout, buf) 
	    }
	} until (nchars == EOF)

	call close (fout)
	call close (fd)
end
