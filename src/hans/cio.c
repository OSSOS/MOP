/*
 * NOV 2001
 * Modified by JJK so that routines have only one trailing underscore
 * when compiled with the variable SOLARIS defined
 */

#include	<stdio.h>
#include	<sys/types.h>
#include	<unistd.h>
#include	<netinet/in.h>
/*
 **************************************************************************
 *
 *   C_OPENER  --  Open A NEW File
 */
#ifdef SOLARIS
c_opener_(bb)
#else
c_opener__(bb)
#endif
char bb[];
{
	return(creat(bb,0644));
}
/*
 ***************************************************************************
 *
 *   C_CLOSER  --  Close A File
 */
#ifdef SOLARIS
c_closer_(fd)
#else
c_closer__(fd)
#endif
int *fd;
{
	return(close(*fd));
}
/*
 ***************************************************************************
 *
 *  C_WRITER  --  Use HTONL() To Select ENDIAN
 */
#ifdef SOLARIS
c_writer_(fd,buf,n)
#else
c_writer__(fd,buf,n)
#endif
int *fd, *n;
char buf[];
{
	u_long x;
	int i, nn;
	char b;

	nn = (*n);
	x = 102030405;
	if (htonl(x) != x) {
		for (i=0; i<nn; i+=4) {
			b = buf[i  ];
			buf[i  ] = buf[i+3];
			buf[i+3] = b;
			b = buf[i+1];
			buf[i+1] = buf[i+2];
			buf[i+2] = b;
		}
	}
	i = write(*fd,buf,nn);
	if (nn == i)
		return(0);
	else
		return(i);
}
/*
 *****************************************************************************
 *
 *  C_READER  --  Use HTONL() To Select ENDIAN
 */
#ifdef SOLARIS
c_reader_(fd,buf,n)
#else
c_reader__(fd,buf,n)
#endif
int *fd, *n;
char buf[];
{
	u_long x;
	int i, nn;
	char b;

	nn = (*n);
	i = read(*fd,buf,nn);
	if (i == nn) {
		x = 102030405;
		if (htonl(x) != x) {
			for (i=0; i<nn; i+=4) {
				b = buf[i  ];
				buf[i  ] = buf[i+3];
				buf[i+3] = b;
				b = buf[i+1];
				buf[i+1] = buf[i+2];
				buf[i+2] = b;
			}
		}
		return(0);
	} else
		return(i);
}
/*
 ***************************************************************************
 *
 *   C_ROOPEN -- Open A ReadOnly File
 */
#ifdef SOLARIS
c_roopen_(bb)
#else
c_roopen__(bb)
#endif
char bb[];
{
	return(open(bb,0));
}
/*
 ****************************************************************************
 *
 *  C_GENLEN  --  Return The Length Of A File
 */
#ifdef SOLARIS
c_genlen_(bb)
#else
c_genlen__(bb)
#endif
char bb[];
{
	int fd, n;

	if ((fd=open(bb,0)) < 3)
		return(-1);
	n = (int)lseek(fd, 0, SEEK_END);
	close(fd);
	return(n);
}
/*
 ****************************************************************************
 *
 *   position  --  Move file pointer
 */
#ifdef SOLARIS
c_position_(fd,nb)
#else
c_position__(fd,nb)
#endif
int *fd,*nb;
{
	return(lseek(*fd, *nb, SEEK_SET));
}
/*
 **************************************************************************
 *
 *   rawwriter  --  Just scribble
 */
#ifdef SOLARIS
c_rawwriter_(fd,bb,n)
#else
c_rawwriter__(fd,bb,n)
#endif
int *fd,*n;
char bb[];
{
	int i, nn;

	nn = (*n);
	i = write(*fd,bb,nn);
	if (i == nn)
		return(0);
	else
		return(i);
}
/*
 ****************************************************************************
 *
 *   rawreader -- Just read
 */
#ifdef SOLARIS
c_rawreader_(fd,bb,n)
#else
c_rawreader__(fd,bb,n)
#endif
int *fd,*n;
char bb[];
{
	int i, nn;

	nn = (*n);
	i = read(*fd,bb,nn);
	if (i == nn)
		return(0);
	else
		return(i);
}

