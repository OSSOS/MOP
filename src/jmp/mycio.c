#include	<stdio.h>
#include	<sys/types.h>
#include	<unistd.h>
#include	<netinet/in.h>
#include        <fcntl.h>
/*
 **************************************************************************
 *
 *   C_OPENER  --  Open A NEW File
 */
int c_opener__(bb)
char bb[];
{
	return(creat(bb,0644));
}
/*
 ***************************************************************************
 *
 *   C_CLOSER  --  Close A File
 */
int c_closer__(fd)
int *fd;
{
	return(close(*fd));
}
/*
 ***************************************************************************
 *
 *  C_WRITER  --  Use HTONL() To Select ENDIAN
 */
int c_writer__(fd,buf,n)
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
int c_reader__(fd,buf,n)
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
int c_roopen__(bb)
char bb[];
{
	return(open(bb,0));
}
/*
 ****************************************************************************
 *
 *  C_GENLEN  --  Return The Length Of A File
 */
int c_genlen__(bb)
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
int c_position__(fd,nb)
int *fd,*nb;
{
	return(lseek(*fd, *nb, SEEK_SET));
}
/*
 **************************************************************************
 *
 *   rawwriter  --  Just scribble
 */
int c_rawwriter__(fd,bb,n)
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
int c_rawreader__(fd,bb,n)
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
