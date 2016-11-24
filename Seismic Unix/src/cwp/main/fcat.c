/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/* 
 * FCAT - fast cat with 1 read per file 
 *	
 * Usage: fcat file1 file2 ... > file3
 *	
 */

/*
 * Credits:
 *	Shuki
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
*/
/**************** end self doc ********************************/

#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "cwp.h"

#define MAXSIZE	(8*1024*1024)
char x[MAXSIZE];

int
main(int ac, char **av)
{
	register int fd, ic, n;
	struct stat buf;

	if (ac < 2) {
		fprintf(stderr,"usage: fcat file1 file2 ... > file3\n");
		return EXIT_FAILURE;
	}

	for (ic = 1; ic < ac; ic++) {
		fd = open(av[ic], O_RDONLY);
		if (fd == -1) fprintf(stderr, "can't open %s\n", av[ic]);
		fstat(fd, &buf);
		n = (int) buf.st_size;
		if (n > MAXSIZE) {
			fprintf(stderr, "%s is too big\n", av[ic]);
			return EXIT_FAILURE;
		}
		if (read(fd, x, n) != n) {
			fprintf(stderr, "read error from %s\n", av[ic]);
			return EXIT_FAILURE;
		}
		close(fd);
		if (write(1, x, n) != n) {
			fprintf(stderr, "write error from %s\n", av[ic]);
			return EXIT_FAILURE;
		}
	}
	return EXIT_SUCCESS;
}
