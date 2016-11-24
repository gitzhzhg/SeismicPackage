/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/* 
 * ISATTY - pass on return from isatty(2)
 *
 * Usage:  isatty filedes
 *
 * See:   man isatty     for further information 
 */

/*
 * Credits:
 *	CWP: Shuki
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
 */
/**************** end self doc ********************************/

#include "cwp.h"
#include <unistd.h>


int
main(int argc, char **argv)
{
	int fildes;

	if (argc != 2) {
		fprintf(stderr, "Usage: %s fildes\n", argv[0]);
		return EXIT_FAILURE;
	}

	fildes = atoi(argv[1]);
	printf("isatty: saw fildes=%d, isatty? %d\n", fildes, isatty(fildes));

	return EXIT_SUCCESS;
}
