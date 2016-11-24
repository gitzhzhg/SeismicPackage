/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/* 
 * MAXINTS - Compute maximum and minimum sizes for integer types 
 *	(quick and dirty)
 *
 * Usage:  maxints
 *
 * Note: These results will be in limits.h on most systems
 *
 */

/*
 * Credits:
 *	CWP: Jack K. Cohen
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
 */
/**************** end self doc ********************************/

#include "cwp.h"

/* Prototype of function used internally */
short maxshort(void);
long maxlong(void);
int maxint(void);
unsigned short maxushort(void);
unsigned long maxulong(void);
unsigned int maxuint(void);

int
main()
{
	printf("max short =  %d\n", maxshort());
	printf("min short = %d\n", -(1 + maxshort()));

	printf("max long =  %ld\n", maxlong());
	printf("min long = %ld\n", -(1 + maxlong()));

	printf("max int =  %d\n", maxint());
	printf("min int = %d\n", -(1 + maxint()));

	printf("max unsigned short = %u\n", maxushort());

	printf("max unsigned long = %lu\n", maxulong());

	printf("max unsigned int = %u\n", maxuint());

	return EXIT_SUCCESS;
}

short maxshort(void)
{
	short i = 1, j;
	while (i > 0) {
		j = i;
		i <<= 1;
	}
	return(j - 1 + j);
}

long maxlong(void)
{
	long i = 1, j;
	while (i > 0) {
		j = i;
		i <<= 1;
	}
	return(j - 1 + j);
}

int maxint(void)
{
	int i = 1, j;
	while (i > 0) {
		j = i;
		i <<= 1;
	}
	return(j - 1 + j);
}

unsigned short maxushort(void)
{
	unsigned short i = 1, j = 0;
	while (i > j) {
		j = i;
		i <<= 1;
	}
	return(j - 1 + j);
}

unsigned long maxulong(void)
{
	unsigned long i = 1, j = 0;
	while (i > j) {
		j = i;
		i <<= 1;
	}
	return(j - 1 + j);
}

unsigned int maxuint(void)
{
	unsigned int i = 1, j = 0;
	while (i > j) {
		j = i;
		i <<= 1;
	}
	return(j - 1 + j);
}
