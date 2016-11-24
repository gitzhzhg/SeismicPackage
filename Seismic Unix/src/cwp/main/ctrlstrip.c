/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/* 
 * CTRLSTRIP - Strip non-graphic characters
 *
 * ctrlstrip <dirtyfile >cleanfile
 * 
 */
/**************** end self doc ********************************/

/* Credits:
 *	Masscomp RTU Programming Manual
 *	CWP: Jack
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
*/

#include <stdio.h>
#include <ctype.h>

#define printable(c)	isprint((c)) || (c) == '\t' || (c) == '\n'

int main()	/* ctrlstrip - Strip non-graphic characters */
{
	int c;

	while (EOF != (c = getchar())) {
		if (printable(c)) {
			putchar(c);
		}
	}

	return(0);
}
