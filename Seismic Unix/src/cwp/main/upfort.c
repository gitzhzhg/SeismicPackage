/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*
 * UPFORT - change Fortran programs to upper case, preserving strings
 *
 * Usage:   upfort < infile.f > outfile.f
 *
 * Reverse of: downfort
 *
 */
/**************** end self doc ********************************/

/*
 * Author: Brian Sumner
 */

#include "cwp.h"
#include <ctype.h>

int
main()
{

	register int c;
	while ((c = getchar()) != EOF) {
		if (c=='\n') putchar(c);
		else if (c=='*' || c=='C' || c=='c' || c=='D') {
			putchar(c);
			while((c = getchar()) != '\n') putchar(c);
			putchar(c);
		} else {
			putchar(c);
			while ((c = getchar()) != '\n') {
				if (c != '\'') putchar(islower(c) ? toupper(c)
								  : c);
				else {
					putchar(c);
					if ((c = getchar()) != '(') {
						putchar(c);
						while((c = getchar()) != '\'')
							putchar(c);
						putchar(c);
					} else {
						putchar(c);
						while((c = getchar()) != '\'')
				 			putchar(islower(c)
								 ? toupper(c)
								 : c);
						putchar(c);
					}
				}
			}
			putchar(c);
		}
	}
	fclose(stdout);
	return EXIT_SUCCESS;
}

