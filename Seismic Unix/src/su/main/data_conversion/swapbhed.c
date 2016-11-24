/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SWAPBHED: $Revision: 1.2 $ ; $Date: 2011/11/12 00:01:45 $	*/

#include <stdint.h>
#include "su.h"
#include "segy.h"
#include "bheader.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SWAPBHED - SWAP the BYTES in a SEGY Binary tape HEaDer file		",
" 									",
" swapbhed < binary_in > binary out					",
" 									",
" Required parameter:							",
" 	none								",
" Optional parameters:							",
"	none 								",
" 									",
NULL};

/* Credits:
 *
 *	CWP: John Stockwell  13 May 2011
 */
/**************** end self doc ***********************************/

segy tr;
bhed bh;

int
main(int argc, char **argv)
{
	int i;			/* counter				*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(0); /* stdin not used */

	/* Read in binary header from standard in  */
	efread(&bh,BNYBYTES, 1, stdin);

	/* swap bytes */
	for (i = 0; i < BHED_NKEYS; ++i) swapbhval(&bh, i);

	/* Write binary header from bh structure to standard out */
	efwrite( (char *) &bh, 1, BNYBYTES, stdout);

	return(CWP_Exit());
}
