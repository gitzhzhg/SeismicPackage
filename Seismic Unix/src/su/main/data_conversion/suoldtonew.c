/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUOLDTONEW: $Revision: 1.5 $ ; $Date: 2011/11/12 00:01:45 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUOLDTONEW - convert existing su data to xdr format		",
" 								",
" suoldtonew <oldsu >newsu  					",
"								",
" Required parameters:						",
"	none							",
" 								",
" Optional parameters:						",
"	none							",
"								",
" Notes:							",
" This program is used to convert native machine datasets to	",
" xdr-based, system-independent format.				",
"								",
NULL};

/*
 * Author: Stewart A. Levin, Mobil, 1966
 *  
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	while (!(feof(stdin) || ferror(stdin))) {
		static int ntr=0; /* for user info only */

		/* Do read of header for the segy */
		if (0 >= efread(&tr, HDRBYTES, 1, stdin)) {
			warn("converted %d traces to XDR format", ntr);
			break; /* loop exit */
		}

		/* Do read of data for the segy */
		switch(efread((char *) (&(tr.data[0])), FSIZE, tr.ns, stdin)) {
		case 0: /* oops, no data for this header */
			err("header without data for trace #%d", ntr+1);
		default:
			puttr(&tr);
			++ntr;
		}
	}


	return(CWP_Exit());
}
