/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* FTNSTRIP: $Revision: 1.6 $ ; $Date: 2011/10/20 21:08:50 $		*/

#include "par.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" FTNSTRIP - convert a file of binary data plus record delimiters created",
"      via Fortran to a file containing only binary values (as created via C)",
" 									",
" ftnstrip <ftn_data >c_data 						",
" 									",
" Caveat: this code assumes the conventional Fortran format of header	",
"         and trailer integer containing the number of byte in the	",
"         record.  This is overwhelmingly common, but not universal.	",
" 									",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen
 */
/**************** end self doc *******************************************/


int
main(int argc, char **argv)
{
	int n1bytes;
	char *buf;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	while (efread(&n1bytes, ISIZE, 1, stdin)) {
		buf = ealloc1(n1bytes, 1);
		efread(buf, n1bytes, 1, stdin);
		efwrite(buf, n1bytes, 1, stdout);
		free1(buf);
		efread(&n1bytes, ISIZE, 1, stdin);
	}

	return(CWP_Exit());
}
