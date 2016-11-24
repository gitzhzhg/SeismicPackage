/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* B2A: $Revision: 1.7 $ ; $Date: 2011/11/16 16:42:16 $	*/
#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" FTNUNSTRIP - convert C binary floats to Fortran style floats	",
" 								",
" ftnunstrip <stdin >stdout 					",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	n1=1		floats per line in output file 		",
" 								",
" 	outpar=/dev/tty output parameter file, contains the	",
"			number of lines (n=)			",
" 			other choices for outpar are: /dev/tty,	",
" 			/dev/stderr, or a name of a disk file	",
" 								",
" Notes: This program assumes that the record length is constant",
" throughout the input and output files. 			",
" In fortran code reading these floats, the following implied	",
" do loop syntax would be used: 				",
"        DO i=1,n2						",
"                 READ (10) (someARRAY(j), j=1,n1) 		",
"        END DO							",
" Here n1 is the number of samples per record, n2 is the number ",
" of records, 10 is some default file (fort.10, for example), and",
" someArray(j) is an array dimensioned to size n1		",
" 								",
NULL};

/* Credits:
 *	CWP: John Stockwell, Feb 1998,
 *            based on ftnstrip by: Jack K. Cohen
 */
/**************** end self doc ***********************************/


int
main(int argc, char **argv)
{
	char *outpar;		/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	int n1;			/* number of floats per line		*/
	size_t n1read;		/* number of items read			*/
	int n2 = 0;		/* number of lines in input file 	*/
	float *x;		/* binary floats			*/
	int n1bytes;		/* size in bytes of record length	*/
	

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters and do set up */
	if (!getparstring("outpar", &outpar))	outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");
	if (!getparint("n1", &n1))		n1 = 1;
	n1bytes = FSIZE*n1;
	x = ealloc1float(n1);

        checkpars();

	/* Loop over data prepending and appending BOR and EOR */
	while ((n1read = efread(x, FSIZE, n1, stdin))) {

		if (n1read != n1)
			err("out of data in forming line #%d", n2+1);

		/* write out the record length integer beginning of record */
		efwrite(&n1bytes,ISIZE,1,stdout);

		/* write out the floats */
		efwrite(x, n1bytes, 1, stdout);

		/* write out the record length integer end of record */
		efwrite(&n1bytes,ISIZE,1,stdout);

		++n2;
	}

	/* Make par file */
	fprintf(outparfp, "n=%d\n", n2);


	return(CWP_Exit());
}
