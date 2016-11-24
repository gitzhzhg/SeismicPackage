/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* B2A: $Revision: 1.16 $ ; $Date: 2012/10/09 18:33:44 $	*/
#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" B2A - convert binary floats to ascii				",
" 								",
" b2a <stdin >stdout 						",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	n1=2		floats per line in output file 		",
"       format=0	scientific notation	 		",
" 			=1 long decimal float form		",
" 								",
" 	outpar=/dev/tty output parameter file, contains the	",
"			number of lines (n=)			",
"                       other choices for outpar are: /dev/tty, ",
"                       /dev/stderr, or a name of a disk file   ",
" 								",
" Note: 							",
" Parameter:							", 
"  format=0 uses printf(\"%15.10e \", x[i1])			",
"  format=1 uses printf(\"%15.15f \", x[i1])			",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen
 */
/**************** end self doc ***********************************/


int
main(int argc, char **argv)
{
	char *outpar=NULL;	/* name of file holding output parfile	*/
	FILE *outparfp=NULL;	/* ... its file pointer			*/
	int n1;			/* number of floats per line		*/
	size_t n1read;		/* number of items read			*/
	int n2 = 0;		/* number of lines in input file 	*/
	float *x=NULL;		/* binary floats			*/
	int format=0;		/* notation =0 scientific =1 long decimal */


	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters and do set up */
	if (!getparstring("outpar", &outpar))	outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");
	if (!getparint("n1", &n1))		n1 = 2;
	if (!getparint("format", &format))		format = 0;
	x = ealloc1float(n1);


        checkpars();

	/* Loop over data converting to ascii */
	while ((n1read = efread(x, FSIZE, n1, stdin))) {
		register int i1;

		if (n1read != n1)
			err("out of data in forming line #%d", n2+1);
		if (!format){
			for (i1 = 0; i1 < n1; ++i1)  printf("%15.10e ", x[i1]);
		} else {
			for (i1 = 0; i1 < n1; ++i1)  printf("%20.20f ", x[i1]);
		}
			
		putchar('\n');
		++n2;
	}


	/* Make par file */
	fprintf(outparfp, "n=%d\n", n2);

	return(CWP_Exit());
}
