/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Z2XYZ: $Revision: 1.11 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" Z2XYZ - convert binary floats representing Z-values to ascii	",
" 	   form in X Y Z ordered triples			",
" 								",
"    z2xyz <stdin >stdout 					",
" 								",
" Required parameters:						",
" n1=		number of floats in 1st (fast) dimension	",
" 								",
" Optional parameters:						",
" 								",
" outpar=/dev/tty		 output parameter file		",
" 								",
" Notes: This program is useful for converting panels of float	",
" data (representing evenly spaced z values) to the x y z	",
" ordered triples required for certain 3D plotting packages.	",
" 								",
" Example of NXplot3d usage on a NeXT:				",
" suplane | sufilter | z2xyz n1=64 > junk.ascii			",
" 								",
" Now open junk.ascii as a mesh data file with NXplot3d.	",
" (NXplot3d is a NeXTStep-only utility for viewing 3d data sets	",
" 								",
NULL};

/* Credits:
 *	CWP: John Stockwell based on "b2a" by Jack Cohen
 */

/**************** end self doc ***********************************/


int
main(int argc, char **argv)
{
	char *outpar;		/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	int n1;			/* number of floats per line		*/
	size_t n1read;		/* number of items read			*/
	size_t n2 = 0;		/* number of lines in input file 	*/
	float *z;		/* binary floats			*/

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* Get parameters and do set up */
	if (!getparstring("outpar", &outpar))	outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");
	MUSTGETPARINT("n1",&n1);

        checkpars();

	z = ealloc1float(n1);

	/* Loop over data converting to ascii */
	while ((n1read = efread(z, FSIZE, n1, stdin))) {
		register int i1;

		if (n1read != n1)
			err("out of data in forming line #%d", n2+1);
		for (i1 = 0; i1 < n1; ++i1)
/* z2xyz.c:70: warning: format ‘%d’ expects type ‘int’, but argument 2 has type ‘size_t’ */
/* 			printf("%d %d %11.4e \n",n2,i1,z[i1]); */
#if __WORDSIZE == 64
			printf("%lu %d %11.4e \n",n2,i1,z[i1]);
#else
			printf("%u %d %11.4e \n",n2,i1,z[i1]);
#endif
		++n2;
	}


	/* Make par file */
/* z2xyz.c:76: warning: format ‘%d’ expects type ‘int’, but argument 3 has type ‘size_t’ */
/* 	fprintf(outparfp, "n2=%d\n", n2); */
#if __WORDSIZE == 64
	fprintf(outparfp, "n2=%lu\n", n2);
#else
	fprintf(outparfp, "n2=%u\n", n2);
#endif

	return(CWP_Exit());
}
