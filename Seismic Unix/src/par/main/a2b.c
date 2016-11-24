/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* A2B: $Revision: 1.16 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" A2B - convert ascii floats to binary 				",
" 								",
" a2b <stdin >stdout outpar=/dev/null 				",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	n1=2		floats per line in input file		",
" 								",
" 	outpar=/dev/null output parameter file, contains the	",
"			number of lines (n=)			",
" 			other choices for outpar are: /dev/tty,	",
" 			/dev/stderr, or a name of a disk file	",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen, Dave Hale
 *	Hans Ecke 2002: Replaced line-wise file reading via gets() with 
 *			float-wise reading via fscanf(). This makes it 
 *			much more robust: it does not impose a specific 
 *			structure on the input file.
 */

/**************** end self doc ***********************************/

#define N1_DEFAULT 2
#define OUTPAR_DEFAULT "/dev/null"

int
main(int argc, char **argv)
{
	char *outpar;		  /* name of file holding output parfile   */
	FILE *outparfp;		  /* ... its file pointer		   */
	int n1;			  /* number of floats per line	   	   */
	float *x;		  /* binary floats			   */
	int ret;		  /* fscanf() return value		 */
	unsigned long int vnum;   /* how many floats we have filled in the */
				  /*  current vector already */
	unsigned long int vec;    /* number of vector we are reading	*/
 

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	switch(filestat(STDOUT)) { /* Prevent floats from dumping on screen */
	case BADFILETYPE:
		warn("stdout is illegal filetype");
		pagedoc();
	break;
	case TTY:
		warn("stdout can't be tty");
		pagedoc();
	break; 
	default:			   /* rest are OK */
	break;

	}


	/* Get parameters and do set up */
	if (!getparstring("outpar", &outpar))  outpar = OUTPAR_DEFAULT;
	outparfp = efopen(outpar, "w");

	if (!getparint("n1", &n1))		n1 = N1_DEFAULT;
	x = ealloc1float(n1);

	checkpars();


	vnum=0;		/* offset inside vector, in range 0->n1 */
	vec =0;		/* vector number, ends up as n2	 */
	while(1) {	
	   ret=fscanf(stdin," %f ",x+vnum);
	/*  fscanf returns: 0   : characters there, but no conversion (error)
	*		  EOF : eof before conversion
	*		  else: number of conversions 
	*/
	   if(ret == EOF) {
		if(vnum != 0) 
		err("vector #%lu, float #%lu: we encountered an EOF but the vector has not been read in completely",vec+1,vnum+1);
		break;  /* else everything is okay: get out of the loop */
		} 
	   if(ret == 0)	
		err("could not read float in vector #%lu, float #%lu",vec+1,vnum+1);
	   vnum++;
	   if(vnum == n1) {
		vnum=0;
		fwrite(x, FSIZE, n1, stdout);
		vec++;
		}
	   }
	
	fprintf(outparfp, "n2=%lu\n", vec);/* Make par file */

	return EXIT_SUCCESS;
}
