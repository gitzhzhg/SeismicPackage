/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* IBM2FLOAT: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $     */

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" IBM2FLOAT - convert IBM tape FLOATS to native binary FLOATS	",
" 								",
" ibm2float <stdin >stdout 					",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" endian=	byte order of your system (autodetected)	",
" outpar=/dev/tty output parameter file, contains the		",
"			number of values (n=)			",
"		       other choices for outpar are: /dev/tty, ",
"		       /dev/stderr, or a name of a disk file   ",
" 								",
" Notes:							",
" endian=1 (big endian) endian=0 (little endian) byte order 	",
" You probably will not have to set this, as the byte order of  ",
" your system is autodetected by the program. 			",
" This program is usable for reading SEG Y files with the headers",
" stripped off.							",
NULL};

/* Credits:
 *	CWP: John Stockwell, based on code by Jack K. Cohen
 */
/**************** end self doc ***********************************/

/* Subroutine prototypes */
static void ibm_to_float(int from[], int to[], int n, int endian);

int
main(int argc, char **argv)
{
	char *outpar;		/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	float *x;		/* binary floats			*/
	float *z;		/* binary floats			*/
	int n1=0;		/* number of values read		*/ 

	int endian;		/* byte order 				*/


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
	if (!getparint("endian", &endian))      {
	      union { short s; char c[2]; } testend;
		testend.s = 1;
		endian = (testend.c[0] == '\0') ? 1 : 0;
	}
	if (!getparstring("outpar", &outpar))	outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");

        checkpars();

	x = ealloc1float(1);
	z = ealloc1float(1);


	/* Loop over data converting to ascii */
	while ((efread(x, FSIZE, 1, stdin))) {

		ibm_to_float((int *) x  , (int *) z, 1, endian);
		/* write floats */
		fwrite(z,sizeof(float),1,stdout);

		++n1;
	}


	/* Make par file */
	fprintf(outparfp, "n=%d\n", n1);

	return(CWP_Exit());
}


#ifdef _HPUX_SOURCE
static void ibm_to_float(int from[], int to[], int n, int endian)
{
	/* HP version of ibm_to_float */

    register int fconv, fmant, i, t, dummy;

	dummy = endian;

    for (i = 0; i < n; ++i) {
	fconv = from[i];

	/* next lines modified (M.J.Rutty 20/9/92) */
	/* if (fconv) { */
	/* fmant = 0x00ffffff & fconv; */

	fmant = 0x00ffffff & fconv;
	if (!fmant)
	  fconv = 0;
	else {
	  /* end modifications */
	    t = (int) ((0x7f000000 & fconv) >> 22) - 130;
	    while (!(fmant & 0x00800000)) { --t; fmant <<= 1; }
	    if (t > 254) fconv = (0x80000000 & fconv) | 0x7f7fffff;
	    else if (t <= 0) fconv = 0;
	    else fconv = (0x80000000 & fconv) | (t << 23)
			 | (0x007fffff & fmant);
	}
	to[i] = fconv;
    }
    return;
}

#else /* use the regular ibm_to_float routine */

static void ibm_to_float(int from[], int to[], int n, int endian)
/***********************************************************************
ibm_to_float - convert between 32 bit IBM and IEEE floating numbers
************************************************************************
Input::
from		input vector
to		output vector, can be same as input vector
endian		byte order =0 little endian (DEC, PC's)
			    =1 other systems
*************************************************************************
Notes:
Up to 3 bits lost on IEEE -> IBM

Assumes sizeof(int) == 4

IBM -> IEEE may overflow or underflow, taken care of by
substituting large number or zero

Only integer shifting and masking are used.
*************************************************************************
Credits: CWP: Brian Sumner,  c.1985
*************************************************************************/
{
    register int fconv, fmant, i, t;

    for (i = 0;i < n; ++i) {

	fconv = from[i];

	/* if little endian, i.e. endian=0 do this */
	if (endian == 0) fconv = (fconv << 24) | ((fconv >> 24) & 0xff) |
		((fconv & 0xff00) << 8) | ((fconv & 0xff0000) >> 8);

	if (fconv) {
	    fmant = 0x00ffffff & fconv;
	    /* The next two lines were added by Toralf Foerster */
	    /* to trap non-IBM format data i.e. conv=0 data  */
	    if (fmant == 0)
		warn("mantissa is zero data may not be in IBM FLOAT Format !");
	    t = (int) ((0x7f000000 & fconv) >> 22) - 130;
	    while (!(fmant & 0x00800000)) { --t; fmant <<= 1; }
	    if (t > 254) fconv = (0x80000000 & fconv) | 0x7f7fffff;
	    else if (t <= 0) fconv = 0;
	    else fconv =   (0x80000000 & fconv) | (t << 23)
			 | (0x007fffff & fmant);
	}
	to[i] = fconv;
    }
    return;
}
#endif
