/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* FLOAT2IBM: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $     */

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" FLOAT2IBM - convert native binary FLOATS to IBM tape FLOATS	",
" 								",
" float2ibm <stdin >stdout 					",
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
" This program is usable for writing SEG Y traces with the headers",
" stripped off.							",
NULL};

/* Credits:
 *	CWP: John Stockwell, based on code by Jack K. Cohen
 */
/**************** end self doc ***********************************/

#ifdef SUXDR            /* begin if SUXDR */
#if defined(_CRAYMPP)     /* begin if _CRAYMPP */
typedef short fourbyte;
#else                     /* else if SUXDR but not _CRAYMPP */
typedef int fourbyte;
#endif                    /* end if _CRAYMPP */
#else

typedef int fourbyte; 

#endif                  /* end if SUXDR */

/* subroutine prototypes */
#ifdef SUXDR            /* begin if  SUXDR */
static void float_to_ibm(fourbyte *from, fourbyte *to, int n, int endian);

#else                   /* if not SUXDR */
static void float_to_ibm(int from[], int to[], int n, int endian);

#endif                  /* end if SUXDR */



int
main(int argc, char **argv)
{
	char *outpar;		/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	float *x;		/* binary floats			*/
	float *z;		/* binary floats			*/
	int n1=0;		/* number of values read		*/ 

	int endian;		/* byte order 				*/

#ifdef SUXDR    /* begin if SUXDR */
#if defined(CRAY) /* begin if defined CRAY */
#if defined(_CRAYMPP)  /* begin if defined _CRAYMPP */
        fourbyte imone = -1;    /* constant for Fortran linkage         */
        fourbyte fns;           /* for Fortran CRAYMPP linkage          */
#else           /* CRAY but not _CRAYMPP */
        int ier;                /* CRAY ibmfloat error flag             */
        fourbyte ione = -1;     /* constant for Fortran linkage         */
#endif          /* end if _CRAYMPP */
#endif /* end if SUXDR and CRAY but not _CRAYMPP  */
#endif


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


#if defined(CRAY)
#if defined(_CRAYMPP)
                            float_to_ibm((fourbyte *) x,
                                         (fourbyte *) z,
                                         1, endian);
*/
#else /* !_CRAYMPP */
                            USSCTI(tr.data,tr.data,&ione,&ns,&ier);
#endif /* _CRAYMPP */
#else /* !CRAY */
                            float_to_ibm((fourbyte *) x,
                                         (fourbyte *) z,
                                         1, endian);
#endif /* !CRAY */

		/* write floats */
		fwrite(z,sizeof(float),1,stdout);

		++n1;
	}


	/* Make par file */
	fprintf(outparfp, "n=%d\n", n1);

	return(CWP_Exit());
}

#ifdef SUXDR    /* begin SUXDR */

/* Assumes fourbyte == 4 byte integer */
static void float_to_ibm(fourbyte *from, fourbyte *to, int n, int endian)
/**********************************************************************
 float_to_ibm - convert between 32 bit IBM and IEEE floating numbers
***********************************************************************
Input:
from       input vector
n          number of floats in vectors
endian     =0 for little endian machine, =1 for big endian machines

Output:
to         output vector, can be same as input vector

***********************************************************************
Notes:
Up to 3 bits lost on IEEE -> IBM

IBM -> IEEE may overflow or underflow, taken care of by
substituting large number or zero

Only integer shifting and masking are used.
***********************************************************************
Credits:     CWP: Brian Sumner
***********************************************************************/
{
    register fourbyte fconv, fmant, t;
    register int i;

    for (i=0;i<n;++i) {
        fconv = from[i];
        if (fconv) {
            fmant = (0x007fffff & fconv) | 0x00800000;
            t = (fourbyte) ((0x7f800000 & fconv) >> 23) - 126;
            while (t & 0x3) { ++t; fmant >>= 1; }
            fconv = (0x80000000 & fconv) | (((t>>2) + 64) << 24) | fmant;
        }
        if(endian==0)
                fconv = (fconv<<24) | ((fconv>>24)&0xff) |
                        ((fconv&0xff00)<<8) | ((fconv&0xff0000)>>8);

        to[i] = fconv;
    }
    return;
}


#else   /* not SUXDR */

#ifdef _HPUX_SOURCE
void float_to_ibm(int from[], int to[], int n, int endian)
{
    register int fconv, fmant, i, t, dummy;

        dummy = endian;

    for (i=0;i<n;++i) {
        fconv = from[i];
        if (fconv) {
            fmant = (0x007fffff & fconv) | 0x00800000;
            t = (int) ((0x7f800000 & fconv) >> 23) - 126;
            while (t & 0x3) { ++t; fmant >>= 1; }
            fconv = (0x80000000 & fconv) | (((t>>2) + 64) << 24) | fmant;
        }
        to[i] = fconv;
    }
    return;
}

#else

/* Assumes sizeof(int) == 4 */
static void float_to_ibm(int from[], int to[], int n, int endian)
/**********************************************************************
 float_to_ibm - convert between 32 bit IBM and IEEE floating numbers
***********************************************************************
Input:
from       input vector
n          number of floats in vectors
endian     =0 for little endian machine, =1 for big endian machines

Output:
to         output vector, can be same as input vector

***********************************************************************
Notes:
Up to 3 bits lost on IEEE -> IBM

IBM -> IEEE may overflow or underflow, taken care of by
substituting large number or zero

Only integer shifting and masking are used.
***********************************************************************
Credits:     CWP: Brian Sumner
***********************************************************************/
{
    register int fconv, fmant, i, t;

    for (i=0;i<n;++i) {
        fconv = from[i];
        if (fconv) {
            fmant = (0x007fffff & fconv) | 0x00800000;
            t = (int) ((0x7f800000 & fconv) >> 23) - 126;
            while (t & 0x3) { ++t; fmant >>= 1; }
            fconv = (0x80000000 & fconv) | (((t>>2) + 64) << 24) | fmant;
        }
        if(endian==0)
                fconv = (fconv<<24) | ((fconv>>24)&0xff) |
                        ((fconv&0xff00)<<8) | ((fconv&0xff0000)>>8);

        to[i] = fconv;
    }
    return;
}

#endif
#endif
