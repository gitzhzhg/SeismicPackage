/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* XY2Z: $Revision: 1.8 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"	 								",
" XY2Z - converts (X,Y)-pairs to spike Z values on a uniform grid	",
" 	      								",
"    xy2z < stdin npairs= [optional parameters] >stdout 		",
" 	      								",
" Required parameter:							",
" npairs= 	number of pairs input					",
" 	      								",
" Optional parameter:							",
" scale=1.0	value to scale spikes by				",
" nx1=100 	dimension of first (fast) dimension of output array	",
" nx2=100 	dimension of second (slow) dimension of output array	",
" x1pad=2	zero padding in x1 dimension				",
" x2pad=2	zero padding in x2 dimension				",
" yx=0		assume (x,y) pairs 					",
" 		=1	assume (y,x) pairs 				",
" 									",
" Notes: 								",
" Converts ordered (x,y) pairs to spike x1values, of height=scale on a 	",
" uniform grid.								",
" 									",
NULL};

/* Credits:
 *	CWP: John Stockwell, Nov 1995
 */
/**************** end self doc ***********************************/


int
main(int argc, char **argv)
{
	int npairs;		/* number of pairs		*/
	int i;			/* counter			*/
	float *x1;		/* x1-values			*/
	float *x2;		/* x2-values			*/
	float z[2];		/* input ordered pair		*/
	float **outarray;	/* output array			*/
	float x1min;		/* min x1 value			*/
	float x1max;		/* max x1 value			*/
	float x2min;		/* min x2 value			*/
	float x2max;		/* max x2 value			*/
	float scale;		/* scaling parameter		*/
	int nx1;		/* x1 dimension of output array	*/
	int nx2;		/* x2 dimension of output array	*/
	int x1pad;		/* amount of x1 zero padding	*/
	int x2pad;		/* amount of x2 zero padding	*/
	int yx;			/* (x,y) versus (y,x)		*/

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* Get parameters and do set up */
	MUSTGETPARINT("npairs", &npairs);
	if (!getparfloat("scale", &scale))	scale=1.0;
	if (!getparint("nx1", &nx1))		nx1=100;
	if (!getparint("nx2", &nx2))		nx2=100;
	if (!getparint("x1pad", &x1pad))	x1pad=2;
	if (!getparint("x2pad", &x2pad))	x2pad=2;
	if (!getparint("yx", &yx))		yx=0;

        checkpars();

	/* Check values */
	if (x1pad>=nx1)
		err("x1pad=%d is too large!",x1pad);
	if (x2pad>=nx2)
		err("x2pad=%d is too large!",x2pad);

	/* Allocate space */
	x1 = ealloc1float(npairs);
	x2 = ealloc1float(npairs);
	outarray = ealloc2float(nx1,nx2);

	/* Zero outarray */
	memset( (void *) outarray[0], 0, nx1*nx2*FSIZE);

	/* Read in data */
	for (i=0; i<npairs; ++i) {
		efread(z,FSIZE,2,stdin);

		if (yx) {
			x1[i]=z[0];
			x2[i]=z[1];

		} else {
			x2[i]=z[0];
			x1[i]=z[1];
		}
	}

	/* Find min and max of x1 and x2 */
	x1min=x1[0]; x1max=x1[0];
	x2min=x2[0]; x2max=x2[0];
	for (i=0; i<npairs; ++i) {

		x1min = MIN(x1min,x1[i]);
		x1max = MAX(x1max,x1[i]);
		x2min = MIN(x2min,x2[i]);
		x2max = MAX(x2max,x2[i]);
	}
	
	/* Rescale data to go from (0,0) to (x1max,x2max) */
	for (i=0; i<npairs; ++i) {

		x1[i] = (nx1 - 2*x1pad)*(x1[i] - x1min)/(x1max - x1min) + x1pad;
		x2[i] = (nx2 - 2*x2pad)*(x2[i] - x2min)/(x2max - x2min) + x2pad;
	}


	/* Loop over output values */
	for (i=0; i<npairs; ++i) {
		outarray[NINT(x2[i])][NINT(x1[i])] = scale;
	}
		
	/* Write out array */
	efwrite(outarray[0],FSIZE,nx2*nx1,stdout);


	return(CWP_Exit());
}
