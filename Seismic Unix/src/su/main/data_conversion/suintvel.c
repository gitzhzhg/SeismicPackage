/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUINTVEL: $Revision: 1.14 $ ; $Date: 2011/11/16 17:43:20 $		*/

#include "su.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUINTVEL - convert stacking velocity model to interval velocity model	",
"									",
" suintvel vs= t0= outpar=/dev/tty					",
"									",
" Required parameters:					        	",
"	vs=	stacking velocities 					",
"	t0=	normal incidence times		 			",
"									",
" Optional parameters:							",
"	mode=0			output h= v= ; =1 output v=  t= 	",
"	outpar=/dev/tty		output parameter file in the form:	",
"				h=layer thicknesses vector		",
"				v=interval velocities vector		",
"				....or ...				",
"				t=vector of times from t0		",
"				v=interval velocities vector		",
"									",
" Examples:								",
"    suintvel vs=5000,5523,6339,7264 t0=.4,.8,1.125,1.425 outpar=intpar	",
"									",
"    suintvel par=stkpar outpar=intpar					",
"									",
" If the file, stkpar, contains:					",
"    vs=5000,5523,6339,7264						",
"    t0=.4,.8,1.125,1.425						",
" then the two examples are equivalent.					",
"									",
" Note: suintvel does not have standard su syntax since it does not	",
"      operate on seismic data.  Hence stdin and stdout are not used.	",
"									",
" Note: may go away in favor of par program, velconv, by Dave		",
"									",
NULL};

/* Credits:
 *	CWP: Jack 
 *
 * Technical Reference:
 *	The Common Depth Point Stack
 *	William A. Schneider
 *	Proc. IEEE, v. 72, n. 10, p. 1238-1254
 *	1984
 *
 * Formulas:
 *    	Note: All sums on i are from 1 to k
 *
 *	From Schneider:
 *	Let h[i] be the ith layer thickness measured at the cmp and
 *	v[i] the ith interval velocity.
 *	Set:
 *		t[i] = h[i]/v[i]
 *	Define:
 *		t0by2[k] = 0.5 * t0[k] = Sum h[i]/v[i]
 *		vh[k] = vs[k]*vs[k]*t0by2[k] = Sum v[i]*h[i]
 *	Then:
 *		dt[i] = h[i]/v[i] = t0by2[i] - t0by2[i-1]
 *		dvh[i] = h[i]*v[i] = vh[i] - vh[i-1]
 *		h[i] = sqrt(dvh[i] * dt[i])
 *		v[i] = sqrt(dvh[i] / dt[i])
 *
 *
 */
/**************** end self doc *******************************************/

int
main(int argc, char **argv)
{
	register float *v=NULL;		/* interval velocities		*/
	register float *h=NULL;		/* layer thicknesses at the cmp	*/
	register float *vs=NULL;	/* stacking velocities		*/
	register float *t0=NULL;	/* zero incidence times		*/
	register int i;		/* counter				*/
	int n;			/* number of layers			*/
	float t1, t2;		/* temporaries for one-way times	*/
	float v1, v2;		/* temporaries for stacking v's		*/
	float dt;		/* temporary for t0/2 difference	*/
	float dvh;		/* temporary for v*h difference		*/
	cwp_String outpar;	/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	int mode;		/* mode=0  h= v= ; mode=1 t= v= 	*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(0);


	outpar = "/dev/tty" ;	getparstring("outpar", &outpar);
	outparfp = efopen(outpar, "w");


	/* Allocate space for the model */
	if ((n = countparval("t0"))) {
		v  = ealloc1float(n);
		h  = ealloc1float(n);
		vs = ealloc1float(n);
		t0 = ealloc1float(n);
	} else err("no t0's specified");

	/* Get the normal incidence times and stacking velocities */
	if (n != getparfloat("t0", t0))
		err("expected %d intervals", n);
	if (n != getparfloat("vs", vs))
		err("expected %d velocities", n);
	if (!getparint("mode", &mode))		mode = 0;
        checkpars();

	/* Check that vs's and t0's are positive */
	for (i = 0; i < n; i++) {
		if (vs[i] <= 0.0)
			err("vs's must be positive: vs[%d] = %f", i, vs[i]);
		if (t0[i] < 0.0)
			err("t0's must be positive: t0[%d] = %f", i, t0[i]);
	}

	/* Compute h(i), v(i) */
	h[0] = 0.5 * vs[0] * t0[0];
	v[0] = vs[0];
	for (i = 1; i < n; i++) {
		t2 = 0.5 * t0[i]; t1 = 0.5 * t0[i-1];
		v2 = vs[i]; v1 = vs[i-1];
		dt = t2 - t1;
		dvh = v2*v2*t2 - v1*v1*t1;
		h[i] = sqrt(dvh * dt);
		v[i] = sqrt(dvh / dt);
	}

	/* Make par file */
	if (!mode) {
		fprintf(outparfp, "h=");
		for (i = 0; i < n - 1; i++) {
			fprintf(outparfp, "%g,", h[i]);
		}
		fprintf(outparfp, "%g\n", h[n-1]);
	} else {
		fprintf(outparfp, "t=");
		for (i = 0; i < n - 1; i++) {
			fprintf(outparfp, "%g,", t0[i]);
		}
		fprintf(outparfp, "%g\n", t0[n-1]);
	}
	fprintf(outparfp, "v=");
	for (i = 0; i < n - 1; i++) {
		fprintf(outparfp, "%g,", v[i]);
	}
	fprintf(outparfp, "%g\n", v[n-1]);


	return(CWP_Exit());
}
