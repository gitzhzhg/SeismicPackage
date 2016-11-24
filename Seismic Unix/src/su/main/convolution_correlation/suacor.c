/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUACOR: $Revision: 1.16 $ ; $Date: 2015/10/14 15:38:38 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUACOR - auto-correlation						",
"									",
" suacor <stdin >stdout [optional parms]				",
"									",
" Optional Parameters:							",
" ntout=101	odd number of time samples output			",
" norm=1	if non-zero, normalize maximum absolute output to 1	",
" sym=1		if non-zero, produce a symmetric output from		",
"			lag -(ntout-1)/2 to lag +(ntout-1)/2		",
NULL};

/* Credits:
 *	CWP: Dave Hale
 *
 * Trace header fields accessed:  ns
 * Trace header fields modified:  ns and delrt
 */
/**************** end self doc *******************************************/

segy tr;

int
main(int argc, char **argv)
{
	int nt;				/* number of samples on input */
	int ntout;			/* number of samples on output */
	int it;				/* counter */
	int istart;			/* beginning sample */
	int izero;			/* - istart */
	int norm;			/* user defined normalization value */
	int sym;			/* symmetric plot? */
	float scale;			/* scale factor computed from norm */
	float *temp=NULL;		/* temporary array */
	float dt;			/* time sampling interval (sec) */

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = tr.dt/1000000.0;

	/* get parameters */
	if (!getparint("ntout",&ntout)) ntout=101;
	if (!getparint("norm",&norm)) norm = 1;
	if (!getparint("sym",&sym)) sym = 1;
        checkpars();
	
	/* allocate workspace */
	temp = ealloc1float(ntout);
	
	/* index of first sample */
	if (sym == 0) istart = 0;
	else istart = -(ntout-1)/2;

	/* index of sample at time zero */
	izero = -istart;
	
	/* loop over traces */
	do {
		xcor(nt,0,tr.data,nt,0,tr.data,ntout,istart,temp);
		if (norm) {
			scale = 1.0/(temp[izero]==0.0?1.0:temp[izero]);
			for (it=0; it<ntout; ++it)  temp[it] *= scale;
		}
		memcpy((void *) tr.data, (const void *) temp, ntout*FSIZE);
		tr.ns = ntout;
		tr.f1 = -dt*ntout/2.0;
		tr.delrt = 0;
		puttr(&tr);
	} while(gettr(&tr));

	return(CWP_Exit());
}
