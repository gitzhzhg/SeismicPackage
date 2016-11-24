/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSPIKE: $Revision: 1.15 $ ; $Date: 2011/11/12 00:40:42 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" SUSPIKE - make a small spike data set 			",
"								",
" suspike [optional parameters] > out_data_file  		",
"								",
" Creates a common offset su data file with up to four spikes	",
" for impulse response studies					",
"								",
" Optional parameters:						",
"	nt=64 		number of time samples			",
"	ntr=32		number of traces			",
" 	dt=0.004 	time sample rate in seconds		",
" 	offset=400 	offset					",
"	nspk=4		number of spikes			",
"	ix1= ntr/4	trace number (from left) for spike #1	",
"	it1= nt/4 	time sample to spike #1			",
"	ix2 = ntr/4	trace for spike #2			",
"	it2 = 3*nt/4 	time for spike #2			",
"	ix3 = 3*ntr/4;	trace for spike #3			",
"	it3 = nt/4;	time for spike #3			",
"	ix4 = 3*ntr/4;	trace for spike #4			",
"	it4 = 3*nt/4;	time for spike #4			",
"								",
NULL};

/* Credits:
 *	CWP: Shuki Ronen, Chris Liner
 *
 * Trace header fields set: ns, dt, offset
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int nt;				/* number of time samples	*/
	int ntr;			/* number of traces		*/
	int itr;			/* trace counter		*/
	int nspk;			/* number of spikes		*/
	int it1;			/* time of 1st spike		*/
	int ix1;			/* position of 1st spike	*/
	int it2;			/* time of 2nd spike		*/
	int ix2;			/* position of 2nd spike	*/
	int ix3;			/* position of 3rd spike	*/
	int it3;			/* time of 3rd spike		*/
	int ix4;			/* position of 4th spike	*/
	int it4;			/* time of 4th spike		*/
	float dt;			/* time sampling interval	*/
	float offset;			/* offset			*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(0); /* stdin not used */


	nt = 64;	getparint("nt", &nt);
	CHECK_NT("nt",nt);				tr.ns = nt;
	ntr = 32;	getparint("ntr", &ntr);
	dt = 0.004;	getparfloat("dt", &dt);		tr.dt = dt*1000000;
	offset = 400;	getparfloat("offset", &offset);	tr.offset = offset;
	nspk = 4;	getparint("nspk", &nspk);
	ix1 = ntr/4;	getparint("ix1", &ix1); 
	it1 = nt/4;	getparint("it1", &it1);
	ix2 = ntr/4;	getparint("ix2", &ix2);
	it2 = 3*nt/4;	getparint("it2", &it2);
	ix3 = 3*ntr/4;	getparint("ix3", &ix3);
	it3 = nt/4;	getparint("it3", &it3);
	ix4 = 3*ntr/4;	getparint("ix4", &ix4);
	it4 = 3*nt/4;	getparint("it4", &it4);

	for (itr = 0; itr < ntr; itr++) {
		memset( (void *) tr.data, 0, nt * FSIZE);
		if (itr == ix1-1) tr.data[it1-1] = 1.0;  
		if (nspk > 1 && itr == ix2-1) tr.data[it2-1] = 1.0;
		if (nspk > 2 && itr == ix3-1) tr.data[it3-1] = 1.0;
		if (nspk > 3 && itr == ix4-1) tr.data[it4-1] = 1.0;
		tr.tracl = itr + 1;
		puttr(&tr);
	}


	return(CWP_Exit());
}
