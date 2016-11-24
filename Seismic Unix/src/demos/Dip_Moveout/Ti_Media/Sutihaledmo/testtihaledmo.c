/* TESTTIHALEDMO: $Revision: 1.2 $ ; $Date: 95/05/03 12:59:34 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" TESTTIHALEDMO - make a small spike data set for testing SUTIHALEDMO	",
"								",
" testtihaledmo [optional parameters] > out_data_file  		",
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
"	ix1=ntr/4	trace number (from left) for spike #1	",
"	it1=nt/4 	time sample to spike #1			",
"	ix2=ntr/4	trace for spike #2			",
"	it2=3*nt/4 	time for spike #2			",
"	ix3=3*ntr/4;	trace for spike #3			",
"	it3=nt/4;	time for spike #3			",
"	ix4=3*ntr/4;	trace for spike #4			",
"	it4=3*nt/4;	time for spike #4			",
"								",
NULL};
/**************** end self doc ***********************************/

/* Credits:
 *	CWP:  Shuki Ronen, Chris Liner
 *	adapted as a test for ti-haledmo by John E. Anderson.
 */


segy tr;

int
main(int argc, char **argv)
{
	int nt,ntr,itr;
	int nspk,ix1,it1,ix2,it2;
	int ix3,it3,ix4,it4;
	float dt,offset;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(0); /* stdin not used */


	nt = 250;	getparint("nt", &nt);		tr.ns = nt;
	ntr = 33;	getparint("ntr", &ntr);
	dt = 0.001;	getparfloat("dt", &dt);		tr.dt = dt*1000000;
	offset = 400;	getparfloat("offset", &offset);	tr.offset = offset;
	nspk = 2;	getparint("nspk", &nspk);
	ix1 = 17;	getparint("ix1", &ix1); 
	it1 = 100;	getparint("it1", &it1);
	ix2 = 17;	getparint("ix2", &ix2);
	it2 = 200;	getparint("it2", &it2);
	ix3 = 3*ntr/4;	getparint("ix3", &ix3);
	it3 = nt/4;	getparint("it3", &it3);
	ix4 = 3*ntr/4;	getparint("ix4", &ix4);
	it4 = 3*nt/4;	getparint("it4", &it4);

	for (itr = 0; itr < ntr; itr++) {
		memset((void *) tr.data, (int) '\0', nt * FSIZE);
		if (itr == ix1-1) tr.data[it1-1] = 1.0;  
		if (nspk > 1 && itr == ix2-1) tr.data[it2-1] = 1.0;
		if (nspk > 2 && itr == ix3-1) tr.data[it3-1] = 1.0;
		if (nspk > 3 && itr == ix4-1) tr.data[it4-1] = 1.0;
		tr.tracl = itr + 1;
		tr.cdp = tr.tracl;
		tr.f1=0;
		tr.d1=dt;
		tr.f2=-.4;
		tr.d2=.025;
		puttr(&tr);
	}


	return EXIT_SUCCESS;
}
