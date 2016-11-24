/* TESTTIFOWLER: $Revision: 1.2 $ ; $Date: 95/05/03 12:57:25 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" TESTTIFOWLER - make a small spike data set 			",
"								",
" testtifowler [optional parameters] > out_data_file  		",
"								",
" Creates a common offset su data file with up to four spikes	",
" for impulse response studies for sutifowler			",
"								",
" Optional parameters:						",
"	nt=250 		number of time samples			",
"	ntr=33		number of traces			",
" 	dt=0.001 	time sample rate in seconds		",
" 	offset=400 	offset					",
"       v=10000         velocity                                ",
"	nspk=2		number of spikes			",
"	ix1= 17		trace number (from left) for spike #1	",
"	t1= 0.100 	time s spike #1			",
"	ix2 = 17	trace for spike #2			",
"	t2 = 0.200 	time s for spike #2			",
"	ix3 = 17;	trace for spike #3			",
"	t3 = 0.100;	time for spike #3			",
"	ix4 = 17;	trace for spike #4			",
"	t4 = 0.200;	time s for spike #4			",
"								",
NULL};

/* Credits:
 *	CWP: Shuki Ronen, Chris Liner
 *      modified from SUSPIKE by John Anderson, April, 1994,
 *      to have appropriate trace header words and default 
 *      values for SUTIFOWLER tests
 */
/**************** end self doc ***********************************/


segy tr;

main(int argc, char **argv)
{
	int nt,ntr,itr;
	int nspk,ix1,it1,ix2,it2;
	int ix3,it3,ix4,it4;
	float dt,offset,v,t1,t2,t3,t4;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(0); /* stdin not used */


	nt = 250;	getparint("nt", &nt);	
	ntr = 33;	getparint("ntr", &ntr);
	dt = 0.001;	getparfloat("dt", &dt);		
	offset = 400;	getparfloat("offset", &offset);
	v=10000;	getparfloat("v", &v);	
	nspk = 2;	getparint("nspk", &nspk);

	ix1 = 17;	getparint("ix1", &ix1); 
	t1 = 0.100;	getparfloat("t1", &t1);
	ix2 = 17;	getparint("ix2", &ix2);
	t2 = 0.200;	getparfloat("t2", &t2);
	ix3 = 17;	getparint("ix3", &ix3);
	t3 = 0.100;	getparfloat("t3", &t3);
	ix4 = 17;	getparint("ix4", &ix4);
	t4 = 0.200;	getparfloat("t4", &t4);


	
	t1=sqrt(t1*t1 + (offset*offset)/(v*v) );
	it1=t1/dt;
	t2=sqrt(t2*t2 + (offset*offset)/(v*v) );
	it2=t2/dt;
	t3=sqrt(t3*t3 + (offset*offset)/(v*v) );
	it3=t3/dt;
	t4=sqrt(t4*t4 + (offset*offset)/(v*v) );
	it4=t4/dt;
	fprintf(stderr," it1,it2,it3,it4=%d %d %d %d \n",it1,it2,it3,it4);
	for (itr = 0; itr < ntr; itr++) {
		memset((void *) tr.data, (int) '\0', nt * FSIZE);

		if (itr == ix1-1 && it1<nt) tr.data[it1] = 1.0;  
		if (nspk > 1 && itr == ix2-1 && it2<nt) tr.data[it2] = 1.0;
		if (nspk > 2 && itr == ix3-1 && it3<nt) tr.data[it3] = 1.0;
		if (nspk > 3 && itr == ix4-1 && it4<nt) tr.data[it4] = 1.0;

		tr.tracl = itr + 1;
		tr.cdp = tr.tracl;
		tr.offset = offset;
		tr.ns = nt;
		tr.dt = dt*1000000;
		tr.offset = offset;
		tr.f1=0;
		tr.d1=dt;
		tr.f2=-.4;
		tr.d2=.025;

		puttr(&tr);
	}


	return EXIT_SUCCESS;
}
