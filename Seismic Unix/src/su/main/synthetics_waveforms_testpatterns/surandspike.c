/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SURANDSPIKE: $Revision: 1.4 $ ; $Date: 2011/11/17 22:39:35 $	*/

#include "su.h"
#include "segy.h"
#include <time.h>

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" SURANDSPIKE - make a small data set of RANDom SPIKEs 		",
"								",
"   surandspike [optional parameters] > out_data_file  		",
"								",
" Creates a common offset su data file with random spikes	",
"								",
" Optional parameters:						",
"	n1=500 			number of time samples		",
"	n2=200			number of traces		",
" 	dt=0.002 		time sample rate in seconds	",
"	nspk=20			number of spikes per trace	",
"	amax=0.2		abs(max) spike value		",
"	mode=1			different spikes on each trace	",
"				=2 same spikes on each trace	",
" 	seed=from_clock    	random number seed (integer)    ",
"								",
NULL};

/* Credits:
 *	ARAMCO: Chris Liner
 *
 * Trace header fields set: ns, dt, offset
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int n1,n2;
	int nspk,ispk, it, itr;
	float dt;
	int mode;
	unsigned int seed;      /* random number seed */
	float amax, a, tmax, t;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(0); /* stdin not used */

	/*   get mode from user    */
	if( !getparint("mode",&mode) ) mode = 1 ;
	if( mode < 1  || mode > 2 ) err("mode must be 1 or 2" ) ;

	/* Set seed */
	if (!getparuint("seed", &seed)) { /* if not supplied, use clock */
		if (-1 == (seed = (unsigned int) time((time_t *) NULL))) {
			err("time() failed to set seed");
		}
	}
	sranuni(seed);

	/* other getpars */
	if( !getparint("n1",&n1) )       n1 = 500 ;
	if( !getparfloat("dt",&dt) )     dt = .002 ;
	if( !getparint("n2",&n2) )     	 n2 = 100 ;
	if( !getparint("nspk",&nspk) )   nspk = 20 ;
	if( !getparfloat("amax",&amax) ) amax = 0.2 ;
        checkpars();
	if ( amax < 0.0 ) amax = -amax;

	tmax = (n1-1)*dt;

	/* set output header words */
	tr.dt = dt*1000000.;
	tr.ns = n1;

	/* loop over traces */
	for (itr = 0; itr < n2; itr++) {

                if( mode == 1 || (mode == 2 && itr == 0 ) ){

			/* zero trace */
			memset( (void *) tr.data, 0, n1 * FSIZE);

			/* loop over spikes */
			for (ispk = 0; ispk < nspk; ispk++) {

				/* get spike time sample */
				t = franuni()*tmax;	
				it = t/dt + 1;

				/* get spike amplitude */
				a = 2 * ( 0.5-franuni() ) * amax;	

				/* put spike in trace (ignore collisions) */
				tr.data[it] = a;
			}
                }
		/* bump trace counter and write trace */
		tr.tracl = itr + 1;
		puttr(&tr);
	}


	return(CWP_Exit());
}
