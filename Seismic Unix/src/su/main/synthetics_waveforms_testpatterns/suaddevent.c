/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.		       */

/* SUADDEVENT: $Revision: 1.6 $ ; $Date: 2011/11/16 23:30:27 $	       */

#define SQ(x) ((x))*((x))
#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/

char *sdoc[] = {
"								       ",
" SUADDEVENT - add a linear or hyperbolic moveout event to seismic data ",
"								       ",
" suaddevent <stdin >stdout [optional parameters]		       ",
"								       ",
" Required parameters:						  ",
"       none								",
"								       ",
" Optional parameters:						  ",
"     type=nmo    =lmo for linear event 				",
"     t0=1.0      zero-offset intercept time IN SECONDS			",
"     vel=3000.   moveout velocity in m/s				",
"     amp=1.      amplitude						",
"     dt=	 must provide if 0 in headers (seconds)		",
"   									",
" Typical usage: ",
"     sunull nt=500 dt=0.004 ntr=100 | sushw key=offset a=-1000 b=20 \\ ",
"     | suaddevent v=1000 t0=0.05 type=lmo | suaddevent v=1800 t0=0.8 \\",
"     | sufilter f=8,12,75,90 | suxwigb clip=1 &	     		",
"								       ",
"								       ",
NULL};

/* Credits:
 *      Gary Billings, Talisman Energy, May 1996, Apr 2000, June 2001
 *
 * Note:  code is inefficient in that to add a single "spike", with sinc
 *	interpolation, an entire trace is generated and added to 
 *	the input trace.  In fact, only a few points needed be created
 *	and added, but the current coding avoids the bookkeeping re
 *	which are the relevant points!
 */
/**************** end self doc *******************************************/

segy tr;

int
main(int argc, char **argv)
{

	int nsegy, nt;
	int i;
	float dt, tmin, t0, vel, tx;
	float amp;
	char *type;
	int typecode=0;
	float *addtr;
	float *times;

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	
	nsegy=gettr(&tr);
	if (!nsegy) err("cannot get first trace");

	/* Get nt, etc. from first trace */
	nt   = (int) tr.ns;
	dt = ((double) tr.dt)/1000000.0;   /* microsecs to secs */
	tmin = tr.delrt/1000.0;		   /* millisecs to secs */

	addtr=alloc1float( nt );
	times=alloc1float( nt );
	for(i=0;i<nt;i++){ times[i]=tmin+i*dt; addtr[i]=0.; }

	if (!dt) MUSTGETPARFLOAT("dt", &dt);

	if(!getparstring("type",&type)){ type="nmo"; typecode=0; }
	if(!strcmp(type,"lmo"))typecode=1;

	if(!getparfloat("t0",&t0))t0=1.;
	if(!getparfloat("vel",&vel))vel=3000.;
	if(!getparfloat("amp",&amp))amp=1.;

	while(nsegy){
		if(typecode==1) /* lmo */ tx=(t0+abs(tr.offset)/vel);   
		else /* nmo */ tx=sqrt( SQ(t0)+SQ((float)(tr.offset/vel)) ); 

		ints8r( 1, dt, tx, &amp, 0.0, 0.0, nt, times, addtr );

		for(i=0;i<nt;i++){
			tr.data[i]+=addtr[i];
			addtr[i]=0;
		}

		puttr(&tr);
		nsegy=gettr(&tr);
	}
	return(CWP_Exit());
}
