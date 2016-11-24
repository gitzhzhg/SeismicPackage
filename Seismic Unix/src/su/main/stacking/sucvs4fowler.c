/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUCVS4FOWLER: $Revision: 1.5 $ ; $Date: 2011/11/16 23:14:54 $	*/

#include "su.h"
#include "segy.h"
#include "VND.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUCVS4FOWLER --compute constant velocity stacks for input to Fowler codes",
"									",
" Required Parameter:							",
" ncdps=		number of input cdp gathers			",
" Optional Parameters:							",
" vminstack=1500.	minimum velocity panel in m/s to output		",
" nvstack=180		number of stacking velocity panels to compute	",
"			( Let offmax be the maximum offset, fmax be	",
"			the maximum freq to preserve, and tmute be	",
"			the starting mute time in sec on offmax, then	",
"			the recommended value for nvstack would be	",
"			nvstack = 4 +(offmax*offmax*fmax)/(0.6*vmin*vmin*tmute)",
"			---you may want to make do with less---)		",
" lmute=24		length of mute taper in ms			",
" nonhyp=1		1 if do mute at 2*offset/vhyp to avoid		",
"			non-hyperbolic moveout, 0 otherwise		",
" vhyp=2500.		velocity to use for non-hyperbolic moveout mute	",
" lbtaper=0		length of bottom taper in ms			",
" lstaper=0		length of side taper in traces			",
" dtout=1.5*dt		output sample rate in s,			",
"			note: typically fmax=salias*0.5/dtout		",
" mxfold=120		maximum number of offsets/input cmp		",
" salias=0.8		fraction of output frequencies to force within sloth",
"			antialias limit.  This controls muting by offset of",
"			the input data prior to computing the cv stacks	",
"			for values of choose=1 or choose=2.		",
" Required trace header words on input are ns, dt, cdp, offset.		",
"									",
NULL};

/*
 * Author:  (Visitor to CSM from Mobil): John E. Anderson, Spring 1994
 * 
 *	References:
 *
 *	Fowler, P., 1988, Ph.D. Thesis, Stanford University.
 *	Anderson, J.E., Alkhalifah, T., and Tsvankin, I., 1994, Fowler
 *		DMO and time migration for transversely isotropic media,
 *		1994 CWP project review
 *
 */

/**************** end self doc ********************************/

static void taper (int lxtaper, int lbtaper, int nx, int 
	ix, int nt, float *trace);

int main(int argc, char **argv)
{
	int oldcmp;	/* current cdp header value */
	segy tr,trout;	/* input and output SEGY data */
	float vmin;	/* minimum output velocity */
	float dt;	/* input sample rate in seconds */
	float dtout;	/* output sample rate in seconds */
	float *p2stack;	/* array of slowness**2 values */
	float *mute;	/* array of mute times for this cmp */
	float *off;	/* array of offsets for this cmp */
	float *buf;	/* buffer for input trace */
	float *rt;	/* buffer for stack trace */
	float *ttn;	/* buffer for desired time samples */
	float *qtn;	/* buffer for values at desired time samples */
	float *fold;	/* fold of output */
	float *gath;	/* gather of input traces */
	float dp2;	/* increment in slowness squared for input cvstacks */
	float factor;	/* scale factor */
	float fmax;	/* maximum frequency to use for antialias mute */
	float salias;	/* fraction of frequencies to be within sloth antialias limit */
	float t;	/* output time in s */
	float vhyp;	/* velocity to use for computing hyperbolic mute */
	int nvstack;	/* number of cvstack panels to generate */
	int lmute;	/* number of samples to taper mute */
	int lbtaper;	/* length of bottom time taper in ms */
	int lstaper;	/* length of side taper in traces */
	int mxfold;	/* maximum allowed number of input offsets/cmp */
	int icmp;	/* cmp index */
	int noff;	/* number of offsets */
	int it;		/* time index */
	int nt;		/* number of input time samples */
	int ntout;	/* number of output time samples */
	int iv;		/* velocity index */
	int nonhyp;	/* flag equals 1 if do mute to avoid non-hyperbolic events */
	int ncmps;	/* number of cmps */
	int ioff;	/* offset index */
	int itmute;	/* mute index */
	int iend;	/* end of loop flag */
	int ieod;	/* end of input data flag */

	initargs(argc, argv);
	requestdoc(1);

	/* get first trace and extract critical info from header */
	if(!gettr(&tr)) err("Can't get first trace \n");
	nt=tr.ns;
	dt=0.000001*tr.dt;
	oldcmp=tr.cdp;
	if (!getparfloat("salias",&salias)) salias=0.8;
 	if(salias>1.0) salias=1.0;
	if (!getparfloat("dtout",&dtout)) dtout=1.5*dt;
	ntout=1+nt*dt/dtout;
	fmax=salias*0.5/dtout;
	if (!getparfloat("vminstack",&vmin)) vmin=1500.;
	if (!getparfloat("vhyp",&vhyp)) vhyp=2500.;
	if (!getparint("nvstack", &nvstack)) nvstack = 180;
	if (!getparint("lmute", &lmute)) lmute = 24;
	lmute=1 + 0.001*lmute/dtout;	
	if (!getparint("ncdps", &ncmps)) err("must enter ncdps");
	if (!getparint("lbtaper", &lbtaper)) lbtaper = 0;
	if (!getparint("lstaper", &lstaper)) lstaper = 0;
	if (!getparint("mxfold", &mxfold)) mxfold = 120;
	if (!getparint("nonhyp",&nonhyp)) nonhyp=1.;
	lbtaper=lbtaper/(1000.*dt);

        checkpars();

	p2stack=(float *)VNDemalloc(nvstack*sizeof(float),"p2stack");
	mute=(float *)VNDemalloc(mxfold*sizeof(float),"mute");
	off=(float *)VNDemalloc(mxfold*sizeof(float),"off");
	rt=(float *)VNDemalloc(ntout*sizeof(float),"rt");
	ttn=(float *)VNDemalloc(ntout*sizeof(float),"ttn");
	qtn=(float *)VNDemalloc(ntout*sizeof(float),"qtn");
	fold=(float *)VNDemalloc(ntout*sizeof(float),"fold");
	gath=(float *)VNDemalloc(nt*mxfold*sizeof(float),"gath");

	dp2 = 1./(vmin*vmin*(nvstack-5));
	for(iv=0;iv<nvstack;iv++) {
		p2stack[iv]=iv*dp2;
		if(iv>0) factor=1./sqrt(p2stack[iv]);
	}		

	icmp=0;
	noff=0;
	iend=0;
	ieod=0;
	do {
		if(tr.cdp!=oldcmp || ieod) {
			for(iv=0;iv<nvstack;iv++) {
		    		for(it=0;it<ntout;it++) rt[it]=0.;
		    		for(it=0;it<ntout;it++) fold[it]=0.;
		    		for(ioff=0;ioff<noff;ioff++) {
					buf=&gath[ioff*nt];
					factor=off[ioff]*off[ioff]*p2stack[iv];
					itmute=mute[ioff]/dtout;
			
					for(it=itmute;it<ntout;it++) {
						t=it*dtout;
						ttn[it]=sqrt(t*t+factor);
					}

					/* do nmo via 8-point sinc interpolation */
					ints8r(nt,dt,0.,buf,0.0,0.0,
						ntout-itmute,&ttn[itmute],&qtn[itmute]);

					/* apply linear ramp to taper mute */
					for (it=itmute; it<(itmute+lmute) && it<ntout; ++it)
						qtn[it] *= (float)(it-itmute+1)/(float)lmute;

					/* sum NMO corrected trace to stacked trace  */
					for(it=itmute;it<ntout;it++) rt[it]+=qtn[it];

					/* count fold information */
					for(it=itmute;it<ntout;it++) fold[it]+=1.;
		    		}
		    		for(it=0;it<ntout;it++) {
					if(fold[it]==0.) fold[it]=1.;
				}
		    		for(it=0;it<ntout;it++) 
					trout.data[it]=rt[it]/fold[it];
		    		trout.ns=ntout;
		    		trout.dt=1000000*dtout;
		    		trout.cdp=oldcmp;
		    		trout.tracf=iv; 
		   		puttr(&trout);
			}
			if(icmp==20*(icmp/20))
				fprintf(stderr,
				"cvstacks: completed stacking velocity analysis for cmp %d\n",icmp);
			icmp++;
			if(icmp>=ncmps) {
		 		iend=1;
				ieod=1;
			}
			oldcmp=tr.cdp;
			noff=0;
	   	}
		
		if(lbtaper>0 || lstaper>0) taper(lstaper,lbtaper,ncmps,icmp,nt,tr.data);
		buf=&gath[noff*nt];
		for(it=0;it<nt;it++) buf[it]=tr.data[it];
		off[noff]=tr.offset;
		mute[noff]=fmax*off[noff]*off[noff]*dp2;
		if(nonhyp) mute[noff]=MAX(mute[noff],2*off[noff]/vhyp);
		noff++;
		if(noff>mxfold) err("cvstacks: input cdp has more traces than mxfold");
		if(ieod) {
			iend++;
		}else{
			if(gettr(&tr)) {
				iend=0;
			}else{
				iend=1;
				ieod=1;
			}
		}
	}while( iend<2 );
	VNDfree(p2stack,"p2stack");
	VNDfree(mute,"mute");
	VNDfree(off,"off");
	VNDfree(rt,"rt");
	VNDfree(ttn,"ttn");
	VNDfree(qtn,"qtn");
	VNDfree(fold,"fold");
	if(VNDtotalmem()!=0) {
		fprintf(stderr,"total VND memory at end = %ld\n",
		VNDtotalmem());
	}

	fprintf(stderr,"cvstacks: read and stacked %d cmp gathers\n",icmp);
	return EXIT_SUCCESS;
}

static void taper (int lxtaper, int lbtaper, int nx, int ix, int nt, float *trace)
/*****************************************************************************
Taper traces near left and right sides of trace window
******************************************************************************
Input:
lxtaper		length (in traces) of side taper
lbtaper		length (in samples) of bottom taper
nx		number of traces in window
ix		index of this trace (0 <= ix <= nx-1)
nt		number of time samples
trace		array[nt] containing trace

Output:
trace		array[nt] containing trace, tapered if within lxtaper of side
*****************************************************************************/
{
	int it;
	float xtaper;

	/* if near left side */
	if (ix<lxtaper) {
		xtaper = 0.54+0.46*cos(PI*(lxtaper-ix)/lxtaper);
	
	/* else if near right side */
	} else if (ix>=nx-lxtaper) {
		xtaper = 0.54+0.46*cos(PI*(lxtaper+ix+1-nx)/lxtaper);
	
	/* else x tapering is unnecessary */
	} else {
		xtaper = 1.0;
	}

	/* if x tapering is necessary, apply it */
	if (xtaper!=1.0)
		for (it=0; it<nt; ++it)
			trace[it] *= xtaper;
	
	/* if requested, apply t tapering */
	for (it=MAX(0,nt-lbtaper); it<nt; ++it)
		trace[it] *= (0.54+0.46*cos(PI*(lbtaper+it+1-nt)/lbtaper));
}


