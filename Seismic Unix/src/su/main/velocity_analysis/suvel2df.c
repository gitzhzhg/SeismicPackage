/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUVEL2DF: $Revision: 1.4 $ ; $Date: 2011/11/16 23:40:27 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUVEL2DF - compute stacking VELocity semblance for a single time in   ",
"			    over Vnmo and eta in 2-D			",
"									",
"    suvel2df <stdin >stdout [optional parameters]			",
"									",
" Required Parameters:							",
" tn			zero-offset time of reflection			",
" offsetm		Maximum offset considerd			",
"									",
" Optional Parameters:							",
" nv=50			number of velocities				",
" dv=50.0		velocity sampling interval			",
" fv=1500.0		first velocity					",
" nvh=50		number of horizotal velocities			",
" dvh=50.0		horizontal velocity sampling interval		",
" fvh=1500.0		first horizontal velocity			",
" xod=1.5		maximum offset-to-depth ratio to resolve	",
" dtratio=5		ratio of output to input time sampling intervals",
" nsmooth=dtratio*2+1	length of semblance num and den smoothing window",
" verbose=0		=1 for diagnostic print on stderr		",
" vavg=fv+0.5*(nv-1)*dv   average velocity used in the search		",
"									",
" Notes:								",
" Semblance is defined by the following quotient:			",
"									",
"		 n-1		 					",
"		[ sum q(t,j) ]^2					",
"		 j=0		 					",
"	s(t) = ------------------					",
"		 n-1		 					",
"		n sum [q(t,j)]^2					",
"		 j=0		 					",
"									",
" where n is the number of non-zero samples after muting.		",
" Smoothing (nsmooth) is applied separately to the numerator and denominator",
" before computing this semblance quotient.				",
"									",
" Input traces should be sorted by cdp - suvel2df outputs a group of	",
" semblance traces every time cdp changes.  Therefore, the output will	",
" be useful only if cdp gathers are input.				",
"									",
NULL};

/* Credits:
 *	CWP: Tariq Alkhalifah,  February 1997
 * Trace header fields accessed:  ns, dt, delrt, offset, cdp.
 * Trace header fields modified:  ns, dt, offset.
 */
/**************** end self doc *******************************************/

segy tr;

int
main(int argc, char **argv)
{
	int nv;		/* number of velocities */
	float dv;	/* velocity sampling interval */
	float fv;	/* first velocity */
	int iv;		/* velocity index */
	int dtratio;	/* ratio of output to input sampling intervals */
	int nsmooth;	/* length in samples of num and den smoothing window */
	int nt;		/* number of time samples per input trace */
	float dt;	/* time sampling interval for input traces */
	float ft;	/* time of first sample input and output */
	int it;		/* input time sample index */
	int is;		/* time sample index for smoothing window */
	int ismin;	/* lower limit on is */
	int ismax;	/* upper limit on is */
	int iti;	/* time sample index used in linear interpolation */
	float ti;	/* normalized time for linear interpolation */
	float frac;	/* fractional distance from sample in interpolation */
	int gottrace;	/* =1 if an input trace was read */
	int done;	/* =1 if done with everything */
	int verbose;	/* =1 for diagnostic print */
	long cdp;	/* cdp from current input trace header */
	long cdpprev;	/* cdp from previous input trace header */
	float xod;	/* NMO stretch mute factor */
	float offset;	/* offset from input trace header */
	float offovs;	/* (offset/velocity)^2 */
	float tn;	/* time after NMO */
	float nsum;	/* semblance numerator sum */
	float dsum;	/* semblance denominator sum */
	float v;	/* velocity */
	float temp;	/* temporary scalar */
	float *data;	/* array[nt] of input trace */
	float *sem;	/* array[ntout] of semblance */
	float ***num;	/* array[nv][nt] of semblance numerators */
	float ***den;	/* array[nv][nt] of semblance denominators */
	float ***nnz;	/* array[nv][nt] for counting non-zero samples */
	int ivh,nvh;
	float offset2,fac1,fac2,v2,v4,offsetmax,vh,vh2;
	float dvh,offsetm,fvh,max=0,vmax,vhmax,tnn,vavg;

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(0);

	/* get parameters from the first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = tr.dt/1000000.0;
	ft = tr.delrt/1000.0;
	cdp = tr.cdp;
	offset = tr.offset;

	/*get required parameters*/
	if (!getparfloat("tn",&tnn)) err("must specify tn!");
	if (!getparfloat("offsetm",&offsetm)) err("must specify offsetm!");

	/* get optional parameters */
	if (!getparint("nv",&nv)) nv = 50;
	if (!getparfloat("dv",&dv)) dv = 50.0;
	if (!getparfloat("fv",&fv)) fv = 1500;
	if (!getparint("nvh",&nvh)) nvh = 50;
	if (!getparfloat("dvh",&dvh)) dvh = 50.0;
	if (!getparfloat("fvh",&fvh)) fvh = 1500;
	if (!getparfloat("xod",&xod)) xod = 1.5;
	if (!getparint("dtratio",&dtratio)) dtratio = 5;
	if (!getparint("nsmooth",&nsmooth)) nsmooth = dtratio*2+1;
	if (!getparint("verbose",&verbose)) verbose = 0;
	if (!getparfloat("vavg",&vavg)) vavg = fv+0.5*(nv-1)*dv;
	if (!getparfloat("offsetmax",&offsetmax)) offsetmax=0.5*tnn*xod*vavg;

	vmax=fv;
	vhmax=fvh;

        checkpars();

	/* allocate memory */
	data = ealloc1float(nt);
	num = ealloc3float(nsmooth,nvh,nv);
	den = ealloc3float(nsmooth,nvh,nv);
	nnz = ealloc3float(nsmooth,nvh,nv);
	sem = ealloc1float(nvh);

	/* zero accumulators */
	for (iv=0; iv<nv; ++iv) {
		for (ivh=0; ivh<nvh; ++ivh) {
		  for (it=0; it<nsmooth; ++it) {
			num[iv][ivh][it] = 0.0;
			den[iv][ivh][it] = 0.0;
			nnz[iv][ivh][it] = 0.0;
		  }
		}
	}

	/* initialize flags */
	gottrace = 1;
	done = 0;

	/* remember previous cdp */
	cdpprev = tr.cdp;

	/* loop over input traces */
	do {

		/* if got a trace */
		if (gottrace) {

			/* determine offset and cdp */
			offset = tr.offset;
			cdp = tr.cdp;

			/* get trace samples */
			memcpy( (void *) data,
				(const void *) tr.data,nt*sizeof(float));
		}

		/* if cdp has changed or no more input traces */
		if (cdp!=cdpprev || !gottrace) {

			/* set output trace header fields */
			tr.offset = 0;
			tr.cdp = cdpprev;
			tr.ns = nvh;
			tr.dt = dvh;

			/* loop over velocities */
			for (iv=0; iv<nv; ++iv) {

				/* compute semblance quotients */
				for (ivh=0; ivh<nvh; ++ivh) {
				  ismin = 0;
				  ismax = nsmooth;
				  nsum = dsum = 0.0;
				  for (is=ismin; is<ismax; ++is) {
				    nsum += num[iv][ivh][is]*num[iv][ivh][is];
				    dsum += nnz[iv][ivh][is]*den[iv][ivh][is];
				  }
				  sem[ivh] = (dsum!=0.0?nsum/dsum:0.0);
				  if(sem[ivh]>max) {
				    max=sem[ivh];
				    vhmax = fvh+ivh*dvh;
				    vmax = fv+iv*dv;
				  }
				}

				/* output semblances */
				memcpy((void *) tr.data,
					(const void *) sem,nvh*sizeof(float));
				puttr(&tr);

				/* zero accumulators */
				for (ivh=0; ivh<nvh; ++ivh) {
				  for (it=0; it<nsmooth; ++it) {
				    num[iv][ivh][it] = 0.0;
				    den[iv][ivh][it] = 0.0;
				    nnz[iv][ivh][it] = 0.0;
				  }
				}
			}

			warn("max=%f vmax=%f vhmax=%f\n",max,vmax,vhmax);
			/* diagnostic print */
			if (verbose) 
				warn("semblance output for cdp=%ld\n", cdpprev);

			/* if no more input traces, break input trace loop */
			if (!gottrace) break;

			/* remember previous cdp */
			cdpprev = cdp;
		}

		/* loop over velocities */
		if(ABS(offset)>offsetmax) {
			if (!gettr(&tr)) gottrace = 0;
			continue;
		}
			
		offset2=offset*offset;
	
		for (iv=0,v=fv; iv<nv; ++iv,v+=dv) {
		  for (ivh=0,vh=fvh; ivh<nvh; ++ivh,vh+=dvh) {
			
			/* compute offset/velocity squared */
			v2=v*v;
			v4=v2*v2;
			vh2=vh*vh;
			offovs = offset2/v2;
			fac1   = (vh2-v2)*offset2*offset2/v2;
			fac2   = vh2*offset2;

			ismin = tnn/dt-nsmooth/2;
			ismax = ismin+nsmooth;
			if (ismin<0) ismin = 0;
			if (ismax>nt-1) ismax = nt-1;
			
			/* do nmo via quick and dirty linear interpolation */
			/* (accurate enough for velocity analysis) and */
			/* accumulate semblance numerator and denominator */
			for (it=ismin, is=0,tn=ft+ismin*dt; it<ismax; ++it, ++is,tn+=dt) {
			  if(offset==0)
			    ti = (sqrt(tn*tn+offovs)-ft)/dt;
			  else
			    ti = (sqrt(tn*tn+offovs-fac1/
					(tn*tn*v4+fac2))-ft)/dt;
			  iti = ti;
			  /*warn("offs=%f v=%f tn=%f ti=%f\n",offset,v,tn,ti);*/
			  if (iti<nt-1) {
			    frac = ti-iti;
			    temp = (1.0-frac)*data[iti]+
				frac*data[iti+1];
			    if (temp!=0.0) {
				num[iv][ivh][is] += temp;
				den[iv][ivh][is] += temp*temp;
				nnz[iv][ivh][is] += 1.0;
			    }
			  }
			}
		  }
		}

		/* get next trace (if there is one) */
		if (!gettr(&tr)) gottrace = 0;

	} while (!done);

	return(CWP_Exit());
}
