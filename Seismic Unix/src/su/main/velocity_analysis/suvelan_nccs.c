/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUVELAN_NCCS : $Revision: 1.6 $ ; $Date: 2011/11/16 23:40:27 $	*/ 

#include "su.h"
#include "segy.h"
#include "string.h"

/*********************** self documentation **********************************/
char *sdoc[] = {
"									     ",
" SUVELAN_NCCS - compute stacking VELocity panel for cdp gathers	     ",
"		using Normalized CrossCorrelation Sum 	                     ",
"									     ",
" suvelan_uccs <stdin >stdout [optional parameters]			     ",
"									     ",
" Optional Parameters:							     ",
" nx=tr.cdpt              number of traces in cdp			     ",
" nv=50                   number of velocities				     ",
" dv=50.0                 velocity sampling interval			     ",
" fv=1500.0               first velocity				     ",
" smute=1.5               samples with NMO stretch exceeding smute are zeroed",
" dtratio=5               ratio of output to input time sampling intervals   ",
" nsmooth=dtratio*2+1     length of smoothing window                         ",
" verbose=0               =1 for diagnostic print on stderr		     ",
" pwr=1.0                 semblance value to the power      		     ",
"									     ",
" Notes:								     ",
" Normalized CrossCorrelation sum: sum all possible crosscorrelation	     ",
" trace pairs in a CMP gather for each trial velocity and zero-offset        ",
" two-way travel time inside a time window. This coherence measure is        ",
" normalized by dividing each crosscorrelation trace pair by the geometric   ",
" mean of the energy, inside the chosen time window, of each trace pair      ",
" involved in each crosscorrelation. Then, to achieve a maximum amplitude    ",
" of unity, the result is multiplied by  2/(M(M-1)), which is the inverse    ",
" of the total number of crosscorrelation. The normalization allows to	     ",
" bring out weak reflection as long as these reflections have moveouts close ",
" to a hyperbola.							     ",
NULL};


/* 
 * Credits:  
 *
 * CWP:  Valmore Celis, Sept 2002	
 *
 * Based on the original code: suvelan.c 
 *    Colorado School of Mines:  Dave Hale, c. 1989
 *
 * Trace header fields accessed:  ns, dt, delrt, offset, cdp, cdpt 
 * Trace header fields modified:  ns, dt, offset, cdp
 *
 * Reference: Neidell, N.S., and Taner, M.T., 1971, Semblance and 
 *             other coherency measures for multichannel data: 
 *             Geophysics, 36, 498-509. 
 *
 */
/**************** end self doc *******************************************/

segy tr;

int
main(int argc, char **argv)
{
	int nx; 	/* number of traces in the cmp gather */
	int ix;		/* trace number index */ 
	int nv;		/* number of velocities */
	float dv;	/* velocity sampling interval */
	float fv;	/* first velocity */
	float fac;	/* cumulative product of traces in window (ismin,ismax) */
	float ff;	/* sum over time window (ismin,ismax) */
	int iv;		/* velocity index */
	int dtratio;	/* ratio of output to input sampling intervals */
	int nsmooth;	/* length in samples of smoothing window */
	int nt;		/* number of time samples per input trace */
	float dt;	/* time sampling interval for input traces */
	float ft;	/* time of first sample input and output */
	int ntout;	/* number of output samples */
	float dtout;	/* time sampling interval for output traces */
	int it;		/* input time sample index */
	int itout;	/* output time sample index */
	int is;		/* time sample index for smoothing window */
	int ismin;	/* lower limit on is */
	int ismax;	/* upper limit on is */
	int itmute;	/* time sample index of first sample not muted */
	int iti;	/* time sample index used in linear interpolation */
	int inc;	/* total number of crosscorrelations */
	float ti;	/* normalized time for linear interpolation */
	float frac;	/* fractional distance from sample in interpolation */
	int gottrace;	/* =1 if an input trace was read */
	int verbose;	/* =1 for diagnostic print */
	long cdp;	/* cdp from current input trace header */
	long cdpprev;	/* cdp from previous input trace header */
	float smute;	/* NMO stretch mute factor */
	float offset;	/* offset from input trace header */
	float offovs;	/* (offset/velocity)^2 */
	float tn;	/* time after NMO */
	float temp1=0.0;/* temporal variable */
	float tnmute;	/* mute time after NMO */
	float nsum;	/* actual trace in the time window (ismin,ismax) */
	float dsum;	/* trace accumulation */
	float v;	/* velocity */
	float *norm;	/* array[nx] of input trace offsets */
	float *aoff;	/* array[nx] of input trace offsets */
	float *sem;	/* array[ntout] of semblance */
	float **datm;	/* array[nt][nx] of input traces */
	float **datn;	/* array[nt][nx] of traces with NMO correction*/
	float pwr;      /* power of semblance */

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(0);

	/* get parameters from the first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	ft = tr.delrt/1000.0;
	cdp = tr.cdp;
	
	/* get optional parameters */
	if (!getparint("nx",&nx)) nx = tr.cdpt;
	inc = nx*(nx-1)/2;

	if (!getparint("nv",&nv)) nv = 50;
	if (!getparfloat("dv",&dv)) dv = 100.0;
	if (!getparfloat("fv",&fv)) fv = 1500.0;
	if (!getparfloat("smute",&smute)) smute = 1.5;
	if (smute<=1.0) err("smute must be greater than 1.0");
	if (!getparint("dtratio",&dtratio)) dtratio = 5;
	if (!getparint("nsmooth",&nsmooth)) nsmooth = dtratio*2+1;
	if (!getparint("verbose",&verbose)) verbose = 0;
	if (!getparfloat("pwr",&pwr)) pwr = 1.0;
	if (pwr < 0.0)   
	  err("we are not looking for noise: pwr < 0");
	if (pwr == 0.0)   
	  err("we are creating an all-white semblance: pwr = 0");

        checkpars();
	/* determine output sampling */
	ntout = 1+(nt-1)/dtratio;   CHECK_NT("ntout",ntout);
	dtout = dt*dtratio;
	if (verbose) {
		fprintf(stderr,
			"\tnumber of output time samples is %d\n",ntout);
		fprintf(stderr,
			"\toutput time sampling interval is %g\n",dtout);
		fprintf(stderr,
			"\toutput time of first sample is %g\n",ft);
	}

	/* allocate memory */
	norm = ealloc1float(nx);
	aoff = ealloc1float(nx);
	datm = ealloc2float(nt,nx);
	datn = ealloc2float(nt,nx);
	sem = ealloc1float(ntout);

	/* zero accumulators */
	memset((void *) datm[0], 0, FSIZE*nt*nx);
	memset((void *) datn[0], 0, FSIZE*nt*nx);
	memset((void *) norm, 0, FSIZE*nx);

	/* initialize flag */
	gottrace = 1;

	/* remember previous cdp */
	cdpprev = tr.cdp;
        ix = 0; 
	/* loop over input traces */
	while (gottrace|(~gottrace)/*True*/) { /* middle exit loop */
		/* if got a trace */
		if (gottrace) {
			/* determine offset and cdp */
			offset = tr.offset;
			aoff[ix] = offset;

			cdp = tr.cdp;

			/* get trace samples */
			memcpy( (void *) datm[ix],
				(const void *) tr.data,nt*sizeof(float));
			++ix;
		}

		/* if cdp has changed or no more input traces */
		if (cdp!=cdpprev || !gottrace) {

		/*  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  */

		for (iv=0,v=fv; iv<nv; ++iv,v+=dv) {
			for (ix=0;ix<nx;++ix) {

                        /* compute offset/velocity squared */
                        offovs = (aoff[ix]*aoff[ix])/(v*v);

                        /* determine mute time after nmo */
                        tnmute = sqrt(offovs/(smute*smute-1.0));
                        if (tnmute > ft) {
                                itmute = (tnmute-ft)/dt;
                        } else {
                                itmute = 0 ;
                        }

                        /* do nmo via quick and dirty linear interpolation
                           (accurate enough for velocity analysis) */
 			
			for (it=itmute,tn=ft+itmute*dt; it<nt; ++it,tn+=dt) {
                                ti = (sqrt(tn*tn+offovs)-ft)/dt;
                                iti = ti;
                                if (iti<nt-1) {
                                        frac = ti-iti;
                                        datn[ix][it] = (1.0-frac)*datm[ix][iti]+
                                                frac*datm[ix][iti+1];
                                        }
                                }
                        }
			for (itout=0; itout<ntout; ++itout) {
                                it = itout*dtratio;
                                ismin = it-nsmooth/2;
                                ismax = it+nsmooth/2;
                                if (ismin<0) ismin = 0;
                                if (ismax>nt-1) ismax = nt-1;
                                ff = 0.0;
				for (ix=0;ix<nx;++ix){
                                        norm[ix]=0.0;
                                        for (is=ismin; is<ismax; ++is){
                                            temp1+=(datn[ix][is]*datn[ix][is]);
                                        }
                                        norm[ix] = sqrt(temp1)+1;
                                        temp1=0.0;
                                }
                                for (is=ismin; is<ismax; ++is) {
                                        nsum = dsum = fac = 0.0;
				        for (ix=0;ix<nx;++ix) {
                                                nsum = datn[ix][is]/norm[ix];
						dsum = dsum + nsum;
						fac  = fac + nsum*dsum;
                                        }
					ff  = ff + fac; 
                                }
			sem[itout]=ff/inc;  
			}	

		/* set output trace header fields */
			tr.offset = 0;
			tr.cdp = (int) cdpprev;
			tr.ns = ntout;
			tr.dt = dtout*1000000.0;
		/* output semblance */
			memcpy((void *) tr.data,
                       	       (const void *) sem,ntout*sizeof(float));
                	puttr(&tr);
		}

		/*  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  */

			/* diagnostic print */
			if (verbose) 
				warn("semblance output for cdp=%d",cdpprev);

			/* if no more input traces, break input trace loop */
			if (!gottrace) break;

			/* remember previous cdp */
			cdpprev = cdp;
		}

		/* get next trace (if there is one) */
		if (!gettr(&tr)) gottrace = 0;
	}
	return(CWP_Exit());
}
