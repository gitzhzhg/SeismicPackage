/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SURELAN: $Revision: 1.8 $ ; $Date: 2011/11/16 23:40:27 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SURELAN - compute residual-moveout semblance for cdp gathers based	",
"	on z(h)*z(h) = z(0)*z(0) + r*h*h where z depth and h offset.	",
"									",
" surelan <stdin >stdout   [optional parameters]			",
"									",
" Optional Parameters:							",
" nr=51			number of r-parameter samples   		",
" dr=0.01               r-parameter sampling interval			",
" fr=-0.25               first value of b-parameter			",
" smute=1.5             samples with RMO stretch exceeding smute are zeroed",
" dzratio=5             ratio of output to input depth sampling intervals",
" nsmooth=dzratio*2+1   length of semblance num and den smoothing window",
" verbose=0             =1 for diagnostic print on stderr		",
"									",
" Note: 								",
" 1. This program is part of Zhenyue Liu's velocity analysis technique.	",
" 2. Input migrated traces should be sorted by cdp - surelan outputs a 	",
"    group of semblance traces every time cdp changes.  Therefore, the  ",
"    output will be useful only if cdp gathers are input.  		",
" 3. The parameter r may take negative values. The range of r can be 	",
"     controlled by maximum of (z(h)*z(h)-z(0)*z(0))/(h*h)   		",
NULL};
/**************** end self doc *******************************************/

/* 
 * Reference: 
 * Liu, Z. 1995, "Migration Velocity Analysis", Ph.D. Thesis, Colorado
 *      School of Mines, CWP report #168.
 *
 * Credits:
 *	CWP: Zhenyue Liu, 1995.
 *
 * Trace header fields accessed:  ns, d1, f1, offset, cdp
 * Trace header fields modified:  ns, d1, offset, cdp
 */

segy tr;

int
main(int argc, char **argv)
{
	float r;	/* defined in z*z = z0*z0+r*offset^2 */
	int nr;		/* number of r-parameter samples */
	float dr;	/* r-parameter samples sampling interval */
	float fr;	/* first r-parameter samples */
	int ir;		/* r-parameter samples index */
	int dzratio;	/* ratio of output to input sampling intervals */
	int nsmooth;	/* length in samples of num and den smoothing window */
	int nz;		/* number of time samples per input trace */
	float dz;	/* time sampling interval for input traces */
	float fz;	/* time of first sample input and output */
	int nzout;	/* number of output samples */
	float dzout;	/* time sampling interval for output traces */
	int iz;		/* input depth sample index */
	int izout;	/* output depth sample index */
	int is;		/* depth sample index for smoothing window */
	int ismin;	/* lower limit on is */
	int ismax;	/* upper limit on is */
	int izmute;	/* depth sample index of first sample not muted */
	int izi;	/* depth sample index used in linear interpolation */
	float zi;	/* normalized time for linear interpolation */
	float frac;	/* fractional distance from sample in interpolation */
	int gottrace;	/* =1 if an input trace was read */
	int done;	/* =1 if done with everything */
	int verbose;	/* =1 for diagnostic print */
	long cdp;	/* cdp from current input trace header */
	long cdpprev;	/* cdp from previous input trace header */
	float smute;	/* residual-moveout stretch mute factor */
	float offset;	/* offset from input trace header */
	float roffs2;	/* r*offset*offset */
	float zn;	/* time after residual-moveout */
	float znmute;	/* mute time after residual-moveout */
	float nsum;	/* semblance numerator sum */
	float dsum;	/* semblance denominator sum */
	float temp;	/* temporary scalar */
	float *data;	/* array[nz] of input trace */
	float *sem;	/* array[nzout] of semblance */
	float **num;	/* array[nr][nz] of semblance numerators */
	float **den;	/* array[nr][nz] of semblance denominators */
	float **nnz;	/* array[nr][nz] for counting non-zero samples */

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(0);

	/* get parameters from the first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nz = tr.ns;
	dz = tr.d1;
 	fz = tr.f1;
	cdp = tr.cdp;
	offset = tr.offset;

	
	/* get optional parameters */
	if (!getparint("nr",&nr)) nr = 51;
	if (!getparfloat("dr",&dr)) dr = 0.01;
	if (!getparfloat("fr",&fr)) fr = -0.25;
	if (!getparfloat("smute",&smute)) smute = 1.5;
	if (smute<=1.0) err("smute must be greater than 1.0");
	if (!getparint("dzratio",&dzratio)) dzratio = 5;
	if (!getparint("nsmooth",&nsmooth)) nsmooth = dzratio*2+1;
	if (!getparint("verbose",&verbose)) verbose = 0;

        checkpars();
	/* determine output sampling */
	nzout = 1+(nz-1)/dzratio; CHECK_NT("nzout", nzout);
	dzout = dz*dzratio;
	if (verbose) {
		warn("number of depth samples is %d\n" ,nzout);
		warn("output depth sampling interval is %g\n",dzout);
		warn("output depth of first sample is %g\n",fz);
	}

	/* allocate memory */
	data = ealloc1float(nz);
	num = ealloc2float(nz,nr);
	den = ealloc2float(nz,nr);
	nnz = ealloc2float(nz,nr);
	sem = ealloc1float(nzout);

	/* zero accumulators */
	for (ir=0; ir<nr; ++ir) {
		for (iz=0; iz<nz; ++iz) {
			num[ir][iz] = 0.0;
			den[ir][iz] = 0.0;
			nnz[ir][iz] = 0.0;
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
			memcpy( (void *) data, (const void *) tr.data,
				nz*sizeof(float));
		}

		/* if cdp has changed or no more input traces */
		if (cdp!=cdpprev || !gottrace) {

			/* set output trace header fields */
			tr.offset = 0;
			tr.cdp = (int) cdpprev;
			tr.ns = nzout;
			tr.d1 = dzout;

			/* loop over r-parameters */
			for (ir=0; ir<nr; ++ir) {

				/* compute semblance quotients */
				for (izout=0; izout<nzout; ++izout) {
					iz = izout*dzratio;
					ismin = iz-nsmooth/2;
					ismax = iz+nsmooth/2;
					if (ismin<0) ismin = 0;
					if (ismax>nz-1) ismax = nz-1;
					nsum = dsum = 0.0;
					for (is=ismin; is<=ismax; ++is) {
						nsum += num[ir][is]*
							num[ir][is];
						dsum += nnz[ir][is]*
							den[ir][is];
					}
					sem[izout] = (dsum!=0.0?nsum/dsum:0.0);
				}

				/* output semblances */
				memcpy( (void *) tr.data,(const void *) sem,
					nzout*sizeof(float));
				puttr(&tr);

				/* zero accumulators */
				for (iz=0; iz<nz; ++iz) {
					num[ir][iz] = 0.0;
					den[ir][iz] = 0.0;
					nnz[ir][iz] = 0.0;
				}
			}

			/* diagnostic print */
			if (verbose) 
				warn("tsemblance output for cdp=%d\n",cdpprev);

			/* if no more input traces, break input trace loop */
			if (!gottrace) break;

			/* remember previous cdp */
			cdpprev = cdp;
		}

		/* loop over residual moveouts */
		for (ir=0,r=fr; ir<nr; ++ir,r+=dr) {
 			
			/* ignore trace in which offset square is larger
				than the maximum */
			
			/* compute r multiplied by offset squared  */
			roffs2 = r*offset*offset;


			/* determine mute depth after rmo */
			znmute = roffs2/(smute*smute-1.0);
			znmute = (znmute>0)?sqrt(znmute):sqrt(-znmute);
			izmute = (znmute-fz)/dz;
			if(izmute<0) izmute = 0;
			
			/* do rmo via linear interpolation and  */
			/* accumulate semblance numerator and denominator */
			for (iz=izmute,zn=fz+izmute*dz; iz<nz; ++iz,zn+=dz) {
				temp = zn*zn+roffs2;
				zi = (temp>fz*fz)?(sqrt(temp)-fz)/dz:0;
				izi = zi;
				if (izi<nz-1) {
					frac = zi-izi;
					temp = (1.0-frac)*data[izi]+
						frac*data[izi+1];
					if (temp!=0.0) {
						num[ir][iz] += temp;
						den[ir][iz] += temp*temp;
						nnz[ir][iz] += 1.0;
					}
			    }
			}
		}

		/* get next trace (if there is one) */
		if (!gettr(&tr)) gottrace = 0;

	} while (!done);

	return(CWP_Exit());
}
