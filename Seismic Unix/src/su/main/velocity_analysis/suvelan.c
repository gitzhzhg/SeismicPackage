/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUVELAN: $Revision: 1.20 $ ; $Date: 2011/11/16 23:40:27 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************************/
char *sdoc[] = {
"									     ",
" SUVELAN - compute stacking velocity semblance for cdp gathers		     ",
"									     ",
" suvelan <stdin >stdout [optional parameters]				     ",
"									     ",
" Optional Parameters:							     ",
" nv=50                   number of velocities				     ",
" dv=50.0                 velocity sampling interval			     ",
" fv=1500.0               first velocity				     ",
" anis1=0.0               quartic term, numerator of an extended quartic term",
" anis2=0.0               in denominator of an extended quartic term         ",
" smute=1.5               samples with NMO stretch exceeding smute are zeroed",
" dtratio=5               ratio of output to input time sampling intervals   ",
" nsmooth=dtratio*2+1     length of semblance num and den smoothing window   ",
" verbose=0               =1 for diagnostic print on stderr		     ",
" pwr=1.0                 semblance value to the power      		     ",
"									     ",
" Notes:								     ",
" Velocity analysis is usually a two-dimensional screen for optimal values of",
" the vertical two-way traveltime and stacking velocity. But if the travel-  ",
" time curve is no longer close to a hyperbola, the quartic term of the      ",
" traveltime series should be considered. In its easiest form (with anis2=0) ",
" the optimizion of all parameters requires a three-dimensional screen. This ",
" is done by a repetition of the conventional two-dimensional screen with a  ",
" variation of the quartic term. The extended quartic term is more accurate, ",
" though the function is no more a polynomial. When screening for optimal    ",
" values the theoretical dependencies between these paramters can be taken   ",
" into account. The traveltime function is defined by                        ",
"                                                                            ",
"                1            anis1                                          ",
" t^2 = t_0^2 + --- x^2 + ------------- x^4                                  ",
"               v^2       1 + anis2 x^2                                      ",
"                                                                            ",
" The coefficients anis1, anis2 are assumed to be small, that means the non- ",
" hyperbolicity is assumed to be small. Triplications cannot be handled.     ",
"                                                                            ",
" Semblance is defined by the following quotient:			     ",
"									     ",
"                 n-1                 					     ",
"               [ sum q(t,j) ]^2      					     ",
"                 j=0                 					     ",
"       s(t) = ------------------     					     ",
"                 n-1                 					     ",
"               n sum [q(t,j)]^2      					     ",
"                 j=0                 					     ",
"									     ",
" where n is the number of non-zero samples after muting.		     ",
" Smoothing (nsmooth) is applied separately to the numerator and denominator ",
" before computing this semblance quotient.				     ",
"									     ",
" Then, the semblance is set to the power of the parameter pwr. With pwr > 1 ",
" the difference between semblance values is stretched in the upper half of  ",
" the range of semblance values [0,1], but compressed in the lower half of   ",
" it; thus, the few large semblance values are enhanced. With pwr < 1 the    ",
" many small values are enhanced, thus more discernible against background   ",
" noise. Of course, always at the expanse of the respective other feature.   ",
"									     ",
" Input traces should be sorted by cdp - suvelan outputs a group of	     ",
" semblance traces every time cdp changes.  Therefore, the output will	     ",
" be useful only if cdp gathers are input.				     ",
NULL};

/* Credits:
 *	CWP, Colorado School of Mines:
 *           Dave Hale (everything except ...)
 *           Bjoern Rommel (... the quartic term)
 *      SINTEF, IKU Petroleumsforskning
 *           Bjoern Rommel (... the power-of-semblance function)
 *
 * Trace header fields accessed:  ns, dt, delrt, offset, cdp
 * Trace header fields modified:  ns, dt, offset, cdp
 */
/**************** end self doc *******************************************/

segy tr;

int
main(int argc, char **argv)
{
	int nv;		/* number of velocities */
	float dv;	/* velocity sampling interval */
	float fv;	/* first velocity */
        float anis1;    /* quartic term, or numerator of an extended one */
        float anis2;    /* inside denominator of an extended quartic term */
	int iv;		/* velocity index */
	int dtratio;	/* ratio of output to input sampling intervals */
	int nsmooth;	/* length in samples of num and den smoothing window */
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
	float ti;	/* normalized time for linear interpolation */
	float frac;	/* fractional distance from sample in interpolation */
	int gottrace;	/* =1 if an input trace was read */
	int verbose;	/* =1 for diagnostic print */
	long cdp;	/* cdp from current input trace header */
	long cdpprev;	/* cdp from previous input trace header */
	float smute;	/* NMO stretch mute factor */
	float offset;	/* offset from input trace header */
	float offovs;	/* (offset/velocity)^2 */
        float offan=0.0;    /* shift of tnmo due to anisotropy */
	float tn;	/* time after NMO */
	float tnmute;	/* mute time after NMO */
	float nsum;	/* semblance numerator sum */
	float dsum;	/* semblance denominator sum */
	float v;	/* velocity */
	float temp;	/* temporary scalar */
	float *data;	/* array[nt] of input trace */
	float *sem;	/* array[ntout] of semblance */
	float **num;	/* array[nv][nt] of semblance numerators */
	float **den;	/* array[nv][nt] of semblance denominators */
	float **nnz;	/* array[nv][nt] for counting non-zero samples */
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
	offset = tr.offset;

	/* get optional parameters */
	if (!getparint("nv",&nv)) nv = 50;
	if (!getparfloat("dv",&dv)) dv = 50.0;
	if (!getparfloat("fv",&fv)) fv = 1500.0;
	if (!getparfloat("anis1",&anis1)) anis1 = 0.0;
	if (!getparfloat("anis2",&anis2)) anis2 = 0.0;
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
	data = ealloc1float(nt);
	num = ealloc2float(nt,nv);
	den = ealloc2float(nt,nv);
	nnz = ealloc2float(nt,nv);
	sem = ealloc1float(ntout);

	/* zero accumulators */
	for (iv=0; iv<nv; ++iv) {
		for (it=0; it<nt; ++it) {
			num[iv][it] = 0.0;
			den[iv][it] = 0.0;
			nnz[iv][it] = 0.0;
		}
	}

	/* initialize flag */
	gottrace = 1;

	/* remember previous cdp */
	cdpprev = tr.cdp;

	/* loop over input traces */
	while (gottrace|(~gottrace)/*True*/) { /* middle exit loop */

		/* if got a trace */
		if (gottrace) {

			/* determine offset and cdp */
			offset = tr.offset;
			cdp = tr.cdp;
                        if ((1.0 + offset*offset*anis2) <= 0.0)
                           err ("anis2 too small");
                        offan = (offset*offset*offset*offset*anis1) / 
                                (1.0 + offset*offset*anis2);

			/* get trace samples */
			memcpy( (void *) data,
				(const void *) tr.data,nt*sizeof(float));
		}

		/* if cdp has changed or no more input traces */
		if (cdp!=cdpprev || !gottrace) {

			/* set output trace header fields */
			tr.offset = 0;
			tr.cdp = (int) cdpprev;
			tr.ns = ntout;
			tr.dt = dtout*1000000.0;

			/* loop over velocities */
			for (iv=0; iv<nv; ++iv) {

				/* compute semblance quotients */
				for (itout=0; itout<ntout; ++itout) {
					it = itout*dtratio;
					ismin = it-nsmooth/2;
					ismax = it+nsmooth/2;
					if (ismin<0) ismin = 0;
					if (ismax>nt-1) ismax = nt-1;
					nsum = dsum = 0.0;
					for (is=ismin; is<ismax; ++is) {
						nsum += num[iv][is]*
							num[iv][is];
						dsum += nnz[iv][is]*
							den[iv][is];
					}
					sem[itout] = (dsum!=0.0?nsum/dsum:0.0);
				}

				/* powering the semblance */
				if (pwr != 1.0) {
				  for (itout=0; itout<ntout; ++itout)
				    sem[itout] = pow (sem[itout], pwr);
				};

				/* output semblances */
				memcpy((void *) tr.data,
				       (const void *) sem,ntout*sizeof(float));
				puttr(&tr);

				/* zero accumulators */
				for (it=0; it<nt; ++it) {
					num[iv][it] = 0.0;
					den[iv][it] = 0.0;
					nnz[iv][it] = 0.0;
				}
			}

			/* diagnostic print */
			if (verbose) 
				warn("semblance output for cdp=%d",cdpprev);

			/* if no more input traces, break input trace loop */
			if (!gottrace) break;

			/* remember previous cdp */
			cdpprev = cdp;
		}

		/* loop over velocities */
		for (iv=0,v=fv; iv<nv; ++iv,v+=dv) {
			
			/* compute offset/velocity squared */
			offovs = (offset*offset)/(v*v) + offan;
                        /* decrease of traveltime with distance due to highly
                           increasing velocity cannot be handled yet
                        */
                        if (offovs < 0.0)
                           warn("no moveout; check anis1 and anis2");

			/* determine mute time after nmo */
			tnmute = sqrt(offovs/(smute*smute-1.0));
			if (tnmute > ft) {
				itmute = (tnmute-ft)/dt;
			} else {
				itmute = 0 ;
			}
			
			/* do nmo via quick and dirty linear interpolation
			   (accurate enough for velocity analysis) and
			   accumulate semblance numerator and denominator
			*/

			for (it=itmute,tn=ft+itmute*dt; it<nt; ++it,tn+=dt) {
				ti = (sqrt(tn*tn+offovs)-ft)/dt;
				iti = ti;
				if (iti<nt-1) {
					frac = ti-iti;
					temp = (1.0-frac)*data[iti]+
						frac*data[iti+1];
					if (temp!=0.0) {
						num[iv][it] += temp;
						den[iv][it] += temp*temp;
						nnz[iv][it] += 1.0;
					}
				}
			}
		}

		/* get next trace (if there is one) */
		if (!gettr(&tr)) gottrace = 0;

	}

	return(CWP_Exit());
}
