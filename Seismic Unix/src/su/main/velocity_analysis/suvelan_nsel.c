/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUVELAN_NSEL: $Revision: 1.7 $ ; $Date: 2011/11/16 23:40:27 $	*/

#include "su.h"
#include "segy.h"
#include "string.h"

/*********************** self documentation **********************************/
char *sdoc[] = {
"									     ",
" SUVELAN_NSEL - compute stacking VELocity panel for cdp gathers	     ",
"		using the Normalized Selective CrossCorrelation sum	     ",
"									     ",
" suvelan_usel <stdin >stdout [optional parameters]			     ",
"									     ",
" Optional Parameters:							     ",
" nx=tr.cdpt              number of traces in cdp			     ",
" dx=tr.d2 	          offset increment				     ",
" nv=50                   number of velocities				     ",
" dv=100.0                velocity sampling interval			     ",
" fv=1500.0               first velocity				     ",
" tau=0.5                 threshold for significance values                  ",
" smute=1.5               samples with NMO stretch exceeding smute are zeroed",
" dtratio=5               ratio of output to input time sampling intervals   ",
" nsmooth=dtratio*2+1     length of smoothing window                         ",
" verbose=0               =1 for diagnostic print on stderr		     ",
" pwr=1.0                 semblance value to the power      		     ",
"									     ",
" Notes:								     ",
" Normalized Selective CrossCorrelation Sum: is based on the coherence       ",
" measure known as crosscorrelation sum. The difference is that the selective",
" approach sum only crosscorrelation pairs with relatively large differential",
" moveout, thus increasing the resolving power in the velocity spectra       ",
" compared to that achieved by conventional methods. The normalization is    ",
" achieved in much the same way of normalizing the conventional              ",
" crosscorrelation sum.						             ",
"									     ",
" Each crosscorrelation is divided by the geometric mean		     ",
" of the energy of the traces involved, and the multiplying by a constant to ",
" achieve maximum amplitude of unity. The constant is just the inverse of the",
" total number of crosscorrelations included in the sum.  The selection is   ",
" made using a parabolic approximation of the differential moveout and       ",
" imposing a threshold for those differential moveouts.		   	     ",
"									     ",
" That threshold is the parameter tau in this program, which varies between 0",
" to 1.	 A value of tau=0, means conventional crosscorrelation sum is applied",
" implying that all crosscorrelations are included in the sum. In contrast,  ",
" a value of tau=1 (not recomended) means that only the crosscorrelation     ",
" formed by the trace pair involving the shortest and longest offset is      ",
" included in the sum. Intermediate values will produce percentages of the   ",
" crosscorrelations included in the sum that will be shown in the screen     ",
" before computing the velocity spectra. Typical values for tau are between  ",
" 0.2 and 0.6, producing approximated percentages of crosscorrelations summed",
" between 60% and 20%. The higher the value of tau the lower the percentage",
" and higher the increase in the resolving power of velocity spectra.        ",
"									     ",
" Keeping the percentage of crosscorrelations included in the sum between 20%",
" and 60% will increase resolution and avoid the precense of artifacts in   ",
" the results.  In data contaminated by random noise or statics distortions   ",
" is recomended to mantaing the percentage of crosscorrelations included in   ",
" the sum above 25%. After computing the velocity spectra one might want to  ",
" adjust the level  and number of contours before velocity picking.	      ",
NULL};

/* 
 * Credits: CWP:  Valmore Celis, Sept 2002	
 * 
 * Based on the original code: suvelan.c 
 *    Colorado School of Mines:  Dave Hale c. 1989
 *
 * References: 
 * Neidell, N.S., and Taner, M.T., 1971, Semblance and other 
 *   coherency measures for multichannel data: Geophysics, 36, 498-509.
 * Celis, V. T., 2002, Selective-correlation velocity analysis: CSM thesis.
 *
 *
 * Trace header fields accessed:  ns, dt, delrt, offset, cdp
 * Trace header fields modified:  ns, dt, offset, cdp
 */
/**************** end self doc *******************************************/

segy tr;

int
main(int argc, char **argv)
{
	int ol; 	/* ol */
	int oaux; 	/* oaux */
	int j0t; 	/* j0t */
	int m=0.0; 	/* m */
	int beta; 	/* beta */
	int tncc; 	/* tncc */
	int nx; 	/* number of traces in the cmp gather */
	int ix;		/* trace number index */ 
	int ik;		/* trace number index */ 
	int ktj;	/* trace number index */ 
	int ktj1;	/* trace number index */ 
	int ktj2;	/* trace number index */ 
	int nv;		/* number of velocities */
	float dx;	/* dx */
	float dv;	/* velocity sampling interval */
	float fv;	/* first velocity */
	float fac;	/* cumulative product of traces in window (ismin,ismax) */
	float ff;	/* sum over time window (ismin,ismax) */
	float tau;	/* threshold for significance values */
	float temp;	/* temp */
	float temp1;	/* temp1 */
	float pcc;	/* percentage of crosscorrelation included in the sum */
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
	float tnmute;	/* mute time after NMO */
	float nsum;	/* actual trace in the time window (ismin,ismax) */
	float dsum;	/* trace accumulation */
	float v;	/* velocity */
	float *norm;	/* array[nx] of input trace energy inside the time window */
	float *aoff;	/* array[nx] of input trace samples */
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
	if (!getparfloat("dx",&dx)) dx = tr.d2;
	if (!getparint("nv",&nv)) nv = 50;
	if (!getparfloat("dv",&dv)) dv = 50.0;
	if (!getparfloat("fv",&fv)) fv = 1500.0;
	if (!getparfloat("tau",&tau)) tau = 0.5;
	if (!getparfloat("smute",&smute)) smute = 15;
	if (smute<=1.0) err("smute must be greater than 1.0");
	if (!getparint("dtratio",&dtratio)) dtratio = 5;
	if (!getparint("nsmooth",&nsmooth)) nsmooth = dtratio*1+1;
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
	aoff = ealloc1float(nx);
	norm = ealloc1float(nx);
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

		ol = floor(fabs(aoff[0]));
		for (ix=1;ix<nx;++ix) {
			oaux = floor(fabs(aoff[ix]));
			m = (oaux < ol) ? oaux : ol;
		}
		ol  = m/dx;
		j0t = floor(sqrt(tau*((nx+ol-1)*(nx+ol-1)-ol*ol)+ol*ol)-ol)+2;
		beta = 0;
		for (ix=j0t;ix<=nx;++ix) {
			beta += floor(sqrt((ix+ol-1)*(ix+ol-1)-
				tau*((nx+ol-1)*(nx+ol-1)-ol*ol))-ol);
		}
		tncc = (nx-1)*nx/2;
		pcc = (float) beta*100/tncc;
		warn("\t---- the percentage of crosscorrelations included in the sum is = %6.2f ----\n\n",pcc);

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
				temp1=0.0;
				
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
			ktj = floor(sqrt((j0t+ol-1)*(j0t+ol-1)-
				tau*((nx+ol-1)*(nx+ol-1)-ol*ol))-ol);
				for (ik=0;ik<ktj;++ik){
				dsum +=datn[ik][is]/norm[ik];
				}
			fac =datn[j0t][is]/norm[j0t]*dsum; 
				        for (ix=j0t+1;ix<nx;++ix) {
			ktj1 = floor(sqrt(((ix-1)+ol-1)*((ix-1)+ol-1)-
				tau*((nx+ol-1)*(nx+ol-1)-ol*ol))-ol)+1;
			ktj2 = floor(sqrt((ix+ol-1)*(ix+ol-1)-
				tau*((nx+ol-1)*(nx+ol-1)-ol*ol))-ol);
						ik = ktj1; temp = 0;
						while (ik<=ktj2){
						temp += datn[ik][is]/norm[ik];
						++ik;
						}
						dsum = dsum + temp;
                                		nsum = datn[ix][is]/norm[ix]; 
						fac  = fac + nsum*dsum;
                                        }
					ff  = ff + fac; 
                                }
			if (ff < 0)
				ff = 0;   
			sem[itout]=ff/beta;  
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
