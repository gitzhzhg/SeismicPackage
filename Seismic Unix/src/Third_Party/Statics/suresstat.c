/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/* SURESSTAT: $Revision: 1.10 $ ; $Date: 1998/01/15 22:55:39 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SURESSTAT - Surface consistent source and receiver statics calculation",
" 									",
"   suresstat <stdin [optional parameters]				",
" 									",
" Required parameters: 							",
" ssol=		output file source statics				",
" rsol=		output file receiver statics				",
" 									",
" Optional parameters:							",
" ntraces	number of traces in input data set (must be correct!)	",
" ntpick=50 	maximum static shift (samples)         			",
" niter=5 	number of iterations					",
" ns=240 	largest shot number (fldr=1 to ns)			",
" nr=335 	largest receiver number (tracf=1 to nr)			",
" nc=574 	maximum number of cmp's (for array allocation)		",
" sfold=96 	maximum shot gather fold				",
" rfold=96 	maximum receiver gather fold				",
" cfold=48 	maximum cmp gather fold					",
" sub=0 	subtract super trace 1 from super trace 2 (=1)		",
" 		sub=0 strongly biases static to a value of 0		",
" mode=0 	use global maximum in cross-correllation window		",
"		=1 choose the peak perc=percent smaller than the global max.",
" perc=10. 	percent of global max (used only for mode=1)		",
" 									",
" Notes:								",
" Estimates surface-consistent source and receiver statics, meaning that",
" there is one static correction value estimated for each shot and receiver",
" position.								",
" 									",
" The method employed here is based on the method of Ronen and Claerbout:",
" Geophysics 50, 2759-2767 (1985).					",
"  									",
" The input consists of moveout-corrected SU data sorted in shot gathers.",
" The output files are ascii files containing the source and receiver	",
" statics, as a function of shot number (trace header fldr) and      	",
" receiver station number (trace header tracf). 			",
"  									",
" The code builds a supertrace1 and supertrace2, which are subsequently	",
" cross-correllated. The program then picks the time lag associated with",
" the largest peak in the cross-correllation according to two possible	",
" criteria set by the parameter \"mode\". If mode=0, the maximum of the	",
" cross-correllation window is chosen. If mode=1, the program will pick ",
" a peak which is up to perc=percent smaller than the global maximum, but",
" closer to zero lag than the global maximum.	(Choosing mode=0 is	",
" recommended.)								",
"  									",
" The geometry can be irregular: the program simply computes a static 	",
" correction for each shot record (fldr=1 to fldr=ns), with any missing ",
" shots being assigned a static of 0.  A static correction for each    	",
" receiver station (tracf=1 to tracf=nr) is calculated, with missing    ",
" receivers again assigned a static of 0.                               ", 
"									",
" to apply the static corrections, use sustatic with hdrs=3		",
NULL};

/* Reference:
 *
 *  Ronen, J. and Claerbout, J., 1985, Surface-consistent residual statics
 *      estimation  by stack-power maximization: Geophysics, vol. 50,
 *      2759-2767.
 *
 * Credits:
 *	CWP: Timo Tjan, 4 October 1994
 *
 *      rewritten by Thomas Pratt, USGS, Feb. 2000.
 *
 * Trace header fields accessed: ns, dt, tracf, fldr, cdp
 */
/**************** end self doc *******************************************/


segy tr, tr2;

/* prototypes for functions defined and used below */
int max (float *trace, int mode, float perc, int nt);
void window (float *trace, int nt, int nnt, float *ntrace);

int
main(int argc, char **argv)
{
	int nt;			/* number of points on input traces*/
	int mdt;		/* sample rate in milliseconds*/
	int nt_super;		/* number of points on traces in supertrace*/
	int ntotal_super;	/* total number of points in supertrace*/
	int ntraces;		/* number of input traces*/
	int ns, nc, nr;		/* number of shots, cmps and recs */
	int sfold, cfold, rfold;/* source, cmp and receiver fold */
	int n_o;		/* near offset */
	int ntcc;		/* nr. of points on c-c in traces */
	int mint, maxt;		/* min. and max time in c-c */
	int ntpick;		/* nr. of points on trace for picking */
	int ntout;		/* nr. of points on c-c out traces */
	int nt_r;		/* nr. of points on resamp. c-c in traces */
	int resamp;		/* resampling rate */
	int ntr;		/* number of trace on input */
	int iter, niter;	/* iteration vars */
	register int ishot, ichan, irec, icmp;	/* gather counters */
	register int itrace;    /* gather counters */
	int icmpshift;			/* shift applied to icmp */
	register int it, itr, i, j=0, k, l;	/* counters */
	int *cmpntr, cmp_max, cmp_min;		/* cmp selector */
	int *recntr;				/* receiver gather fold */
	int *shotntr;				/* shot gather fold */
	int **cmp_loc, **rec_loc, **shot_loc;	/* position arrays */
	int **header;
	int mode;			/* pick global(=0) or local(=1) max */
	int sub;			/* subtract or not */
	int tlag;			/* time lag of c-c trace */
	int *sstat, *rstat;		/* shot and receiver static */
	float *tsstat, *trstat;		/* total shot and receiver static */
	float perc;			/* local max. witin perc. of global */
	float dt;			/* sampling rate */
	float **data, **model;		/* data arrays */
	float *g_trace;			/* trace arrays */
	float *t;			/* trace arrays */
	float *model_trace, *corr_trace;/* more trace arrays */
	float *filter, *cc_tr, *pick_tr;/* more trace arrays */
	float *filter_r, *cc_tr_r;	/* more trace arrays */
	FILE *fps, *fpr;		/* file pointers for output */
	cwp_String ssol, rsol;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get info from first trace and store first header */
	if (!(ntr = gettra(&tr, 0)))  err("can't get first trace");
	dt = ((double) tr.dt)/1000.0;   /* dt in milliseconds (microseconds in trace header */
	mdt = tr.dt/1000;
	if (!dt) getparfloat("dt", &dt);
	if (!dt) MUSTGETPARFLOAT("dt", &dt);
	nt = tr.ns;
	/* if (nt%2 == 0) err("nt must be odd"); */

	/* Get optional parameters */
	if (!getparint("ntcc",&ntcc)) ntcc=250; 
	if (!getparint("icmpshift",&icmpshift)) icmpshift=9; 
	if (ntcc%2 == 0) ++ntcc;
	if (!getparint("ntpick",&ntpick)) ntpick=50; 
	if (!getparint("ntraces",&ntraces)) ntraces=50; 
	if (!getparint("resamp",&resamp)) resamp=4; 
	if (!getparint("n_o",&n_o)) n_o=7; 
	if (!getparint("niter",&niter)) niter=5;
	if (!getparint("ns",&ns)) ns=240; 
	if (!getparint("nr",&nr)) nr=335; 
	if (!getparint("nc",&nc)) nc=574; 
	if (!getparint("sfold",&sfold)) sfold=96; 
	if (!getparint("rfold",&rfold)) rfold=96; 
	if (!getparint("cfold",&cfold)) cfold=48; 
	if (!getparint("mode",&mode)) mode=0; 
	if (!getparint("sub",&sub)) sub=0; 
	if (!getparfloat("perc",&perc)) perc=10.; 

	if (!getparstring("ssol",&ssol))
		err("must specify a source statics output file");
	if (!getparstring("rsol",&rsol))
		err("must specify a receiver statics output file");

	/* Compute time windowing parameters */
	nt_r = ntcc*resamp;
	fprintf(stderr,"nt_r=%i\n",nt_r);
	ntout = 2*ntpick + 1;
	nt_super = nt+(2*ntpick);  /*trace length plus buffer on each end*/
	i=(nr>ns) ? nr : ns;
	ntotal_super=nt_super*i;
	fprintf(stderr,"nt=%i, nt_super=%i, ntotal_super=%i\n",nt, nt_super, ntotal_super);

	/* Allocate space */
	cmpntr = alloc1int(nc+1);
	recntr = alloc1int(nr+1);
	shotntr = alloc1int(ns+1);
	g_trace = alloc1float(ntotal_super);
	t = alloc1float(nt_r);

	fprintf(stderr,"nr=%i rfold=%i\n",nr,rfold);
	cmp_loc = alloc2int(cfold+1,nc+1);
	rec_loc = alloc2int(rfold+1,nr+1);
	shot_loc = alloc2int(sfold+1,ns+1);
	header = alloc2int(3,ntraces+1);

	model = alloc2float(nt,nc+1);
	model_trace = alloc1float(ntotal_super);

	filter = alloc1float(ntcc+1);
	cc_tr = alloc1float(nt_r+1);
	filter_r = alloc1float(nt_r+1);
	cc_tr_r = alloc1float(ntcc+1);
	corr_trace = alloc1float(ntout);
	pick_tr = alloc1float(ntpick+1);

	data = alloc2float(nt,ntraces+1);

	tsstat = alloc1float(ns+1);
	trstat = alloc1float(nr+1);
	sstat = alloc1int(ns+1);
	rstat = alloc1int(nr+1);

	/* Zero out arrays */
	memset((void *) tsstat, (float) '\0' , (ns+1)*FSIZE);
	memset((void *) sstat, (int) '\0' , (ns+1)*FSIZE);
	memset((void *) trstat, (float) '\0' , (nr+1)*FSIZE);
	memset((void *) rstat, (int) '\0' , (nr+1)*FSIZE);
	memset((void *) data[0], (int) '\0' , (nr+1)*FSIZE);
	memset((void *) cmpntr, (int) '\0' , (nc+1)*ISIZE);
	memset((void *) recntr, (int) '\0' , (nr+1)*ISIZE);
	memset((void *) shotntr, (int) '\0' , (ns+1)*ISIZE);

	/* Read rest of data */
	for (itrace=0; itrace<ntraces; itrace++){

	/* we already read the first trace, so just put it into the arrays */
			if(itrace>0) gettra(&tr,itrace);

			icmp = tr.cdp;     /*location of cdp number in header*/
			irec = tr.tracf;   /*location of receiver station in header*/
			ishot = tr.fldr;   /*location of shot number in header*/

			j = ++cmpntr[icmp];
			k = ++recntr[irec];
			l = ++shotntr[ishot];

			cmp_loc[icmp][j] = itrace+1;
			rec_loc[irec][k] = itrace+1;
			shot_loc[ishot][l] = itrace+1;
			header[itrace+1][1]=icmp;
			header[itrace+1][2]=irec;
			header[itrace+1][3]=ishot;

			for (it=1; it<=nt; it++)
				data[itrace+1][it] = tr.data[it];
	}

	/* start iterations */
	for (iter=1; iter<=niter; iter++) {

		fprintf(stderr,"iteration #= %i\n", iter);

		/* construct CMP stack */
		fprintf(stderr,"constructing CMP stack\n");
		for (i=1; i<=nc; i++){

			for (it=1; it<=nt; it++) model[i][it] = 0.;

			for (j=1; j<=cmpntr[i]; j++)
				for (it=1; it<=nt; it++)
					model[i][it] += data[cmp_loc[i][j]][it];

/* normalize - no, I cannot normalize the individual traces in the supertrace */
			/* j=(1>cmpntr[i]) ? 1 : cmpntr[i];	*/
			/* for (it=1; it<=nt; it++)		*/
			/*	model[i][it] = model[i][it]/j;	*/
		}


		/* Loop over shots */
		for (ishot=1; ishot<=ns; ishot++){
		fprintf(stderr,"\nstarting shot loop, shot=%i\n", ishot);
		if (shotntr[ishot] > 1) {

			/* construct shot and cmp super traces */
			fprintf(stderr,"creating supertraces\n");
			for (it=1; it<=ntotal_super; it++) g_trace[it] = 0.;
			for (it=1; it<=ntotal_super; it++) model_trace[it] = 0.;

			for (ichan=1; ichan<=shotntr[ishot]; ichan++){
				j = (ichan-1)*nt_super + ntpick;
				l = header[shot_loc[ishot][ichan]][1];
				/* j=location in supertrace; l=cdp number */
					for (it=1; it<=nt; it++){
						g_trace[j+it] = data[shot_loc[ishot][ichan]][it];
						model_trace[j+it] = model[l][it];
				}
                        }

			/* subtract shot supertrace from cmp supertrace if desired */
			if (sub == 1) fprintf(stderr,"subtracting supertraces\n");
			if (sub == 1) for (it=1; it<=ntotal_super; it++) 
                                        model_trace[it] = model_trace[it] - g_trace[it];


			/* cross-correlate super trace 1 with super trace 2 */
			fprintf(stderr,"crosscorrelating supertraces\n");

			for (i=1; i<=ntout; i++){
				corr_trace[i] = 0.0;
				tlag=i-ntpick-1;
				for (j=ntpick; j<=(ntotal_super-ntpick); j++){
					corr_trace[i] = corr_trace[i] + g_trace[j+tlag]*model_trace[j];
				}
			}

			/* for (i=1; i<=ntout; i++) fprintf(stderr,"i=%i, corr_trace(i)=%f\n",i,corr_trace[i]); */



			/* pick cross-correlation peak */ 
			/*window(corr_trace, ntout, ntpick, pick_tr);*/
			tlag = max(corr_trace, mode, perc, ntout);

			/* remember initial estimation and total correction */
			sstat[ishot] = tlag;
			tsstat[ishot] += tlag;
			fprintf(stderr,"finished shot %i, sstat=%i, tsstat=%f (samples)\n", ishot,sstat[ishot],tsstat[ishot]);

		}
		}

		/* end shot statics loop */

		/* correct traces for shot statics (use g_trace as temp trace) */ 
		for (i=1; i<=ntraces; i++){
			ishot=header[i][3];
			for (it=1; it<=nt; it++) g_trace[it] = 0.0;
			if(sstat[ishot] > 0) {
				for (it=1; it<=(nt-sstat[nt]+1); it++) g_trace[it] = data[i][it+sstat[ishot]];
				}
			if(sstat[ishot] < 0) {
				for (it=(sstat[ishot]+1); it<=nt; it++) g_trace[it-sstat[ishot]] = data[i][it];
				}
			for (it=1; it<=nt; it++) data[i][it] = g_trace[it];
			}

		/* construct CMP stack of corrected traces */
		fprintf(stderr,"constructing new CMP stack using shot-corrected traces\n");
		for (i=1; i<=nc; i++){

			for (it=1; it<=nt; it++) model[i][it] = 0.;

			for (j=1; j<=cmpntr[i]; j++)
				for (it=1; it<=nt; it++)
					model[i][it] += data[cmp_loc[i][j]][it];

	    		/* normalize - no, I cannot normalize the individual traces in the supertrace */
			/* j=(1>cmpntr[i]) ? 1 : cmpntr[i];	*/
			/* for (it=1; it<=nt; it++)		*/
			/*	model[i][it] = model[i][it]/j;	*/
		}



		/* Loop over receivers */
		fprintf(stderr,"\n\nstarting reciever loop\n");
		for (irec=1; irec<=nr; irec++){
                if (recntr[irec] > 1) {

                        /* construct receiver and cmp super traces */
                        fprintf(stderr,"creating supertraces\n");
                        for (it=1; it<=ntotal_super; it++) g_trace[it] = 0.;
                        for (it=1; it<=ntotal_super; it++) model_trace[it] = 0.;

                        for (ichan=1; ichan<=recntr[irec]; ichan++){
                                /* j=location in supertrace; l=cdp number */
                                j = (ichan-1)*nt_super + ntpick;
                                l = header[rec_loc[irec][ichan]][1];
                                        for (it=1; it<=nt; it++){
                                                g_trace[j+it] = data[rec_loc[irec][ichan]][it];
                                                model_trace[j+it] = model[l][it];
                                }
                        }
                        /* for (i=1; i<=nt; i++) fprintf(stderr,"i=%i, model_trace(i)=%f\n",i,model_trace[i]); */
                        /* for (i=1; i<=nt; i++) fprintf(stderr,"i=%i, g_trace(i)=%f\n",i,g_trace[i]); */


                        /* subtract receiver supertrace from cmp supertrace if desired */
                        if (sub == 1) fprintf(stderr,"subtracting supertraces\n");
                        if (sub == 1) for (it=1; it<=ntotal_super; it++) 
                                        model_trace[it] = model_trace[it] - g_trace[it];


                        /* cross-correlate super trace 1 with super trace 2 */
                        fprintf(stderr,"crosscorrelating supertraces\n");

                        for (i=1; i<=ntout; i++){
                                corr_trace[i] = 0.0;
                                tlag=i-ntpick-1;
                                for (j=ntpick; j<=(ntotal_super-ntpick); j++)
					corr_trace[i] = corr_trace[i] + g_trace[j+tlag]*model_trace[j];
                        }

                        /* for (i=1; i<=ntout; i++) fprintf(stderr,"i=%i, corr_trace(i)=%f\n",i,corr_trace[i]); */



                        /* pick cross-correlation peak */
                        /*window(corr_trace, ntout, ntpick, pick_tr);*/
                        tlag = max(corr_trace, mode, perc, ntout);

                        /* remember initial estimation and total correction */
                        rstat[irec] = tlag;
                        trstat[irec] += tlag;
                        fprintf(stderr,"finished receiver %i, rstat=%i, trstat=%f (samples)\n", irec,rstat[irec],trstat[irec]);

	fprintf(stderr,"finished one receiver\n");

              }
                }

                /* end receiver statics loop */

                /* correct traces for receiver statics (use g_trace as temp trace) */
                for (i=1; i<=ntraces; i++){
                        irec=header[i][2];
                        for (it=1; it<=nt; it++) g_trace[it] = 0.0;
                        if(rstat[irec] > 0) {
                                for (it=1; it<=(nt-rstat[nt]+1); it++) g_trace[it] = data[i][it+rstat[irec]];
                                }
                        if(rstat[irec] < 0) {
                                for (it=(rstat[irec]+1); it<=nt; it++) g_trace[it-rstat[irec]] = data[i][it];
                                }
                        for (it=1; it<=nt; it++) data[i][it] = g_trace[it];
                        }

                /* construct CMP stack of corrected traces */
		if(iter<niter){
                fprintf(stderr,"constructing new CMP stack using shot-corrected traces\n");
                for (i=1; i<=nc; i++){

                        for (it=1; it<=nt; it++) model[i][it] = 0.;

                        for (j=1; j<=cmpntr[i]; j++)
                                for (it=1; it<=nt; it++)
                                        model[i][it] += data[cmp_loc[i][j]][it];

                        /* normalize - no, I cannot normalize the individual traces in the supertrace */
                        /* j=(1>cmpntr[i]) ? 1 : cmpntr[i];     */
                        /* for (it=1; it<=nt; it++)             */
                        /*      model[i][it] = model[i][it]/j;  */
		}
                }
	}
	/* end iterations */

	/* output final statics */ 
	for (it=1; it<=ns; it++) tsstat[it] = tsstat[it]*mdt;
        fps    = efopen(ssol,"wb");
	efwrite(tsstat,sizeof(float),ns,fps);
        efclose(fps);

	for (it=1; it<=nr; it++) tsstat[it] = tsstat[it]*mdt;
        fpr    = efopen(rsol,"wb");
	efwrite(trstat,sizeof(float),nr,fpr);
        efclose(fpr);

	return EXIT_SUCCESS;
}

void window (float *trace, int nt, int nnt, float *ntrace)
{
	int j;	
	int ft;	
	
	if (nnt%2 == 0) ++nnt;
	ft = (nt - 1)/2 - (nnt - 1)/2;
	for (j = 0; j < nnt; j++) {
		ntrace[j] = trace[j+ft];
	}
}

int max (float *trace, int mode, float perc, int nt)
{
	float maxamp = 0.;	/* max. amplitude sample rate */
	int globmax = 0;	/* sample of global max value */
	int locmax = 0;		/* sample of local max value */
	int zero; 		/* zero-lag sample */
	int lag = 0; 		/* lag picked */
	int j;			/* counter */

	/* Set parameters */
	if (nt%2 == 0) ++nt;
	zero = (nt+1)/2;
	globmax = (nt+1)/2;
	perc = 1.0-(perc/100);

	/* determine global max for each trace*/
	for (j = 1; j <= nt; j++) {
		if (trace[j] > maxamp) {
			maxamp = trace[j];
			globmax = j;
		}
	}
	if (mode == 1) {
	/* determine max within perc% of global max, but closer to zero lag*/
		for (j = 1; j <= nt; j++) {
			if ((trace[j] > perc*maxamp) && 
			  (ABS(j-zero) < ABS(locmax-zero))) 
				locmax = j;
		}
	}

	/* Output the result */
	switch(mode) {
		case 0:
			lag = globmax - zero;
			break;
		case 1:
			lag = locmax - zero;
			break;
	}
	return lag;
}
