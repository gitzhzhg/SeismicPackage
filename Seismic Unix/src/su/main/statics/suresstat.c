/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SURESSTAT: $Revision: 1.21 $ ; $Date: 2015/04/24 16:56:22 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SURESSTAT - Surface consistent source and receiver statics calculation",
" 									",
"   suresstat fn=  [optional parameters]				",
" 									",
" Required parameters: 							",
" fn=		seismic file				",
" ssol=		output file source statics				",
" rsol=		output file receiver statics				",
" 									",
" Optional parameters:							",
" ntpick=50 	maximum static shift (samples)         			",
" niter=5 	number of iterations					",
" imax=100000 	largest shot (fldr),reciver(tracf) or cmp(cdp) number	",
" sub=0 	subtract super trace 1 from super trace 2 (=1)		",
" 		sub=0 strongly biases static to a value of 0		",
" mode=0 	use global maximum in cross-correllation window		",
"		=1 choose the peak perc=percent smaller than the global max.",
" perc=10. 	percent of global max (used only for mode=1)		",
" verbose=0 	print diagnostic output (verbose=1)                     ",
" 									",
" Notes:								",
" Estimates surface-consistent source and receiver statics, meaning that",
" there is one static correction value estimated for each shot and receiver",
" position.								",
" 									",
" The method employed here is based on the method of Ronen and Claerbout:",
" Geophysics 50, 2759-2767 (1985).					",
"  									",
" The input data are NMO-corrected and sorted into shot gathers ( fldr)  ",
" Rreceiver id position should be stored in headerword tracf	        ",
" The output files are binary files containing the source and receiver	",
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
" correction for each shot record (fldr=1 to fldr=nshot), with any missing ",
" shots being assigned a static of 0.  A static correction for each    	",
" receiver station (tracf=1 to tracf=nr) is calculated, with missing    ",
" receivers again assigned a static of 0.                               ", 
" To window out the most cohherent region use suwind tmin= tmax= and 	",
" save the result into a file. This will reduce the amount of time  	",
" the code will spent on scaning the file,since the file is much smaller",
" The ntpick parameter sets the maximum allowable shift desired (in	",
"   samples NOT time).							",
"									",
" To apply the static corrections, use sustatic with hdrs=3		",
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
 *      Modified by A. Bitri, BRGM-France, Apr. 2015
 * Trace header fields accessed: ns, dt, tracf, fldr, cdp
 */
/**************** end self doc *******************************************/


segy tr;

FILE *fp;               /* File pointer */

/* prototypes for functions defined and used below */
int max (float *trace, int mode, float perc, int nt);
void window (float *trace, int nt, int nnt, float *ntrace);

int main(int argc, char *argv[])
{
	char *fn="";            /* name of the file */
	int nt;			/* number of points on input traces*/
	float mdt;		/* sample rate in milliseconds*/
	int nt_super;		/* number of points on traces in supertrace*/
	int ntotal_super;	/* total number of points in supertrace*/
	int ntraces;		/* number of input traces*/
	int nshot, nc, nr;	/* number of shots, cmps and recs */
	int imax;                /* max number of shots or cmp or recs*/ 
	int sfold, cfold, rfold;/* source, cmp and receiver fold */
	int ntcc;		/* nr. of points on c-c in traces */
	int ntpick;		/* nr. of points on trace for picking */
	int ntout;		/* nr. of points on c-c out traces */
	int nt_r;		/* nr. of points on resamp. c-c in traces */
	int ntr;		/* number of trace on input */
	int iter, niter;	/* iteration vars */
	register int ishot, ichan, irec, icmp;	/* gather counters */
	register int itrace;    /* gather counters */
	register int it, i, j=0, k, l;	/* counters */
	int *ST;                /* temporary shot array */
	int *RT;                /* temporary receiver array */
	int *CT;                /* temporary cdp array */
	int *cmpntr;		/* cmp selector */
	int *recntr;				/* receiver gather fold */
	int *shotntr;				/* shot gather fold */
	int **cmp_loc, **rec_loc, **shot_loc;	/* position arrays */
	int **header;
	int mode;			/* pick global(=0) or local(=1) max */
	int sub;			/* subtract or not */
	int tlag;			/* time lag of c-c trace */
	int verbose;			/* diagnostic output (=1) */
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
	int s;				/* local static shift */
	cwp_String ssol, rsol;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	getparstring("fn",&fn);
	
	/* Open the file */
	fp = efopen(fn,"r");
	if ((fp = fopen(fn, "r")) == NULL)
		err("suresstat: can't open input data file");

	
	/* get the number of traces in data */

	ntraces = fgettra(fp, &tr, 0);
	


	dt = ((double) tr.dt)/1000.0;   /* dt in milliseconds (microseconds in trace header) */
	mdt = tr.dt/1000.0;
	if (!dt) getparfloat("dt", &dt);
	if (!dt) MUSTGETPARFLOAT("dt", &dt);
	nt = tr.ns;
	rewind(fp);

	/* Get optional parameters */
	if (!getparint("ntcc",&ntcc)) ntcc=250; 
	if (ntcc%2 == 0) ++ntcc;
	if (!getparint("ntpick",&ntpick)) ntpick=50; 
	if (!getparint("niter",&niter)) niter=5;

	if (!getparint("imax",&imax)) imax=100000; 

	if (!getparint("mode",&mode)) mode=0; 
	if (!getparint("sub",&sub)) sub=0; 
	if (!getparfloat("perc",&perc)) perc=10.; 
	if (!getparint("verbose",&verbose)) verbose=0; 

	if (!getparstring("ssol",&ssol))
		err("must specify a source statics output file");
	if (!getparstring("rsol",&rsol))
		err("must specify a receiver statics output file");

        checkpars();
	/* Compute time windowing parameters */
	nt_r = ntcc;
	if (verbose == 1) warn("nt_r=%i",nt_r);
	ntout = 2*ntpick + 1;
	nt_super = nt+(2*ntpick);  /*trace length plus buffer on each end*/

	/* Allocate temporary arrays */
	ST = ealloc1int(imax);
	RT = ealloc1int(imax);
	CT = ealloc1int(imax);
	
	/* Initialization*/

        sfold=0;
        cfold=0;
        rfold=0;
	nc=0;
	nr=0;
	nshot=0;

	/* Scan the datafile to define sfold, rfold,cfold,nshot,nr,nc */
		
		 for (itrace=0; itrace<ntraces; itrace++){

				fgettra(fp, &tr, itrace);
			icmp = tr.cdp;     /*location of cdp number in header*/
			irec = tr.tracf;   /*location of receiver station in header*/
			ishot = tr.fldr;   /*location of shot number in header*/

			j = ++CT[icmp];
			k = ++RT[irec];
			l = ++ST[ishot];

			if( j > cfold) cfold=j;
			if( k > rfold) rfold=k;
			if( l > sfold) sfold=l;
			if(icmp > nc) nc=icmp;
			if(irec > nr) nr=irec;
			if(ishot > nshot) nshot=ishot;
		
		} 

	rewind(fp);

	/* Free tempory arrays */

	free1int(ST);
	free1int(RT);
	free1int(CT);

	/**************/
	
	i=(nr>nshot) ? nr : nshot;
	ntotal_super=nt_super*i;
	
	/* Allocate space */

	cmpntr = alloc1int(nc+1);
	recntr = alloc1int(nr+1);
	shotntr = alloc1int(nshot+1);
	t = alloc1float(nt_r);

	cmp_loc = alloc2int(cfold+1,nc+1);
	rec_loc = alloc2int(rfold+1,nr+1);
	shot_loc = alloc2int(sfold+1,nshot+1); 
	header = alloc2int(3,ntraces+1);

	model = alloc2float(nt,nc+1);

	filter = alloc1float(ntcc+1);
	cc_tr = alloc1float(nt_r+1);
	filter_r = alloc1float(nt_r+1);
	cc_tr_r = alloc1float(ntcc+1);
	corr_trace = alloc1float(ntout);
	pick_tr = alloc1float(ntpick+1);

	model_trace = alloc1float(ntotal_super);
	g_trace = alloc1float(ntotal_super);
	data = alloc2float(nt,ntraces+1);

	tsstat = alloc1float(nshot+1);
	trstat = alloc1float(nr+1);
	sstat = alloc1int(nshot+1);
	rstat = alloc1int(nr+1);

	/* Zero out arrays */
	memset((void *) tsstat, 0 , (nshot+1)*FSIZE);
	memset((void *) sstat, 0 , (nshot+1)*FSIZE);
	memset((void *) trstat, 0 , (nr+1)*FSIZE);
	memset((void *) rstat, 0 , (nr+1)*FSIZE);
	memset((void *) data[0], 0 , (nr+1)*FSIZE);
	memset((void *) cmpntr, 0 , (nc+1)*ISIZE);
	memset((void *) recntr, 0 , (nr+1)*ISIZE);
	memset((void *) shotntr, 0 , (nshot+1)*ISIZE);

	/**********************************************/	

	 if (verbose == 1) warn(" cfold=%d,rfold=%d,sfold=%d\n",cfold,rfold,sfold);



	fprintf(stderr," Number of traces= %10d\n",ntraces);
	fprintf(stderr," Number of shots= %10d\n",nshot);
	fprintf(stderr," Number of receivers= %10d\n",nr);
	fprintf(stderr," Number of cdps= %10d\n",nc);


		

	/* scan the datafile and load info into the table */

	for (itrace=0; itrace<ntraces; itrace++){
		
			/* if(itrace>0) gettra(&tr,itrace);*/
			fgettra(fp, &tr, itrace);

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

		if (verbose == 1) fprintf(stderr,"iteration #= %i\n", iter);

		/* construct CMP stack */
		if (verbose == 1) fprintf(stderr,"constructing CMP stack\n");
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
		for (ishot=1; ishot<=nshot; ishot++){
		if (verbose == 1) fprintf(stderr,"\nstarting shot loop, shot=%i\n", ishot);
		if (shotntr[ishot] > 1) {

			/* construct shot and cmp super traces */
			if (verbose == 1) fprintf(stderr,"creating supertraces\n");
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
			if (sub == 1 && verbose == 1) fprintf(stderr,"subtracting supertraces\n");
			if (sub == 1) for (it=1; it<=ntotal_super; it++) 
                                        model_trace[it] = model_trace[it] - g_trace[it];


			/* cross-correlate super trace 1 with super trace 2 */
			if (verbose == 1) fprintf(stderr,"crosscorrelating supertraces\n");

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
			if (verbose == 1) fprintf(stderr,"finished shot %i, sstat=%i, tsstat=%f (samples)\n", ishot,sstat[ishot],tsstat[ishot]);

		}
		}

		/* end shot statics loop */
		/* correct traces for shot statics (use g_trace as temp trace) */ 
		for (i=1; i<=ntraces; i++){
			ishot=header[i][3];
			for (it=1; it<=nt; it++) g_trace[it] = 0.0;
			s = sstat[ishot];
			for (it=1-(s>0?0:s); it<=nt-(s>0?s:0); it++) g_trace[it] = data[i][it+s];
			for (it=1; it<=nt; it++) data[i][it] = g_trace[it];
			}

		/* construct CMP stack of corrected traces */
		if (verbose == 1) fprintf(stderr,"constructing new CMP stack using shot-corrected traces\n");
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
		if (verbose == 1) fprintf(stderr,"\n\nstarting reciever loop\n");
		for (irec=1; irec<=nr; irec++){
                if (recntr[irec] > 1) {

                        /* construct receiver and cmp super traces */
                        if (verbose == 1) fprintf(stderr,"creating supertraces\n");
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
                        if (sub == 1 && verbose == 1) fprintf(stderr,"subtracting supertraces\n");
                        if (sub == 1) for (it=1; it<=ntotal_super; it++) 
                                        model_trace[it] = model_trace[it] - g_trace[it];


                        /* cross-correlate super trace 1 with super trace 2 */
                        if (verbose == 1) fprintf(stderr,"crosscorrelating supertraces\n");

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
                        if (verbose == 1) fprintf(stderr,"finished receiver %i, rstat=%i, trstat=%f (samples)\n", irec,rstat[irec],trstat[irec]);

	if (verbose == 1) fprintf(stderr,"finished one receiver\n");

              }
                }

                /* end receiver statics loop */

                /* correct traces for receiver statics (use g_trace as temp trace) */
                for (i=1; i<=ntraces; i++){
                        irec=header[i][2];
                        for (it=1; it<=nt; it++) g_trace[it] = 0.0;
			s = rstat[irec];
			for (it=1-(s>0?0:s); it<=nt-(s>0?s:0); it++) g_trace[it] = data[i][it+s];
                        for (it=1; it<=nt; it++) data[i][it] = g_trace[it];
                        }

                /* construct CMP stack of corrected traces */
		if(iter<niter){
                if (verbose == 1) fprintf(stderr,"constructing new CMP stack using shot-corrected traces\n");
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
	for (it=1; it<=nshot; it++) tsstat[it] = tsstat[it]*mdt;
        fps    = efopen(ssol,"wb");
	efwrite(tsstat,sizeof(float),nshot+1,fps);
        efclose(fps);

	for (it=1; it<=nr; it++) trstat[it] = trstat[it]*mdt;
        fpr    = efopen(rsol,"wb");
	efwrite(trstat,sizeof(float),nr+1,fpr);
        efclose(fpr);

	return(CWP_Exit());
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
