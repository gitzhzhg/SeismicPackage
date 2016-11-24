/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPEF: $Revision: 1.47 $ ; $Date: 2014/12/04 23:10:17 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUPEF - Wiener (least squares) predictive error filtering		",
" 									",
" supef <stdin >stdout  [optional parameters]				",
" 									",
" Required parameters:							",
" dt is mandatory if not set in header			 		",
" 									",
" Optional parameters:							",
" cdp= 			CDPs for which minlag, maxlag, pnoise, mincorr, ",
"			maxcorr are set	(see Notes)			",
" minlag=dt		first lag of prediction filter (sec)		",
" maxlag=last		lag default is (tmax-tmin)/20			",
" pnoise=0.001		relative additive noise level			",
" mincorr=tmin		start of autocorrelation window (sec)		",
" maxcorr=tmax		end of autocorrelation window (sec)		",
" wienerout=0		=1 to show Wiener filter on each trace		",
" mix=1,...	 	array of weights (floats) for moving		",
"				average of the autocorrelations		",
" outpar=/dev/null	output parameter file, contains the Wiener filter",
" 			if wienerout=1 is set				",
" method=linear	 for linear interpolation of cdp values			",
"		       =mono for monotonic cubic interpolation of cdps	",
"		       =akima for Akima's cubic interpolation of cdps	",
"		       =spline for cubic spline interpolation of cdps	",
" 									",
" Trace header fields accessed: ns, dt					",
" Trace header fields modified: none					",
" 									",
" Notes:								",
" 									",
" 1) To apply spiking decon (Wiener filtering with no gap):		",
"									",
" Run the following command						",
"									",
"    suacor < data.su | suximage perc=95				",
"									",
" You will see horizontal stripe running across the center of your plot.",
" This is the autocorrelation wavelet for each trace. The idea of spiking",
" decon is to apply a Wiener filter with no gap to the data to collapse	",
" the waveform into a spike. The idea is to pick the width of the	",
" autocorrelation waveform _from beginning to end_ (not trough to trough)",
" and use this time for MAXLAG_SPIKING:					",
"									",
"  supef < data.su maxlag=MAXLAG_SPIKING  > dataspiked.su		",
"									",
" 2) Prediction Error Filter (i.e. gapped Wiener filtering)		",
" The purpose of gapped decon is to suppress repetitions in the data	",
" such as those caused by water bottom multiples.			",
"									",
" To look for the period of the repetitions				",
" 									",
"    suacor ntout=1000 < dataspiked.su | suximage perc=95		",
" or 									",
"    suacor ntout=1000 < dataspiked.su | sustack key=dt |suxwigb	",
" 									",
" The value of ntout must be larger than the default 100. The idea is	",
" to look for repetitions in the autocorrelation. These repetitions will",
" appear as a family of parallel stripes above and below the main	",
" autocorrelation waveform. Or, if you stack the data, these will be	",
" repetitive spikes.  This repetition time is the GAP. We set 		",
" MINLAG_PEF to the value of this repetition time.			",
" 									",
" We set the minlag to MINLAG_PEF = GAP					",
"									",
" We set the maxlag to MAXLAG_PEF = GAP + MAXLAG_SPIKING		",
" 									",
"  supef < dataspiked.su minlag=MINLAG_PEF maxlag=MAXLAG_PEF > datapef.su",
" 									",
" Some experimentation may be required to get a satisfactory result.	",
" In particular you may find that you need to reduce the value of the   ",
" minlag 								",
" 									",
" 3) It may be effective to sort your data into cdp gathers with susort,",
" and perform sunmo correction to the water speed with sunmo, prior to 	",
" attempts to suppress water bottom multiples. After applying supef, the",
" user should apply inverse nmo to undo the nmo to water speed prior to	",
" further processing. Or, do the predictive decon on fully nmo-corrected",
" gathers.								",
"									",
" If you flatten your data with sunmo, then make sure that you turn off ",
" the stretch mute by using smute=20					",
"									",
"  | sunmo vnmo=v1,v2,... tnmo=t1,t2,... smute=20 | supef ...		",
"									",
" For a filter expressed as a function of cdp, specify the array	",
"     cdp=cdp1,cdp2,...							",
" and for each cdp specified, specify the minlag and maxlag arrays as	",
"      minlag=min1,min2,...     maxlag=max1,max2,...   			",
"  									",
" It is required that the number of minlag and maxlag values be equal to",
" the number of cdp's specified.  If the number of			",
" values in these arrays does not equal the number of cdp's, only the first",
" value will be used.							",
"									",
" Caveat:								",
" The wienerout=1 option writes out the wiener filter to outpar, and   ",
" the prediction error filter to stdout, which is			", 
"     1,0,0,...,-wiener[0],...,-wiener[imaxlag-1] 			",
" where the sample value of -wiener[0], is  iminlag in the pe-filter.	",
" The pe-filter is output as a SU format datafile, one pe-filter for each",
" trace input.								",
"	...| supef ... wienerout | suxwigb				",
" shows the prediction error filters					",
 NULL};

/* Credits:
 *	CWP: Shuki Ronen, Jack K. Cohen, Ken Larner
 *      CWP: John Stockwell, added mixing feature (April 1998)
 *      CSM: Tanya Slota (September 2005) added cdp feature
 *
 *      Technical Reference:
 *	A. Ziolkowski, "Deconvolution", for value of maxlag default:
 *		page 91: imaxlag < nt/10.  I took nt/20.
 *
 * Notes:
 *	The prediction error filter is 1,0,0...,0,-wiener[0], ...,
 *	so no point in explicitly forming it.
 *
 *	If imaxlag < 2*iminlag - 1, then we don't need to compute the
 *	autocorrelation for lags:
 *		imaxlag-iminlag+1, ..., iminlag-1
 *	It doesn't seem worth the duplicated code to implement this.
 *
 * Trace header fields accessed: ns
 */
/**************** end self doc *******************************************/


/* External definitions */
#define PNOISE	0.001			/* default pnoise value		*/
#define VAL0    1.0			/* default weighting value	*/
#define OUTPAR_DEFAULT "/dev/null"	/* default output filename	*/




segy intrace, outtrace;

int
main(int argc, char **argv)
{
	int nt;			/* number of points on trace		*/
	int i,ilag;		/* counters				*/
	int ncdp;		/* number of cdp's specified		*/
	int nminlag;		/* number of minlags specified		*/
	int nmaxlag;		/* number of maxlag specified		*/
	int icdp=0;		/* counter of cdp's			*/
	int jcdp=0;		/* counter of cdp's			*/

	float *cdp=NULL;	/* array[ncdp] of cdps specified 	*/
	float dt;		/* time sample interval (sec)		*/
	float *wiener=NULL;	/* Wiener error filter coefficients	*/
	float *spiker=NULL;	/* spiking decon filter			*/

	float pnoise;		/* pef additive noise level		*/

	float *minlag=NULL;	/* start of error filter (sec)		*/
	int *iminlag=NULL;	/* ... in samples			*/
	float *maxlag=NULL;	/* end of error filter (sec)		*/
	int *imaxlag=NULL;	/* ... in samples			*/
	int nlag;		/* length of error filter in samples	*/
	int ncorr;		/* length of corr window in samples	*/
	int lcorr;		/* length of autocorr in samples	*/

	long oldcdp;		/* cdp of previous trace */

	float *crosscorr=NULL;	/* right hand side of Wiener eqs	*/
	float *autocorr=NULL;	/* vector of autocorrelations		*/

	float mincorr;		/* start time of correlation window	*/
	int imincorr;		/* .. in samples			*/
	float maxcorr;		/* end time of correlation window	*/
	int imaxcorr;		/* .. in samples			*/
	int wienerout;		/* flag to display pred. error filter	*/
	
	size_t lagbytes;	/* bytes in wiener and spiker filters	*/
	size_t maxlagbytes;	/* bytes in autocorrelation		*/
	
	int imix;		/* mixing counter			*/
	int nmix;		/* number of traces to average over	*/
	size_t mixbytes;	/* number of bytes = maxlagbytes*nmix	*/ 
	float *mix=NULL;	/* array of averaging weights		*/
	float **mixacorr=NULL;	/* mixing array				*/
	float *temp=NULL;	/* temporary array			*/

	/* Interpolation */
	float (*zind)[4]=NULL;	/* array of interpolation coefficients  */
	char  *method="linear";	/* interpolation method			*/
	float *cdpinterp=NULL;	/* interpolated cdps			*/
	float *ominlag=NULL;	/* interpolated minlags			*/ 
	float *omaxlag=NULL;	/* interpolated maxlags			*/

        char *outpar=NULL;	/* name of file holding output parfile	*/
        FILE *outparfp=NULL;	/* ... its file pointer			*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);



	/* Get info from first trace */ 
	if (!gettr(&intrace)) err("can't get first trace");
	nt = intrace.ns;
	dt = ((double) intrace.dt)/1000000.0;
	if (!dt) MUSTGETPARFLOAT ("dt", &dt); 

	/* Get parameters and do set up */
	if (!getparstring("outpar", &outpar))	outpar = OUTPAR_DEFAULT;
	outparfp = efopen(outpar, "w");


	/* Count parameters and allocate space */
	ncdp = countparval("cdp");
	if (ncdp>0) {
		if ((nminlag = countparval("minlag"))!=0) {
			if (!(nminlag==ncdp)) 
			   err("number of cdp values must equal number of minlag values");
		} else {
			err("cdp set, but minlag not getparred!");
		}

		if ((nmaxlag = countparval("maxlag"))!=0) {
			if (!(nmaxlag==ncdp)) 
			   err("number of cdp values must equal number of maxlag values");
		} else {
			err("cdp set, but maxlag not getparred!");
		}

		/* allocate space */
		cdp = ealloc1float(ncdp);
		minlag = ealloc1float(ncdp);
		maxlag = ealloc1float(ncdp);
		
		/* get parameters */
		getparfloat("minlag",minlag);
		getparfloat("maxlag",maxlag);
		getparfloat("cdp",cdp);

		/* check cdp values */
		for (icdp = 0; icdp < ncdp-1; ++icdp)  {
			if(cdp[icdp] > cdp[icdp+1])
				err("cdp values must increase monotonically!");
		}


		iminlag = ealloc1int(ncdp);
		imaxlag = ealloc1int(ncdp);

		for (icdp = 0; icdp < ncdp; ++icdp) {
			iminlag[icdp] = NINT(minlag[icdp]/dt);	
			imaxlag[icdp] = NINT(maxlag[icdp]/dt);
			if (iminlag[icdp] < 1) 
				err("minlag[%d]=%g too small",
					icdp, minlag[icdp]);
			if (imaxlag[icdp] >= nt)
				err("maxlag[%d]=%g too large",
					icdp, maxlag[icdp]);
			if (iminlag >= imaxlag)
				err("minlag[%d]=%g, maxlag[%d]=%g",
					icdp, minlag[icdp], icdp, maxlag[icdp]);
		}

	} else {
		ncdp = 1;
		cdp = ealloc1float(ncdp);
		minlag = ealloc1float(ncdp);
		maxlag = ealloc1float(ncdp);
		iminlag = ealloc1int(ncdp);
		imaxlag = ealloc1int(ncdp);

		/* zero out array values */
		memset( (void *) minlag , 0, ncdp * FSIZE);
		memset( (void *) maxlag , 0, ncdp * FSIZE);
		memset( (void *) imaxlag , 0, ncdp * ISIZE);
		memset( (void *) iminlag , 0, ncdp * ISIZE);


		cdp[0] = intrace.cdp;

		if (getparfloat("minlag", minlag)) {
			iminlag[0] = NINT(minlag[0]/dt);
		} else {
			iminlag[0] = 1;
		}

		if (getparfloat("maxlag", maxlag)) {
			imaxlag[0] = NINT(maxlag[0]/dt);
		} else {
			imaxlag[0] = NINT(0.05 * nt);
		}

	}

		
	/* Get parameters  */
	if (!getparint("wienerout",  &wienerout))	wienerout = 0;
	if (!getparfloat("pnoise",  &pnoise))	pnoise = PNOISE;
	
	/* .. mincorr and maxcorr */
	if (getparfloat("mincorr", &mincorr))	imincorr = NINT(mincorr/dt);
	else					imincorr = 0;
	if (imincorr < 0) err("mincorr=%g too small", mincorr);
	
	if (getparfloat("maxcorr", &maxcorr))	imaxcorr = NINT(maxcorr/dt);
	else					imaxcorr = nt;
	if (imaxcorr > nt) err("maxcorr=%g too large", maxcorr);

	if (imincorr >= imaxcorr)
		err("mincorr=%g, maxcorr=%g", mincorr, maxcorr);
	
	/*... Get mix weighting values values */
	if ((nmix = countparval("mix"))!=0) {
		mix = ealloc1float(nmix);
		getparfloat("mix",mix);
		
	} else { /* else use default values */
		nmix = 1;
		mix = ealloc1float(nmix);

		mix[0] = VAL0;
	}

	/* Divide mixing weight by number of traces to mix over */
	for (imix = 0; imix < nmix; ++imix)
		mix[imix]=mix[imix]/((float) nmix);

	/* Getpar interpolation method */
	getparstring("method",&method);
        checkpars();
	

	/* compute filter sizes and correlation number */
	nlag  = imaxlag[0] - iminlag[0] + 1;
	ncorr = imaxcorr - imincorr + 1;
	lcorr = imaxlag[0] + 1;

	/* Compute byte sizes in wiener/spiker and autocorr */
	lagbytes = FSIZE*nlag;
	maxlagbytes = FSIZE*lcorr;
	mixbytes = maxlagbytes*nmix;

	/* Allocate memory */
	wiener	 = ealloc1float(nlag);
	spiker	 = ealloc1float(nlag);
	autocorr = ealloc1float(lcorr);
	temp = ealloc1float(lcorr);
	mixacorr = ealloc2float(lcorr,nmix);

	/* Set pointer to "cross" correlation */
	crosscorr = autocorr + iminlag[0];

	/* Zero out mixing array */
	memset((void *) mixacorr[0], 0, mixbytes);

	/* set old cdp  first trace */
	oldcdp = intrace.cdp;

	jcdp = 0;
	/* Main loop over traces */
	do {
		static int itr = 0;
		++itr;
		
		/* if neccessary, compute new filter parameters */
		if (intrace.cdp!=oldcdp && ncdp>1 && jcdp<ncdp-1) {
			 int cdptotal = cdp[ncdp - 1] - intrace.cdp;

			++jcdp;

			free1float(cdpinterp);
			free1float(ominlag);
			free1float(omaxlag);

			

			 /* Compute uniformly sampled */
		         cdpinterp = ealloc1float(cdptotal);
			 ominlag = ealloc1float(cdptotal);
			 omaxlag = ealloc1float(cdptotal);
			
			 memset( (void *) omaxlag, 0, cdptotal * FSIZE);
                 	 memset( (void *) ominlag, 0, cdptotal * FSIZE);

        		 for(icdp=0; icdp<cdptotal; ++icdp)
                	 	cdpinterp[icdp] = intrace.cdp + icdp;

			 for(icdp=0; icdp<cdptotal; ++icdp)
                                ominlag[icdp] = minlag[icdp] + icdp;

			 for(icdp=0; icdp<cdptotal; ++icdp)
                                omaxlag[icdp] = maxlag[icdp] + icdp;
			

				
			 /* if linear interpolation or only one input sample */
   			if (method[0]=='l' || ncdp==1) {
                        	intlin(cdptotal,minlag,cdp,cdp[0],cdp[ncdp-1],ncdp,cdpinterp,ominlag);
				intlin(cdptotal,maxlag,cdp,cdp[0],cdp[ncdp-1],ncdp,cdpinterp,omaxlag);
				for (icdp = 0; icdp < ncdp; ++icdp) {
                        		iminlag[icdp] = NINT(ominlag[icdp]/dt);
                        		imaxlag[icdp] = NINT(omaxlag[icdp]/dt);
                		}

        		/* else, if monotonic interpolation */
        		} else if (method[0]=='m') {
                       		zind = (float (*)[4])ealloc1float(ncdp*4);
                        	cmonot(ncdp,minlag,cdp,zind);
                        	intcub(0,ncdp,minlag,zind,ncdp,cdpinterp,ominlag);
				cmonot(ncdp,maxlag,cdp,zind);
                                intcub(0,ncdp,maxlag,zind,ncdp,cdpinterp,omaxlag);			
				for (icdp = 0; icdp < ncdp; ++icdp) {
                                        iminlag[icdp] = NINT(ominlag[icdp]/dt);
                                        imaxlag[icdp] = NINT(omaxlag[icdp]/dt);
                                }

        		/* else, if Akima interpolation */
        		} else if (method[0]=='a') {
                        	zind = (float (*)[4])ealloc1float(ncdp*4);
                        	cakima(ncdp,minlag,cdp,zind);
                        	intcub(0,ncdp,minlag,zind,ncdp,cdpinterp,ominlag);
				cakima(ncdp,maxlag,cdp,zind);
                                intcub(0,ncdp,maxlag,zind,ncdp,cdpinterp,omaxlag);
				for (icdp = 0; icdp < ncdp; ++icdp) {
                                        iminlag[icdp] = NINT(ominlag[icdp]/dt);
                                        imaxlag[icdp] = NINT(omaxlag[icdp]/dt);
                                }

       			/* else, if cubic spline interpolation */
       			} else if (method[0]=='s') {
                       		zind = (float (*)[4])ealloc1float(ncdp*4);
                       		csplin(ncdp,minlag,cdp,zind);
                       		intcub(0,ncdp,minlag,zind,ncdp,cdpinterp,ominlag);
				csplin(ncdp,maxlag,cdp,zind);
                                intcub(0,ncdp,maxlag,zind,ncdp,cdpinterp,omaxlag);
				for (icdp = 0; icdp < ncdp; ++icdp) {
                                        iminlag[icdp] = NINT(ominlag[icdp]/dt);
                                        imaxlag[icdp] = NINT(omaxlag[icdp]/dt);
                                }

       			/* else, if unknown method specified */
       			} else {
               			err("%s is an unknown interpolation method!\n",method);
       			}


			/* compute filter sizes and correlation number */
			nlag  = imaxlag[jcdp] - iminlag[jcdp] + 1;
			ncorr = imaxcorr - imincorr + 1;
			lcorr = imaxlag[jcdp] + 1;

			/* Compute byte sizes in wiener/spiker and autocorr */
			/*lagbytes = FSIZE*nlag;*/
			maxlagbytes = FSIZE*lcorr;

			/* free memory */
			free1float(wiener);
			free1float(spiker);
			free1float(autocorr);
			free1float(temp);
			free2float(mixacorr);
			
			/* Allocate memory */
			wiener	 = ealloc1float(nlag);
			spiker	 = ealloc1float(nlag);
			autocorr = ealloc1float(lcorr);
			temp = ealloc1float(lcorr);
			mixacorr = ealloc2float(lcorr,nmix);

			/* Set pointer to "cross" correlation */
			crosscorr = autocorr + iminlag[jcdp];

			/* Zero out mixing array */
			memset((void *) mixacorr[0], 0, mixbytes);

		}

		/* zero out filter vectors */
		memset((void *) wiener, 0, lagbytes);
		memset((void *) spiker, 0, lagbytes);
		memset((void *) autocorr, 0, maxlagbytes);
		memset((void *) temp, 0, maxlagbytes);

		/* Form autocorrelation vector */
		xcor(ncorr, imincorr, intrace.data,
		     ncorr, imincorr, intrace.data,
		     lcorr, 0, autocorr);

		/* fix supplied by Sanyu Ye */

/*
		xcor(ncorr, 0, &intrace.data[imincorr],
			ncorr, 0, &intrace.data[imincorr],
			lcorr, 0, autocorr);

*/


		/* Leave trace alone if autocorr[0] vanishes */
		if (autocorr[0] == 0.0) {
			puttr(&intrace);
			if (wienerout)
				warn("NO Wiener filter, trace: %d", itr);

			continue;
		}


		/* Whiten */
		autocorr[0] *= 1.0 + pnoise;

		/* Read autocorr into first column of mixacorr[][] */
		memcpy( (void *) mixacorr[0], 
				(const void *) autocorr, maxlagbytes);

		/* Loop over values of the autocorrelation array */
		for (ilag = 0; ilag < lcorr; ++ilag) {

			/* Weighted moving average (mix) */
			for(imix=0; imix<nmix; ++imix)
				temp[ilag]+=mixacorr[imix][ilag]*mix[imix];

			/* put mixed data back in seismic trace */
			autocorr[ilag] = temp[ilag]; 

		}

		/* Bump columns of mixacorr[][] over by 1 */
		/* to make space for autocorr from next trace */
		for (imix=nmix-1; 0<imix; --imix)
			for (ilag=0; ilag<lcorr; ++ilag) 
				mixacorr[imix][ilag] = mixacorr[imix-1][ilag];


		/* Get inverse filter by Wiener-Levinson */
		stoepf(nlag, autocorr, crosscorr, wiener, spiker);
		

		/* Convolve pefilter with trace - don't do zero multiplies */
		for (i = 0; i < nt; ++i) {
			register int j;
			register int n = MIN(i, imaxlag[jcdp]); 
			register float sum = intrace.data[i];

			for (j = iminlag[jcdp]; j <= n; ++j)
				sum -= wiener[j-iminlag[jcdp]] * intrace.data[i-j];

			outtrace.data[i] = sum;
		}


		/* Show pefilter on request */
		if (wienerout && autocorr[0] != 0.0) {
			register int i;
			warn("Wiener filter, trace: %d", itr);


                        for (i = 0; i < imaxlag[icdp]; ++i) {
                                fprintf(outparfp, "%10g%c ", wiener[i],' ');
			}

                      	fprintf(outparfp, "\n");

			memcpy( (void *) &outtrace, 
				(const void *) &intrace, HDRBYTES);
		
			memset( (void *) outtrace.data, 0, nt*FSIZE);

			outtrace.data[0] = 1.0;

			for (i=0; i< imaxlag[icdp]; ++i)
				outtrace.data[iminlag[icdp]+i] = -wiener[i];

			puttr(&outtrace);

		} else {
			/* Output filtered trace */
			memcpy( (void *) &outtrace, 
				(const void *) &intrace, HDRBYTES);

			puttr(&outtrace);
		}

		/* update value of oldcdp */
		oldcdp = intrace.cdp;
		
	} while (gettr(&intrace));


	return(CWP_Exit());
}
