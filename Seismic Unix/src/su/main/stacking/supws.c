/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUPWS - Phase stack or phase-weighted stack (PWS) of adjacent traces	",
"	 having the same key header word				",
"									",
" supws <stdin >stdout [optional parameters]				",
"									",
" Required parameters:							",
"	none								",
"									",
" Optional parameters:						 	",
"	key=cdp	   key header word to stack on				",
"	pwr=1.0	   raise phase stack to power pwr			",
"	dt=(from header)  time sampling intervall in seconds		",
"	sl=0.0		window length in seconds used for smoothing	",
"			of the phase stack (weights)			",
"	ps=0		0 = output is PWS, 1 = output is phase stack	",
"	verbose=0	 1 = echo additional information		",
"									",
" Note:								 	",
"	Phase weighted stacking is a tool for efficient incoherent noise",
"	reduction. An amplitude-unbiased coherency measure is designed	",
"	based on the instantaneous phase, which is used to weight the	",
"	samples of an ordinary, linear stack. The result is called the	",
"	phase-weighted stack (PWS) and is cleaned from incoherent noise.",
"	PWS thus permits detection of weak but coherent arrivals.	",
"									",
"	The phase-stack (coherency measure) has values between 0 and 1.	",
"									",
"	If the stacking is over cdp and the PWS option is set, then the	",
"	offset header field is set to zero. Otherwise, output traces get",
"	their headers from the first trace of each data ensemble to stack,",
"	including the offset field. Use \"sushw\" afterwards, if this is",
"	not acceptable.							",
"									",
NULL};

/*
 * Author: Nils Maercklin,
 *	 GeoForschungsZentrum (GFZ) Potsdam, Germany, 2001.
 *	 E-mail: nils@gfz-potsdam.de
 *
 * References:
 *	B. L. N. Kennett, 2000: Stacking three-component seismograms.
 *	 Geophysical Journal International, vol. 141, p. 263-269.
 *	M. Schimmel and H. Paulssen, 1997: Noise reduction and detection
 *	 of weak , coherent signals through phase-weighted stacks.
 *	 Geophysical Journal International, vol. 130, p. 497-505.
 *	M. T. Taner, A. F. Koehler, and R. E. Sheriff, 1979: Complex
 *	 seismic trace analysis. Geophysics, vol. 44, p. 1041-1063.
 *
 * Trace header fields accessed: ns
 * Trace header fields modified: nhs, offset
 */

/**************** end self doc ********************************/

/* function prototype */
void do_smooth(float *data, int nt, int isl);


segy intr, outtr;

int
main(int argc, char **argv)
{
	FILE *headerfp=NULL;	/* temporary file for trace header	*/
				/*  ... (1st trace of ensemble);	*/
	char *key=NULL;		/* header key word from segy.h		*/
	char *type=NULL;	/* ... its type				*/
	int index;		/* ... its index			*/
	Value val;		/* ... its value			*/
	float fval = 0;		/* ... its value cast to float		*/
	float prevfval;		/* ... its value of the previous trace	*/

	complex *ct=NULL;	/* complex trace			*/
	complex *psct=NULL;	/* phase-stack data array		*/

	float *data=NULL;	/* input data array			*/
	float *hdata=NULL;	/* array of Hilbert transformed input data */
	float *stdata=NULL;	/* stacked data ("ordinary" stack)	*/
	float *psdata;	/* phase-stack data array (real weights for PWS)*/
	float a;	/* inst. amplitude				*/
	float dt;	/* time sample spacing in seconds		*/
	float pwr;	/* raise  phase stack to power pwr		*/
	float sl;	/* smoothing window length in seconds		*/

	int gottrace;	/* flag: set to 1, if trace is read from stdin	*/

	int i;		/* loop index					*/
	int isl;	/* smoothing window length in samples		*/
	int nt;		/* number of points on input trace		*/
	int ntr;	/* trace counter				*/
	int ps;		/* flag: output is PWS (0) or phase stack (1)	*/
	int verbose;	/* verbose flag					*/

	cwp_Bool pws_and_cdp=cwp_false;	/* are PWS and CDP set?		*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get info from first trace */
	if(!gettr(&intr)) err("can't get first trace");
	nt = intr.ns;

	/* Get parameters */
	if (!getparstring("key", &key))			key="cdp";
	if (!getparfloat("pwr", &pwr))			pwr = 1.0;
	if (!getparfloat("dt", &dt)) dt = ((double) intr.dt)/1000000.0;
	if (!getparfloat("sl", &sl))			sl = 0.0;
	if (!getparint("ps", &ps))			ps = 0;
	if (!getparint("verbose", &verbose))		verbose = 0;

        checkpars();

	if (STREQ(key, "cdp") &&  !(ps))
		pws_and_cdp = cwp_true;

	/* convert seconds to samples (necessary only for smoothing) */
	if (!dt) {
		dt = 0.004;
		warn("dt not set, assuming dt=0.004");
	}

	/* integerized smoothing window length */
	isl = NINT(fabs(sl)/dt);
	if (isl>nt)
		err("sl=%g too long for trace", fabs(sl));

	/* diagnostic print */
	if (verbose && isl)
		warn("smoothing window = %d samples", isl);

	/* initialize flag */
	gottrace = 1;

	/* get key type and index */
	type = hdtype(key);
	index = getindex(key);

	/* Get value of key and convert to float */
	prevfval = fval;
	gethval(&intr, index, &val);
	fval = vtof(type,val);

	/* remember previous value of key */
	prevfval = fval;

	/* allocate space for data, hilbert transformed data, */
	/* phase-stacked data and complex trace */
	data = ealloc1float(nt);
	hdata = ealloc1float(nt);
	stdata = ealloc1float(nt);
	psdata = ealloc1float(nt);
	psct = ealloc1complex(nt);
	ct = ealloc1complex(nt);


	/* zero out accumulators */
	memset( (void *) stdata, 0, nt*FSIZE);
	memset( (void *) psct, 0, nt*(sizeof(complex)));


	/* open temporary file for trace header   */
	headerfp = etmpfile();

	/* store trace header in temporary file and read data */
	efwrite(&intr, HDRBYTES, 1, headerfp);

	/* loop over input traces */
	ntr=0;
	while (gottrace|(~gottrace) ) { /* middle exit loop */

		/* if got a trace */
		if (gottrace) {
			/* get value of key */
			gethval(&intr, index, &val);
			fval = vtof(type,val);

			/* get data */
			memcpy((void *) data, (const void *) intr.data,
					nt*FSIZE);
		}

		/* construct quadrature trace with hilbert transform */
		hilbert(nt, data, hdata);

		/* build the complex trace and get rid of amplitude */
		for (i=0; i<nt; i++) {
			ct[i] = cmplx(data[i],hdata[i]);
			a = (rcabs(ct[i])) ? 1.0 / rcabs(ct[i]) : 0.0;
			ct[i] = crmul(ct[i], a);
		}

		/* stacking */
		if (fval==prevfval && gottrace) {
			++ntr;
			for (i=0; i<nt; ++i) {
				stdata[i] += data[i];
				psct[i] = cadd(psct[i],ct[i]);
			}
		}

		/* if key-value has changed or no more input traces */
		if (fval!=prevfval || !gottrace) {

			/* diagnostic print */
			if (verbose)
				warn("%s=%g, fold=%d\n", key, prevfval, ntr);

			/* convert complex phase stack to real weights */
			for (i=0; i<nt; ++i) {
				psdata[i] = rcabs(psct[i]) / (float) ntr;
				psdata[i] = pow(psdata[i], pwr);
			}

			/* smooth phase-stack (weights) */
			if (isl) do_smooth(psdata,nt,isl);

			/* apply weights to "ordinary" stack (do PWS) */
			if (!ps) {
				for (i=0; i<nt; ++i) {
					stdata[i] *= psdata[i] / (float) ntr;
				}
			}

			/* set header and write PS trace or */
			/* PWS trace to stdout */
			erewind(headerfp);
			efread(&outtr, 1, HDRBYTES, headerfp);
			outtr.nhs=ntr;
			if (ps) {
				memcpy((void *) outtr.data,
					(const void *) psdata, nt*FSIZE);
			} else {
				memcpy((void *) outtr.data,
					(const void *) stdata, nt*FSIZE);
			}

			/* zero offset field if a pws and cdp stack */
			if (pws_and_cdp) outtr.offset = 0;

			puttr(&outtr);

			/* if no more input traces, break input trace loop* */
			if (!gottrace) break;


			/* remember previous value of key */
			prevfval = fval;

			/* zero out accumulators */
			ntr=0;
			memset( (void *) stdata, 0, nt*FSIZE);
			memset( (void *) psct, 0, nt*(sizeof(complex)));

			/* stacking */
			if (gottrace) {
				++ntr;
				for (i=0; i<nt; ++i) {
					stdata[i] += data[i];
					psct[i] = cadd(psct[i],ct[i]);
				}
			}

			/* save trace header for output trace */
			erewind(headerfp);
			efwrite(&intr, HDRBYTES, 1, headerfp);
		}

		/* get next trace (if there is one) */
		if (!gettr(&intr)) gottrace = 0;

	} /* end loop over traces */

	return(CWP_Exit());
}


/**********************************************************************/
/* Functions used internally					  */
/**********************************************************************/


void do_smooth(float *data, int nt, int isl)
/**********************************************************************
do_smooth - smooth data in a window of length isl samples
**********************************************************************
Input:
data[]		array of floats of size nt
nt		size of array
isl		integerized window length
Output:
returns smoothed data.

**********************************************************************
Author: Nils Maercklin,
 	 GeoForschungsZentrum (GFZ) Potsdam, Germany, 2001.
 	 E-mail: nils@gfz-potsdam.de
**********************************************************************/
{
	register int it,jt;
	float *tmpdata, sval;

	tmpdata=ealloc1float(nt);
	for (it=0;it<nt;it++) {
	sval=0.0;
	if ( (it >= isl/2) && (it < nt-isl/2) ) {
		for (jt=it-isl/2;jt<it+isl/2;jt++) {
			sval += data[jt];
		}
		tmpdata[it] = sval / (float) isl;
		} else {
			tmpdata[it] = data[it];
		}
	}

	memcpy((void *) data, (const void *) tmpdata, nt*FSIZE);

	free1float(tmpdata);
}

/* END OF FILE */
