/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUANALYTIC: $Revision: 1.2 $ ; $Date: 2015/08/07 22:32:52 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUANALYTIC - use the Hilbert transform to generate an ANALYTIC	",
"		(complex) trace						",
"									",
" suanalytic <stdin >sdout 						",
"							       		",
" Optional Parameter:							",
" phaserot=		phase rotation in degrees of complex trace	",
" 									",
" Notes:								",
" 									",
" The output are complex valued traces. The analytic trace is defined as",  
"   ctr[ i ] = indata[i] + i hilb[indata[t]]				",
" where the imaginary part is the hilbert tranform of the original trace",
" 									",
" The Hilbert transform is computed in the direct (time) domain		",
" 									",
" If phaserot is set, then a phase rotated complex trace is produced	",
"   ctr[ i ] = cos[phaserot]*indata[i] + i sin[phaserot]* hilb[indata[t]]",
" 									",
" Use \"suamp\" to extract real, imaginary, amplitude (modulus), etc 	",
" Exmple:								",
" suanalytic < sudata | suamp mode=amp | suxgraph 			",
" 									",

" 									",
" Use \"suattributes\" for instantaneous phase, frequency, etc.		",
"							       		",
NULL};

/* Credits:
 *    CWP: John Stockwell, based on suhilb by Jack K. Cohen.
 *
 * Trace header fields accessed: ns, trid
 *
 * Technical references:
 * Oppenheim, A. V. and Schafer, R. W. (1999).
 *     Discrete-Time Signal Processing. Prentice Hall Signal Processing Series.
 *     Prentice Hall, New Jersey, 2.
 * Taner, M. T., F. Koehler, and R. E. Sheriff, 1979, Complex seismic 
 *    trace analysis: Geophysics, 44, 1041-1063. 
 *
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int nt;			/* number of points on input trace	*/
	float *data=NULL;	/* data values from each trace		*/
	float *hdata=NULL;	/* Hilbert transformed data values	*/
	float phaserot=0.0;	/* phase rotation angle in degrees	*/
	register int i;		/* counter				*/
	cwp_Bool seismic;	/* is this seismic data?		*/
	cwp_Bool is_phaserot=cwp_true;	/* is phaserot set?		*/
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get info from first trace */
	if (!gettr(&tr)) err ("can't get first trace");
	nt = tr.ns;
	if (!getparfloat("phaserot",&phaserot))	{
		phaserot=0.0;
		is_phaserot = cwp_false;
	} else {
		
		is_phaserot = cwp_true;
	}
	


	/* Check that data is correct format */
	seismic = ISSEISMIC(tr.trid);
	if (!seismic)
		warn("input is not seismic data, trid=%d", tr.trid);

	/* allocate space for data and hdata */
	data = ealloc1float(nt);
	hdata = ealloc1float(nt);

	/* Loop over traces */
	do {


		/* zero out arrays */
		memset((void *) data, 0, nt*FSIZE);
		memset((void *) hdata, 0, nt*FSIZE);

		/* read data from trace */
		for (i = 0; i < nt; i++) data[i] = tr.data[i];

		/* apply the Hilbert tranform */		
		hilbert(nt,data,hdata);

		/* put Hilbert tranformed data back in trace */
		if (!is_phaserot) {
			for (i = 0; i < nt; ++i) {
				tr.data[2*i] = data[i];
				tr.data[2*i + 1] = hdata[i];
			}
		} else {
			for (i = 0; i < nt; ++i) {
				tr.data[2*i] = data[i]*cos((double) PI*phaserot/180.0);
				tr.data[2*i + 1] = hdata[i]*sin((double) PI*phaserot/180.0);
			}
		}
		
		/* set trace id to complex type */
		tr.trid = FUNPACKNYQ;
		tr.ns = 2*nt;
		
		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
