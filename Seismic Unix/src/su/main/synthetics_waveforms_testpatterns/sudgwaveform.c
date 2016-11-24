/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* dgwaveform: $Revision: 1.4 $ ; $Date: 2012/01/26 17:56:44 $	*/

#include "su.h"
#include "segy.h"

/*************************** self documentation *************************/
char *sdoc[] = {
" 	   								",
" SUDGWAVEFORM - make Gaussian derivative waveform in SU format		",
" 	   								",
"  sudgwaveform >stdout  [optional parameters]				",
" 									",
"									",
" Optional parameters:							",
" n=2    	order of derivative (n>=1)				",
" fpeak=35	peak frequency						",
" nfpeak=n*n	max. frequency = nfpeak * fpeak				",
" nt=128	length of waveform					",
" shift=0	additional time shift in s (used for plotting)		",
" sign=1	use =-1 to change sign					",
" verbose=0	=0 don't display diagnostic messages			",
"               =1 display diagnostic messages				",
" Notes:								",
" This code computes a waveform that is the n-th order derivative of a	",
" Gaussian. The variance of the Gaussian is specified through its peak	",
" frequency, i.e. the frequency at which the amplitude spectrum of the	",
" Gaussian has a maximum. nfpeak is used to compute maximum frequency,	",
" which in turn is used to compute the sampling interval. Increasing	",
" nfpeak gives smoother plots. In order to have a (pseudo-) causal	",
" pulse, the program computes a time shift equal to sqrt(n)/fpeak. An	",
" additional shift can be applied with the parameter shift. A positive	",
" value shifts the waveform to the right.				",
"									",
" Examples:								",
" 2-loop Ricker: dgwaveform n=1	>ricker2.su				",
" 3-loop Ricker: dgwaveform n=2 >ricker3.su				",
" Sonic transducer pulse: dgwaveform n=10 fpeak=300 >sonic.su		",
"									",
" To display use suxgraph. For example:					",
" dgwaveform n=10 fpeak=300 | suxgraph style=normal &			",
"									",
" For other seismic waveforms, please use \"suwaveform\".		",
"									",
NULL};

/* Credits:
 *
 *	Werner M. Heigl, February 2007
 *
 * This copyright covers parts that are not part of the original
 * CWP/SU: Seismic Un*x codes called by this program:
 *
 * Copyright (c) 2007 by the Society of Exploration Geophysicists.
 * For more information, go to http://software.seg.org/2007/0004 .
 * You must read and accept usage terms at:
 * http://software.seg.org/disclaimer.txt before use.
 *
 * Revision history:
 * Original SEG version by Werner M. Heigl, Apache E&P Technology,
 * February 2007.
 *
 * Jan 2010 - subroutines deriv_n_gauss and hermite_n_polynomial moved
 * to libcwp.a
*/
/************************ end self doc **********************************/

static segy tr;	/* structure of type segy that contains the waveform */

int
main(int argc, char **argv)
{
	int i;			/* loop variable			*/
	int n;			/* order of derivative			*/
	int nfpeak;		/* fmax = nfpeak*fpeak			*/
	int nt;			/* length of time vector t[]		*/
	int sign;		/* scalar for polarity			*/
	int verbose;		/* flag for diagnostic messages		*/
	float fmax;		/* max. frequency			*/
	float fpeak;		/* peak frequency			*/
	double dt;		/* sampling interval 			*/
	double shift;		/* additional time shift		*/
	double t0;		/* time delay				*/
	double *w = NULL;	/* waveform and Gaussian		*/	

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(0);
	
	/* Get parameters and do setup */
	if (!getparint("n",&n))			n = 2;
	if (!getparfloat("fpeak",&fpeak))	fpeak = 35;
	if (!getparint("sign",&sign))		sign = 1;
	if (!getparint("nfpeak",&nfpeak))	nfpeak = n * n;
	if (!getpardouble("shift",&shift))	shift = 0;
	if (!getparint("verbose",&verbose))	verbose = 0;	
	fmax = nfpeak * fpeak;
	dt = 0.5 / fmax;	
	t0 = shift + sqrt(n) / fpeak;
	if (!getparint("nt",&nt))	nt = (int) (2 * t0 / dt + 1);
        checkpars();
	warn("n=%d fpeak=%.0f fmax=%.0f t0=%f nt=%d dt=%.12f",n,fpeak,fmax,t0,nt,dt);
	if (dt < 1e-6)
		err("single-precision exceeded: reduce nfpeak or fpeak");

	/* allocate & initialize memory */
	w = ealloc1double(nt);
	memset((void *) w, 0, DSIZE * nt);
	if (verbose)	warn("memory for waveform allocated and initialized");
	
	/* compute waveform */
	if (n >= 1) {
		
		/* compute n-th derivative of Gaussian */
		deriv_n_gauss(dt, nt, t0, fpeak, n, w, sign, verbose);
	
		/* write out waveform */	

			for (i = 0; i < nt; ++i) tr.data[i] = (float) w[i];
			tr.tracl = 1;
			tr.ns    = nt;
			tr.trid  = 1;
			tr.dt    = NINT(dt*1000000.0);
			tr.ntr   = 1;
			puttr(&tr);
			warn("waveform written to stdout");
			
		
	} else	err("specified n not >=1 !!");

	/* free memory */
	free1double(w);
	if (verbose)	warn("memory freed");
	
	return(CWP_Exit());
}
