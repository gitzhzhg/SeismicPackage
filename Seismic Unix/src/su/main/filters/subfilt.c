/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUBFILT: $Revision: 1.22 $ ; $Date: 2012/11/28 22:13:13 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUBFILT - apply Butterworth bandpass filter 			",
" 								",
" subfilt <stdin >stdout [optional parameters]			",
" 							        ",
" Required parameters:						",
" 	if dt is not set in header, then dt is mandatory	",
" 							        ",
" Optional parameters: (nyquist calculated internally)		",
" 	zerophase=1		=0 for minimum phase filter 	",
" 	locut=1			=0 for no low cut filter 	",
" 	hicut=1			=0 for no high cut filter 	",
" 	fstoplo=0.10*(nyq)	freq(Hz) in low cut stop band	",
" 	astoplo=0.05		upper bound on amp at fstoplo 	",
" 	fpasslo=0.15*(nyq)	freq(Hz) in low cut pass band	",
" 	apasslo=0.95		lower bound on amp at fpasslo 	",
" 	fpasshi=0.40*(nyq)	freq(Hz) in high cut pass band	",
" 	apasshi=0.95		lower bound on amp at fpasshi 	",
" 	fstophi=0.55*(nyq)	freq(Hz) in high cut stop band	",
" 	astophi=0.05		upper bound on amp at fstophi 	",
" 	verbose=0		=1 for filter design info 	",
" 	dt = (from header)	time sampling interval (sec)	",
" 							        ",
" ... or  set filter by defining  poles and 3db cutoff frequencies",
"	npoleselo=calculated     number of poles of the lo pass band",
"	npolesehi=calculated     number of poles of the lo pass band",
"	f3dblo=calculated	frequency of 3db cutoff frequency",
"	f3dbhi=calculated	frequency of 3db cutoff frequency",
" 							        ",
" Notes:						        ",
" Butterworth filters were originally of interest because they  ",
" can be implemented in hardware form through the combination of",
" inductors, capacitors, and an amplifier. Such a filter can be ",
" constructed in such a way as to have very small oscillations	",
" in the flat portion of the bandpass---a desireable attribute.	",
" Because the filters are composed of LC circuits, the impulse  ",
" response is an ordinary differential equation, which translates",
" into a polynomial in the transform domain. The filter is expressed",
" as the division by this polynomial. Hence the poles of the filter",
" are of interest.					        ",
" 							        ",
" The user may define low pass, high pass, and band pass filters",
" that are either minimum phase or are zero phase.  The default	",
" is to let the program calculate the optimal number of poles in",
" low and high cut bands. 					",
" 							        ",
" Alternately the user may manually define the filter by the 3db",
" frequency and by the number of poles in the low and or high	",
" cut region. 							",
" 							        ",
" The advantage of using the alternate method is that the user  ",
" can control the smoothness of the filter. Greater smoothness  ",
" through a larger pole number results in a more bell shaped    ",
" amplitude spectrum.						",
" 							        ",
" For simple zero phase filtering with sin squared tapering use ",
" \"sufilter\".						        ",
NULL};

/* Credits:
 *	CWP: Dave Hale c. 1993 for bf.c subs and test drivers
 *	CWP: Jack K. Cohen for su wrapper c. 1993
 *      SEAM Project: Bruce Verwest 2009 added explicit pole option
 *                    in a program called "subfiltpole"
 *      CWP: John Stockwell (2012) combined Bruce Verwests changes
 *           into the original subfilt.
 *
 * Caveat: zerophase will not do good if trace has a spike near
 *	   the end.  One could make a try at getting the "effective"
 *	   length of the causal filter, but padding the traces seems
 *	   painful in an already expensive algorithm.
 *
 *
 * Theory:
 * The 
 *
 * Trace header fields accessed: ns, dt, trid
 */
/**************** end self doc ***********************************/



segy tr;

int
main(int argc, char **argv)
{
	int zerophase;		/* flag for zero phase filtering	*/
	int locut;		/* flag for low cut filtering		*/
	int hicut;		/* flag for high cut filtering		*/
	float fstoplo;		/* left lower corner frequency		*/
	float fpasslo;		/* left upper corner frequency		*/
	float fpasshi;		/* right lower corner frequency		*/
	float fstophi;		/* right upper corner frequency		*/
	float astoplo;		/* amp at fstoplo			*/
	float apasslo;		/* amp at fpasslo			*/
	float apasshi;		/* amp at fpasshi			*/
	float astophi;		/* amp at fstophi			*/
	int npoleslo=0;		/* poles in low cut filter		*/
	int npoleshi=0;		/* poles in high cut filter		*/
	float f3dblo=0.0;	/* 3 db point of low cut filter		*/
	float f3dbhi=0.0;	/* 3 db point of high cut filter	*/
	float dt;		/* sample spacing			*/
	float nyq;		/* nyquist frequency			*/
	int nt;			/* number of points on input trace	*/
	int verbose;		/* design info flag 			*/
	cwp_Bool seismic;	/* is this seismic data?		*/
	cwp_Bool is_npoleslo=cwp_false;		/* is npoleslo set */
	cwp_Bool is_npoleshi=cwp_false;		/* is npoleshi set */

	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	seismic = ISSEISMIC(tr.trid); 
		 
	if (!seismic)
		warn("input is not seismic data, trid=%d", tr.trid);
	nt = tr.ns;
	if (!getparfloat("dt", &dt))	dt = ((double) tr.dt)/1000000.0;
	if (!dt) err("dt field is zero and not getparred");
	nyq = 0.5/dt;


	/* Get design frequencies and amplitudes */
	if (!getparint("verbose", &verbose))	verbose = 0;
	if (!getparint("zerophase", &zerophase)) zerophase = 1;
	if (!getparint("locut", &locut))	locut = 1;
	if (!getparint("hicut", &hicut))	hicut = 1;
	if (!getparfloat("fstoplo", &fstoplo))	fstoplo = .10 * nyq;
	if (!getparfloat("fpasslo", &fpasslo))	fpasslo = .15 * nyq;
	if (!getparfloat("fpasshi", &fpasshi))	fpasshi = .40 * nyq;
	if (!getparfloat("fstophi", &fstophi))	fstophi = .55 * nyq;
	if (locut) {
		if (fstoplo <= 0.0)      err("fstoplo must be positive");
		if (fstoplo > fpasslo)  err("fstoplo must be < fpasslo");
	}
	if (hicut) {
		if (fpasshi > fstophi)  err("fpasshi must be < fstophi");
		if (fstophi > nyq)  err("fstophi must be < nyquist (%f)", nyq);
	}
	if (!getparfloat("astoplo", &astoplo))	astoplo = .05;
	if (!getparfloat("apasslo", &apasslo))	apasslo = .95;
	if (!getparfloat("apasshi", &apasshi))	apasshi = .95;
	if (!getparfloat("astophi", &astophi))	astophi = .05;
	if (astoplo > apasslo || apasshi < astophi)
		err("Bad amplitude parameters");
		
		
	/* Normalize frequencies to [0, 0.5] for bfdesign */
	fstoplo *= dt;
	fpasslo *= dt;
	fstophi *= dt;
	fpasshi *= dt;
	
	
	/* Adapt user frequencies if zerophase selected */
	if (zerophase) {	
		astoplo = sqrt(astoplo);
		apasslo = sqrt(apasslo);
		astophi = sqrt(astophi);
		apasshi = sqrt(apasshi);
	}

	if(getparint("npoleslo",&npoleslo)) 	is_npoleslo=cwp_true;
	if(getparint("npoleshi",&npoleshi)) 	is_npoleshi=cwp_true;

	if (is_npoleslo) { 
		if (!getparfloat("f3dblo", &f3dblo))	f3dblo = .15 * nyq;
        	f3dblo *= dt;
	} else { /* Use bdesign to make lo cut filters */
	  if (locut) bfdesign(fpasslo,apasslo,fstoplo,astoplo,&npoleslo,&f3dblo);
	}
	
	if (is_npoleshi) {
		if (!getparfloat("f3dbhi", &f3dbhi))	f3dbhi = .40 * nyq;
        	f3dbhi *= dt;
	} else { /* Use bdesign to make hi cut filters */
	   if (hicut) bfdesign(fpasshi,apasshi,fstophi,astophi,&npoleshi,&f3dbhi);
	}


	/* Give verbose info if requested */
	if (verbose && locut) {
		if (zerophase) {
			warn("low-cut filter: npoles = %d, 3db point = %f(Hz)",
				2*npoleslo, f3dblo/dt);
		} else {
			warn("low-cut filter: npoles = %d, 3db point = %f(Hz)",
				npoleslo, f3dblo/dt);
		}
	}
	if (verbose && hicut) {
		if (zerophase) {
			warn("high-cut filter: npoles = %d, 3db point = %f(Hz)",
				2*npoleshi, f3dbhi/dt);
		} else {
			warn("high-cut filter: npoles = %d, 3db point = %f(Hz)",
				npoleshi, f3dbhi/dt);

		}
	}

	/* Main loop over traces */
	do {
		/* low-cut (high pass) filter */
		if (locut) {
		    bfhighpass(npoleslo,f3dblo,nt,tr.data,tr.data);
		    if (zerophase) {
			register int i;
		        for (i=0; i<nt/2; ++i) { /* reverse trace in place */
				register float tmp = tr.data[i];
				tr.data[i] = tr.data[nt-1 - i];
				tr.data[nt-1 - i] = tmp;
			}
		        bfhighpass(npoleslo,f3dblo,nt,tr.data,tr.data);
		        for (i=0; i<nt/2; ++i) { /* flip trace back */
				register float tmp = tr.data[i];
				tr.data[i] = tr.data[nt-1 - i];
				tr.data[nt-1 - i] = tmp;
			}
		    }
		}

		/* high-cut (low pass) filter */
		if (hicut) {
		    bflowpass(npoleshi,f3dbhi,nt,tr.data,tr.data);
		    if (zerophase) {
			register int i;
			for (i=0; i<nt/2; ++i) { /* reverse trace */
				register float tmp = tr.data[i];
				tr.data[i] = tr.data[nt-1 - i];
				tr.data[nt-1 - i] = tmp;
			}
			bflowpass(npoleshi,f3dbhi,nt,tr.data,tr.data);
		        for (i=0; i<nt/2; ++i) { /* flip trace back */
				register float tmp = tr.data[i];
				tr.data[i] = tr.data[nt-1 - i];
				tr.data[nt-1 - i] = tmp;
			}
		    }
		}
		
		puttr(&tr);
	} while (gettr(&tr));

	return(CWP_Exit());
}
