/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.			*/

/* SUWAVEFORM: $Revision: 1.8 $ ; $Date: 2015/06/02 20:15:23 $	*/


#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUWAVEFORM - generate a seismic wavelet				",
"									",
" suwaveform <stdin >stdout [optional parameters]			",
"									",
" Required parameters:						  	",
"	one of the optional parameters listed below			",
"									",
" Optional parameters:						  	",
"	type=akb	wavelet type					",
"		   akb:	AKB wavelet defined by max frequency fpeak	",
"		   berlage: Berlage wavelet				",
"		   gauss:   Gaussian wavelet defined by frequency fpeak	",
"		   gaussd:  Gaussian first derivative wavelet		",
"		   ricker1: Ricker wavelet defined by frequency fpeak	",
"		   ricker2: Ricker wavelet defined by half and period	",
"		   spike:   spike wavelet, shifted by time tspike	",
"		   unit:	unit wavelet, i.e. amplitude = 1 = const.",
"									",
"	dt=0.004	time sampling interval in seconds		",
"	ns=		if set, number of samples in  output trace	",
"									",
"	fpeak=20.0	peak frequency of a Berlage, Ricker, or Gaussian,",
"		   and maximum frequency of an AKB wavelet in Hz	",
"									",
"	half=1/fpeak   Ricker wavelet \"ricker2\": half-length		",
"	period=c*half  Ricker wavelet \"ricker2\": period (c=sqrt(6)/pi)",
"	distort=0.0	Ricker wavelet \"ricker2\": distortion factor	",
"	decay=4*fpeak  Berlage wavelet: exponential decay factor in 1/sec",
"	tn=2	   Berlage wavelet: time exponent			",
"	ipa=-90	Berlage wavelet: initial phase angle in degrees		",
"	tspike=0.0	Spike wavelet: time at spike in seconds		",
"	verbose=0	1: echo output wavelet length			",
"									",
"									",
" Notes:								",
"	If ns is not defined, the program determines the trace length	",
"	depending on the dominant signal period.			   ",
"									",
"	The Ricker wavelet \"ricker1\" and the Gaussian wavelet \"gauss\"  ",
"	are zero-phase. For these two wavelets, the trace header word	",
"	delrt is set such that the peak amplitude is at t=0 seconds.	",
"	If this is not acceptable, use \"sushw key=delrt a=0\".		",
"									",
"	The Ricker wavelets can be defined either by the peak frequency	",
"	fpeak (\"ricker1\") or by its half-length, the period, and a	",
"	distortion factor (\"ricker2\"). \"ricker\" is an acceptable	",
"	alias for \"ricker1\".						",
"									",
"	The Berlage wavelet is defined by the peak frequency fpeak, a time ",
"	time exponent tn describing the wavelet shape at its beginning,	",
"	and an exponential decay factor describing the amplitude decay	",
"	towards later times. The parameters tn and decay are non-negative, ",
"	real numbers; tn is typically a small integer number and decay a   ",
"	multiple of the dominant signal period 1/fpeak. Additionally, an   ",
"	initial phase angle can be given; use -90 or 90 degrees for	",
"	zero-amplitude at the beginning.				   ",
"									",
"	For an AKB wavelet, fpeak is the maximum frequency; the peak	",
"	frequency is about 1/3 of the fpeak value.			 ",
"									",
"	The output wavelet can be normalized or scaled with \"sugain\".	",
"	Use \"suvibro\" to generate a Vibroseis sweep.			",
"									",
" Example:								",
" A normalized, zero-phase Ricker wavelet with a peak frequency		",
" of 15 Hz is generated and convolved with a spike dataset:		",
"									",
"	suwaveform type=ricker1 fpeak=15 | sugain pbal=1 > wavelet.su	",
"	suplane npl=1 | suconv sufile=wavelet.su | suxwigb		",
" 									",
" Gaussian and derivatives of Gaussians:				",
" Use \"sudgwaveform\" to generate these				",
" 									",
" Caveat:								",
"	This program does not check for aliasing.			",
"									",
NULL};

/*
 * Author: 
 *	Nils Maercklin, RISSC, University of Napoli, Italy, 2006
 *
 * References:
 *	Aldridge, D. F. (1990). The Berlage wavelet. 
 *	Geophysics, vol. 55(11), p. 1508-1511.
 *	Alford, R., Kelly, K., and Boore, D. (1947). Accuracy
 *	of finite-difference modeling of the acoustic wave
 *	equation. Geophysics, vol. 39, p. 834-842. (AKB wavelet)
 *	Sheriff, R. E. (2002). Encyclopedic dictionary of 
 *	applied geophysics. Society of Exploration Geophysicists,
 *	Tulsa. (Ricker wavelet, page 301)
 *
 * Notes:
 *	For more information on the wavelets type "sudoc waveforms" 
 *	or have a look at "$CWPROOT/src/cwp/lib/waveforms.c".
 *
 * Credits: 
 *	CWP, the authors of the subroutines in "waveforms.c".
 *
 * Trace header fields set: ns, dt, trid, delrt
 */
/**************** end self doc ***********************************/

segy tr;

int
main(int argc, char **argv)
{
	char *wtype=NULL;	/* wavelet type */
	int nt;		/* number of trace samples */
	float dt;		/* time sampling in seconds */
	float fpeak;	 /* peak of maximum frequency in Hz */
	float *wavelet=NULL; /* wavelet */
	float period;	/* period of ricker2 wavelet */
	float half;	  /* half-length of ricker2 in seconds */
	int hlw;		/* half-length of ricker2 in samples */
	float ampl;	  /* amplitude scale factor (ricker2, berlage) */
	float distort;	/* distortion factor of ricker2 */
	float tn;		/* non-negative time exponent (berlage wavelet) */
	float decay;	 /* non-negative exponential decay factor (berlage) */
	float ipa;	   /* initial phase angle in radians (berlage) */
	float tspike;	/* time of spike in seconds (spike wavelet) */
	int verbose=0;	/* flag: verbose option */
	

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get parameters and validate input */
	if (!getparfloat("dt", &dt)) dt=0.004;
	if (dt<=0.0) err("dt=%g must be positive", dt);
	if (!getparfloat("fpeak", &fpeak)) fpeak=20.0;
	if (fpeak<=0.0) err("fpeak=%g must be positive", fpeak);
	if (!getparfloat("half", &half)) half=1.0/fpeak;
	if (half<=0.0) err("half=%g must be positive", half);
	if (!getparfloat("period", &period)) period=half*0.77969680123;
	if (period<=0.0) err("period=%g must be positive", period);
	if (!getparfloat("tspike", &tspike)) tspike=0.0;
	if (tspike<0.0) err("tspike=%g must be non-negative", tspike);	
	if (!getparfloat("ampl", &ampl)) ampl=1.0;
	if (!getparfloat("distort", &distort)) distort=0.0;
	if (!getparfloat("tn", &tn)) tn=2.0;
	if (tn<0.0) err("tn=%g must be non-negative", tn);  
	if (!getparfloat("decay", &decay)) decay=4.0*fpeak;
	if (decay<0.0) err("decay=%g must be non-negative", decay);  
	if (!getparfloat("ipa", &ipa)) ipa=-90.0;
	ipa *= PI/180.0;

	if (!getparint("verbose", &verbose)) verbose=0;
	if (!getparstring("type", &wtype)) wtype="akb";

	/* Alias "ricker" for "ricker1" */
	if (STREQ(wtype,"ricker")) wtype="ricker1";
	
	/* Get number of samples or estimate "useful" trace length */
	if (!getparint("ns", &nt) && !getparint("nt", &nt)) {
	if (STREQ(wtype,"berlage")) {
		if (decay) {
		/* empiric trace-length estimate */
		nt = NINT((floor(fpeak*(8.0+2.0*tn)/decay))/(dt*fpeak))+1;
		}
		else {
		nt = NINT( 2.0 / (dt*fpeak))+1;
		}
	}
	else if (STREQ(wtype,"ricker1")) {
		nt = NINT(2.0/(fpeak*dt))+1;
	}
	else if (STREQ(wtype,"ricker2")) {
		nt = NINT(2.0*half/dt)+1;
	}
	else if (STREQ(wtype,"gauss")) {
		nt = NINT(2.0/(fpeak*dt))+1;
	}
	else if (STREQ(wtype,"gaussd")) {
		nt = NINT(2.0/(fpeak*dt))+1;
	}
	else if (STREQ(wtype,"akb")) {
		nt = NINT(4.0/(fpeak*dt))+1;
	}
	else if (STREQ(wtype,"spike")) {
		nt = NINT(tspike/dt)+5;
	}
	else if (STREQ(wtype,"unit")) {
		nt = NINT(half/dt)+1;
	}
	else {
		err("unknown wavelet type=%s", wtype);
	}
	};
	
	/* Check trace length */
	if (nt<1) {
	warn("ns=%d too small, using ns=1", nt); nt=1;
	}
	if (nt>SU_NFLTS) {
	warn("trace too long, using maximum ns=%d", SU_NFLTS);
	nt=SU_NFLTS;
	}
	
	/* Allocate space for wavelet */
	wavelet=ealloc1float(nt);
	
	/* Compute wavelet */
	if (STREQ(wtype,"berlage")) {
	berlage_wavelet (nt, dt, fpeak, ampl, tn, decay, ipa, wavelet);
	}
	else if (STREQ(wtype,"ricker1")) {
	ricker1_wavelet (nt, dt, fpeak, wavelet);
	tr.delrt=-NINT(1000.0/(fpeak));
	}
	else if (STREQ(wtype,"ricker2")) {
	hlw=NINT(half/dt);
	if (hlw==0) hlw=1;
	ricker2_wavelet (hlw, dt, period, ampl, distort, wavelet);
	if (!distort) tr.delrt=-NINT(1000.0*half);
	}
	else if (STREQ(wtype,"gauss")) {
	gaussian_wavelet (nt, dt, fpeak, wavelet);
	tr.delrt=-NINT(1000.0/(fpeak));
	}
	else if (STREQ(wtype,"gaussd")) {
	gaussderiv_wavelet (nt, dt, fpeak, wavelet);
	}
	else if (STREQ(wtype,"akb")) {
	akb_wavelet (nt, dt, fpeak, wavelet);
	}
	else if (STREQ(wtype,"spike")) {
	spike_wavelet (nt, NINT(tspike/dt), wavelet);
	}
	else if (STREQ(wtype,"unit")) {
	unit_wavelet (nt, wavelet);
	}
	else {
	err("unknown wavelet type=%s", wtype);
	}

	
	/* Copy wavelet to trace and set header */
	memcpy((void *) tr.data, (const void *) wavelet, nt*FSIZE);
	tr.tracl = 1;
	tr.dt = NINT(dt*1000000.0);
	tr.ns = nt;
	tr.trid = 1;
	
	/* Write trace to stdout */
	puttr(&tr);
	
	/* Free space */
	free1float(wavelet);

	/* Echo trace length, if verbose=1 */
	if (verbose) {
	warn("%s wavelet with %d samples (%g seconds)", \
		wtype, nt, dt*(float)(nt-1));
	}

	return(CWP_Exit());
}
