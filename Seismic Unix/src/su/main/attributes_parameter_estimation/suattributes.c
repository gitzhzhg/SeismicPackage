/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.		       */

/* SUATTRIBUTES:  $Revision: 1.34 $ ; $Date: 2013/08/20 22:24:04 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUATTRIBUTES - instantaneous trace ATTRIBUTES 			",
" 									",
" suattributes <stdin >stdout mode=amp					",
" 									",
" Required parameters:							",
" 	none								",
" 									",
" Optional parameter:							",
" 	mode=amp	output flag 					",
" 	       		=amp envelope traces				",
" 	       		=phase phase traces				",
" 	       		=freq frequency traces				",
"			=bandwith Instantaneous bandwidth		",
"			=normamp Normalized Phase (Cosine Phase)	",
" 	       		=fdenv 1st envelope traces derivative		",
" 	       		=sdenv 2nd envelope traces derivative		",
" 	       		=q Ins. Q Factor				",
" ... unwrapping related options ....					",
"	unwrap=		default unwrap=0 for mode=phase			",
" 			default unwrap=1 for freq, uphase, freqw, Q	",
" 			dphase_min=PI/unwrap				",
"       trend=0		=1 remove the linear trend of the inst. phase	",
" 	zeromean=0	=1 assume instantaneous phase is zero mean	",
" 									",
"			=freqw Frequency Weighted Envelope		",
"			=thin  Thin-Bed (inst. freq - average freq)	",
"	wint=		windowing for freqw				",
"			windowing for thin				",
"			default=1 					",
" 			o--------o--------o				",
" 			data-1	data	data+1				",
" 									",
" Notes:								",
" This program performs complex trace attribute analysis. The first three",
" attributes, amp,phase,freq are the classical Taner, Kohler, and	",
" Sheriff, 1979.							",
"  									",
" The unwrapping algorithm is the \"simple\" unwrapping algorithm that	",
" searches for jumps in phase.						",
" 									",
" The quantity dphase_min is the minimum change in the phase angle taken",
" to be the result of phase wrapping, rather than natural phase	 ",
" variation in the data. Setting unwrap=0 turns off phase-unwrapping	",
" alltogether. Choosing  unwrap > 1 makes the unwrapping function more	",
" sensitive to instantaneous phase changes.				",
" Setting unwrap > 1 may be necessary to resolve higher frequencies in	",
" data (or sample data more finely).					",
"	 					       			",
" Examples:								",
" suvibro f1=10 f2=50 t1=0 t2=0 tv=1 | suattributes2 mode=amp | ...	",
" suvibro f1=10 f2=50 t1=0 t2=0 tv=1 | suattributes2 mode=phase | ...	",
" suvibro f1=10 f2=50 t1=0 t2=0 tv=1 | suattributes2 mode=freq | ...	",
" suplane | suattributes mode=... | supswigb |...       		",
"	 					       			",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen
 *      CWP: John Stockwell (added freq and unwrap features)
 *	UGM (Geophysics Students): Agung Wiyono
 *	   email:aakanjas@gmail.com (others) added more attributes
 *					
 *
 * Algorithm:
 *	c(t) = hilbert_tranform_kernel(t) convolved with data(t)  
 *
 *  amp(t) = sqrt( c.re^2(t) + c.im^2(t))
 *  phase(t) = arctan( c.im(t)/c.re(t))
 *  freq(t) = d(phase)/dt
 *
 * Reference: Taner, M. T., Koehler, A. F., and  Sheriff R. E.
 * "Complex seismic trace analysis", Geophysics,  vol.44, p. 1041-1063, 1979
 *
 * Trace header fields accessed: ns, trid
 * Trace header fields modified: d1, trid

 */
/**************** end self doc ********************************/

#define	AMP		 1
#define	ARG		 2
#define	FREQ		 3
#define BANDWIDTH	 4
#define NORMAMP		 5
#define FREQW		 6
#define THIN		 7
#define FENV		 8
#define SENV		 9
#define Q		 10
#define UPHASE		 11

/* function prototype of functions used internally */
void differentate1d(int n, float h, float *f);
void twindow(int nt, int wtime, float *data);

segy tr;

int
main(int argc, char **argv)
{
	cwp_String mode;	/* display: real, imag, amp, arg	*/
	int imode=AMP;		/* integer abbrev. for mode in switch	*/
	register complex *ct=NULL;	/* complex trace		*/
	int nt;			/* number of points on input trace	*/
	float dt;		/* sample spacing			*/
	float *data=NULL;	/* array of data from each trace	*/
	float *hdata=NULL;	/* array of Hilbert transformed data	*/
	float unwrap;		/* PI/unwrap=min dphase assumed to by wrap*/
	int wint;		/* n time sampling to window */
	cwp_Bool seismic;	/* is this seismic data?		*/
	int ntout;
	int trend=0;		/* =1 remove trend from instantaneous phase */
	int zeromean=0;		/* =1 assume zero mean inst. phase func. */

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get info from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	ntout = nt + nt -1;

	/* check to see if data type is seismic */
	seismic = ISSEISMIC(tr.trid);

	if (!seismic)
		warn("input is not seismic data, trid=%d", tr.trid);

	/* Get mode; note that imode is initialized to AMP */
	if (!getparstring("mode", &mode))	mode = "amp";
	if (!getparint("trend", &trend))	trend=0;

	if      (STREQ(mode, "phase"))  imode = ARG;
	else if (STREQ(mode, "freq"))	imode = FREQ;
	else if (STREQ(mode, "uphase"))  imode = UPHASE;
	else if (STREQ(mode, "bandwidth")) imode = BANDWIDTH;
	else if (STREQ(mode, "normamp")) imode = NORMAMP;
	else if (STREQ(mode, "freqw")) imode = FREQW;
	else if (STREQ(mode, "thin")) imode = THIN;
	else if (STREQ(mode, "fdenv")) imode = FENV;
	else if (STREQ(mode, "sdenv")) imode = SENV;
	else if (STREQ(mode, "q")) imode = Q;
	else if (!STREQ(mode, "amp"))
		err("unknown mode=\"%s\", see self-doc", mode);

	/* getpar value of unwrap */
	switch(imode) {
	case FREQ:
		if (!getparfloat("unwrap", &unwrap))	unwrap=1;
	break;
	case UPHASE:
		if (!getparfloat("unwrap", &unwrap))	unwrap=1;
	break;
	case Q:
		if (!getparfloat("unwrap", &unwrap))	unwrap=1;
	break;
	case FREQW:
		if (!getparfloat("unwrap", &unwrap))	unwrap=1;
		if (!getparint("wint", &wint))	wint=3; 
	break;
	case THIN:
		if (!getparfloat("unwrap", &unwrap))	unwrap=1;
		if (!getparint("wint", &wint))	wint=3;
	break;
	case ARG:
		if (!getparfloat("unwrap", &unwrap))	unwrap=0;
	break;
	}

	checkpars();

	/* allocate space for data and hilbert transformed data, cmplx trace */
	data = ealloc1float(nt);
	hdata = ealloc1float(nt);
	ct = ealloc1complex(nt);

	/* Loop over traces */
	do {
		register int i;

		/* Get data from trace */
		for (i = 0; i < nt; ++i)  data[i] = tr.data[i];

		
		/* construct quadrature trace with hilbert transform */
		hilbert(nt, data, hdata);

		/* build the complex trace */
		for (i = 0; i < nt; ++i)  ct[i] = cmplx(data[i],hdata[i]);

		/* Form absolute value, phase, or frequency */
		switch(imode) {
		case AMP:
			for (i = 0; i < nt; ++i) {
				float re = ct[i].r;
				float im = ct[i].i;
				tr.data[i] = sqrt(re*re + im*im);
			}
			
			/* set trace id */
			tr.trid = ENVELOPE;
		break;
		case ARG:
		{
			float *phase = ealloc1float(nt);

			/* capture real and imaginary parts of the data */
			/* calculate unwrapped phase			*/
			for (i = 0; i < nt; ++i) {
				float re = ct[i].r;
				float im = ct[i].i;
				if (re*re+im*im){
					phase[i] = atan2(im, re);
				} else {
					phase[i] = 0.0;
					}
			}
			    
			/* unwrap the phase */
			if (unwrap!=0) 
			simple_unwrap_phase(nt, trend, zeromean, unwrap, phase);
			
			/* write phase values to tr.data */
			for (i = 0; i < nt; ++i) tr.data[i] = phase[i];
			
			/* set trace id */
			tr.trid = INSTPHASE;
		}
		break;
		case FREQ:
		{
			float *freq = ealloc1float(nt);
			float *u = ealloc1float(nt);
			float *uprime = ealloc1float(nt);
			float *v = ealloc1float(nt);
			float *vprime = ealloc1float(nt);
			float	fnyq = 0.5 / dt;

			for (i = 0; i < nt; ++i) {
				u[i] = ct[i].r;
				uprime[i] = ct[i].r;
				v[i] = ct[i].i;
				vprime[i] = ct[i].i;
			}

			/* compute inst. frequency by computing the	*/
			/* derivative of the instantaneous phase.	*/
			/* Note that: 					*/
			/* freq(t) = d/dt[ phase[t] ] 			*/ 
			/*  = d/dt ( arctan (v/u) )			*/
			/*   = [ 1/ (1 +(v/u)^2) ] ( v'/u - vu'/u^2 )	*/
			/*   = ( v'u - vu' )/(u^2 +v^2)			*/

			differentate1d(nt, 2.0*PI*dt,uprime );
			differentate1d(nt, 2.0*PI*dt,vprime );

			for (i=0; i < nt ; ++i){
				float num = (vprime[i]*u[i] - v[i]*uprime[i]);
				float den = (u[i]*u[i] + v[i]*v[i]);
			
				if (ABS(den)>FLT_EPSILON){
					freq[i] = num/den;
				} else {
					freq[i] = 0.0;
				}
			
				/* correct values greater nyquist frequency */
				if (freq[i] > fnyq)
					freq[i] = 2 * fnyq - freq[i];
				if (freq[i] < 0 )
					freq[i] = ABS(freq[i]);

				/* write freq(t) values to tr.data */
				tr.data[i] = freq[i];
			}
					

			/* set trace id */
			tr.trid = INSTFREQ;
		}
		break;
		case UPHASE:
		{
			float *phase = ealloc1float(nt);

			for (i = 0; i < nt; ++i) {
				float re = ct[i].r;
				float im = ct[i].i;
				if (re*re+im*im) {
					phase[i] = atan2(im, re);
				} else {
					phase[i] = 0.0;
				}
				
			}

			/* unwrap the phase */
			simple_unwrap_phase(nt, trend, zeromean, unwrap, phase);

			/* write phase values into the trace*/
			for (i=0 ; i < nt; ++i) tr.data[i] = phase[i];

			/* set trace id */
			tr.trid = INSTPHASE;
		}
		break;
		case FREQW:
		{
			float	fnyq = 0.5 / dt;
			float *u = ealloc1float(nt);
			float *uprime = ealloc1float(nt);
			float *v = ealloc1float(nt);
			float *vprime = ealloc1float(nt);
			float *freq = ealloc1float(nt);
			float *envelop = ealloc1float(nt);
			float *envelop2 = ealloc1float(nt);

			for (i = 0; i < nt; ++i) {
				u[i] = ct[i].r;
				uprime[i] = ct[i].r;
				v[i] = ct[i].i;
				vprime[i] = ct[i].i;
			}

			/* compute inst. frequency by computing the	*/
			/* derivative of the instantaneous phase.	*/
			/* Note that: 					*/
			/* freq(t) = d/dt[ phase[t] ] 			*/ 
			/*  = d/dt ( arctan (v/u) )			*/
			/*   = [ 1/ (1 +(v/u)^2) ] ( v'/u - vu'/u^2 )	*/
			/*   = ( v'u - vu' )/(u^2 +v^2)			*/

			differentate1d(nt, 2.0*PI*dt,uprime );
			differentate1d(nt, 2.0*PI*dt,vprime );


			for (i=0; i < nt ; ++i){
				float num = (vprime[i]*u[i] - v[i]*uprime[i]);
				float den = (u[i]*u[i] + v[i]*v[i]);
			

				if (ABS(den)>FLT_EPSILON){
					freq[i] = num/den;
				} else {
					freq[i] = 0.0;
				}
			
				/* correct values greater nyquist frequency */
				if (freq[i] > fnyq)
					freq[i] = 2 * fnyq - freq[i];
				if (freq[i] < 0 )
					freq[i] = ABS(freq[i]);
			
				envelop[i] = sqrt(den);

				envelop2[i]=envelop[i]*freq[i];

			}
			
			twindow(nt, wint, envelop);
			twindow(nt, wint, envelop2);

			/* correct values greater nyquist frequency */
			for (i=0 ; i < nt; ++i) {
			freq[i] = (envelop[i] == 0.0) ? 0.0 :envelop2[i]/envelop[i];
			}

			/* write freq(t) values to tr.data */
			for (i=0 ; i < nt; ++i) tr.data[i] = freq[i];
			
			/* set trace id */
			tr.trid = INSTFREQ;
		}
		break;
		case THIN:
		{
			float	fnyq = 0.5 / dt;
			float *phase = ealloc1float(nt);
			float *freqw = ealloc1float(nt);
			float *phase2 = ealloc1float(nt);
			

			/* calculate the unwrapped phase */
			for (i = 0; i < nt; ++i) {
				float re = ct[i].r;
				float im = ct[i].i;

				if (re*re+im*im) {
					phase[i] = atan2(im, re);
				} else {
					phase[i] = 0.0;
				}
			}

			/* unwrap the phase */
			simple_unwrap_phase(nt, trend, zeromean, unwrap, phase);

			/* compute freq(t)=dphase/dt */
			differentate1d(nt, 2.0*PI*dt, phase);

			/* correct values greater nyquist frequency */
			for (i=0 ; i < nt; ++i)	{
				if (phase[i] > fnyq)
					phase[i] = 2 * fnyq - phase[i];
				phase2[i]=phase[i];
			}
			/* Do windowing for Average Ins . Freq over wint*/
			twindow(nt, wint, phase2);

			for (i=0 ; i < nt; ++i)	{
				freqw[i] = phase[i] - phase2[i];
			/*	if (abs(freqw[i]) > fnyq)
				freqw[i] = 2 * fnyq - freqw[i];
			*/
			/* write Thin-Bed(t) values to tr.data */
				tr.data[i] = freqw[i];
				}
			/* set trace id */
			tr.trid = INSTFREQ;
		}
		break;
		case BANDWIDTH:
		{
			float *envelop = ealloc1float(nt);
			float *envelop2 = ealloc1float(nt);

		/* Bandwidth (Barnes 1992)

			  |d(envelope)/dt|
		band =abs |--------------|
			  |2 PI envelope |
	 	*/

			for (i = 0; i < nt; ++i) {
				float er = ct[i].r;
				float em = ct[i].i;
				envelop[i] = sqrt(er*er + em*em);
				envelop2[i]=sqrt(er*er + em*em);

			}
				differentate1d(nt, dt, envelop);

				for (i = 0; i < ntout; ++i) {
				   if (2.0*PI*envelop2[i]!=0.0) {
					tr.data[i] = ABS(envelop[i]/(2.0*PI*envelop2[i]));
				   } else {
					tr.data[i]=0.0;
				   }
				}
				tr.trid = ENVELOPE;
		}
		break;
		case NORMAMP:
		{
			float phase;
			float *na = ealloc1float(nt);
			for (i = 0; i < nt; ++i) {
				float re = ct[i].r;
				float im = ct[i].i;
				if (re*re+im*im)  phase = atan2(im, re);
				else	      phase = 0.0;
				na[i] = cos(phase);
			}
			for (i=0 ; i < nt; ++i) tr.data[i] = na[i];
			
			/* set trace id */
			tr.trid = INSTPHASE;
			}
		break;
		case FENV:
		{
			float *amp = ealloc1float(nt);
			for (i = 0; i < nt; ++i) {
				float re = ct[i].r;
				float im = ct[i].i;
				amp[i] = sqrt(re*re + im*im);
			}
		/*conv(nt, 0, envelop, nt, 0, time, ntout, 0, ouput);*/

			differentate1d(nt, 2.0*PI*dt, amp);

			for (i=0 ; i < nt; ++i) tr.data[i] = amp[i];

			/* set trace id */
			tr.trid = ENVELOPE;
		}
		break;
		case SENV:
		{
			float *amp = ealloc1float(nt);
			for (i = 0; i < nt; ++i) {
				float re = ct[i].r;
				float im = ct[i].i;
				amp[i] = sqrt(re*re + im*im);
			}

		differentate1d(nt, 2.0*PI*dt, amp);
		differentate1d(nt, 2.0*PI*dt, amp);
		for (i=0 ; i < nt; ++i) tr.data[i] = amp[i];
			/* set trace id */
			tr.trid = ENVELOPE;
		}
		break;

		case Q:
		{
			float *envelop = ealloc1float(nt);
			float *envelop2 = ealloc1float(nt);
			float *phase = ealloc1float(nt);
			float	fnyq = 0.5 / dt;

		/* Bandwidth (Barnes 1992)

			-PI Freq(t) d(envelope)/dt
		band =  --------------------------
				 envelope(t)
	 	*/

			for (i = 0; i < nt; ++i) {
				float re = ct[i].r;
				float im = ct[i].i;
				envelop[i] = sqrt(re*re + im*im);
				envelop2[i]=sqrt(re*re + im*im);
				if (re*re+im*im) {
					phase[i] = atan2(im, re);
				} else {
					phase[i] = 0.0;
				}

			}
			/* get envelope diff */
			differentate1d(nt, dt, envelop);

			/* unwrap the phase */
			simple_unwrap_phase(nt, trend, zeromean, unwrap, phase);

			/* compute freq(t)=dphase/dt */
			differentate1d(nt, 2.0*PI*dt, phase);

			for (i=0 ; i < nt; ++i)	{
				if (phase[i] > fnyq)
					phase[i] = 2 * fnyq - phase[i];
			}

			for (i = 0; i < ntout; ++i) {
				if (envelop[i]!=0.0)
				tr.data[i] = -1*PI*phase[i]*envelop2[i]/envelop[i];
				else
				tr.data[i]=0.0;
				}
				tr.trid = INSTFREQ;
		}
		break;
		default:
			err("%s: mysterious mode=\"%s\"", __LINE__, mode);
		}


		tr.d1 = dt;   /* for graphics */
		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}


void differentate1d(int n, float h, float *f)
/************************************************************************
differentate1d - compute the 1st derivative of a function f[]
************************************************************************
Input:
n		number of samples
h		sample rate
f		array[n] of input values

Output:
f		array[n], the derivative of f
************************************************************************
Notes:
This is a simple 2 point centered-difference differentiator.
The derivatives at the endpoints are computed via 2 point leading and
lagging differences. 
************************************************************************
Author: John Stockwell, CWP, 1994
************************************************************************/
{
	int i;	
	float *temp;
	float h2=2*h;

	/* allocate space in temporary vector */
	temp = ealloc1float(n);

	/* do first as a leading difference */
	temp[0] = (f[1] - f[0])/h;

	/* do the middle values as a centered difference */
	for (i=1; i<n-1; ++i) temp[i] = (f[i+1] - f[i-1])/h2;

	/* do last value as a lagging difference */
	temp[n-1] = (f[n-1] - f[n-2])/h;

	for (i=0 ; i < n ; ++i) f[i] = temp[i];

	free1float(temp);
}

void twindow(int nt, int wtime, float *data)
/************************************************************
twindow - simple time gating
*************************************************************
Input:
nt	number of time samples
wtime	= n*dt   where n are integer ex=1,2,3,4,5,...
	  wtime=3 as default
 used for Frequency Weighted and Thin-bed attributes
*************************************************************
Author:	UGM (Geophysics Students): Agung Wiyono, 2005
************************************************************/
{
	float val;
	float *temp;
	int i;
	float sum;
	int nwin;
	
	nwin=2*wtime+1;
	temp = ealloc1float(nt);
	sum=0.0;
	for (i = 0; i< wtime+1; ++i) {
		val = data[i];
		sum +=val;
	}
	/* weighted */
	temp[0] = sum/nwin;
	
	/* dt<wtime */
	for (i = 1; i < wtime; ++i) {
		val = data[i+wtime];
		sum+=val;
		++nwin;
		temp[i] = sum/nwin;
		}
	/*wtime<dt<dt-wtime */
	for (i = wtime ; i < nt-wtime; ++i) {
		val = data[i+wtime];
		sum += val;
		val = data[i-wtime];
		sum -=val;
		temp[i] = sum/nwin;
	}

	/*dt-wtime<dt*/
	for (i = nt - wtime; i < nt; ++i) {
		val = data[i-wtime];
		sum -= val;
		--nwin;
		temp[i] = sum/nwin;
	}
	
	
	for (i=0; i<nt; ++i) data[i] = temp[i];

	/* Memory free */
	free1float(temp);
}



