/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPHASEVEL: $Revision: 1.5 $ ; $Date: 2013/05/29 21:23:23 $		*/

#include "su.h"
#include "segy.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUPHASEVEL - Multi-mode PHASE VELocity dispersion map computed",
"              from shot record(s)				",
" 								",
" suphasevel <infile >outfile [optional parameters]		",
"								",
" Optional parameters:						",
" fv=330	minimum phase velocity (m/s)			",
" nv=100	number of phase velocities			",
" dv=25		phase velocity step (m/s)			",
" fmax=50	maximum frequency to process (Hz)		",
"		=0 process to nyquist				",
" norm=0	do not normalize by amplitude spectrum		",
"		=1 normalize by amplitude spectrum		",
" verbose=0	verbose = 1 echoes information			",
" 								",
" Notes:  Offsets read from headers.			 	",
"  1. output is complex frequency data				",
"  2. offset header word must be set (signed offset is ok)	",
"  3. norm=1 tends to blow up aliasing and other artifacts	",
"  4. For correct suifft later, use fmax=0			",
"  5. For later processing outtrace.dt=domega			",
"  6. works for 2D or 3D shots in any offset order		",
"								",
"								",
" Using this program:						",
"								",
" First: use 							",
" 	suspecfx < shotrecord.su | suximage			",
" to see what the maximum bandwidth is in your data. This will	",
" give you an idea about the possible value for fmax.		",
"								",
" Second: Plot your data or some subset of your data via:	",
"     suxwigb < shotrecord.su key=offset			",
"								",
" You can then estimate the range of phase velocities by looking",
" at the maximum and minimum slopes of arrivals in your data.	",
" This will allow you do set first velocity fv and the increment",
" in velocity dv, that make sense for your data.		",
" 								",
" You can pick values of offset and time by placing the cursor  ",
" on the desired location on the plot and pressing the \'s\' key",
" The picks will appear in your terminal window. 		",
"								",
" When displaying, don't forget to use suamp to compute the	",
" modulus of the complex values that this program puts out.	",
"								",
"   suphasevel < shotrecord.su [parameters] | suamp | suximage	",
"								",
" 								",
NULL};

/* Credits:
 *
 *	UHouston: Chris Liner June2008 (cloned from suspecfk)
 *
 *  This code implements the following integral transform
 *             _
 *            /
 *  u(w,v) = / k(w,x,v) u(w,x) dx
 *         _/
 *  where
 *	u(w,v) is the phase velocity dispersion image
 *	k(w,x,v) is the transform kernel.... exp(-i w x / v)
 *	u(w,x) = FT[u(t,x)] is the input shot record(s) 
 *         _/
 * Reference: Park, Miller, and Xia (1998, SEG Abstracts)
 *
 * Trace header fields accessed: dt, offset, ns
 * Trace header fields modified: nx,dt,trid,d1,f1,d2,f2,tracl
 */
/**************** end self doc ***********************************/

#define	I		cmplx(0.0, 1.0)
#define PFA_MAX	720720	/* Largest allowed nfft	          	*/
#define MAX_OFFS 10000 	/* Largest allowable offset		*/

/* Prototype */
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
FILE *tracefp;		/* fp for trace storage file		*/

segy intrace, outtrace;

int
main(int argc, char **argv)
{
	int nt,nx;		/* numbers of samples			*/
	float dt;		/* sampling intervals			*/
	int it,ix;		/* sample indices			*/
	int ntfft;		/* dimensions after padding for FFT	*/
	int nF;			/* transform (output) dimensions	*/
	int iF;			/* transform sample indices		*/

	register complex **ct=NULL;	/* complex FFT workspace	*/
	register float **rt=NULL;	/* float FFT workspace		*/

	int verbose;		/* flag for echoing information		*/

	char *tmpdir=NULL;	/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path		*/

	float v,fv,dv;		/* phase velocity, first, step	 	*/
	float amp,oamp;		/* temp vars for amplitude spectrum	*/
	int nv,iv;		/* number of phase vels, counter	*/
	float x;		/* offset  				*/
	float omega;		/* circular frequency			*/
	float domega;		/* circular frequency spacing (from dt)	*/
	float onfft;		/* 1 / nfft				*/
	float phi;		/* omega/phase_velocity			*/
	complex *cDisp=NULL;	/* temp array for complex dispersion	*/
	float arg;		/* temp var for phase calculation	*/
	complex cExp;		/* temp vars for phase calculation	*/
	float *offs=NULL;	/* input data offsets			*/
	float fmax;		/* max freq to proc (Hz)    		*/

	int out;		/* output real or abs v(f) spectrum	*/
	int norm;		/* normalization flag			*/

	float xmax;		/* maximum abs(offset) of input		*/
	float twopi, f;		/* constant and frequency (Hz)		*/



	/* Hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!gettr(&intrace))  err("can't get first trace");
	nt = intrace.ns;

	/* dt is used only to set output header value d1 */
	if (!getparfloat("dt", &dt)) {
		if (intrace.dt) { /* is dt field set? */
			dt = ((double) intrace.dt)/ 1000000.0;
		} else { /* dt not set, exit */
			err("tr.dt not set, stop.");
		}
	}
	warn("dt=%f",dt);
	if (!getparfloat("fv",&fv))	fv   = 330;
	if (!getparfloat("dv",&dv))     dv   = 25;
	if (!getparint("nv",&nv))       nv   = 100;
	if (!getparint("out",&out))     out  = 0;
	if (!getparint("norm",&norm))   norm = 0;
	if (!getparfloat("fmax",&fmax)) fmax = 50;

	if (!getparint("verbose", &verbose))	verbose = 0;

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);


        checkpars();

	/* Set up tmpfile */
	if (STREQ(tmpdir,"")) {
		tracefp = etmpfile();
		if (verbose) warn("using tmpfile() call");
	} else { /* user-supplied tmpdir */
		char directory[BUFSIZ];
		strcpy(directory, tmpdir);
		strcpy(tracefile, temporary_filename(directory));
		/* Trap signals so can remove temp files */
		signal(SIGINT,  (void (*) (int)) closefiles);
		signal(SIGQUIT, (void (*) (int)) closefiles);
		signal(SIGHUP,  (void (*) (int)) closefiles);
		signal(SIGTERM, (void (*) (int)) closefiles);
		tracefp = efopen(tracefile, "w+");
      		istmpdir=cwp_true;		
		if (verbose) warn("putting temporary files in %s", directory);
	}
	
	/* we have to allocate offs(nx) before we know nx */
	offs = alloc1float(MAX_OFFS);	

	ix = 0;
	nx = 0;
	xmax = 0.0;
	
	/* get nx and max abs(offset) */
	do { 
		++nx;
		efwrite(intrace.data, FSIZE, nt, tracefp);
		offs[ix] = intrace.offset;
		if ( abs(intrace.offset) > xmax ) xmax = abs(intrace.offset);
		++ix;
	} while (gettr(&intrace));
	
	/* confirm that offsets are set */
	if ( xmax == 0.0 ) err("tr.offset not set, stop.");


	/* Determine lengths for prime-factor FFTs */
	ntfft = npfar(nt);
	if (ntfft >= SU_NFLTS || ntfft >= PFA_MAX)
			err("Padded nt=%d--too big",ntfft);

	/* Determine complex transform sizes */
	nF = ntfft/2+1;  /* must be this nF for fft */
        onfft = 1.0 / ntfft;
	twopi = 2.0 * PI;
	domega = twopi * onfft / dt;

	/* Allocate space */
	ct = alloc2complex(nF,nx);
	rt = alloc2float(ntfft,nx);

	/* Load traces into fft arrays and close tmpfile */
	erewind(tracefp);
	for (ix=0; ix<nx; ++ix) {

		efread(rt[ix], FSIZE, nt, tracefp);

		/* pad dimension 1 with zeros */
		for (it=nt; it<ntfft; ++it)  rt[ix][it] = 0.0;
	}
	efclose(tracefp);
	
	/* Fourier transform dimension 1 */
	pfa2rc(1,1,ntfft,nx,rt[0],ct[0]);

	/* set nF for processing */
	if (fmax == 0) { 	
		/* process to nyquist */
		nF = ntfft/2+1;
	} else {
		/* process to given fmax */
		nF = (int) (twopi * fmax / domega);
	}
	
	/* data now in (w,x) domain 
	   allocate arrays  */
	cDisp = alloc1complex(nF);	
	
	/* if requested, normalize by amplitude spectrum 
	    (normalizing by amplitude blows up aliasing and other artifacts) */			
	if (norm == 1) {
		for (iF=0; iF<nF; ++iF)  {
			/* calc this frequency */
			omega = iF * domega;
			f = omega / twopi;
			/* loop over traces */
			for (ix=0; ix<nx; ++ix) {
				/* calc amplitude at this (f,x) location */
				amp = rcabs(ct[ix][iF]);
				oamp = 1.0/amp;
				/* scale field by amp spectrum */
				ct[ix][iF] = crmul(ct[ix][iF],oamp);
			}
		}
	}
	
	/* set global output trace headers */
	outtrace.ns = 2 * nF;
	outtrace.dt = dt*1000000.;  
	outtrace.trid = FUNPACKNYQ;
	outtrace.d1 = 1.0 / (ntfft * dt); /* Hz */
	outtrace.f1 = 0;
	outtrace.d2 = dv;
	outtrace.f2 = fv;

	/* loop over phase velocities */
	for (iv=0; iv<nv; ++iv) {

		/* this velocity */
		v = fv + iv*dv;
	
		/* loop over frequencies */
		for (iF=0; iF<nF; ++iF)  {

			/* this frequency and phase */
			omega = iF * domega;
			f = omega / twopi;
			phi = omega / v;

			/* initialize */
			cDisp[iF] = cmplx(0.0,0.0);		

			/* sum over abs offset (this is ok for 3D, too) */
			for (ix=0; ix<nx; ++ix) {

				/* get this x */
				x = abs(offs[ix]);

				/* target phase */
				arg = - phi * x;
				cExp = cwp_cexp(crmul(cmplx(0.0,1.0), arg));
				
				/* phase vel profile for this frequency */				 
				cDisp[iF] = cadd(cDisp[iF],cmul(ct[ix][iF],cExp));
			}
			
		}
		
		/* set trace counter */
		outtrace.tracl = iv + 1;
			
		/* copy results to output trace 
		   interleaved format like sufft.c */
		for (iF = 0; iF < nF; ++iF) {
			outtrace.data[2*iF]   = cDisp[iF].r;
			outtrace.data[2*iF+1] = cDisp[iF].i;
		}
		
		/* output freqs at this vel */
		puttr(&outtrace);

	}  /* next frequency */
	
	
	/* Clean up */
	if (istmpdir) eremove(tracefile);
	return(CWP_Exit());
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(tracefp);
	eremove(tracefile);
	exit(EXIT_FAILURE);
}
