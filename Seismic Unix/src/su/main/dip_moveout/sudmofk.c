/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUDMOFK: $Revision: 1.20 $ ; $Date: 2011/11/16 17:51:02 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUDMOFK - DMO via F-K domain (log-stretch) method for common-offset gathers",
"									",
" sudmofk <stdin >stdout cdpmin= cdpmax= dxcdp= noffmix= [...]		",
"									",
" Required Parameters:							",
" cdpmin	minimum cdp (integer number) for which to apply DMO	",
" cdpmax	maximum cdp (integer number) for which to apply DMO	",
" dxcdp		distance between adjacent cdp bins (m)			",
" noffmix	number of offsets to mix (see notes)			",
"									",
" Optional Parameters:							",
" tdmo=0.0	times corresponding to rms velocities in vdmo (s)	",
" vdmo=1500.0	rms velocities corresponding to times in tdmo (m/s)	",
" sdmo=1.0	DMO stretch factor; try 0.6 for typical v(z)		",
" fmax=0.5/dt	maximum frequency in input traces (Hz)			",
" verbose=0	=1 for diagnostic print					",
" tmpdir=	if non-empty, use the value as a directory path	prefix	",
"		for storing temporary files; else if the CWP_TMPDIR	",
"		environment variable is set use	its value for the path;	",
"		else use tmpfile()					",
"									",
" Notes:								",
" Input traces should be sorted into common-offset gathers.  One common- ",
" offset gather ends and another begins when the offset field of the trace",
" headers changes.							",
"									",
" The cdp field of the input trace headers must be the cdp bin NUMBER, NOT",
" the cdp location expressed in units of meters or feet.		",
"									",
" The number of offsets to mix (noffmix) should typically no smaller than",
" the ratio of the shotpoint spacing to the cdp spacing.  This choice	",
" ensures that every cdp will be represented in each offset mix.  Traces ",
" in each mix will contribute through DMO to other traces in adjacent cdps",
" within that mix. (Values of noffmix 2 or 3 times the ratio of shotpoint",
" spacing to the cdp spacing often yield better results.)		",
"									",
" The tdmo and vdmo arrays specify a velocity function of time that is	",
" used to implement a first-order correction for depth-variable velocity.",
" The times in tdmo must be monotonically increasing.			",
"									",
" For each offset, the minimum time at which a non-zero sample exists is ",
" used to determine a mute time.  Output samples for times earlier than this", 
" mute time will be zeroed.  Computation time may be significantly reduced",
" if the input traces are zeroed (muted) for early times at large offsets.",
NULL};

/* Credits:
 *	CWP: Dave
 *
 * Technical Reference:
 *	Dip-Moveout Processing - SEG Course Notes
 *	Dave Hale, 1988
 *
 * Trace header fields accessed:  ns, dt, delrt, offset, cdp.
 */
/**************** end self doc *******************************************/

static void mkvrms (int ntdmo, float *tdmo, float *vdmo,
	int nt, float dt, float ft, float *vrms);
static void dmooff (float offset, float fmax, float sdmo, int nx, float dx,
	int nt, float dt, float ft, float *vrms, float **ptx);
static void maketu (float offset, int itmin, float fmax,
	int nt, float dt, float ft, float *vrms, float **uoftp,
	int *nup, float *dup, float *fup, float **tofup, float *tconp);
static void closefiles(void);


/* Globals (so can trap signal) defining temporary disk files */
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *headerfp;		/* fp for header storage file		*/

segy tr,tro;

int
main(int argc, char **argv)
{
	int nt;		/* number of time samples per trace */
	float dt;	/* time sampling interval */
	float ft;	/* time of first sample */
	int it;		/* time sample index */
	int cdpmin;	/* minimum cdp to process */
	int cdpmax;	/* maximum cdp to process */
	float dx;	/* cdp sampling interval */
	int nx;		/* number of cdps to process */
	int nxfft;	/* number of cdps after zero padding for fft */
	int nxpad;	/* minimum number of cdps for zero padding */
	int ix;		/* cdp index, starting with ix=0 */
	int noffmix;	/* number of offsets to mix */
	float *tdmo;	/* times at which rms velocities are specified */
	float *vdmo;	/* rms velocities at times specified in tdmo */
	float sdmo;	/* DMO stretch factor */
	int ntdmo;	/* number tnmo values specified */
	int itdmo;	/* index into tnmo array */
	int nvdmo;	/* number vnmo values specified */
	float fmax;	/* maximum frequency */
	float *vrms;	/* uniformly sampled vrms(t) */
	float **p;	/* traces for one offset - common-offset gather */
	float **q;	/* DMO-corrected and mixed traces to be output */
	float offset;	/* source-receiver offset of current trace */
	float oldoffset;/* offset of previous trace */
	int noff;	/* number of offsets processed in current mix */
	int ntrace;	/* number of traces processed in current mix */
	int itrace;	/* trace index */
	int gottrace;	/* non-zero if an input trace was read */
	int done;	/* non-zero if done */
	int verbose;	/* =1 for diagnostic print */
	char *tmpdir;	/* directory path for tmp files	*/
	cwp_Bool istmpdir=cwp_false;/* true for user given path */

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	ft = tr.delrt/1000.0;
	offset = tr.offset;

	/* get parameters */
	if (!getparint("cdpmin",&cdpmin)) err("must specify cdpmin");
	if (!getparint("cdpmax",&cdpmax)) err("must specify cdpmax");
	if (cdpmin>cdpmax) err("cdpmin must not be greater than cdpmax");
	if (!getparfloat("dxcdp",&dx)) err("must specify dxcdp");
	if (!getparint("noffmix",&noffmix)) err("must specify noffmix");
	ntdmo = countparval("tdmo");
	if (ntdmo==0) ntdmo = 1;
	tdmo = ealloc1float(ntdmo);
	if (!getparfloat("tdmo",tdmo)) tdmo[0] = 0.0;
	nvdmo = countparval("vdmo");
	if (nvdmo==0) nvdmo = 1;
	if (nvdmo!=ntdmo) err("number of tdmo and vdmo must be equal");
	vdmo = ealloc1float(nvdmo);
	if (!getparfloat("vdmo",vdmo)) vdmo[0] = 1500.0;
	for (itdmo=1; itdmo<ntdmo; ++itdmo)
		if (tdmo[itdmo]<=tdmo[itdmo-1])
			err("tdmo must increase monotonically");
	if (!getparfloat("sdmo",&sdmo)) sdmo = 1.0;
	if (!getparfloat("fmax",&fmax)) fmax = 0.5/dt;
	if (!getparint("verbose",&verbose)) verbose=0;
	
	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);
	
        checkpars();

	/* make uniformly sampled rms velocity function of time */
	vrms = ealloc1float(nt);
	mkvrms(ntdmo,tdmo,vdmo,nt,dt,ft,vrms);
	
	/* determine number of cdps to process */
	nx = cdpmax-cdpmin+1;
	
	/* allocate and zero common-offset gather p(t,x) */
	nxpad = 0.5*ABS(offset/dx);
	nxfft = npfar(nx+nxpad);
	p = ealloc2float(nt,nxfft+2);
	for (ix=0; ix<nxfft; ++ix)
		for (it=0; it<nt; ++it)
			p[ix][it] = 0.0;

	/* allocate and zero offset mix accumulator q(t,x) */
	q = ealloc2float(nt,nx);
	for (ix=0; ix<nx; ++ix)
		for (it=0; it<nt; ++it)
			q[ix][it] = 0.0;
		
	/* open temporary file for headers */
	if (STREQ(tmpdir,"")) {
		headerfp = etmpfile();
		if (verbose) warn("using tmpfile() call");
	} else { /* user-supplied tmpdir */
		char directory[BUFSIZ];
		strcpy(directory, tmpdir);
		strcpy(headerfile, temporary_filename(directory));
		/* Trap signals so can remove temp files */
		signal(SIGINT,  (void (*) (int)) closefiles);
		signal(SIGQUIT, (void (*) (int)) closefiles);
		signal(SIGHUP,  (void (*) (int)) closefiles);
		signal(SIGTERM, (void (*) (int)) closefiles);
		headerfp = efopen(headerfile, "w+");
      		istmpdir=cwp_true;		
		if (verbose)
			warn("putting temporary header file in %s", directory);
	}

	/* initialize */
	oldoffset = offset;
	gottrace = 1;
	done = 0;
	ntrace = 0;
	noff = 0;

	/* loop over traces */
	do {
		/* if got a trace, determine offset */
		if (gottrace) offset = tr.offset;
		
		/* if an offset is complete */
		if ((gottrace && offset!=oldoffset) || !gottrace) {
		
			/* do dmo for old common-offset gather */
			dmooff(oldoffset,fmax,sdmo,nx,dx,nt,dt,ft,vrms,p);
			
			/* add dmo-corrected traces to mix */
			for (ix=0; ix<nx; ++ix)
				for (it=0; it<nt; ++it)
					q[ix][it] += p[ix][it];
			
			/* count offsets in mix */
			noff++;
							
			/* free space for old common-offset gather */
			free2float(p);
			
			/* if beginning a new offset */
			if (offset!=oldoffset) {
				
				/* allocate space for new offset */
				nxpad = 0.5*ABS(offset/dx);
				nxfft = npfar(nx+nxpad);
				p = ealloc2float(nt,nxfft+2);
				for (ix=0; ix<nxfft; ++ix)
					for (it=0; it<nt; ++it)
						p[ix][it] = 0.0;
			}
		}
		
		/* if a mix of offsets is complete */
		if (noff==noffmix || !gottrace) {
			
			/* rewind trace header file */
			erewind(headerfp);
			
			/* loop over all output traces */
			for (itrace=0; itrace<ntrace; ++itrace) {
			
				/* read trace header and determine cdp index */
				efread(&tro,HDRBYTES,1,headerfp);
				
				/* get dmo-corrected data */
				memcpy( (void *) tro.data,
					(const void *)	q[tro.cdp-cdpmin],
					nt*sizeof(float));
				
				/* write output trace */
				puttr(&tro);
			}
			
			/* report */
			if (verbose) 
				fprintf(stderr,"\tCompleted mix of "
					"%d offsets with %d traces\n",
					noff,ntrace);
			
			/* if no more traces, break */
			if (!gottrace) break;
			
			/* rewind trace header file */
			erewind(headerfp);

			/* reset number of offsets and traces in mix */
			noff = 0;
			ntrace = 0;
			
			/* zero offset mix accumulator */
			for (ix=0; ix<nx; ++ix)
				for (it=0; it<nt; ++it)
					q[ix][it] = 0.0;
		}
			
		/* if cdp is within range to process */
		if (tr.cdp>=cdpmin && tr.cdp<=cdpmax) {
	
			/* save trace header and update number of traces */
			efwrite(&tr,HDRBYTES,1,headerfp);
			ntrace++;

			/* remember offset */
			oldoffset = offset;

			/* get trace samples */
			memcpy( (void *) p[tr.cdp-cdpmin],
				   (const void *) tr.data, nt*sizeof(float));
		}

		/* get next trace (if there is one) */
		if (!gettr(&tr)) gottrace = 0;
		
	} while (!done);

	/* clean up */
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	return(CWP_Exit());
}
	
static void mkvrms (int ndmo, float *tdmo, float *vdmo,
	int nt, float dt, float ft, float *vrms)
/*****************************************************************************
make uniformly sampled vrms(t) for DMO
******************************************************************************
Input:
ndmo		number of tdmo,vdmo pairs
tdmo		array[ndmo] of times
vdmo		array[ndmo] of rms velocities
nt		number of time samples
dt		time sampling interval
ft		first time sample

Output:
vrms		array[nt] of rms velocities
******************************************************************************
Author:	 Dave Hale, Colorado School of Mines, 10/03/91
*****************************************************************************/
{
	int it;
	float t,(*vdmod)[4];
	
	vdmod = (float(*)[4])ealloc1float(ndmo*4);
	cmonot(ndmo,tdmo,vdmo,vdmod);
	for (it=0,t=ft; it<nt; ++it,t+=dt)
		intcub(0,ndmo,tdmo,vdmod,1,&t,&vrms[it]);
	free1float((float*)vdmod);
}

static void dmooff (float offset, float fmax, float sdmo, int nx, float dx,
	int nt, float dt, float ft, float *vrms, float **ptx)
/*****************************************************************************
perform dmo in f-k domain for one offset
******************************************************************************
Input:
offset		source receiver offset
fmax		maximum frequency
sdmo		DMO stretch factor
nx		number of midpoints
dx		midpoint sampling interval
nt		number of time samples
dt		time sampling interval
ft		first time
vrms		array[nt] of rms velocities 
ptx		array[nx][nt] for p(t,x), zero-padded for fft (see notes)

Output:
ptx		array[nx][nt] for dmo-corrected p(t,x)
******************************************************************************
Notes:
To avoid having to allocate a separate work space that is larger than the
array ptx[nx][nt], ptx must be zero-padded to enable Fourier transform from x
to k via prime factor FFT.  nxpad (nx after zero-padding) can be estimated by
	nxpad = 2+npfar(nx+(int)(0.5*ABS(offset/dx)));
where npfar() is a function that returns a valid length for real-to-complex 
prime factor FFT.  ptx[nx] to ptx[nxfft-1] must point to zeros.
******************************************************************************
Author:	 Dave Hale, Colorado School of Mines, 08/08/91
*****************************************************************************/
{
	int nxfft,itmin,nu,nufft,nw,nk,ix,iu,iw,ik,it,iwn,
		iwmin,iwmax,nupad,ikmax;
	float dw,dk,tcon,wwscl,scale,scales,kmax,
		amp,phase,fr,fi,pwr,pwi,
		wmin,wmax,fftscl,du,fu,w,k,osdmo,*uoft,*tofu; 
	complex czero=cmplx(0.0,0.0),**ptk,*pu,*pw;

	/* number of cdps after padding for fft */
	nxfft = npfar(nx+(int)(0.5*ABS(offset/dx)));

	/* get minimum time of first non-zero sample */
	for (ix=0,itmin=nt; ix<nx; ++ix) {
		for (it=0; it<itmin && ptx[ix][it]==0.0; ++it);
		itmin = it;
	}
	
	/* if all zeros, simply return */
	if (itmin>=nt) return;
	
	/* make stretch and compress functions t(u) and u(t) */
	maketu(offset,itmin,fmax,nt,dt,ft,vrms,&uoft,&nu,&du,&fu,&tofu,&tcon);

	/* adjust DMO stretch factor for nominal error in log stretch; */
	/* solve sdmo*(sqrt(1-a/sdmo)-1) = 0.5*log(1-a), where a=0.5 */
	sdmo *= .62;

	/* inverse of dmo stretch factor */
	osdmo = 1.0/sdmo;

	/* maximum DMO shift (in samples) for any wavenumber k */
	nupad = 1.5*sdmo*tcon/du;
	
	/* frequency sampling */
	nufft = npfa(nu+nupad);
	nw = nufft;
	dw = 2.0*PI/(nufft*du);
	
	/* allocate workspace */
	pu = pw = ealloc1complex(nufft);
	
	/* wavenumber sampling and maximum wavenumber to apply dmo */
	nk = nxfft/2+1;
	dk = 2.0*PI/ABS(nxfft*dx);
	kmax = PI/ABS(dx);
	ikmax = NINT(kmax/dk);

	/* pointers to complex p(t,k) */
	ptk = (complex**)ealloc1(nk,sizeof(complex*));
	for (ik=0; ik<nk; ++ik)
		ptk[ik] = (complex*)ptx[0]+ik*nt;
	
	/* fft scale factor */
	fftscl = (float)nk/(float)(ikmax+1)/(nufft*nxfft);
	
	/* Fourier transform p(t,x) to p(t,k) */
	pfa2rc(-1,2,nt,nxfft,ptx[0],ptk[0]);

	/* loop over wavenumbers less than maximum */
	for (ik=0,k=0.0; ik<=ikmax; ++ik,k+=dk) {

		/* stretch p(t;k) to p(u) */
		ints8c(nt,dt,ft,ptk[ik],czero,czero,nu,tofu,pu);
		
		/* pad with zeros and Fourier transform p(u) to p(w) */
		for (iu=nu; iu<nufft; ++iu)
			pu[iu].r = pu[iu].i = 0.0;
		pfacc(1,nufft,pu);

		/* minimum and maximum frequencies to process */
		wmin = ABS(0.5*vrms[0]*k);
		wmax = ABS(PI/du);
		iwmin = MAX(1,MIN((nw-1)/2,NINT(wmin/dw)));
		iwmax = MAX(0,MIN((nw-1)/2,NINT(wmax/dw)));
		
		/* constant independent of w */
		wwscl = osdmo*pow(k*0.5*offset/tcon,2.0);
		
		/* zero dc (should be zero anyway) */
		pw[0].r = pw[0].i = 0.0;

		/* zero frequencies below minimum */
		for (iw=1,iwn=nw-iw; iw<iwmin; ++iw,--iwn)
			pw[iw].r = pw[iw].i = pw[iwn].r = pw[iwn].i = 0.0;
		
		/* do dmo between minimum and maximum frequencies */
		for (iw=iwmin,iwn=nw-iwmin,w=iwmin*dw; 
			iw<=iwmax; ++iw,--iwn,w+=dw) {
			scales = 1.0+wwscl/(w*w);
			scale = sqrt(scales);
			phase = sdmo*w*tcon*(scale-1.0);
			amp = fftscl*(1.0-sdmo+sdmo/scale);
			fr = amp*cos(phase);
			fi = amp*sin(phase);
			pwr = pw[iw].r;
			pwi = pw[iw].i;
			pw[iw].r = pwr*fr-pwi*fi;
			pw[iw].i = pwr*fi+pwi*fr;
			pwr = pw[iwn].r;
			pwi = pw[iwn].i;
			pw[iwn].r = pwr*fr+pwi*fi;
			pw[iwn].i = pwi*fr-pwr*fi;
		}

		/* zero frequencies above maximum to Nyquist (if present) */
		for (iw=iwmax+1,iwn=nw-iw; iw<=nw/2; ++iw,--iwn)
			pw[iw].r = pw[iw].i = pw[iwn].r = pw[iwn].i = 0.0;
		
		/* Fourier transform p(w) to p(u) */
		pfacc(-1,nufft,pu);
		
		/* compress p(u) to p(t;k) */
		ints8c(nu,du,fu,pu,czero,czero,nt,uoft,ptk[ik]);
	}

	/* zero wavenumber between maximum and Nyquist */
	for (; ik<nk; ++ik)
		for (it=0; it<nt; ++it)
			ptk[ik][it].r = ptk[ik][it].i = 0.0;
	
	/* Fourier transform p(t,k) to p(t,x) */
	pfa2cr(1,2,nt,nxfft,ptk[0],ptx[0]);
	
	/* free workspace */
	free1float(tofu);
	free1float(uoft);
	free1complex(pu);
	free1(ptk);
}

static void maketu (float offset, int itmin, float fmax,
	int nt, float dt, float ft, float *vrms, float **uoftp,
	int *nup, float *dup, float *fup, float **tofup, float *tconp)
/*****************************************************************************
make stretch and compress functions t(u) and u(t)
******************************************************************************
Input:
offset		source receiver offset
itmin		index of minimum first non-zero sample for this offset
fmax		maximum frequency
nt		number of time samples
dt		time sampling interval
ft		first time
vrms		array[nt] of rms velocities

Output:
uoftp		array[nt] of u(t)
nup		number of u (stretched t) samples
dup		u sampling interval
fup		first u
tofup		array[nu] of t(u)
tconp		time constant relating t(u) and u(t)
******************************************************************************
Author:	 Dave Hale, Colorado School of Mines, 08/08/91
*****************************************************************************/
{
	int it,numax,nu;
	float tmin,dumin,et,eu,t1,t2,
		v2,v22,v44,gamma,
		v2m,v22m,v44m,gammam,t,dv2,vi2,vi4,v24,
		*uoft,du,fu,*tofu,tcon;
	
	/* determine maximum number of u */
	numax = 500+log((float)nt)*(float)(nt-1);
		
	/* allocate space for u(t) */
	uoft = ealloc1float(nt);
	
	/* determine t1 and t2, rounded to nearest sampled times */
	tmin = ft+itmin*dt;
	et = ft+(nt-1)*dt;
	t1 = MIN(et,MAX(ft+dt,tmin));
	if (offset!=0.0)
		t2 = MAX(t1,1.0/(1.0/et+0.2*dt*vrms[0]*vrms[0]/
			(offset*offset)));
	else
		t2 = t1;
	t1 = ft+NINT(t1/dt)*dt;
	t2 = ft+NINT(t2/dt)*dt;
	
	/* compute u(t) */
	v2 = vrms[0];
	v22 = v2*v2;
	v44 = v22*v22;
	gamma = 1.0;
	for (it=0,t=ft; it<nt; ++it,t+=dt) {
		v2m = v2;
		v22m = v22;
		v44m = v44;
		gammam = gamma;
		if (t>0.0) {
			v2 = vrms[it];
			v22 = v2*v2;
			vi2 = (t*v22-(t-dt)*v22m)/dt;
			vi4 = vi2*vi2;
			v44 = (dt*vi4+(t-dt)*v44m)/t;
		} else {
			v2 = v2m;
			v22 = v22m;
			v44 = v44m;
		}
		dv2 = (v2-v2m)/dt;
		v24 = v22*v22;
		gamma = 1.5*v44/v24-t*dv2/v2-0.5;
		if (t<=t1) {
			uoft[it] = t-t1;
		} else if (t>t1 && t<=t2) {
			du = t1*(gamma*log(t/(t-0.5*dt)) -
				gammam*log((t-dt)/(t-0.5*dt)));
			dumin = 0.1*dt*t1/t;
			uoft[it] = uoft[it-1]+MAX(dumin,du);
		} else if (t>t2) {
			uoft[it] = 2.0*uoft[it-1]-uoft[it-2];
		}
	}
	
	/* determine minimum u(t)-u(t-dt) */
	dumin = uoft[1]-uoft[0];
	for (it=1; it<nt; ++it)
		dumin = MIN(dumin,uoft[it]-uoft[it-1]);
	
	/* determine u sampling for t(u) to avoid aliasing */
	fu = 0.0;
	eu = uoft[nt-1];
	du = dumin/MIN(1.0,2.0*fmax*dt);
	nu = 1+NINT((eu-fu)/du);
	if (nu>numax) {
		nu = numax;
		du = (eu-fu)/(nu-1);
	}
	
	/* allocate space for t(u) */
	tofu = ealloc1float(nu);
	
	/* compute t(u) by inverse linear interpolation of u(t) */
	yxtoxy(nt,dt,ft,uoft,nu,du,fu,ft,et,tofu);
	
	/* set time constant */
	tcon = t1;
	
	/* set returned values */
	*uoftp = uoft;
	*nup = nu;
	*dup = du;
	*fup = fu;
	*tofup = tofu;
	*tconp = tcon;
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(headerfp);
	eremove(headerfile);
	exit(EXIT_FAILURE);
}
