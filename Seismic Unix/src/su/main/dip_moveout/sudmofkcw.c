/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUDMOFKCW: $Revision: 1.8 $ ; $Date: 2011/11/16 17:51:02 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUDMOFKCW - converted-wave DMO via F-K domain (log-stretch) method for",
" 		common-offset gathers					",
" 									",
" sudmofkcw <stdin >stdout cdpmin= cdpmax= dxcdp= noffmix= [...]	",
" 									",
" Required Parameters:							",
" cdpmin		  minimum cdp (integer number) for which to apply DMO",
" cdpmax		  maximum cdp (integer number) for which to apply DMO",
" dxcdp		   distance between adjacent cdp bins (m)		",
" noffmix		 number of offsets to mix (see notes)		",
" 									",
" Optional Parameters:							",
" tdmo=0.0		times corresponding to rms velocities in vdmo (s)",
" vdmo=1500.0		rms velocities corresponding to times in tdmo (m/s)",
" gamma=0.5		 velocity ratio, upgoing/downgoing		",
" ntable=1000		 number of tabulated z/h and b/h (see notes)	",
" sdmo=1.0		DMO stretch factor; try 0.6 for typical v(z)	",
" flip=0		 =1 for negative shifts and exchanging s1 and s2",
" 			 (see notes below)				",
" fmax=0.5/dt		maximum frequency in input traces (Hz)		",
" verbose=0		=1 for diagnostic print				",
" 									",
" Notes:								",
" Input traces should be sorted into common-offset gathers.  One common-",
" offset gather ends and another begins when the offset field of the trace",
" headers changes.							",
" 									",
" The cdp field of the input trace headers must be the cdp bin NUMBER, NOT",
" the cdp location expressed in units of meters or feet.		",
" 									",
" The number of offsets to mix (noffmix) should typically equal the ratio of",
" the shotpoint spacing to the cdp spacing.  This choice ensures that every",
" cdp will be represented in each offset mix.  Traces in each mix will	",
" contribute through DMO to other traces in adjacent cdps within that mix.",
" 									",
" The tdmo and vdmo arrays specify a velocity function of time that is	",
" used to implement a first-order correction for depth-variable velocity.",
" The times in tdmo must be monotonically increasing. The velocity function",
" is assumed to have been gotten by traditional NMO. 			",
" 									",
" For each offset, the minimum time at which a non-zero sample exists is",
" used to determine a mute time.  Output samples for times earlier than this",
" mute time will be zeroed.  Computation time may be significantly reduced",
" if the input traces are zeroed (muted) for early times at large offsets.",
" 									",
" z/h is horizontal-reflector depth normalized to half source-reciver offset",
" h.  Normalized shift of conversion point is b/h.  The code now does not",
" support signed offsets, therefore it is recommended that only end-on data,",
" not split-spread, be used as input (of course after being sorted into	",
" common-offset gathers).  z/h vs b/h depends on gamma (see Alfaraj's Ph.D.",
" thesis, 1993).							",
" 									",
" Flip factor = 1 implies positive shift of traces (in the increasing CDP",
" bin number direction).  When processing split-spread data, for example,",
" if one side of the spread is processed with flip=0, then the other side",
" of the spread should be processed with flip=1.  The flip factor also	",
" determines the actions of the factors s1 and s2, i.e., stretching or	",
" squeezing.								",
" 									",
" Trace header fields accessed:  nt, dt, delrt, offset, cdp.		",
"									",
NULL};

/* Credits:
 *	CWP: Mohamed Alfaraj
 *		Dave Hale
 *
 * Technical Reference:
 *	Transformation to zero offset for mode-converted waves
 *	Mohammed Alfaraj, Ph.D. thesis, 1993, Colorado School of Mines
 *
 *	Dip-Moveout Processing - SEG Course Notes
 *	Dave Hale, 1988
 */
/**************** end self doc *******************************************/

/* Prototypes of functions used internally */
static void mkvrms (int ntdmo, float *tdmo, float *vdmo,
	int nt, float dt, float ft, float *vrms);
static void dmooff (float offset, float fmax, int nx, float dx,
	int nt, float dt, float ft, float *vrms, float **ptx, float gamma,
	float *boh, float *zoh, int ntable, float s1, float s2, float sign);
static void maketu (float offset, int itmin, float fmax,
	int nt, float dt, float ft, float *vrms, float **uoftp,
	int *nup, float *dup, float *fup, float **tofup, float *tconp);
static float getbbohitn (float x, int itmin, int nt, float dt, float *v,
	int ntable, float *boh, float *zoh, float gamma,
	float **bbohp, int **t0p);
static void table (int ntable, float gamma, float *zoh, float *boh);
static void stretchfactor (float sdmo, float gamma, float *s1, float *s2);

/* Globals */
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
	float gamma;	/* upgoing to downging velocity ratio */
	float *zoh=NULL;/* tabulated z/h */
	float *boh=NULL;/* tabulated b/h */
	int ntable;	/* number of tabulated zoh and boh */
	float sdmo;	/* DMO stretch factor */
	float s1;	/* DMO stretch factor */
	float s2;	/* DMO stretch factor */
	float temps;	/* temp value used in excahnging s1 and s2 */
	int flip;	/* apply negative shifts and exchange s1 and s2 */
	float sign; 	/* + if flip=0, negative if flip=1 */
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
	FILE *hfp;	/* file pointer for temporary header file */

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = tr.dt/1000000.0;
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
	if (!getparfloat("gamma",&gamma)) gamma = 0.5;
	if (!getparint("ntable",&ntable)) ntable = 1000;
	if (!getparfloat("sdmo",&sdmo)) sdmo = 1.0;
	if (!getparint("flip",&flip)) flip=0;
	if (flip)
		sign = -1.0;
	else
		sign = 1.0;
	if (!getparfloat("fmax",&fmax)) fmax = 0.5/dt;
	if (!getparint("verbose",&verbose)) verbose=0;
        checkpars();

	/* allocate and generate tables of b/h and z/h if gamma not equal 1 */
	if(gamma != 1.0){
		zoh=alloc1float(ntable);
		boh=alloc1float(ntable);
		table(ntable, gamma, zoh, boh);
	}

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
	hfp = tmpfile();

	/* initialize */
	oldoffset = offset;
	gottrace = 1;
	done = 0;
	ntrace = 0;
	noff = 0;

	/* get DMO stretch/squeeze factors s1 and s2 */
	stretchfactor (sdmo,gamma,&s1,&s2);
	if(flip) {
		temps = s1;
		s1 = s2;
		s2 = temps;
	}

	/* print useful information if requested */
	if (verbose)fprintf(stderr,"stretching factors: s1=%f s2=%f\n",s1,s2);

	/* loop over traces */
	do {
		/* if got a trace, determine offset */
		if (gottrace) offset = tr.offset;

		/* if an offset is complete */
		if ((gottrace && offset!=oldoffset) || !gottrace) {

			/* do dmo for old common-offset gather */
			dmooff(oldoffset,fmax,nx,dx,nt,dt,ft,vrms,p,
			gamma,boh,zoh,ntable,s1,s2,sign);

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
			efseeko(hfp, (off_t) 0,SEEK_SET);

			/* loop over all output traces */
			for (itrace=0; itrace<ntrace; ++itrace) {

				/* read trace header and determine cdp index */
				efread(&tro,HDRBYTES,1,hfp);

				/* get dmo-corrected data */
				memcpy((void *) tro.data,
					(const void *) q[tro.cdp-cdpmin],
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
			efseeko(hfp, (off_t) 0,SEEK_SET);

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
			efwrite(&tr,HDRBYTES,1,hfp);
			ntrace++;

			/* remember offset */
			oldoffset = offset;

			/* get trace samples */
			memcpy((void *) p[tr.cdp-cdpmin],
				(const void *) tr.data, nt*sizeof(float));
		}

		/* get next trace (if there is one) */
		if (!gettr(&tr)) gottrace = 0;

	} while (!done);

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
Author:  Dave Hale, Colorado School of Mines, 10/03/91
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

static void dmooff (float offset, float fmax, int nx, float dx,
	int nt, float dt, float ft, float *vrms, float **ptx, float gamma,
	float *boh, float *zoh, int ntable, float s1, float s2, float sign)
/*****************************************************************************
perform dmo in f-k domain for one offset
******************************************************************************
Input:
offset		source receiver offset
fmax		maximum frequency
s1		DMO stretch factor
s2		DMO stretch factor
sign		sign of shift
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
Author:  Dave Hale, Colorado School of Mines, 08/08/91
*****************************************************************************/
{
	int nxfft,itmin,nu,nufft,nw,nk,ix,iu,iw,ik,it,iwn,
		iwmin,iwmax,nupad,ikmax,*itn,lnt;
	float dw,dk,tcon,wwscl,wwscl2,scale,scales,kmax,lt,
		amp,phase,fr,fi,pwr,pwi,cphase,sphase,os1,os2,
		wmin,wmax,fftscl,du,fu,w,k,*uoft,*tofu,g1,h,vmin,hk,t,*bboh=NULL;
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

	/* constants depending on gamma, offset, vrms[0], and nt */
	g1 = 2.0*sqrt(gamma)/(1.0+gamma);
	h = offset/2.0;
	lnt = nt-1;
	lt = (nt-1)*dt;
	vmin = 0.5*MIN((1.0+1.0/gamma)*vrms[0],(1.0+gamma)*vrms[0]);

	/* if gamma != 1, get bboh[] and itn[] for this offset */
	if(gamma!=1.0)
		getbbohitn (offset,itmin,nt,dt,vrms,ntable,boh,zoh, \
		gamma,&bboh,&itn);

	/* inverse of dmo stretch/squeeze factors */
	os1 = 1.0/s1;
	os2 = 1.0/s2;

	/* maximum DMO shift (in samples) for any wavenumber k */
	nupad = 1.5*((s1+s2)/2.0)*tcon/du;

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

		/* apply time vriant linear phase shift */
		hk=sign*h*k;
	 	for (it=lnt,t=lt; it>=itmin; --it,t-=dt) {

			/* calculate phase-shift=boh*h*k, h=offset/2 */
		 	if(gamma != 1.0)
			   phase = bboh[it]*hk;
			else
			   phase = 0.0;

			/* phase shift p(t,k) */
			cphase=cos(phase);
			sphase=sin(phase);
			fr = ptk[ik][it].r;
			fi = ptk[ik][it].i;
			ptk[ik][it].r = fr*cphase + fi*sphase;
			ptk[ik][it].i = -fr*sphase + fi*cphase;
		}

		/* stretch p(t;k) to p(u) */
		ints8c(nt,dt,ft,ptk[ik],czero,czero,nu,tofu,pu);

		/* pad with zeros and Fourier transform p(u) to p(w) */
		for (iu=nu; iu<nufft; ++iu)
			pu[iu].r = pu[iu].i = 0.0;
		pfacc(1,nufft,pu);

		/* minimum and maximum frequencies to process */
		wmin = ABS(0.5*vmin*k);
		wmax = ABS(PI/du);
		iwmin = MAX(1,MIN((nw-1)/2,NINT(wmin/dw)));
		iwmax = MAX(0,MIN((nw-1)/2,NINT(wmax/dw)));

		/* constant independent of w */
		wwscl = os1*pow(g1*hk/tcon,2.0);
		wwscl2 = wwscl*os2/os1;

		/* zero dc (should be zero anyway) */
		pw[0].r = pw[0].i = 0.0;

		/* zero frequencies below minimum */
		for (iw=1,iwn=nw-iw; iw<iwmin; ++iw,--iwn)
			pw[iw].r = pw[iw].i = pw[iwn].r = pw[iwn].i = 0.0;

		/* do dmo between minimum and maximum frequencies */
		for (iw=iwmin,iwn=nw-iwmin,w=iwmin*dw;
			iw<=iwmax; ++iw,--iwn,w+=dw) {

			/* positive w */
			scales = 1.0+wwscl/(w*w);
			scale = sqrt(scales);
			phase = s1*w*tcon*(scale-1.0);
			amp = fftscl*(1.0-s1+s1/scale);
			fr = amp*cos(phase);
			fi = amp*sin(phase);
			pwr = pw[iw].r;
			pwi = pw[iw].i;
			pw[iw].r = pwr*fr-pwi*fi;
			pw[iw].i = pwr*fi+pwi*fr;

			/* negative w */
			scales = 1.0+wwscl2/(w*w);
			scale = sqrt(scales);
			phase = s2*w*tcon*(scale-1.0);
			amp = fftscl*(1.0-s2+s2/scale);
			fr = amp*cos(phase);
			fi = amp*sin(phase);
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
Author:  Dave Hale, Colorado School of Mines, 08/08/91
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

static void table (int ntable, float gamma, float *zoh, float *boh)
/*****************************************************************************
make table of z/h vs b/h, z is depth, b is lateral shift, and h is half offset
******************************************************************************
Input:
ntable		number of elements in table
gamma		upgoing-to-downgoing velocity ratio

Output:
zoh		arrat[ntable] of z/h
boh		array[ntable] of b/h
******************************************************************************
Author:  Mohammed Alfaraj, Colorado School of Mines, 01/08/92
*****************************************************************************/
{
	int	i;
	float 	f, alpha, beta, bstart, b, z, db;

	f=(1.0-gamma)/(1.0+gamma);
	alpha=1.0+1.0/(gamma*gamma);
	beta=1.0-1.0/(gamma*gamma);

	if(gamma<1.0)
		db = -((bstart=1.0)-f)/(ntable+2);
	else
		db = (f -(bstart=(-1.0)))/(ntable+2);

	/* generate table */
	for(i=0,b=bstart; i<ntable; i++,b+=(db)){
		z=(1+b)*(1-b*b)*(2.0-alpha-beta*b)/(alpha-2.0+ \
		b*(beta-alpha-2.0)-beta*b*b);
		if(z<0) err("negative sqrt when generating table");
		zoh[i] = sqrt(z);
		boh[i] = b;
	}
}

static float getbbohitn (float x, int itmin, int nt, float dt, float *v,
	int ntable, float *boh, float *zoh, float gamma,
	float **bbohp, int **itnp)
/*****************************************************************************
convert b/h from a function of depth to a function of NMO time
******************************************************************************
Input:
x		offset
itmin		mininimum NMO time index to process
nt		number of time samples
dt		time sampling interval
v		array[nt] of RMS velocities
ntable		number of tabulated zoh or boh
boh		array[ntable] of b/h
zoh		array[ntable] of z/h
gamma		velocity ratio (upgoing/downgoing)

Output:
bbohp		array[nt] of b/h
itnp		array[nt] of time-sample indexes
******************************************************************************
Author:  Mohammed Alfaraj, Colorado School of Mines, 01/08/92
*****************************************************************************/
{
	int 	i, j=0, it, gotit;
	float	alpha, beta, tossq, xov, nz, t, xsq, tog, temp;
	float	*bboh;
	int	*itn;

	/* allocate space */
	bboh = ealloc1float(nt);
	itn = ealloc1int(nt);

	/* constants depending on gamma */
	alpha = 1.0+1.0/(gamma*gamma);
	beta = 1.0-1.0/(gamma*gamma);
	tossq = 2.0/((1.0+1.0/gamma)*(1.0+1.0/gamma));
	tog=2.0/gamma;
	xsq = x*x;

	/* loop over it */
	for (it=itmin,t=itmin*dt; it<nt; it++,t+=dt) {
		xov = x/v[it];
		nz = t/xov;
		gotit = 0;

		/* loop over table */
		for (i=j; i<ntable; i++)
			if(zoh[i]>=nz) {
				bboh[it] = boh[i];
				temp = (t*t/(1-boh[i]*boh[i])-xov*xov \
				*(tog/(alpha+ beta*boh[i])-1))* \
				(alpha+beta*boh[i])*tossq;

				/* zero time if evanscent */
				if (temp<0.0)
					itn[it] = 0;
				else{
					itn[it] = NINT(sqrt(temp)/dt);
					/* bboh[it]=boh[i-60]; */
				}

				j = i;
				gotit = 1;
				break;
			}
		if (gotit) continue;

		/* else set boh to asymtotic value then break */
		for (j=it; j<nt; j++,t+=dt) {
			bboh[j] = boh[ntable-1];
			itn[j]=NINT(sqrt((t*t/(1-bboh[j]*bboh[j])-(xsq/(v[j]* \
			v[j]))*(tog/(alpha+ beta*bboh[j])-1))* \
			(alpha+beta*bboh[j])*tossq)/dt);
		}
		break;
	}

	/* set returned values */
  	*bbohp = bboh;
	*itnp = itn;

	return(CWP_Exit());
}

static void stretchfactor (float sdmo, float gamma, float *s1, float *s2)
/*****************************************************************************
calculate stretch/squeeze factors s1 and s2
******************************************************************************
Input:
sdmo		DMO squeeze factor
gamma		velocity ratio (upgoing/downgoing)

Output:
s1		DMO stretch (or squeeze) factor
s2		DMO squeeze (or stretch) factor
******************************************************************************
Author:  Mohammed Alfaraj, Colorado School of Mines, 01/08/92
*****************************************************************************/
{
	/* solve for DMO stretch factors s1 and s2 for nominal error
	   in log stretch; for gamma=1, s1=s2=0.62 */
	if(gamma <= 1.0) {
		*s1 = 0.62+0.633*(pow(gamma,-3.0)-1.0);
		*s2 = 0.62+0.615*(pow(2.0-gamma,-7.0)-1.0);
	} else {
		*s1 = 0.62+0.615*(pow(2.0-1.0/gamma,-7.0)-1.0);
		*s2 = 0.62+0.633*(pow(1.0/gamma,-3.0)-1.0);
	}

	/* modify stretch/squeeze factors for v(z) */
	*s1 *= sdmo;
	*s2 *= sdmo;
}
