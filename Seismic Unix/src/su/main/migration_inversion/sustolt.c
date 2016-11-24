/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSTOLT: $Revision: 1.22 $ ; $Date: 2011/11/16 22:14:43 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUSTOLT - Stolt migration for stacked data or common-offset gathers	",
"									",
" sustolt <stdin >stdout cdpmin= cdpmax= dxcdp= noffmix= [...]		",
"									",
" Required Parameters:							",
" cdpmin=		  minimum cdp (integer number) in dataset	",
" cdpmax=		  maximum cdp (integer number) in dataset	",
" dxcdp=		  distance between adjacent cdp bins (m)	",
"									",
" Optional Parameters:							",
" noffmix=1		number of offsets to mix (for unstacked data only)",
" tmig=0.0		times corresponding to rms velocities in vmig (s)",
" vmig=1500.0		rms velocities corresponding to times in tmig (m/s)",
" smig=1.0		stretch factor (0.6 typical if vrms increasing)",
" vscale=1.0		scale factor to apply to velocities		",
" fmax=Nyquist		maximum frequency in input data (Hz)		",
" lstaper=0		length of side tapers (# of traces)		",
" lbtaper=0		length of bottom taper (# of samples)		",
" verbose=0		=1 for diagnostic print				",
" tmpdir=		if non-empty, use the value as a directory path	",
"			prefix for storing temporary files; else if the	",
"			the CWP_TMPDIR environment variable is set use	",
"			its value for the path; else use tmpfile()	",
"									",
" Notes:								",
" If unstacked traces are input, they should be NMO-corrected and sorted",
" into common-offset  gathers.  One common-offset gather ends and another",
" begins when the offset field of the trace headers changes. If both	",
" NMO and DMO are applied, then this is equivalent to prestack time 	",
" migration (though the velocity profile is assumed v(t), only).	",
"									",
" The cdp field of the input trace headers must be the cdp bin NUMBER, NOT",
" the cdp location expressed in units of meters or feet.		",
"									",
" The number of offsets to mix (noffmix) should be specified for	",
" unstacked data only.	noffmix should typically equal the ratio of the	",
" shotpoint spacing to the cdp spacing.	 This choice ensures that every	",
" cdp will be represented in each offset mix.  Traces in each mix will	",
" contribute through migration to other traces in adjacent cdps within	",
" that mix.								",
"									",
" The tmig and vmig arrays specify a velocity function of time that is	",
" used to implement Stolt's stretch for depth-variable velocity.  The	",
" stretch factor smig is often referred to as the \"W\" factor.		",
" The times in tmig must be monotonically increasing.			",
NULL};

/* Credits:
 *	CWP: Dave Hale c. 1990
 *
 * Trace header fields accessed:  ns, dt, delrt, offset, cdp
 */
/**************** end self doc *******************************************/

static void makev (int nmig, float *tmig, float *vmig, float vscale,
	int nt, float dt, float ft, float **v, float *vmin, float *vmax);
static void makeut (float vstolt, float fmax, float *vt,
	int nt, float dt, float **ut, int *nu, float *du, float **tu);
static void makeu (float vstolt, float *v, int nt, float dt, float *u);
static void stolt1k (float k, float v, float s, float fmax, int nt, float dt,
	complex *p, complex *q);
static void taper (int lxtaper, int lbtaper, 
	int nx, int ix, int nt, float *trace);
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *headerfp=NULL;		/* fp for header storage file		*/

segy tr,tro;

int
main(int argc, char **argv)
{
	int nt;			/* number of time samples in traces	*/
	int it;			/* counter for time values		*/
	int cdpmin;		/* minimum CDP value in input data	*/
	int cdpmax;		/* maximum CDP value in input data	*/

	int nx;			/* number of traces			*/
	int ix;			/* counter for spatial values		*/
	int nxfft;		/* fft size in x direction		*/
	int nxpad;		/* ...padding				*/

	int noffmix;		/* number of offsets to mix for prestack*/

	int lstaper;		/* length of side tapers		*/
	int lbtaper;		/* length of bottom taper		*/

	int ntmig;		/* number of migration velocity times	*/
	int nvmig;		/* number of migration velocity values	*/
	int itmig;		/* counter on migration time/values	*/

	int nk;			/* number of wave numbers		*/
	int nu;
	int noff;		/* number of offsets			*/
	int ntrace;		/* number of traces			*/
	int itrace;		/* counter on traces			*/
	int gottrace;		/* got a trace				*/

	int done;		/* out of data				*/
	int verbose;		/* verbose =1 chatty =0 silent		*/

	float dt;
	float ft;
	float dx;
	float *tmig=NULL;
	float *vmig=NULL;
	float vscale;
	float vstolt;
	float smig;
	float fmax;
	float offset;
	float oldoffset;
	float vmin;
	float vmax;
	float dk;
	float du;

	float *v=NULL;
	float *ut=NULL;
	float *tu=NULL;
	float **px=NULL;
	float **q=NULL;
	complex **pk=NULL;
	char *tmpdir=NULL;	/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path		*/


	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("cannot get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	ft = tr.delrt/1000.0;
	if (ft!=0.0) err("cannot handle non-zero time of first sample");
	offset = tr.offset;

	/* get parameters */
	if (!getparint("cdpmin",&cdpmin)) err("must specify cdpmin");
	if (!getparint("cdpmax",&cdpmax)) err("must specify cdpmax");
	if (cdpmin>cdpmax) err("cdpmin must not be greater than cdpmax");
	if (!getparfloat("dxcdp",&dx)) err("must specify dxcdp");
	if (!getparint("noffmix",&noffmix)) noffmix = 1;

	/* get times and velocities */
	/* ...times */
	ntmig = countparval("tmig") + 1; 
	tmig = ealloc1float(ntmig);
	if (!getparfloat("tmig",tmig)) tmig[0] = 0.0;
	tmig[ntmig - 1] = (nt*dt + ft);  /* set last time to end of data */
	
	/* ... velocities */
	nvmig = countparval("vmig") + 1 ;
	if (nvmig!=ntmig) err("number of tmig and vmig must be equal");
	vmig = ealloc1float(nvmig);
	if (!getparfloat("vmig",vmig)) vmig[0] = 1500.0;

	vmig[nvmig -1 ] = vmig[nvmig - 2]; /* set velocity at end of data to */
					   /* last velocity getparred */
	/* check for monotonicity in times */
	for (itmig=1; itmig<ntmig; ++itmig)
		if (tmig[itmig]<=tmig[itmig-1])
			err("tmig must increase monotonically");
	/* end of velocities and times */

	if (!getparfloat("smig",&smig)) smig = 1.0;
	if (!getparfloat("fmax",&fmax)) fmax = 0.5/dt;
	fmax = MIN(fmax,0.5/dt);
	if (!getparfloat("vscale",&vscale)) vscale = 1.0;
	if (!getparint("lstaper",&lstaper)) lstaper=0;
	if (!getparint("lbtaper",&lbtaper)) lbtaper=0;
	if (!getparint("verbose",&verbose)) verbose=0;
	
	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

        checkpars();

	/* make uniformly sampled rms velocity function of time */
	makev(ntmig,tmig,vmig,vscale,nt,dt,ft,&v,&vmin,&vmax);
	
	/* Stolt migration velocity is the minimum velocity */
	vstolt = vmin;


	/* make u(t) and t(u) for Stolt stretch */
	makeut(vstolt,fmax,v,nt,dt,&ut,&nu,&du,&tu);
	free1float(v);
	
	/* determine number of cdps to process */
	nx = cdpmax-cdpmin+1;

	/* wavenumber (k) sampling */
	nxpad = 0.5*vmax*nt*dt/dx;
	nxfft = npfar(nx+nxpad);
	nk = nxfft/2+1;
	dk = 2.0*PI/(nxfft*dx);
		
	/* allocate and zero common-offset gather p(t,x) */
	pk = alloc2complex(MAX(nu,nt),nk);
	px = alloc1(nxfft,sizeof(float*));
	px[0] = (float*)pk[0];
	for (ix=1; ix<nxfft; ++ix)
		px[ix] = px[0]+ix*MAX(nu,nt);
	memset((void *) px[0],0,nxfft*MAX(nu,nt)*sizeof(float));

	/* allocate and zero offset mix accumulator q(t,x) */
	q = ealloc2float(nt,nx);
	memset((void *) q[0],0,nt*nx*sizeof(float));
		
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
		if (verbose) warn("putting temporary files in %s", directory);
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
		
		/* if an offset is complete, migrate it */
		if ((gottrace && offset!=oldoffset) || !gottrace) {
		
			/* local variables */
			int ik,jx,iu;
			float scale=1.0/nxfft,k;
			
			/* apply side and bottom tapers */
			for (ix=0; ix<nx; ++ix)
				taper(lstaper,lbtaper,nx,ix,nt,px[ix]);
			
			/* if necessary, stretch */
			if (nu!=nt && tu!=NULL) {
				float *temp=ealloc1float(nu);
				for (ix=0; ix<nx; ++ix) {
					ints8r(nt,dt,0.0,px[ix],0.0,0.0,
						nu,tu,temp);
					for (iu=0; iu<nu; ++iu)
						px[ix][iu] = temp[iu];
				}
				free1float(temp);
			}

			/* Fourier transform p(u,x) to p(u,k) */
			pfa2rc(-1,2,MAX(nu,nt),nxfft,px[0],pk[0]);

			/* migrate each wavenumber */
			for (ik=1,k=dk; ik<nk; ++ik,k+=dk)
				stolt1k(k,vstolt,smig,fmax,
					MAX(nu,nt),du,pk[ik],pk[ik]);

			/* Fourier transform p(u,k) to p(u,x) and scale */
			pfa2cr(1,2,MAX(nu,nt),nxfft,pk[0],px[0]);
			for (jx=0; jx<nx; ++jx)
				for (iu=0; iu<nu; ++iu)
					px[jx][iu] *= scale;

			/* if necessary, unstretch */
			if (nu!=nt && ut!=NULL) {
				float *temp=ealloc1float(nt);
				for (ix=0; ix<nx; ++ix) {
					ints8r(nu,du,0.0,px[ix],0.0,0.0,
						nt,ut,temp);
					for (it=0; it<nt; ++it)
						px[ix][it] = temp[it];
				}
				free1float(temp);
			}
			
			/* add migrated traces to mix */
			for (ix=0; ix<nx; ++ix)
				for (it=0; it<nt; ++it)
					q[ix][it] += px[ix][it];
			
			/* zero common-offset gather */
			for (ix=0; ix<nxfft; ++ix)
				for (iu=0; iu<nu; ++iu)
					px[ix][iu] = 0.0;

			
			/* count offsets in mix */
			noff++;
		}
		
		/* if a mix of offsets is complete */
		if (noff==noffmix || !gottrace) {
			
			/* rewind trace header file */
			efseeko(headerfp,(off_t) 0,SEEK_SET);
			
			/* loop over all output traces */
			for (itrace=0; itrace<ntrace; ++itrace) {
			
				/* read trace header and determine cdp index */
				efread(&tro,HDRBYTES,1,headerfp);
				
				/* index of cdp, zero-based */
				ix = tro.cdp-cdpmin;
				
				/* get migrated data */
				for (it=0; it<nt; ++it)
					tro.data[it] = q[ix][it];
				
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
			efseeko(headerfp,(off_t) 0,SEEK_SET);
			
			/* reset number of offsets and traces in mix */
			noff = 0;
			ntrace = 0;
			
			/* zero offset mix accumulator */
			memset((void *) q[0],0,nt*nx*sizeof(float));
		}
		if (verbose && oldoffset!=offset)
				if (verbose)
					warn("migrating offset = %f", offset);
			
		/* if cdp is within range to process */
		if (tr.cdp>=cdpmin && tr.cdp<=cdpmax) {

	
			/* save trace header and update number of traces */
			efwrite(&tr,HDRBYTES,1,headerfp);
			ntrace++;

			/* remember offset */
			oldoffset = offset;
		
			/* index of cdp, zero-based */
			ix = tr.cdp-cdpmin;

			/* save trace */
			for (it=0; it<nt; ++it)
				px[ix][it] = tr.data[it];
		}

		/* get next trace (if there is one) */
		if (!gettr(&tr)) gottrace = 0;
		
	} while (!done);

	/* Clean up */
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	return(CWP_Exit());
}
	
static void makev (int nmig, float *tmig, float *vmig, float vscale,
	int nt, float dt, float ft, float **v, float *vmin, float *vmax)
/*****************************************************************************
make uniformly sampled rms velocity function v(t) for migration
******************************************************************************
Input:
nmig		number of tmig,vmig pairs
tmig		array[nmig] of times
vmig		array[nmig] of rms velocities
vscale		velocity scale factor
nt		number of time samples
dt		time sampling interval
ft		first time sample

Output:
v		array[nt] of rms velocities
vmin		minimum velocity
vmax		maximum velocity
******************************************************************************
Author:	 Dave Hale, Colorado School of Mines, 10/22/91
*****************************************************************************/
{
	int it;
	float t,*vel=NULL,velmin=0.0,velmax=0.0,(*vmigd)[4];
	
	vmigd = (float(*)[4])ealloc1float(nmig*4);

	cmonot(nmig,tmig,vmig,vmigd);
	
	vel = ealloc1float(nt);
	memset((void *) vel, 0 , nt*FSIZE);

	for (it=0,t=ft; it<nt; ++it,t+=dt) 
		intcub(0,nmig,tmig,vmigd,1,&t,&vel[it]);

	for (it=0; it<nt; ++it) { 
		vel[it] *= vscale;
	}

	for (it=1,velmin=velmax=vel[0]; it<nt; ++it) {
		velmin = MIN(velmin,vel[it]);
		velmax = MAX(velmax,vel[it]);
	}
	free1float((float*)vmigd);
	*v = vel;
	*vmin = velmin;
	*vmax = velmax;

}

static void makeut (float vstolt, float fmax, float *vrms,
	int nt, float dt, float **ut, int *nu, float *du, float **tu)
/*****************************************************************************
Compute u(t) and t(u) for Stolt stretch
******************************************************************************
Input:
vstolt		Stolt migration velocity
fmax		maximum frequency
vrms		array[nt] of RMS velocities
nt		number of t samples
dt		t sampling interval (first t assumed to be zero)

Output
ut		array[nt] of u(t); NULL if constant velocity
nu		number of u samples
du		u sampling interval (first u assumed to be zero)
tu		array[nu] of t(u); NULL if constant velocity
*****************************************************************************/
{
	int it;
	
	/* check for constant velocity */
	for (it=1; it<nt; ++it)
		if (vrms[it]!=vrms[0]) break;
		
	/* if constant velocity */
	if (it==nt) {
		*ut = NULL;
		*tu = NULL;
		*nu = nt;
		*du = dt;

	/* else if velocity not constant */
	} else {
		int it;
		int nuu;
		float duu=0.0;
		float delu=0.0;
		float umax=0.0;
		float *u=NULL;
		float *t=NULL;
		
		/* u(t) */
		u = alloc1float(nt);

		makeu(vstolt,vrms,nt,dt,u);


		/* smallest du and maximum u */
		duu = FLT_MAX;
		for (it=1; it<nt; ++it) {
			delu =	u[it]-u[it-1];
		
			if (delu<duu  ) duu = delu;

		}
		umax = u[nt-1];

		/* u sampling */
		duu = duu/(2.0*fmax*dt);
		nuu = 1+NINT(umax/duu);

		/* t(u) */
		t = alloc1float(nuu);
		yxtoxy(nt,dt,0.0,u,nuu,duu,0.0,0.0,(nt-1)*dt,t);

		/* set output parameters before returning */
		*ut = u;
		*tu = t;
		*nu = nuu;
		*du = duu;
	}
}

static void makeu (float vstolt, float *v, int nt, float dt, float *u)
/*****************************************************************************
Compute				     t
	u(t) = sqrt( 2/vstolt^2 * Integral ds*s*(v(s)^2) )
				     0
via the trapezoidal rule.
******************************************************************************
Input:
vstolt		Stolt migration velocity
v		array[nt] of RMS velocities
nt		number of t samples
dt		t sampling interval

Output
u		array[nt] of u(t)
*****************************************************************************/
{
	int it;
	float t,scale,sum;

	scale = 2.0/(vstolt*vstolt);
	u[0] = sum = 0.0;
	for (it=1,t=dt; it<nt; ++it,t+=dt) {
		sum += 0.5*dt*(t*v[it]*v[it]+(t-dt)*v[it-1]*v[it-1]);
		u[it] = sqrt(scale*sum);
	}
}

static void stolt1k (float k, float v, float s, float fmax, int nt, float dt,
	complex *p, complex *q)
/*****************************************************************************
Stolt's migration for one wavenumber k
******************************************************************************
Input:
k		wavenumber
v		velocity
s		Stolt stretch factor (0<s<2; use s=1 for constant velocity)
fmax		maximum frequency (in cycles per unit time)
nt		number of time samples
dt		time sampling interval (first time assumed to be zero)
p		array[nt] containing data to be migrated

Output
q		array[nt] containing migrated data (may be equivalenced to p)
*****************************************************************************/
{
	int nw,it,nwtau,iwtau,ntau,itau,iwtaul,iwtauh;
	float vko2s,wmax,dw,fw,dwtau,fwtau,wtau,dtau,
		wtauh,wtaul,scale,fftscl,a,b,*wwtau=NULL;
	complex czero=cmplx(0.0,0.0),*pp=NULL,*qq=NULL;

	/* modify stolt stretch factor to simplify calculations below */
	if (s!=1.0) s = 2.0-s;

	/* (v*k/2)^2 */
	vko2s = 0.25*v*v*k*k;

	/* maximum frequency to migrate in radians per unit time */
	wmax = 2.0*PI*MIN(fmax,0.5/dt);

	/* frequency sampling - must pad to avoid interpolation error;
	 * pad by factor of 2 because time axis is not centered;
	 * pad by factor of 1/0.6 because 8-point sinc is valid
	 * only to about 0.6 Nyquist
	 */
	nw = nt*2/0.6;
	nw = npfao(nw,nw*2);
	dw = 2.0*PI/(nw*dt);
	fw = -PI/dt;

	/* migrated time */
	ntau = nt;
	dtau = dt;

	/* migrated frequency - no need to pad since no interpolation */
	nwtau = npfao(ntau,ntau*2);
	dwtau = 2.0*PI/(nwtau*dtau);
	fwtau = -PI/dtau;

	/* tweak first migrated frequency to avoid wtau==0.0 below */
	fwtau += 0.001*dwtau;

	/* high and low migrated frequencies - don't migrate evanescent */
	wtauh = sqrt(MAX(0.0,wmax*wmax-s*vko2s));
	iwtauh = MAX(0,MIN(nwtau-1,NINT((wtauh-fwtau)/dwtau)));
	iwtaul = MAX(0,MIN(nwtau-1,NINT((-wtauh-fwtau)/dwtau)));
	wtauh = fwtau+iwtauh*dwtau;
	wtaul = fwtau+iwtaul*dwtau;

	/* workspace */
	pp = alloc1complex(nw);
	qq = alloc1complex(nwtau);
	wwtau = alloc1float(nwtau);

	/* pad with zeros and Fourier transform t to w, with w centered */
	for (it=0; it<nt; it+=2)
		pp[it] = p[it];
	for (it=1; it<nt; it+=2) {
		pp[it].r = -p[it].r;
		pp[it].i = -p[it].i;
	}
	for (it=nt; it<nw; ++it)
		pp[it].r = pp[it].i = 0.0;
	pfacc(1,nw,pp);

	/* zero -Nyquist frequency for symmetry */
	pp[0] = czero;

	/* frequencies at which to interpolate */
	if (s==1.0) {
		for (iwtau=iwtaul,wtau=wtaul; wtau<0.0; ++iwtau,wtau+=dwtau)
			wwtau[iwtau] = -sqrt(wtau*wtau+vko2s);
		for (; iwtau<=iwtauh; ++iwtau,wtau+=dwtau)
			wwtau[iwtau] = sqrt(wtau*wtau+vko2s);
	} else {
		a = 1.0/s;
		b = 1.0-a;
		for (iwtau=iwtaul,wtau=wtaul; wtau<0.0; ++iwtau,wtau+=dwtau)
			wwtau[iwtau] = b*wtau-a*sqrt(wtau*wtau+s*vko2s);
		for (; iwtau<=iwtauh; ++iwtau,wtau+=dwtau)
			wwtau[iwtau] = b*wtau+a*sqrt(wtau*wtau+s*vko2s);
	}
	
	/* interpolate */
	ints8c(nw,dw,fw,pp,czero,czero,
		iwtauh-iwtaul+1,wwtau+iwtaul,qq+iwtaul);

	/* fft scaling and obliquity factor */
	fftscl = 1.0/nwtau;
	if (s==1.0) {
		for (iwtau=iwtaul,wtau=wtaul; iwtau<=iwtauh; 
			++iwtau,wtau+=dwtau) {
			scale = fftscl*wtau/wwtau[iwtau];
			qq[iwtau].r *= scale;
			qq[iwtau].i *= scale;
		}
	} else {
		a = 1.0/(s*s);
		b = 1.0-1.0/s;
		for (iwtau=iwtaul,wtau=wtaul; iwtau<=iwtauh; 
			++iwtau,wtau+=dwtau) {
			scale = fftscl*(b+a*wtau/(wwtau[iwtau]-b*wtau));
			qq[iwtau].r *= scale;
			qq[iwtau].i *= scale;
		}
	}

	/* zero evanescent frequencies */
	for (iwtau=0; iwtau<iwtaul; ++iwtau)
		qq[iwtau] = czero;
	for (iwtau=iwtauh+1; iwtau<nwtau; ++iwtau)
		qq[iwtau] = czero;

	/* Fourier transform wtau to tau, accounting for centered wtau */
	pfacc(-1,nwtau,qq);
	for (itau=0; itau<ntau; itau+=2)
		q[itau] = qq[itau];
	for (itau=1; itau<ntau; itau+=2) {
		q[itau].r = -qq[itau].r;
		q[itau].i = -qq[itau].i;
	}
	
	/* free workspace */
	free1complex(pp);
	free1complex(qq);
	free1float(wwtau);
}

static void taper (int lxtaper, int lbtaper, 
	int nx, int ix, int nt, float *trace)
/*****************************************************************************
Taper traces near left and right sides of trace window
******************************************************************************
Input:
lxtaper		length (in traces) of side taper
lbtaper		length (in samples) of bottom taper
nx		number of traces in window
ix		index of this trace (0 <= ix <= nx-1)
nt		number of time samples
trace		array[nt] containing trace

Output:
trace		array[nt] containing trace, tapered if within lxtaper of side
*****************************************************************************/
{
	int it;
	float xtaper;

	/* if near left side */
	if (ix<lxtaper) {
		xtaper = 0.54+0.46*cos(PI*(lxtaper-ix)/lxtaper);
	
	/* else if near right side */
	} else if (ix>=nx-lxtaper) {
		xtaper = 0.54+0.46*cos(PI*(lxtaper+ix+1-nx)/lxtaper);
	
	/* else x tapering is unnecessary */
	} else {
		xtaper = 1.0;
	}

	/* if x tapering is necessary, apply it */
	if (xtaper!=1.0)
		for (it=0; it<nt; ++it)
			trace[it] *= xtaper;
	
	/* if requested, apply t tapering */
	for (it=MAX(0,nt-lbtaper); it<nt; ++it)
		trace[it] *= (0.54+0.46*cos(PI*(lbtaper+it+1-nt)/lbtaper));
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(headerfp);
	eremove(headerfile);
	exit(EXIT_FAILURE);
}
