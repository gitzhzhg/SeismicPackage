/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMIGTK: $Revision: 1.17 $ ; $Date: 2011/11/16 22:14:43 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUMIGTK - MIGration via T-K domain method for common-midpoint stacked data",
"									",
" sumigtk <stdin >stdout dxcdp= [optional parms]			",
"									",
" Required Parameters:							",
" dxcdp                   distance between successive cdps		",
"									",
" Optional Parameters:							",
" fmax=Nyquist            maximum frequency				",
" tmig=0.0                times corresponding to interval velocities in vmig",
" vmig=1500.0             interval velocities corresponding to times in tmig",
" vfile=                  binary (non-ascii) file containing velocities v(t)",
" nxpad=0                 number of cdps to pad with zeros before FFT	",
" ltaper=0                length of linear taper for left and right edges", 
" verbose=0               =1 for diagnostic print			",
"									",
" tmpdir= 	 if non-empty, use the value as a directory path	",
"		 prefix for storing temporary files; else if the	",
"	         the CWP_TMPDIR environment variable is set use		",
"	         its value for the path; else use tmpfile()		",
" 									",
" Notes:								",
" Input traces must be sorted by either increasing or decreasing cdp.	",
" 									",
" The tmig and vmig arrays specify an interval velocity function of time.",
" Linear interpolation and constant extrapolation is used to determine	",
" interval velocities at times not specified.  Values specified in tmig	",
" must increase monotonically.						",
"									",
" Alternatively, interval velocities may be stored in a binary file	",
" containing one velocity for every time sample.  If vfile is specified,",
" then the tmig and vmig arrays are ignored.				",
"									",
" The time of first sample is assumed to be zero, regardless of the value",
" of the trace header field delrt.					",
" 									",
" The migration is a reverse time migration in the (t,k) domain. In the	",
" first step, the data g(t,x) are Fourier transformed x->k into the	",	
" the time-wavenumber domain g(t,k).					",
"									",
" Then looping over wavenumbers, the data are then reverse-time		",
" finite-difference migrated, wavenumber by wavenumber.  The resulting	",
" migrated data m(tau,k), now in the tau (migrated time) and k domain,	",
" are inverse fourier transformed back into m(tau,xout) and written out.",	
"									",
NULL};

/* Credits:
 *	CWP: Dave Hale
 *
 * Trace header fields accessed:  ns and dt
 */
/**************** end self doc *******************************************/

void mig1k (float k, float fmax, float speed, int nt, float dt, 
	float *v, complex *p, complex *q);
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *tracefp;		/* fp for trace storage file		*/
FILE *headerfp;		/* fp for header storage file		*/

segy tr;

int
main(int argc, char **argv)
{
	int nt,nx,nxfft,nxpad,ix,it,nk,ik,
		ltaper,ntmig,nvmig,itmig,verbose;
	float dt,dx,dk,taper,t,k,fftscl,fmax,speed,
		*tmig,*vmig,*vt,**gtx;
	complex **gtk;
	char *vfile="";
	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path		*/

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;

	/* get parameters */
	if (!getparfloat("dxcdp",&dx)) err("must specify dxcdp");
	if (!getparfloat("fmax",&fmax)) fmax=0.5/dt;
	if (!getparint("nxpad",&nxpad)) nxpad=0;
	if (!getparint("ltaper",&ltaper)) ltaper=0;
	if (!getparfloat("speed",&speed)) speed=1.0;
	if (!getparint("verbose",&verbose)) verbose=0;
	
	/* look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

	/* determine velocity function v(t) */
	vt = ealloc1float(nt);
	if (!getparstring("vfile",&vfile)) {
		ntmig = countparval("tmig");
		if (ntmig==0) ntmig = 1;
		tmig = ealloc1float(ntmig);
		if (!getparfloat("tmig",tmig)) tmig[0] = 0.0;
		nvmig = countparval("vmig");
		if (nvmig==0) nvmig = 1;
		if (nvmig!=ntmig) err("number of tmig and vmig must be equal");
		vmig = ealloc1float(nvmig);
		if (!getparfloat("vmig",vmig)) vmig[0] = 1500.0;
		for (itmig=1; itmig<ntmig; ++itmig)
			if (tmig[itmig]<=tmig[itmig-1])
				err("tmig must increase monotonically");
		for (it=0,t=0.0; it<nt; ++it,t+=dt)
			intlin(ntmig,tmig,vmig,vmig[0],vmig[ntmig-1],
				1,&t,&vt[it]);
	} else {
		if (efread(vt,sizeof(float),nt,fopen(vfile,"r"))!=nt)
			err("cannot read %d velocities from file %s",nt,vfile);
	}
	
        checkpars();

	/* copy traces and headers to temporary files */
	if (STREQ(tmpdir,"")) {
		tracefp = etmpfile();
		headerfp = etmpfile();
		if (verbose) warn("using tmpfile() call");
	} else { /* user-supplied tmpdir */
		char directory[BUFSIZ];
		strcpy(directory, tmpdir);
		strcpy(tracefile, temporary_filename(directory));
		strcpy(headerfile, temporary_filename(directory));
		/* Trap signals so can remove temp files */
		signal(SIGINT,  (void (*) (int)) closefiles);
		signal(SIGQUIT, (void (*) (int)) closefiles);
		signal(SIGHUP,  (void (*) (int)) closefiles);
		signal(SIGTERM, (void (*) (int)) closefiles);
		tracefp = efopen(tracefile, "w+");
		headerfp = efopen(headerfile, "w+");
      		istmpdir=cwp_true;		
		if (verbose) warn("putting temporary files in %s", directory);
	}

	/* count the traces */
	nx = 0;
	do {
		nx++;
		efwrite(&tr,HDRBYTES,1,headerfp);
		efwrite(tr.data,sizeof(float),nt,tracefp);
	} while(gettr(&tr));
	erewind(headerfp);
	erewind(tracefp);
	if (verbose) fprintf(stderr,"\t%d traces input\n",nx);
	
	/* determine wavenumber sampling */
	nxfft = npfaro(nx+nxpad,2*(nx+nxpad));
	nk = nxfft/2+1;
	dk = 2.0*PI/(nxfft*dx);

	/* allocate space for Fourier transform */
	gtk = ealloc2complex(nt,nk);
	gtx = ealloc1(nxfft,sizeof(float*));
	for (ix=0; ix<nxfft; ++ix)
		gtx[ix] = (float*)gtk[0]+ix*nt;

	/* read and apply fft scaling to traces and pad with zeros */
	fftscl = 1.0/nxfft;
	for (ix=0; ix<nx; ++ix) {
		efread(gtx[ix],sizeof(float),nt,tracefp);
		for (it=0; it<nt; ++it)
			gtx[ix][it] *= fftscl;
		if (ix<ltaper) {
			taper = (float)(ix+1)/(float)(ltaper+1);
			for (it=0; it<nt; ++it)
				gtx[ix][it] *= taper;
		} else if (ix>=nx-ltaper) {
			taper = (float)(nx-ix)/(float)(ltaper+1);
			for (it=0; it<nt; ++it)
				gtx[ix][it] *= taper;
		}
	}
	for (ix=nx; ix<nxfft; ++ix)
		for (it=0; it<nt; ++it)
			gtx[ix][it] = 0.0;
	
	/* Fourier transform g(t,x) to g(t,k) */
	pfa2rc(-1,2,nt,nxfft,gtx[0],gtk[0]);
	if (verbose) fprintf(stderr,"\tFourier transform done\n");
	
	/* loop over wavenumbers */
	for (ik=0,k=0.0; ik<nk; ++ik,k+=dk) {
	
		/* report */
		if (verbose && ik%(nk/10>0?nk/10:1)==0)
			fprintf(stderr,"\t%d of %d wavenumbers done\n",
				ik,nk);
		
		/* migrate */
		mig1k(k,fmax,speed,nt,dt,vt,gtk[ik],gtk[ik]);
	}
	
	/* Fourier transform g(t,k) to g(t,x) */
	pfa2cr(1,2,nt,nxfft,gtk[0],gtx[0]);
	if (verbose) fprintf(stderr,"\tinverse Fourier transform done\n");
	
	/* output migrated traces with headers */
	for (ix=0; ix<nx; ++ix) {
		efread(&tr,HDRBYTES,1,headerfp);
		memcpy( (void *) tr.data,
				(const void *) gtx[ix], nt*sizeof(float));
		puttr(&tr);
	}

	/* clean up */
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	efclose(tracefp);
	if (istmpdir) eremove(tracefile);

	return(CWP_Exit());
}

void mig1k (float k, float fmax, float speed, int nt, float dt, 
	float *v, complex *p, complex *q)
/*****************************************************************************
migration in t-k domain for one wavenumber
******************************************************************************
Input:
k		wavenumber
fmax		maximum frequency
speed		speed parameter - >>1.0 for lots of dispersion
nt		number of time samples
dt		time sampling interval
v		array[nt] containing interval velocities v(t)
p		array[nt] containing input p(t;k)

Output:
q		array[nt] containing migrated q(t;k)
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 11/05/90
*****************************************************************************/
{
	int nfac=5,ns,is,it,i1,i2,i1stop;
	float fac=0.75,ds,g00r,g00i,g01r,g01i,g10r,g10i,g11r,g11i,
		temp,vmin,vmax,ct,
		*vs,*cs,*cp,*grp,*gip;
	complex czero=cmplx(0.0,0.0),*gs;
	
	/* determine time sampling to avoid excessive grid dispersion */
	for (it=1,vmin=vmax=v[0]; it<nt; ++it) {
		if (v[it]<vmin) vmin = v[it];
		if (v[it]>vmax) vmax = v[it];
	}
	if (k!=0.0) {
		ds = fac*MAX(0.4*PI/ABS(vmax*k),0.1*vmin/(vmax*fmax));
		ds *= speed;
		ds = MIN(ds,dt);
	} else {
		ds = dt;
	}
	ns = 1+(nt-1)*dt/ds;
	ns = MIN(ns,nfac*nt);
	ds = (nt-1)*dt/(ns-1);
	fprintf(stderr,"ns=%d\n",ns);
	
	/* allocate workspace */
	vs = ealloc1float(ns);
	cs = ealloc1float(ns*2);
	gs = ealloc1complex(ns);
	
	/* resample v(t) and p(t) */
	ress8r(nt,dt,0.0,v,v[0],v[nt-1],ns,ds,0.0,vs);
	ress8c(nt,dt,0.0,p,czero,czero,ns,ds,0.0,gs);

        /* compute finite-difference coefficients */
	for (is=0; is<ns; is++) {
		temp = 0.125*vs[is]*k*ds;
		temp = temp*temp;
		temp = (1.0-temp)/(1.0+temp);
		cs[2*is] = cs[2*is+1] = temp;
	}

	/* loop over t2 = (tau-t)/sqrt(2) */
	for (i2=2-ns; i2<ns; i2++) {

		/* determine t1 stop index */
		i1stop = (i2<=0)?1-i2:i2;

		/* initialize finite-difference star and coefficient */
		g00r = g00i = g01r = g01i = 0.0;
		grp = (float*)(&gs[ns-1]);
		gip = grp+1;
		cp = &cs[i2+ns-2];
		ct = *cp--;

		/* loop over t1 = (tau+t)/sqrt(2) */
		for (i1=ns-1; i1>=i1stop; i1--) {

			/* update real part */
			g10r = g00r;
			g11r = g01r;
			g00r = *grp;
			*grp = g01r = ct*(g11r+g00r)-g10r;

			/* update imaginary part */
			g10i = g00i;
			g11i = g01i;
			g00i = *gip;
			*gip = g01i = ct*(g11i+g00i)-g10i;

			/* update pointers and finite-difference coefficient */
			grp -= 2; gip -= 2;
			ct = *cp--;
		}
	}
	
	/* resample q(t) */
	ress8c(ns,ds,0.0,gs,czero,czero,nt,dt,0.0,q);
	
	/* free workspace */
	free1float(vs);
	free1float(cs);
	free1complex(gs);
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(headerfp);
	efclose(tracefp);
	eremove(headerfile);
	eremove(tracefile);
	exit(EXIT_FAILURE);
}
