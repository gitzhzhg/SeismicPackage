/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMIGPSTI: $Revision: 1.4 $ ; $Date: 2011/11/16 22:14:43 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUMIGPSTI - MIGration by Phase Shift for TI media with turning rays	",
"									",
" sumigpsti <stdin >stdout [optional parms]				",
"									",
" Required Parameters:							",
" 	None								",
"									",
" Optional Parameters:							",
" dt=from header(dt) or .004	time sampling interval			",
" dx=from header(d2) or 1.0	distance between sucessive cdp's	",
" ffil=0,0,0.5/dt,0.5/dt  trapezoidal window of frequencies to migrate	",
" tmig=0.0	times corresponding to interval velocities in vmig	",
" vnmig=1500.0	interval NMO velocities corresponding to times in tmig	",
" vmig=1500.0	interval velocities corresponding to times in tmig	",
" etamig=0.0	interval eta values corresponding to times in tmig	",
" vnfile=	binary (non-ascii) file containing NMO velocities vn(t)	",
" vfile=	binary (non-ascii) file containing velocities v(t)	",
" etafile=	binary (non-ascii) file containing eta values eta(t)	",
" nxpad=0	number of cdps to pad with zeros before FFT		",
" ltaper=0	length of linear taper for left and right edges		", 
" verbose=0	=1 for diagnostic print					",
"									",
" Notes:								",
" Input traces must be sorted by either increasing or decreasing cdp.	",
"									",
" The tmig, vnmig, vmig and etamig arrays specify an interval values	",
" function of time. Linear interpolation and constant extrapolation is	",
" used to determine interval velocities at times not specified.  Values	",
" specified in tmig must increase monotonically.			",
" Alternatively, interval velocities may be stored in a binary file	",
" containing one velocity for every time sample.  If vnfile is specified,",
" then the tmig and vnmig arrays are ignored.				",
"									",
" The time of first sample is assumed to be zero, regardless of the value",
" of the trace header field delrt.					",
"									",
" Trace header fields accessed:  ns and dt				",
"									",
NULL};

/**************** end self doc *******************************************/

/* Credits:
 *	CWP: Dave Hale (originally call supsmig.c)
 *		modified to TI media by Tariq Alkhalifah
 */

typedef struct TATableStruct {
	int np;
	float *p;
	float *taumax;
	float *taumin;
	float *tturn;
	float **t;
	float **a;
} TATable;

void mig1k (TATable *table, float k, float ffil[4], int ntflag,
	int nt, float dt, complex *cp,
	int ntau, float dtau, complex *cq);
TATable *tableta (int np, int ntau, float dtau, float a3333[],
	float a1111[], float a1133[], float a1313[],
	int nt, float dt, int verbose);

segy tr;

int
main(int argc, char **argv)
{
	int nt,nx,nxfft,nxpad,ix,it,nk,ik,nfil,ntflag,
		ltaper,ntmig,nvmig,nvnmig,netamig,itmig,verbose,np;
	float dt,dx,dk,taper,t,k,fftscl,ffil[4],
		*a1111,*a3333,*a1313,*a1133,
		*tmig,*vmig,*vnmig,*etamig,*vt,*vnt,*etat,**gtx;
	float vnt2,vt2,delta;
	complex **gtk;
	char *vfile="",*vnfile="",*etafile="";
	FILE *hfp,*tfp;
	TATable *table;

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;

	/* let user give dt and/or dx from command line */
	if (!getparfloat("dt", &dt)) {
		if (tr.dt) { /* is dt field set? */
			dt = (float) tr.dt / 1000000.0;
		} else { /* dt not set, assume 4 ms */
			dt = 0.004;
			warn("tr.dt not set, assuming dt=0.004");
		}
	}
	if (!getparfloat("dx",&dx)) {
		if (tr.d2) { /* is d2 field set? */
			dx = tr.d2;
		} else {
			dx = 1.0;
			warn("tr.d2 not set, assuming d2=1.0");
		}
	}


	/* get parameters */
	if (!(nfil=getparfloat("ffil",ffil))) {
		ffil[0] = ffil[1] = 0.0;
		ffil[2] = ffil[3] = 0.5/dt;
	} else if (nfil!=4) {
		err("if ffil specified, exactly 4 values must be provided");
	}
	if (!getparint("nxpad",&nxpad)) nxpad=0;
	if (!getparint("ltaper",&ltaper)) ltaper=0;
	if (!getparint("verbose",&verbose)) verbose=0;
	if (!getparint("np",&np)) np=200;
	if (!getparint("ntflag",&ntflag)) ntflag=1;

	/* determine NMO velocity function vnm(t) */
	vnt = ealloc1float(nt);
        if (!getparstring("vnfile",&vnfile)) {
                ntmig = countparval("tmig");
                if (ntmig==0) ntmig = 1;
                tmig = ealloc1float(ntmig);
                if (!getparfloat("tmig",tmig)) tmig[0] = 0.0;
                nvnmig = countparval("vnmig");
                if (nvnmig==0) nvnmig = 1;
                if (nvnmig!=ntmig) err("number of tmig and vnmig must be equal");
                vnmig = ealloc1float(nvnmig);
                if (!getparfloat("vnmig",vnmig)) vnmig[0] = 1500.0;
                for (itmig=1; itmig<ntmig; ++itmig)
                        if (tmig[itmig]<=tmig[itmig-1])
                                err("tmig must increase monotonically");
                for (it=0,t=0.0; it<nt; ++it,t+=dt)
                        intlin(ntmig,tmig,vnmig,vnmig[0],vnmig[ntmig-1],
                                1,&t,&vnt[it]);
        } else {
                if (fread(vnt,sizeof(float),nt,fopen(vnfile,"r"))!=nt)
                        err("cannot read %d velocities from file %s",nt,vnfile);
        }

	/* determine function eta(t) */
	etat = ealloc1float(nt);
        if (!getparstring("etafile",&etafile)) {
                ntmig = countparval("tmig");
                if (ntmig==0) ntmig = 1;
                tmig = ealloc1float(ntmig);
                if (!getparfloat("tmig",tmig)) tmig[0] = 0.0;
                netamig = countparval("etamig");
                if (netamig==0) netamig = 1;
                if (netamig!=ntmig) err("number of tmig and etamig must be equal");
                etamig = ealloc1float(netamig);
                if (!getparfloat("etamig",etamig)) etamig[0] = 0.0;
                for (itmig=1; itmig<ntmig; ++itmig)
                        if (tmig[itmig]<=tmig[itmig-1])
                                err("tmig must increase monotonically");
                for (it=0,t=0.0; it<nt; ++it,t+=dt)
                        intlin(ntmig,tmig,etamig,etamig[0],etamig[ntmig-1],
                                1,&t,&etat[it]);
        } else {
                if (fread(etat,sizeof(float),nt,fopen(etafile,"r"))!=nt)
                        err("cannot read %d velocities from file %s",nt,etafile);
        }
	
	/* determine velocity function v(t) */
	vt = ealloc1float(nt);
	if (!getparstring("vfile",&vfile)) {
		ntmig = countparval("tmig");
		if (ntmig==0) ntmig = 1;
		tmig = ealloc1float(ntmig);
		if (!getparfloat("tmig",tmig)) tmig[0] = 0.0;
		nvmig = countparval("vmig");
		if (nvmig!=ntmig) {
			for(it=0; it<nt; ++it)
				vt[it] = vnt[it];
		} else {
			vmig = ealloc1float(nvmig);
			if (!getparfloat("vmig",vmig)) vmig[0] = vnt[0];
			for (itmig=1; itmig<ntmig; ++itmig)
				if (tmig[itmig]<=tmig[itmig-1])
					err("tmig must increase monotonically");
			for (it=0,t=0.0; it<nt; ++it,t+=dt)
				intlin(ntmig,tmig,vmig,vmig[0],vmig[ntmig-1],
					1,&t,&vt[it]);
		}
	} else {
		if (fread(vt,sizeof(float),nt,fopen(vfile,"r"))!=nt)
			err("cannot read %d velocities from file %s",nt,vfile);
	}
        checkpars();

	/* allocate space for velocity arrays */
	a1111 = ealloc1float(nt);
	a3333 = ealloc1float(nt);
	a1313 = ealloc1float(nt);
	a1133 = ealloc1float(nt);

	/*convert eta and NMO velocity values to elastic coeffecients*/
	for(it=0; it<nt; ++it){
		vnt2   =vnt[it]*vnt[it];
		vt2    =vt[it]*vt[it];
		a1111[it]=vnt2*(1+2*etat[it]);
		a3333[it]=vt2;
		a1313[it]=0.25*vt2;
		delta= 0.5*(vnt2/vt2-1);
		a1133[it]=sqrt(2*delta*a3333[it]*
			(a3333[it]-a1313[it])+(a3333[it]-
			a1313[it])*(a3333[it]-a1313[it]))-a1313[it];
	}
	
	/* copy traces and headers to temporary files */
	tfp = tmpfile();
	hfp = tmpfile();
	nx = 0;
	do {
		nx++;
		fwrite(&tr,HDRBYTES,1,hfp);
		fwrite(tr.data,sizeof(float),nt,tfp);
	} while(gettr(&tr));
	efseeko(hfp,(off_t) 0,SEEK_SET);
	efseeko(tfp,(off_t) 0,SEEK_SET);
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
		efread(gtx[ix],sizeof(float),nt,tfp);
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
	
	/* build time/amplitude table */
	table = tableta(np,nt,dt,a3333,a1111,a1133,a1313,nt,dt,verbose);
	if (verbose) fprintf(stderr,"\tTime/amplitude table built\n");

	/*free space*/
	free1float(vt);
	free1float(vnt);
	free1float(etat);
	free1float(a1111);
	free1float(a3333);
	free1float(a1133);
	free1float(a1313);
	
	/* loop over wavenumbers */
	for (ik=0,k=0.0; ik<nk; ++ik,k+=dk) {
	
		/* report */
		if (verbose && ik%(nk/10>0?nk/10:1)==0)
			fprintf(stderr,"\t%d of %d wavenumbers done\n",
				ik,nk);
		
		/* migrate */
		mig1k(table,k,ffil,ntflag,nt,dt,gtk[ik],nt,dt,gtk[ik]);
	}
	
	/* Fourier transform g(t,k) to g(t,x) */
	pfa2cr(1,2,nt,nxfft,gtk[0],gtx[0]);
	if (verbose) fprintf(stderr,"\tinverse Fourier transform done\n");
	
	/* output migrated traces with headers */
	for (ix=0; ix<nx; ++ix) {
		efread(&tr,HDRBYTES,1,hfp);
		memcpy((void *) tr.data,
				(const void *) gtx[ix], nt*sizeof(float));
		puttr(&tr);
	}

	return(CWP_Exit());
}

void mig1k (TATable *table, float k, float ffil[4], int ntflag,
	int nt, float dt, complex *cp,
	int ntau, float dtau, complex *cq)
/*****************************************************************************
phase shift migration for one wavenumber k via tabulated times and amps
******************************************************************************
Input:
table	 	pointer to the TATable
k		wavenumber
ffil[4]		four frequencies (in Hz) defining trapezoidal filter
ntflag		=1 for normal, 2 for turned, 3 for normal+turned
nt		number of time samples
dt		time sampling interval
cp		array[nt] of data to be migrated
ntau		number of vertical two-way times tau
dtau		vertical time sampling interval

Output:
cq		array[ntau] of migrated data
******************************************************************************/
{
	int ntfft,nw,iwnyq,iwmin,iwmax,iw,itau,it,np,
		ip1,ip2,iwfil0,iwfil1,iwfil2,iwfil3,
		itaumax,itaumin,itab;
	float dw,wnyq,wmin,wmax,w,ampf,p1,p2,a1,a2,wa1,wa2,
		taumaxi,taumini,kow,phst,pwsr,pwsi,pwdr,pwdi,
		ampi,phsi,phsn,
		*p,*taumax,*taumin,*tturn,**t,**a;
	complex *pt,*pw,*qn,*qt;
	static int tabbed=0;
	static float fntab,*ctab,*stab,opi2=1.0/(PI*2.0);

	/* if not already built, build cosine/sine tables */
	if (!tabbed) {
		int itab,ntab=1025;
		float angle,dangle=2.0*PI/(ntab-1);
		ctab = alloc1float(ntab);
		stab = alloc1float(ntab);
		for (itab=0,angle=0.0; itab<ntab; ++itab,angle+=dangle) {
			ctab[itab] = cos(angle);
			stab[itab] = sin(angle);
		}
		tabbed = 1;
		fntab = (float)ntab;
	}
	
	/* parts of time/amplitude table */
	np = table->np;
	p = table->p;
	taumax = table->taumax;
	taumin = table->taumin;
	tturn = table->tturn;
	t = table->t;
	a = table->a;
	
	/* frequency sampling */
	ntfft = npfao(2*nt,4*nt);
	nw = ntfft;
	dw = 2.0*PI/(ntfft*dt);
	
	/* allocate workspace */
	pt = pw = alloc1complex(ntfft);
	qn = alloc1complex(ntau);
	qt = alloc1complex(ntau);
	
	/* nyquist frequency */
	wnyq = PI/dt;
	
	/* index of smallest frequency >= nyquist */
	iwnyq = (ntfft%2)?ntfft/2:ntfft/2+1;
	
	/* determine frequency filter sample indices */
	iwfil0 = MAX(0,MIN(iwnyq,NINT(2.0*PI*ffil[0]/dw)));
	iwfil1 = MAX(0,MIN(iwnyq,NINT(2.0*PI*ffil[1]/dw)));
	iwfil2 = MAX(0,MIN(iwnyq,NINT(2.0*PI*ffil[2]/dw)));
	iwfil3 = MAX(0,MIN(iwnyq,NINT(2.0*PI*ffil[3]/dw)));
	
	/* pad p(t) with zeros */
	for (it=0; it<nt; ++it)
		pt[it] = cp[it];
	for (it=nt; it<ntfft; ++it)
		pt[it].r = pt[it].i = 0.0;
	
	/* Fourier transform p(t) to p(w) (include scaling) */
	pfacc(1,ntfft,pt);
	for (iw=0; iw<nw; ++iw) {
		pw[iw].r *= 1.0/ntfft;
		pw[iw].i *= 1.0/ntfft;
	}
	
	/* initially zero normal and turned images */
	for (itau=0; itau<ntau; ++itau)
		qn[itau].r = qn[itau].i = qt[itau].r = qt[itau].i = 0.0;
	
	/* minimum and maximum frequency indices */
	wmin = ABS(k)/p[np-1];
	iwmin = MAX(MAX(1,iwfil0),(int)(wmin/dw));
	if (p[0]<=ABS(k)/wnyq)
		wmax = wnyq;
	else
		wmax = ABS(k)/p[0];
	iwmax = MIN(MIN(iwnyq-1,iwfil3),(int)(wmax/dw));
	
	/* loop over frequencies */
	for (iw=iwmin,w=iwmin*dw; iw<=iwmax; ++iw,w+=dw) {
			
		/* if slope not within range of table, continue */
		kow = ABS(k)/w;
		if (kow>p[np-1]) continue;
		
		/* amplitude of frequency filter */
		if (iwfil0<=iw && iw<iwfil1)
			ampf = (float)(iw-iwfil0)/(float)(iwfil1-iwfil0);
		else if (iwfil2<iw && iw<=iwfil3)
			ampf = (float)(iwfil3-iw)/(float)(iwfil3-iwfil2);
		else
			ampf = 1.0;
		
		/* weights for interpolation in table */
		xindex(np,p,kow,&ip1);
		ip1 = MIN(ip1,np-2);
		ip2 = ip1+1;
		p1 = p[ip1];
		p2 = p[ip2];
		a1 = (p2*p2-kow*kow)/(p2*p2-p1*p1);
		a2 = (kow*kow-p1*p1)/(p2*p2-p1*p1);
		wa1 = w*a1;
		wa2 = w*a2;
		
		/* maximum tau index for normal image */
		taumaxi = a1*taumax[ip1]+a2*taumax[ip2];
		itaumax = MIN(ntau-1,NINT(taumaxi/dtau));
		
		/* minimum tau index for turned image */
		taumini = a1*taumin[ip1]+a2*taumin[ip2];
		itaumin = MAX(0,NINT(taumini/dtau));
		
		/* phase at turning point */
		phst = 2.0*(wa1*tturn[ip1]+wa2*tturn[ip2]);
		
		/* filtered sum and differences for positive and negative w */
		pwsr = ampf*(pw[iw].r+pw[nw-iw].r);
		pwsi = ampf*(pw[iw].i+pw[nw-iw].i);
		pwdr = ampf*(pw[iw].r-pw[nw-iw].r);
		pwdi = ampf*(pw[iw].i-pw[nw-iw].i);
		
		/* accumulate normal image */
		if (ntflag&1) {
		for (itau=0; itau<itaumax; ++itau) {
			ampi = a1*a[ip1][itau]+a2*a[ip2][itau];
			phsi = wa1*t[ip1][itau]+wa2*t[ip2][itau];
			phsn = phsi*opi2;
			itab = fntab*(phsn-(int)phsn);
			qn[itau].r = qn[itau].r +
				ampi*(pwsr*ctab[itab]+pwdi*stab[itab]);
			qn[itau].i = qn[itau].i +
				ampi*(pwsi*ctab[itab]-pwdr*stab[itab]);
		}
		}
		
		/* accumulate turned image */
		if (ntflag&2) {
		for (itau=itaumin+1; itau<itaumax; ++itau) {
			ampi = a1*a[ip1][itau]+a2*a[ip2][itau];
			phsi = phst-(wa1*t[ip1][itau]+wa2*t[ip2][itau]);
			phsn = phsi*opi2;
			itab = fntab*(phsn-(int)phsn);
			qt[itau].r = qt[itau].r +
				ampi*(pwsr*stab[itab]-pwdi*ctab[itab]);
			qt[itau].i = qt[itau].i +
				ampi*(pwsi*stab[itab]+pwdr*ctab[itab]);
		}
		}
	}
	
	/* sum normal and turned images */
	for (itau=0; itau<ntau; ++itau) {
		cq[itau].r = qn[itau].r+qt[itau].r;
		cq[itau].i = qn[itau].i+qt[itau].i;
	}
	
	/* free workspace */
	free1complex(pt);
	free1complex(qn);
	free1complex(qt);
}	

TATable *tableta (int np, int ntau, float dtau, float a3333[],
	float a1111[], float a1133[], float a1313[],
	int nt, float dt, int verbose)
/*****************************************************************************
tabulate time shifts and amplitudes for use in phase-shift migration
******************************************************************************
Input:
np		number of slopes p
ntau		number of vertical two-way times tau
dtau		vertical time sampling interval
a1111		array[ntau] of a1111 elastic coeffecient as a function of tau
a3333		array[ntau] of a3333 elastic coeffecient as a function of tau
a1133		array[ntau] of a1133 elastic coeffecient as a function of tau
a1313		array[ntau] of a1313 elastic coeffecient as a function of tau
nt		number of time samples
dt		time sampling interval
verbose	 non-zero to print diagnostic info on stderr

Returned: 	pointer to the TATable
******************************************************************************/
{
	int jp,ktau,itau,jtau,ltau,it;
	float pj,taui,taul,tauj,ti,tl,ai,al,
		angle,cosa,frac,
		*p,*taumax,*taumin,*tturn,**t,**a;
	float f1,f2,f3,f4,f5,f6,px2,pz2,vp,eps,
		alpha,beta,gamma,det,rad,signbeta,q;
	float a1111t,a3333t,a1313t,a1133t;
	TATable *table;
	
	/* allocate table */
	table = alloc1(1,sizeof(TATable));
	table->np = np;
	table->p = p = alloc1float(np);
	table->taumax = taumax = alloc1float(np);
	table->taumin = taumin =alloc1float(np);
	table->tturn = tturn = alloc1float(np);
	table->t = t = alloc2float(ntau,np);
	table->a = a = alloc2float(ntau,np);
	
	for (it=0; it<nt; ++it){
		a1111[it] = .25*a1111[it];
		a3333[it] = .25*a3333[it];
		a1313[it] = .25*a1313[it];
		a1133[it] = .25*a1133[it];
	}
		
	/* loop over slopes p */
	for (jp=0; jp<np; ++jp) {
	
		/* slope p */
		p[jp] = pj = 2.0/sqrt(a1111[0])*sqrt((float)(jp)/(float)(np-1));
			
		/* time and amplitude at tau = 0 */
		t[jp][0] = 0.0;
		a[jp][0] = 1.0;
		
		/* tau index */
		ktau = 0;
		
		/* initial ray tracing parameters */
		taui = 0.0;
		ti = 0.0;
		ai = 1.0;

		a1111t = a1111[0];
		a3333t = a3333[0];
		a1313t = a1313[0];
		a1133t = a1133[0];

		f1 = a1111t+a1313t;
		f2 = a3333t+a1313t;
		f3 = a1111t-a1313t;
		f4 = a3333t-a1313t;
		f5 = 2.*(a1133t+a1313t)*(a1133t+a1313t);
		f6 = 2;

		eps   = .0001;
		px2   = pj*pj;
	    	alpha = f2*f2-f4*f4;
	    	beta  = 2*((f1*f2+f3*f4-f5)*px2-f2*f6);
	    	gamma = f6*f6-(2.*f1*f6-(f1*f1-f3*f3)*px2)*px2;
	    	det   = beta*beta-4.*alpha*gamma;

		if (det<0) continue;
		rad = sqrt(det);
		if(ABS(beta)>eps)   signbeta = ABS(beta)/beta;
	    	else                signbeta = 1.;
		q    = -.5*(beta+signbeta*rad);
		pz2  = gamma/q;
		if(pz2<0) continue;
		vp   = 1/sqrt(px2+pz2);

		angle = asin(MIN(1.0,pj*vp));
		
		/* loop over times t */
		for (it=1; it<nt; ++it) {
			
			/* remember last tau, t, and a */
			taul = taui;
			tl = ti;
			al = ai;
			
			/* update cosine of propagation angle */
			cosa = cos(angle)*sqrt(a3333t)/vp;
			
			/* update tau, t, and a */
			taui += dt*cosa;
			ti += dt*cosa*cosa;
			ai = 1.0;
			
			/* if ray emerges at surface, break */
			if (taui<0.0) break;
			
			/* update turning time and max,min tau */
			if (taui>=taul) {
				tturn[jp] = ti;
				taumax[jp] = taui;
				taumin[jp] = taui;
			} else {
				taumin[jp] = taui;
			}
			
			/* compute tau sample indices */
			itau = (int)(taui/dtau);
			ltau = (int)(taul/dtau);
			
			/* loop over tau samples crossed by ray */
			for (jtau=ltau+1; jtau<=MIN(itau,ntau-1); ++jtau) {
				
				/* tau of sample crossed */
				tauj = jtau*dtau;
				
				/* time and amp via linear interpolation */
				frac = (tauj-taul)/(taui-taul);
				ktau++;
				t[jp][ktau] = (1.0-frac)*tl+frac*ti;
				a[jp][ktau] = (1.0-frac)*al+frac*ai;
			}

			if (itau<ntau-1) {
				frac = (taui-(itau*dtau))/dtau;
				a1111t = (1.0-frac)*a1111[itau] +
					frac*a1111[itau+1];
				a3333t = (1.0-frac)*a3333[itau] +
					frac*a3333[itau+1];
				a1313t = (1.0-frac)*a1313[itau] +
					frac*a1313[itau+1];
				a1133t = (1.0-frac)*a1133[itau] +
					frac*a1133[itau+1];
			} else {
				a1111t = a1111[ntau-1] +
					(taui-(ntau-1)*dtau)*
					(a1111[itau]-a1111[itau-1])/dtau;
				a3333t = a3333[ntau-1] +
					(taui-(ntau-1)*dtau)*
					(a3333[itau]-a3333[itau-1])/dtau;
				a1313t = a1313[ntau-1] +
					(taui-(ntau-1)*dtau)*
					(a1313[itau]-a1313[itau-1])/dtau;
				a1133t = a1133[ntau-1] +
					(taui-(ntau-1)*dtau)*
					(a1133[itau]-a1133[itau-1])/dtau;
			}
			
		

			f1 = a1111t+a1313t;
			f2 = a3333t+a1313t;
			f3 = a1111t-a1313t;
			f4 = a3333t-a1313t;
			f5 = 2.*(a1133t+a1313t)*(a1133t+a1313t);
			f6 = 2;

			eps   = .0001;
	    		alpha = f2*f2-f4*f4;
	    		beta  = 2*((f1*f2+f3*f4-f5)*px2-f2*f6);
	    		gamma = f6*f6-(2.*f1*f6-(f1*f1-f3*f3)*px2)*px2;
	    		det   = beta*beta-4.*alpha*gamma;

			if (det<0) continue;
			rad = sqrt(det);
			if(ABS(beta)>eps)   signbeta = ABS(beta)/beta;
	    		else                signbeta = 1.;
			q    = -.5*(beta+signbeta*rad);
			pz2  = gamma/q;
			if(pz2<0) continue;
			vp   = 1/sqrt(px2+pz2);
			angle = asin(MIN(1.0,pj*vp));

		}

		
		/* extrapolate times and amplitudes for interpolation */
		for (jtau=ktau+1; jtau<ntau; ++jtau) {
			t[jp][jtau] = t[jp][ktau];
			a[jp][jtau] = a[jp][ktau];
		}
		
		/* print turning time and max,min tau */
		/*
		if (verbose)
			fprintf(stderr,"p=%g tturn=%g taumax=%g taumin=%g\n",
				p[jp],tturn[jp],taumax[jp],taumin[jp]);
		*/
	}
	return table;
}
