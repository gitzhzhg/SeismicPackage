/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMIGPS: $Revision: 1.15 $ ; $Date: 2011/11/16 22:14:43 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUMIGPS - MIGration by Phase Shift with turning rays			",
"									",
" sumigps <stdin >stdout [optional parms]				",
"									",
" Required Parameters:							",
" 	None								",
"									",
" Optional Parameters:							",
" dt=from header(dt) or .004	time sampling interval			",
" dx=from header(d2) or 1.0	distance between sucessive cdp's	",
" ffil=0,0,0.5/dt,0.5/dt  trapezoidal window of frequencies to migrate	",
" tmig=0.0		times corresponding to interval velocities in vmig",
" vmig=1500.0		interval velocities corresponding to times in tmig",
" vfile=		binary (non-ascii) file containing velocities v(t)",
" nxpad=0		number of cdps to pad with zeros before FFT	",
" ltaper=0		length of linear taper for left and right edges", 
" verbose=0		=1 for diagnostic print				",
"									",
"									",
" tmpdir= 	 if non-empty, use the value as a directory path	",
"		 prefix for storing temporary files; else if the	",
"	         the CWP_TMPDIR environment variable is set use		",
"	         its value for the path; else use tmpfile()		",
" 									",
" Notes:								",
" Input traces must be sorted by either increasing or decreasing cdp.	",
"									",
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
NULL};

/* Credits:
 *	CWP: Dave Hale (originally called supsmig.c)
 *
 *  Trace header fields accessed:  ns, dt, d2
 */
/**************** end self doc *******************************************/

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
TATable *tableta (int np, int ntau, float dtau, float vtau[],
	int nt, float dt, int verbose);
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
	int nt,nx,nxfft,nxpad,ix,it,nk,ik,nfil,ntflag,
		ltaper,ntmig,nvmig,itmig,verbose,np;
	float dt,dx,dk,taper,t,k,fftscl,ffil[4],
		*tmig,*vmig,*vt,**gtx;
	complex **gtk;
	char *vfile="";
	TATable *table;
	char *tmpdir;	/* directory path for tmp files			*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path		*/

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;

	/* let user give dt and/or dx from command line */
	if (!getparfloat("dt", &dt)) {
		if (tr.dt) { /* is dt field set? */
			dt = ((double) tr.dt)/1000000.0;
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
	
	/* build time/amplitude table */
	table = tableta(np,nt,dt,vt,nt,dt,verbose);
	if (verbose) fprintf(stderr,"\tTime/amplitude table built\n");
	
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
		efread(&tr,HDRBYTES,1,headerfp);
		memcpy((void *) tr.data,
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

TATable *tableta (int np, int ntau, float dtau, float vtau[],
	int nt, float dt, int verbose)
/*****************************************************************************
tabulate time shifts and amplitudes for use in phase-shift migration
******************************************************************************
Input:
np		number of slopes p
ntau		number of vertical two-way times tau
dtau		vertical time sampling interval
vtau		array[ntau] of velocities as a function of tau
nt		number of time samples
dt		time sampling interval
verbose	 non-zero to print diagnostic info on stderr

Returned: 	pointer to the TATable
******************************************************************************/
{
	int jp,ktau,itau,jtau,ltau,it;
	float pj,taui,taul,tauj,ti,tl,ai,al,
		vel,dvel,angle,cosa,frac,
		*p,*taumax,*taumin,*tturn,**t,**a;
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
		
	/* loop over slopes p */
	for (jp=0; jp<np; ++jp) {
	
		/* slope p */
		p[jp] = pj = 2.0/vtau[0]*sqrt((float)(jp)/(float)(np-1));
			
		/* time and amplitude at tau = 0 */
		t[jp][0] = 0.0;
		a[jp][0] = 1.0;
		
		/* tau index */
		ktau = 0;
		
		/* initial ray tracing parameters */
		taui = 0.0;
		ti = 0.0;
		ai = 1.0;
		vel = 0.5*vtau[0];
		dvel = 0.5*(vtau[1]-vtau[0]);
		angle = asin(MIN(1.0,pj*vel));
		
		/* loop over times t */
		for (it=1; it<nt; ++it) {
			
			/* remember last tau, t, and a */
			taul = taui;
			tl = ti;
			al = ai;
			
			/* update cosine of propagation angle */
			cosa = cos(angle);
			
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
			
			/* update angle */
			angle += pj*dvel;
			
			/* update velocity and first difference */
			if (itau<ntau-1) {
				frac = (taui-(itau*dtau))/dtau;
				dvel = 0.5*(vtau[itau+1]-vtau[itau]);
				vel = 0.5*((1.0-frac)*vtau[itau] +
					frac*vtau[itau+1]);
			} else {
				dvel = 0.5*(vtau[ntau-1]-vtau[ntau-2]);
				vel = 0.5*vtau[ntau-1] +
					(taui-(ntau-1)*dtau)*dvel/dtau;
			}
		}
		
		/* extrapolate times and amplitudes for interpolation */
		for (jtau=ktau+1; jtau<ntau; ++jtau) {
			t[jp][jtau] = t[jp][ktau];
			a[jp][jtau] = a[jp][ktau];
		}
		
		/* print turning time and max,min tau */
		if (verbose)
			fprintf(stderr,"p=%g tturn=%g taumax=%g taumin=%g\n",
				p[jp],tturn[jp],taumax[jp],taumin[jp]);
	}
	return table;
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
