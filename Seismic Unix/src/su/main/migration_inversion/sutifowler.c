/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTIFOWLER: $Revision: 1.5 $ ; $Date: 2011/11/16 22:14:43 $	*/

#include "su.h"
#include "segy.h"
#include "VND.h"
#define LTABLE 8	/* number of coef in phased sinc table */
#define NTABLE 513	/* number of table entries in phased sinc table */
#define NP 10003	/* number of entries in (velocity,slowness) tables */

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUTIFOWLER   VTI constant velocity prestack time migration		",
"	      velocity analysis via Fowler's method			",
"									",
" sutifowler ncdps=250 vmin=1500 vmax=6000 dx=12.5			",
"									",
" Required Parameter:							",
" ncdps=		number of input cdp's				",
" Optional Parameters:							",
" choose=1	1 do full prestack time migration			",
"		2 do only DMO						",
"		3 do only post-stack migrations				",
"		4 do only stacking velocity analysis			",
" getcvstacks=0	flag to set to 1 if inputting precomputed cvstacks	",
"		(vmin, nvstack, and ncdps must match SUCVS4FOWLER job)	",
" vminstack=vmin	minimum velocity panel in m/s in input cvstacks	",
" etamin=0.		minimum eta (see paper by Tariq Alkhalifah)	",
" etamax=0.5	maximum eta (see paper by Tariq Alkhalifah)		",
" neta=1	number of eta values to image				",
" d=0.		Thomsen's delta						",
" vpvs=0.5	assumed vp/vs ratio (not critical -- default almost always ok)",
" dx=25.	cdp x increment						",
" vmin=1500.	minimum velocity panel in m/s to output			",
" vmax=8000.	maximum velocity panel in m/s to output			",
" nv=75	 number of velocity panels to output				",
" nvstack=180	number of stacking velocity panels to compute		",
"		     ( Let offmax be the maximum offset, fmax be	",
"		     the maximum freq to preserve, and tmute be		",
"		     the starting mute time in sec on offmax, then	",
"		     the recommended value for nvstack would be		",
"		     nvstack = 4 +(offmax*offmax*fmax)/(0.6*vmin*vmin*tmute)",
"		     ---you may want to make do with less---)		",
" nxpad=0	  number of traces to padd for spatial fft		",
"		     Ideally nxpad = (0.5*tmax*vmax+0.5*offmax)/dx	",
" lmute=24	 length of mute taper in ms				",
" nonhyp=1	  1 if do mute at 2*offset/vmin to avoid non-hyperbolic ",
"				moveout, 0 otherwise			",
" lbtaper=0	length of bottom taper in ms				",
" lstaper=0	length of side taper in traces				",
" dtout=1.5*dt	output sample rate in s,   note: typically		",
"				fmax=salias*0.5/dtout			",
" mxfold=120	maximum number of offsets/input cmp			",
" salias=0.8	fraction of output frequencies to force within sloth	",
"		     antialias limit.  This controls muting by offset of",
"		     the input data prior to computing the cv stacks	",
"		     for values of choose=1 or choose=2.		",
" file=sutifowler	root name for temporary files			",
" p=not		Enter a path name where to put temporary files.		",
"	  	specified  Can enter multiple times to use multiple disk",
"		systems.						",
"		     The default uses information from the .VND file	",
"		     in the current directory if it exists, or puts 	",
"		     unique temporary files in the current directory.	",
" ngroup=20	Number of cmps per velocity analysis group.		",
" printfile=stderr    The output file for printout from this program.	",
"									",
" Required trace header words on input are ns, dt, cdp, offset.		",
" On output, trace headers are rebuilt from scratch with		",
" ns - number of samples						",
" dt - sample rate in usec						",
" cdp - the output cmp number (0 based)					",
" offset - the output velocity						",
" tracf	- the output velocity index (0 based)				",
" fldr - index for velocity analysis group (0 based, groups of ngroup cdps)",
" ep - central cmp for velocity analysis group				",
" igc - index for choice of eta (0 based)				",
" igi - eta*100								",
" sx=gx	- x coordinate as icmp*dx					",
" tracl=tracr -sequential trace count (1 based)				",
"									",
" Note: Due to aliasing considerations, the small offset-to-depth	",
" ratio assumption inherent in the TI DMO derivation, and the		",
" poor stacking of some large-offset events associated with TI non-hyperbolic",
" moveout, a fairly stiff initial mute is recommended for the		",
" long offsets.  As a result, this method may not work well		",
" where you have multiple reflections to remove via stacking.		",
" 									",
" Note: The temporary files can be split over multiple disks by building",
" a .VND file in your working directory.  The .VND file is ascii text	",
" with the first line giving the number of directories followed by	",
" successive lines with one line per directory name.			",
"									",
" Note: The output data order has primary key equal to cdp, secondary	",
" key equal to eta, and tertiary key equal to velocity.			",
NULL};

/* Credits:
 *	CWP: John Anderson (visitor to CSM from Mobil) Spring 1993
 *
 */
/**************** end self doc ********************************/


static void cvstack(VND *vnda, VND *vnd, int icmp, int noff, float *off,
		float *mute, int lmute, int nv, float *p2,
		float dt, float dtout);
static void vget( float a, float b, float e, float d,
		float theta, float *vel);
VND *ptabledmo(int nv, float *v, float etamin, float deta, int neta,
		float d, float vsvp, int np, float dp, float dp2, char *file);
VND *ptablemig(int nv, float *v, float etamin, float deta,
		int neta, float d, float vsvp, int np, char *file);
static void taper (int lxtaper, int lbtaper,
		int nx, int ix, int nt, float *trace);

segy tr;	/* input and output SEGY data */
FILE *fpl;	/* file pointer for print listing */
int main(int argc, char **argv)
{
	VND *vnd=NULL;	/* big file holding data, all cmps, all etas, all velocities */
	VND *vnda=NULL;	/* holds one input cmp gather */
	VND *vndb=NULL;	/* holds (w,v) for one k component */
	VND *vndvnmo=NULL;	/* holds (vnmo,p) table for ti dmo */
	VND *vndvphase=NULL;	/* holds (vphase,p) table for ti Stolt migration */
	long N[2];	/* holds number of values in each dimension for VND opens */
	long key[2];	/* holds key in each dimension for VND i/o */
	char **dir=NULL; /* could hold list of directories where to put VND temp files */
	char *file;	/* root name for temporary files */
	char *printfile; /* name of file for printout */
	complex *crt;
	complex *ctemp;
	complex czero;
	float *rt;
	char *ccrt;
	char *fname;
	
	float etamin;	/* minimum eta scan to compute */
	float etamax;	/* maximum eta scan to compute */
	float deta;	/* increment in eta to compute for eta scan */
	float dx;	/* cmp spatial sampling interval */
	float dk;	/* wavenumber increment */
	float dv;	/* velocity increment */
	float vmin;	/* minimum output velocity */
	float vmax;	/* maximum output velocity */
	float dt;	/* input sample rate in seconds */
	float dtout;	/* output sample rate in seconds */
	float *mute;	/* array of mute times for this cmp */
	float *off;	/* array of offsets for this cmp */
	float *v;	/* array of output velocities */
	float *p2stack;	/* array of stacking 1/(v*v) */
	float *rindex;	/* array of interpolation indices */
	float dp2=0.0;	/* increment in slowness squared for input cvstacks */
	float scale;	/* used for trace scale factor */
	float p;	/* horizontal slowness */
	float p2;	/* p*p */
	float v2;	/* velocity squared */
	float ak;	/* horizontal wavenumber */
	float dw;	/* angular frequency increment */
	float *w;	/* array holding w values for Fowler */
	float factor;	/* scale factor */
	float d;	/* Thomsen's delta */
	float vsvp;	/* vs/vp ratio */
	float dp;	/* increment of slowness values in vndvnmo table */
	float rp;	/* real valued index in p */
	float wgt;	/* weight for linear interpolation */
	float fmax;	/* maximum frequency to use for antialias mute */
	float salias;	/* fraction of frequencies to be within sloth antialias limit */
	float dpm;	/* slowness increment in TI migration table */
	float fw;	/* first w in Stolt data table */
	float vminstack;/* only used if reading precomputed cvstacks, minimum stacking vel */

	int neta;	/* number of eta scans to compute */
	int ichoose;	/* defines type of processing to do */
	int ncmps;	/* number of input and output cmps */
	int nv;		/* number of output velocity panels to generate */
	int nvstack;	/* number of cvstack panels to generate */
	int ntpad;	/* number of time samples to padd to avoid wraparound */
	int nxpad;	/* number of traces to padd to avoid wraparound */
	int lmute;	/* number of samples to taper mute */
	int lbtaper;	/* length of bottom time taper in ms */
	int lstaper;	/* length of side taper in traces */
	int mxfold;	/* maximum allowed number of input offsets/cmp */
	int icmp;	/* cmp index */
	int ntfft;	/* length of temporal fft for Fowler */
	int ntffts;	/* length of temporal fft for Stolt */
	int nxfft;	/* length of spatial fft */
	int ntfftny;	/* count of freq to nyquist */
	int nxfftny;	/* count of wavenumbers to nyquist */
	int nmax;	/* used to compute max number of samples for array allocation */
	int oldcmp;	/* current cdp header value */
	int noff;	/* number of offsets */
	int k;		/* wavenumber index */
	int iwmin;	/* minimum freq index */
	int TI;		/* 0 for isotropic, 1 for transversely isotropic */
	long it;	/* time index */
	long iw;	/* index for angular frequency */
	long nt;	/* number of input time samples */
	long ntout;	/* number of output time samples */
	long iv;	/* velocity index */
	long ip;	/* slowness index */
	long ieta;
	int nonhyp;	/* flag equals 1 if do mute to avoid non-hyperbolic events */
	int getcvstacks;/* flag equals 1 if input cvstacks precomputed */
	int ngroup;	/* number of traces per vel anal group */
	int ndir;	/* number of user specified directories for temp files */

/******************************************************************************/
/* 	input parameters, allocate buffers, and define reusable constants     */
/******************************************************************************/
	initargs(argc, argv);
	requestdoc(1);

	/* get first trace and extract critical info from header */
	if(!gettr(&tr)) err("Can't get first trace \n");
	nt=tr.ns;
	dt=0.000001*tr.dt;
	oldcmp=tr.cdp;

	if (!getparstring("printfile",&printfile)) printfile=NULL;
	if (printfile==NULL) {
		fpl=stderr;
	}else{
		fpl=fopen(printfile,"w");
	}

	if (!getparfloat("salias",&salias)) salias=0.8;
 	if(salias>1.0) salias=1.0;
	if (!getparfloat("dtout",&dtout)) dtout=1.5*dt;
	ntout=1+nt*dt/dtout;
	if (!getparint("getcvstacks",&getcvstacks)) getcvstacks=0;
	if(getcvstacks) {
		dtout=dt;
		ntout=nt;
	}
	fmax=salias*0.5/dtout;
	fprintf(fpl,"sutifowler: ntin=%ld dtin=%f\n",nt,dt);
	fprintf(fpl,"sutifowler: ntout=%ld dtout=%f\n",ntout,dtout);
	if (!getparstring("file",&file)) file="sutifowler";
	if (!getparfloat("dx",&dx)) dx=25.;
	if (!getparfloat("vmin",&vmin)) vmin=1500.;
	if (!getparfloat("vmax",&vmax)) vmax=8000.;
	if (!getparfloat("vminstack",&vminstack)) vminstack=vmin;
	if (!getparfloat("d",&d)) d=0.0;
	if (!getparfloat("etamin",&etamin)) etamin=0.0;
	if (!getparfloat("etamax",&etamax)) etamax=0.5;
	if (!getparfloat("vsvp",&vsvp)) vsvp=0.5;
	if (!getparint("neta", &neta)) neta = 1;
	if (fabs(etamax-etamin)<1.0e-7) neta = 1;
	if (neta < 1) neta = 1;
	if (!getparint("choose", &ichoose)) ichoose = 1;
	if (!getparint("ncdps", &ncmps)) err("sutifowler: must enter ncdps");
	if (!getparint("nv", &nv)) nv = 75;
	if (!getparint("nvstack", &nvstack)) nvstack = 180;
	if (!getparint("ntpad", &ntpad)) ntpad = 0.1*ntout;
	if (!getparint("nxpad", &nxpad)) nxpad = 0;
	if (!getparint("lmute", &lmute)) lmute = 24;
	lmute=1 + 0.001*lmute/dtout;
	if (!getparint("lbtaper", &lbtaper)) lbtaper = 0;
	if (!getparint("lstaper", &lstaper)) lstaper = 0;
	if (!getparint("mxfold", &mxfold)) mxfold = 120;
	if (!getparint("nonhyp",&nonhyp)) nonhyp=1.;
	if (!getparint("ngroup", &ngroup)) ngroup = 20;
	ndir = countparname("p");
	if(ndir==0) {
		ndir=-1;
	}else{
		dir = (char **)VNDemalloc(ndir*sizeof(char *),"dir");
		for(k=0;k<ndir;k++) {
			it=getnparstring(k+1,"p",&dir[k]);
		}
	}
        checkpars();

	lbtaper=lbtaper/(1000.*dt);
	TI=0;
	if(fabs(d)>0. || fabs(etamin)>0 || neta>1 ) TI=1;
	if(TI) fprintf(fpl,"sutifowler: operation in TI mode\n");
	deta = 0.;
	if(neta>1) deta=(etamax-etamin)/(neta-1);
	dp=1./(vmin*(NP-5));
	if(TI) dp=dp*sqrt(1.+2.*fabs(etamin));
	if(ichoose>2) nvstack=nv;
	if(ichoose==1 || ichoose==2 || ichoose==3) {
		ntfft=ntout+ntpad;
	}else{
		ntfft=1;
	}
	if(ichoose==1 || ichoose==3) {
		ntffts=2*ntout/0.6;
	}else{
		ntffts=1;
	}
	ntfft=npfao(ntfft,2*ntfft);
	ntffts=npfao(ntffts,2*ntffts);
	dw=2.*PI/(ntfft*dtout);
	nxfft=npfar(ncmps+nxpad);
	dk=2.*PI/(nxfft*dx);
	fprintf(fpl,"sutifowler: ntfft=%d ntffts=%d nxfft=%d\n",ntfft,ntffts,nxfft);
	czero.r=czero.i=0.;
	scale=1.;
	if(ichoose<5) scale=1./(nxfft);
	if(ichoose==1 || ichoose==2 ) scale*=1./ntfft;
	if(ichoose==1 || ichoose==3 ) scale*=1./ntffts;
	nxfftny = nxfft/2 + 1;
	ntfftny = ntfft/2 + 1;
	nmax = nxfftny;
	if(ntfft > nmax) nmax=ntfft;
	if((NP/2+1)>nmax) nmax=(NP/2+1);
	if(nvstack>nmax) nmax=nvstack;
	if(nv*neta>nmax) nmax=nv*neta;
	ctemp = (complex *)VNDemalloc(nmax*sizeof(complex),"allocating ctemp");
	rindex=(float *)VNDemalloc(nmax*sizeof(float),"allocating rindex");
	if(ntffts > nmax) nmax=ntffts;
	crt = (complex *)VNDemalloc(nmax*sizeof(complex),"allocating crt");
	rt = (float *)crt;
	ccrt = (char *)crt;
	fprintf(fpl,"sutifowler: nv=%d nvstack=%d\n",nv,nvstack);
	v=(float *)VNDemalloc(nv*sizeof(float),"allocating v");
	p2stack=(float *)VNDemalloc(nvstack*sizeof(float),"allocating p2stack");
	mute=(float *)VNDemalloc(mxfold*sizeof(float),"allocating mute");
	off=(float *)VNDemalloc(mxfold*sizeof(float),"allocating off");
	fprintf(fpl,"sutifowler: allocating and filling w array\n");
	w=(float *)VNDemalloc(ntfft*sizeof(float),"allocating w");
	for(iw=0;iw<ntfft;iw++) {
		if(iw<ntfftny){
			w[iw]=iw*dw;
		}else{
			w[iw]=(iw-ntfft)*dw;
		}
		if(iw==0) w[0]=0.1*dw;  	/* fudge for dc component */
	}

/******************************************************************************/
	fprintf(fpl,"sutifowler: building function for stacking velocity analysis\n");
/******************************************************************************/
	dv=(vmax-vmin)/MAX((nv-1),1);
	for(iv=0;iv<nv;iv++) v[iv]=vmin+iv*dv;
	if(ichoose>=3){
	  	for(iv=0;iv<nvstack;iv++) {
			p2stack[iv]=1./(v[iv]*v[iv]);
			fprintf(fpl,"	    stacking velocity %ld %f\n",iv,v[iv]);
		}
	}else{
		if(nvstack<6) err("sutifowler: nvstack must be 6 or more");
		dp2 = 1./(vminstack*vminstack*(nvstack-5));
		for(iv=0;iv<nvstack;iv++) {
			p2stack[iv]=iv*dp2;
			if(iv>0) {
				factor=1./sqrt(p2stack[iv]);
				fprintf(fpl,"	    stacking velocity %ld %f\n",iv,factor);
			}else{
				fprintf(fpl,"	    stacking velocity %ld infinity\n",iv);
			}
		}		
	}

/******************************************************************************/
	fprintf(fpl,"sutifowler: Opening and zeroing large block matrix disk file\n");
	fprintf(fpl,"	    This can take a while, but all is fruitless if the \n");
	fprintf(fpl,"	    necessary disk space is not there...\n");
/******************************************************************************/
	N[0]=nxfft+2;
	N[1]=ntout*MAX(nv*neta,nvstack);
	fname=VNDtempname(file);
	vnd = VNDop(2,0,2,N,1,sizeof(float),fname,ndir,dir,1);
	VNDfree(fname,"main: freeing fname 1");
	fprintf(fpl,"sutifowler: large file RAM mem buf = %ld bytes\n",
		vnd->NumBytesMemBuf);
	fprintf(fpl,"sutifowler: large file disk area = %ld bytes\n",
		vnd->NumBytesPerBlock*vnd->NumBlocksPerPanel*vnd->NumPanels);


	if(getcvstacks) {
/******************************************************************************/
		fprintf(fpl,"sutifowler: reading input cvstacks\n");
/******************************************************************************/
		for(icmp=0;icmp<ncmps;icmp++) {
			key[0]=icmp;
			key[1]=0;
			for(iv=0;iv<nvstack;iv++) {
				VNDrw('w',0,vnd,1,key,0,
					(char *) tr.data,iv*ntout,1,ntout,
					1,"writing cvstacks to disk");
				if( !gettr(&tr) ) {
				    if(icmp==ncmps-1 && iv==nvstack-1 ) {
					/* all ok, read all the input data */
				    }else{
					err("sutifowler: error reading input cvstacks");
				    }
				}
			}
		}
		goto xffts;
	}
/******************************************************************************/
	fprintf(fpl,
	"sutifowler: beginning constant velocity stacks of the input cmp gathers\n");
/******************************************************************************/
	fname=VNDtempname(file);
	vnda = V2Dop(2,1000000,sizeof(float),fname,nt,mxfold);
	VNDfree(fname,"main: freeing fname 2");
	fprintf(fpl,"sutifowler: cmp gather RAM mem buf = %ld bytes\n",
		vnda->NumBytesMemBuf);

	icmp=0;
	noff=0;
	do {
	   if(tr.cdp!=oldcmp) {
		cvstack(vnda,vnd,icmp,noff,off,mute,lmute,
			nvstack,p2stack,dt,dtout);
		icmp++;
		if(icmp==ncmps) {
			fprintf(fpl,"sutifowler: more input cdps than ncdps parameter\n");
			fprintf(fpl,"	    Will only process ncdps gathers.\n");
			goto done_with_input;
			}
		oldcmp=tr.cdp;
		noff=0;
	   }
	   if(lbtaper>0 || lstaper>0) taper (lstaper,lbtaper,ncmps,icmp,nt,tr.data);
	   factor=scale;
	   for(it=0;it<nt;it++) tr.data[it]*=factor;
	   V2Dw0(vnda,noff,(char *)tr.data,1);
	   off[noff]=tr.offset;
 	   if(ichoose==1 || ichoose==2) {
 		mute[noff]=fmax*off[noff]*off[noff]*dp2;
 	   }else{
 		mute[noff]=0.;
 	   }
	   if(nonhyp) mute[noff]=MAX(mute[noff],2*off[noff]/vmin);
	   noff++;
	   if(noff>mxfold) err("tifowler: input cdp has more traces than mxfold");
	} while ( gettr(&tr) );
	cvstack(vnda,vnd,icmp,noff,off,mute,lmute,
		nvstack,p2stack,dt,dtout);
	icmp++;
done_with_input:
	ncmps=icmp;
	fprintf(fpl,"sutifowler: read and stacked %d cmp gathers\n",ncmps);
	VNDcl(vnda,1);
xffts:
	VNDflush(vnd);

	if(ichoose<5){
/******************************************************************************/
	    fprintf(fpl,"sutifowler: doing forward x -> k spatial fft's\n");
/******************************************************************************/
	    for(it=0;it<(ntout*nvstack);it++) {
		V2Dr0(vnd,it,ccrt,21);
		for(k=ncmps;k<nxfft+2;k++) rt[k]=0.;
		pfarc(1,nxfft,rt,crt);
		V2Dw0(vnd,it,ccrt,22);
	    }
	    VNDr2c(vnd);
	}

	if(ichoose<=3) {
	    fprintf(fpl,"sutifowler: looping over k\n");
	    if(TI && (ichoose==1 || ichoose==2)) { /* build ti vnmo(p) table */
		vndvnmo=ptabledmo(nv,v,etamin,deta,neta,d,vsvp,NP,dp,dp2,file);
		fprintf(fpl,"sutifowler: dmo index(p) RAM mem buf = %ld bytes\n",
			vndvnmo->NumBytesMemBuf);
	    }
	    if(TI && (ichoose==1 || ichoose==3)){ /* build ti vphase(p) table */
		vndvphase=ptablemig(nv,v,etamin,deta,neta,d,vsvp,NP,file);
		fprintf(fpl,"sutifowler: migration scaler(p) RAM mem buf = %ld bytes\n",
			vndvphase->NumBytesMemBuf);
	    }
	    if(ichoose==1 || ichoose==2){
	    	iv=MAX(nv*neta,nvstack);
		fname=VNDtempname(file);
	    	vndb = V2Dop(2,750000,sizeof(complex),
			fname,(long)ntfft,iv);
	    		fprintf(fpl,"sutifowler: (w,v) RAM mem buf = %ld bytes\n",
				vndb->NumBytesMemBuf);
		VNDfree(fname,"main: freeing fname 3");
	    }

/******************************************************************************/
	    for(k=0;k<nxfftny;k++){ 	/* loop over spatial wavenumbers */
/******************************************************************************/
		if(k==(20*(k/20))) {
			fprintf(fpl,"sutifowler: k index = %d out of %d\n",
				k,nxfftny);
		}
		ak=k*dk;
		key[0]=k;
		key[1]=0;
/******************************************************************************/
		if(ichoose==1 || ichoose==2) { /* do Fowler DMO */
/******************************************************************************/
			for(iv=0;iv<nvstack;iv++) {	/* loop over input velocities */
				VNDrw('r',0,vnd,1,key,0,ccrt,iv*ntout,1,ntout,
				31,"Fowler DMO t -> w fft read");
				for(it=ntout;it<ntfft;it++) crt[it]=czero;
				pfacc(-1,ntfft,crt);
				V2Dw0(vndb,iv,ccrt,32);
			}

			for(iw=0;iw<ntfft;iw++) {
				p=0.5*ak/fabs(w[iw]);
				if(TI) {	/* anisotropic TI*/
				    ip=p/dp;
				    if(ip<NP) {
					V2Dr0(vndvnmo,ip,(char *)rindex,40);
				    }else{
					for(iv=0;iv<(nv*neta);iv++) rindex[iv]=-1.;
				    }
				}else{			/* isotropic */
				    p2=p*p;
				    for(iv=0;iv<nv;iv++){
					v2=v[iv]*v[iv];
					rindex[iv]=(1-v2*p2)/(v2*dp2);
				    }
				}	
				V2Dr1(vndb,iw,ccrt,41);
				for(iv=0;iv<nvstack;iv++) ctemp[iv]=crt[iv];
				ints8c(nvstack,1.0,0.0,ctemp,czero,czero,nv*neta,rindex,crt);
				V2Dw1(vndb,iw,ccrt,42);
			}
			for(iv=0;iv<(nv*neta);iv++) {	/* loop over output vel */
				V2Dr0(vndb,iv,ccrt,51);
				pfacc(1,ntfft,crt);
				VNDrw('w',0,vnd,1,key,0,ccrt,iv*ntout,1,ntout,
				52,"Fowler DMO w -> t fft write");		
			}
		}
/******************************************************************************/
		if( ichoose==3 && neta>1 ) {  /* fix up disk order if only doing TI migrations */
/******************************************************************************/
			for(iv=0;iv<nv;iv++) {
				VNDrw('r',0,vnd,1,key,0,ccrt,iv*ntout,1,ntout,
				57,"option 3 fixup for multiple eta read");
				for(ieta=1;ieta<neta;ieta++) {
					VNDrw('w',0,vnd,1,key,0,ccrt,
					iv*ntout+ieta*nv*ntout,1,ntout,
					58,"option 3 fixup for multiple eta write");
				}
			}
		}
/******************************************************************************/
		if( (ichoose==1 || ichoose==3 ) ) { 	/* do Stolt migration */
/******************************************************************************/
			for(iv=0;iv<(nv*neta);iv++) {
				if(TI) {	/* anisotropic TI */
				    V2Dr0(vndvphase,iv,ccrt,50);
				    dpm=rt[0];
				    dw=2.*PI/(ntfft*dtout);
				    iwmin=0.5*ak/( (NP-3)*dpm*dw);
				    for(iw=iwmin+1;iw<ntfftny;iw++) {
					p=0.5*ak/fabs(w[iw]);
					rp=1.0+p/dpm;
					ip=rp;
					wgt=rp-ip;
					factor=wgt*rt[ip+1]+(1.-wgt)*rt[ip];
					rindex[iw]=w[iw]*factor;
					rindex[ntfft-iw]=w[ntfft-iw]*factor;
				    }
				    fw=-2.*PI/dtout;
				    rindex[0]=fw;
				    for(iw=1;iw<iwmin+1;iw++) {
					rindex[iw]=fw;
					rindex[ntfft-iw]=fw;
				    }
				}else{			/* isotropic */
					scale=0.5*v[iv]*ak;
				    	for(iw=0;iw<ntfft;iw++) {
					    if(fabs(w[iw])>scale) {
						factor=scale/w[iw];
						factor=sqrt(1+factor*factor);
						rindex[iw]=w[iw]*factor;
					    }else{
						rindex[iw]=-2.*PI/dtout;
					    }
					}
				}

				VNDrw('r',0,vnd,1,key,0,ccrt,iv*ntout,1,ntout,
					61,"Stolt t -> w fft read");
				for(it=1;it<ntout;it+=2){
					crt[it].r=-crt[it].r;
					crt[it].i=-crt[it].i;
				}
				for(it=ntout;it<ntffts;it++) crt[it]=czero;
				pfacc(1,ntffts,crt);
				dw=2.*PI/(ntffts*dtout);
				fw=-PI/dtout;
				ints8c(ntffts,dw,fw,crt,czero,czero,
					ntfft,rindex,ctemp);
				/* obliquity factor code */
 				for(iw=0;iw<ntfft;iw++){
 					factor=fabs(w[iw]/rindex[iw]);
 					crt[iw].r=factor*ctemp[iw].r;
 					crt[iw].i=factor*ctemp[iw].i;
 				}
				pfacc(-1,ntfft,crt);
				VNDrw('w',0,vnd,1,key,0,ccrt,iv*ntout,1,ntout,
					62,"Stolt w->t fft write");		
			}
		}

	    }
	    fprintf(fpl,"sutifowler: completed loop over wavenumbers\n");
	    if(ichoose==1 || ichoose==2) VNDcl(vndb,1);
	    if(TI && (ichoose==1 || ichoose==2)) VNDcl(vndvnmo,1);
	    if(TI && (ichoose==1 || ichoose==3)) VNDcl(vndvphase,1);
	}

	if(ichoose<5) {
/******************************************************************************/
	    fprintf(fpl,"sutifowler: doing inverse spatial fft's k->x\n");
/******************************************************************************/
	    for(it=0;it<(ntout*nv*neta);it++) {
		V2Dr0(vnd,it,ccrt,71);
		pfacr(-1,nxfft,crt,rt);
		V2Dw0(vnd,it,ccrt,72);
	    }
	    VNDc2r(vnd);
	}

/*****************************************************************/
	fprintf(fpl,"sutifowler: outputting results\n");
/******************************************************************/
	it=0;
	for(icmp=0;icmp<ncmps;icmp++) {
		key[0]=icmp;
		key[1]=0;
		for(ieta=0;ieta<neta;ieta++) {
			for(iv=0;iv<nv;iv++) {
				VNDrw('r',0,vnd,1,key,0,(char *)tr.data,
					iv*ntout+ieta*nv*ntout,1,ntout,82,
					"outputting all velocities for each cmp");
				tr.ns=ntout;
				tr.dt=1000000*dtout;
				tr.cdp=icmp;
				tr.tracf=iv;
				tr.offset=v[iv];
				tr.cdpt=iv;
				tr.sx=icmp*dx;
				tr.gx=icmp*dx;
				it++;
				tr.tracl=it;
				tr.tracr=it;
				tr.fldr=icmp/ngroup;
				tr.ep=10+tr.fldr*ngroup;
				tr.igc=ieta;
				tr.igi=100*(etamin+ieta*deta);
				tr.d1=dtout;
				tr.f1=0.;
				tr.d2=1.;
				tr.f2=0.;
				puttr(&tr);
			}
		}
	}

/* close files and return */
	VNDcl(vnd,1);
	VNDfree(crt,"main: freeing crt");
	VNDfree(ctemp,"main: freeing ctemp");
	VNDfree(v,"main: freeing v");
	VNDfree(p2stack,"main: freeing p2stack");
	VNDfree(mute,"main: freeing mute");
	VNDfree(off,"main: freeing off");
	VNDfree(rindex,"main: freeing rindex");
	VNDfree(w,"main: freeing w");
	if(VNDtotalmem()!=0) {
		fprintf(stderr,"total VND memory at end = %ld\n",
		VNDtotalmem());
	}
	return EXIT_SUCCESS;
}

static void cvstack(VND *vnda, VND *vnd, int icmp, int noff, float *off,
		float *mute, int lmute, int nv, float *p2,
		float dt, float dtout)
/************************************************************************
	compute constant velocity stacks for this cmp and
	load out to disk
*************************************************************************
vnda	VND file with input cmp gather
vnd	VND file for output constant velocity stacks
icmp	cmp number (zero based)
noff	number of offsets for this cmp
off[]	list of offsets for this cmp
mute[]	list of mute times for this cmp
lmute	length of mute taper
nv	number of velocities at which to compute stacks
p2[]	list of 1/(v*v) at which to compute stacks
dt	input sample rate
dtout 	output sample rate
***************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
***************************************************************************/
{
	int iv;
	int it;
	int itmute;
	long ioff;
	long nt;
	long ntout;
	long key[2];	
	float *rt;
	float *buf;
	float *ttn;
	float *qtn;
	float *fold;
	float *big;
	float factor;
	float t;
	char *ccrt;

	nt=vnda->N[0];
	ntout=vnd->N[1]/nv;
	rt=(float *)VNDemalloc(ntout*sizeof(float),"sutifowler:cvstack: rt");
	buf=(float *)VNDemalloc(MAX(nt,ntout)*sizeof(float),"sutifowler:cvstack: buf");
	ttn=(float *)VNDemalloc(ntout*sizeof(float),"sutifowler:cvstack: ttn");
	qtn=(float *)VNDemalloc(ntout*sizeof(float),"sutifowler:cvstack: qtn");
	fold=(float *)VNDemalloc(ntout*sizeof(float),"sutifowler:cvstack: fold");
	big=(float *)VNDemalloc(noff*sizeof(float),"sutifowler:cvstack: big");
	ccrt=(char *)rt;
	
	/* check for completely dead traces so can skip them */
	for(ioff=0;ioff<noff;ioff++) {
		big[ioff]=0.;
		V2Dr0(vnda,ioff,(char *)buf,3);
		for(it=0;it<nt;it++)
			big[ioff]=MAX( big[ioff], fabs(buf[it]) );	
	}

	for(iv=0;iv<nv;iv++) {
		for(it=0;it<ntout;it++) rt[it]=0.;
		for(it=0;it<ntout;it++) fold[it]=0.;
		for(ioff=0;ioff<noff;ioff++) {
		    if(big[ioff]>0.) {
			V2Dr0(vnda,ioff,(char *)buf,4);

			factor=off[ioff]*off[ioff]*p2[iv];
			itmute=mute[ioff]/dtout;
			
			for(it=itmute;it<ntout;it++) {
				t=it*dtout;
				ttn[it]=sqrt(t*t+factor);
				}

			/* do nmo via 8-point sinc interpolation */
			ints8r(nt,dt,0.,buf,0.0,0.0,
				ntout-itmute,&ttn[itmute],&qtn[itmute]);
			
			/* apply linear ramp to taper mute */
			for (it=itmute; it<(itmute+lmute) && it<ntout; ++it)
				qtn[it] *= (float)(it-itmute+1)/(float)lmute;

			/* sum NMO corrected trace to stacked trace  */
			for(it=itmute;it<ntout;it++) rt[it]+=qtn[it];

			/* count fold information */
			for(it=itmute;it<ntout;it++) fold[it]+=1.;
		    }
		}
		for(it=0;it<ntout;it++) {
			if(fold[it]==0.) fold[it]=1.;
		}
		for(it=0;it<ntout;it++) rt[it]/=fold[it]; 
		key[0]=icmp;
		key[1]=0;
		VNDrw('w',0,vnd,1,key,0,ccrt,iv*ntout,1,ntout,1,"cv stacking");
	}

	VNDfree(rt,"cvstack: freeing rt");
	VNDfree(buf,"cvstack: freeing buf");
	VNDfree(ttn,"cvstack: freeing ttn");
	VNDfree(qtn,"cvstack: freeing qtn");
	VNDfree(fold,"cvstack: freeing fold");
	VNDfree(big,"cvstack: freeing big");
	if(icmp==20*(icmp/20)){
		fprintf(fpl,
		"sutifowler: completed stacking velocity analysis for cmp %d\n",icmp);
	}
	return;
}
VND *ptabledmo(int nv, float *v, float etamin, float deta, int neta,
	float d, float vsvp, int np, float dp, float dp2, char *file)
/*************************************************************************
	returns a VND table where dimension 0 is along v axis
	and dimension 1 is along p axis providing 

	1./(vnmo(p)*vnmo(p)*dp2)

	which will be the desired index in the stacking velocity
	analysis table for a given output velocity index and p
**************************************************************************
int nv		number of input velocities
float v[]	array of input velocities
float etamin	Alkhalifah's minimum eta value
float deta	Alkhalifah's eta increment
int neta	number of eta values
float d		Thomsen's delta
float vsvp	vs/vp ratio
int np		number of output slownesses
float dp	output slowness increment
float dp2	increment in slowness squared used for stacking
		velocity analysis
char *file	root name for temporary file to be created
***************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
***************************************************************************/
{
	VND *vndvel;
	float a,b,dangle,angle,theta,dscale;
	float vel[5];
	int nangle,iangle,ip;
	long iv,newnv,newnp,ieta;
	float *pin;
	float *vin;
	float *pout;
	float *vout;
	float e;
	char *fname;

	newnv=nv*neta;
	newnp=np;
	fname=VNDtempname(file);
	dscale=1./sqrt(1+2*d);
	vndvel=V2Dop(2,125000,sizeof(float),
		fname,newnv,newnp);
	VNDfree(fname,"ptabledmo: freeing fname");

	nangle=10001;
	dangle = 89./(nangle-1);
	pin = (float *)VNDemalloc(nangle*sizeof(float),
			"sutifowler:ptable: allocating pin");
	vin = (float *)VNDemalloc(nangle*sizeof(float),
			"sutifowler:ptable: allocating vin");
	pout = (float *)VNDemalloc(np*sizeof(float),
			"sutifowler:ptable: allocating pout");
	vout = (float *)VNDemalloc(np*sizeof(float),
			"sutifowler:ptable: allocating vout");
	for(ip=0;ip<np;ip++) pout[ip]=ip*dp;
	for(ieta=0;ieta<neta;ieta++) {
	    e= (etamin + ieta*deta)*(1+2*d)+d;
	    for(iv=0;iv<nv;iv++) {
		a=v[iv]*dscale;	/* vertical p-wave phase velocity */
		b=vsvp*a;	/* vertical s-wave phase velocity */
		for(iangle=0;iangle<nangle;iangle++) {
			angle=iangle*dangle;
			theta=angle*PI/180.;
			vget(a,b,e,d,theta,vel);
			pin[iangle]=vel[4];
			vin[iangle]=vel[3];
		}
		intlin(nangle,pin,vin,vin[0],vin[nangle-1],np,pout,vout);
		for(ip=0;ip<np;ip++)
			vout[ip]=1./(vout[ip]*vout[ip]*dp2);
		V2Dw1(vndvel,iv+ieta*nv,(char *)vout,101);
	    }
	}
	VNDfree(pin,"ptabledmo: freeing pin");
	VNDfree(vin,"ptabledmo: freeing vin");
	VNDfree(pout,"ptabledmo: freeing pout");
	VNDfree(vout,"ptabledmo: freeing vout");
	return (vndvel);
}
VND *ptablemig(int nv, float *v, float etamin, float deta, int neta,
		float d, float vsvp, int np, char *file)
/***************************************************************************
	returns a VND table where dimension 0 is along k/(2*wmig) axis
	and dimension 1 is along v axis (note a=v/sqrt(1+2d)).

	The first element in dimension 0 will be dp, the mig slowness
	increment which corresponds to increments in tan(theta)/a. 
	Thereafter, the values will be

	factor = v/( a*cos(theta) )

	where p corresponds to (i-1)*dp and a is the vertical p-wave
	phase velocity.  A Stolt migration can use this factor to
	compute		

		wdmo = wmig*factor

	using k/(2*wmig) = tan(theta)/a to find the appropriate location
	in the table.
*****************************************************************************
int nv		number of input velocities
float v[]	array of input velocities
float etamin	Alkhalifah's minimum eta value
float deta	Alkhalifah's eta increment
int neta	number of eta values
float d	 Thomsen's delta
float vsvp      vs/vp ratio
int np		number of output slownesses
char *file	root name for temporary file to be created
***************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
***************************************************************************/
{
	VND *vndvel;
	float a,b,dangle,angle,theta,pmax,dp,dscale;
	float vel[5];
	int nangle,iangle,ip;
	long iv,newnv,newnp,ieta;
	float *pin;
	float *vin;
	float *pout;
	float *vout;
	float e;
	char *fname;

	newnv=nv*neta;
	newnp=np;
	fname=VNDtempname(file);
	vndvel=V2Dop(2,125000,sizeof(float),
		fname,newnp,newnv);
	VNDfree(fname,"pmigtable: freeing fname");

	dscale=1./sqrt(1+2*d);
	nangle=10001;
	dangle = 89./(nangle-1);
	pin = (float *)VNDemalloc(nangle*sizeof(float),
			"sutifowler:ptable: allocating pin");
	vin = (float *)VNDemalloc(nangle*sizeof(float),
			"sutifowler:ptable: allocating vin");
	pout = (float *)VNDemalloc(np*sizeof(float),
			"sutifowler:ptable: allocating pout");
	vout = (float *)VNDemalloc(np*sizeof(float),
			"sutifowler:ptable: allocating vout");
	for(ieta=0;ieta<neta;ieta++) {
	    e= (etamin + ieta*deta)*(1+2*d)+d;
	    for(iv=0;iv<nv;iv++) {
		a=v[iv]*dscale;	/* vertical p-wave phase velocity */
		b=vsvp*a;	/* vertical s-wave phase velocity */
		for(iangle=0;iangle<nangle;iangle++) {
			angle=iangle*dangle;
			theta=angle*PI/180.;
			vget(a,b,e,d,theta,vel);
			pin[iangle]=tan(theta)/a;
			vin[iangle]=vel[0]/( a*cos(theta) );
		}
		pmax=pin[nangle-1];
		dp=pmax/(np-2);
		vout[0]=dp;
		for(ip=0;ip<(np-1);ip++) pout[ip]=ip*dp;
		intlin(nangle,pin,vin,vin[0],vin[iangle-1],np-1,pout,&vout[1]);
		V2Dw0(vndvel,iv+ieta*nv,(char *)vout,101);
	    }
	}
	VNDfree(pin,"ptablemig: freeing pin");
	VNDfree(vin,"ptablemig: freeing vin");
	VNDfree(pout,"ptablemig: freeing pout");
	VNDfree(vout,"ptablemig: freeing vout");
	return (vndvel);
}

static void vget( float a, float b, float e, float d, float theta, float *vel)
/***************************************************************************

This routine returns phase and NMO velocity information given
an input angle and Thomsen's parameters for a TI medium.
****************************************************************************
input parameters:
----------------
a      = alpha = vertical p-wave phase velocity = vrms/sqrt(1+2*d)   
b      = beta = vertical shear wave phase velocity
e      = epsilon = anisotropy factor for horizontally propagating p waves 
d      = delta = 0.5*(e + ds/(1-(b*b)/(a*a)))      
theta  = angle in radians

returned parameters:
-------------------
vel[0] = p-wave phase velocity
vel[1] = first derivative of p-wave phase velocity with respect to theta
vel[2] = second derivative of p-wave phase velocity with respect to theta
vel[3] = NMO velocity
vel[4] = ray parameter p = sin(theta)/vphase
***************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
***************************************************************************/
{
	float p, psi, sint, cost, sin2t, cos2t, tant, eps;
	float vnmo,v,dv,d2v,gamma,dgamma,d2gamma,sqgamma;
	eps=1.0e-7;
	sint=sin(theta);
	cost=cos(theta);
	sin2t=sint*sint;
	cos2t=cost*cost;
	psi=1.-(b*b)/(a*a);
	gamma=0.25*psi*psi+(2*d-e)*psi*sin2t*cos2t+(psi+e)*e*sin2t*sin2t;
	dgamma=(2*d-e)*psi*2*sint*cost*cos2t +
		( 4*(psi+e)*e - 2*(2*d-e)*psi )*sint*sin2t*cost;
	d2gamma=(2*d-e)*psi*2*(cos2t*cos2t-3.*cos2t*sin2t) +
		( 4*(psi+e)*e - 2*(2*d-e)*psi )*(3.*cos2t*sin2t-sin2t*sin2t);	
	sqgamma=sqrt(gamma);
	v=a*sqrt(1.+e*sin2t-0.5*psi+sqgamma);
	dv=0.5*a*a*(2*e*sint*cost + 0.5*dgamma/sqgamma)/v;
	d2v=0.5*a*a*(2*e*(cos2t-sin2t)+0.5*d2gamma/sqgamma
		-0.25*dgamma/(sqgamma*gamma) )/v -
		0.5*a*a*dv*(2*e*sint*cost + 0.5*dgamma/sqgamma)/(v*v);
	if(cost<eps) cost=eps;
	tant=sint/cost;
	vnmo = ( v*sqrt( 1 + d2v/v) )/( cost * (1-tant*dv/v) );
	p=sint/v;
	vel[0]=v;
	vel[1]=dv;
	vel[2]=d2v;
	vel[3]=vnmo;
	vel[4]=p;
	return;
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
*****************************************************************************
Based on code by David Hale
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


