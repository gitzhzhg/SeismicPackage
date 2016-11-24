/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUINTERP: $Revision: 1.15 $ ; $Date: 2011/11/16 22:12:22 $                */

#include "su.h"
#include "segy.h"
#include "header.h"
#include "VND.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = { 
"									",
" SUINTERP - interpolate traces using automatic event picking		",
"									",
"           suinterp < stdin > stdout					",
"									",
" ninterp=1    number of traces to output between each pair of input traces",
" nxmax=500    maximum number of input traces				",
" freq1=4.     starting corner frequency of unaliased range		",
" freq2=20.    ending corner frequency of unaliased range		",
" deriv=0      =1 means take vertical derivative on pick section        ",
"              (useful if interpolating velocities instead of seismic)  ",
" linear=0     =0 means use 8 point sinc temporal interpolation         ",
"              =1 means use linear temporal interpolation               ",
"              (useful if interpolating velocities instead of seismic)  ",
" lent=5       number of time samples to smooth for dip estimate	",
" lenx=1       number of traces to smooth for dip estimate		",
" lagc=400     number of ms agc for dip estimate			",
" xopt=0       0 compute spatial derivative via FFT			",
"                 (assumes input traces regularly spaced and relatively	",
"                  noise-free)						",
"              1 compute spatial derivative via differences		",
"                 (will work on irregulary spaced data)			",
" iopt=0     0 = interpolate",
"            1 = output low-pass model: useful for QC if interpolator failing",
"            2 = output dip picks in units of samples/trace		",
"									",
" verbose=0	verbose = 1 echoes information				",
"									",
" tmpdir= 	 if non-empty, use the value as a directory path	",
"		 prefix for storing temporary files; else if the	",
"	         the CWP_TMPDIR environment variable is set use		",
"	         its value for the path; else use tmpfile()		",
" 									",
" Notes:								",
" This program outputs 'ninterp' interpolated traces between each pair of",
" input traces.  The values for lagc, freq1, and freq2 are only used for",
" event tracking. The output data will be full bandwidth with no agc.  The",
" default parameters typically will do a satisfactory job of interpolation",
" for dips up to about 12 ms/trace.  Using a larger value for freq2 causes",
" the algorithm to do a better job on the shallow dips, but to fail on the",
" steep dips.  Only one dip is assumed at each time sample between each pair",
" of input traces.							",
" 									",
" The key assumption used here is that the low frequency data are unaliased",
" and can be used for event tracking. Those dip picks are used to interpolate",
" the original full-bandwidth data, giving some measure of interpolation",
" at higher frequencies which otherwise would be aliased.  Using iopt equal",
" to 1 allows you to visually check whether the low-pass picking model is",
" aliased.								",
" 									",
" Trace headers for interpolated traces are not updated correctly.	",
" The output header for an interpolated traces equals that for the preceding",
" trace in the original input data.  The original input traces are passed",
" through this module without modification.				",
"									",
" The place this code is most likely to fail is on the first breaks.	",
"									",
" Example run:    suplane | suinterp | suxwigb &			",
"									",
NULL}; 

/*
 * Credit: John Anderson (visiting scholar from Mobil) July 1994
 *
 * Trace header fields accessed: ns, dt
 */

/**************** end self doc ********************************/

void runav(int n,int len,float *a,float *b);
void jea_xinterpolate(VND *vndorig, VND *vndinterp, int ninterp, 
		int nt, int nx, float freq1, float freq2, int lagc, 
		int lent, int lenx, int xopt, float dt, int iopt,
		int deriv, int linear);

static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *headerfp;		/* fp for header storage file		*/

segy tr;	/* Input and output trace data of length nt */

int
main (int argc, char **argv)
{
	int	nx,nt,ix,nxmax,ninterp,k;
	int	lent,lenx,lagc,xopt,iopt,deriv,linear;
	float	dt,freq1,freq2;
	VND	*vndorig,*vndinterp;
	char	*file;
	int verbose;	/* flag for echoing info			*/
	char *tmpdir;	/* directory path for tmp files			*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path		*/

	initargs(argc,argv);
	requestdoc(1);

	/* Get info from first trace */
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;

	if (!getparint("lagc",&lagc))lagc=400;
	if (!getparfloat("freq1",&freq1)) freq1=4.;
	if (!getparfloat("freq2",&freq2)) freq2=20.;
	if (!getparint("lent",&lent)) lent=5;
	if (!getparint("lenx",&lenx)) lenx=1;
	if (!getparint("nxmax",&nxmax)) nxmax=500;
	if (!getparint("xopt",&xopt)) xopt=0;
	if (!getparint("ninterp",&ninterp)) ninterp=1;
	if (!getparint("iopt",&iopt)) iopt=0;
	if (!getparint("deriv",&deriv)) deriv=0;
	if (!getparint("linear",&linear)) linear=0;
	if (!getparint("verbose", &verbose)) verbose = 0;

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

        checkpars();
	file=VNDtempname("suinterp");
	vndorig = V2Dop(2,1000000,sizeof(float),
			file,nt,nxmax);
	VNDfree(file,"file");
	file=VNDtempname("suinterp");
	vndinterp = V2Dop(2,1000000,sizeof(float),
			file,
			nt,1+(nxmax-1)*(ninterp+1));
	VNDfree(file,"file");

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

	/* Main loop for saving input traces */
	nx=0;
	do {
		V2Dw0(vndorig,nx,(char *)tr.data,1);
		efwrite(&tr,HDRBYTES,1,headerfp);
		nx++;
		if(nx>=nxmax) break;
	} while(gettr(&tr));

	jea_xinterpolate(vndorig,vndinterp,ninterp,nt,nx,freq1,freq2,
			lagc,lent,lenx,xopt,dt,iopt,deriv,linear);

	/* loop outputting results */
	if(iopt!=0) ninterp=0;
	efseeko(headerfp, (off_t) 0,SEEK_SET);
	for(ix=0;ix<nx-1;ix++) {
		efread(&tr,HDRBYTES,1,headerfp);
		for(k=0;k<=ninterp;k++) {
			V2Dr0(vndinterp,k+ix*(ninterp+1),(char *)tr.data,18);
			puttr(&tr);
		}
	} 
	efread(&tr,HDRBYTES,1,headerfp);
	V2Dr0(vndinterp,(nx-1)*(ninterp+1),(char *)tr.data,18);
	puttr(&tr);

	/* close files and return */
	VNDcl(vndorig,1);
	VNDcl(vndinterp,1);
	if(VNDtotalmem()!=0) {
		fprintf(stderr,"total VND memory at end = %ld\n",
		VNDtotalmem());
	}
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);

	return(CWP_Exit());
}

void jea_xinterpolate(VND *vndorig, VND *vndinterp, int ninterp, 
		int nt, int n2, float freq1, float freq2, int lagc, 
		int lent, int len2, int xopt, float dt, int iopt,
		int deriv, int linear)
/*******************************************************************
interpolate input data in space placing "ninterp" synthetic traces 
between each pair of original input traces
******************************************************************
Function parameters:

VND *vndorig		VND file with input data
VND *vndinterp		VND file with output original plus interpolated data
int ninterp		number of traces to interpolate between each
			input trace
int nt			number of time samples
int n2			number of input traces
float freq1		low-end frequency in Hz for picking
						(good default: 3 Hz)
float freq2		high-end frequency in Hz for picking
						(good default: 20 Hz)
int lagc		length of AGC operator for picking
						(good default: 400 ms)
int lent		length of time smoother in samples for picker
                        (good default: 5 samples)
int len2		length of space smoother in samples for picker
                        (good default: 1 sample)
int xopt		1 = use differences for spatial derivative
                            (works with irregular spacing)
                        0 = use FFT derivative for spatial derivatives
                            (more accurate but requires regular spacing and
                            at least 16 input tracs--will switch to differences
                            automatically if have less than 16 input traces)
float dt		sample rate in sec
int iopt		0 = interpolate: output 1+(n2-1)*(1+ninterp) traces
                            with ninterp traces between each pair of
			    input traces
			1 = compute low-pass model: output n2 traces
                            on original trace locations -- This is typically
                            used for Quality Control if the interpolator
                            is failing for any reason
			2 = compute dip picks in units of samples/trace: 

                            output n2 traces on original trace locations
int deriv		0 = default
			1 = take vertical derivative for picking section
int linear		0 = default use 8 point sinc interpolators
			1 = use linear interpolation

This routine outputs 'ninterp' interpolated traces between each pair of 
input traces.  The values for lagc, freq1, and freq2 are only used for
event tracking. The output data will be full bandwidth with no agc.  The 
suggested default parameters typically will do a satisfactory job of 
interpolation for dips up to about 12 ms/trace.  Using a larger value for 
freq2 causes the algorithm to do a better job on the shallow dips, but to 
fail on the steep dips.  Only one dip is assumed at each time sample between 
each pair of input traces.  The original input traces are passed through
this routine without modification.

The key assumption used here is that the low frequency data are unaliased
and can be used for event tracking.  Those dip picks are used to interpolate
the original full-bandwidth data, giving some measure of interpolation
at higher frequencies which otherwise would be aliased.  Using iopt equal
to 1 allows you to visually check whether the low-pass picking model
is aliased.
If you can't visually pick dips correctly on the low-pass picking 
model, this computer routine will fail.

The place this code is most likely to fail is on the first breaks.

This routine assumes that the input and output files hav been allocated in
the calling routine as

vndorig = V2Dop(2,1000000,sizeof(float),VNDtempname("suinterp"),nt,n2max);
vndinterp = V2Dop(2,1000000,sizeof(float),VNDtempname("suinterp"),
			nt,1+(n2max-1)*(ninterp+1));

where n2max is the maximum number of input traces and nt is the number 
of time samples.
*******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*******************************************************************/

{
	int	ntfft,ntfftny,n2fft,n2fftny,j,k,ixm;
	long	ix,it,ntlong,n2long;
	float	df,dff,wa,wb,dxx,eps=1.0e-30,f,fcl,fch;
	float 	*rt,*rrt,*a,*b,*p,*time,*aa,*bb,*save,*tin=NULL;
	complex	*crt,*ccrt;
	VND	*vnda,*vndb;
	char 	*file;

	ntlong=nt;
	n2long=n2;
	lent=1+2*(lent/2);
	len2=1+2*(len2/2);
	lagc=1 + lagc*0.001/dt;

	ntfft=npfar(nt);
	ntfftny=1+ntfft/2;
	n2fft=npfar(n2);
	n2fftny=1+n2fft/2;

	df=1./(ntfft*dt);

	crt = (complex *)VNDemalloc( MAX(ntfftny,n2fftny)*sizeof(complex),
		"jea_xinterpolate:allocating crt" );
	rt = (float *)crt;

	if(n2<2 || (iopt==0 && ninterp==0) ) {
	    	for(ix=0;ix<n2;ix++) {
			V2Dr0(vndorig,ix,(char *)rt,101);	
			V2Dw0(vndinterp,ix,(char *)rt,102);
		}
		free(crt);
		return;
	}

	ccrt = (complex *)VNDemalloc( ntfftny*sizeof(complex),
		"jea_xinterpolate:allocating ccrt" );
	rrt = (float *)ccrt;
	a =  (float *)VNDemalloc( MAX(n2,nt)*sizeof(float),
		"jea_xinterpolate:allocating a" );
	b =  (float *)VNDemalloc( MAX(n2,nt)*sizeof(float),
		"jea_xinterpolate:allocating b" );
	p =  (float *)VNDemalloc( nt*sizeof(float),
		"jea_xinterpolate:allocating p" );
	time =  (float *)VNDemalloc( nt*sizeof(float),
		"jea_xinterpolate:allocating time" );
	aa =  (float *)VNDemalloc( MAX(n2,nt)*sizeof(float),
		"jea_xinterpolate:allocating aa" );
	bb =  (float *)VNDemalloc( MAX(n2,nt)*sizeof(float),
		"jea_xinterpolate:allocating bb" );
	if(linear) {
		tin=(float *)VNDemalloc(nt*sizeof(float),
			"jea_xinterpolate: allocating tin");
		for(j=0;j<nt;j++) tin[j]=j;
	}

	file=VNDtempname("suinterp");
	vnda  = V2Dop(2,500000,sizeof(float),file,ntlong,n2long);
	VNDfree(file,"file");
	file=VNDtempname("suinterp");
	vndb  = V2Dop(2,500000,sizeof(float),file,ntlong,n2long);
	VNDfree(file,"file");

	/* loop computing filtered data for picking purposes in vnda */
	/* compute time derivative of filtered data in vndb */
	dff=2.*PI/ntfft;
	for(ix=0;ix<n2;ix++) {
		V2Dr0(vndorig,ix,(char *)rt,103);
		for(j=0;j<nt;j++) a[j]=fabs(rt[j]);
		runav(nt,lagc,a,b);
		runav(nt,lagc,b,a);
		for(j=0;j<nt;j++) rt[j]=rt[j]/(a[j]+eps);	
		for(j=nt;j<ntfft;j++) rt[j]=0.;
		if(deriv) {
			for(j=nt-1;j>0;j--){
				rt[j]=rt[j]-rt[j-1];
			}
			rt[0]=rt[1];
		}
		pfarc(1,ntfft,rt,crt);
		if(freq1>0.){
			for(j=0;j<ntfftny;j++){
				f=j*df;
				fcl=(f/freq1);
				fcl=fcl*fcl*fcl*fcl;
				fch=(f/freq2);
				fch=fch*fch*fch*fch;
				f=fcl/( (1.+fcl)*(1.+fch) );
				crt[j]=crmul(crt[j],f);
				ccrt[j]=cmul(crt[j],cmplx(0.,-j*dff));
			}
		}else{
			for(j=0;j<ntfftny;j++){
				f=j*df;
				fch=(f/freq2);
				f=1./(1.+fch*fch*fch*fch);
				crt[j]=crmul(crt[j],f);
				ccrt[j]=cmul(crt[j],cmplx(0.,-j*dff));
			}
		}
		pfacr(-1,ntfft,crt,rt); 
		V2Dw0(vnda,ix,(char *)rt,104);
		pfacr(-1,ntfft,ccrt,rrt); 
		V2Dw0(vndb,ix,(char *)rrt,105);
	} 

	if(iopt==1){
		for(ix=0;ix<n2;ix++){
			V2Dr0(vnda,ix,(char *)rt,104);
			V2Dw0(vndinterp,ix,(char *)rt,104);
		}
		VNDcl(vnda,1);
		VNDcl(vndb,1);
		VNDfree(crt,"jea_xinterpolate: crt");
		VNDfree(ccrt,"jea_xinterpolate: ccrt");
		VNDfree(a,"jea_xinterpolate: a");
		VNDfree(b,"jea_xinterpolate: b");
		VNDfree(p,"jea_xinterpolate: p");
		VNDfree(time,"jea_xinterpolate: time");
		VNDfree(aa,"jea_xinterpolate: aa");
		VNDfree(bb,"jea_xinterpolate: bb");
		if(linear) VNDfree(tin,"jea_xinterpolate: tin");
		return;
	}

	/* loop computing spatial derivative of data for picking purposes*/
	n2fft=npfar(n2);
	n2fftny=1+n2fft/2;
	dxx=2.*PI/(n2fft*n2fft);
	if(n2<16) xopt=1;
	for(it=0;it<nt;it++) {
		V2Dr1(vnda,it,(char *)rt,106);
		if(xopt) {
			for(j=0;j<n2-1;j++) rt[j]=rt[j+1]-rt[j];
			rt[n2-1]=rt[n2-2];
		}else{
			for(j=n2;j<n2fft;j++) rt[j]=0.;
			pfarc(1,n2fft,rt,crt);
			for(j=0;j<n2fftny;j++){
				crt[j]=cmul(crt[j],cmplx(0.,-j*dxx));
			}
			pfacr(-1,n2fft,crt,rt); 
		}
		V2Dw1(vnda,it,(char *)rt,107);
	} 

	/* compute dot products and smooth over time */
	for(ix=0;ix<n2;ix++) {
		V2Dr0(vnda,ix,(char *)a,108);
		V2Dr0(vndb,ix,(char *)b,109);
		for(it=0;it<nt;it++) {
			aa[it]=a[it]*b[it];
			bb[it]=b[it]*b[it];
		}
		runav(nt,lent,aa,a);
		runav(nt,lent,a,aa);
		runav(nt,lent,bb,b);
		runav(nt,lent,b,bb);
		V2Dw0(vnda,ix,(char *)aa,110);
		V2Dw0(vndb,ix,(char *)bb,111);
	}

	/* smooth dot products in x */
	if(len2>1){
	    for(it=0;it<nt;it++) {
		V2Dr1(vnda,it,(char *)a,112);
		V2Dr1(vndb,it,(char *)b,113);
		runav(n2,len2,a,aa);
		runav(n2,len2,aa,a);
		runav(n2,len2,b,bb);
		runav(n2,len2,bb,b);
		V2Dw1(vnda,it,(char *)a,114);
		V2Dw1(vndb,it,(char *)b,115);
	    }
	}

	/* loop computing p, interpolating, and outputting results */
	V2Dr0(vndorig,0,(char *)a,116);
	for(ix=1;ix<n2;ix++) {
		ixm=((int) (ix-1));
		V2Dr0(vnda,ixm,(char *)aa,117);
		V2Dr0(vndb,ixm,(char *)bb,118);
		for(it=0;it<nt;it++) {
			p[it] = - aa[it]/( bb[it] + eps );
		}
		V2Dr0(vndorig,ix,(char *)b,119);
		if(iopt==2) {
			V2Dw0(vndinterp,ixm,(char *)p,120);
			/* don't output dip picks except on original traces */
		}else{
			V2Dw0(vndinterp,ixm*(ninterp+1),(char *)a,120);
			for(k=0;k<ninterp;k++){
				wa=(1.+k)/(1+ninterp);
				wb=1.-wa;
				for(it=0;it<nt;it++) time[it] = ((float ) it) - p[it]*wa;
				if(linear){
					intlin(nt,tin,a,a[0],a[nt-1],nt,
					       time,aa);
				}else{		
					ints8r(nt,1.0,0.,a,0.0,0.0,nt,time,aa);
				}
				for(it=0;it<nt;it++) time[it] = ((float ) it) + p[it]*wb;
				if(linear){
					intlin(nt,tin,a,a[0],a[nt-1],nt,
					       time,bb);
				}else{		
					ints8r(nt,1.0,0.,b,0.0,0.0,nt,time,bb);
				}
				for(it=0;it<nt;it++)
					aa[it]=wb*aa[it]+wa*bb[it];
				V2Dw0(vndinterp,k+1+ixm*(ninterp+1),
				      (char *)aa,121);
			}
		}
		save=a;
		a=b;
		b=save;  
	} 
	if(iopt==2) {
		V2Dw0(vndinterp,n2-1,(char *)p,122);
	}else{
		V2Dw0(vndinterp,(n2-1)*(ninterp+1),(char *)a,122);
	}


/* close files, free temporary memory, and return results in file vndinterp */
	VNDcl(vnda,1);
	VNDcl(vndb,1);
	VNDfree(crt,"jea_xinterpolate: crt");
	VNDfree(ccrt,"jea_xinterpolate: ccrt");
	VNDfree(a,"jea_xinterpolate: a");
	VNDfree(b,"jea_xinterpolate: b");
	VNDfree(p,"jea_xinterpolate: p");
	VNDfree(time,"jea_xinterpolate: time");
	VNDfree(aa,"jea_xinterpolate: aa");
	VNDfree(bb,"jea_xinterpolate: bb");
	if(linear) VNDfree(tin,"jea_xinterpolate: tin");
	return;
}
void runav(int n,int len,float *a,float *b)
/*
compute a boxcar running average filter

int n   	number of samples in a[] and b[]
int len 	length of running average in samples
float a[n]	input array
float b[n]	output array
*******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*******************************************************************/
{
	float sum=0.;
	int j,lenh=len/2;
	if(len<=1) {
		for(j=0;j<n;j++) b[j]=a[j];
		return;
	}
	for(j=0;j<MIN(len,n);j++) sum+=a[j];
	for(j=0;j<MIN(lenh+1,n);j++) b[j]=sum;
	for(j=lenh+1;j<n-lenh;j++) {
		sum=sum+a[j+lenh]-a[j-lenh-1];
		b[j]=sum;
	}
	for(j=MAX(0,n-lenh);j<n;j++) b[j]=sum;
	sum=1./len;
	for(j=0;j<n;j++) b[j]*=sum;
	return;
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(headerfp);
	eremove(headerfile);
	exit(EXIT_FAILURE);
}
