/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SURADON: $Revision: 1.21 $ ; $Date: 2015/08/07 22:26:35 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include "VND.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SURADON - compute forward or reverse Radon transform or remove multiples",
"           by using the parabolic Radon transform to estimate multiples",
"           and subtract.						",
"									",
"     suradon <stdin >stdout [Optional Parameters]			",
"									",
" Optional Parameters:							",
" choose=0    0  Forward Radon transform				",
"             1  Compute data minus multiples				",
"             2  Compute estimate of multiples				",
"             3  Compute forward and reverse transform			",
"             4  Compute inverse Radon transform			",
" igopt=1     1  parabolic transform: g(x) = offset**2			",
"             2  Foster/Mosher psuedo hyperbolic transform		",
"                   g(x) = sqrt(depth**2 + offset**2)			",
"             3  Linear tau-p: g(x) = offset				",
"             4  abs linear tau-p: g(x) = abs(offset)			",
" offref=2000.    reference maximum offset to which maximum and minimum	",
"                 moveout times are associated				",
" interoff=0.     intercept offset to which tau-p times are associated	",
" pmin=-200       minimum moveout in ms on reference offset		",
" pmax=400        maximum moveout in ms on reference offset		",
" dp=16           moveout increment in ms on reference offset		",
" pmula=80        moveout in ms on reference offset where multiples begin",
"                     at maximum time					",
" pmulb=200       moveout in ms on reference offset where multiples begin",
"                     at zero time					",
" depthref=500.   Reference depth for Foster/Mosher hyperbolic transform",
" nwin=1          number of windows to use through the mute zone	",
" f1=60.          High-end frequency before taper off			",
" f2=80.          High-end frequency					",
" prewhite=0.1    Prewhitening factor in percent.			",
" cdpkey=cdp      name of header word for defining ensemble		",
" offkey=offset   name of header word with spatial information		",
" nxmax=240       maximum number of input traces per ensemble		",
" ltaper=7	  taper (integer) for mute tapering function		",
"									",
" Optimizing Parameters:						",
" The following parameters are occasionally used to avoid spatial aliasing",
" problems on the linear tau-p transform.  Not recommended for other	",
" transforms...								",
" ninterp=0      number of traces to interpolate between each input trace",
"                   prior to computing transform			",
" freq1=4.0      low-end frequency in Hz for picking (good default: 3 Hz)",
"                (Known bug: freq1 cannot be zero) ",
" freq2=20.0     high-end frequency in Hz for picking (good default: 20 Hz)",
" lagc=400       length of AGC operator for picking (good default: 400 ms)",
" lent=5         length of time smoother in samples for picker		",
"                     (good default: 5 samples)				",
" lenx=7         length of space smoother in samples for picker		",
"                     (good default: 1 sample)				",
" xopt=1         1 = use differences for spatial derivative		",
"                        (works with irregular spacing)			",
"                0 = use FFT derivative for spatial derivatives		",
"                      (more accurate but requires regular spacing and	",
"                      at least 16 input tracs--will switch to differences",
"                      automatically if have less than 16 input traces)	",
"									",
NULL};

/* Credits:
 *	CWP: John Anderson (visitor to CSM from Mobil) Spring 1993
 *
 * Multiple removal notes:
 *	Usually the input data are NMO corrected CMP gathers.  The
 *	first pass is to compute a parabolic Radon transform and
 * 	identify the multiples in the transform domain.  Then, the
 * 	module is run on all the data using "choose=1" to estimate
 * 	and subtract the multiples.  See the May, 1993 CWP Project
 *	Review for more extensive documentation.
 *
 * NWIN notes:
 *	The parabolic transform runs with higher resolution if the
 * 	mute zone is honored.  When "nwin" is specified larger than
 *   	one (say 6), then multiple windows are used through the mute
 * 	zone.  It is assumed in this case that the input data are
 * 	sorted by the offkey header item from small offset to large
 * 	offset.  This causes the code to run 6 times longer.  The
 *      mute time is taken from the "muts" header word.
 *      You may have to manually set this header field yourself, if
 *      it is not already set.
 *
 * References:
 * Anderson, J. E., 1993, Parabolic and linear 2-D, tau-p transforms
 *       using the generalized radon tranform, in May 11-14, 1993
 *       Project Review, Consortium Project on Seismic Inverse methods
 *       for Complex Structures, CWP-137, Center for Wave Phenomena
 *       internal report.
 * Other References cited in above paper:
 * Beylkin, G,.1987, The discrete Radon transform: IEEE Transactions
 *       of Acoustics, Speech, and Signal Processing, 35, 162-712.
 * Chapman, C.H.,1981, Generalized Radon transforms and slant stacks:
 *       Geophysical Journal of the Royal Astronomical Society, 66,
 *       445-453.
 * Foster, D. J. and Mosher, C. C., 1990, Multiple supression
 *       using curvilinear Radon transforms: SEG Expanded Abstracts 1990,
 *       1647-1650.
 * Foster, D. J. and Mosher, C. C., 1992, Suppression of multiples
 *       using the Radon transform: Geophysics, 57, No. 3, 386-395.
 * Gulunay, N., 1990, F-X domain least-squares Tau-P and Tau-Q: SEG
 *       Expanded Abstracts 1990, 1607-1610.
 * Hampson, D., 1986, Inverse velocity stacking for multiple elimination:
 *       J. Can. Soc. Expl. Geophs., 22, 44-55.
 * Hampson, D., 1987, The discrete Radon transform: a new tool for image
 *       enhancement and noise suppression: SEG Expanded Abstracts 1978,
 *       141-143.
 * Johnston, D.E., 1990, Which multiple suppression method should I use?
 *       SEG Expanded Abstracts 1990, 1750-1752.
 *
 * Trace header words accessed: ns, dt, cdpkey, offkey, muts
 */
/**************** end self doc ********************************/

static void forward_p_transform(VND *vnda,VND *vndb,int nx, int nt, float *g,
	float dt, int ntfft, int np, float pmin, float dp,
	float *mutetime, float *offset, int nk,float f1,
	float f2,float prewhite);

static void inverse_p_transform(VND *vnda,int nx, float *g,
	float dt, int ntfft, int np, float pmin, float dp, int ip1,
	float f1,float f2);
static void compute_r(float w, int nx, float *g, int np, float dp, complex *r);
static void compute_rhs(float w, int nx, float *g, complex *data, int np,
		float pmin, float dp, complex *rhs);
static int ctoep(int n, complex *r, complex *a, complex *b,
		complex *f, complex *wrk);
static int ctoephcg(int niter, int n, complex *r, complex *a, complex *b,
		complex *wrk1, complex *wrk2, complex *wrk3, complex *wrk4 );
static float rcdot(int n, complex *a, complex *b);
static void htmul(int n, complex *a, complex *x, complex *y);
static float freqweight(int j, float df, float f1, float f2);
static float gofx(int igopt, float offset, float intercept_off,
		  float refdepth);
static void taupmute(int ip,int ipa,int ipb,int nt, int ltap, float *rt);
static void jea_xinterpolate(VND *vndorig, VND *vndinterp, int ninterp,
		int nt, int nx, float freq1, float freq2, int lagc,
		int lent, int lenx, int xopt, float dt, int iopt);
static void runav(int n,int len,float *a,float *b);

segy tr;
segy tro;
int main(int argc, char **argv)
{
	char *cdpkey=NULL;	/* key denoting the ensemble */
	char *offkey=NULL;	/* key denoting trace labeling in an ensemble */
	char *headerfile=NULL;	/* temporary file containing trace headers */
	char *fname=NULL;
	int j;
	int it;
	int icount;
	int oldcmp;
	int icmp;
	int nxmax;
	int nx;
	int nxinterp;
	int nxout=0;
	int np;
	int ipa;
	int ipb;
	int ix;
	int ninterp;
	int k;
	int nt;
	int lent;
	int lenx;
	int lagc;
	int xopt=0;
	int ntfft;
	int nxm;
	int nmax;
	int choose;
	int nk;
	int igopt;
	int ltaper;
	int cdpindex;
	int offindex;
	int iend;
	int ieod;
	float offref;
	float depthref;
	float intercept_off;
	float pmin;
	float pmax;
	float pmula;
	float pmulb;
	float dp;
	float dt;
	float freq1;
	float freq2;
	float f1;
	float f2;
	float prewhite;
	float fac;
	float d;
	float *rt=NULL;
	float *xin=NULL;
	float *offset=NULL;
	float *mutetime=NULL;
	float *g=NULL;
	float *gg=NULL;
	float *trace=NULL;
	Value hdrwd;
	FILE *headerfp=NULL;
	VND *vndorig=NULL;
	VND *vndinterp=NULL;
	VND *vndresult=NULL;

	initargs(argc, argv);
	requestdoc(1);

	if (!getparint("xopt",&xopt))	xopt=1;
	if (!getparstring("cdpkey",&cdpkey)) cdpkey="cdp";
	if (!getparstring("offkey",&offkey)) offkey="offset";
	if (!getparint("choose",&choose)) choose=0;
	if (!getparint("ninterp",&ninterp)) ninterp=0;
	if (!getparint("nwin",&nk)) nk=1;
	if (!getparint("igopt",&igopt)) igopt=1;
	if (!getparint("nxmax",&nxmax)) nxmax=240;
	if (!getparint("ninterp",&ninterp)) ninterp=1;
	if (!getparint("lagc",&lagc))lagc=400;
	if (!getparint("lent",&lent)) lent=5;
	if (!getparint("lenx",&lenx)) lenx=7;
	if (!getparfloat("freq1",&freq1)) freq1=4.;
	if (!getparfloat("freq2",&freq2)) freq2=20.;
	if (!getparfloat("offref",&offref)) offref=2000.;
	if (!getparfloat("f1",&f1)) f1=60.;
	if (!getparfloat("f2",&f2)) f2=80.;
	if (!getparfloat("pmin",&pmin)) pmin=-200.;
	if (!getparfloat("pmax",&pmax)) pmax=400.;
	if (!getparfloat("dp",&dp)) dp=16.;
	if (!getparfloat("pmula",&pmula)) pmula=80.;
	if (!getparfloat("pmulb",&pmulb)) pmulb=200.;
	if (!getparfloat("depthref",&depthref)) depthref=500.;
	if (!getparfloat("interoff",&intercept_off)) intercept_off=0.;
	if (!getparfloat("prewhite",&prewhite)) prewhite=0.1;
	if (!getparint("ltaper",&ltaper)) ltaper=7;


        checkpars();

	cdpindex=getindex(cdpkey);
	offindex=getindex(offkey);
	if(nk<0) nk=1;
	fac=1000.*gofx(igopt,offref,intercept_off,depthref);
	pmin/=fac;
	pmax/=fac;
	dp/=fac;
	pmula/=fac;
	pmulb/=fac;
	ipa=( pmula -  pmin)/ dp;
	ipb=( pmulb -  pmin)/ dp;
	np=1+( pmax -  pmin)/ dp;
	if(np<1)err("Range of PMIN and PMAX invalid");
	if(choose==0) ipa=0;
	if(choose==3) ipa=0;
	if(choose==4) ipa=0;
	if(ipa<0) ipa=0;

	if(!gettr(&tr)) err("Can't get first trace \n");
	nt=tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	gethval(&tr,cdpindex,&hdrwd);
	oldcmp=hdrwd.i;

	ntfft=npfar(nt);
	nxinterp = (1+ ninterp)*(nxmax-1)+1;
	nmax=MAX(nxinterp, np);
	nmax=MAX(nmax, ntfft+4);

	offset   = (float *)VNDemalloc(nxmax*sizeof(float),
				       "suradon_main: offset");
	rt       = (float *)VNDemalloc(nmax*sizeof(float),
				       "suradon_main: rt");
	trace    = (float *)VNDemalloc(nt*sizeof(float),
				       "suradon_main: trace");
	xin      = (float *)VNDemalloc(nxmax*sizeof(float),
				       "suradon_main: xin");
	g        = (float *)VNDemalloc(nxmax*sizeof(float),
				       "suradon_main: g");
	gg       = (float *)VNDemalloc(nxinterp*sizeof(float),
				       "suradon_main: gg");
	mutetime = (float *)VNDemalloc(nxmax*sizeof(float),
				       "suradon_main: mutetime");

	headerfile=VNDtempname("radontmp");
	if( (headerfp = fopen(headerfile,"w+"))==NULL)
		err("couldn't open temp file for trace headers");
	fname = VNDtempname("radontmp");
	vndorig = V2Dop(2,1000000,sizeof(float),fname,nt, nxmax);
	VNDfree(fname,"suradon_main: fname 1");
	fname = VNDtempname("radontmp");
	vndinterp = V2Dop(2,2000000,sizeof(float),fname,nt, nxinterp);
	VNDfree(fname,"suradon_main: fname 2");
	fname = VNDtempname("radontmp");
	nmax=MAX( nxinterp, np);
	vndresult = V2Dop(2,1000000,sizeof(float),fname,ntfft+2, nmax);
	VNDfree(fname,"suradon_main: fname 3");

	icount=0;
	icmp=0;
	nx=0;
	iend=0;
	ieod=0;
	do {
		gethval(&tr,cdpindex,&hdrwd);
		if(hdrwd.i!=oldcmp || ieod) { /* process this ensemble */
		   icmp++;
		   nxm= nx-1;
		   if( nx>1) {
			nxinterp=1 + nxm*(ninterp+1);
			if( choose==4) { /* count number of original offsets */
				k=1;
				for(j=1;j<nx;j++) {
					if(fabs(offset[j]-offset[j-1])>0.001)k++;
				}
				np=nx;
				nx=k;
			{ /* Jonathan Raven's fix */
		            int i1 = vndorig->N[0];      /* No. of samples read */
            		    int i2 = vndresult->N[0];   /* No. of samples written */
           		    for(j=0;j< np;j++){
                                V2Dr0(vndorig,j,(char *)rt,1001);
                		for (it=i1; it<i2; it++) rt[it] = 0.0;
                		V2Dw0(vndresult,j,(char *)rt,1002);
            			}
			}


/*	for(j=0;j< np;j++){					
		V2Dr0(vndorig,j,(char *)rt,1001);
		V2Dw0(vndresult,j,(char *)rt,1002);
	}
*/
			}else{
				jea_xinterpolate( vndorig, vndinterp,
					 ninterp, nt, nx,
					 freq1, freq2, lagc,
					 lent, lenx, xopt, dt,0);
				d=1./(1.+ ninterp);
				for(j=0;j<nxinterp;j++) rt[j]=j*d;
				for(j=0;j<nx;j++) xin[j]=j;
				intlin(nx,xin,offset,offset[0],
					 offset[ nxm],nxinterp,rt,gg);
				for(j=0;j<nxinterp;j++)
					 gg[j]=gofx(igopt,gg[j],
						 intercept_off,depthref);
				forward_p_transform( vndinterp,
					 vndresult, nxinterp,nt,gg,dt,
					 ntfft,np,pmin,dp,mutetime,
					 offset,nk,f1,f2,prewhite);
				nxout= np;
			}
			if(choose>=1) {
				if( choose==1){
					/* do tau-p mute here */
					for(j=0;j< ipb;j++) {
						V2Dr0( vndresult,j,
						      (char *) rt,1003);
						taupmute(j,ipa,ipb,
							 ntfft,ltaper,rt);
						V2Dw0( vndresult,j,
						      (char *) rt,1004);
					}
				}
				inverse_p_transform( vndresult, nx,
					 g, dt, ntfft, np,
					 pmin, dp, ipa, f1, f2);
				if(choose==1){
				    for(ix=0;ix< nx;ix++) {
					V2Dr0( vndorig,ix,(char *)trace,1005);
					V2Dr0( vndresult,ix,(char *) rt,1006);
					for(it=0;it< nt;it++)
						 rt[it]=trace[it]- rt[it];
					V2Dw0( vndresult,ix,(char *) rt,1007);
				    }
				}
				nxout= nx;
			}
						VNDmemchk(rt,"rt 01 e");
			erewind(headerfp);
			for(ix=0;ix<nxout;ix++) {
				if(ix<nx) efread(&tro,HDRBYTES,1,headerfp);
				V2Dr0(vndresult,ix,(char *)rt,1008);
				for(j=0;j< nt;j++) tro.data[j]=rt[j];
				icount++;
				tro.tracl = icount;
				tro.tracr = ix+1;
				if( choose==0) {
					tro.f2=1000.*( pmin+ ix* dp)*
						gofx( igopt, offref,
						     intercept_off, depthref);
					tro.d2=1000.*dp*gofx( igopt, offref,
							     intercept_off,
							     depthref);
				}
				puttr(&tro);
			}
		   }else{  /* pass input data unmodified */
			erewind(headerfp);
			efread(&tro,HDRBYTES,1,headerfp);
			V2Dr0(vndorig,0,(char *)rt,1009);
			for(j=0;j< nt;j++) tro.data[j]=rt[j];
			icount++;
			tro.tracl = icount;
			tro.tracr = 1;
			puttr(&tro);
		   }
		   oldcmp=hdrwd.i;
		   erewind(headerfp);
		   nx=0;
		}
		/* save trace and header */
		efwrite(&tr,HDRBYTES,1,headerfp);
		gethval(&tr,offindex,&hdrwd);
		offset[nx]=hdrwd.i;
		g[nx]=gofx(igopt,offset[nx],intercept_off,depthref);
		mutetime[nx]=tr.muts;
		V2Dw0(vndorig,nx,(char *)tr.data,1010);
		nx++;
		if(ieod) {
			iend++;
		}else{
			if(gettr(&tr)) {
				iend=0;
			}else{
				iend=1;
				ieod=1;
			}
		}
	}while(iend<2);

/* free memory and temporary file space */
    	VNDfree(offset,"suradon_main: offset");
	VNDfree(rt,"suradon_main: rt");
	VNDfree(trace,"suradon_main: trace");
	VNDfree(xin,"suradon_main: xin");
	VNDfree(g,"suradon_main: g");
	VNDfree(gg,"suradon_main: gg");
	VNDfree(mutetime,"suradon_main: mutetime");
	fclose(headerfp);
	remove(headerfile);
	VNDfree(headerfile,"suradon_main: headerfile");
	VNDcl(vndorig,1);
	VNDcl(vndinterp,1);
	VNDcl(vndresult,1);
	if(VNDtotalmem()!=0) {
		fprintf(stderr,"Warning, not all of the VND memory \n");
		fprintf(stderr,"has been freed and checked for overruns\n");
		fprintf(stderr,"Total VND memory at end of job = %ld\n",
		VNDtotalmem());
	}
	return(CWP_Exit());
}

static void forward_p_transform(VND *vnda,VND *vndb,int nx, int nt, float *g,
	float dt, int ntfft, int np, float pmin, float dp,
	float *mutetime, float *offset, int nk,float f1, float f2,
	float prewhite)
/*******************************************************************
do forward generalized radon transform

******************************************************************
Function parameters:

VND *vnda	input data in time-space domain
VND *vndb	output data in tau-p domain
int nx		number of input traces
int nt		number of intput time samples
float *g	g[ix]=offset**2 for parabolic transform
float dt	sample rate in seconds
int ntfft	length of time fft and output tau-p data in samples
int np		number of generalized ray parameters
float pmin	minimum generalized ray parameter
float dp	generalized ray parameter increment
float *mutetime array of mute times in ms
float *offset   array of offsets (ignored)
int nk          number of offset ranges to transform separately
		through the mute zone
float f1        max freq without taper
float f2        max non-zero freq component
prewhite	0.01 means prewhiten 1 percent

key assumption: offsets are sorted to increase with index
*******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*******************************************************************/
{
	int ix,ip,it,iw,j,ntfftny,k,nxx,nxxinc,ik,ik2;
	size_t nmax;
	float *rt=NULL,*rrt=NULL,*kindex=NULL,*tindex=NULL,w,dw,rsum,fac,wa,wb,rk[2],rit[2],df;
	complex czero,*crt=NULL,*ccrt=NULL,*r=NULL,*rhs=NULL,*wrk1=NULL,*wrk2=NULL,*wrk3=NULL,*wrk4=NULL;
	VND *vndc=NULL;
	char *fname=NULL;

	fac=1./ntfft;
	ntfftny=1+ntfft/2;
	df=1./(ntfft*dt);
	dw=2.*PI*df;
	nmax=MAX(vndb->N[0],vndb->N[1]);
	nxxinc=1+(nx-1)/nk;
	czero.r=czero.i=0.;

	if(nk>1) {
/* allocate file space and build a set of (mute time, group index) pairs */
		fname=VNDtempname("radontmp");
		vndc = V2Dop(2,1000000,sizeof(complex),fname,
				ntfftny,np*nk);
		VNDfree(fname,"forward_p_transform: fname");
		kindex = offset; /* dummy assignment */
		kindex=(float *)VNDemalloc(nk*sizeof(float),
			"forward_p_transform:kindex");
		tindex=(float *)VNDemalloc(nk*sizeof(float),
			"forward_p_transform:tindex");
		for(k=0;k<nk;k++) {
			kindex[k]=k;
			nxx=MIN(nx,(k+0.75)*nxxinc);
			tindex[k]=0.001*mutetime[nxx]/dt;
		}
	}else{
		vndc = vndb;
	}

	crt=(complex *)VNDemalloc(nmax*sizeof(complex),
		"forward_transform:crt");
	rt=(float *)crt;
	ccrt=(complex *)VNDemalloc(MAX((nk+1)*np,vndb->N[1])*sizeof(complex),
		"forward_transform:ccrt");
	rrt=(float *)ccrt;
	r=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:r");
	rhs=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:rhs");
	wrk1=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:wrk1");
	wrk2=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:wrk2");
	wrk3=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:wrk3");
	wrk4=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:wrk4");


/* do forward time to frequency fft */
	for(ix=0;ix<nx;ix++) {
		V2Dr0(vnda,ix,(char *)rt,201);
		for(it=0;it<nt;it++) rt[it]*=fac;
		for(j=nt;j<ntfft;j++) rt[j]=0.;
		pfarc(1,ntfft,rt,crt);
		V2Dw0(vndb,ix,(char *)crt,202);
	}
	VNDr2c(vndb);

/* do radon transform, frequency by frequency, for multiple spatial windows */
	for(iw=0;iw<ntfftny;iw++) {
		wa=freqweight(iw,df,f1,f2);
		if(wa>0.) {
		    w=iw*dw;
		    V2Dr1(vndb,iw,(char *)crt,203);
		    if(wa<1.) {
		    	for(ix=0;ix<nx;ix++) crt[ix]=crmul(crt[ix],wa);
		    }
		    for(k=0;k<nk;k++) {
			nxx=MIN(nx,(k+1)*nxxinc);
			compute_rhs(w,nxx,g,crt,np,pmin,dp,rhs);
			compute_r(w,nxx,g,np,dp,r);
			r[0].r *= (1.+prewhite);
			for(rsum=0.,j=1;j<np;j++)
				rsum +=sqrt(r[j].r*r[j].r + r[j].i*r[j].i);
			rsum=rsum/r[0].r;
			if (rsum>1.+np/5) {
				j=ctoephcg(np/7,np,r,&ccrt[k*np],rhs,
					wrk1,wrk2,wrk3,wrk4);
			}else{
				j=ctoep(np,r,&ccrt[k*np],rhs,wrk1,wrk2);
			}
		    }
		}else{
		    for(ip=0;ip<np*nk;ip++) ccrt[ip]=czero;
		}
		V2Dw1(vndc,iw,(char *)ccrt,204);
	}

/* do fourier transform from frequency to tau */
	for(ip=0;ip<np*nk;ip++) {
		V2Dr0(vndc,ip,(char *)crt,205);
		pfacr(-1,ntfft,crt,rt);
		V2Dw0(vndc,ip,(char *)rt,206);
	}
	VNDc2r(vndc);

/* merge appropriate tau-p transform for each window using mute zone
   information */
	if(nk>1) {
		VNDc2r(vndb);
		for(it=0;it<ntfft;it++) {
			rit[0]=it;
			intlin(nk,tindex,kindex,kindex[0],kindex[nk-1],1,rit,rk);
			ik=rk[0];
			ik2=MIN(ik+1,nk-1);
			wb=rk[0]-ik;
			wa=1-wb;
			V2Dr1(vndc,it,(char *)rrt,207);
			for(ip=0;ip<np;ip++) {
				rt[ip]=wa*rrt[ik*np+ip]+ wb*rrt[ik2*np+ip];
			}
			V2Dw1(vndb,it,(char *)rt,208);
		}
		VNDcl(vndc,1);
		VNDfree(tindex,"forward_p_transform: tindex");
		VNDfree(kindex,"forward_p_transform: kindex");
	}
	VNDfree(crt,"forward_p_transform: crt");
	VNDfree(ccrt,"forward_p_transform: ccrt");
	VNDfree(r,"forward_p_transform: r");
	VNDfree(rhs,"forward_p_transform: rhs");
	VNDfree(wrk1,"forward_p_transform: wrk1");
	VNDfree(wrk2,"forward_p_transform: wrk2");
	VNDfree(wrk3,"forward_p_transform: wrk3");
	VNDfree(wrk4,"forward_p_transform: wrk4");
	return;
}
static float gofx(int igopt, float offset, float intercept_off,float refdepth)
/*******************************************************************
return g(x) for various options
******************************************************************
Function parameters:

int igopt		1 = parabolic transform
			2 = Foster/Mosher pseudo hyperbolic option
			3 = linear tau-p
			4 = linear tau-p using absolute value of
				offset
float offset		offset in m
float intercept_off	offset corresponding to intercept time
float refdepth		reference depth in m for igopt=2
*******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*******************************************************************/
{
	offset=offset-intercept_off;
	if(igopt==1) {
		return(offset*offset);
	}
	if(igopt==2) {
		return( sqrt(refdepth*refdepth + offset*offset) );
	}
	if(igopt==3) {
		return(offset);
	}
	if(igopt==4) {
		return(fabs(offset));
	}
	return(offset);
}
static float freqweight(int j, float df, float f1, float f2)
/*******************************************************************
return weight for each frequency
******************************************************************
Function parameters:

int j		freq index
float df	freq increment
float f1	taper off freq
float f2	freq beyond which all components are zero
*******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*******************************************************************/
{
	float w;
	float f=j*df;
	if(f<=f1) return (1.);
	if(f>=f2) return (0.);
	w = (f2-f)/(f2-f1);
	return (w);
}

static void taupmute(int ip,int ipa,int ipb,int nt, int ltap, float *rt)
/*******************************************************************
do simple tau-p mute to elliminate multiples
******************************************************************
Function parameters:

int ip		current ray parameter index
int ipa		max ray parameter primary  pick at maximum time
int ipb		max ray parmater primary pick at minimum time
int nt		number of time samples
int ltap	length of mute taper in samples
float rt[nt]	tau-p data for all tau values

*******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*******************************************************************/

{
	int j,k;
	float w;
	if(ip>=ipb) return;
	if(ip<=ipa) {
		for(k=0;k<nt;k++) rt[k]=0;
		return;
	}
	w=MAX(ipb-ipa,1);
	w=(ipb-ip)/w;
	j=w*nt;
	for(k=0;k<j;k++) rt[k]=0.;
	for(k=j;k<MIN(nt,j+ltap);k++) rt[k]*=(k-j)/ltap;
}

static void inverse_p_transform(VND *vnda,int nx, float *g, float dt,
	int ntfft, int np, float pmin, float dp, int ip1,
	float f1, float f2)
/*******************************************************************
do inverse generalized radon transform

******************************************************************
Function parameters:

VND *vnda	output data in tau-p domain and data in time-space domain
int nx		number of output traces
int nt		number of output time samples
float *g	g[ix]=offset**2 for parabolic transform
float dt	sample rate in seconds
int ntfft	length of time fft and input tau-p data in samples
int np		number of generalized ray parameters
float pmin	minimum generalized ray parameter
float dp	generalized ray parameter increment
int ip1		starting generalized ray parameter index to compute
		(use as 0 for full inversion, positive integer to
		just invert multiples)
float f1        max freq without taper
float f2        max non-zero freq component
*******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*******************************************************************/
{
	int ip,iw,ntfftny,ix,it;
	size_t nmax;
	float w,dw,p,rsum,isum,dr,di,tr,ti,fac,wa,df;
	float *rt=NULL;
	complex *crt=NULL, *ctemp=NULL,czero;

	ntfftny=1+ntfft/2;
	df=1./(ntfft*dt);
	dw=2.*PI*df;
	czero.r=czero.i=0.;

	nmax=MAX(vnda->N[0],2*vnda->N[1])*vnda->NumBytesPerNode;
	nmax=MAX(nmax,nx*sizeof(complex));

	crt=(complex *)VNDemalloc(nmax,
		"inverse_p_transform:crt");
	rt=(float *)crt;
	ctemp=(complex *)VNDemalloc(np*sizeof(complex),
		"inverse_p_transform:ctemp");

	fac=1./ntfft;
	ntfftny=ntfft/2+1;
	for(ip=0;ip<np;ip++) {
		V2Dr0(vnda,ip,(char *)rt,301);
		for(it=0;it<ntfft;it++) rt[it]*=fac;
		pfarc(1,ntfft,rt,crt);
		V2Dw0(vnda,ip,(char *)crt,302);
	}
	VNDr2c(vnda);

	fac=1./np;
	for(iw=0;iw<ntfftny;iw++) {
		wa=freqweight(iw,df,f1,f2);
		if(wa>0.) {
			w=iw*dw;
			V2Dr1(vnda,iw,(char *)crt,303);
			if(wa<1.) {
				for(ip=0;ip<np;ip++) crt[ip]=crmul(crt[ip],wa);
			}
			for(ip=0;ip<np;ip++) ctemp[ip]=crt[ip];
			for(ix=0;ix<nx;ix++) {
			    rsum = isum = 0.;
			    for(ip=ip1;ip<np;ip++) {
				p = pmin + ip*dp;
				tr = cos(w*p*g[ix]);
				ti = sin(w*p*g[ix]);
				dr = ctemp[ip].r;
				di = ctemp[ip].i;
				rsum += tr*dr - ti*di;
				isum += tr*di + ti*dr;
			    }
			    crt[ix].r   = fac*rsum;
			    crt[ix].i   = fac*isum;
			}
		}else{
			for(ix=0;ix<nx;ix++) crt[ix]=czero;
		}
		V2Dw1(vnda,iw,(char *)crt,304);
	}
	for(ix=0;ix<nx;ix++) {
		V2Dr0(vnda,ix,(char *)crt,305);
		pfacr(-1,ntfft,crt,rt);
		V2Dw0(vnda,ix,(char *)rt,306);
	}
	VNDc2r(vnda);
	VNDfree(crt,"inverse_p_transform: crt");
	VNDfree(ctemp,"inverse_p_transform: ctemp");
	return;
}

static void compute_r( float w, int nx, float *g, int np, float dp, complex *r)
/*******************************************************************
Compute the top row of the Hermitian Toeplitz Matrix
			+
		  R = B B

		  i w p g(x)
where B = (1/np) e	    for equal increments in p as

     +           -i w p g(x)
and B = (1/nx) e

as used for the Discrete Radon Transform computation for
linear or parabolic tau-p.


		 nx-1	i w j dp g(x )
r[j] = 1/(nx*np) Sum	e	    k
		 k=0
						  2
g(x ) is initialized to  x  for linear tau-p or x   for the parabolic transform
   k		          k		         k
prior to calling this routine.  The use of g is intended to emphasize that the
spatial locations do not have to be equally spaced for either method.
In general, this routine can be called for g specified as any function
of spatial position only.  For a more general function of x, dp will
not correspond to an increment in slowness or slowness squared but
rather to a more general parameter.

******************************************************************
Function parameters:

float w	input as angular frequency component of interest
int   nx      number of spatial positions stored in g
float g[]     spatial function for this Radon Transform
int   np      number of slowness (or slowness squared) components
float dp      increment in slownes (or slowness squared)
float r[]     output vector of { real r0, imaginary r0, real r1,
	      imaginary r1, ...}
******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
******************************************************************/
{
	int j,k;
	float rsum, isum, fac;
	fac= 1./(nx*np);
	for(j=0;j<np;j++) {
		rsum=0.;
		isum=0.;
		for(k=0;k<nx;k++) {
			rsum = rsum+cos( w*j*dp*g[k] );
			isum = isum+sin( w*j*dp*g[k] );
		}
		r[j].r    = fac*rsum;
		r[j].i    = fac*isum;
	}
}

static void compute_rhs( float w, int nx, float *g, complex *data, int np,
		float pmin, float dp, complex *rhs)
/*********************************************************************
				     +
Compute the right-hand-side vector  B  data(x)

	+	    -i w p g(x)
where B   = (1/nx) e	        for equal increments in p as
used for the Discrete Radon Transform computation for
linear or parabolic tau-p.

Function parameters:

float w	input angular frequency of interest
int   nx	number of spatial positions ( defines length of g and data )
float g[]      spatial function corresponding to spatial locations of data
complex data[] data as a function of spatial position for a single
		angular frequency w as complex values
int   np	number of output slownesses p (may be slowness squared
		or a more general function)
float pmin     starting value of output p
float dp	increment in output p
complex rhs[]  np complex values for the result
*********************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*********************************************************************/

{
	int ip, ix;
	float p, rsum, isum, dr, di, tr, ti, fac;
	fac=1./nx;
	for(ip=0;ip<np;ip++) {
		p = pmin + ip*dp;
		rsum = isum = 0.;
		for(ix=0;ix<nx;ix++) {
			tr = cos(w*p*g[ix]);
			ti = -sin(w*p*g[ix]);
			dr = data[ix].r;
			di = data[ix].i;
			rsum += tr*dr - ti*di;
			isum += tr*di + ti*dr;
		}
		rhs[ip].r   = fac*rsum;
		rhs[ip].i   = fac*isum;
	}
}

static int ctoep( int n, complex *r, complex *a, complex *b,
		 complex *f, complex *g )
/***********************************************************************
Complex Hermitian Toeplitz Solver for

N-1
Sum  R	     A  = B      for i=0,1,2,...,N-1
j=0   (i-j)   j    i

where R is Hermitian Toeplitz and A and B are complex.  For
an example 4 x 4 system,  A returns as the solution of


   R0  R1  R2  R3	A0	     B0

     *
   R1  R0  R1  R2	A1	     B1
				=
     *   *
   R2  R1  R0  R1	A2	     B2

     *   *   *
   R3  R2  R1  R0	A3	     B3

and


   R0  R1  R2  R3	F0	     1

     *
   R1  R0  R1  R2	F1	     0
				=
     *   *
   R2  R1  R0  R1	F2	     0

     *   *   *
   R3  R2  R1  R0	F3	     0


***********************************************************************
where the function parameters are defined by

n     dimension of system
*r    provides the top row of the Hermitian Toeplitz matrix R
*a    returns the complex solution vector A
*b    input as complex vector B (not changed during call)
*f    returns the complex spiking filter F
      (may be needed later for Simpson's sideways recursion
      if do search for optimum filter lag)
*g    work space of length n complex values

The function value returns as the number of successfully
computed complex filter coefficients (up to n) if successful or
0 if no coefficients could be computed.
***********************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
***********************************************************************/

{
	float er, ei, vr, vi, cr, ci, vsq;
	int j;	  	/*  for the jth iteration, j=0,n-1 	*/
	int k;			/*  for the kth component, k=0,j-1 	*/
	int jmk;		/*  j-k 				*/
	if (r[0].r==0.) return 0;

	f[0].r = 1.0/r[0].r;
	f[0].i = 0.;
	a[0].r = b[0].r/r[0].r;
	a[0].i = b[0].i/r[0].r;
	vr=1.;
	vi=0.;
	vsq=1.;

	for(j=1;j<n;j++) {     	/* iteration loop for iteration j	*/
	/*  	Compute spiking filter that outputs {v,0,0,...}
		for this iteration step j			*/
		f[j].r=0.;
		f[j].i=0.;
		er=ei=0.;
		for(k=0;k<j;k++) {
			jmk=j-k;
			er+=r[jmk].r*f[k].r+r[jmk].i*f[k].i;
			ei+=r[jmk].r*f[k].i-r[jmk].i*f[k].r;
		}
		cr  = (er*vr - ei*vi)/vsq;
		ci  = (er*vi + ei*vr)/vsq;
		vr  = vr - (cr*er+ci*ei);
		vi  = vi + (cr*ei-ci*er);
		vsq =  vr*vr + vi*vi;
		if (vsq <= 0.) break;
		for(k=0;k<=j;k++) {
			jmk=j-k;
			g[k].r = f[k].r - cr*f[jmk].r - ci*f[jmk].i;
			g[k].i = f[k].i + cr*f[jmk].i - ci*f[jmk].r;
		}
		for(k=0;k<=j;k++) {
			f[k]=g[k];
		}

		/*  Compute shaping filter for this iteration */
		a[j].r=0.;
		a[j].i=0.;
		er=ei=0.;
		for(k=0;k<j;k++) {
			jmk=j-k;
			er+=r[jmk].r*a[k].r+r[jmk].i*a[k].i;
			ei+=r[jmk].r*a[k].i-r[jmk].i*a[k].r;
		}
		er  = er-b[j].r;
		ei  = ei-b[j].i;
		cr  = (er*vr - ei*vi)/vsq;
		ci  = (er*vi + ei*vr)/vsq;
		for(k=0;k<=j;k++) {
			jmk=j-k;
			a[k].r += - cr*f[jmk].r - ci*f[jmk].i;
			a[k].i += + cr*f[jmk].i - ci*f[jmk].r;
		}
	}

	/* Properly normalize the spiking filter so that R F = {1,0,0,...} */
	/* instead of {v,0,0,...}.  To be accurate, recompute vr,vi,vsq */
	vr=vi=0.;
	for(k=0;k<j;k++) {
		vr+=r[k].r*f[k].r-r[k].i*f[k].i;
		vi+=r[k].r*f[k].i+r[k].i*f[k].r;
	}

	vsq = vr*vr + vi*vi;

	/*  Compute (er,ei) = 1./(vr,vi)   */
	er = vr/vsq;
	ei = -vi/vsq;
	for(k=0;k<j;k++) {
		f[k].r = er*f[k].r - ei*f[k].i;
		f[k].i = er*f[k].i + ei*f[k].r;
	}
	return (j);
}

static int ctoephcg( int niter, int n, complex *a, complex *x, complex *y,
	complex *s, complex *ss, complex *g, complex *rr)

/*********************************************************************

Hestenes and Stiefel conjugate gradient algorithm
specialized for solving Hermitian Toeplitz
system.  a[] is input as a vector defining the only the
top row of A.  x[] is the solution vector returned.
y[] is input.  niter is the maximum number of conjugate
gradient steps to compute.  The function returns as
the number of steps actually computed.  The other
vectors provide workspace.

Complex Hermitian Toeplitz Solver for

N-1
Sum  A	     x  = y      for i=0,1,2,...,N-1
j=0   (i-j)   j    i

where A is Hermitian Toeplitz and x and y are complex.  For
an example 4 x 4 system,  x returns as the solution of


   A0  A1  A2  A3	x0	     y0

     *
   A1  A0  A1  A2	x1	     y1
				=
     *   *
   A2  A1  A0  A1	x2	     y2

     *   *   *
   A3  A2  A1  A0	x3	     y3

********************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*********************************************************************/
{
	int j, iter;
	complex czero;
	float alpha, beta, gamma, gammam, rsq, rp, test;
	float eps=1.0e-6;
	/* fix for */
/* suradon.c:1135: warning: 'rcdot' defined but not used */
/* 	float rcdot(int n, complex *a, complex *b); */
	rp   = rcdot(n,y,y);
	test = n*eps*eps*rp;
	czero.r=czero.i=0.;

	for(j=0;j<n;j++) {
		x[j]=czero;
		rr[j]=y[j];
	}
	htmul(n,a,rr,g);	   /*  adjoint matrix multiply */

	for(j=0;j<n;j++) s[j]=g[j];
	gammam=rcdot(n,g,g);

	for(iter=0;iter<niter;iter++) { /* forward matrix multiply  */
		htmul(n,a,s,ss);
		alpha  = gammam/rcdot(n,ss,ss);
		for(j=0;j<n;j++) {
			x[j] =cadd(x[j],crmul(s[j],alpha));
			rr[j]=csub(rr[j],crmul(ss[j],alpha));
		}
		rsq = rcdot(n,rr,rr);
		if ( iter>0 && ( rsq==rp || rsq<test ) ) return(iter-1);
		rp = rsq;

		htmul(n,a,rr,g);   /*  adjoint matrix multiply  */
		gamma  = rcdot(n,g,g);
		if (gamma<eps) break;
		beta   = gamma/gammam;
		gammam = gamma;

		for(j=0;j<n;j++) {
			s[j] =cadd(g[j],crmul(s[j],beta));
		}
	}
return(iter);
}

/* suradon.c:1135: warning: 'rcdot' defined but not used */
static float rcdot(int n, complex *a, complex *b)
/********************************************************************
return the real part of a complex dot product where
    the first vector is the one complex conjugated
*********************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*********************************************************************/
{
	int j;
	float sum=0.;
	for(j=0;j<n;j++) sum += a[j].r * b[j].r + a[j].i * b[j].i;
	return(sum);
}

static void htmul(int n, complex *a, complex *x, complex *y)

/*******************************************************************
   Hermitian Toeplitz matrix multiply

     solve for y = A x   where A is Hermitian Toeplitz

     and defined by the vector a giving the top row of A.
     x is input.  y is output.
*******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*******************************************************************/
{
	int j,irow;
	complex czero;
	czero.r=czero.i=0.;

	for(irow=0;irow<n;irow++) {
		y[irow]=czero;
		for(j=0;j<irow;j++)
			y[irow] = cadd(cmul(conjg(a[irow-j]),x[j]),y[irow]);
		for(j=irow;j<n;j++)
			y[irow] = cadd(cmul(a[j-irow],x[j]),y[irow]);
	}
}

static void jea_xinterpolate(VND *vndorig, VND *vndinterp, int ninterp,
		int nt, int nx, float freq1, float freq2, int lagc,
		int lent, int lenx, int xopt, float dt, int iopt)
/*******************************************************************
interpolate input data in space placing "ninterp" synthetic traces
between each pair of original input traces
******************************************************************
Function parameters:

VND *vndorig		VND file with input data
VND *vndinterp		VND file with output original plus interpolated data
int ninterp		number of traces to interpolate between each input
			trace
int nt			number of time samples
int nx			number of input traces
float freq1		low-end frequency in Hz for picking
						(good default: 3 Hz)
float freq2		high-end frequency in Hz for picking
						(good default: 20 Hz)
int lagc		length of AGC operator for picking
						(good default: 400 ms)
int lent		length of time smoother in samples for picker
                        (good default: 5 samples)
int lenx		length of space smoother in samples for picker
                        (good default: 1 sample)
int xopt		1 = use differences for spatial derivative
                            (works with irregular spacing)
                        0 = use FFT derivative for spatial derivatives
                            (more accurate but requires regular spacing and
                            at least 16 input tracs--will switch to differences
                            automatically if have less than 16 input traces)
float dt		sample rate in sec
int iopt		0 = interpolate: output 1+(nx-1)*(1+ninterp) traces
                            with ninterp traces between each pair of
			    input traces
			1 = compute low-pass model: output nx traces
                            on original trace locations -- This is typically
                            used for Quality Control if the interpolator
                            is failing for any reason
			2 = compute dip picks in units of samples/trace:
                            output nx traces on original trace locations

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

char *fname;
fname=VNDtempname("main_prog");
vndorig = V2Dop(2,1000000,sizeof(float),fname,nt,nxmax);
VNDfree(fname,"jea_xinterpolate: fname");
fname=VNDtempname("main_prog");
vndinterp = V2Dop(2,1000000,sizeof(float),fname,
			nt,1+(nxmax-1)*(ninterp+1));
VNDfree(fname,"main: fname");

where nxmax is the maximum number of input traces and nt is the number
of time samples.
*******************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*******************************************************************/
{
	int	ntfft,ntfftny,nxfft,nxfftny,j,k,ix,it,ixm;
	float	df,dff,wa,wb,dxx,eps=1e-30,f,fcl,fch;
	float 	*rt=NULL,*rrt=NULL,*a=NULL,*b=NULL,*p=NULL,*time=NULL,*aa=NULL,*bb=NULL,*save=NULL;
	complex	*crt=NULL,*ccrt=NULL;
	VND	*vnda=NULL,*vndb=NULL;
	char 	*fname=NULL;

	lent=1+2*(lent/2);
	lenx=1+2*(lenx/2);
	lagc=1 + lagc*0.001/dt;

	ntfft=npfar(nt);
	ntfftny=1+ntfft/2;
	nxfft=npfar(nx);
	nxfftny=1+nxfft/2;

	df=1./(ntfft*dt);

	crt = (complex *)VNDemalloc( MAX(ntfftny,nxfftny)*sizeof(complex),
		"jea_xinterpolate:allocating crt" );
	rt = (float *)crt;

	if(nx<2 || (iopt==0 && ninterp==0) ) {
	    	for(ix=0;ix<nx;ix++) {
			V2Dr0(vndorig,ix,(char *)rt,101);
			V2Dw0(vndinterp,ix,(char *)rt,102);
		}
		VNDfree(crt,"crt");
		return;
	}

	ccrt = (complex *)VNDemalloc( ntfftny*sizeof(complex),
		"jea_xinterpolate:allocating ccrt" );
	rrt = (float *)ccrt;
	a =  (float *)VNDemalloc( MAX(nx,nt)*sizeof(float),
		"jea_xinterpolate:allocating a" );
	b =  (float *)VNDemalloc( MAX(nx,nt)*sizeof(float),
		"jea_xinterpolate:allocating b" );
	p =  (float *)VNDemalloc( nt*sizeof(float),
		"jea_xinterpolate:allocating p" );
	time =  (float *)VNDemalloc( nt*sizeof(float),
		"jea_xinterpolate:allocating time" );
	aa =  (float *)VNDemalloc( MAX(nx,nt)*sizeof(float),
		"jea_xinterpolate:allocating aa" );
	bb =  (float *)VNDemalloc( MAX(nx,nt)*sizeof(float),
		"jea_xinterpolate:allocating bb" );

	fname = VNDtempname("jea_xinterpolate");
	vnda  = V2Dop(2,500000,sizeof(float),fname,nt,nx);
	VNDfree(fname,"jea_xinterpolate: fname");
	fname = VNDtempname("jea_xinterpolate");
	vndb  = V2Dop(2,500000,sizeof(float),fname,nt,nx);
	VNDfree(fname,"jea_xinterpolate: fname");

	eps = 0;
	for(ix=0;ix<nx;ix++) {
		V2Dr0(vndorig,ix,(char *)rt,203);
		for(j=0;j<nt;j++) eps = MAX(eps, fabs(rt[j]));
	}
	eps = MAX(1e-2*eps, 1e-30);

	/* loop computing filtered data for picking purposes in vnda */
	/* compute time derivative of filtered data in vndb */
	dff=2.*PI/ntfft;
	for(ix=0;ix<nx;ix++) {
		V2Dr0(vndorig,ix,(char *)rt,103);
		for(j=0;j<nt;j++) a[j]=fabs(rt[j]);
		runav(nt,lagc,a,b);
		runav(nt,lagc,b,a);
		for(j=0;j<nt;j++) rt[j]=rt[j]/(a[j]+ eps);
		for(j=nt;j<ntfft;j++) rt[j]=0.;
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
		for(ix=0;ix<nx;ix++){
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
		return;
	}

	/* loop computing spatial derivative of data for picking purposes*/
	nxfft=npfar(nx);
	nxfftny=1+nxfft/2;
	dxx=2.*PI/(nxfft*nxfft);
	if(nx<16) xopt=1;
	for(it=0;it<nt;it++) {
		V2Dr1(vnda,it,(char *)rt,106);
		if(xopt) {
			for(j=0;j<nx-1;j++) rt[j]=rt[j+1]-rt[j];
			rt[nx-1]=rt[nx-2];
		}else{
			for(j=nx;j<nxfft;j++) rt[j]=0.;
			pfarc(1,nxfft,rt,crt);
			for(j=0;j<nxfftny;j++){
				crt[j]=cmul(crt[j],cmplx(0.,-j*dxx));
			}
			pfacr(-1,nxfft,crt,rt);
		}
		V2Dw1(vnda,it,(char *)rt,107);
	}

	/* compute dot products and smooth over time */
	for(ix=0;ix<nx;ix++) {
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
	if(lenx>1){
	    for(it=0;it<nt;it++) {
		V2Dr1(vnda,it,(char *)a,112);
		V2Dr1(vndb,it,(char *)b,113);
		runav(nx,lenx,a,aa);
		runav(nx,lenx,aa,a);
		runav(nx,lenx,b,bb);
		runav(nx,lenx,bb,b);
		V2Dw1(vnda,it,(char *)a,114);
		V2Dw1(vndb,it,(char *)b,115);
	    }
	}

	eps = 0;
	for(ix=0;ix<nx;ix++) {
		V2Dr0(vndb,ix,(char *)bb,218);
		for(it=0;it<nt;it++) eps = MAX(eps, bb[it]);
	}
	eps = MAX(1e-5*eps, 1e-30);

	/* loop computing p, interpolating, and outputting results */
	V2Dr0(vndorig,0,(char *)a,116);
	for(ix=1;ix<nx;ix++) {
		ixm=ix-1;
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
				for(it=0;it<nt;it++) time[it] = it - p[it]*wa;
				ints8r(nt,1.0,0.,a,0.0,0.0,nt,time,aa);
				for(it=0;it<nt;it++) time[it] = it + p[it]*wb;
				ints8r(nt,1.0,0.,b,0.0,0.0,nt,time,bb);
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
		V2Dw0(vndinterp,nx-1,(char *)p,122);
	}else{
		V2Dw0(vndinterp,(nx-1)*(ninterp+1),(char *)a,122);
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

	return;
}
static void runav(int n,int len,float *a,float *b)
/******************************************************************
compute a boxcar running average filter
*******************************************************************
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
