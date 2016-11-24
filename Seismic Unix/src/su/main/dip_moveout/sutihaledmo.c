/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTIHALEDMO: $Revision: 1.5 $ ; $Date: 2011/11/16 17:51:02 $        */

#include "su.h"
#include "segy.h"
#include "header.h" 
#include "VND.h"

/*********************** self documentation **********************/
char *sdoc[] = { 
" SUTIHALEDMO - TI Hale Dip MoveOut (based on Hale's PhD thesis)	",
"									",
"  sutihaledmo <infile >outfile [optional parameters]			",
"									",
"									",
" Required Parameters:							",
" nxmax		  maximum number of midpoints in common offset gather",
"									",
" Optional Parameters:							",
" option=1		1 = traditional Hale DMO (from PhD thesis)	",
"			2 = Bleistein's true amplitude DMO		",
"			3 = Bleistein's cos*cos weighted DMO		",
"			4 = Zhang's DMO					",
"			5 = Tsvankin's anisotropic DMO			",
"			6 = Tsvankin's VTI DMO weak anisotropy approximation",
" dx=50.		 midpoint sampling interval between traces	",
"			in a common offset gather.  (usually shot	",
"			interval in meters)				",
" v=1500.0		velocity (in meters/sec)			",
"			(must enter a positive value for option=3)	",
"			(for excluding evanescent energy)		",
" h=200.0		source-receiver half-offset (in meters)		",
" ntpad=0		number of time samples to pad			",
" nxpad=h/dx		number of midpoints to pad			",
" file=vnmo		name of file with vnmo as a function of p	",
"			used for option=5--otherwise not used		",
"			(Generate this file by running program		",
"			sutivel with appropriate list of Thomsen's	",
"			parameters.)					",
" e=0.			Thompsen's epsilon				",
" d=0.			Thompsen's delta				",
"									",
"Note:									",
"									",
" This module assumes a single common offset gather after NMO is	",
" to be input, DMO corrected, and output.  It is useful for computing	",
" theoretical DMO impulse responses.  The Hale algorithm is		",
" computationally intensive and not commonly used for bulk processing	",
" of all of the offsets on a 2-D line as there are cheaper alternative	",
" algorithms.  The Hale algorithm is commonly used in theoretical studies.",
" Bulk processing for multiple common offset gathers is typically done	",
" using other modules.							",
"									",
" Test run:   suspike | sutihaledmo nxmax=32 option=1 v=1500 | suxwigb & ",
NULL}; 

/*
 * Author:  (Visitor to CSM from Mobil) John E. Anderson Spring 1994
 * References: Anderson, J.E., and Tsvankin, I., 1994, Dip-moveout by
 *	Fourier transform in anisotropic media, CWP-146
 */
/**************** end self doc ********************************/

/* implementation notes: requires VND routines */
void haledmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt, float v);
void bleisteindmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt, float v);
void bleisteindmo2(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt, float v);
void zhangdmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt, float v);
void tsvankindmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt, int np, float dp, 
		float *vnmo);
void weakdmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt, float e, float d,float v);
/* the main program */

segy tr;	/* Input and output trace data of length nt */
int main (int argc, char **argv)
{
	int 	ntfft;
	int	ntpad;
	int 	nxmax;
	int	nxfft;
	int	nxpad;
	int 	nmax;
	int	option;
	int 	np;
	int	ip;
	long	nt;
	long	it;
	long	nx;
	long	ix;
	long	ik;
	float	dt;
	float	dx;
	float	v;
	float	h;
	float	k;
	float 	dk;
	float	scale;
	float	dp;
	float	*rt;
	float	*vnmo=NULL;
	float	d;
	float	e;
	complex *crt;
	complex *ctemp;
	complex czero;
	char	*ccrt;
	char	*file;
	char 	*fname;
	VND	*vnd;
	FILE	*hfp;
	FILE	*fp;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);
		
	/* Get info from first trace */
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;
	dt = 0.000001*tr.dt;
	nx = 0;

	/* get optional parameters */
	if (!getparfloat("dx",&dx)) dx = 50.;
	if (!getparfloat("v",&v)) v = 1500.0;
	if (!getparfloat("h",&h)) h = 200.0;
	if (!getparfloat("e",&e)) e = 0.0;
	if (!getparfloat("d",&d)) d = 0.0;
	if (!getparint("nxpad",&nxpad)) nxpad = h/dx;
	if (!getparint("ntpad",&ntpad)) ntpad = 0;
	if (!getparint("option",&option)) option = 1;
	if (!getparstring("file",&file)) file="vnmo";
	if (v <= 0.0 && (option==3 || option==6)) 
		err("must enter positive value for v when option=3"); 
	
	/* get required parameters */
	if (!getparint("nxmax",&nxmax)) err("must enter nxmax");

        checkpars();

	hfp = tmpfile();

	if(option==5) {
		if((fp=efopen(file,"r"))==NULL)
		   err("suhaledmo: couldn't open vnmo(p) file, option=5");
		fscanf(fp,"%d",&np);
		vnmo=(float *)VNDemalloc(np*sizeof(float),"vnmo");
		fscanf(fp,"%e",&dp);
		fprintf(stderr,"suhaledmo: np=%d dp=%e\n",np,dp);
		for(ip=0;ip<np;ip++)
			fscanf(fp,"%e",&vnmo[ip]);
		fclose(fp);
		}

	ntfft=npfao(nt+ntpad,2*(nt+ntpad));
	nxfft=npfar(nxmax+nxpad);
	dk=2.*PI/(nxfft*dx);
	fprintf(stderr,"suhaledmo: ntfft=%d nxfft=%d\n",ntfft,nxfft);
	czero.r=0.;
	czero.i=0.;
	scale=1./(nxfft);

	nmax = nxfft/2 + 1;
	if(nt > nmax) nmax=nt;
	crt = (complex *)VNDemalloc(nmax*sizeof(complex),"crt");
	ctemp = (complex *)VNDemalloc(ntfft*sizeof(complex),"crt");
	rt = (float *)crt;
	ccrt = (char *)crt;
	
	nx=nxfft+2;
	fname=VNDtempname("suhaledmo");
	vnd = V2Dop(2,100000,sizeof(float),fname,nx,nt);
	
	/* Main loop for saving input traces */
	nx=0;
	do {
		for(it=0;it<nt;it++) tr.data[it]*=scale;
		V2Dw1(vnd,nx,(char *)tr.data,1);
		efwrite(&tr,HDRBYTES,1,hfp);
		nx++;
		if(nx>nxmax) break;
	} while ( gettr(&tr) );

	/* Do forward FFT from x to k */
	for(it=0;it<nt;it++) {
		V2Dr0(vnd,it,ccrt,3);
		for(ix=nx;ix<nxfft;ix++) rt[ix]=0.;
		pfarc(1,nxfft,rt,crt); 
		V2Dw0(vnd,it,ccrt,4);
	}

	/* change vnd dimension 0 from real to complex */
	VNDr2c(vnd);

	/* Do DMO correction on each wavenumber component */
	for(ik=0;ik<(nxfft/2 + 1);ik++) {
		V2Dr1(vnd,ik,ccrt,5);
		k = ik*dk;
		if(option==1) 
			haledmo(h,k,crt,ctemp,nt,ntfft,dt,v);
		if(option==2)
			bleisteindmo(h,k,crt,ctemp,nt,ntfft,dt,v);
		if(option==3)
			bleisteindmo2(h,k,crt,ctemp,nt,ntfft,dt,v);
		if(option==4)
			zhangdmo(h,k,crt,ctemp,nt,ntfft,dt,v);
		if(option==5)
			tsvankindmo(h,k,crt,ctemp,nt,ntfft,dt,np,dp,vnmo);
		if(option==6)
			weakdmo(h,k,crt,ctemp,nt,ntfft,dt,e,d,v);

		V2Dw1(vnd,ik,ccrt,6);
	}


	/* Do inverse FFT from k to x */
	for(it=0;it<nt;it++) {
		V2Dr0(vnd,it,ccrt,7);
		pfacr(-1,nxfft,crt,rt); 
		V2Dw0(vnd,it,ccrt,8);
	}

	/* change vnd dimension 0 from complex to real */
	VNDc2r(vnd);

	/* output the DMO result */
	efseek(hfp,(off_t)0L,SEEK_SET);
	for(ix=0;ix<nx;ix++) {
		V2Dr1(vnd,ix,(char *)tr.data,7);
		efread(&tr,HDRBYTES,1,hfp);
		puttr(&tr);
	}

	/* close files and return */
	VNDcl(vnd,1);
	VNDfree(crt,"crt");
	VNDfree(ctemp,"ctemp");
	if(option==5) VNDfree(vnmo ,"vnmo");
	if(VNDtotalmem()!=0) {
		fprintf(stderr,"total VND memory at end = %ld\n",
		VNDtotalmem());
	}
	return(CWP_Exit());
}
void haledmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt,float v)
/*  hale fk dmo

h	half offset
k	wavenumber
p	input NMO corrected data in wavenumber domain, p(tn,k,h)
pwork	complex array of lenght ntfft for work space
nt	number of time samples
ntfft	lenght of temporal fft
dt	temporal sample rate in seconds
v	velocity in m/sec (used to limit aperature of DMO response)

*/
{
	complex csum;		/* complex variable for sum 		*/
	complex cfac;	
	float w;		/* 	angular freq 			*/
	float hk;		/* 	h*k				*/
	float hkpw;		/*	h*k/w				*/
	float hkpwt;		/*	h*k/(w*tn)			*/
	float A;		/*	Hale's A			*/
	float phase;		/*	phase				*/
	float dw;		/*	angular freq inc		*/
	float tn;		/*	input event time after NMO	*/
	float eps=1.0e-20;	/*	zero offset*wavenumber thresh	*/
	float scale;		/*	forward/inverse fft scale factor*/
	int iw;			/*	freq index			*/
	int it;			/*	tn time index			*/
	int iwtest;		/*	test for pos or neg freq	*/
	

	hk = h*k;
	if(hk<eps) return;
	dw = 2*PI/(ntfft*dt);
	scale=1./ntfft;
	iwtest=ntfft/2+1;
	pwork[0]=cmplx(0.,0.);
	for(iw=1;iw<ntfft;iw++) {
		if(iw<iwtest)
			w=iw*dw;
		else
			w=(iw-ntfft)*dw;
		csum=cmplx(0.,0.);
		if( (k*v) < (2*fabs(w)) ) {
			hkpw=hk/(w);
			for(it=1;it<nt;it++) {
				tn=it*dt;
				hkpwt=hkpw/tn;
				A = sqrt(1. + hkpwt*hkpwt);
				phase = w*tn*A;
				cfac = cmplx( cos(phase)/A, sin(phase)/A ); 		
				csum = cadd( csum, cmul(p[it],cfac) );

			}
		}
		pwork[iw]=csum;
	}
	pfacc(-1,ntfft,pwork);
	for(it=0;it<nt;it++) p[it]=crmul(pwork[it],scale);
	return;
}
void bleisteindmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt,float v)
/*  bleistein true amplitude fk dmo

h	half offset
k	wavenumber
p	input NMO corrected data in wavenumber domain, p(tn,k,h)
pwork	complex array of lenght ntfft for work space
nt	number of time samples
ntfft	lenght of temporal fft
dt	temporal sample rate in seconds
v	velocity in m/sec (used to limit aperature of DMO response)

*/
{
	complex csum;		/* complex variable for sum 		*/
	complex cfac;	
	float w;		/* 	angular freq 			*/
	float hk;		/* 	h*k				*/
	float hkpw;		/*	h*k/w				*/
	float hkpwt;		/*	h*k/(w*tn)			*/
	float A;		/*	Hale's A			*/
	float B;		/*	Bleistein's scale factor	*/
	float phase;		/*	phase				*/
	float dw;		/*	angular freq inc		*/
	float tn;		/*	input event time after NMO	*/
	float eps=1.0e-20;	/*	zero offset*wavenumber thresh	*/
	float scale;		/*	forward/inverse fft scale factor*/
	int iw;			/*	freq index			*/
	int it;			/*	tn time index			*/
	int iwtest;		/*	test for pos or neg freq	*/
	

	hk = h*k;
	if(hk<eps) return;
	dw = 2*PI/(ntfft*dt);
	scale=1./ntfft;
	iwtest=ntfft/2+1;
	pwork[0]=cmplx(0.,0.);
	for(iw=1;iw<ntfft;iw++) {
		if(iw<iwtest)
			w=iw*dw;
		else
			w=(iw-ntfft)*dw;
		csum=cmplx(0.,0.);
		if( (k*v) < (2*fabs(w)) ) {
			hkpw=hk/(w);
			for(it=1;it<nt;it++) {
				tn=it*dt;
				hkpwt=hkpw/tn;
				A = sqrt(1. + hkpwt*hkpwt);
				B = sqrt(1. + 2*hkpwt*hkpwt);
				phase = w*tn*A;
				cfac = cmplx( B*cos(phase)/A, B*sin(phase)/A ); 		
				csum = cadd( csum, cmul(p[it],cfac) );
			}
		}
		pwork[iw]=csum;
	}
	pfacc(-1,ntfft,pwork);
	for(it=0;it<nt;it++) p[it]=crmul(pwork[it],scale);
	return;
}
void bleisteindmo2(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt,float v)
/*  bleistein cos*cos weighted amplitude fk dmo

h	half offset
k	wavenumber
p	input NMO corrected data in wavenumber domain, p(tn,k,h)
pwork	complex array of lenght ntfft for work space
nt	number of time samples
ntfft	lenght of temporal fft
dt	temporal sample rate in seconds
v	velocity in m/sec (used to limit aperature of DMO response)

*/
{
	complex csum;		/* complex variable for sum 		*/
	complex cfac;	
	float w;		/* 	angular freq 			*/
	float hk;		/* 	h*k				*/
	float hkpw;		/*	h*k/w				*/
	float hkpwt;		/*	h*k/(w*tn)			*/
	float hkpwt2;		/*	square of above			*/
	float hpv;		/* 	2*h/v				*/
	float hpvt2;		/*	[(2*h)/(v*tn)]^2		*/
	float A;		/*	Hale's A			*/
	float B;		/*	Bleistein's scale factor	*/
	float phase;		/*	phase				*/
	float dw;		/*	angular freq inc		*/
	float tn;		/*	input event time after NMO	*/
	float scale;		/*	forward/inverse fft scale factor*/
	int iw;			/*	freq index			*/
	int it;			/*	tn time index			*/
	int iwtest;		/*	test for pos or neg freq	*/
	

	hk = h*k;
	dw = 2*PI/(ntfft*dt);
	scale=1./ntfft;
	iwtest=ntfft/2+1;
	pwork[0]=cmplx(0.,0.);
	hpv=2.*h/v;
	for(iw=1;iw<ntfft;iw++) {
		if(iw<iwtest)
			w=iw*dw;
		else
			w=(iw-ntfft)*dw;
		csum=cmplx(0.,0.);
		if( (k*v) < (2*fabs(w)) ) {
			hkpw=hk/(w);
			for(it=1;it<nt;it++) {
				tn=it*dt;
				hkpwt=hkpw/tn;
				hkpwt2=hkpwt*hkpwt;
				hpvt2=hpv/tn;
				hpvt2=hpvt2*hpvt2;
				A = sqrt(1. + hkpwt2);
				B = (1.+2*hkpwt2)*(1+hkpwt2)/(1+hpvt2);
				phase = w*tn*A;
				cfac = cmplx( B*cos(phase)/A, B*sin(phase)/A ); 		
				csum = cadd( csum, cmul(p[it],cfac) );
			}
		}
		pwork[iw]=csum;
	}
	pfacc(-1,ntfft,pwork);
	for(it=0;it<nt;it++) p[it]=crmul(pwork[it],scale);
	return;
}
void zhangdmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt,float v)
/*  zhang fk dmo

h	half offset
k	wavenumber
p	input NMO corrected data in wavenumber domain, p(tn,k,h)
pwork	complex array of lenght ntfft for work space
nt	number of time samples
ntfft	lenght of temporal fft
dt	temporal sample rate in seconds
v	velocity in m/sec (used to limit aperature of DMO response)

*/
{
	complex csum;		/* complex variable for sum 		*/
	complex cfac;	
	float w;		/* 	angular freq 			*/
	float hk;		/* 	h*k				*/
	float hkpw;		/*	h*k/w				*/
	float hkpwt;		/*	h*k/(w*tn)			*/
	float hkpwt2;		/* 	square of above			*/
	float A;		/*	Hale's A			*/
	float B;		/*	Zhang's scale factor		*/
	float phase;		/*	phase				*/
	float dw;		/*	angular freq inc		*/
	float tn;		/*	input event time after NMO	*/
	float eps=1.0e-20;	/*	zero offset*wavenumber thresh	*/
	float scale;		/*	forward/inverse fft scale factor*/
	int iw;			/*	freq index			*/
	int it;			/*	tn time index			*/
	int iwtest;		/*	test for pos or neg freq	*/
	
	hk = h*k;
	if(hk<eps) return;
	dw = 2*PI/(ntfft*dt);
	scale=1./ntfft;
	iwtest=ntfft/2+1;
	pwork[0]=cmplx(0.,0.);
	for(iw=1;iw<ntfft;iw++) {
		if(iw<iwtest)
			w=iw*dw;
		else
			w=(iw-ntfft)*dw;
		csum=cmplx(0.,0.);
		if( (k*v) < (2*fabs(w)) ) {
			hkpw=hk/(w);
			for(it=1;it<nt;it++) {
				tn=it*dt;
				hkpwt=hkpw/tn;
				hkpwt2=hkpwt*hkpwt;
				A = sqrt(1. + hkpwt2);
				B = (1.+2*hkpwt2)/(1+hkpwt2);
				phase = w*tn*A;
				cfac = cmplx( B*cos(phase)/A, B*sin(phase)/A ); 		
				csum = cadd( csum, cmul(p[it],cfac) );
			}
		}
		pwork[iw]=csum;
	}
	pfacc(-1,ntfft,pwork);
	for(it=0;it<nt;it++) p[it]=crmul(pwork[it],scale);
	return;
}
void tsvankindmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt,int np, float dp, float *vnmo)
/*  tsvankin fk dmo

h	half offset
k	wavenumber
p	input NMO corrected data in wavenumber domain, p(tn,k,h)
pwork	complex array of lenght ntfft for work space
nt	number of time samples
ntfft	lenght of temporal fft
dt	temporal sample rate in seconds
np	number of slowness values for which vnmo has been tabled
dp	slowness increment for vnmo table
vnmo	nmo velocity in m/sec as a function of p 

*/
{
	complex csum;		/* complex variable for sum 		*/
	complex cfac;	
	float w;		/* 	angular freq 			*/
	float hk;		/* 	h*k				*/
	double v;		/*	vnmo(p)				*/
	double v0;		/*	vnmo(0)				*/
	float A;		/*	Hale's A			*/
	float phase;		/*	phase				*/
	float dw;		/*	angular freq inc		*/
	float tn;		/*	input event time after NMO	*/
	float tn2;
	float eps=1.0e-20;	/*	zero offset*wavenumber thresh	*/
	float scale;		/*	forward/inverse fft scale factor*/
	float pp;		/*	ray parameter			*/
	double fac;
	float rp;
	float wgt;
	int iw;			/*	freq index			*/
	int it;			/*	tn time index			*/
	int it1;		/* 	first stable time index		*/
	int iwtest;		/*	test for pos or neg freq	*/
	int ip;
	int npm;

	hk = h*k;
	if(hk<eps) return;
	npm=np-2;
	dw = 2*PI/(ntfft*dt);
	scale=1./ntfft;
	iwtest=ntfft/2+1;
	pwork[0]=cmplx(0.,0.);
	for(iw=1;iw<ntfft;iw++) {

		if(iw<iwtest)
			w=iw*dw;
		else
			w=(iw-ntfft)*dw;

		csum=cmplx(0.,0.);
		pp=fabs(k/(2.*w));
		rp=pp/dp;
		ip=rp;
		if(ip<npm) {
			wgt=rp-ip;
			v=(1.-wgt)*vnmo[ip] + wgt*vnmo[ip+1];
			v0=vnmo[0];
			fac=4*h*h*(1./(v0*v0) - 1./(v*v) );
			if(fac<0.) {
				it1=2+sqrt(fabs(fac))/dt;
			}else{
				it1=1;
			}
			for(it=it1;it<nt;it++) {
				tn=it*dt;
				tn2=tn*tn;
				A=sqrt(1. + fac/tn2);
				phase = w*tn*A;
				cfac = cmplx( cos(phase)/A, sin(phase)/A ); 		
				csum = cadd( csum, cmul(p[it],cfac) );	
			}
		}
		pwork[iw]=csum;
	}
	pfacc(-1,ntfft,pwork);
	for(it=0;it<nt;it++) p[it]=crmul(pwork[it],scale);
	return;
}
void weakdmo(float h,float k,complex *p,complex *pwork,
		int nt,int ntfft,float dt,float e,float d, float v)
/*  tsvankin fk dmo weak approximation

h	half offset
k	wavenumber
p	input NMO corrected data in wavenumber domain, p(tn,k,h)
pwork	complex array of lenght ntfft for work space
nt	number of time samples
ntfft	lenght of temporal fft
dt	temporal sample rate in seconds
e	epsilon
d	delta
v	v0 velocity
*/
{
	complex csum;		/* complex variable for sum 		*/
	complex cfac;	
	float w;		/* 	angular freq 			*/
	float hk;		/* 	h*k				*/
	float A;		/*	Hale's A			*/
	float phase;		/*	phase				*/
	float dw;		/*	angular freq inc		*/
	float tn;		/*	input event time after NMO	*/
	float tn2;
	float eps=1.0e-20;	/*	zero offset*wavenumber thresh	*/
	float scale;		/*	forward/inverse fft scale factor*/
	float pp;		/*	ray parameter			*/
	float y;
	double fac;
	int iw;			/*	freq index			*/
	int it;			/*	tn time index			*/
	int it1;		/* 	first stable time index		*/
	int iwtest;		/*	test for pos or neg freq	*/

	hk = h*k;
	if(hk<eps) return;
	dw = 2*PI/(ntfft*dt);
	scale=1./ntfft;
	iwtest=ntfft/2+1;
	pwork[0]=cmplx(0.,0.);
	for(iw=1;iw<ntfft;iw++) {

		if(iw<iwtest)
			w=iw*dw;
		else
			w=(iw-ntfft)*dw;

		csum=cmplx(0.,0.);
		pp=fabs(k/(2.*w));
		if(pp<(1/v)) {
			y=pp*pp*v*v;
			fac=4*h*h*pp*pp*(1+2*(e-d)*(4*y*y-9*y+6));
			if(fac<0.) {
				it1=2+sqrt(fabs(fac))/dt;
			}else{
				it1=1;
			}
			for(it=it1;it<nt;it++) {
				tn=it*dt;
				tn2=tn*tn;
				A=sqrt(1. + fac/tn2);
				phase = w*tn*A;
				cfac = cmplx( cos(phase)/A, sin(phase)/A ); 		
				csum = cadd( csum, cmul(p[it],cfac) );	
			}
		}
		pwork[iw]=csum;
	}
	pfacc(-1,ntfft,pwork);
	for(it=0;it<nt;it++) p[it]=crmul(pwork[it],scale);
	return;
}
