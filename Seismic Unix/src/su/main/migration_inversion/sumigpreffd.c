/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMIGPREFFD: $Vision: 1.00 $ ; $Date: 2015/08/07 22:19:43 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
"SUMIGPREFFD - The 2-D prestack common-shot Fourier finite-difference	",
"		depth  migration.					",
"									",
"  sumigpreffd <indata >outfile [parameters]				", 
"									",
" Required Parameters:							",  
" nxo=	   number of total horizontal output samples			",
" nxshot=	number of shot gathers to be migrated			",
" nz=		number of depth sapmles					",
" dx=		horizontal sampling interval				",
" dz=		depth sampling interval					",
" vfile=	 velocity profile, it must be binary format.		",
"									",  
" Optional Parameters:							",
" fmax=25	the peak frequency of Ricker wavelet used as source wavelet",
" f1=5,f2=10,f3=40,f4=50	frequencies to build a Hamming window	",
" lpad=9999,rpad=9999		number of zero traces padded on both	",
"				sides of depth section to determine the ",
"				migration aperature, the default	",
"				values are using the full aperature.	",
" verbose=0		silent, =1 additional runtime information	",
"									",  
" Notes:								",
" The input velocity file consists of C-style binary floats.		",  
" The structure of this file is vfile[iz][ix]. Note that this means that",
" the x-direction is the fastest direction instead of z-direction! Such a",
" structure is more convenient for the downward continuation type	",
" migration algorithm than using z as fastest dimension as in other SU  ", 
" programs.								",
"									",
" Because most of the tools in the SU package (such as  unif2, unisam2, ", 
" and makevel) produce output with the structure vfile[ix][iz], you will",
" need to transpose the velocity files created by these programs. You may",
" use the SU program \'transp\' in SU to transpose such files into the  ",
" required vfile[iz][ix] structure.					",
" (In C  v[iz][ix] denotes a v(x,z) array, whereas v[ix][iz]  		",
" denotes a v(z,x) array, the opposite of what Matlab and Fortran	",
" programmers may expect.)						", 
"									",
" Also, sx must be monotonically increasing throughout the dataset, and ",
" and gx must be monotonically increasing within a shot. You may resort ",
" your data with \'susort\', accordingly.				",
"									",
" The scalco header field is honored so this field must be set correctly.",
" See selfdocs of \'susort\', \'suchw\'. Also:   sukeyword scalco	",
"									",
NULL};

/*
 * Credits: CWP, Baoniu Han, bhan@dix.mines.edu, April 19th, 1998
 *
 *	  Modified: Chris Stolk, 11 Dec 2005, - changed data input
 *		    to remove erroneous time delay.
 *	  Modified: CWP, John Stockwell 26 Sept 2006 - replaced Han's
 *	  "goto-loop" with  "do { }while loops".
 *	  Fixed it so that sx, gx, and scalco are honored.
 *
 *
 *
 * Trace header fields accessed: ns, dt, delrt, d2
 * Trace header fields modified: ns, dt, delrt
 */

/**************** end self doc *******************************************/

/* Prototypes of subroutines used internally */
float *ricker(float Freq,float dt,int *Npoint);
void retris(complex *data,complex *a,complex *c,complex *b,complex
		endl,complex endr, int nx, complex *d);
void fdmig( complex **cp, int nx, int nw, float *v,float fw,float
		dw,float dz,float dx,float dt,float vc,int dip);
void get_sx_gx(float *sx, float *gx);

segy tr;

int
main (int argc, char **argv)
{
	int nt;			/* number of time samples		*/
	int nz;			/* number of migrated depth samples	*/
	int nx,nxshot;	/* number of midpoints			*/
	int nxshot_orig;	/* first value of nxshot		*/

	int iz,iw,ix,it,ik;	/* loop counters			*/
	int igx;		/* integerized gx value		 */

	int ntfft,nxfft;	/* fft size				*/
	int nw,truenw,nk;	/* number of wave number, frequency	*/
	int dip=45;		/* dip angle				*/

	float sx,gx;		/* x source and geophone location	*/
	float gxmin=0.0,gxmax=0.0;/* x source and geophone location	*/
	float min_sx_gx;	/* min(sx,gx)			   	*/
	float oldgx;		/* old gx position			*/
	float oldgxmin;		/* old gx position			*/
	float oldgxmax;		/* old gx position			*/
	float oldsx=0.0;	/* old sx position			*/
	int oldigx=0;		/* old value of integerized gx value	*/
	int oldisx=0;		/* old value of integerized sx value	*/

	int isx=0,nxo;		/* index for source and geophone	*/
	int ix1,ix2,ix3,ixshot;	/* dummy index				*/
	int lpad,rpad;/* padding on both sides of the migrated section	*/

	float *wl=NULL,*wtmp=NULL;
	float fmax;
	float f1,f2,f3,f4;
	int nf1,nf2,nf3,nf4;
	int ntw;

	float dt=0.004,dz;	/* time sampling interval 		*/
	float dw,dk;		/* wave number,frequency sampling interval*/
	float fw,fk;		/* first wave number and frequency	*/
	float w,k;		/* wave number and frequency		*/
	float dx;		/* spatial sampling interval		*/
	float **p=NULL;		/* input, output data			*/
	float **cresult=NULL;	/* input, output data			*/
	float v1,vmin;		/* average, min velocity		*/
	double kz1,kz2;
	double phase1;

	float **v=NULL,**vp=NULL;
	complex cshift1,cshift2;
	complex *wlsp=NULL;
	complex **cp=NULL;
	complex **cp1=NULL;
	complex **cq=NULL;
	complex **cq1=NULL;	/* complex input,output			*/
	char *vfile="";		/* name of file containing velocities	*/
	FILE *vfp=NULL;
	
	int verbose;		/* verbose flag				*/

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);

	/* get required parameters */
	MUSTGETPARINT("nz",&nz);
	MUSTGETPARINT("nxo",&nxo);
	MUSTGETPARINT("nxshot",&nxshot);
	MUSTGETPARFLOAT("dz",&dz);
	MUSTGETPARSTRING("vfile", &vfile);

	/* get optional parameters */
	if (!getparfloat("fmax",&fmax)) fmax = 25.0;  
	if (!getparfloat("f1",&f1)) f1 = 10.0;
	if (!getparfloat("f2",&f2)) f2 = 20.0;
	if (!getparfloat("f3",&f3)) f3 = 40.0;
	if (!getparfloat("f4",&f4)) f4 = 50.0;

	if (!getparint("lpad",&lpad)) lpad=9999;
	if (!getparint("rpad",&rpad)) rpad=9999;
	if (!getparint("dip",&dip)) dip=45;

	if (!getparint("verbose",&verbose))     verbose = 0;


	/* allocating space */
	cresult = alloc2float(nz,nxo);
	vp=alloc2float(nxo,nz);

	/* load velocity file */
	vfp=efopen(vfile,"r");
	efread(vp[0],FSIZE,nz*nxo,vfp);
	efclose(vfp);

	/* zero out cresult array */
	memset((void *) cresult[0],0, nxo*nz*FSIZE);
			
	/* save value of nxshot */
	nxshot_orig=nxshot;

	/* get info from first trace */
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;
	get_sx_gx(&sx,&gx);
	min_sx_gx = MIN(sx,gx);
	sx = sx - min_sx_gx;
	gx = gx - min_sx_gx;

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

        checkpars();

	do {    /* begin loop over shots */
 
		/* determine frequency sampling interval */
		ntfft = npfar(nt);
		nw = ntfft/2+1;
		dw = 2.0*PI/(ntfft*dt);

		/* compute the index of the frequency to be migrated */
		fw=2.0*PI*f1;
		nf1=fw/dw+0.5;
		 
		fw=2.0*PI*f2;
		nf2=fw/dw+0.5;

		fw=2.0*PI*f3;
		nf3=fw/dw+0.5;

		fw=2.0*PI*f4;
		nf4=fw/dw+0.5;  

		/* the number of frequency to migrated */
		truenw=nf4-nf1+1;
		fw=0.0+nf1*dw;
		if (verbose)
			warn("nf1=%d nf2=%d nf3=%d nf4=%d nw=%d",nf1,nf2,nf3,nf4,truenw);

		/* allocate space */
		wl=alloc1float(ntfft);
		wlsp=alloc1complex(nw);

		/* generate the Ricker wavelet */
		wtmp=ricker(fmax,dt,&ntw);

		/* zero out wl[] array */
		memset((void *) wl, 0, ntfft*FSIZE);
	
		/* CHANGE BY CHRIS STOLK, Dec. 11, 2005 */
		/* The next two lines are the old code, */
		/* it is erroneous because the peak of  */
		/* the wavelet occurs at positive time  */
		/* instead of time zero. */
		/*
		for(it=0;it<ntw;it++)
	  		wl[it]=wtmp[it];
		*/
		/* New code: we put in the wavelet in a centered fashion */ 
		for(it=0;it<ntw;it++) 
	  		wl[(it-ntw/2+ntfft) % ntfft]=wtmp[it];
		/* End of new code */
		free1float(wtmp);

		pfarc(-1,ntfft,wl,wlsp);

		/* allocate space */
		p = alloc2float(ntfft,nxo);
		cq = alloc2complex(nw,nxo);
	
		/* zero out p[][] array */
		memset((void *) p[0], 0, ntfft*nxo*FSIZE);

		/* initialize a number of items before looping over traces */
		nx = 0;
		igx=0;
		oldsx=sx;
		oldgx=gx;
		oldgxmax=gxmax;
		oldgxmin=gxmin;
		
		do { /* begin looping over traces within a shot gather */

			memcpy( (void *) p[igx], (const void *) tr.data,nt*FSIZE);
			
			/* get sx and gx */
			get_sx_gx(&sx,&gx);
			sx = (sx - min_sx_gx);
			gx = (gx - min_sx_gx);

			igx = gx/dx;
			if (igx==oldigx) 
			   warn("repeated igx!!! check dx or scalco value!!!");
			oldigx = igx;


			if(gxmin>gx)gxmin=gx;
			if(gxmax<gx)gxmax=gx;

			if(verbose)
				warn(" inside loop:  min_sx_gx %f isx %d igx %d gx %f sx %f",min_sx_gx,isx,igx,gx,sx);


			/* sx, gx must increase monotonically */
			if (!(oldsx <= sx) )
			 err("sx field must be monotonically increasing!");
			if (!(oldgx <= gx) )
			 err("gx field must be monotonically increasing!");

			++nx;
		} while(gettr(&tr) && sx==oldsx);

		isx=oldsx/dx;
		if (isx==oldisx) 
			warn("repeated isx!!! check dx or scalco value!!!");
		oldisx=isx;
		ixshot=isx;
		if(verbose) {
			warn("sx %f, gx %f , gxmin %f  gxmax %f nx %d",sx,gx,gxmin,gxmax, nx);
			warn("isx %d igx %d ixshot %d" ,isx,igx,ixshot);
		}


		/* transform the shot gather from time to frequency domain */
		pfa2rc(1,1,ntfft,nxo,p[0],cq[0]);


		/* compute the most left and right index for the migrated */
		/* section */
		ix1=oldsx/dx;
		ix2=gxmin/dx;
		ix3=gxmax/dx;

		if(ix1>=ix3)ix3=ix1;
		if(ix1<=ix2)ix2=ix1;

		ix2-=lpad;
		ix3+=rpad;
		if(ix2<0)ix2=0;
		if(ix3>nxo-1)ix3=nxo-1;

		/* the total traces to be migrated */
		nx=ix3-ix2+1;
		nw=truenw;

		/* determine wavenumber sampling (for complex to complex FFT) */
		nxfft = npfa(nx);
		nk = nxfft;
		dk = 2.0*PI/(nxfft*dx);
		fk = -PI/dx;


		/* allocate space for velocity profile within the aperature */
		v=alloc2float(nx,nz);
		for(iz=0;iz<nz;iz++) 
			for(ix=0;ix<nx;ix++)
				v[iz][ix]=vp[iz][ix+ix2];

		/* allocate space */
		cp = alloc2complex(nx,nw);
		cp1 = alloc2complex(nx,nw);

		/* transpose the frequency domain data from	*/
		/* data[ix][iw] to data[iw][ix] and apply a	*/
		/* Hamming at the same time			*/

		for (ix=0; ix<nx;++ix) {
			for (iw=0; iw<nw; iw++){
				float tmpp=0.0,tmppp=0.0;

				if(iw>=(nf1-nf1)&&iw<=(nf2-nf1)){
					tmpp=PI/(nf2-nf1);
					tmppp=tmpp*(iw-nf1)-PI;
					tmpp=0.54+0.46*cos(tmppp);
					cp[iw][ix]=crmul(cq[ix+ix2][iw+nf1],tmpp);
				} else {
					if(iw>=(nf3-nf1)&&iw<=(nf4-nf1)) {
						tmpp=PI/(nf4-nf3);
						tmppp=tmpp*(iw-nf3);
						tmpp=0.54+0.46*cos(tmppp);
						cp[iw][ix]=crmul(cq[ix+ix2][iw+nf1],tmpp);
					} else {
						cp[iw][ix]=cq[ix+ix2][iw+nf1];
					}
				}
				cp1[iw][ix]=cmplx(0.0,0.0);
			}

		}
		for(iw=0;iw<nw;iw++) {
			cp1[iw][ixshot-ix2]=wlsp[iw+nf1];
		}
	
		if(verbose) {
				warn("ixshot %d ix %d ix1 %d ix2 %d ix3 %d",ixshot,ix,ix1,ix2,ix3);
				warn("oldsx %f ",oldsx);
		}

			
		free2float(p);
		free2complex(cq);
		free1float(wl);
		free1complex(wlsp);

		cq=alloc2complex(nxfft,nw);
		cq1=alloc2complex(nxfft,nw);

		/* loops over depth */
		for(iz=0;iz<nz;++iz){

			/* the imaging condition */
			for(ix=0;ix<nx;ix++){
				for(iw=0,w=fw;iw<nw;w+=dw,iw++){   
					complex tmp;
					float ratio=10.0;
		
					if(fabs(ix+ix2-ixshot)*dx<ratio*iz*dz)
						tmp=cmul(cp[iw][ix],cp1[iw][ix]);
					else
						tmp=cmplx(0.0,0.0);  

					cresult[ix+ix2][iz]+=tmp.r/ntfft;
				}
			}

		
			/* get the minimum velocity */
			vmin=v[iz][0];
			for(ix=0;ix<nx;ix++){
				if(v[iz][ix]<vmin)vmin=v[iz][ix];
			}
		
			/* compute time-invariant wavefield */
			for (ik=0;ik<nx;++ik) {
				for (iw=0; iw<nw; ++iw) {
					cq[iw][ik] = ik%2 ? cneg(cp[iw][ik]) : cp[iw][ik];
					cq1[iw][ik] = ik%2 ? cneg(cp1[iw][ik]) : cp1[iw][ik];
				}
			}

		 
			/* zero out parts of the cq[][] and cq1[][] arrays */
			for (ik=nx; ik<nk; ++ik) {
				for (iw=0; iw<nw; ++iw) {
					cq[iw][ik] = cmplx(0.0,0.0);
					cq1[iw][ik] = cmplx(0.0,0.0);
				}
			}
			/* FFT to W-K domain */
			pfa2cc(-1,1,nk,nw,cq[0]);
			pfa2cc(-1,1,nk,nw,cq1[0]);
	
			v1=vmin;

			/* apply phase shift */
			for(ik=0,k=fk;ik<nk;++ik,k+=dk) {
				for(iw=0,w=fw;iw<nw;++iw,w+=dw){
					if(w==0.0)w=1.0e-10/dt; 

					kz1=1.0-pow(v1*k/w,2.0);
					if(kz1>0.15){
						phase1 = -w*sqrt(kz1)*dz/v1;
						cshift1 = cmplx(cos(phase1), sin(phase1));
						cq[iw][ik] = cmul(cq[iw][ik],cshift1);
						cq1[iw][ik] = cmul(cq1[iw][ik],cshift1);
					} else {
						cq[iw][ik] = cq1[iw][ik] = cmplx(0.0,0.0);
					}
				}
			}
	
			/* fourier transform */
			pfa2cc(1,1,nk,nw,cq[0]);
			pfa2cc(1,1,nk,nw,cq1[0]);

			for(ix=0;ix<nx;++ix) {
				for(iw=0,w=fw;iw<nw;w+=dw,++iw){
					float a=0.015,g=1.0;
					int I=10;
				
					if(ix<=I)
						g=exp(-a*(I-ix)*(I-ix));
					if(ix>=nx-I)
						g=exp(-a*(-nx+I+ix)*(-nx+I+ix));
				 
					cq[iw][ix] = crmul( cq[iw][ix],1.0/nxfft);
					cq[iw][ix] =ix%2 ? cneg(cq[iw][ix]) : cq[iw][ix];
					kz2=(1.0/v1-1.0/v[iz][ix])*w*dz;
					cshift2=cmplx(cos(kz2),sin(kz2));
					cp[iw][ix]=cmul(cq[iw][ix],cshift2);
		
					cq1[iw][ix] = crmul( cq1[iw][ix],1.0/nxfft);
					cq1[iw][ix] =ix%2 ? cneg(cq1[iw][ix]) : cq1[iw][ix];
					cp1[iw][ix]=cmul(cq1[iw][ix],cshift2);
		 
				}
			}
				
			/* apply fdmig algorithm */
			fdmig( cp, nx, nw,v[iz],fw,dw,dz,dx,dt,v1,dip);
			fdmig( cp1,nx, nw,v[iz],fw,dw,dz,dx,dt,v1,dip);

		}

		free2complex(cp);
		free2complex(cp1);
		free2complex(cq);
		free2complex(cq1);
		free2float(v);

		--nxshot;
	} while	(nxshot);


	/* restore header fields and write output */
	for(ix=0; ix<nxo; ix++) {
		tr.ns = nz;
		tr.d1 = dz;
		tr.d2 = dx;
		tr.offset = 0;
		tr.cdp = tr.tracl = ix;
		memcpy( (void *) tr.data, (const void *) cresult[ix],nz*FSIZE);	
		puttr(&tr);
	}

	return(CWP_Exit());	
}


float *ricker(float Freq,float dt,int *Npoint) 
{
	int i;			/* they are the dummy counter*/
	float Bpar,t,u,*Amp;
	int Np1,N;
	
	if(Freq==0.0)Freq=30.0;
	if(dt==0.0)dt=0.004;
	Bpar=sqrt(6.0)/(PI*Freq);
	N=ceil(1.35*Bpar/dt);
	Np1=N;
	*Npoint=2*N+1;
	 
	Amp=alloc1float(*Npoint);
	
	Amp[Np1]=1.0;
  
	for(i=1;i<=N;i++) {
		t=dt*(float)i;
		u=2.0*sqrt(6.0)*t/Bpar;
		Amp[Np1+i]=Amp[Np1-i]=0.5*(2.0-u*u)*exp(-u*u/4.0);
	}

	return Amp;
}

void fdmig( complex **cp, int nx, int nw, float *v,float fw,float
	dw,float dz,float dx,float dt,float vc,int dip)
{
	int iw,ix;
	float *p,*s1,*s2,w,coefa,coefb,v1,vn,trick=0.1;
	complex cp2,cp3,cpnm1,cpnm2;
	complex a1,a2,b1,b2;
	complex endl,endr;
	complex *data,*d,*a,*b,*c;

	p=alloc1float(nx);
	s1=alloc1float(nx);
	s2=alloc1float(nx);

	data=alloc1complex(nx);
	d=alloc1complex(nx);
	a=alloc1complex(nx);
	b=alloc1complex(nx);
	c=alloc1complex(nx);

	for(ix=0;ix<nx;ix++){
		p[ix]=vc/v[ix];
		p[ix]=(p[ix]*p[ix]+p[ix]+1.0);
	}

	
	if(dip!=65){
		coefa=0.5;coefb=0.25;
	} else {
		coefa=0.4784689;
		coefb=0.37607656;
	}

	v1=v[0];
	vn=v[nx-1];

	for(iw=0,w=fw;iw<nw;iw++,w+=dw){
		if(fabs(w)<=1.0e-10)w=1.0e-10/dt; 

		for(ix=0;ix<nx;ix++){
			s1[ix]=(v[ix]*v[ix])*p[ix]*coefb/(dx*dx*w*w)+trick;
			s2[ix]=-(1-vc/v[ix])*v[ix]*dz*coefa/(w*dx*dx)*0.5;
		}

		for(ix=0;ix<nx;ix++){
			data[ix]=cp[iw][ix];
		}

		cp2=data[1];
		cp3=data[2];
		cpnm1=data[nx-2];
		cpnm2=data[nx-3];
		a1=crmul(cmul(cp2,conjg(cp3)),2.0);
		b1=cadd(cmul(cp2,conjg(cp2)),cmul(cp3,conjg(cp3)));

		if(b1.r==0.0 && b1.i==0.0)
			a1=cwp_cexp(cmplx(0.0,-w*dx*0.5/v1));
		else
			a1=cdiv(a1,b1);

		if(a1.i>0.0)
			a1=cwp_cexp(cmplx(0.0,-w*dx*0.5/v1));

		a2=crmul(cmul(cpnm1,conjg(cpnm2)),2.0);
		b2=cadd(cmul(cpnm1,conjg(cpnm1)),cmul(cpnm2,conjg(cpnm2)));

		if(b2.r==0.0 && b2.i==0.0)
			a2=cwp_cexp(cmplx(0.0,-w*dx*0.5/vn));
		else
			a2=cdiv(a2,b2);

		if(a2.i>0.0)
			a2=cwp_cexp(cmplx(0.0,-w*dx*0.5/vn));

		for(ix=0;ix<nx;ix++){
			a[ix]=cmplx(s1[ix],s2[ix]);
			b[ix]=cmplx(1.0-2.0*s1[ix],-2.0*s2[ix]);
		}

		for(ix=1;ix<nx-1;ix++){

			d[ix]=cadd(cadd(cmul(data[ix+1],a[ix+1]),
					cmul(data[ix-1],a[ix-1])),
					cmul(data[ix],b[ix]));
		}

		d[0]=cadd(cmul(cadd(b[0],cmul(a[0],a1)),
				data[0]),cmul(data[1],a[1]));

		d[nx-1]=cadd(cmul(cadd(b[nx-1],
			cmul(a[nx-1],a2)),data[nx-1]),
			cmul(data[nx-2],a[nx-2]));

		for(ix=0;ix<nx;ix++){
			data[ix]=cmplx(s1[ix],-s2[ix]);
			b[ix]=cmplx(1.0-2.0*s1[ix],2.0*s2[ix]);
		}
		endl=cadd(b[0],cmul(data[0],a1));
		endr=cadd(b[nx-1],cmul(data[nx-1],a2));

		
		for(ix=1;ix<nx-1;ix++){
			a[ix]=data[ix+1];
			c[ix]=data[ix-1];
		}
		a[0]=data[1];
		c[nx-1]=data[nx-2];
			
		retris(data,a,c,b,endl,endr,nx,d);

		for(ix=0;ix<nx;ix++){
			cp[iw][ix]=data[ix];
		}

	}


	free1complex(data);
	free1float(p);
	free1complex(d);
	free1complex(b);
	free1complex(c);
	free1complex(a);
	free1float(s1);
	free1float(s2);
		
	return;
}
		 

void retris(complex *data,complex *a,complex *c, complex *b,
		complex endl,complex endr, int nx, complex *d)
{
		 
	int ix;
	complex *e,den;
	complex *f;

	e=alloc1complex(nx);
	f=alloc1complex(nx);
	e[0]=cdiv(cneg(a[0]),endl);
	f[0]=cdiv(d[0],endl);

	for(ix=1;ix<nx-1;++ix){
		den=cadd(b[ix],cmul(c[ix],e[ix-1]));
		e[ix]=cdiv(cneg(a[ix]),den);
		f[ix]=cdiv(csub(d[ix],cmul(f[ix-1],c[ix])),den);
	}
		 

	data[nx-1]=cdiv(csub(d[nx-1],cmul(f[nx-2],c[nx-2])),
			cadd(endr,cmul(c[nx-2],e[nx-2])));
		
	for(ix=nx-2;ix>-1;--ix)
	data[ix]=cadd(cmul(data[ix+1],e[ix]),f[ix]);

	free1complex(e);
	free1complex(f);
	return;  
}

void get_sx_gx(float *sx, float *gx)
{ 
/*****************************************************************************
get_sx_gx - get sx and gx from headrs
*****************************************************************************/

	float sy;		/* source coordinates */
	float gy;		/* geophone coordinates */

	if (tr.scalco) { /* if tr.scalco is set, apply value */
		if (tr.scalco>0) {
			*sx = (float) tr.sx*tr.scalco;
			*gx = (float) tr.gx*tr.scalco;
			sy = (float) tr.sy*tr.scalco;
			gy = (float) tr.gy*tr.scalco;
		} else { /* if tr.scalco is negative divide */
			*sx = (float) tr.sx/ABS(tr.scalco);
			*gx = (float) tr.gx/ABS(tr.scalco);
			sy = (float) tr.sy/ABS(tr.scalco);
			gy = (float) tr.gy/ABS(tr.scalco);
			}

		} else {
				*sx = (float) tr.sx;
				*gx = (float) tr.gx;
				sy = (float) tr.sy;
				gy = (float) tr.gy;
	}

	
	/* use pythagorean theorem to remap radial direction */
	/* to x-direction */
	*sx = SGN(*sx-sy)*sqrt((*sx)*(*sx) + sy*sy);
	*gx = SGN(*gx-gy)*sqrt((*gx)*(*gx) + gy*gy);

	return;
}

