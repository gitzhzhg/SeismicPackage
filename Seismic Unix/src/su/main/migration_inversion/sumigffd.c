/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPSMIGFFD: $Revision: 1.9 $ ; $Date: 2015/08/07 22:19:43 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUMIGFFD - Fourier finite difference depth migration for		",
"	    zero-offset data. This method is a hybrid migration which	",
"	    combines the advantages of phase shift and finite difference", 
"	    migrations.							",
"									",
" sumigffd <infile >outfile vfile= [optional parameters]		",
"									",
" Required Parameters:						  	",
" nz=		   number of depth sapmles			 	", 
" dz=		   depth sampling interval			 	",
" vfile=		name of file containing velocities	      	",
"									",
" Optional Parameters:						  	",
" dt=from header(dt) or .004    time sampling interval		  	",
" dx=from header(d2) or 1.0     midpoint sampling interval	  	",
" ft=0.0			first time sample			",
" fz=0.0			first depth sample		      	",
"									",
" tmpdir=	if non-empty, use the value as a directory path		",
"		prefix for storing temporary files; else if the		",
"		the CWP_TMPDIR environment variable is set use		",
"		its value for the path; else use tmpfile()		",
"									", 
" The input velocity file \'vfile\' consists of C-style binary floats.  ",  
" The structure of this file is vfile[iz][ix]. Note that this means that",
" the x-direction is the fastest direction instead of z-direction! Such a",
" structure is more convenient for the downward continuation type	",
" migration algorithm than using z as fastest dimension as in other SU  ", 
" programs. (In C  v[iz][ix] denotes a v(x,z) array, whereas v[ix][iz]  ",
" denotes a v(z,x) array, the opposite of what Matlab and Fortran	",
" programmers may expect.)						", 
"									",
" Because most of the tools in the SU package (such as  unif2, unisam2, ", 
" and makevel) produce output with the structure vfile[ix][iz], you will",
" need to transpose the velocity files created by these programs. You may",
" use the SU program \'transp\' in SU to transpose such files into the  ",
" required vfile[iz][ix] structure.					",
"									",
"									",
NULL};

/*
 * Credits: CWP Baoniu Han, July 21th, 1997
 *
 *
 * Trace header fields accessed: ns, dt, delrt, d2
 * Trace header fields modified: ns, dt, delrt
 */
/**************** end self doc *******************************************/



/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *tracefp;		/* fp for trace storage file		*/
FILE *headerfp;		/* fp for header storage file		*/
segy tr;

static void closefiles(void);

void retris(complex *data,complex *a,complex *c,complex *b,complex
endl,complex endr, int nx, complex *d);

void fdmig( complex **cp, int nx, int nw, float *v,float fw,float  
dw,float dz,float dx,float dt,float vc);


	int main (int argc, char **argv)
{
	int nt;			/* number of time samples */
	int nz;			/* number of migrated depth samples */
	int nx;			/* number of midpoints 	*/
	int ik,iz,iw,ix,it;	/* loop counters 	*/
	int nxfft,ntfft;	/* fft size		*/
	int nk,nw;		/* number of wave numbers */	


	float dt,dz,tz;		/* sampling interval 	*/
	float ft,fz;		/* first sample		*/
	float dk,dw;		/* wave number sampling interval */
	float fk,fw;		/* first wave number 		*/
	float k,w;		/* time,wave number		*/
	float dx;		/* spatial sampling interval	*/
	float **p,**cresult;	/* input, output data		*/
	float vmin,vmax,v1;
	double kz1,kz2;
	float **v;
	double phase1;
	complex cshift1,cshift2;
	complex **cp,**cq;	/* complex input,output		*/
	char *vfile="";		/* name of file containing velocities */
	FILE *vfp;
	int verbose=1;		/* flag for echoing info		*/
	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* cwp_true for user-given path		*/

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);

	/* get info from first trace */
	if (!gettr(&tr))  err("can't get first trace");
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


	/* get optional parameters */
	if (!getparfloat("ft",&ft)) ft = 0.0;
	if (!getparint("nz",&nz)) err("nz must be specified");
	if (!getparfloat("dz",&dz)) err("dz must be specified");
	if (!getparfloat("fz",&fz)) fz = 0.0;
	if (!getparstring("vfile", &vfile)) err("vfile must be specified");
	if(!getparint("verbose",&verbose)) verbose = 0;

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);
	/* store traces and headers in tempfiles while getting a count */
        checkpars();

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

	nx = 0;
	do {
		 ++nx;
		efwrite(&tr,HDRBYTES,1,headerfp);
		efwrite(tr.data, FSIZE, nt, tracefp);
	} while (gettr(&tr));
	erewind(tracefp);
	erewind(headerfp);
	
	/* determine frequency sampling (for real to complex FFT) */
	ntfft = npfar(nt);
	nw = ntfft/2+1;
	dw = 2.0*PI/(ntfft*dt);
	fw = 0.0;
	
	/* determine wavenumber sampling (for complex to complex FFT) */
	nxfft = npfa(nx);
	nk = nxfft;
	dk = 2.0*PI/(nxfft*dx);
	fk = -PI/dx;


	/* allocate space */
	p = alloc2float(ntfft,nx);
	cp = alloc2complex(nw,nx);
	cq = alloc2complex(nw,nk);
	cresult = alloc2float(nz,nx);
	v=alloc2float(nx,nz);

	/* load traces into the zero-offset array and close tmpfile */
	for(ix=0;ix<nx;ix++)
	efread(p[ix], FSIZE, nt, tracefp);
	efclose(tracefp);

	/*load velicoty file*/
	vfp=efopen(vfile,"r");	
	efread(v[0],FSIZE,nz*nx,vfp);
	efclose(vfp);			
	/* pad with zeros and Fourier transform t to w */
	

	for (ix=0; ix<nx; ix++)
		for (it=nt; it<ntfft; it++)
			p[ix][it] = 0.0;

	pfa2rc(1,1,ntfft,nx,p[0],cp[0]);

	/* loop over depth*/
	for(iz=0,tz=fz;iz<nz;++iz,tz+=dz){

	for(ix=0;ix<nx;ix++){
	cresult[ix][iz] =0.0;
	for(iw=1;iw<nw;iw++)
	cresult[ix][iz]+=cp[ix][iw].r/ntfft;
	}

	vmax=vmin=v[iz][0];

	for(ix=0;ix<nx;ix++){ 
	if(v[iz][ix]>=vmax) vmax=v[iz][ix];
	if(v[iz][ix]<=vmin) vmin=v[iz][ix];
	}

	for (ik=0; ik<nx; ++ik)
	for (iw=0; iw<nw; ++iw) 
	cq[ik][iw] = ik%2 ? cneg(cp[ik][iw]) : cp[ik][iw];

	for (ik=nx; ik<nk; ++ik)
	for (iw=0; iw<nw; ++iw)
	cq[ik][iw] = cmplx(0.0,0.0);


	/* FFT to W-K domain */

	pfa2cc(-1,2,nw,nk,cq[0]);

	v1=vmin*0.5;

	for(ik=0,k=fk;ik<nk;++ik,k+=dk)
		for(iw=1,w=fw+dw;iw<nw;++iw,w+=dw){
		if(w==0.0)w=1.0e-10/dt;
		kz1=1.0-pow(v1*k/w,2.0);
		if(kz1>0){
		phase1 = -w*sqrt(kz1)*dz/v1; 
		cshift1 = cmplx(cos(phase1), sin(phase1));
		cq[ik][iw] = cmul(cq[ik][iw],cshift1);
		}
	else {
		cshift1=cmplx(0.0,0.0);
		cq[ik][iw] = cmul(cq[ik][iw],cshift1);
		}
	}


	pfa2cc(1,2,nw,nk,cq[0]);
	for(ix=0;ix<nx;++ix)
	for(iw=1,w=fw+dw;iw<nw;w+=dw,++iw){
	cq[ix][iw] = crmul( cq[ix][iw], 1.0/nxfft);
	cp[ix][iw] =ix%2 ? cneg(cq[ix][iw]) : cq[ix][iw];
	}


	for(ix=0;ix<nx;++ix)  
		for(iw=1,w=fw+dw;iw<nw;w+=dw,++iw){
		kz2=(1.0/v1-2.0/v[iz][ix])*w*dz;
		cshift2=cmplx(cos(kz2),sin(kz2));
		cp[ix][iw]=cmul(cp[ix][iw],cshift2);
	}

	fdmig( cp, nx, nw,v[iz],fw,dw,dz,dx,dt,v1);

	}


	/* restore header fields and write output */
	for(ix=0; ix<nx; ix++) {

	efread(&tr,HDRBYTES,1,headerfp);
        tr.ns = nz ;
        tr.d1 = dz ;

	memcpy( (void *) tr.data, (const void *) cresult[ix],nz*FSIZE);
	puttr(&tr);
	}
	
	/* Clean up */
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	if (istmpdir) eremove(tracefile);

	return(CWP_Exit());	

}

	/*Following are functions called in main() */
	static void closefiles(void)
{
	efclose(headerfp);
	efclose(tracefp);
	eremove(headerfile);
	eremove(tracefile);
	exit(EXIT_FAILURE);
}

	void fdmig( complex **cp, int nx, int nw, float *v,float fw,float
	dw,float dz,float dx,float dt,float vc) 
{
	int iw,ix;
	float *p,*s1,*s2,w,a0=2.0,v1,vn,trick=0.14;
	complex endl,endr;
	complex cp2,cp3,cpnm1,cpnm2;
	complex a1,a2,b1,b2;
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
	v[ix]=v[ix];
	p[ix]=vc*2.0/v[ix];
	p[ix]=0.5*(p[ix]*p[ix]+p[ix]+1.0);
	}
	
	v1=v[0];vn=v[nx-1];

	for(iw=1,w=fw+dw;iw<nw;iw++,w+=dw){
	if(w==0)w=1.0e-10/dt;

	for(ix=0;ix<nx;ix++){
		s1[ix]=p[ix]*v[ix]*v[ix]/(4.0*a0*dx*dx*w*w)+trick;
		s2[ix]=(1-vc*2.0/v[ix])*v[ix]*dz*0.5/(2.0*w*a0*dx*dx);
	}

	for(ix=0;ix<nx;ix++){
		data[ix]=cp[ix][iw];
	}

	cp2=data[1];
	cp3=data[2];
	cpnm1=data[nx-2];
	cpnm2=data[nx-3];
	a1=crmul(cmul(cp2,conjg(cp3)),2.0);
	b1=cadd(cmul(cp2,conjg(cp2)),cmul(cp3,conjg(cp3)));
	
	if(b1.r==0.0&&b1.i==0.0)
	a1=cwp_cexp(cmplx(0.0,-w*dx*0.5/v1));
	else a1=cdiv(a1,b1);
 
	if(a1.i>0.0)a1=cwp_cexp(cmplx(0.0,-w*dx*0.5/v1));
	
	a2=crmul(cmul(cpnm1,conjg(cpnm2)),2.0);
	b2=cadd(cmul(cpnm1,conjg(cpnm1)),cmul(cpnm2,conjg(cpnm2)));

	if(b2.r==0.0&&b2.i==0.0)
	a2=cwp_cexp(cmplx(0.0,-w*dx*0.5/vn));
	else a2=cdiv(a2,b2);
	if(a2.i>0.0)a2=cwp_cexp(cmplx(0.0,-w*dx*0.5/vn));


	for(ix=0;ix<nx;ix++){
		a[ix]=cmplx(s1[ix],-s2[ix]);
		b[ix]=cmplx(1.0-2.0*s1[ix],2.0*s2[ix]);	
	}


	d[0]=cadd(cmul(cadd(b[0],cmul(a[0],a1)),data[0]),cmul(data[1],a[1]));
	d[nx-1]=cadd(cmul(cadd(b[nx-1],cmul(a[nx-1],a2)),data[nx-1]),cmul(data[nx-2],a[nx-2]));

	for(ix=1;ix<nx-1;ix++){
	d[ix]=cadd(cadd(cmul(data[ix+1],a[ix+1]),cmul(data[ix-1],a[ix-1])),cmul(data[ix],b[ix]));
	}

	for(ix=0;ix<nx;ix++){
	data[ix]=cmplx(s1[ix],s2[ix]);
	b[ix]=cmplx(1.0-2.0*s1[ix],-2.0*s2[ix]);
	}

	endl=cadd(b[0],cmul(data[0],a1));
	endr=cadd(b[nx-1],cmul(data[nx-1],a2));

	for(ix=1;ix<nx-1;ix++) {
		a[ix]=data[ix+1];
		c[ix]=data[ix-1];
	}	
	a[0]=data[1];
	c[nx-1]=data[nx-2];


	retris(data,a,c,b,endl,endr,nx,d);

	for(ix=0;ix<nx;ix++){
		cp[ix][iw]=data[ix];
	}

	}	

	free1complex(data);
	free1complex(d);
	free1float(p);
	free1complex(b);
	free1complex(a);
	free1float(s1);
	free1float(s2);
	return;
}
 

	void retris(complex *data,complex *a,complex *c,complex *b,complex endl,complex 
	endr, int nx, complex *d)
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

	data[nx-1]=cdiv(csub(d[nx-1],cmul(f[nx-2],c[nx-1])),cadd(endr,cmul(c[nx-1],e[nx-2])));
 
	for(ix=nx-2;ix>-1;--ix)
	data[ix]=cadd(cmul(data[ix+1],e[ix]),f[ix]);

	free1complex(e);
	free1complex(f);
	return;
}

