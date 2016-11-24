/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMIGFD: $Revision: 1.13 $ ; $Date: 2015/08/07 22:19:43 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUMIGFD - 45-90 degree Finite difference depth migration for		",
"           zero-offset data.						",
"									",
"   sumigfd <infile >outfile vfile= [optional parameters]		",
"									",
" Required Parameters:							",
" nz=		number of depth sapmles					",
" dz=		depth sampling interval					",
" vfile=	name of file containing velocities			",
" 		(see Notes below concerning format of this file)	",
"									",
" Optional Parameters:							",
" dt=from header(dt) or .004    time sampling interval			",
" dx=from header(d2) or 1.0	midpoint sampling interval		",
" dip=45,65,79,80,87,89,90  	Maximum angle of dip reflector		",
"									",
" tmpdir=	if non-empty, use the value as a directory path		",
"		prefix for storing temporary files; else if the		",
"		the CWP_TMPDIR environment variable is set use		",
"		its value for the path; else use tmpfile()		",
"									", 
" Notes:								", 
" The computation cost by dip angle is 45=65=79<80<87<89<90		",
"									", 
" The input velocity file \'vfile\' consists of C-style binary floats.	", 
" The structure of this file is vfile[iz][ix]. Note that this means that",
" the x-direction is the fastest direction instead of z-direction! Such a",
" structure is more convenient for the downward continuation type	",
" migration algorithm than using z as fastest dimension as in other SU	",
" programs. (In C  v[iz][ix] denotes a v(x,z) array, whereas v[ix][iz]  ",
" denotes a v(z,x) array, the opposite of what Matlab and Fortran	",
" programmers may expect.)						", 
"									", 
" Because most of the tools in the SU package (such as  unif2, unisam2,	",
" and makevel) produce output with the structure vfile[ix][iz], you will",
" need to transpose the velocity files created by these programs. You may",
" use the SU program \'transp\' in SU to transpose such files into the	",
" required vfile[iz][ix] structure.					",
"									",
NULL};

/* 
 * Credits: CWP Baoniu Han, April 20th, 1998
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
char tmp;

/* Prototypes of functions used internally */
static void closefiles(void);
void retris(complex *data,complex *a,complex *c,complex *b,complex
		endl,complex endr, int nx, complex *d);

void fdmig( complex **cp, int nx, int nw, float *v,float fw,float  
		dw,float dz,float dx,float dt,int dip);

int
main (int argc, char **argv)
{
	int nt;			/* number of time samples		*/
	int nz;			/* number of migrated depth samples	*/
	int nx;			/* number of midpoints			*/
	int iz,iw,ix;		/* loop counters			*/
	int ntfft;		/* fft size				*/
	int nw;			/* number of wave numbers		*/	
	int dip;		/* maximum dip angle			*/

	float dt,dz,tz=0.0;	/* sampling interval			*/
	float dw;		/* wave number sampling interval	*/
	float fw;		/* first wave number			*/
	float w;		/* wave number				*/
	float dx;		/* spatial sampling interval		*/
	float **p=NULL;		/* input data				*/
	float **cresult=NULL;	/* output data				*/

	double kz2;
	float **v=NULL;
	complex cshift2;
	complex **cp=NULL;	/* complex input,output			*/
	char *vfile="";		/* name of file containing velocities	*/
	FILE *vfp=NULL;
	int verbose=1;		/* flag for echoing info		*/
	char *tmpdir=NULL;	/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* cwp_true for user-given path	*/

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
	if (!getparint("nz",&nz)) err("nz must be specified");
	if (!getparfloat("dz",&dz)) err("dz must be specified");
	if (!getparstring("vfile", &vfile)) err("vfile must be specified");
	if(!getparint("verbose",&verbose)) verbose = 0;
	if(!getparint("dip",&dip)) dip=65;

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);
        checkpars();

	/* store traces and headers in tempfiles while getting a count */
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

	/* allocate space */
	p = alloc2float(ntfft,nx);
	cp = alloc2complex(nw,nx);
	cresult = alloc2float(nz,nx);
	v=alloc2float(nx,nz);

	/* Zero all arrays */
	memset((void *) p[0], 0, FSIZE*ntfft*nx);
	memset((void *) cp[0], 0, sizeof(complex)*nw*nx);
	memset((void *) cresult[0], 0, FSIZE*nz*nx);
	memset((void *) v[0], 0, FSIZE*nz*nx);

	/* load traces into the zero-offset array and close tmpfile */
	for(ix=0;ix<nx;ix++)
		efread(p[ix], FSIZE, nt, tracefp);

	efclose(tracefp);

	/* load velocity file */
	vfp=efopen(vfile,"r");	
	efread(v[0],FSIZE,nz*nx,vfp);
	efclose(vfp);			

	/* Fourier transform */
	pfa2rc(1,1,ntfft,nx,p[0],cp[0]);

	/* loop over depth*/
	for(iz=0;iz<nz;++iz,tz+=dz){
		for(ix=0;ix<nx;ix++){
			cresult[ix][iz] =0.0;
			v[iz][ix]=v[iz][ix]/2.0;
			for(iw=1;iw<nw;iw++)
			cresult[ix][iz]+=cp[ix][iw].r/ntfft;
		}


		for(ix=0;ix<nx;++ix)  
		for(iw=1,w=fw+dw;iw<nw;w+=dw,++iw) {
			kz2=-1.0/v[iz][ix]*w*dz;
			cshift2=cmplx(cos(kz2),sin(kz2));
			cp[ix][iw]=cmul(cp[ix][iw],cshift2);
		}

		fdmig( cp, nx, nw,v[iz],fw,dw,dz,dx,dt,dip);

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

/* Functions used internally */

static void closefiles(void)
{
	efclose(headerfp);
	efclose(tracefp);
	eremove(headerfile);
	eremove(tracefile);
	exit(EXIT_FAILURE);
}



void fdmig( complex **cp, int nx, int nw, float *v,float fw,float
	dw,float dz,float dx,float dt,int dip)
{
	int iw,ix,step=1;
	float *s1,*s2,w,coefa[5],coefb[5],v1,vn,trick=0.1;
	complex cp2,cp3,cpnm1,cpnm2;
	complex a1,a2,b1,b2;
	complex endl,endr;
	complex *data,*d,*a,*b,*c;

	s1=alloc1float(nx);
	s2=alloc1float(nx);

	data=alloc1complex(nx);
	d=alloc1complex(nx);
	a=alloc1complex(nx);
	b=alloc1complex(nx);
	c=alloc1complex(nx);

	if(dip!=45&&dip!=65&&dip!=79&&dip!=80&&dip!=87&&dip!=89&&dip!=90)
	dip=79;


	if(dip==45){
	coefa[0]=0.5;coefb[0]=0.25;
	step=1;
	}

	if(dip==65){
	coefa[0]=0.478242060;coefb[0]=0.376369527;
	step=1;
	}

	if(dip==79){
	coefa[0]=coefb[0]=0.4575;
	step=1;
	}
	
	if(dip==80){
	coefa[1]=0.040315157;coefb[1]=0.873981642;
	coefa[0]=0.457289566;coefb[0]=0.222691983;
	step=2;
	}

	if(dip==87){
	coefa[2]=0.00421042;coefb[2]=0.972926132;
	coefa[1]=0.081312882;coefb[1]=0.744418059;
	coefa[0]=0.414236605;coefb[0]=0.150843924;
	step=3;
	}

	if(dip==89){
	coefa[3]=0.000523275;coefb[3]=0.994065088;
	coefa[2]=0.014853510;coefb[2]=0.919432661;
	coefa[1]=0.117592008;coefb[1]=0.614520676;
	coefa[0]=0.367013245;coefb[0]=0.105756624;
	step=4;
	}

	if(dip==90){
	coefa[4]=0.000153427;coefb[4]=0.997370236;
	coefa[3]=0.004172967;coefb[3]=0.964827992;
	coefa[2]=0.033860918;coefb[2]=0.824918565;
	coefa[1]=0.143798076;coefb[1]=0.483340757;
	coefa[0]=0.318013812;coefb[0]=0.073588213;
	step=5;
	}


	v1=v[0];vn=v[nx-1];
	 
loop:
	
step--;

	for(iw=0,w=fw;iw<nw;iw++,w+=dw){

		if(fabs(w)<=1.0e-10)w=1.0e-10/dt;
			
		for(ix=0;ix<nx;ix++){
			s1[ix]=(v[ix]*v[ix])*coefb[step]/(dx*dx*w*w)+trick;
			s2[ix]=-v[ix]*dz*coefa[step]/(w*dx*dx)*0.5;
		}
		
		for(ix=0;ix<nx;ix++){
			data[ix]=cp[ix][iw];
		}

		cp2=data[0];
		cp3=data[1];
		cpnm1=data[nx-1];
		cpnm2=data[nx-2];
		a1=cmul(cp2,conjg(cp3));
/*
		b1=cadd(cmul(cp2,conjg(cp2)),cmul(cp3,conjg(cp3)));
 */
		b1=cmul(cp3,conjg(cp3));
		if(b1.r==0.0 && b1.i==0.0)

			a1=cwp_cexp(cmplx(0.0,-w*dx*0.5/v1));
		else
			a1=cdiv(a1,b1);
		 
		if(a1.i>0.0)a1=cwp_cexp(cmplx(0.0,-w*dx*0.5/v1));
		
		a2=cmul(cpnm1,conjg(cpnm2));
		b2=cmul(cpnm2,conjg(cpnm2));
	 
		if(b2.r==0.0 && b2.i==0.0)
			a2=cwp_cexp(cmplx(0.0,-w*dx*0.5/vn));
		else
			a2=cdiv(a2,b2);
		
		if(a2.i>0.0)a2=cwp_cexp(cmplx(0.0,-w*dx*0.5/vn));
  
		
		for(ix=0;ix<nx;ix++){
			a[ix]=cmplx(s1[ix],s2[ix]);
			b[ix]=cmplx(1.0-2.0*s1[ix],-2.0*s2[ix]);
		}
			
		for(ix=1;ix<nx-1;ix++){
		d[ix]=cadd(cadd(cmul(data[ix+1],a[ix+1]),cmul(data[ix-1],a[ix-1])),
		cmul(data[ix],b[ix]));
		}
		
		d[0]=cadd(cmul(cadd(b[0],cmul(a[0],a1)),data[0]),cmul(data[1],a[1]));
		d[nx-1]=cadd(cmul(cadd(b[nx-1],cmul(a[nx-1],a2)),data[nx-1]),
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
			cp[ix][iw]=data[ix];
		}
	
	}

if(step) goto loop;
	
	free1complex(data);
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

	data[nx-1]=cdiv(csub(d[nx-1],cmul(f[nx-2],c[nx-2])),cadd(endr,cmul(c[nx-2],e[nx-2])));
 
	for(ix=nx-2;ix>-1;--ix)
		data[ix]=cadd(cmul(data[ix+1],e[ix]),f[ix]);

	free1complex(e);
	free1complex(f);
	return;
}

