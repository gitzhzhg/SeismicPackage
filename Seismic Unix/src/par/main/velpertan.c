/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "par.h"
#define EPS_smooth FLT_MIN

/*********************** self documentation **********************/
char *sdoc[]={
"									",
" VELPERTAN - Velocity PERTerbation analysis in ANisotropic media to    ",
"             determine the model update required to flatten image gathers",  
"									",
" velpertan boundary= par=cig.par refl1= refl2= npicks1= 		",
"	npicks2= cdp1= cdp2= vfile= efile= dfile= nx= dx= fx= 		",
"	ncdp= dcdp= fcdp= off0= noff= doff= >outfile [parameters]	",
"									",
" Required Parameters:							",
" refl1=	file with picks on the 1st reflector	  		",
" refl2=	file with picks on the 2nd reflector  			",
" vfile=	file defining VP0 at all grid points from prev. iter.	", 
" efile=	file defining eps at all grid points from prev. iter.	", 
" dfile=	file defining del at all grid points from prev. iter.	", 
" boundary=	file defining the boundary above which 			",
"        	parameters are known; update is done below this		", 
"		boundary						", 
" npicks1=	number of picks on the 1st reflector			",
" npicks2=	number of picks on the 2nd reflector			",
" ncdp=		number of cdp's		 				",
" dcdp=		cdp spacing			 			",
" fcdp=		first cdp			 			",
" off0=		first offset in common image gathers 			",
" noff=		number of offsets in common image gathers  		",
" doff=		offset increment in common image gathers  		",
" cip1=x1,r1,r2,..., cip=xn,r1n,r2n         description of input CIGS	",
" cip2=x2,r1,r2,..., cip=xn,r1n,r2n         description of input CIGS	",
"	x	x-value of a common image point				",
"	r1	hyperbolic component of the residual moveout		",
"	r2	non-hyperbolic component of residual moveout		",
" Optional Parameters:							",
" method=akima         for linear interpolation of the interface       ",
"                       =mono for monotonic cubic interpolation of interface",
"                       =akima for Akima's cubic interpolation of interface ",
"                       =spline for cubic spline interpolation of interface ",
" VP0=2000	Starting value for vertical velocity at a point in the   ",
"							target layer	",
" x00=0.0	x-coordinate at which VP0 is defined			",
" z00=0.0	z-coordinate at which VP0 is defined			",
" eps=0.0	Starting value for Thomsen's parameter epsilon		",
" del=0.0	Starting value for Thomsen's parameter delta		",
" kz=0.0	Starting value for the vertical gradient in VP0		",
" kx=0.0	Starting value for the lateral gradient in VP0		",
" nx=100	number of nodes in the horizontal direction for the     ",
"							velocity grid 	",
" nz=100	number of nodes in the vertical direction for the	",
"							velocity grid	",
" dx=10	horizontal grid increment				",
" dz=10	vertical grid increment					",
" fx=0		first horizontal grid point				",
" fz=0		first vertical grid point				",
" dt=0.008	traveltime increment					",
" nt=500	no. of points on the ray				",
" amax=360	max. angle of emergence					",
" amin=0	min. angle of emergence					",
"									",
" Smoothing parameters:							",
" r1=0                  smoothing parameter in the 1 direction          ",
" r2=0                  smoothing parameter in the 2 direction          ",
" win=0,n1,0,n2         array for window range                          ",
" rw=0                  smoothing parameter for window function         ",
" nbound=2	number of points picked on the boundary			",
" tol=0.1	tolerance in computing the offset (m)			",
" Notes:								",
" This program is used as part of the velocity analysis technique developed",
" by Debashish Sarkar, CWP:2003.					",
"									",
" Notes:								",
" The output par file contains the coefficients describing the residual ",
" moveout. This program is used in conjunction with surelanan.		",
"									",
NULL};
/*
 * Author: CSM: Debashish Sarkar, December 2003 
 * based on program: velpert.c written by Zhenuye Liu.
 */

/**************** end self doc ***********************************/

/* functions defined and used internally */
/* one step along ray */
typedef struct RayStepStruct {
        float t;                /* time */
        float x,z;              /* x,z coordinates */
        float q1,p1,q2,p2;      /* Cerveny's dynamic ray tracing solution */
        int kmah;               /* KMAH index */
        float c,s;              /* cos(angle) and sin(angle) */
        float v,dvdx,dvdz;      /* velocity and its derivatives */
} RayStep;

/* one ray */
typedef struct RayStruct {
        int nrs;                /* number of ray steps */
        RayStep *rs;            /* array[nrs] of ray steps */
        int nc;                 /* number of circles */
        int ic;                 /* index of circle containing nearest step */
        void *c;                /* array[nc] of circles */
} Ray;

void interpolate(int ncdp, float *xcdp, float *xint, float *zint, float *zcdp, float *zdcdp, int np, char *method);
float velpertan_time(float x00,float z00,float *xxbound,float *zzbound,float x,float z,
	float zd,float off,float VP0,float eps,float del,float kz,float kx,
	int nx,int nz, float fx,float dx,float fz,float dz,int nt,float dt,
	float amin, float amax,float tol,float *p,float *vp,float *e,float *d,
	float r1,float r2,int *win,float rw);

/* functions (velocity interpolation)*/
void* vel2Alloc (int nx, float dx, float fx,
        int nz, float dz, float fz, float **v);
void vel2Free (void *vel2);
void vel2Interp (void *vel2, float x, float z,
        float *v, float *vx, float *vz, float *vxx, float *vxz, float *vzz);

Ray *makeRay (float x0, float z0, float a0, int nt, float dt,
	float **a1111xz, float **a3333xz, float **a1133xz, float **a1313xz, 
	float **a1113xz, float **a3313xz, 
	int nx, float dx, float fx, int nz, float dz, float fz, float amax, 
	float amin);
float zbrentou(float da,float da_2,float tol,float off,float x, float z,
	float a_normal,int nt,float dt,float **a1111,
	float **a3333,float **a1133, float **a1313,float **a1113,
	float **a3313,int nx,float dx,float fx,int nz,float dz,
	float fz,float amax,float amin);
void freeRay (Ray *ray);
void conjugate_gradient(float **A, float *x, float *b, int nrx, float tol);
void smooth2 (int n1, int n2, float r1, float r2, int *win, float rw, float **v);

int main (int argc, char **argv)
{
	int npicks1,npicks2,ipicks,ncip1,icdp,win[4];
	int ncdp,noff,ioff,nx,nz,nt,ipar1,ipar2,nbound;
	float dcdp,fcdp,doff,off0,off,VP0,kz,kx,eps,del,dt,p;
	float fx,fz,dx,dz,amin,amax,tol,x00,z00;
	float r1,r2,rw;
	float *xcdp, *zcdp1, *zcdp2, *zdcdp1, *zdcdp2;
	float *r11, *r12, *r21, *r22, *x, *xbound, *zbound;
	float *xxbound, *zzbound, *zdbound;
	float *xcip, *r11cip, *r12cip, *r21cip, *r22cip, temp[3];
	float ***t, ***tkz, ***tkx, ***td, *b1, *b2, *b, **A1, **A2, **A;
	float ***dt1, ***dt2, **zz1, **zz2, ***pp, ***tdV, ***tde;
	float **dt1avg,**dt2avg,*zz1avg,*zz2avg,*vp,*e,*d;
	float sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12;
	char *refl1,*refl2,*vfile,*efile,*dfile;
	float *zint1,*xint1,*zint2,*xint2,*norm;
	char *method="akima";
	char *boundary="boundary";
	FILE *rf1,*rf2,*bound,*vf,*ef,*df;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* get required parameters */
	if (!getparint("ncdp",&ncdp)) err("must specify ncdp!\n");
	if (!getparfloat("dcdp",&dcdp)) err("must specify dcdp!\n");
	if (!getparfloat("fcdp",&fcdp)) err("must specify fcdp!\n");
	if (!getparint("npicks1",&npicks1)) err("must specify npicks1!\n");
	if (!getparint("npicks2",&npicks2)) err("must specify npicks2!\n");
	if (!getparfloat("off0",&off0)) err("must specify off0!\n");
	if (!getparfloat("doff",&doff)) err("must specify doff!\n");
	if (!getparint("noff",&noff)) err("must specify noff!\n");
	if (!getparstring("refl1",&refl1)) err("must specify reflector 1!\n");
	if (!getparstring("refl2",&refl2)) err("must specify reflector 2!\n");
	if (!getparstring("vfile",&vfile)) err("must specify velocity file!\n");
	if (!getparstring("efile",&efile)) err("must specify epsilon file!\n");
	if (!getparstring("dfile",&dfile)) err("must specify delta file!\n");
	if (!getparstring("boundary",&boundary)) err("must specify boundary!\n");

	/* get optional parameters */
	getparstring("method",&method);
	if(!getparfloat("VP0",&VP0)) VP0=2000;
	if(!getparfloat("kz",&kz)) kz=0.0;
	if(!getparfloat("kx",&kx)) kx=0.0;
	if(!getparfloat("eps",&eps)) eps=0.0;
	if(!getparfloat("del",&del)) del=0.0;
	if(!getparint("nx",&nx)) nx=100;
	if(!getparint("nz",&nz)) nz=100;
	if(!getparfloat("dx",&dx)) dx=10;
	if(!getparfloat("dz",&dz)) dz=10;
	if(!getparfloat("fx",&fx)) fx=0;
	if(!getparfloat("fz",&fz)) fz=0;
	if(!getparfloat("dt",&dt)) dt=0.008;
	if(!getparint("nt",&nt)) nt=500;
	if(!getparfloat("amax",&amax)) amax=180.0;
	if(!getparfloat("amin",&amin)) amin=0.0;
	if(!getparfloat("tol",&tol)) tol=0.1;
	if(!getparint("nbound",&nbound)) nbound=2;
	if(!getparfloat("x00",&x00)) x00=0;
	if(!getparfloat("z00",&z00)) z00=0;
	if (!getparint("win",win)) {
                win[0] = 0;
                win[1] = nz;
                win[2] = 0;
                win[3] = nx;
                }
	if (!getparfloat("rw",&rw)) rw = 0;
	if (!getparfloat("r1",&r1)) r1 = 0;
	if (!getparfloat("r2",&r2)) r2 = 0;
	
	ncip1 = countparname("cip1");
        checkpars();

	if (ncip1<1) err("Number of CIGS must be greater 0!\n");

	rf1=fopen(refl1,"r");
	rf2=fopen(refl2,"r");
	bound=fopen(boundary,"r");
	vf=fopen(vfile,"r");
	ef=fopen(efile,"r");
	df=fopen(dfile,"r");

	/* allocate space */
	xcip = alloc1float(ncip1);
	r11cip = alloc1float(ncip1);
	r21cip = alloc1float(ncip1);
	r22cip = alloc1float(ncip1);
	r12cip = alloc1float(ncip1);
	xcdp = alloc1float(ncdp);
	zcdp1 = alloc1float(ncdp);
	zcdp2 = alloc1float(ncdp);
	zdcdp1 = alloc1float(ncdp);
	zdcdp2 = alloc1float(ncdp);
	r11 = alloc1float(ncdp);
	r12 = alloc1float(ncdp);
	r21 = alloc1float(ncdp);
	r22 = alloc1float(ncdp);
	zint1 = alloc1float(npicks1);
	xint1 = alloc1float(npicks1);
	zint2 = alloc1float(npicks2);
	xint2 = alloc1float(npicks2);
	t     = alloc3float(noff,ncdp,2);
	tkx    = alloc3float(noff,ncdp,2);
	tkz    = alloc3float(noff,ncdp,2);
	td    = alloc3float(noff,ncdp,2);
	tdV    = alloc3float(noff,ncdp,2);
	tde    = alloc3float(noff,ncdp,2);
	b1 = alloc1float(5); 
	b2 = alloc1float(5); 
	b = alloc1float(5); 
	A1 = alloc2float(5,5); 
	A2 = alloc2float(5,5); 
	A = alloc2float(5,5); 
	dt1 = alloc3float(noff,ncdp,5);
	dt2 = alloc3float(noff,ncdp,5);
	zz1 = alloc2float(noff,ncdp);
	zz2 = alloc2float(noff,ncdp);
	dt1avg = alloc2float(ncdp,5);
	dt2avg = alloc2float(ncdp,5);
	zz1avg = alloc1float(ncdp);
	zz2avg = alloc1float(ncdp);
	xbound = alloc1float(nbound);
	zbound = alloc1float(nbound);
	xxbound = alloc1float(nx);
	zzbound = alloc1float(nx);
	zdbound = alloc1float(nx);
	x = alloc1float(5);
	pp = alloc3float(noff,ncdp,2);
	vp = alloc1float(nx*nz);
	e = alloc1float(nx*nz);
	d = alloc1float(nx*nz);
	norm = alloc1float(5);
	
	memset((void *) pp[0][0], 0, FSIZE*noff*ncdp*2);
        memset((void *) dt1[0][0], 0, FSIZE*noff*ncdp*5);
        memset((void *) dt2[0][0], 0, FSIZE*noff*ncdp*5);
        memset((void *) t[0][0], 0, FSIZE*noff*ncdp*2);
        memset((void *) tkx[0][0],0, FSIZE*noff*ncdp*2);
        memset((void *) tkz[0][0], 0, FSIZE*noff*ncdp*2);
        memset((void *) td[0][0], 0, FSIZE*noff*ncdp*2);
        memset((void *) tdV[0][0], 0, FSIZE*noff*ncdp*2);
        memset((void *) tde[0][0], 0, FSIZE*noff*ncdp*2);
        memset((void *) dt1avg[0], 0, FSIZE*5*ncdp);
        memset((void *) dt2avg[0], 0, FSIZE*5*ncdp);
        memset((void *) zz1[0], 0, FSIZE*noff*ncdp);
        memset((void *) A1[0], 0, FSIZE*5*5);
        memset((void *) A2[0], 0, FSIZE*5*5);
        memset((void *) A[0], 0, FSIZE*5*5);
        memset((void *) b1, 0, FSIZE*5);
        memset((void *) b2, 0, FSIZE*5);
        memset((void *) b, 0, FSIZE*5);
        memset((void *) xbound, 0, FSIZE*nbound);
        memset((void *) zbound, 0, FSIZE*nbound);
        memset((void *) norm, 0, FSIZE*5);

	/* input velocity, epsilon, and delta fields */
	fread(vp,sizeof(float),nz*nx,vf);
	fread(e,sizeof(float),nz*nx,ef);
	fread(d,sizeof(float),nz*nx,df);

	for(icdp=0; icdp<ncip1; ++icdp){
		getnparfloat(icdp+1,"cip1",temp);
		xcip[icdp] = temp[0];
		r12cip[icdp] = temp[1];
		r11cip[icdp] = temp[2];
	}

	for(icdp=0; icdp<ncip1; ++icdp){
		getnparfloat(icdp+1,"cip2",temp);
		r22cip[icdp] = temp[1];
		r21cip[icdp] = temp[2];
	}
	/* Input picked interface 1 */
           for(ipicks=0;ipicks<npicks1;ipicks++)
                fscanf(rf1,"%f %f\n", &zint1[ipicks], &xint1[ipicks]);
	/* Input picked interface 2 */
           for(ipicks=0;ipicks<npicks2;ipicks++)
                fscanf(rf2,"%f %f\n", &zint2[ipicks], &xint2[ipicks]);

	/* input the boundary to demarcate the region to update */
	   for(ipicks=0;ipicks<nbound;ipicks++)
		 fscanf(bound,"%f %f\n",&zbound[ipicks],&xbound[ipicks]);
	/* interpolate the boundary to all grid positions: spline, akima, monotonic*/
	for(ipicks=0;ipicks<nx;ipicks++) xxbound[ipicks]=fx+ipicks*dx;
	interpolate(nx,xxbound,xbound,zbound,zzbound,zdbound,nbound,method);

	/* compute uniformly sampled cdp */
                 for(icdp=0; icdp<ncdp; ++icdp)
                        xcdp[icdp] = fcdp+icdp*dcdp;

	/* reflector interpolation: spline, akima, monotonic */
	interpolate(ncdp,xcdp,xint1,zint1,zcdp1,zdcdp1,npicks1,method);
	interpolate(ncdp,xcdp,xint2,zint2,zcdp2,zdcdp2,npicks2,method);

	/* linear interpolation for the residual moveout components */
	intlin(ncip1,xcip,r11cip,r11cip[0],r11cip[ncip1-1],ncdp,xcdp,r11);
	intlin(ncip1,xcip,r12cip,r12cip[0],r12cip[ncip1-1],ncdp,xcdp,r12);
	intlin(ncip1,xcip,r21cip,r21cip[0],r21cip[ncip1-1],ncdp,xcdp,r21);
	intlin(ncip1,xcip,r22cip,r22cip[0],r22cip[ncip1-1],ncdp,xcdp,r22);

	/* find true and perturbed times for each cdp and each offset */
	for(icdp=0;icdp<ncdp;icdp++){
		xcdp[icdp]=fcdp+dcdp*icdp;
		if(xcdp[icdp]>(fx+(nx-1)*dx)){
			warn("cdp exceeds grid dimensions\n"); break;
			}
			for(ioff=0;ioff<noff;ioff++){
			off = off0+doff*ioff; p=0;
		t[0][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,xcdp[icdp],
		zcdp1[icdp],zdcdp1[icdp],off,VP0,eps,del,kz,kx,nx,nz,fx,
		dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,r1,r2,win,rw);
		pp[0][icdp][ioff]=p; p=0;
		t[1][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,xcdp[icdp],
		zcdp2[icdp],zdcdp2[icdp],off,VP0,eps,del,kz,kx,nx,nz,fx,
		dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,r1,r2,win,rw); 
		pp[1][icdp][ioff]=p; p=0;

	       	td[0][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp1[icdp],zdcdp1[icdp],off,VP0,eps,del+0.05,
		kz,kx,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,
		r1,r2,win,rw); 
		td[1][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp2[icdp],zdcdp2[icdp],off,VP0,eps,del+0.05,
		kz,kx,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,
		r1,r2,win,rw);

	 	tkx[0][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp1[icdp],zdcdp1[icdp],off,VP0,eps,del,kz,
		kx+0.05,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,
		r1,r2,win,rw);
		tkx[1][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp2[icdp],zdcdp2[icdp],off,VP0,eps,del,kz,
		kx+0.05,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,
		r1,r2,win,rw);

	 	tkz[0][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp1[icdp],zdcdp1[icdp],off,VP0,eps,del,
		kz+0.05,kx,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,
		vp,e,d,r1,r2,win,rw);
		tkz[1][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp2[icdp],zdcdp2[icdp],off,VP0,eps,del,
		kz+0.05,kx,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,
		vp,e,d,r1,r2,win,rw);

	 	tde[0][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp1[icdp],zdcdp1[icdp],off,VP0,eps+0.05,del,
		kz,kx,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,
		r1,r2,win,rw);
		tde[1][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp2[icdp],zdcdp2[icdp],off,VP0,eps+0.05,del,
		kz,kx,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,
		r1,r2,win,rw);
/*
	 	tdV[0][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp1[icdp],zdcdp1[icdp],off,VP0+10,eps,del,
		kz,kx,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,
		r1,r2,win,rw);
		tdV[1][icdp][ioff]=velpertan_time(x00,z00,xxbound,zzbound,
		xcdp[icdp],zcdp2[icdp],zdcdp2[icdp],off,VP0+10,eps,del,
		kz,kx,nx,nz,fx,dx,fz,dz,nt,dt,amin,amax,tol,&p,vp,e,d,
		r1,r2,win,rw);
*/

			}
	}

	/*compute average of depths and their derivatives*/
	for(icdp=0;icdp<ncdp;icdp++){
	sum1=sum2=sum3=sum4=sum5=sum6=sum7=sum8=sum9=sum10=sum11=sum12=0.0;
		for(ioff=0;ioff<noff;ioff++){
/*
		dt1[0][icdp][ioff]=-pp[0][icdp][ioff]*
			((tdV[0][icdp][ioff]-t[0][icdp][ioff])/10);
		sum1 += dt1[0][icdp][ioff]/noff;
		dt2[0][icdp][ioff]=-pp[1][icdp][ioff]*
			((tdV[1][icdp][ioff]-t[1][icdp][ioff])/10);
		sum2 += dt2[0][icdp][ioff]/noff;
*/

		dt1[1][icdp][ioff]=-pp[0][icdp][ioff]*((tkx[0][icdp][ioff]-
			t[0][icdp][ioff])/0.05);
		sum3 += dt1[1][icdp][ioff]/noff;
		dt2[1][icdp][ioff]=-pp[1][icdp][ioff]*((tkx[1][icdp][ioff]-
			t[1][icdp][ioff])/0.05);
		sum4 += dt2[1][icdp][ioff]/noff;

		dt1[2][icdp][ioff]=-pp[0][icdp][ioff]*((tde[0][icdp][ioff]-
			t[0][icdp][ioff])/0.05);
		sum5 += dt1[2][icdp][ioff]/noff;
		dt2[2][icdp][ioff]=-pp[1][icdp][ioff]*((tde[1][icdp][ioff]-
			t[1][icdp][ioff])/0.05);
		sum6 += dt2[2][icdp][ioff]/noff;

		dt1[3][icdp][ioff]=-pp[0][icdp][ioff]*((td[0][icdp][ioff]-
			t[0][icdp][ioff])/0.05);
		sum7 += dt1[3][icdp][ioff]/noff;
		dt2[3][icdp][ioff]=-pp[1][icdp][ioff]*((td[1][icdp][ioff]-
			t[1][icdp][ioff])/0.05);
		sum8 += dt2[3][icdp][ioff]/noff;

		dt1[4][icdp][ioff]=-pp[0][icdp][ioff]*((tkz[0][icdp][ioff]-
			t[0][icdp][ioff])/0.05);
		sum9 += dt1[4][icdp][ioff]/noff;
		dt2[4][icdp][ioff]=-pp[1][icdp][ioff]*((tkz[1][icdp][ioff]-
			t[1][icdp][ioff])/0.05);
		sum10 += dt2[4][icdp][ioff]/noff;

		off=off0+ioff*doff;
		zz1[icdp][ioff]=sqrt(pow(zcdp1[icdp],2)
				+r11[icdp]*pow((off/2),2)+r12[icdp]*
				pow((off/2),4)/(pow((off/2),2)+pow(zcdp1[icdp],2)));

		sum11 += zz1[icdp][ioff]/noff;
		zz2[icdp][ioff]=sqrt(pow(zcdp2[icdp],2)
				+r21[icdp]*pow((off/2),2)+r22[icdp]*
				pow((off/2),4)/(pow((off/2),2)+pow(zcdp2[icdp],2)));
		sum12 += zz2[icdp][ioff]/noff;
		}
		dt1avg[0][icdp]=sum1;dt2avg[0][icdp]=sum2;
		dt1avg[1][icdp]=sum3;dt2avg[1][icdp]=sum4;
		dt1avg[2][icdp]=sum5;dt2avg[2][icdp]=sum6;
		dt1avg[3][icdp]=sum7;dt2avg[3][icdp]=sum8;
		dt1avg[4][icdp]=sum9;dt2avg[4][icdp]=sum10;
		zz1avg[icdp]=sum11;zz2avg[icdp]=sum12;	
	}

	for(ipar1=0;ipar1<5;ipar1++)
		for(ipar2=0;ipar2<5;ipar2++)
			for(icdp=0;icdp<ncdp;icdp++)
			for(ioff=0;ioff<noff;ioff++){
				A1[ipar1][ipar2] +=
				(dt1[ipar1][icdp][ioff]-dt1avg[ipar1][icdp])
				*(dt1[ipar2][icdp][ioff]-dt1avg[ipar2][icdp]);
			}
	for(ipar1=0;ipar1<5;ipar1++)
		for(ipar2=0;ipar2<5;ipar2++)
			for(icdp=0;icdp<ncdp;icdp++)
			for(ioff=0;ioff<noff;ioff++){
				A2[ipar1][ipar2] +=
				(dt2[ipar1][icdp][ioff]-dt2avg[ipar1][icdp])
				*(dt2[ipar2][icdp][ioff]-dt2avg[ipar2][icdp]);
			}
	
	for(ipar1=0;ipar1<5;ipar1++)
		for(icdp=0;icdp<ncdp;icdp++)
		for(ioff=0;ioff<noff;ioff++){
			b1[ipar1] +=
			(dt1[ipar1][icdp][ioff]-dt1avg[ipar1][icdp])
			*(zz1[icdp][ioff]-zz1avg[icdp]);
		}
		
	for(ipar1=0;ipar1<5;ipar1++)
		for(icdp=0;icdp<ncdp;icdp++)
		for(ioff=0;ioff<noff;ioff++){
			b2[ipar1] +=
			(dt2[ipar1][icdp][ioff]-dt2avg[ipar1][icdp])
			*(zz2[icdp][ioff]-zz2avg[icdp]);
		}
	
	/*compute A and b*/
	printf("Matrix A\n");
	for(ipar1=0;ipar1<5;ipar1++){
		for(ipar2=0;ipar2<5;ipar2++){
			A[ipar1][ipar2]=A1[ipar1][ipar2]+A2[ipar1][ipar2];
			printf("%f \t",A[ipar1][ipar2]);
		}
	printf("\n \n");
	b[ipar1]=-b1[ipar1]-b2[ipar1];
	}

	printf("Vector b\n");
	printf("%f %f %f %f %f\n",b[0],b[1],b[2],b[3],b[4]);
	conjugate_gradient(A,x,b,5,0.0001);

	printf("\n \n");
		printf("d_VP0=%e d_kz=%e d_kx=%e d_epsilon=%e d_delta=%e\n",x[0],x[4],x[1],x[2],x[3]);
	printf("\n \n");
	/* deallocate space */
        free1float(xcip);
        free1float(r11cip);
        free1float(r12cip);
        free1float(r21cip);
        free1float(r22cip);
        free1float(xcdp);
        free1float(zcdp1);
        free1float(zcdp2);
        free1float(zdcdp1);
        free1float(zdcdp2);
        free1float(r12);
        free1float(r22);
        free1float(r21);
        free1float(r11);
        free1float(zint1);
        free1float(xint1);
        free1float(zint2);
        free1float(xint2);
	free3float(t);
	free3float(tkz);
	free3float(tkx);
	free3float(td);

	return EXIT_SUCCESS;
}

/******************************************************************************/
/*****  interpolate depths and compute slope of the reflector at each cdp *****/
/******************************************************************************/
void interpolate (int ncdp, float *xcdp, float *xint, float *zint, float *zcdp, float *zdcdp, int np, char *method)
{
float (*zind)[4];

                /* else, if monotonic interpolation */
                if (method[0]=='m') {
                                zind = (float (*)[4])ealloc1float(np*4);
                                cmonot(np,xint,zint,zind);
                                intcub(0,np,xint,zind,ncdp,xcdp,zcdp);
                                intcub(1,np,xint,zind,ncdp,xcdp,zdcdp);

                /* else, if Akima interpolation */
                } else if (method[0]=='a') {
                                zind = (float (*)[4])ealloc1float(np*4);
                                cakima(np,xint,zint,zind);
                                intcub(0,np,xint,zind,ncdp,xcdp,zcdp);
                                intcub(1,np,xint,zind,ncdp,xcdp,zdcdp);

                /* else, if cubic spline interpolation */
                } else if (method[0]=='s') {
                                zind = (float (*)[4])ealloc1float(np*4);
                                csplin(np,xint,zint,zind);
                                intcub(0,np,xint,zind,ncdp,xcdp,zcdp);
                                intcub(1,np,xint,zind,ncdp,xcdp,zdcdp);
				}
}

/*****************************************************************************/
/*************** Compute time for a given offset and cdp point ***************/
/*****************************************************************************/
float velpertan_time(float x00,float z00,float *xxbound,float *zzbound,float x,float z,
	float zd,float off,float VP0,float eps,float del,float kz,float kx,
	int nx,int nz,float fx,float dx,float fz,float dz,int nt,float dt,
	float amin, float amax,float tol,float *p,float *vp,float *e,float *d,
	float r1,float r2,int *win,float rw){

int iz,ix,NRS;
float a_normal,a_vert=0.0,da,X,da_2,angle,T,t1,t2,c1,c2;
float **a3333, **a1111, **a1133, **a1313, **a1113, **a3313;
float **e_temp, **d_temp;
float X1,X2,p1,p2;
Ray *ray;

a3333=alloc2float(nz,nx);
a1111=alloc2float(nz,nx);
a1133=alloc2float(nz,nx);
a1313=alloc2float(nz,nx);
a1113=alloc2float(nz,nx);
a3313=alloc2float(nz,nx);
e_temp=alloc2float(nz,nx);
d_temp=alloc2float(nz,nx);

a_normal=atan(1/zd);
if (a_normal<0){ a_normal=(a_normal*180/3.1415-90); 
	a_vert=180+a_normal;
	}
if (a_normal>0){ a_normal=(a_normal*180/3.1415+90);
	a_vert=180-a_normal;
	}
if (fabs(a_normal)<=90)
warn("slope of reflector is infeasible; please pick reflector again");

/************************ setup the velocity field ****************************/
                for (ix=0;ix<nx;ix++)
                        for (iz=0;iz<nz;iz++){
			if((iz*dz)<=zzbound[ix]) a3333[ix][iz] = vp[ix*nz+iz];
			else
                        a3333[ix][iz] = VP0+kx*(fx+dx*ix-x00)+kz*(fz+dz*iz-z00);

				a1313[ix][iz] = 0.0;
				a1113[ix][iz] = 0.0;
				a3313[ix][iz] = 0.0;
			}
		smooth2(nz,nx,r1,r2,win,rw,a3333);

                for (ix=0; ix<nx; ++ix)
                        for (iz=0;iz<nz; ++iz)
                                a3333[ix][iz]=pow(a3333[ix][iz],2);

                for (ix=0; ix<nx; ++ix)
                        for (iz=0; iz<nz; ++iz){
			if((iz*dz)<=zzbound[ix]){
			e_temp[ix][iz]=e[ix*nz+iz];
			d_temp[ix][iz]=d[ix*nz+iz];
			}
			else {
			e_temp[ix][iz]=eps;
			d_temp[ix][iz]=del;
			}
		}
		smooth2(nz,nx,r1,r2,win,rw,e_temp);
		smooth2(nz,nx,r1,r2,win,rw,d_temp);

                for (ix=0; ix<nx; ++ix)
                        for (iz=0; iz<nz; ++iz){
			a1111[ix][iz] = a3333[ix][iz]+2*e_temp[ix][iz]* 
							a3333[ix][iz];
			a1133[ix][iz] = sqrt(2*d_temp[ix][iz]*a3333[ix][iz]*
                                    a3333[ix][iz]+a3333[ix][iz]*a3333[ix][iz]);
                }

/************ find the time for a given offset and diffractor point **********/
	/* find the closest two rays that bracket the desired offset */
	X=0; angle=0;da=-10.0;
	do{
	da=da+10.0;
	ray = makeRay(x,z,a_normal+da,nt,dt,a1111,a3333,a1133,a1313,
			a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);          
	NRS=ray->nrs;	
	if(ray->rs[NRS-1].z>0) printf("error in computation\n");
	X1=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
	freeRay(ray);
	ray = makeRay(x,z,a_normal-da,nt,dt,a1111,a3333,a1133,a1313,
				a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);   
	NRS=ray->nrs;	
	X2=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
	freeRay(ray);
	X=X2-X1;
	if (fabs(X-off)<0.003) break;
	}while(X<off);
	da_2=da-10.0;

	if(fabs(X-off)>0.003) 
	angle = zbrentou(da,da_2,tol,off,x,z,a_normal,nt,dt,a1111,a3333,a1133,a1313,a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);

	ray = makeRay(x,z,a_normal+angle,nt,dt,a1111,a3333,a1133,a1313,
				a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);   
	NRS=ray->nrs;	

	X1=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
	t1=-((ray->rs[NRS-1].t-ray->rs[NRS-2].t)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].t;
	p1=1/ray->rs[0].v;
	if(a_normal>0.0) c1=cos((a_vert-angle)*3.1415/180);
		else 	 c1=cos((a_vert+angle)*3.1415/180);
	freeRay(ray);
	ray = makeRay(x,z,a_normal-angle,nt,dt,a1111,a3333,a1133,a1313,
				a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);   
	NRS=ray->nrs;	

	X2=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
	t2=-((ray->rs[NRS-1].t-ray->rs[NRS-2].t)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].t;
	p2=1/ray->rs[0].v;
	if(a_normal>0.0) c2=cos((a_vert+angle)*3.1415/180);
		else 	 c2=cos((a_vert-angle)*3.1415/180);
	freeRay(ray);
	T=t1+t2;
	*p=1/(p1*c1+p2*c2);
	printf("est_offset=%f true_offset=%f time=%f cip=%f \n",X2-X1,off,T,x);

	/* free elements of the stiffness tensor */
	free2float(a3333);
	free2float(a1111);
	free2float(a1133);
	free2float(a1313);
	free2float(a1113);
	free2float(a3313);
	free2float(e_temp);
	free2float(d_temp);

	return T;
}

/**************************************************************************/
/****** the numerical algorithm to calculate the right takeoff angle ******/
/**************************************************************************/
float zbrentou(float da,float da_2,float tol,float off,float x, float z,
		float a_normal,int nt,float dt,float **a1111,
		float **a3333,float **a1133, float **a1313,float **a1113,
		float **a3313,int nx,float dx,float fx,int nz,float dz,
		float fz,float amax,float amin)
{
 int NRS,iter,ITMAX;
 float a=da,b=da_2,c=da_2,d=0,e=0,min1,min2,X1,X2;
 float fa,fb,fc,p,q,r,s,tol1,xm,EPS;
 Ray *ray;

ITMAX=1000; EPS=3e-8;

ray = makeRay(x,z,a_normal+a,nt,dt,a1111,a3333,a1133,a1313,
                        a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);
NRS=ray->nrs;
	X1=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
freeRay(ray);
ray = makeRay(x,z,a_normal-a,nt,dt,a1111,a3333,a1133,a1313,
                        a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);
NRS=ray->nrs;
	X2=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
freeRay(ray);
fa=X2-X1-off;
/*printf("%f \t",X2-X1);*/
ray = makeRay(x,z,a_normal+b,nt,dt,a1111,a3333,a1133,a1313,
                        a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);
NRS=ray->nrs;
	X1=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
freeRay(ray);
ray = makeRay(x,z,a_normal-b,nt,dt,a1111,a3333,a1133,a1313,
                        a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);
NRS=ray->nrs;
	X2=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
freeRay(ray);
fb=X2-X1-off;
/*printf("%f \n",X2-X1);*/
if ((fa > 0.0 && fb > 0.0) || (fa < 0.0 && fb < 0.0)) 
    printf("Root must be bracketed in zbrent");

fc=fb;
for (iter = 1;iter<=ITMAX;iter++){
   if ((fb > 0.0 && fc > 0.0)||(fb < 0.0 && fc < 0.0)) {
       c=a;
       fc=fa;
       e=d=b-a;
}
if (fabs(fc) < fabs(fb)) {
       a=b;
       b=c;
       c=a;
       fa=fb;
       fb=fc;
       fc=fa;
}

tol1=2.0*EPS*fabs(b)+0.5*tol;
xm=0.5*(c-b);
if ((fabs(xm)<=tol1)||(fb==0) ){
return b;
}
if (fabs(e) >= tol1 && fabs(fa) > fabs(fb)) {
       s=fb/fa;
       if (a==c) {
          p=2.0*xm*s;
          q=1.0-s;
       } else {
          q=fa/fc;
          r=fb/fc;
          p=s*(2.0*xm*q*(q-r)-(b-a)*(r-1));
          q=(q-1.0)*(r-1.0)*(s-1.0);
}
if (p>0.0) q = -q;
p=fabs(p);
min1=3.0*xm*q-fabs(tol1*q);
min2=fabs(e*q);
if(2.0*p < (min1-min2 ? min1 : min2)){
     e=d;
     d=p/q;
} else {
     d=xm;
     e=d;
}
}else{
   d=xm;
   e=d;
}
a=b;
fa=fb;
if (fabs(d) > tol1)
    b+=d;
else
    b += fabs(tol1)*xm/fabs(xm);
ray = makeRay(x,z,a_normal+b,nt,dt,a1111,a3333,a1133,a1313,
                        a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);
NRS=ray->nrs;
	X1=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
freeRay(ray);
ray = makeRay(x,z,a_normal-b,nt,dt,a1111,a3333,a1133,a1313,
                        a1113,a3313,nx,dx,fx,nz,dz,fz,amax,amin);
NRS=ray->nrs;
	X2=-((ray->rs[NRS-1].x-ray->rs[NRS-2].x)/(ray->rs[NRS-1].z-
		ray->rs[NRS-2].z))*ray->rs[NRS-2].z+ray->rs[NRS-2].x;
freeRay(ray);
fb=X2-X1-off;
}
printf("Maximum number of iterations exceeded in zbrent\n");
return 0.0;
}

/****************************************************************************/
/***************************  Tariq's ray tracer ****************************/
/****************************************************************************/
Ray *makeRay (float x0, float z0, float a0, int nt, float dt,
	float **a1111xz, float **a3333xz, float **a1133xz, float **a1313xz, 
	float **a1113xz, float **a3313xz, 
	int nx, float dx, float fx, int nz, float dz, float fz, float amax, 
	float amin)
/*****************************************************************************
Trace a ray for uniformly sampled v(x,z).
******************************************************************************
Input:
x0		x coordinate of takeoff point
z0		z coordinate of takeoff point
a0		takeoff angle (radians)
nt		number of time samples
dt		time sampling interval
nx		number of x samples
dx		x sampling interval
fx		first x sample
nz		number of z samples
dz		z sampling interval
fz		first z sample
amax            maximum emergence angle
amin            minimum emergence angle
a1111		array[nx][nz] of uniformly sampled density normalized elastic coef.
a3333		array[nx][nz] of uniformly sampled density normalized elastic coef.
a1133           array[nx][nz] of uniformly sampled density normalized elastic coef.
a1313           array[nx][nz] of uniformly sampled density normalized elastic coef.
a1113           array[nx][nz] of uniformly sampled density normalized elastic coef.
a3313           array[nx][nz] of uniformly sampled density normalized elastic coef.
******************************************************************************
Returned:	pointer to ray parameters sampled at discrete ray steps
******************************************************************************
Notes:
The ray ends when it runs out of time (after nt steps) or with the first 
step that is out of the (x,z) bounds of the velocity function v(x,z).
*****************************************************************************
Technical Reference:

Cerveny, V., 1972, Seismic rays and ray intensities 
	in inhomogeneous anisotropic media: 
	Geophys. J. R. Astr. Soc., 29, 1--13.

*****************************************************************************
 Credits: CWP: Tariq Alkhalifah
*****************************************************************************/
{
	int it,kmah;
	float t,x,z,c,s,p1,q1,p2,q2,px,pz,px2,pz2,pxz,
		lx,lz,cc,ss,
		a1111,da1111dx,da1111dz,dda1111dxdx,dda1111dxdz,dda1111dzdz,
		a3333,da3333dx,da3333dz,dda3333dxdx,dda3333dxdz,dda3333dzdz,
		a1133,da1133dx,da1133dz,dda1133dxdx,dda1133dxdz,dda1133dzdz,
		a1313,da1313dx,da1313dz,dda1313dxdx,dda1313dxdz,dda1313dzdz,
		a1113,da1113dx,da1113dz,dda1113dxdx,dda1113dxdz,dda1113dzdz,
		a3313,da3313dx,da3313dz,dda3313dxdx,dda3313dxdz,dda3313dzdz,
		da1111dn,dda1111dndn,da3333dn,dda3333dndn,da1133dn,dda1133dndn,
		da1313dn,dda1313dndn,da1113dn,dda1113dndn,da3313dn,dda3313dndn,
		gamm11,gamm13,gamm33,vp2,vp,ovp,sqr;
	Ray *ray;
	RayStep *rs;
	void *a11112;
	void *a33332;
	void *a11332;
	void *a13132;
	void *a11132;
	void *a33132;

	/*Convert degrees to radians*/
	a0=a0*3.1415/180; amax=amax*3.1415/180; amin=amin*3.1415/180;
	/* allocate and initialize velocities interpolator */
	a11112 = vel2Alloc(nx,dx,fx,nz,dz,fz,a1111xz);
	a33332 = vel2Alloc(nx,dx,fx,nz,dz,fz,a3333xz);
	a11332 = vel2Alloc(nx,dx,fx,nz,dz,fz,a1133xz);
	a13132 = vel2Alloc(nx,dx,fx,nz,dz,fz,a1313xz);
	a11132 = vel2Alloc(nx,dx,fx,nz,dz,fz,a1113xz);
	a33132 = vel2Alloc(nx,dx,fx,nz,dz,fz,a3313xz);
	
	/* last x and z in velocity model */
	lx = fx+(nx-1)*dx;
	lz = fz+(nz-1)*dz;

	/* ensure takeoff point is within model */
	if (x0<fx || x0>lx || z0<fz || z0>lz ){
	warn("The CRP lies outside the defined grid");
	return NULL;
	}

	/* allocate space for ray and raysteps */
	ray = (Ray*)alloc1(1,sizeof(Ray));
	rs = (RayStep*)alloc1(nt,sizeof(RayStep));

	/* cosine and sine of takeoff angle */
	c = cos(a0);
	s = sin(a0);
	cc = c*c;
	ss = s*s;
	
	/* velocities and derivatives at takeoff point */
	vel2Interp(a11112,x0,z0,&a1111,&da1111dx,&da1111dz,&dda1111dxdx,
		&dda1111dxdz,&dda1111dzdz);
	da1111dn    = da1111dx*c-da1111dz*s;
	dda1111dndn = dda1111dxdx*cc-2.0*dda1111dxdz*s*c+dda1111dzdz*ss;

	vel2Interp(a33332,x0,z0,&a3333,&da3333dx,&da3333dz,&dda3333dxdx,
		&dda3333dxdz,&dda3333dzdz);
	da3333dn    = da3333dx*c-da3333dz*s;
	dda3333dndn = dda3333dxdx*cc-2.0*dda3333dxdz*s*c+dda3333dzdz*ss;
	
	vel2Interp(a11332,x0,z0,&a1133,&da1133dx,&da1133dz,&dda1133dxdx,
		&dda1133dxdz,&dda1133dzdz);
	da1133dn    = da1133dx*c-da1133dz*s;
	dda1133dndn = dda1133dxdx*cc-2.0*dda1133dxdz*s*c+dda1133dzdz*ss;

	vel2Interp(a13132,x0,z0,&a1313,&da1313dx,&da1313dz,&dda1313dxdx,
		&dda1313dxdz,&dda1313dzdz);
	da1313dn    = da1313dx*c-da1313dz*s;
	dda1313dndn = dda1313dxdx*cc-2.0*dda1313dxdz*s*c+dda1313dzdz*ss;

	vel2Interp(a11132,x0,z0,&a1113,&da1113dx,&da1113dz,&dda1113dxdx,
		&dda1113dxdz,&dda1113dzdz);
	da1113dn    = da1113dx*c-da1113dz*s;
	dda1113dndn = dda1113dxdx*cc-2.0*dda1113dxdz*s*c+dda1113dzdz*ss;

	vel2Interp(a33132,x0,z0,&a3313,&da3313dx,&da3313dz,&dda3313dxdx,
		&dda3313dxdz,&dda3313dzdz);
	da3313dn    = da3313dx*c-da3313dz*s;
	dda3313dndn = dda3313dxdx*cc-2.0*dda3313dxdz*s*c+dda3313dzdz*ss;

	/*computing the phase velocity for a0 angle */
	gamm11 = a1111*ss+ a1313*cc +2*a1113*s*c;
	gamm33 = a3333*cc + a1313*ss+2*a3313*s*c;
	gamm13 = (a1133+a1313)*s*c+ a1113*ss+ a3313*cc;
	sqr    = sqrt((gamm11+gamm33)*(gamm11+gamm33)-
			4*(gamm11*gamm33-gamm13*gamm13));
	vp2    = gamm11+gamm33+sqr;
	vp     = sqrt(vp2*.5);
	ovp    = 1/vp;
	px     = s*ovp;
	pz     = c*ovp;

	/* first ray step */
	rs[0].t = t = 0;
	rs[0].x = x = x0;
	rs[0].z = z = z0;
	rs[0].q1 = q1 = 1.0;
	rs[0].p1 = p1 = 0.0;
	rs[0].q2 = q2 = 0.0;
	rs[0].p2 = p2 = 1.0;
	rs[0].kmah = kmah = 0;
	rs[0].c = c;
	rs[0].s = s;
	rs[0].v = vp;
	rs[0].dvdx = .5*da3333dx*vp/a3333;
	rs[0].dvdz = .5*da3333dz*vp/a3333;

	/* loop over time steps */
	for (it=1; it<nt; ++it) {

		/* variables used for Runge-Kutta integration */
		float h=dt,hhalf=dt/2.0,hsixth=dt/6.0,
			q2old,xt,zt,p1t,q1t,p2t,q2t,
			dx,dz,dp1,dq1,dp2,dq2,
			dxt,dzt,dp1t,dq1t,dp2t,dq2t,
			dxm,dzm,dp1m,dq1m,dp2m,dq2m,
			gamma11,gamma33,gamma13,g11,g13,g33,den,
			sxfactor,szfactor,snfact,dpx,dpz,pxt,pzt,dpxt,
			dpzt,dpxm,dpzm,dxdn,dzdn,snfactor,
			dxx,dzz,dcdp1,dcdp3,dcdp13,ddcdnn,ddcdqq,
			ddcdpn,dgamma11dpx,dgamma11dpz,dgamma33dpx,
			dgamma33dpz,dgamma13dpx,dgamma13dpz,dg11dpx,
			dg11dpz,dg33dpx,dg33dpz,dg13dpx,dg13dpz,ddxdpx,
			ddzdpz,ddxdpz,dgamma11dn,dgamma33dn,dgamma13dn,
			dg11dn,dg33dn,dg13dn,dsnfactordn,ddxdn,ddzdn;

		/* if ray is out of bounds, break */
		if (x<fx || x>lx || z<fz || z>lz || c>(cos(amin)+0.01) || c<(cos(amax))-0.01) break;

		/* remember old q2 */
		q2old = q2;
		
	        /* step 1 of 4th-order Runge-Kutta */
		px2   = px*px;
		pz2   = pz*pz;
		pxz   = px*pz;

		/*anisotropy parameters*/
		gamma11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
		gamma33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
		gamma13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
		den     = 1/(gamma11+gamma33-2);
		g11     = (gamma33-1)*den;
		g33     = (gamma11-1)*den;
		g13     = -gamma13*den;
		sxfactor = da1111dx*px2*g11+da3333dx*pz2*g33+
			2*(da1133dx+da1313dx)*pxz*g13+da1313dx*(px2*g33+pz2*g11)+
			2*da3313dx*(pz2*g13+pxz*g33)+2*da1113dx*(pxz*g11+px2*g13);
		szfactor = da1111dz*px2*g11+da3333dz*pz2*g33+
			2*(da1133dz+da1313dz)*pxz*g13+da1313dz*(px2*g33+pz2*g11)+
			2*da3313dz*(pz2*g13+pxz*g33)+2*da1113dz*(pxz*g11+px2*g13);
		snfact = sxfactor*c-szfactor*s;
		
		/*computing ray velocities and derivatives*/
		dx =  (a1111*px*g11+(a1133+a1313)*pz*g13+a3313*pz*g33
			+a1113*(pz*g11+2*px*g13)+a1313*g33*px);
		dz =  (a3333*pz*g33+(a1133+a1313)*px*g13+a1113*px*g11
			+a3313*(px*g33+2*pz*g13)+a1313*g11*pz);

		dgamma11dpx = 2*a1111*px+2*a1113*pz;
		dgamma11dpz = 2*a1313*pz+2*a1113*px;
		dgamma33dpx = 2*a1313*px+2*a3313*pz;
		dgamma33dpz = 2*a3333*pz+2*a3313*px;
		dgamma13dpx= (a1133+a1313)*pz+2*a1113*px;
		dgamma13dpz= (a1133+a1313)*px+2*a3313*pz;
		dgamma11dn = da1111dn*px2+ da1313dn*pz2 +2*da1113dn*pxz;
		dgamma33dn = da3333dn*pz2 + da1313dn*px2+2*da3313dn*pxz;
		dgamma13dn = (da1133dn+da1313dn)*pxz+ da1113dn*px2+ da3313dn*pz2;
		dg11dpx = -(gamma33-1)*(dgamma11dpx+dgamma33dpx-4*dx)*den*den+
			(dgamma33dpx-2*dx)*den;
		dg11dpz = -(gamma33-1)*(dgamma11dpz+dgamma33dpz-4*dz)*den*den+
			(dgamma33dpz-2*dz)*den;
		dg33dpx = -(gamma11-1)*(dgamma11dpx+dgamma33dpx-4*dx)*den*den+
			(dgamma11dpx-2*dx)*den;
		dg33dpz = -(gamma11-1)*(dgamma11dpz+dgamma33dpz-4*dz)*den*den+
			(dgamma11dpz-2*dz)*den;
		dg13dpx = gamma13*(dgamma11dpx+dgamma33dpx-4*dx)*den*den-
			dgamma13dpx*den;
		dg13dpz = gamma13*(dgamma11dpz+dgamma33dpz-4*dz)*den*den-
			dgamma13dpz*den;
		dg11dn = -(gamma33-1)*(dgamma11dn+dgamma33dn-2*snfact)*den*den+
			(dgamma33dn-snfact)*den;
		dg33dn = -(gamma11-1)*(dgamma11dn+dgamma33dn-2*snfact)*den*den+
			(dgamma11dn-snfact)*den;
		dg13dn = gamma13*(dgamma11dn+dgamma33dn-2*snfact)*den*den-
			dgamma13dn*den;
		ddxdpx=   a1111*px*dg11dpx+(a1133+a1313)*pz*dg13dpx+
			a3313*pz*dg33dpx+a1113*(pz*dg11dpx+2*px*dg13dpx)
			+a1313*dg33dpx*px;
		ddzdpz= a3333*pz*dg33dpz+(a1133+a1313)*px*dg13dpz+
			a1113*px*dg11dpz+a3313*(px*dg33dpz+2*pz*dg13dpz)+
			a1313*dg11dpz*pz;
		ddxdpz= a1111*px*dg11dpz+(a1133+a1313)*pz*dg13dpz+
			a3313*pz*dg33dpz+a1113*(pz*dg11dpz+2*px*dg13dpz)+
			a1313*dg33dpz*px;
		dsnfactordn = da1111dn*px2*dg11dn+da3333dn*pz2*dg33dn+
			2*(da1133dn+da1313dn)*pxz*dg13dn+da1313dn*(px2*dg33dn+pz2*dg11dn)+
			2*da3313dn*(pz2*dg13dn+pxz*dg33dn)+2*da1113dn*(pxz*dg11dn+px2*dg13dn);
		ddxdn =  (a1111*px*dg11dn+(a1133+a1313)*pz*dg13dn+a3313*pz*dg33dn
			+a1113*(pz*dg11dn+2*px*dg13dn)+a1313*dg33dn*px);
		ddzdn =  (a3333*pz*dg33dn+(a1133+a1313)*px*dg13dn+a1113*px*dg11dn
			+a3313*(px*dg33dn+2*pz*dg13dn)+a1313*dg11dn*pz);
		

		/*evaluating change in slowness and amplitude along ray*/
		dpx = -0.5*sxfactor;
		dpz = -0.5*szfactor;

		dcdp1  = a1111*g11+a1313*g33+2*a1113*g13+ddxdpx-dx*dx;
		dcdp3  = a3333*g33+a1313*g11+2*a3313*g13+ddzdpz-dz*dz;
		dcdp13 = a1133*g13+a1313*g13+a1113*g11+a3313*g33+ddxdpz-dx*dz;
		ddcdqq = dcdp1*cc-2.0*dcdp13*s*c+dcdp3*ss;
		dxdn   =  (da1111dn*px*g11+(da1133dn+da1313dn)*pz*g13+da3313dn*pz*g33
			+da1113dn*(pz*g11+2*px*g13)+da1313dn*g33*px);
		dzdn   =  (da3333dn*pz*g33+(da1133dn+da1313dn)*px*g13+da1113dn*px*g11
			+da3313dn*(px*g33+2*pz*g13)+da1313dn*g11*pz);
		ddcdpn = dxdn*c-dzdn*s-.5*dx*sxfactor*cc+
			.5*(dx*szfactor+dz*sxfactor)*s*c-.5*dz*szfactor*ss
			+ddxdn*c-ddzdn*s;
		snfactor = dda1111dndn*px2*g11+dda3333dndn*pz2*g33+
			2*(dda1133dndn+dda1313dndn)*pxz*g13+
			dda1313dndn*(px2*g33+pz2*g11)+
			2*dda3313dndn*(pz2*g13+pxz*g33)+
			2*dda1113dndn*(pxz*g11+px2*g13);
		ddcdnn = 0.5*snfactor-.25*sxfactor*sxfactor*cc+
			.5*sxfactor*szfactor*s*c-.25*szfactor*szfactor*ss
			+.5*dsnfactordn;

		dp1 = -ddcdnn*q1-ddcdpn*p1;
		dq1 = ddcdqq*p1+ddcdpn*q1;
		dp2 = -ddcdnn*q2-ddcdpn*p2;
		dq2 = ddcdqq*p2+ddcdpn*q2;
		xt = x+hhalf*dx;
		zt = z+hhalf*dz;
		pxt = px+hhalf*dpx;
		pzt = pz+hhalf*dpz;
		p1t = p1+hhalf*dp1;
		q1t = q1+hhalf*dq1;
		p2t = p2+hhalf*dp2;
		q2t = q2+hhalf*dq2;
		vp  = 1/sqrt(pxt*pxt+pzt*pzt);
		s   = pxt*vp;
		c   = pzt*vp;
		ss  = s*s;
		cc  = c*c;
		
		vel2Interp(a11112,xt,zt,&a1111,&da1111dx,&da1111dz,&dda1111dxdx,
			&dda1111dxdz,&dda1111dzdz);
		da1111dn    = da1111dx*c-da1111dz*s;
		dda1111dndn = dda1111dxdx*cc-2.0*dda1111dxdz*s*c+dda1111dzdz*ss;

		vel2Interp(a33332,xt,zt,&a3333,&da3333dx,&da3333dz,&dda3333dxdx,
		&dda3333dxdz,&dda3333dzdz);
		da3333dn    = da3333dx*c-da3333dz*s;
		dda3333dndn = dda3333dxdx*cc-2.0*dda3333dxdz*s*c+dda3333dzdz*ss;
	
		vel2Interp(a11332,xt,zt,&a1133,&da1133dx,&da1133dz,&dda1133dxdx,
			&dda1133dxdz,&dda1133dzdz);
		da1133dn    = da1133dx*c-da1133dz*s;
		dda1133dndn = dda1133dxdx*cc-2.0*dda1133dxdz*s*c+dda1133dzdz*ss;

		vel2Interp(a13132,xt,zt,&a1313,&da1313dx,&da1313dz,&dda1313dxdx,
			&dda1313dxdz,&dda1313dzdz);
		da1313dn    = da1313dx*c-da1313dz*s;
		dda1313dndn = dda1313dxdx*cc-2.0*dda1313dxdz*s*c+dda1313dzdz*ss;

		vel2Interp(a11132,xt,zt,&a1113,&da1113dx,&da1113dz,&dda1113dxdx,
			&dda1113dxdz,&dda1113dzdz);
		da1113dn    = da1113dx*c-da1113dz*s;
		dda1113dndn = dda1113dxdx*cc-2.0*dda1113dxdz*s*c+dda1113dzdz*ss;

		vel2Interp(a33132,xt,zt,&a3313,&da3313dx,&da3313dz,&dda3313dxdx,
			&dda3313dxdz,&dda3313dzdz);
		da3313dn    = da3313dx*c-da3313dz*s;
		dda3313dndn = dda3313dxdx*cc-2.0*dda3313dxdz*s*c+dda3313dzdz*ss;
		
        	/* step 2 of 4th-order Runge-Kutta */
		px2   = pxt*pxt;
		pz2   = pzt*pzt;
		pxz   = pxt*pzt;

		/*anisotropy parameters*/
		gamma11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
		gamma33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
		gamma13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
		den     = 1/(gamma11+gamma33-2);
		g11     = (gamma33-1)*den;
		g33     = (gamma11-1)*den;
		g13     = -gamma13*den;
		sxfactor = da1111dx*px2*g11+da3333dx*pz2*g33+
			2*(da1133dx+da1313dx)*pxz*g13+da1313dx*(px2*g33+pz2*g11)+
			2*da3313dx*(pz2*g13+pxz*g33)+2*da1113dx*(pxz*g11+px2*g13);
		szfactor = da1111dz*px2*g11+da3333dz*pz2*g33+
			2*(da1133dz+da1313dz)*pxz*g13+da1313dz*(px2*g33+pz2*g11)+
			2*da3313dz*(pz2*g13+pxz*g33)+2*da1113dz*(pxz*g11+px2*g13);
		snfact = sxfactor*c-szfactor*s;
		
		/*computing ray velocities and derivatives*/
		dxt =  (a1111*pxt*g11+(a1133+a1313)*pzt*g13+a3313*pzt*g33
			+a1113*(pzt*g11+2*pxt*g13)+a1313*g33*pxt);
		dzt =  (a3333*pzt*g33+(a1133+a1313)*pxt*g13+a1113*pxt*g11
			+a3313*(pxt*g33+2*pzt*g13)+a1313*g11*pzt);
		dpxt = -0.5*sxfactor;
		dpzt = -0.5*szfactor;

		dgamma11dpx = 2*a1111*pxt+2*a1113*pzt;
		dgamma11dpz = 2*a1313*pzt+2*a1113*pxt;
		dgamma33dpx = 2*a1313*pxt+2*a3313*pzt;
		dgamma33dpz = 2*a3333*pzt+2*a3313*pxt;
		dgamma13dpx= (a1133+a1313)*pzt+2*a1113*pxt;
		dgamma13dpz= (a1133+a1313)*pxt+2*a3313*pzt;
		dgamma11dn = da1111dn*px2+ da1313dn*pz2 +2*da1113dn*pxz;
		dgamma33dn = da3333dn*pz2 + da1313dn*px2+2*da3313dn*pxz;
		dgamma13dn = (da1133dn+da1313dn)*pxz+ da1113dn*px2+ da3313dn*pz2;
		dg11dpx = -(gamma33-1)*(dgamma11dpx+dgamma33dpx-4*dxt)*den*den+
			(dgamma33dpx-2*dxt)*den;
		dg11dpz = -(gamma33-1)*(dgamma11dpz+dgamma33dpz-4*dzt)*den*den+
			(dgamma33dpz-2*dzt)*den;
		dg33dpx = -(gamma11-1)*(dgamma11dpx+dgamma33dpx-4*dxt)*den*den+
			(dgamma11dpx-2*dxt)*den;
		dg33dpz = -(gamma11-1)*(dgamma11dpz+dgamma33dpz-4*dzt)*den*den+
			(dgamma11dpz-2*dzt)*den;
		dg13dpx = gamma13*(dgamma11dpx+dgamma33dpx-4*dxt)*den*den-
			dgamma13dpx*den;
		dg13dpz = gamma13*(dgamma11dpz+dgamma33dpz-4*dzt)*den*den-
			dgamma13dpz*den;
		dg11dn = -(gamma33-1)*(dgamma11dn+dgamma33dn-2*snfact)*den*den+
			(dgamma33dn-snfact)*den;
		dg33dn = -(gamma11-1)*(dgamma11dn+dgamma33dn-2*snfact)*den*den+
			(dgamma11dn-snfact)*den;
		dg13dn = gamma13*(dgamma11dn+dgamma33dn-2*snfact)*den*den-
			dgamma13dn*den;
		ddxdpx=   a1111*pxt*dg11dpx+(a1133+a1313)*pzt*dg13dpx+
			a3313*pzt*dg33dpx+a1113*(pzt*dg11dpx+2*pxt*dg13dpx)
			+a1313*dg33dpx*pxt;
		ddzdpz= a3333*pzt*dg33dpz+(a1133+a1313)*pxt*dg13dpz+
			a1113*pxt*dg11dpz+a3313*(pxt*dg33dpz+2*pzt*dg13dpz)+
			a1313*dg11dpz*pzt;
		ddxdpz= a1111*pxt*dg11dpz+(a1133+a1313)*pzt*dg13dpz+
			a3313*pzt*dg33dpz+a1113*(pzt*dg11dpz+2*pxt*dg13dpz)+
			a1313*dg33dpz*pxt;
		dsnfactordn = da1111dn*px2*dg11dn+da3333dn*pz2*dg33dn+
			2*(da1133dn+da1313dn)*pxz*dg13dn+da1313dn*(px2*dg33dn+pz2*dg11dn)+
			2*da3313dn*(pz2*dg13dn+pxz*dg33dn)+2*da1113dn*(pxz*dg11dn+px2*dg13dn);
		ddxdn =  (a1111*pxt*dg11dn+(a1133+a1313)*pzt*dg13dn+a3313*pzt*dg33dn
			+a1113*(pzt*dg11dn+2*pxt*dg13dn)+a1313*dg33dn*pxt);
		ddzdn =  (a3333*pzt*dg33dn+(a1133+a1313)*pxt*dg13dn+a1113*pxt*dg11dn
			+a3313*(pxt*dg33dn+2*pzt*dg13dn)+a1313*dg11dn*pzt);
		
		dcdp1  = a1111*g11+a1313*g33+2*a1113*g13+ddxdpx-dxt*dxt;
		dcdp3  = a3333*g33+a1313*g11+2*a3313*g13+ddzdpz-dzt*dzt;
		dcdp13 = a1133*g13+a1313*g13+a1113*g11+a3313*g33+ddxdpz-dxt*dzt;
		ddcdqq = dcdp1*cc-2.0*dcdp13*s*c+dcdp3*ss;
		dxdn   =  (da1111dn*pxt*g11+(da1133dn+da1313dn)*pzt*g13+
			da3313dn*pzt*g33+da1113dn*(pzt*g11+2*pxt*g13)+
			da1313dn*g33*pxt);
		dzdn   =  (da3333dn*pzt*g33+(da1133dn+da1313dn)*pxt*g13+
			da1113dn*pxt*g11+da3313dn*(pxt*g33+2*pzt*g13)+
			da1313dn*g11*pzt);
		ddcdpn = dxdn*c-dzdn*s-.5*dxt*sxfactor*cc+
			.5*(dxt*szfactor+dzt*sxfactor)*s*c-.5*dzt*szfactor*ss
			+ddxdn*c-ddzdn*s;
		snfactor = dda1111dndn*px2*g11+dda3333dndn*pz2*g33+
			2*(dda1133dndn+dda1313dndn)*pxz*g13+
			dda1313dndn*(px2*g33+pz2*g11)+
			2*dda3313dndn*(pz2*g13+pxz*g33)+
			2*dda1113dndn*(pxz*g11+px2*g13);
		ddcdnn = 0.5*snfactor-.25*sxfactor*sxfactor*cc+
			.5*sxfactor*szfactor*s*c-.25*szfactor*szfactor*ss
			+.5*dsnfactordn;

		dp1t = -ddcdnn*q1t-ddcdpn*p1t;
		dq1t = ddcdqq*p1t+ddcdpn*q1t;
		dp2t = -ddcdnn*q2t-ddcdpn*p2t;
		dq2t = ddcdqq*p2t+ddcdpn*q2t;
		xt = x+hhalf*dxt;
		zt = z+hhalf*dzt;
		pxt = px+hhalf*dpxt;
		pzt = pz+hhalf*dpzt;
		p1t = p1+hhalf*dp1t;
		q1t = q1+hhalf*dq1t;
		p2t = p2+hhalf*dp2t;
		q2t = q2+hhalf*dq2t;
		vp  = 1/sqrt(pxt*pxt+pzt*pzt);
		s   = pxt*vp;
		c   = pzt*vp;
		ss  = s*s;
		cc  = c*c;
		
		vel2Interp(a11112,xt,zt,&a1111,&da1111dx,&da1111dz,&dda1111dxdx,
			&dda1111dxdz,&dda1111dzdz);
		da1111dn    = da1111dx*c-da1111dz*s;
		dda1111dndn = dda1111dxdx*cc-2.0*dda1111dxdz*s*c+dda1111dzdz*ss;

		vel2Interp(a33332,xt,zt,&a3333,&da3333dx,&da3333dz,&dda3333dxdx,
		&dda3333dxdz,&dda3333dzdz);
		da3333dn    = da3333dx*c-da3333dz*s;
		dda3333dndn = dda3333dxdx*cc-2.0*dda3333dxdz*s*c+dda3333dzdz*ss;
	
		vel2Interp(a11332,xt,zt,&a1133,&da1133dx,&da1133dz,&dda1133dxdx,
			&dda1133dxdz,&dda1133dzdz);
		da1133dn    = da1133dx*c-da1133dz*s;
		dda1133dndn = dda1133dxdx*cc-2.0*dda1133dxdz*s*c+dda1133dzdz*ss;

		vel2Interp(a13132,xt,zt,&a1313,&da1313dx,&da1313dz,&dda1313dxdx,
			&dda1313dxdz,&dda1313dzdz);
		da1313dn    = da1313dx*c-da1313dz*s;
		dda1313dndn = dda1313dxdx*cc-2.0*dda1313dxdz*s*c+dda1313dzdz*ss;

		vel2Interp(a11132,xt,zt,&a1113,&da1113dx,&da1113dz,&dda1113dxdx,
			&dda1113dxdz,&dda1113dzdz);
		da1113dn    = da1113dx*c-da1113dz*s;
		dda1113dndn = dda1113dxdx*cc-2.0*dda1113dxdz*s*c+dda1113dzdz*ss;

		vel2Interp(a33132,xt,zt,&a3313,&da3313dx,&da3313dz,&dda3313dxdx,
			&dda3313dxdz,&dda3313dzdz);
		da3313dn    = da3313dx*c-da3313dz*s;
		dda3313dndn = dda3313dxdx*cc-2.0*dda3313dxdz*s*c+dda3313dzdz*ss;
		
	/* step 3 of 4th-order Runge-Kutta */
		px2   = pxt*pxt;
		pz2   = pzt*pzt;
		pxz   = pxt*pzt;

		/*anisotropy parameters*/
		gamma11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
		gamma33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
		gamma13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
		den     = 1/(gamma11+gamma33-2);
		g11     = (gamma33-1)*den;
		g33     = (gamma11-1)*den;
		g13     = -gamma13*den;
		sxfactor = da1111dx*px2*g11+da3333dx*pz2*g33+
			2*(da1133dx+da1313dx)*pxz*g13+da1313dx*(px2*g33+pz2*g11)+
			2*da3313dx*(pz2*g13+pxz*g33)+2*da1113dx*(pxz*g11+px2*g13);
		szfactor = da1111dz*px2*g11+da3333dz*pz2*g33+
			2*(da1133dz+da1313dz)*pxz*g13+da1313dz*(px2*g33+pz2*g11)+
			2*da3313dz*(pz2*g13+pxz*g33)+2*da1113dz*(pxz*g11+px2*g13);
		snfact = sxfactor*c-szfactor*s;
		
		/*computing ray velocities and derivatives*/
		dxm =  (a1111*pxt*g11+(a1133+a1313)*pzt*g13+a3313*pzt*g33
			+a1113*(pzt*g11+2*pxt*g13)+a1313*g33*pxt);
		dzm =  (a3333*pzt*g33+(a1133+a1313)*pxt*g13+a1113*pxt*g11
			+a3313*(pxt*g33+2*pzt*g13)+a1313*g11*pzt);
		dpxm = -0.5*sxfactor;
		dpzm = -0.5*szfactor;

		dgamma11dpx = 2*a1111*pxt+2*a1113*pzt;
		dgamma11dpz = 2*a1313*pzt+2*a1113*pxt;
		dgamma33dpx = 2*a1313*pxt+2*a3313*pzt;
		dgamma33dpz = 2*a3333*pzt+2*a3313*pxt;
		dgamma13dpx= (a1133+a1313)*pzt+2*a1113*pxt;
		dgamma13dpz= (a1133+a1313)*pxt+2*a3313*pzt;
		dgamma11dn = da1111dn*px2+ da1313dn*pz2 +2*da1113dn*pxz;
		dgamma33dn = da3333dn*pz2 + da1313dn*px2+2*da3313dn*pxz;
		dgamma13dn = (da1133dn+da1313dn)*pxz+ da1113dn*px2+ da3313dn*pz2;
		dg11dpx = -(gamma33-1)*(dgamma11dpx+dgamma33dpx-4*dxm)*den*den+
			(dgamma33dpx-2*dxm)*den;
		dg11dpz = -(gamma33-1)*(dgamma11dpz+dgamma33dpz-4*dzm)*den*den+
			(dgamma33dpz-2*dzm)*den;
		dg33dpx = -(gamma11-1)*(dgamma11dpx+dgamma33dpx-4*dxm)*den*den+
			(dgamma11dpx-2*dxm)*den;
		dg33dpz = -(gamma11-1)*(dgamma11dpz+dgamma33dpz-4*dzm)*den*den+
			(dgamma11dpz-2*dzm)*den;
		dg13dpx = gamma13*(dgamma11dpx+dgamma33dpx-4*dxm)*den*den-
			dgamma13dpx*den;
		dg13dpz = gamma13*(dgamma11dpz+dgamma33dpz-4*dzm)*den*den-
			dgamma13dpz*den;
		dg11dn = -(gamma33-1)*(dgamma11dn+dgamma33dn-2*snfact)*den*den+
			(dgamma33dn-snfact)*den;
		dg33dn = -(gamma11-1)*(dgamma11dn+dgamma33dn-2*snfact)*den*den+
			(dgamma11dn-snfact)*den;
		dg13dn = gamma13*(dgamma11dn+dgamma33dn-2*snfact)*den*den-
			dgamma13dn*den;
		ddxdpx=   a1111*pxt*dg11dpx+(a1133+a1313)*pzt*dg13dpx+
			a3313*pzt*dg33dpx+a1113*(pzt*dg11dpx+2*pxt*dg13dpx)
			+a1313*dg33dpx*pxt;
		ddzdpz= a3333*pzt*dg33dpz+(a1133+a1313)*pxt*dg13dpz+
			a1113*pxt*dg11dpz+a3313*(pxt*dg33dpz+2*pzt*dg13dpz)+
			a1313*dg11dpz*pzt;
		ddxdpz= a1111*pxt*dg11dpz+(a1133+a1313)*pzt*dg13dpz+
			a3313*pzt*dg33dpz+a1113*(pzt*dg11dpz+2*pxt*dg13dpz)+
			a1313*dg33dpz*pxt;
		dsnfactordn = da1111dn*px2*dg11dn+da3333dn*pz2*dg33dn+
			2*(da1133dn+da1313dn)*pxz*dg13dn+da1313dn*(px2*dg33dn+pz2*dg11dn)+
			2*da3313dn*(pz2*dg13dn+pxz*dg33dn)+2*da1113dn*(pxz*dg11dn+px2*dg13dn);
		ddxdn =  (a1111*pxt*dg11dn+(a1133+a1313)*pzt*dg13dn+a3313*pzt*dg33dn
			+a1113*(pzt*dg11dn+2*pxt*dg13dn)+a1313*dg33dn*pxt);
		ddzdn =  (a3333*pzt*dg33dn+(a1133+a1313)*pxt*dg13dn+a1113*pxt*dg11dn
			+a3313*(pxt*dg33dn+2*pzt*dg13dn)+a1313*dg11dn*pzt);

		
		dcdp1  = a1111*g11+a1313*g33+2*a1113*g13+ddxdpx-dxm*dxm;
		dcdp3  = a3333*g33+a1313*g11+2*a3313*g13+ddzdpz-dzm*dzm;
		dcdp13 = a1133*g13+a1313*g13+a1113*g11+a3313*g33+ddxdpz-dxm*dzm;
		ddcdqq = dcdp1*cc-2.0*dcdp13*s*c+dcdp3*ss;
		dxdn   =  (da1111dn*pxt*g11+(da1133dn+da1313dn)*pzt*g13+
			da3313dn*pzt*g33+da1113dn*(pzt*g11+2*pxt*g13)+
			da1313dn*g33*pxt);
		dzdn   =  (da3333dn*pzt*g33+(da1133dn+da1313dn)*pxt*g13+
			da1113dn*pxt*g11+da3313dn*(pxt*g33+2*pzt*g13)+
			da1313dn*g11*pzt);
		ddcdpn = dxdn*c-dzdn*s-.5*dxm*sxfactor*cc+
			.5*(dxm*szfactor+dzm*sxfactor)*s*c-.5*dzm*szfactor*ss
			+ddxdn*c-ddzdn*s;
		snfactor = dda1111dndn*px2*g11+dda3333dndn*pz2*g33+
			2*(dda1133dndn+dda1313dndn)*pxz*g13+
			dda1313dndn*(px2*g33+pz2*g11)+
			2*dda3313dndn*(pz2*g13+pxz*g33)+
			2*dda1113dndn*(pxz*g11+px2*g13);
		ddcdnn = 0.5*snfactor-.25*sxfactor*sxfactor*cc+
			.5*sxfactor*szfactor*s*c-.25*szfactor*szfactor*ss
			+.5*dsnfactordn;

		dp1m = -ddcdnn*q1t-ddcdpn*p1t;
		dq1m = ddcdqq*p1t+ddcdpn*q1t;
		dp2m = -ddcdnn*q2t-ddcdpn*p2t;
		dq2m = ddcdqq*p2t+ddcdpn*q2t;
		xt = x+hhalf*dx;
		zt = z+hhalf*dz;
		pxt = px+h*dpxm;
		pzt = pz+h*dpzm;
		p1t = p1+h*dp1m;
		q1t = q1+h*dq1m;
		p2t = p2+h*dp2m;
		q2t = q2+h*dq2m;
		dxm += dxt;
		dzm += dzt;
		dpxm += dpxt;
		dpzm += dpzt;
		dp1m += dp1t;
		dq1m += dq1t;
		dp2m += dp2t;
		dq2m += dq2t;
		vp  = 1/sqrt(pxt*pxt+pzt*pzt);
		s   = pxt*vp;
		c   = pzt*vp;
		ss  = s*s;
		cc  = c*c;
		
		vel2Interp(a11112,xt,zt,&a1111,&da1111dx,&da1111dz,&dda1111dxdx,
			&dda1111dxdz,&dda1111dzdz);
		da1111dn    = da1111dx*c-da1111dz*s;
		dda1111dndn = dda1111dxdx*cc-2.0*dda1111dxdz*s*c+dda1111dzdz*ss;

		vel2Interp(a33332,xt,zt,&a3333,&da3333dx,&da3333dz,&dda3333dxdx,
		&dda3333dxdz,&dda3333dzdz);
		da3333dn    = da3333dx*c-da3333dz*s;
		dda3333dndn = dda3333dxdx*cc-2.0*dda3333dxdz*s*c+dda3333dzdz*ss;
	
		vel2Interp(a11332,xt,zt,&a1133,&da1133dx,&da1133dz,&dda1133dxdx,
			&dda1133dxdz,&dda1133dzdz);
		da1133dn    = da1133dx*c-da1133dz*s;
		dda1133dndn = dda1133dxdx*cc-2.0*dda1133dxdz*s*c+dda1133dzdz*ss;

		vel2Interp(a13132,xt,zt,&a1313,&da1313dx,&da1313dz,&dda1313dxdx,
			&dda1313dxdz,&dda1313dzdz);
		da1313dn    = da1313dx*c-da1313dz*s;
		dda1313dndn = dda1313dxdx*cc-2.0*dda1313dxdz*s*c+dda1313dzdz*ss;

		vel2Interp(a11132,xt,zt,&a1113,&da1113dx,&da1113dz,&dda1113dxdx,
			&dda1113dxdz,&dda1113dzdz);
		da1113dn    = da1113dx*c-da1113dz*s;
		dda1113dndn = dda1113dxdx*cc-2.0*dda1113dxdz*s*c+dda1113dzdz*ss;

		vel2Interp(a33132,xt,zt,&a3313,&da3313dx,&da3313dz,&dda3313dxdx,
			&dda3313dxdz,&dda3313dzdz);
		da3313dn    = da3313dx*c-da3313dz*s;
		dda3313dndn = dda3313dxdx*cc-2.0*dda3313dxdz*s*c+dda3313dzdz*ss;
		
	/* step 4 of 4th-order Runge-Kutta */
		px2   = pxt*pxt;
		pz2   = pzt*pzt;
		pxz   = pxt*pzt;

		/*anisotropy parameters*/
		gamma11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
		gamma33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
		gamma13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
		den     = 1/(gamma11+gamma33-2);
		g11     = (gamma33-1)*den;
		g33     = (gamma11-1)*den;
		g13     = -gamma13*den;
		sxfactor = da1111dx*px2*g11+da3333dx*pz2*g33+
			2*(da1133dx+da1313dx)*pxz*g13+da1313dx*(px2*g33+pz2*g11)+
			2*da3313dx*(pz2*g13+pxz*g33)+2*da1113dx*(pxz*g11+px2*g13);
		szfactor = da1111dz*px2*g11+da3333dz*pz2*g33+
			2*(da1133dz+da1313dz)*pxz*g13+da1313dz*(px2*g33+pz2*g11)+
			2*da3313dz*(pz2*g13+pxz*g33)+2*da1113dz*(pxz*g11+px2*g13);
		snfact = sxfactor*c-szfactor*s;
		
		/*computing ray velocities and derivatives*/
		dxt =  (a1111*pxt*g11+(a1133+a1313)*pzt*g13+a3313*pzt*g33
			+a1113*(pzt*g11+2*pxt*g13)+a1313*g33*pxt);
		dzt =  (a3333*pzt*g33+(a1133+a1313)*pxt*g13+a1113*pxt*g11
			+a3313*(pxt*g33+2*pzt*g13)+a1313*g11*pzt);
		dpxt = -0.5*sxfactor;
		dpzt = -0.5*szfactor;

		dgamma11dpx = 2*a1111*pxt+2*a1113*pzt;
		dgamma11dpz = 2*a1313*pzt+2*a1113*pxt;
		dgamma33dpx = 2*a1313*pxt+2*a3313*pzt;
		dgamma33dpz = 2*a3333*pzt+2*a3313*pxt;
		dgamma13dpx= (a1133+a1313)*pzt+2*a1113*pxt;
		dgamma13dpz= (a1133+a1313)*pxt+2*a3313*pzt;
		dgamma11dn = da1111dn*px2+ da1313dn*pz2 +2*da1113dn*pxz;
		dgamma33dn = da3333dn*pz2 + da1313dn*px2+2*da3313dn*pxz;
		dgamma13dn = (da1133dn+da1313dn)*pxz+ da1113dn*px2+ da3313dn*pz2;
		dg11dpx = -(gamma33-1)*(dgamma11dpx+dgamma33dpx-4*dxt)*den*den+
			(dgamma33dpx-2*dxt)*den;
		dg11dpz = -(gamma33-1)*(dgamma11dpz+dgamma33dpz-4*dzt)*den*den+
			(dgamma33dpz-2*dzt)*den;
		dg33dpx = -(gamma11-1)*(dgamma11dpx+dgamma33dpx-4*dxt)*den*den+
			(dgamma11dpx-2*dxt)*den;
		dg33dpz = -(gamma11-1)*(dgamma11dpz+dgamma33dpz-4*dzt)*den*den+
			(dgamma11dpz-2*dzt)*den;
		dg13dpx = gamma13*(dgamma11dpx+dgamma33dpx-4*dxt)*den*den-
			dgamma13dpx*den;
		dg13dpz = gamma13*(dgamma11dpz+dgamma33dpz-4*dzt)*den*den-
			dgamma13dpz*den;
		dg11dn = -(gamma33-1)*(dgamma11dn+dgamma33dn-2*snfact)*den*den+
			(dgamma33dn-snfact)*den;
		dg33dn = -(gamma11-1)*(dgamma11dn+dgamma33dn-2*snfact)*den*den+
			(dgamma11dn-snfact)*den;
		dg13dn = gamma13*(dgamma11dn+dgamma33dn-2*snfact)*den*den-
			dgamma13dn*den;
		ddxdpx=   a1111*pxt*dg11dpx+(a1133+a1313)*pzt*dg13dpx+
			a3313*pzt*dg33dpx+a1113*(pzt*dg11dpx+2*pxt*dg13dpx)
			+a1313*dg33dpx*pxt;
		ddzdpz= a3333*pzt*dg33dpz+(a1133+a1313)*pxt*dg13dpz+
			a1113*pxt*dg11dpz+a3313*(pxt*dg33dpz+2*pzt*dg13dpz)+
			a1313*dg11dpz*pzt;
		ddxdpz= a1111*pxt*dg11dpz+(a1133+a1313)*pzt*dg13dpz+
			a3313*pzt*dg33dpz+a1113*(pzt*dg11dpz+2*pxt*dg13dpz)+
			a1313*dg33dpz*pxt;
		dsnfactordn = da1111dn*px2*dg11dn+da3333dn*pz2*dg33dn+
			2*(da1133dn+da1313dn)*pxz*dg13dn+da1313dn*(px2*dg33dn+pz2*dg11dn)+
			2*da3313dn*(pz2*dg13dn+pxz*dg33dn)+2*da1113dn*(pxz*dg11dn+px2*dg13dn);
		ddxdn =  (a1111*pxt*dg11dn+(a1133+a1313)*pzt*dg13dn+a3313*pzt*dg33dn
			+a1113*(pzt*dg11dn+2*pxt*dg13dn)+a1313*dg33dn*pxt);
		ddzdn =  (a3333*pzt*dg33dn+(a1133+a1313)*pxt*dg13dn+a1113*pxt*dg11dn
			+a3313*(pxt*dg33dn+2*pzt*dg13dn)+a1313*dg11dn*pzt);

		
		dcdp1  = a1111*g11+a1313*g33+2*a1113*g13+ddxdpx-dxt*dxt;
		dcdp3  = a3333*g33+a1313*g11+2*a3313*g13+ddzdpz-dzt*dzt;
		dcdp13 = a1133*g13+a1313*g13+a1113*g11+a3313*g33+ddxdpz-dxt*dzt;
		ddcdqq = dcdp1*cc-2.0*dcdp13*s*c+dcdp3*ss;
		dxdn   =  (da1111dn*pxt*g11+(da1133dn+da1313dn)*pzt*g13+
			da3313dn*pzt*g33+da1113dn*(pzt*g11+2*pxt*g13)+
			da1313dn*g33*pxt);
		dzdn   =  (da3333dn*pzt*g33+(da1133dn+da1313dn)*pxt*g13+
			da1113dn*pxt*g11+da3313dn*(pxt*g33+2*pzt*g13)+
			da1313dn*g11*pzt);
		ddcdpn = dxdn*c-dzdn*s-.5*dxt*sxfactor*cc+
			.5*(dxt*szfactor+dzt*sxfactor)*s*c-.5*dzt*szfactor*ss
			+ddxdn*c-ddzdn*s;
		snfactor = dda1111dndn*px2*g11+dda3333dndn*pz2*g33+
			2*(dda1133dndn+dda1313dndn)*pxz*g13+
			dda1313dndn*(px2*g33+pz2*g11)+
			2*dda3313dndn*(pz2*g13+pxz*g33)+
			2*dda1113dndn*(pxz*g11+px2*g13);
		ddcdnn = 0.5*snfactor-.25*sxfactor*sxfactor*cc+
			.5*sxfactor*szfactor*s*c-.25*szfactor*szfactor*ss
			+.5*dsnfactordn;

		dp1t = -ddcdnn*q1t-ddcdpn*p1t;
		dq1t = ddcdqq*p1t+ddcdpn*q1t;
		dp2t = -ddcdnn*q2t-ddcdpn*p2t;
		dq2t = ddcdqq*p2t+ddcdpn*q2t;
		dxx  = hsixth*(dx+dxt+2.0*dxm);
		dzz  = hsixth*(dz+dzt+2.0*dzm);
		x += dxx;
		z += dzz;
		px += hsixth*(dpx+dpxt+2.0*dpxm);
		pz += hsixth*(dpz+dpzt+2.0*dpzm);
		p1 += hsixth*(dp1+dp1t+2.0*dp1m);
		q1 += hsixth*(dq1+dq1t+2.0*dq1m);
		p2 += hsixth*(dp2+dp2t+2.0*dp2m);
		q2 += hsixth*(dq2+dq2t+2.0*dq2m);
		vp  = 1/sqrt(px*px+pz*pz);
		s   = px*vp;
		c   = pz*vp;
		ss  = s*s;
		cc  = c*c;
		
		vel2Interp(a11112,x,z,&a1111,&da1111dx,&da1111dz,&dda1111dxdx,
			&dda1111dxdz,&dda1111dzdz);
		da1111dn    = da1111dx*c-da1111dz*s;
		dda1111dndn = dda1111dxdx*cc-2.0*dda1111dxdz*s*c+dda1111dzdz*ss;

		vel2Interp(a33332,x,z,&a3333,&da3333dx,&da3333dz,&dda3333dxdx,
		&dda3333dxdz,&dda3333dzdz);
		da3333dn    = da3333dx*c-da3333dz*s;
		dda3333dndn = dda3333dxdx*cc-2.0*dda3333dxdz*s*c+dda3333dzdz*ss;
	
		vel2Interp(a11332,x,z,&a1133,&da1133dx,&da1133dz,&dda1133dxdx,
			&dda1133dxdz,&dda1133dzdz);
		da1133dn    = da1133dx*c-da1133dz*s;
		dda1133dndn = dda1133dxdx*cc-2.0*dda1133dxdz*s*c+dda1133dzdz*ss;

		vel2Interp(a13132,x,z,&a1313,&da1313dx,&da1313dz,&dda1313dxdx,
			&dda1313dxdz,&dda1313dzdz);
		da1313dn    = da1313dx*c-da1313dz*s;
		dda1313dndn = dda1313dxdx*cc-2.0*dda1313dxdz*s*c+dda1313dzdz*ss;

		vel2Interp(a11132,x,z,&a1113,&da1113dx,&da1113dz,&dda1113dxdx,
			&dda1113dxdz,&dda1113dzdz);
		da1113dn    = da1113dx*c-da1113dz*s;
		dda1113dndn = dda1113dxdx*cc-2.0*dda1113dxdz*s*c+dda1113dzdz*ss;

		vel2Interp(a33132,x,z,&a3313,&da3313dx,&da3313dz,&dda3313dxdx,
			&dda3313dxdz,&dda3313dzdz);
		da3313dn    = da3313dx*c-da3313dz*s;
		dda3313dndn = dda3313dxdx*cc-2.0*dda3313dxdz*s*c+dda3313dzdz*ss;


		/* update time */
		t  += dt;

		/* update kmah index */
                if ((q2<=0.0 && q2old>0.0) || (q2>=0.0 && q2old<0.0)) kmah++;

		/* save ray parameters */
		rs[it].t = t;
		rs[it].x = x;
		rs[it].z = z;
		rs[it].c = c;
		rs[it].s = s;
		rs[it].q1 = q1;
		rs[it].p1 = p1;
		rs[it].q2 = q2;
		rs[it].p2 = p2;
		rs[it].kmah = kmah;
		rs[it].v = vp;
		rs[it].dvdx = .5*da3333dx*vp/a3333;
		rs[it].dvdz = .5*da3333dz*vp/a3333;
	/*printf("%d %f %f %f %f %f %f %f \n",it,t,x,z,fx,fz,lx,lz);*/
	/*printf("%f %f \n",z,x);*/
	}


	/* free velocity interpolator */
	vel2Free(a11112);
	vel2Free(a33332);
	vel2Free(a11332);
	vel2Free(a13132);
	vel2Free(a11132);
	vel2Free(a33132);

	/* return ray */
	ray->nrs = it;
	ray->rs = rs;
	ray->nc = 0;
	ray->c = NULL;
	return ray;
}

void freeRay (Ray *ray)
/*****************************************************************************
Free a ray.
******************************************************************************
Input:
ray		ray to be freed
*****************************************************************************/
{
	if (ray->c!=NULL) free1((void*)ray->c);
	free1((void*)ray->rs);
	free1((void*)ray);
}


/*****************************************************************************
Functions to support interpolation of velocity and its derivatives.
******************************************************************************
Functions:
vel2Alloc	allocate and initialize an interpolator for v(x,z)
vel2Interp	interpolate v(x,z) and its derivatives
******************************************************************************
Notes:
Interpolation is performed by piecewise cubic Hermite polynomials,
so that velocity and first derivatives are continuous.  Therefore,
velocity v, first derivatives dv/dx and dv/dz, and the mixed
derivative ddv/dxdz are continuous.  However, second derivatives
ddv/dxdx and ddv/dzdz are discontinuous.
*****************************************************************************
Technical Reference:
	Hale, D., 1992, Migration by the Kirchhoff, 
	slant stack, and Gaussian beam methods:
	Colorado School of Mines.
*****************************************************************************
 Credits: 	CWP: Dave Hale
*****************************************************************************/

/* number of pre-computed, tabulated interpolators */
#define NTABLE 101

/* length of each interpolator in table (4 for piecewise cubic) */
#define LTABLE 4

/* table of pre-computed interpolators, for 0th, 1st, and 2nd derivatives */
static float tbl[3][NTABLE][LTABLE];

/* constants */
static int ix=1-LTABLE/2-LTABLE,iz=1-LTABLE/2-LTABLE;
static float ltable=LTABLE,ntblm1=NTABLE-1;

/* indices for 0th, 1st, and 2nd derivatives */
static int kx[6]={0,1,0,2,1,0};
static int kz[6]={0,0,1,0,1,2};

/* function to build interpolator tables; sets tabled=1 when built */
static void buildTables (void);
static int tabled=0;

/* interpolator for velocity function v(x,z) of two variables */
typedef struct Vel2Struct {
	int nx;		/* number of x samples */
	int nz;		/* number of z samples */
	int nxm;	/* number of x samples minus LTABLE */
	int nzm;	/* number of x samples minus LTABLE */
	float xs,xb,zs,zb,sx[3],sz[3],**vu;
} Vel2;

void* vel2Alloc (int nx, float dx, float fx,
	int nz, float dz, float fz, float **v)
/*****************************************************************************
Allocate and initialize an interpolator for v(x,z) and its derivatives.
******************************************************************************
Input:
nx		number of x samples
dx		x sampling interval
fx		first x sample
nz		number of z samples
dz		z sampling interval
fz		first z sample
v		array[nx][nz] of uniformly sampled v(x,z)

*****************************************************************************
Returned:	pointer to interpolator
*****************************************************************************/
{
	Vel2 *vel2;

	/* allocate space */
	vel2 = (Vel2*)alloc1(1,sizeof(Vel2));

	/* set state variables used for interpolation */
	vel2->nx = nx;
	vel2->nxm = nx-LTABLE;
	vel2->xs = 1.0/dx;
	vel2->xb = ltable-fx*vel2->xs;
	vel2->sx[0] = 1.0;
	vel2->sx[1] = vel2->xs;
	vel2->sx[2] = vel2->xs*vel2->xs;
	vel2->nz = nz;
	vel2->nzm = nz-LTABLE;
	vel2->zs = 1.0/dz;
	vel2->zb = ltable-fz*vel2->zs;
	vel2->sz[0] = 1.0;
	vel2->sz[1] = vel2->zs;
	vel2->sz[2] = vel2->zs*vel2->zs;
	vel2->vu = v;
	
	/* if necessary, build interpolator coefficient tables */
	if (!tabled) buildTables();

	return vel2;
}

void vel2Free (void *vel2)
/*****************************************************************************
Free an interpolator for v(x,z) and its derivatives.
******************************************************************************
Input:
vel2		pointer to interpolator as returned by vel2Alloc()
*****************************************************************************/
{
	free1(vel2);
}

void vel2Interp (void *vel2, float x, float z,
	float *v, float *vx, float *vz, float *vxx, float *vxz, float *vzz)
/*****************************************************************************
Interpolation of a velocity function v(x,z) and its derivatives.
******************************************************************************
Input:
vel2		pointer to interpolator as returned by vel2Alloc()
x		x coordinate at which to interpolate v(x,z) and derivatives
z		z coordinate at which to interpolate v(x,z) and derivatives
*****************************************************************************
Output:
v		v(x,z)
vx		dv/dx
vz		dv/dz
vxx		ddv/dxdx
vxz		ddv/dxdz
vzz		ddv/dzdz
*****************************************************************************/
{
	Vel2 *v2=vel2;
	int nx=v2->nx,nz=v2->nz,nxm=v2->nxm,nzm=v2->nzm;
	float xs=v2->xs,xb=v2->xb,zs=v2->zs,zb=v2->zb,
		*sx=v2->sx,*sz=v2->sz,**vu=v2->vu;
	int i,jx,lx,mx,jz,lz,mz,jmx,jmz,mmx,mmz;
	float ax,bx,*px,az,bz,*pz,sum,vd[6];

	/* determine offsets into vu and interpolation coefficients */
	ax = xb+x*xs;
	jx = (int)ax;
	bx = ax-jx;
	lx = (bx>=0.0)?bx*ntblm1+0.5:(bx+1.0)*ntblm1-0.5;
	lx *= LTABLE;
	mx = ix+jx;
	az = zb+z*zs;
	jz = (int)az;
	bz = az-jz;
	lz = (bz>=0.0)?bz*ntblm1+0.5:(bz+1.0)*ntblm1-0.5;
	lz *= LTABLE;
	mz = iz+jz;
	
	/* if totally within input array, use fast method */
	if (mx>=0 && mx<=nxm && mz>=0 && mz<=nzm) {
		for (i=0; i<6; ++i) {
			px = &(tbl[kx[i]][0][0])+lx;
			pz = &(tbl[kz[i]][0][0])+lz;
			vd[i] = sx[kx[i]]*sz[kz[i]]*(
				vu[mx][mz]*px[0]*pz[0]+
				vu[mx][mz+1]*px[0]*pz[1]+
				vu[mx][mz+2]*px[0]*pz[2]+
				vu[mx][mz+3]*px[0]*pz[3]+
				vu[mx+1][mz]*px[1]*pz[0]+
				vu[mx+1][mz+1]*px[1]*pz[1]+
				vu[mx+1][mz+2]*px[1]*pz[2]+
				vu[mx+1][mz+3]*px[1]*pz[3]+
				vu[mx+2][mz]*px[2]*pz[0]+
				vu[mx+2][mz+1]*px[2]*pz[1]+
				vu[mx+2][mz+2]*px[2]*pz[2]+
				vu[mx+2][mz+3]*px[2]*pz[3]+
				vu[mx+3][mz]*px[3]*pz[0]+
				vu[mx+3][mz+1]*px[3]*pz[1]+
				vu[mx+3][mz+2]*px[3]*pz[2]+
				vu[mx+3][mz+3]*px[3]*pz[3]);
		}
		
	/* else handle end effects with constant extrapolation */
	} else {
		for (i=0; i<6; ++i) {
			px = &(tbl[kx[i]][0][0])+lx;
			pz = &(tbl[kz[i]][0][0])+lz;
			for (jx=0,jmx=mx,sum=0.0; jx<4; ++jx,++jmx) {
				mmx = jmx;
				if (mmx<0) mmx = 0;
				else if (mmx>=nx) mmx = nx-1;
				for (jz=0,jmz=mz; jz<4; ++jz,++jmz) {
					mmz = jmz;
					if (mmz<0) mmz = 0;
					else if (mmz>=nz) mmz = nz-1;
					sum += vu[mmx][mmz]*px[jx]*pz[jz];
				}
			}
			vd[i] = sx[kx[i]]*sz[kz[i]]*sum;
		}
	}

	/* set output variables */
	*v = vd[0];
	*vx = vd[1];
	*vz = vd[2];
	*vxx = vd[3];
	*vxz = vd[4];
	*vzz = vd[5];
}

/* hermite polynomials */
static float h00 (float x) {return 2.0*x*x*x-3.0*x*x+1.0;}
static float h01 (float x) {return 6.0*x*x-6.0*x;}
static float h02 (float x) {return 12.0*x-6.0;}
static float h10 (float x) {return -2.0*x*x*x+3.0*x*x;}
static float h11 (float x) {return -6.0*x*x+6.0*x;}
static float h12 (float x) {return -12.0*x+6.0;}
static float k00 (float x) {return x*x*x-2.0*x*x+x;}
static float k01 (float x) {return 3.0*x*x-4.0*x+1.0;}
static float k02 (float x) {return 6.0*x-4.0;}
static float k10 (float x) {return x*x*x-x*x;}
static float k11 (float x) {return 3.0*x*x-2.0*x;}
static float k12 (float x) {return 6.0*x-2.0;}

/* function to build interpolation tables */
static void buildTables(void)
{
	int i;
	float x;
	
	/* tabulate interpolator for 0th derivative */
	for (i=0; i<NTABLE; ++i) {
		x = (float)i/(NTABLE-1.0);
		tbl[0][i][0] = -0.5*k00(x);
		tbl[0][i][1] = h00(x)-0.5*k10(x);
		tbl[0][i][2] = h10(x)+0.5*k00(x);
		tbl[0][i][3] = 0.5*k10(x);
		tbl[1][i][0] = -0.5*k01(x);
		tbl[1][i][1] = h01(x)-0.5*k11(x);
		tbl[1][i][2] = h11(x)+0.5*k01(x);
		tbl[1][i][3] = 0.5*k11(x);
		tbl[2][i][0] = -0.5*k02(x);
		tbl[2][i][1] = h02(x)-0.5*k12(x);
		tbl[2][i][2] = h12(x)+0.5*k02(x);
		tbl[2][i][3] = 0.5*k12(x);
	}
	
	/* remember that tables have been built */
	tabled = 1;
}

#ifdef TEST2
#include "cwp.h"
#define NZ 2
#define NX 2
#define NXOUT 21
#define NZOUT 21
main()
{
	int nx=NX,nz=NZ,nxout=NXOUT,nzout=NZOUT,i,j;
	float dx=2.0,fx=-1.0,dxout=0.2,fxout=-2.0;
	float dz=4.0,fz=-2.0,dzout=0.4,fzout=-4.0;
	float x,z,v,vx,vz,vxx,vxz,vzz,**vu,**vo;
	void *vel2;

	vu = alloc2float(nz,nx);
	vo = alloc2float(nzout,nxout);

	vu[0][0] = 1.0;
	vu[1][0] = 2.0;
	vu[0][1] = 1.0;
	vu[1][1] = 2.0;

	vel2 = vel2Alloc(nx,dx,fx,nz,dz,fz,vu);

	for (i=0; i<nxout; ++i) {
		x = fxout+i*dxout;
		for (j=0; j<nzout; ++j) {
			z = fzout+j*dzout;
			vel2Interp(vel2,x,z,&v,&vx,&vz,&vxx,&vxz,&vzz);
			vo[i][j] = vz;
		}
	}
	vel2Free(vel2);
	fwrite(vo[0],sizeof(float),nxout*nzout,stdout);
}
#endif

void conjugate_gradient(float **A, float *x, float *b, int nrx, float tol)
/**************************************************************************
conjugate_gradient      - Conjugate gradient solver Ax=B
***************************************************************************
conjugate_gradient
Input:
A       square matrix 
b       vector
tol	tolerance

nrx	number of rows of matrix
Output:
x	solution vector	
*************************************************************************
*************************************************************************
Credits: CWP:  Debashish Sarkar,  July 2002 
************************************************************************/
{
	float norm_b=0.0;
	float norm_r=0.0;
	float sum1;
	float sum2;
	float alpha;
	float beta;
	float *q=NULL;
	float *r=NULL;
	float *r_1=NULL;
	float *p=NULL;

	int i,j,k;

	/* Allocate space */
	q = alloc1float(nrx);
	r = alloc1float(nrx);
	r_1 = alloc1float(nrx);
	p = alloc1float(nrx);

	/* Zero out x[] array */
	memset( (void *) x, 0, FSIZE*nrx);
	/* Compute r=Ax+b */
	for(i=0;i<nrx;i++)
		for(j=0;j<nrx;j++)
              		r[i] = b[i]-A[i][j]*x[j];

	/* Compute the norm b */
	for(i=0;i<nrx;i++) norm_b +=  b[i]*b[i];

	norm_b = sqrt(norm_b);
	k=0;
	norm_r=norm_b;
	printf("%f %f\n",tol*norm_b,sqrt(norm_r));
	/* Start the iteration */
	while(sqrt(norm_r)>tol*norm_b){
		norm_r=0;
		sum1=0.0;
		sum2=0.0;

		for(i=0;i<nrx;i++) norm_r += r[i]*r[i];
	/*printf("%f \n",norm_r);*/
		++k;
		if(k==1) {
			for(i=0;i<nrx;i++) p[i]=r[i];
		} else { 
			for(i=0;i<nrx;i++) sum2 += r_1[i]*r_1[i];
				beta = norm_r/sum2;

			for(i=0;i<nrx;i++) p[i]=r[i]+beta*p[i];
		}

		/* Zero out q[] array */
		memset( (void *) q, 0, FSIZE*nrx);

		/* Compute Ap */
                for(i=0;i<nrx;i++)
                     for(j=0;j<nrx;j++)
                           q[i] += A[i][j]*p[j];

		/* Compute alpha */
                for(i=0;i<nrx;i++) sum2 += p[i]*q[i];
                          
		alpha = norm_r/sum2;

		for(i=0;i<nrx;i++){
                        x[i] += alpha*p[i];
                        r_1[i] = r[i];
                         r[i] -= alpha*q[i];
                 }
        }
	free1float(q);
	free1float(r);
	free1float(r_1);
	free1float(p);
}


void smooth2 (int n1, int n2, float r1, float r2, int *win, float rw, float **v)
/**************************************************************************
smooth2 --> smooths the parameter field **v depending on the values of r1 and r2
***************************************************************************
conjugate_gradient
Input:
v       parameter field 
n1      number of samples in the fast direction 
n2	number of samples in the slow direction
r1	smoothing parameter in the fast direction
r2	smoothing parameter in the slow direction
win	1d array defining the corners of smoothing window 
rw	smoothing parameter for window 

*************************************************************************
*************************************************************************
Credits: CWP:  Zhenyue Liu, 1992 
************************************************************************/
{
	int nmax;	/* max of n1 and n2 */
	int ix, iz;	/* counters */
	float **w;	/* intermediate array */
	float *errz;	/* array of error estimates as a function of x1 */
	float *d, *e;	/* input arrays for subroutine tripd */
	float *f;	/* intermediate array */

	/* scale the smoothing parameter */
	r1 = r1*r1*0.25;
	r2 = r2*r2*0.25;

	/* allocate space */
	nmax = (n1<n2)?n2:n1;
	w = alloc2float(n1,n2);
	errz = alloc1float(nmax);
	d = alloc1float(nmax);
	e = alloc1float(nmax);
	f = alloc1float(nmax);

	rw = rw*rw*0.25;
 
	/* define the window function */
	for(ix=0; ix<n2; ++ix)
	 	for(iz=0; iz<n1; ++iz)
			w[ix][iz] = 0;	
	for(ix=win[2]; ix<win[3]; ++ix)
	 	for(iz=win[0]; iz<win[1]; ++iz)
			w[ix][iz] = 1;	

	if(win[0]>0 || win[1]<n1 || win[2]>0 || win[3]<n2){
	/*	smooth the window function */
         	for(iz=0; iz<n1; ++iz){
	 		for(ix=0; ix<n2; ++ix){
				d[ix] = 1.0+2.0*rw;
				e[ix] = -rw;
				f[ix] = w[ix][iz];
			}
        		d[0] -= rw;
         		d[n2-1] -= rw;
         		tripd(d,e,f,n2);
	 		for(ix=0; ix<n2; ++ix)
				w[ix][iz] = f[ix];
		}
         	for(ix=0; ix<n2; ++ix){
	 		for(iz=0; iz<n1; ++iz){
				d[iz] = 1.0+2.0*rw;
				e[iz] = -rw;
				f[iz] = w[ix][iz];
		}
        		d[0] -= rw;
         		d[n1-1] -= rw;
         		tripd(d,e,f,n1);
	 		for(iz=0; iz<n1; ++iz)
				w[ix][iz] = f[iz];
		}
	}

	/*      solving for the smoothing velocity */
        for(iz=0; iz<n1; ++iz){
	 	for(ix=0; ix<n2-1; ++ix){
			d[ix] = 1.0+r2*(w[ix][iz]+w[ix+1][iz]);
			e[ix] = -r2*w[ix+1][iz];
			f[ix] = v[ix][iz];
		}
        	d[0] -= r2*w[0][iz];
         	d[n2-1] = 1.0+r2*w[n2-1][iz];
		f[n2-1] = v[n2-1][iz];
         	tripd(d,e,f,n2);
	 	for(ix=0; ix<n2; ++ix)
			v[ix][iz] = f[ix];
	}
         for(ix=0; ix<n2; ++ix){
	 	for(iz=0; iz<n1-2; ++iz){
			d[iz] = 1.0+r1*(w[ix][iz+1]+w[ix][iz+2]);
			e[iz] = -r1*w[ix][iz+2];
			f[iz] = v[ix][iz+1];
		}
		f[0] += r1*w[ix][1]*v[ix][0];
         	d[n1-2] = 1.0+r1*w[ix][n1-1];
		f[n1-2] = v[ix][n1-1];
         	tripd(d,e,f,n1-1);
	 	for(iz=0; iz<n1-1; ++iz)
			v[ix][iz+1] = f[iz];
	}
	free2float(w);
	free1float(errz);
	free1float(d);
	free1float(e);
	free1float(f);

}
        
