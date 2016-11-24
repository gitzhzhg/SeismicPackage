/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMIGGBZO: $Revision: 1.15 $ ; $Date: 2011/11/16 22:14:43 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUMIGGBZO - MIGration via Gaussian Beams of Zero-Offset SU data	",
"									",
" sumiggbzo <infile >outfile vfile=  nz= [optional parameters]		",
"									",
" Required Parameters:							",
" vfile=                 name of file containing v(z,x)			",
" nz=                    number of depth samples			",
"									",
" Optional Parameters:							",
" dt=from header		time sampling interval			",
" dx=from header(d2) or 1.0	spatial sampling interval 		",
" dz=1.0                 depth sampling interval			",
" fmin=0.025/dt          minimum frequency				",
" fmax=10*fmin           maximum frequency				",
" amin=-amax             minimum emergence angle; must be > -90 degrees	",
" amax=60                maximum emergence angle; must be < 90 degrees	",
" bwh=0.5*vavg/fmin      beam half-width; vavg denotes average velocity	",
" verbose=0		 =0 silent; =1 chatty				",
"									",
" Note: spatial units of v(z,x) must be the same as those of dx.	",
" v(z,x) is represented numerically in C-style binary floats v[x][z],	",
" where the depth direction is the fast direction in the data. Such	",
" models can be created with unif2 or makevel.				",
"									",
"(In C  v[iz][ix] denotes a v(x,z) array, whereas v[ix][iz]  		",
" denotes a v(z,x) array, the opposite of what Matlab and Fortran	",
" programmers may expect.)						", 
"									",
" Caveat:								",
" In the event of a \"Segmentation Violation\" try reducing the value of",
" the \"bwh\" parameter. Run program with verbose=1 do see the default	",
" value.								",
NULL};

/* Credits:
 *
 * CWP: Dave Hale (algorithm), Jack K. Cohen, and John Stockwell
 * (reformatting for SU)
 */

/**************** end self doc ***********************************/

/* Ray types */
/* one step along ray */
typedef struct RayStepStruct {
	float t;		/* time */
	float a;		/* angle */
	float x,z;		/* x,z coordinates */
	float q1,p1,q2,p2;	/* Cerveny's dynamic ray tracing solution */
	int kmah;		/* KMAH index */
	float c,s;		/* cos(angle) and sin(angle) */
	float v,dvdx,dvdz;	/* velocity and its derivatives */
} RayStep;

/* one ray */
typedef struct RayStruct {
	int nrs;		/* number of ray steps */
	RayStep *rs;		/* array[nrs] of ray steps */
	int nc;			/* number of circles */
	int ic;			/* index of circle containing nearest step */
	void *c;		/* array[nc] of circles */
} Ray;

/* Ray functions */
Ray *makeRay (float x0, float z0, float a0, int nt, float dt, float ft,
	int nx, float dx, float fx, int nz, float dz, float fz, float **v);
void freeRay (Ray *ray);
int nearestRayStep (Ray *ray, float x, float z);

/* Velocity functions */
void* vel2Alloc (int nx, float dx, float fx,
	int nz, float dz, float fz, float **v);
void vel2Free (void *vel2);
void vel2Interp (void *vel2, float x, float z,
	float *v, float *vx, float *vz, float *vxx, float *vxz, float *vzz);

/* Beam functions */
void formBeams (float bwh, float dxb, float fmin,
	int nt, float dt, float ft, 
	int nx, float dx, float fx, float **f,
	int ntau, float dtau, float ftau, 
	int npx, float dpx, float fpx, float **g);
void accBeam (Ray *ray, float fmin, float lmin,
	int nt, float dt, float ft, float *f,
	int nx, float dx, float fx, int nz, float dz, float fz, float **g);

/* functions defined and used internally */
static void miggbzo (float bwh, float fmin, float fmax, float amin, float amax,
	int nt, float dt, int nx, float dx, int nz, float dz, 
	float **f, float **v, float **g, int verbose);

segy tr;

int
main(int argc, char **argv)
{
	int nx;		/* number of horizontal samples (traces) in input */
	int nz;		/* number of vertical samples in output	          */	
	int nt;		/* number of time samples in input		  */
	int ix;		/* counter in x					  */
	int iz;		/* counter in z					  */

	float dx;	/* trace spacing in input			  */
	float dz;	/* vertical sampling interval in output		  */
	float dt;	/* time sampling interval in input	          */

	float fmin;	/* minimum frequency in migration		  */
	float fmax;     /* maximum frequency in migration 		  */
	float amin;	/* minimum ray angle				  */
	float amax;     /* maximum ray angle 			          */
	float vavg;	/* average velocity in input vfile		  */

	float bwh;	/* gaussian beam half-width			  */

	float **v=NULL;	/* pointer to background velocities		  */
	float **f=NULL; /* pointer to input data                          */
	float **g=NULL; /* pointer to migrated data 			  */

	char *vfile="";		/* velocity filename		*/
	FILE *vfp;		/* velocity file pointer	*/
	FILE *tracefp;		/* temp file to hold traces	*/
	int verbose;		/* verbose flag			*/

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(0);


	/* get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;

	/* get required parameters */
	MUSTGETPARSTRING("vfile", &vfile);
	MUSTGETPARINT("nz", &nz);
	MUSTGETPARFLOAT("dz", &dz);
	if (!getparint("verbose",&verbose))	verbose = 0;


	/* let user give dt and/or dx from command line */
	if (!getparfloat("dt", &dt)) {
		if (tr.dt) { /* is dt field set? */
			dt = (float) tr.dt / 1000000.0;
		} else { /* dt not set, assume 4 ms */
			dt = 0.004;
			if (verbose) warn("tr.dt not set, assuming dt=0.004");
		}
	}
	if (!getparfloat("dx",&dx)) {
		if (tr.d2) { /* is d2 field set? */
			dx = tr.d2;
		} else {
			dx = 1.0;
			if (verbose) warn("tr.d2 not set, assuming d2=1.0");
		}
	}

	
	/* get optional parameters */
	if (!getparfloat("dz",&dz)) dz = 1.0;
	if (!getparfloat("fmin",&fmin)) fmin = 0.025/dt;
	if (!getparfloat("fmax",&fmax)) fmax = 10.0*fmin;
	if (!getparfloat("amax",&amax)) amax = 60.0;
	if (!getparfloat("amin",&amin)) amin = -amax;

	/* store traces in tmpfile while getting a count */
	tracefp = etmpfile();
	nx = 0;
	do { 
		++nx;
		efwrite(tr.data, FSIZE, nt, tracefp);
	} while (gettr(&tr));


	/* allocate workspace */
	f = ealloc2float(nt,nx);
	v = ealloc2float(nz,nx);
	g = ealloc2float(nz,nx);

	/* load traces into the zero-offset array and close tmpfile */
	rewind(tracefp);
	efread(*f, FSIZE, nt*nx, tracefp);
	efclose(tracefp);

	/* read and halve velocities; determine average */
	vfp = efopen(vfile,"r");
	if (efread(*v, FSIZE, nz*nx, vfp)!=nz*nx)
		err("error reading vfile=%s!\n",vfile);
	for (ix=0,vavg=0.0; ix<nx; ++ix) {
		for (iz=0; iz<nz; ++iz) {
			v[ix][iz] *= 0.5;
			vavg += v[ix][iz];
		}
	}
	vavg /= nx*nz;

	/* get beam half-width */
	if (!getparfloat("bwh",&bwh)) bwh = vavg/fmin;
	
        checkpars();

	if (verbose) warn("bhw=%f",bwh);
	
	/* migrate */
	miggbzo(bwh,fmin,fmax,amin,amax,nt,dt,nx,dx,nz,dz,f,v,g,verbose);
	
	/* set header fields and write output */
	tr.ns = nz;
	tr.trid = TRID_DEPTH;
	tr.d1 = dz;
	tr.d2 = dx;
	
	for (ix=0; ix<nx; ++ix) {
		tr.tracl = ix + 1;
		tr.tracr = ix + 1;
		for (iz=0; iz<nz; ++iz) {
			tr.data[iz] = g[ix][iz];
		}
		puttr(&tr);
	}

	/* free workspace */
	free2float(f);
	free2float(v);
	free2float(g);
	
	return(CWP_Exit());
}

static void miggbzo (float bwh, float fmin, float fmax, float amin, float amax,
	int nt, float dt, int nx, float dx, int nz, float dz, 
	float **f, float **v, float **g, int verbose)
/*****************************************************************************
Migrate zero-offset data via accumulation of Gaussian beams.
******************************************************************************
Input:
bwh		horizontal beam half-width at surface z=0
fmin		minimum frequency (cycles per unit time)
fmax		maximum frequency (cycles per unit time)
amin		minimum emergence angle at surface z=0 (degrees)
amax		maximum emergence angle at surface z=0 (degrees)
nt		number of time samples
dt		time sampling interval (first time assumed to be zero)
nx		number of x samples
dx		x sampling interval
nz		number of z samples
dz		z sampling interval
f		array[nx][nt] containing zero-offset data f(t,x)
v		array[nx][nz] containing half-velocities v(x,z)

Output:
g		array[nx][nz] containing migrated image g(x,z)
*****************************************************************************/
{
	int nxb=0,npx=0,ntau=0,ipx,ix,ixb,ixlo,ixhi,nxw,iz;
	float ft,fx,fz,xwh,dxb=0,fxb,xb,vmin,dpx,fpx,px,
		taupad,dtau,ftau,fxw,pxmin,pxmax,
		a0,x0,z0,bwhc,**b;
	Ray *ray;

	/* first t, x, and z assumed to be zero */
	ft = fx = fz = 0.0;

	/* convert minimum and maximum angles to radians */
	amin *= PI/180.0;
	amax *= PI/180.0;
	if (amin>amax) {
		float atemp=amin;
		amin = amax;
		amax = atemp;
	}
	
	/* window half-width */
	xwh = 3.0*bwh;

	/* beam center sampling */
	dxb = (float) (NINT((bwh*sqrt(2.0*fmin/fmax))/dx)*dx);
	nxb = (int) (1 + (nx-1)*dx/dxb);
	fxb = fx+0.5*((nx-1)*dx-(nxb-1)*dxb);

	if (verbose) warn("nxb=%d dxb=%f fxb=%f ",nxb,dxb,fxb);

	/* minimum velocity at surface z=0 */
	for (ix=1,vmin=v[0][0]; ix<nx; ++ix)
		if (v[ix][0]<vmin) vmin = v[ix][0];
	if (verbose) warn("vmin=%f fmin=%f fmax=%f",vmin,fmin,fmax);

	/* beam sampling */
	pxmin = sin(amin)/vmin;
	pxmax = sin(amax)/vmin;
	dpx = 1.0/(2.0*xwh*sqrt(fmin*fmax));
	npx = 1+(pxmax-pxmin)/dpx;
	fpx = pxmin+0.5*(pxmax-pxmin-(npx-1)*dpx);
	taupad = MAX(ABS(pxmin),ABS(pxmax))*xwh;
	taupad = NINT(taupad/dt)*dt;
	ftau = ft-taupad;
	dtau = dt;
	ntau = nt+2.0*taupad/dtau;

	if (verbose) warn("npx=%d dpx=%g fpx=%g",npx,dpx,fpx);
	if (verbose) warn("pxmin=%f pxmax=%f",pxmin,pxmax);
	if (verbose) warn("amin=%f amax=%f",amin,amax);

	/* zero migrated image */
	for (ix=0; ix<nx; ++ix)
		for (iz=0; iz<nz; ++iz)
			g[ix][iz] = 0.0;

	/* loop over beam centers */
	for (ixb=0,xb=fxb; ixb<nxb; ++ixb,xb+=dxb) {

		/* horizontal window */
		ix = NINT((xb-fx)/dx);
		ixlo = MAX(ix-NINT(xwh/dx),0);
		ixhi = MIN(ix+NINT(xwh/dx),nx-1);
		nxw = 1+ixhi-ixlo;
		fxw = fx+(ixlo-ix)*dx;

		if (verbose) warn("ixb/nxb = %d/%d  ix = %d",ixb,nxb,ix);

		/* allocate space for beams */
		b = alloc2float(ntau,npx);

		/* form beams at surface */
		formBeams(bwh,dxb,fmin,
			nt,dt,ft,nxw,dx,fxw,&f[ixlo],
			ntau,dtau,ftau,npx,dpx,fpx,b);
		
		/* loop over beams */
		for (ipx=0,px=fpx; ipx<npx; ++ipx,px+=dpx) {

			/* sine of emergence angle; skip if out of bounds */
			if (px*v[ix][0]>sin(amax)+0.01) continue;
			if (px*v[ix][0]<sin(amin)-0.01) continue;

			/* emergence angle and location */
			a0 = -asin(px*v[ix][0]);
			x0 = fx+ix*dx;
			z0 = fz;

			/* beam half-width adjusted for cosine of angle */
			bwhc = bwh*cos(a0);

			/* trace ray */
			ray = makeRay(x0,z0,a0,nt,dt,ft,nx,dx,fx,nz,dz,fz,v);

			/* accumulate contribution of beam in migrated image */
			accBeam(ray,fmin,bwhc,
				ntau,dtau,ftau,b[ipx],
				nx,dx,fx,nz,dz,fz,g);
			/*
			fwrite(g[0],sizeof(float),nx*nz,stdout);
			*/
			
			/* free ray */
			freeRay(ray);
		}
		/*
		fwrite(g[0],sizeof(float),nx*nz,stdout);
		*/

		/* free space for beams */
		free2float(b);
	}
}

/* circle for efficiently finding nearest ray step */
typedef struct CircleStruct {
	int irsf;               /* index of first raystep in circle */
	int irsl;               /* index of last raystep in circle */
	float x;                /* x coordinate of center of circle */
	float z;                /* z coordinate of center of circle */
	float r;                /* radius of circle */
} Circle;

/* functions defined and used internally */
Circle *makeCircles (int nc, int nrs, RayStep *rs);

Ray *makeRay (float x0, float z0, float a0, int nt, float dt, float ft,
	int nx, float dx, float fx, int nz, float dz, float fz, float **vxz)
/*****************************************************************************
Trace a ray for uniformly sampled v(x,z).
******************************************************************************
Input:
x0		x coordinate of takeoff point
z0		z coordinate of takeoff point
a0		takeoff angle (radians)
nt		number of time samples
dt		time sampling interval
ft		first time sample
nx		number of x samples
dx		x sampling interval
fx		first x sample
nz		number of z samples
dz		z sampling interval
fz		first z sample
vxz		array[nx][nz] of uniformly sampled velocities v(x,z)

Returned:	pointer to ray parameters sampled at discrete ray steps
******************************************************************************
Notes:
The ray ends when it runs out of time (after nt steps) or with the first 
step that is out of the (x,z) bounds of the velocity function v(x,z).
*****************************************************************************/
{
	int it,kmah;
	float t,x,z,a,c,s,p1,q1,p2,q2,
		lx,lz,cc,ss,
		v,dvdx,dvdz,ddvdxdx,ddvdxdz,ddvdzdz,
		vv,ddvdndn;
	Ray *ray;
	RayStep *rs;
	void *vel2;

	/* allocate and initialize velocity interpolator */
	vel2 = vel2Alloc(nx,dx,fx,nz,dz,fz,vxz);

	/* last x and z in velocity model */
	lx = fx+(nx-1)*dx;
	lz = fz+(nz-1)*dz;

	/* ensure takeoff point is within model */
	if (x0<fx || x0>lx || z0<fz || z0>lz) return NULL;

	/* allocate space for ray and raysteps */
	ray = (Ray*)alloc1(1,sizeof(Ray));
	rs = (RayStep*)alloc1(nt,sizeof(RayStep));

	/* cosine and sine of takeoff angle */
	c = cos(a0);
	s = sin(a0);
	cc = c*c;
	ss = s*s;

	/* velocity and derivatives at takeoff point */
	vel2Interp(vel2,x0,z0,&v,&dvdx,&dvdz,&ddvdxdx,&ddvdxdz,&ddvdzdz);
	ddvdndn = ddvdxdx*cc-2.0*ddvdxdz*s*c+ddvdzdz*ss;
	vv = v*v;

	/* first ray step */
	rs[0].t = t = ft;
	rs[0].a = a = a0;
	rs[0].x = x = x0;
	rs[0].z = z = z0;
	rs[0].q1 = q1 = 1.0;
	rs[0].p1 = p1 = 0.0;
	rs[0].q2 = q2 = 0.0;
	rs[0].p2 = p2 = 1.0;
	rs[0].kmah = kmah = 0;
	rs[0].c = c;
	rs[0].s = s;
	rs[0].v = v;
	rs[0].dvdx = dvdx;
	rs[0].dvdz = dvdz;

	/* loop over time steps */
	for (it=1; it<nt; ++it) {

		/* variables used for Runge-Kutta integration */
		float h=dt,hhalf=dt/2.0,hsixth=dt/6.0,
			q2old,xt,zt,at,p1t,q1t,p2t,q2t,
			dx,dz,da,dp1,dq1,dp2,dq2,
			dxt,dzt,dat,dp1t,dq1t,dp2t,dq2t,
			dxm,dzm,dam,dp1m,dq1m,dp2m,dq2m;

		/* if ray is out of bounds, break */
		if (x<fx || x>lx || z<fz || z>lz) break;

		/* remember old q2 */
		q2old = q2;
		
		/* step 1 of 4th-order Runge-Kutta */
		dx =  v*s;
		dz =  v*c;
		da =  dvdz*s-dvdx*c;
		dp1 = -ddvdndn*q1/v;
		dq1 = vv*p1;
		dp2 = -ddvdndn*q2/v;
		dq2 =  vv*p2;
		xt = x+hhalf*dx;
		zt = z+hhalf*dz;
		at = a+hhalf*da;
		p1t = p1+hhalf*dp1;
		q1t = q1+hhalf*dq1;
		p2t = p2+hhalf*dp2;
		q2t = q2+hhalf*dq2;
		c = cos(at);
		s = sin(at);
		cc = c*c;
		ss = s*s;
		vel2Interp(vel2,xt,zt,
			&v,&dvdx,&dvdz,&ddvdxdx,&ddvdxdz,&ddvdzdz);
		ddvdndn = ddvdxdx*cc-2.0*ddvdxdz*s*c+ddvdzdz*ss;
		vv = v*v;
		
		/* step 2 of 4th-order Runge-Kutta */
		dxt =  v*s;
		dzt =  v*c;
		dat =  dvdz*s-dvdx*c;
		dp1t = -ddvdndn*q1t/v;
		dq1t = vv*p1t;
		dp2t = -ddvdndn*q2t/v;
		dq2t =  vv*p2t;
		xt = x+hhalf*dxt;
		zt = z+hhalf*dzt;
		at = a+hhalf*dat;
		p1t = p1+hhalf*dp1t;
		q1t = q1+hhalf*dq1t;
		p2t = p2+hhalf*dp2t;
		q2t = q2+hhalf*dq2t;
		c = cos(at);
		s = sin(at);
		cc = c*c;
		ss = s*s;
		vel2Interp(vel2,xt,zt,
			&v,&dvdx,&dvdz,&ddvdxdx,&ddvdxdz,&ddvdzdz);
		ddvdndn = ddvdxdx*cc-2.0*ddvdxdz*s*c+ddvdzdz*ss;
		vv = v*v;
		
		/* step 3 of 4th-order Runge-Kutta */
		dxm =  v*s;
		dzm =  v*c;
		dam =  dvdz*s-dvdx*c;
		dp1m = -ddvdndn*q1t/v;
		dq1m = vv*p1t;
		dp2m = -ddvdndn*q2t/v;
		dq2m =  vv*p2t;
		xt = x+h*dxm;
		zt = z+h*dzm;
		at = a+h*dam;
		p1t = p1+h*dp1m;
		q1t = q1+h*dq1m;
		p2t = p2+h*dp2m;
		q2t = q2+h*dq2m;
		dxm += dxt;
		dzm += dzt;
		dam += dat;
		dp1m += dp1t;
		dq1m += dq1t;
		dp2m += dp2t;
		dq2m += dq2t;
		c = cos(at);
		s = sin(at);
		cc = c*c;
		ss = s*s;
		vel2Interp(vel2,xt,zt,
			&v,&dvdx,&dvdz,&ddvdxdx,&ddvdxdz,&ddvdzdz);
		ddvdndn = ddvdxdx*cc-2.0*ddvdxdz*s*c+ddvdzdz*ss;
		vv = v*v;
		
		/* step 4 of 4th-order Runge-Kutta */
		dxt =  v*s;
		dzt =  v*c;
		dat =  dvdz*s-dvdx*c;
		dp1t = -ddvdndn*q1t/v;
		dq1t = vv*p1t;
		dp2t = -ddvdndn*q2t/v;
		dq2t =  vv*p2t;
		x += hsixth*(dx+dxt+2.0*dxm);
		z += hsixth*(dz+dzt+2.0*dzm);
		a += hsixth*(da+dat+2.0*dam);
		p1 += hsixth*(dp1+dp1t+2.0*dp1m);
		q1 += hsixth*(dq1+dq1t+2.0*dq1m);
		p2 += hsixth*(dp2+dp2t+2.0*dp2m);
		q2 += hsixth*(dq2+dq2t+2.0*dq2m);
		c = cos(a);
		s = sin(a);
		cc = c*c;
		ss = s*s;
		vel2Interp(vel2,x,z,
			&v,&dvdx,&dvdz,&ddvdxdx,&ddvdxdz,&ddvdzdz);
		ddvdndn = ddvdxdx*cc-2.0*ddvdxdz*s*c+ddvdzdz*ss;
		vv = v*v;

		/* update kmah index */
		if ((q2<=0.0 && q2old>0.0) || (q2>=0.0 && q2old<0.0)) kmah++;
		
		/* update time */
		t += dt;

		/* save ray parameters */
		rs[it].t = t;
		rs[it].a = a;
		rs[it].x = x;
		rs[it].z = z;
		rs[it].c = c;
		rs[it].s = s;
		rs[it].q1 = q1;
		rs[it].p1 = p1;
		rs[it].q2 = q2;
		rs[it].p2 = p2;
		rs[it].kmah = kmah;
		rs[it].v = v;
		rs[it].dvdx = dvdx;
		rs[it].dvdz = dvdz;
	}

	/* free velocity interpolator */
	vel2Free(vel2);

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

int nearestRayStep (Ray *ray, float x, float z)
/*****************************************************************************
Determine index of ray step nearest to point (x,z).
******************************************************************************
Input:
ray		ray
x		x coordinate
z		z coordinate

Returned:	index of nearest ray step
*****************************************************************************/
{
	int nrs=ray->nrs,ic=ray->ic,nc=ray->nc;
	RayStep *rs=ray->rs;
	Circle *c=ray->c;
	int irs,irsf,irsl,irsmin=0,update,jc,js,kc;
	float dsmin,ds,dx,dz,dmin,rdmin,xrs,zrs;

	/* if necessary, make circles localizing ray steps */
	if (c==NULL) {
		ray->ic = ic = 0;
		ray->nc = nc = sqrt((float)nrs);
		ray->c = c = makeCircles(nc,nrs,rs);
	}
	
	/* initialize minimum distance and minimum distance-squared */
	dx = x-c[ic].x;
	dz = z-c[ic].z;
	dmin = 2.0*(sqrt(dx*dx+dz*dz)+c[ic].r);
	dsmin = dmin*dmin;

	/* loop over all circles */
	for (kc=0,jc=ic,js=0; kc<nc; ++kc) {
		
		/* distance-squared to center of circle */
		dx = x-c[jc].x;
		dz = z-c[jc].z;
		ds = dx*dx+dz*dz;

		/* radius of circle plus minimum distance (so far) */
		rdmin = c[jc].r+dmin;

		/* if circle could possible contain a nearer ray step */
		if (ds<=rdmin*rdmin) {

			/* search circle for nearest ray step */ 
			irsf = c[jc].irsf;
			irsl = c[jc].irsl;
			update = 0;
			for (irs=irsf; irs<=irsl; ++irs) {
				xrs = rs[irs].x;
				zrs = rs[irs].z;
				dx = x-xrs;
				dz = z-zrs;
				ds = dx*dx+dz*dz;
				if (ds<dsmin) {
					dsmin = ds;
					irsmin = irs;
					update = 1;
				}
			}

			/* if a nearer ray step was found inside circle */
			if (update) {

				/* update minimum distance */
				dmin = sqrt(dsmin);

				/* remember the circle */
				ic = jc;
			}
		}
		
		/* search circles in alternating directions */
		js = (js>0)?-js-1:-js+1;
		jc += js;
		if (jc<0 || jc>=nc) {
			js = (js>0)?-js-1:-js+1;
			jc += js;
		}
	}

	/* remember the circle containing the nearest ray step */
	ray->ic = ic;

	if (irsmin<0 || irsmin>=nrs)
		warn("irsmin=%d",irsmin);

	/* return index of nearest ray step */
	return irsmin;
}

int xxx_nearestRayStep (Ray *ray, float x, float z)
/*****************************************************************************
Determine index of ray step nearest to point (x,z).  Simple (slow) version.
******************************************************************************
Input:
ray		ray
x		x coordinate
z		z coordinate

Returned:	index of nearest ray step
*****************************************************************************/
{
	int nrs=ray->nrs;
	RayStep *rs=ray->rs;
	int irs,irsmin=0;
	float dsmin,ds,dx,dz,xrs,zrs;

	for (irs=0,dsmin=FLT_MAX; irs<nrs; ++irs) {
		xrs = rs[irs].x;
		zrs = rs[irs].z;
		dx = x-xrs;
		dz = z-zrs;
		ds = dx*dx+dz*dz;
		if (ds<dsmin) {
			dsmin = ds;
			irsmin = irs;
		}
	}
	return irsmin;
}

Circle *makeCircles (int nc, int nrs, RayStep *rs)
/*****************************************************************************
Make circles used to speed up determination of nearest ray step.
******************************************************************************
Input:
nc		number of circles to make
nrs		number of ray steps
rs		array[nrs] of ray steps

Returned:	array[nc] of circles
*****************************************************************************/
{
	int nrsc,ic,irsf,irsl,irs;
	float xmin,xmax,zmin,zmax,x,z,r;
	Circle *c;

	/* allocate space for circles */
	c = (Circle*)alloc1(nc,sizeof(Circle));

	/* determine typical number of ray steps per circle */
	nrsc = 1+(nrs-1)/nc;

	/* loop over circles */
	for (ic=0; ic<nc; ++ic) {
		
		/* index of first and last raystep */
		irsf = ic*nrsc;
		irsl = irsf+nrsc-1;
		if (irsf>=nrs) irsf = nrs-1;
		if (irsl>=nrs) irsl = nrs-1;

		/* coordinate bounds of ray steps */
		xmin = xmax = rs[irsf].x;
		zmin = zmax = rs[irsf].z;
		for (irs=irsf+1; irs<=irsl; ++irs) {
			if (rs[irs].x<xmin) xmin = rs[irs].x;
			if (rs[irs].x>xmax) xmax = rs[irs].x;
			if (rs[irs].z<zmin) zmin = rs[irs].z;
			if (rs[irs].z>zmax) zmax = rs[irs].z;
		}

		/* center and radius of circle */
		x = 0.5*(xmin+xmax);
		z = 0.5*(zmin+zmax);
		r = sqrt((x-xmin)*(x-xmin)+(z-zmin)*(z-zmin));

		/* set circle */
		c[ic].irsf = irsf;
		c[ic].irsl = irsl;
		c[ic].x = x;
		c[ic].z = z;
		c[ic].r = r;
	}

	return c;
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

/* Beam subroutines */
/* size of cells in which to linearly interpolate complex time and amplitude */
#define CELLSIZE 8

/* factor by which to oversample time for linear interpolation of traces */
#define NOVERSAMPLE 4

/* number of exponential decay filters */
#define NFILTER 6

/* exp(EXPMIN) is assumed to be negligible */
#define EXPMIN (-5.0)

/* filtered complex beam data as a function of real and imaginary time */
typedef struct BeamDataStruct {
	int ntr;		/* number of real time samples */
	float dtr;		/* real time sampling interval */
	float ftr;		/* first real time sample */
	int nti;		/* number of imaginary time samples */
	float dti;		/* imaginary time sampling interval */
	float fti;		/* first imaginary time sample */
	complex **cf;		/* array[nti][ntr] of complex data */
} BeamData;

/* one cell in which to linearly interpolate complex time and amplitude */
typedef struct CellStruct {
	int live;	/* random number used to denote a live cell */
	int dead;	/* random number used to denote a dead cell */
	float tr;	/* real part of traveltime */
	float ti;	/* imaginary part of traveltime */
	float ar;	/* real part of amplitude */
	float ai;	/* imaginary part of amplitude */
} Cell;

/* structure containing information used to set and fill cells */
typedef struct CellsStruct {
	int nt;		/* number of time samples */
	float dt;	/* time sampling interval */
	float ft;	/* first time sample */
	int lx;		/* number of x samples per cell */
	int mx;		/* number of x cells */
	int nx;		/* number of x samples */
	float dx;	/* x sampling interval */
	float fx;	/* first x sample */
	int lz;		/* number of z samples per cell */
	int mz;		/* number of z cells */
	int nz;		/* number of z samples */
	float dz;	/* z sampling interval */
	float fz;	/* first z sample */
	int live;	/* random number used to denote a live cell */
	int dead;	/* random number used to denote a dead cell */
	float wmin;	/* minimum (reference) frequency */
	float lmin;	/* minimum beamwidth for frequency wmin */
	Cell **cell;	/* cell array[mx][mz] */
	Ray *ray;	/* ray */
	BeamData *bd;	/* complex beam data as a function of complex time */
	float **g;	/* array[nx][nz] containing g(x,z) */
} Cells;

/* functions defined and used internally */
static void xtop (float w,
	int nx, float dx, float fx, complex *g,
	int np, float dp, float fp, complex *h);
static BeamData* beamData (float wmin, int nt, float dt, float ft, float *f);
static void setCell (Cells *cells, int jx, int jz);
static void accCell (Cells *cells, int jx, int jz);
static int cellTimeAmp (Cells *cells, int jx, int jz);
static void cellBeam (Cells *cells, int jx, int jz);


/* functions for external use */

void formBeams (float bwh, float dxb, float fmin,
	int nt, float dt, float ft, 
	int nx, float dx, float fx, float **f,
	int ntau, float dtau, float ftau, 
	int npx, float dpx, float fpx, float **g)
/*****************************************************************************
Form beams (filtered slant stacks) for later superposition of Gaussian beams.
******************************************************************************
Input:
bwh		horizontal beam half-width
dxb		horizontal distance between beam centers
fmin		minimum frequency (cycles per unit time)
nt		number of input time samples
dt		input time sampling interval
ft		first input time sample
nx		number of horizontal samples
dx		horizontal sampling interval
fx		first horizontal sample
f		array[nx][nt] of data to be slant stacked into beams
ntau		number of output time samples
dtau		output time sampling interval (currently must equal dt)
ftau		first output time sample
npx		number of horizontal slownesses
dpx		horizontal slowness sampling interval
fpx		first horizontal slowness

Output:
g		array[npx][ntau] containing beams
*****************************************************************************/
{
	int ntpad,ntfft,nw,ix,iw,ipx,it,itau;
	float wmin,pxmax,xmax,x,dw,fw,w,fftscl,
		amp,phase,scale,a,b,as,bs,es,cfr,cfi,
		*fpad=NULL,*gpad=NULL;
	complex **cf=NULL,**cg=NULL,*cfx=NULL,*cgpx=NULL;

	/* minimum frequency in radians */
	wmin = 2.0*PI*fmin;

	/* pad time axis to avoid wraparound */
	pxmax = (dpx<0.0)?fpx:fpx+(npx-1)*dpx;
	xmax = (dx<0.0)?fx:fx+(nx-1)*dx;
	ntpad = ABS(pxmax*xmax)/dt;

	/* fft sampling */
	ntfft = npfar(MAX(nt+ntpad,ntau));
	nw = ntfft/2+1;
	dw = 2.0*PI/(ntfft*dt);
	fw = 0.0;
	fftscl = 1.0/ntfft;

	/* allocate workspace */
	fpad = alloc1float(ntfft);
	gpad = alloc1float(ntfft);
	cf = alloc2complex(nw,nx);
	cg = alloc2complex(nw,npx);
	cfx = alloc1complex(nx);
	cgpx = alloc1complex(npx);

	/* loop over x */
	for (ix=0; ix<nx; ++ix) {

		/* pad time with zeros */
		for (it=0; it<nt; ++it)
			fpad[it] = f[ix][it];
		for (it=nt; it<ntfft; ++it)
			fpad[it] = 0.0;
		
		/* Fourier transform time to frequency */
		pfarc(1,ntfft,fpad,cf[ix]);
	}

	/* loop over w */
	for (iw=0,w=fw; iw<nw; ++iw,w+=dw) {
		
		/* frequency-dependent amplitude scale factors */
		scale = -0.5*w/(wmin*bwh*bwh);
		amp = fftscl*dpx*w/(2.0*PI)*sqrt(w/(PI*wmin))*dxb/bwh;

		/* phase shift to account for ft not equal to ftau */
		phase = w*(ft-ftau);

		/* apply complex filter */
		a = amp*cos(phase);
		b = amp*sin(phase);
		for (ix=0,x=fx; ix<nx; ++ix,x+=dx) {
			es = exp(scale*x*x);
			as = a*es;
			bs = b*es;
			cfr = cf[ix][iw].r;
			cfi = cf[ix][iw].i;
			cfx[ix].r = as*cfr-bs*cfi;
			cfx[ix].i = bs*cfr+as*cfi;
		}

		/* transform x to p */
		xtop(w,nx,dx,fx,cfx,npx,dpx,fpx,cgpx);
		for (ipx=0; ipx<npx; ++ipx) {
			cg[ipx][iw].r = cgpx[ipx].r;
			cg[ipx][iw].i = cgpx[ipx].i;
		}
	}

	/* loop over px */
	for (ipx=0; ipx<npx; ++ipx) {
		
		/* Fourier transform frequency to time */
		pfacr(-1,ntfft,cg[ipx],gpad);

		/* copy to output array */
		for (itau=0; itau<ntau; ++itau)
			g[ipx][itau] = gpad[itau];
	}

	free1float(fpad);
	free1float(gpad);
	free2complex(cf);
	free2complex(cg);
	free1complex(cfx);
	free1complex(cgpx);
}

void accBeam (Ray *ray, float fmin, float lmin,
	int nt, float dt, float ft, float *f,
	int nx, float dx, float fx, int nz, float dz, float fz, float **g)
/*****************************************************************************
Accumulate contribution of one Gaussian beam.
******************************************************************************
Input:
ray		ray parameters sampled at discrete ray steps
fmin		minimum frequency (cycles per unit time)
lmin		initial beam width for frequency wmin
nt		number of time samples
dt		time sampling interval
ft		first time sample
f		array[nt] containing data for one ray f(t)		
nx		number of x samples
dx		x sampling interval
fx		first x sample
nz		number of z samples
dz		z sampling interval
fz		first z sample
g		array[nx][nz] in which to accumulate beam

Output:
g		array[nx][nz] after accumulating beam
*****************************************************************************/
{
	int lx,lz,mx,mz,jx,jz,live,dead;
	float wmin;
	RayStep *rs=ray->rs;
	Cells *cells;
	Cell **cell;
	BeamData *bd;

	/* frequency in radians per unit time */
	wmin = 2.0*PI*fmin;

	/* random numbers used to denote live and dead cells */
	live = 1+(int)(1.0e7*franuni());
	dead = 1+(int)(1.0e7*franuni());
	
	/* number of samples per cell */
	lx = CELLSIZE;
	lz = CELLSIZE;

	/* number of cells */
	mx = 2+(nx-1)/lx;
	mz = 2+(nz-1)/lz;

	/* compute complex beam data */
	bd = beamData(wmin,nt,dt,ft,f);

	/* allocate cells */
	cells = (Cells*)alloc1(1,sizeof(Cells));
	cell = (Cell**)alloc2(mz,mx,sizeof(Cell));

	/* set information needed to set and fill cells */
	cells->nt = nt;
	cells->dt = dt;
	cells->ft = ft;
	cells->lx = lx;
	cells->mx = mx;
	cells->nx = nx;
	cells->dx = dx;
	cells->fx = fx;
	cells->lz = lz;
	cells->mz = mz;
	cells->nz = nz;
	cells->dz = dz;
	cells->fz = fz;
	cells->live = live;
	cells->dead = dead;
	cells->wmin = wmin;
	cells->lmin = lmin;
	cells->cell = cell;
	cells->ray = ray;
	cells->bd = bd;
	cells->g = g;

	/* cell closest to initial point on ray will be first live cell */
	jx = NINT((rs[0].x-fx)/dx/lx);
	jz = NINT((rs[0].z-fz)/dz/lz);
	
	/* set first live cell and its neighbors recursively */
	setCell(cells,jx,jz);

	/* accumulate beam in first live cell and its neighbors recursively */
	accCell(cells,jx,jz);

	/* free complex beam data */
	free2complex(bd->cf);
	free1((void*)bd);

	/* free cells */
	free2((void**)cells->cell);
	free1((void*)cells);
}

/* functions for internal use only */

static void xtop (float w,
	int nx, float dx, float fx, complex *g,
	int np, float dp, float fp, complex *h)
/*****************************************************************************
Slant stack for one frequency w, where slant stack is defined by

           fx+(nx-1)*dx
    h(p) =   integral   exp(-sqrt(-1)*w*p*x) * g(x) * dx
                fx
******************************************************************************
Input:
w		frequency (radians per unit time)
nx		number of x samples
dx		x sampling interval
fx		first x sample
g		array[nx] containing g(x)
np		number of p samples
dp		p sampling interval
fp		first p sample

Output:
h		array[np] containing h(p)
******************************************************************************
Notes:
The units of w, x, and p must be consistent.

Slant stack is performed via FFT and 8-point (tapered-sinc) interpolation.

The Fourier transform over time (t) is assumed to have been performed with
positive sqrt(-1)*w*t in the exponent;  if negative sqrt(-1)*w*t was used
instead, call this function with negative w.
*****************************************************************************/
{
	int nxfft,nk,nka,ix,ik,ip,lwrap;
	float dk,fk,ek,fka,k,p,phase,c,s,x,xshift,temp,*kp;
	complex czero=cmplx(0.0,0.0),*gx,*gk,*gka,*hp;

	/* number of samples required to make wavenumber k periodic */
	lwrap = 8;
	
	/* wavenumber k sampling */
	nxfft = npfa((nx+lwrap)*2);
	nk = nxfft;
	dk = 2.0*PI/(nxfft*dx);
	fk = -PI/dx;
	ek = PI/dx;
	fka = fk-lwrap*dk;
	nka = lwrap+nk+lwrap;
	
	/* allocate workspace */
	gka = alloc1complex(nka);
	gx = gk = gka+lwrap;
	hp = alloc1complex(np);
	kp = alloc1float(np);
	
	/* scale g(x) by x sampling interval dx */
	for (ix=0; ix<nx; ++ix,x+=dx) {
		gx[ix].r = dx*g[ix].r;
		gx[ix].i = dx*g[ix].i;
	}
	
	/* pad g(x) with zeros */
	for (ix=nx; ix<nxfft; ++ix)
		gx[ix].r = gx[ix].i = 0.0;
	
	/* negate every other sample so k-axis will be centered */
	for (ix=1; ix<nx; ix+=2) {
		gx[ix].r = -gx[ix].r;
		gx[ix].i = -gx[ix].i;
	}
	
	/* Fourier transform g(x) to g(k) */
	pfacc(-1,nxfft,gx);
	
	/* wrap-around g(k) to avoid interpolation end effects */
	for (ik=0; ik<lwrap; ++ik)
		gka[ik] = gk[ik+nk-lwrap];
	for (ik=lwrap+nk; ik<lwrap+nk+lwrap; ++ik)
		gka[ik] = gk[ik-lwrap-nk];
	
	/* phase shift to account for non-centered x-axis */
	xshift = 0.5*(nx-1)*dx;
	for (ik=0,k=fka; ik<nka; ++ik,k+=dk) {
		phase = k*xshift;
		c = cos(phase);
		s = sin(phase);
		temp = gka[ik].r*c-gka[ik].i*s;
		gka[ik].i = gka[ik].r*s+gka[ik].i*c;
		gka[ik].r = temp;
	}
	
	/* compute k values at which to interpolate g(k) */
	for (ip=0,p=fp; ip<np; ++ip,p+=dp) {
		kp[ip] = w*p;
		
		/* if outside Nyquist bounds, do not interpolate */
		if (kp[ip]<fk && kp[ip]<ek)
			kp[ip] = fk-1000.0*dk;
		else if (kp[ip]>fk && kp[ip]>ek)
			kp[ip] = ek+1000.0*dk;
	}
		
	/* interpolate g(k) to obtain h(p) */
	ints8c(nka,dk,fka,gka,czero,czero,np,kp,hp);
	
	/* phase shift to account for non-centered x-axis and non-zero fx */
	xshift = -fx-0.5*(nx-1)*dx;
	for (ip=0; ip<np; ++ip) {
		phase = kp[ip]*xshift;
		c = cos(phase);
		s = sin(phase);
		h[ip].r = hp[ip].r*c-hp[ip].i*s;
		h[ip].i = hp[ip].r*s+hp[ip].i*c;
	}

	/* free workspace */
	free1complex(gka);
	free1complex(hp);
	free1float(kp);
}

static BeamData* beamData (float wmin, int nt, float dt, float ft, float *f)
/*****************************************************************************
Compute filtered complex beam data as a function of real and imaginary time.
******************************************************************************
Input:
wmin		minimum frequency (in radians per unit time)
nt		number of time samples
dt		time sampling interval
ft		first time sample
f		array[nt] containing data to be filtered

Returned:	pointer to beam data
*****************************************************************************/
{
	int ntpad,ntfft,nw,iwnyq,ntrfft,ntr,nti,nwr,it,itr,iti,iw;
	float dw,fw,dtr,ftr,dti,fti,w,ti,scale,*fa;
	complex *ca,*cb,*cfi,**cf;
	BeamData *bd;

	/* pad to avoid wraparound in Hilbert transform */
	ntpad = 25;

	/* fft sampling */
	ntfft = npfaro(nt+ntpad,2*(nt+ntpad));
	nw = ntfft/2+1;
	dw = 2.0*PI/(ntfft*dt);
	fw = 0.0;
	iwnyq = nw-1;

	/* real time sampling (oversample for future linear interpolation) */
	ntrfft = nwr = npfao(NOVERSAMPLE*ntfft,NOVERSAMPLE*ntfft+ntfft);
	dtr = dt*ntfft/ntrfft;
	ftr = ft;
	ntr = 1+(nt+ntpad-1)*dt/dtr;

	/* imaginary time sampling (exponential decay filters) */
	nti = NFILTER;
	dti = EXPMIN/(wmin*(nti-1));
	fti = 0.0;

	/* allocate space for filtered data */
	cf = alloc2complex(ntr,nti);

	/* allocate workspace */
	fa = alloc1float(ntfft);
	ca = alloc1complex(nw);
	cb = alloc1complex(ntrfft);

	/* pad data with zeros */
	for (it=0; it<nt; ++it)
		fa[it] = f[it];
	for (it=nt; it<ntfft; ++it)
		fa[it] = 0.0;

	/* Fourier transform and scale to make complex analytic signal */
	pfarc(1,ntfft,fa,ca);
	for (iw=1; iw<iwnyq; ++iw) {
		ca[iw].r *= 2.0;
		ca[iw].i *= 2.0;
	}

	/* loop over imaginary time */
	for (iti=0,ti=fti; iti<nti; ++iti,ti+=dti) {

		/* apply exponential decay filter */
		for (iw=0,w=fw; iw<nw; ++iw,w+=dw) {
			scale = exp(w*ti);
			cb[iw].r = ca[iw].r*scale;
			cb[iw].i = ca[iw].i*scale;
		}

		/* pad with zeros */
		for (iw=nw; iw<nwr; ++iw)
			cb[iw].r = cb[iw].i = 0.0;

		/* inverse Fourier transform and scale */
		pfacc(-1,ntrfft,cb);
		cfi = cf[iti];
		scale = 1.0/ntfft;
		for (itr=0; itr<ntr; ++itr) {
			cfi[itr].r = scale*cb[itr].r;
			cfi[itr].i = scale*cb[itr].i;
		}
	}

	/* free workspace */
	free1float(fa);
	free1complex(ca);
	free1complex(cb);

	/* return beam data */
	bd = (BeamData*)alloc1(1,sizeof(BeamData));
	bd->ntr = ntr;
	bd->dtr = dtr;
	bd->ftr = ftr;
	bd->nti = nti;
	bd->dti = dti;
	bd->fti = fti;
	bd->cf = cf;
	return bd;
}

static void setCell (Cells *cells, int jx, int jz)
/*****************************************************************************
Set a cell by computing its Gaussian beam complex time and amplitude.  
If the amplitude is non-zero, set neighboring cells recursively.
******************************************************************************
Input:
cells		pointer to cells
jx		x index of the cell to set
jz		z index of the cell to set
******************************************************************************
Notes:
To reduce the amount of memory required for recursion, the actual 
computation of complex time and amplitude is performed by the cellTimeAmp() 
function, so that no local variables are required in this function, except
for the input arguments themselves.
*****************************************************************************/
{
	/* if cell is out of bounds, return */
	if (jx<0 || jx>=cells->mx || jz<0 || jz>=cells->mz) return;

	/* if cell is live, return */
	if (cells->cell[jx][jz].live==cells->live) return;

	/* make cell live */
	cells->cell[jx][jz].live = cells->live;

	/* compute complex time and amplitude.  If amplitude is
	 * big enough, recursively set neighboring cells. */
	if (cellTimeAmp(cells,jx,jz)) {
		setCell(cells,jx+1,jz);
		setCell(cells,jx-1,jz);
		setCell(cells,jx,jz+1);
		setCell(cells,jx,jz-1);
	}
}

static int cellTimeAmp (Cells *cells, int jx, int jz)
/*****************************************************************************
Compute complex and time and amplitude for a cell.
******************************************************************************
Input:
cells		pointer to cells
jx		x index of the cell to set
jz		z index of the cell to set

Returned:	1 if Gaussian amplitude is significant, 0 otherwise
*****************************************************************************/
{
	int lx=cells->lx,lz=cells->lz;
	float dx=cells->dx,fx=cells->fx,dz=cells->dz,fz=cells->fz,
		wmin=cells->wmin,lmin=cells->lmin;
	Ray *ray=cells->ray;
	Cell **cell=cells->cell;
	int irs,kmah;
	float tmax,xc,zc,t0,x0,z0,x,z,tr,ti,ar,ai,
		v,q1,p1,q2,p2,e,es,scale,phase,vvmr,vvmi,c,s,ov,
		px,pz,pzpz,pxpx,pxpz,dvdx,dvdz,dvds,dvdn,
		wxxr,wxzr,wzzr,wxxi,wxzi,wzzi;
	RayStep *rs;

	/* maximum time */
	tmax = ray->rs[ray->nrs-1].t;

	/* cell coordinates */
	xc = fx+jx*lx*dx;
	zc = fz+jz*lz*dz;

	/* ray step nearest to cell */
	irs = nearestRayStep(ray,xc,zc);
	rs = &(ray->rs[irs]);
		
	/* ray time and coordinates */
	t0 = rs->t;
	x0 = rs->x;
	z0 = rs->z;
	
	/* real and imaginary parts of v*v*m = v*v*p/q */
	v = rs->v;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	e = wmin*lmin*lmin;
	es = e*e;
	scale = v*v/(q2*q2+es*q1*q1);
	vvmr = (p2*q2+es*p1*q1)*scale;
	vvmi = -e*scale;
	
	/* components of slowness vector, px and pz */
	c = rs->c;
	s = rs->s;
	ov = 1.0/v;
	px = s*ov;
	pz = c*ov;
	pzpz = pz*pz;
	pxpx = px*px;
	pxpz = px*pz;
	
	/* velocity derivatives along tangent and normal */
	dvdx = rs->dvdx;
	dvdz = rs->dvdz;
	dvds = s*dvdx+c*dvdz;
	dvdn = c*dvdx-s*dvdz;
	
	/* real part of W matrix */
	wxxr = pzpz*vvmr-2.0*pxpz*dvdn-pxpx*dvds;
	wxzr = (pxpx-pzpz)*dvdn-pxpz*(vvmr+dvds);
	wzzr = pxpx*vvmr+2.0*pxpz*dvdn-pzpz*dvds;
	
	/* imaginary part of W matrix */
	wxxi = pzpz*vvmi;
	wxzi = -pxpz*vvmi;
	wzzi = pxpx*vvmi;
	
	/* vector from ray to cell */
	x = xc-x0;
	z = zc-z0;
	
	/* real and imaginary parts of complex time */
	tr = t0+(px+0.5*wxxr*x)*x+(pz+wxzr*x+0.5*wzzr*z)*z;
	ti = 0.5*wxxi*x*x+(wxzi*x+0.5*wzzi*z)*z;

	/* real and imaginary parts of complex amplitude */
	kmah = rs->kmah;
	scale = pow(es/(q2*q2+es*q1*q1),0.25);
	phase = 0.5*(atan2(q2,q1*e)+2.0*PI*((kmah+1)/2));
	ar = scale*cos(phase);
	ai = scale*sin(phase);

	/* set cell parameters */
	cell[jx][jz].tr = tr;
	cell[jx][jz].ti = ti;
	cell[jx][jz].ar = ar;
	cell[jx][jz].ai = ai;

	/* return 1 if Gaussian amplitude is significant, 0 otherwise */
	return (wmin*ti>EXPMIN && tr<=tmax)?1:0;
}

static void accCell (Cells *cells, int jx, int jz)
/*****************************************************************************
Accumulate the contribution of a Gaussian beam in a cell and its
neighbors, recursively.
******************************************************************************
Input:
cells		pointer to cells
jx		x index of the cell to fill
jz		z index of the cell to fill
******************************************************************************
Notes:
To reduce the amount of memory required for recursion, the actual
accumulation is performed by function cellBeam(), so that no local
variables are required in this function, except for the input
arguments themselves.
*****************************************************************************/
{
	/* if cell is out of bounds, return */
	if (jx<0 || jx>=cells->mx-1 || jz<0 || jz>=cells->mz-1) return;

	/* if cell is dead, return */
	if (cells->cell[jx][jz].dead==cells->dead) return;

	/* make cell dead */
	cells->cell[jx][jz].dead = cells->dead;

	/* if upper-left corner of cell is not live, return */
	if (cells->cell[jx][jz].live!=cells->live) return;

	/* if remaining three corners of cell are live */
	if (cells->cell[jx+1][jz].live==cells->live &&
		cells->cell[jx][jz+1].live==cells->live &&
		cells->cell[jx+1][jz+1].live==cells->live) {

		/* accumulate beam in cell */
		cellBeam(cells,jx,jz);
	}

	/* recursively accumulate in neighboring cells */
	accCell(cells,jx+1,jz);
	accCell(cells,jx-1,jz);
	accCell(cells,jx,jz+1);
	accCell(cells,jx,jz-1);
}

static void cellBeam (Cells *cells, int jx, int jz)
/*****************************************************************************
Accumulate Gaussian beam for one cell.
******************************************************************************
Input:
cells		pointer to cells
jx		x index of the cell in which to accumulate beam
jz		z index of the cell in which to accumulate beam
*****************************************************************************/
{
	int lx=cells->lx,lz=cells->lz,nx=cells->nx,nz=cells->nz;
	float **g=cells->g;
	Cell **cell=cells->cell;
	BeamData *bd=cells->bd;
	int ntr=bd->ntr,nti=bd->nti;
	float dtr=bd->dtr,ftr=bd->ftr,dti=bd->dti,fti=bd->fti;
	complex **cf=bd->cf;
	int kxlo,kxhi,kzlo,kzhi,kx,kz,itr,iti;
	float odtr,odti,t00r,t01r,t10r,t11r,t00i,t01i,t10i,t11i,
		a00r,a01r,a10r,a11r,a00i,a01i,a10i,a11i,
		tx0r,tx1r,tx0i,tx1i,ax0r,ax1r,ax0i,ax1i,
		txzr,txzi,axzr,axzi,
		dtx0r,dtx0i,dtx1r,dtx1i,dax0r,dax0i,dax1r,dax1i,
		dtxzr,dtxzi,daxzr,daxzi,xdelta,zdelta,
		trn,tin,trfrac,mtrfrac,tifrac,mtifrac,
		cf0r,cf0i,cf1r,cf1i,cfr,cfi;
	complex *cf0,*cf1;

	/* inverse of time sampling intervals */
	odtr = 1.0/dtr;
	odti = 1.0/dti;

	/* complex time and amplitude for each corner */
	t00r = cell[jx][jz].tr;
	t01r = cell[jx][jz+1].tr;
	t10r = cell[jx+1][jz].tr;
	t11r = cell[jx+1][jz+1].tr;
	t00i = cell[jx][jz].ti;
	t01i = cell[jx][jz+1].ti;
	t10i = cell[jx+1][jz].ti;
	t11i = cell[jx+1][jz+1].ti;
	a00r = cell[jx][jz].ar;
	a01r = cell[jx][jz+1].ar;
	a10r = cell[jx+1][jz].ar;
	a11r = cell[jx+1][jz+1].ar;
	a00i = cell[jx][jz].ai;
	a01i = cell[jx][jz+1].ai;
	a10i = cell[jx+1][jz].ai;
	a11i = cell[jx+1][jz+1].ai;

	/* x and z samples for cell */
	kxlo = jx*lx;
	kxhi = kxlo+lx;
	if (kxhi>nx) kxhi = nx;
	kzlo = jz*lz;
	kzhi = kzlo+lz;
	if (kzhi>nz) kzhi = nz;

	/* fractional increments for linear interpolation */
	xdelta = 1.0/lx;
	zdelta = 1.0/lz;

	/* increments for times and amplitudes at top and bottom of cell */
	dtx0r = (t10r-t00r)*xdelta;
	dtx1r = (t11r-t01r)*xdelta;
	dtx0i = (t10i-t00i)*xdelta;
	dtx1i = (t11i-t01i)*xdelta;
	dax0r = (a10r-a00r)*xdelta;
	dax1r = (a11r-a01r)*xdelta;
	dax0i = (a10i-a00i)*xdelta;
	dax1i = (a11i-a01i)*xdelta;

	/* times and amplitudes at top-left and bottom-left of cell */
	tx0r = t00r;
	tx1r = t01r;
	tx0i = t00i;
	tx1i = t01i;
	ax0r = a00r;
	ax1r = a01r;
	ax0i = a00i;
	ax1i = a01i;

	/* loop over x samples */
	for (kx=kxlo; kx<kxhi; ++kx) {

		/* increments for time and amplitude */
		dtxzr = (tx1r-tx0r)*zdelta;
		dtxzi = (tx1i-tx0i)*zdelta;
		daxzr = (ax1r-ax0r)*zdelta;
		daxzi = (ax1i-ax0i)*zdelta;

		/* time and amplitude at top of cell */
		txzr = tx0r;
		txzi = tx0i;
		axzr = ax0r;
		axzi = ax0i;

		/* loop over z samples */
		for (kz=kzlo; kz<kzhi; ++kz) {

			/* index of imaginary time */
			iti = tin = (txzi-fti)*odti;
			if (iti<0 || iti>=nti-1) continue;

			/* pointers to left and right imaginary time samples */
			cf0 = cf[iti];
			cf1 = cf[iti+1];

			/* imaginary time linear interpolation coefficients */
			tifrac = tin-iti;
			mtifrac = 1.0-tifrac;

			/* index of real time */
			itr = trn = (txzr-ftr)*odtr;
			if (itr<0 || itr>=ntr-1) continue;

			/* real time linear interpolation coefficients */
			trfrac = trn-itr;
			mtrfrac = 1.0-trfrac;

			/* real and imaginary parts of complex beam data */
			cf0r = mtrfrac*cf0[itr].r+trfrac*cf0[itr+1].r;
			cf1r = mtrfrac*cf1[itr].r+trfrac*cf1[itr+1].r;
			cfr = mtifrac*cf0r+tifrac*cf1r;
			cf0i = mtrfrac*cf0[itr].i+trfrac*cf0[itr+1].i;
			cf1i = mtrfrac*cf1[itr].i+trfrac*cf1[itr+1].i;
			cfi = mtifrac*cf0i+tifrac*cf1i;

			/* accumulate beam */
			g[kx][kz] += axzr*cfr-axzi*cfi;

			/* increment time and amplitude */
			txzr += dtxzr;
			txzi += dtxzi;
			axzr += daxzr;
			axzi += daxzi;
		}

		/* increment times and amplitudes at top and bottom of cell */
		tx0r += dtx0r;
		tx1r += dtx1r;
		tx0i += dtx0i;
		tx1i += dtx1i;
		ax0r += dax0r;
		ax1r += dax1r;
		ax0i += dax0i;
		ax1i += dax1i;
	}
}
