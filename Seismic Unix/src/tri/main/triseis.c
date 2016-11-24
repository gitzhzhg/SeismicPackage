/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* TRISEIS: $Test Release: 1.1 $ ; $Date: 2011/11/21 16:56:25 $	*/

#include "par.h"
#include "Triangles/tri.h"
#include "Triangles/sloth.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" TRISEIS - Gaussian beam synthetic seismograms for a sloth model	",
"									",
"  triseis <modelfile >seisfile xs= zs= xg= zg= [optional parameters]	",
"									",
" Required Parameters:							",
" xs=            x coordinates of source surface			",
" zs=            z coordinates of source surface			",
" xg=            x coordinates of receiver surface			",
" zg=            z coordinates of receiver surface			",
"									",
" Optional Parameters:							",
" ns=1           number of sources uniformly distributed along s surface",
" ds=            increment between source locations (see notes below)	",
" fs=0.0         first source location (relative to start of s surface)	",
" ng=101         number of receivers uniformly distributed along g surface",
" dg=            increment between receiver locations (see notes below)	",
" fg=0.0         first receiver location (relative to start of g surface)",
" dgds=0.0       change in receiver location with respect to source location",
" krecord=1      integer index of receiver surface (see notes below)	",
" kreflect=-1    integer index of reflecting surface (see notes below)	",
" prim           =1, only single-reflected rays are considered 		",     
"                =0, only direct hits are considered  			",
" bw=0           beamwidth at peak frequency				",
" nt=251         number of time samples					",
" dt=0.004       time sampling interval					",
" ft=0.0         first time sample					",
" nangle=101     number of ray takeoff angles				",
" fangle=-45     first ray takeoff angle (in degrees)			",
" langle=45      last ray takeoff angle (in degrees)			",
" reftrans=0     =1 complex refl/transm. coefficients considered 	",
" atten=0        =1 add noncausal attenuation 				",
"                =2 add causal attenuation 				",
" lscale=        if defined restricts range of extrapolation		",
" fpeak=0.1/dt   peak frequency of ricker wavelet 			",
" aperture=      maximum angle of receiver aperture 			",
"									",
" NOTES:								",
" Only rays that terminate with index krecord will contribute to the	",
" synthetic seismograms at the receiver (xg,zg) locations.  The		",
" source and receiver locations are determined by cubic spline		",
" interpolation of the specified (xs,zs) and (xg,zg) coordinates.	",
" The default source location increment (ds) is determined to span	",
" the source surface defined by (xs,zs).  Likewise for dg.		",
"									",
NULL};

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 02/09/91
 * MODIFIED:  Andreas Rueger, Colorado School of Mines, 08/18/93
 *	Modifications include: 2.5-D amplitudes, correction for ref/transm,
 *			timewindow, lscale, aperture, beam width, etc.
 */
/**************** end self doc ***********************************/

#define TAPER 0.97
#define C 0.99
#define EPS 0.0001

/* parameters for one step of dynamic ray tracing */
typedef struct RSStruct {
	float sigma,x,z,px,pz,t;
	float q1,p1,q2,p2;
        float atten;
        float ampli,ampliphase;
	int kmah,nref;
	EdgeUse *eu;
	Face *f;
} RayStep;

/* prototypes for functions defined and used internally */
static void makexzs (int nu, float *xu, float *zu, 
	int ns, float ds, float fs, float *xs, float *zs);
static void makesyn (float fpeak,int prim,int nre, RayEnd *re, float lscale,
	int krecord, int ng, float *xg, float *zg,
	float bw, int nt, float dt, float ft,
        int natten, int reftrans, float aperture);
static complex cricker (float w, float wpeak, float delay);
static int insideModel (Model *m, float x, float z);
static void shootRays (Model *m, float xs, float zs,
	int nangle, float dangle, float fangle,
	int kstop, int kreflect,
	RayEnd re[]);
static void traceRayInTri (RayStep *rs, RayStep *rsnew);
static int traceRayAcrossEdge (RayStep *rs, RayStep *rsnew);
static void reflectRayFromEdge (RayStep *rs, RayStep *rsnew);
static void evaluateDynamic (float sigma, 
	float px, float pz, float dsdx, float dsdz,
	float *q1, float *p1, float *q2, float *p2, int *kmah);
int checkIfSourceOnEdge (Face *tris, float zs, float xs);

/* the main program */
int main (int argc, char **argv)
{
	int nxs,nzs,nxzs,nxg,nzg,nxzg,
		ns,ng,krecord,kreflect,nt,nangle,it,is,ig;
	int natten,reftrans,prim;
	float ds,fs,dg,fg,dgds,s,bw,dt,ft,cpuray,cpuseis,
		fangle,langle,dangle,lscale,fpeak,aperture,
		*xsu,*zsu,*xgu,*zgu,
		*xs,*zs,*xg,*zg,*zeros;
	Model *m;	
	RayEnd *re;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* read sloth model */
	m = readModel(stdin);

	/* source location parameters */
	nxs = countparval("xs");
	nzs = countparval("zs");
	if (nxs==0 || nzs==0) err("must specify both xs and zs");
	if (nxs!=nzs) err("number of xs must equal number of zs");
	nxzs = nxs;
	xsu = ealloc1float(nxzs);
	zsu = ealloc1float(nxzs);
	getparfloat("xs",xsu);
	getparfloat("zs",zsu);
	if (!getparint("ns",&ns)) ns = 1;
	if (!getparfloat("ds",&ds)) ds = 0.0;
	if (!getparfloat("fs",&fs)) fs = 0.0;

	/* receiver location parameters */
	nxg = countparval("xg");
	nzg = countparval("zg");
	if (nxg==0 || nzg==0) err("must specify both xg and zg");
	if (nxg!=nzg) err("number of xg must equal number of zg");
	nxzg = nxg;
	xgu = ealloc1float(nxzg);
	zgu = ealloc1float(nxzg);
	getparfloat("xg",xgu);
	getparfloat("zg",zgu);
	if (!getparint("ng",&ng)) ng = 101;
	if (!getparfloat("dg",&dg)) dg = 0.0;
	if (!getparfloat("fg",&fg)) fg = 0.0;
	if (!getparfloat("dgds",&dgds)) dgds = 0.0;
	if (!getparint("krecord",&krecord)) krecord = 1;
	if (!getparint("prim",&prim)) prim = INT_MAX;

	/* other parameters */
	if (!getparint("kreflect",&kreflect)) kreflect = -1;
	if (!getparfloat("bw",&bw)) bw = 0.0;
	if (!getparint("nt",&nt)) nt = 251;
	if (!getparfloat("dt",&dt)) dt = 0.004;
	if (!getparfloat("ft",&ft)) ft = 0.0;
	if (!getparfloat("fpeak",&fpeak)) fpeak = 0.1/dt;

	if (0.5/dt<2.0*fpeak) {
		fprintf(stderr,"WARNING: ALIASING POSSIBLE\n");
		fprintf(stderr,"decrease dt or reduce fpeak\n");
	}

	if (!getparint("nangle",&nangle)) nangle = 101;
	if (!getparfloat("fangle",&fangle)) fangle = -45.0;
	if (!getparfloat("langle",&langle)) langle = 45.0;
	if (!getparint("reftrans",&reftrans)) reftrans = 0;
	if (!getparint("atten",&natten)) natten = 0;
	if (!getparfloat("aperture",&aperture)) aperture = FLT_MAX;
	if (!getparfloat("lscale",&lscale)) lscale = FLT_MAX;
        checkpars();

	if (aperture != FLT_MAX) aperture = asin(aperture*PI/180.0);

	/* convert angles to radians and determine angle increment */
	fangle *= PI/180.0;
	langle *= PI/180.0;
	dangle = (nangle>1) ? (langle-fangle)/(nangle-1) : 1.0;

	/* allocate space */
	xs = ealloc1float(ns);
	zs = ealloc1float(ns);
	xg = ealloc1float(ng);
	zg = ealloc1float(ng);
	zeros = ealloc1float(nt);
	re = ealloc1(nangle,sizeof(RayEnd));

	/* determine source locations */
	makexzs(nxzs,xsu,zsu,ns,ds,fs,xs,zs);

	/* make trace with all zeros */
	for (it=0; it<nt; ++it)
		zeros[it] = 0.0;

	/* loop over source locations */
	for (is=0,s=fs,cpuray=cpuseis=0.0; is<ns; ++is,s+=ds) {

		/* uncomment for diagnostic print */
		fprintf(stderr,"shot %3d of %3d at x=%g\n",is,ns,xs[is]);
/*		fprintf(stderr,"shot %d at x=%.25g,z=%.25g\n",
			is,xs[is],zs[is]);
		fprintf(stderr,"rec %d at x=%.25g\n",is,fg+dgds*(s-fs));

		if (is<32 || is>33) continue;
*/
		/* if source is outside of model boundaries */
		if (!insideModel(m,xs[is],zs[is])) {

			/* write zeros */
			for (ig=0; ig<ng; ++ig)
				if (efwrite(zeros,sizeof(float),nt,stdout)!=nt)
					err("error writing output traces!\n");

			/* continue with next source location */
			continue;
		}

		/* shoot rays */
		/* cputemp = cpusec();*/
		shootRays(m,xs[is],zs[is],nangle,dangle,
				fangle,krecord,kreflect,re);
		/* cpuray += cpusec()-cputemp;*/

		/* compute receiver locations */
		makexzs(nxzg,xgu,zgu,ng,dg,fg+dgds*(s-fs),xg,zg);

		/* compute and write synthetic seismograms */
		/* cputemp = cpusec(); */
		makesyn(fpeak,prim,nangle,re,lscale,krecord,						
			ng,xg,zg,bw,nt,dt,ft,natten,reftrans,aperture);
	/*	cpuseis += cpusec()-cputemp; */
	}
	cpuray += 0.0;
	cpuseis += 0.0;

	/* cpu timings */
	/*
         * fprintf(stderr,"\ntriseis:  total cpu time = %g s\n",cpusec());
         * fprintf(stderr,"triseis:  ray tracing cpu time = %g s\n",cpuray);
         * fprintf(stderr,"triseis:  seismogram cpu time = %g s\n",cpuseis);
	 */

	return EXIT_SUCCESS;
}

static void makexzs (int nu, float *xu, float *zu, 
	int ns, float ds, float fs, float *xs, float *zs)
/* interpolate (x,z) coordinates uniformly sampled in distance s */
{
	int iu,nuu,iuu,is,js;
	float x,z,xlast,zlast,dx,dz,duu,uu,temp,
		*u,*s,(*xud)[4],(*zud)[4],*us;

	xud = (float(*)[4])ealloc1float(4*nu);
	zud = (float(*)[4])ealloc1float(4*nu);
	u = ealloc1float(nu);
	for (iu=0; iu<nu; ++iu)
		u[iu] = iu;
	csplin(nu,u,xu,xud);
	csplin(nu,u,zu,zud);
	nuu = 10*nu;
	duu = (u[nu-1]-u[0])/(nuu-1);
	s = ealloc1float(nuu);
	s[0] = 0.0;
	xlast = xu[0];
	zlast = zu[0];
	for (iuu=0,uu=0.0,s[0]=0.0; iuu<nuu; ++iuu,uu+=duu) {
		intcub(0,nu,u,xud,1,&uu,&x);
		intcub(0,nu,u,zud,1,&uu,&z);
		dx = x-xlast;
		dz = z-zlast;
		s[iuu] = s[iuu-1]+sqrt(dx*dx+dz*dz);
		xlast = x;
		zlast = z;
	}			
	us = ealloc1float(ns);
	if (ds==0.0) ds = (s[nuu-1]-s[0])/((ns>1)?ns-1:1);
	if (ds<0.0) {
		yxtoxy(nuu,duu,0.0,s,ns,-ds,fs+(ns-1)*ds,0.0,(float)(nu-1),us);
		for (is=0,js=ns-1; is<js; ++is,--js) {
			temp = us[is];
			us[is] = us[js];
			us[js] = temp;
		}
	} else {
		yxtoxy(nuu,duu,0.0,s,ns,ds,fs,0.0,(float)(nu-1),us);
	}
	intcub(0,nu,u,xud,ns,us,xs);
	intcub(0,nu,u,zud,ns,us,zs);
	free1float(us);
	free1float(s);
	free1float(u);
	free1float((float*)xud);
	free1float((float*)zud);
}

static void makesyn (float fpeak, int prim, int nre, RayEnd *re, float lscale,
	int krecord, int ng, float *xg, float *zg, float bw,
	int nt, float dt, float ft, int natten, int reftrans, float aperture)
/* make and write synthetic seismograms */
{
	int ntfft,nw,iw,ig,ire,kend,kmah,nref;
	float dw,w,wpeak,delay,scale,wref,eps1,eps2,n,
		t,x,z,px,pz,q1,p1,q2,p2,v0,v,dsdx,dsdz,
		dx,dz,delt,dels,nn,phase,ta,cpoqr,cpoqi,disq,
		*syn,sigma,*lnvec,sqlscale,dangle;
        float ampli,ampliphase,atten,temp,c,eps,v2,v4,dvds,dvdn;
	double campr,campi,cosw,sinw,expw,cosd,sind,expd,tempd;
	complex ceps,cq,cp,cpoq,cqr,camp,
		*cwave,**csyn;
	
	/* constants */
	ntfft = npfaro(nt,2*nt);
	nw = ntfft/2+1;
	dw = 2.0*PI/(ntfft*dt);
	wpeak = 2*PI*fpeak;
	wref = 1000.0;
	delay = 0.0;
	sqlscale = lscale*lscale;
	c = C;
	eps = EPS;

	/* allocate workspace */
	syn = ealloc1float(ntfft);
	cwave = ealloc1complex(nw);
	csyn = ealloc2complex(nw,ng);
	lnvec = ealloc1float(nw);

	/* initialize synthetics */
	for (ig=0; ig<ng; ++ig)
		for (iw=0; iw<nw; ++iw)
			csyn[ig][iw] = cmplx(0.0,0.0);

	/* precomputing causal attenuation term */
	if (natten==2) {
		temp = 1.0/PI;
		lnvec[0] = -1.0;
		for(iw=1,w=dw; iw<nw; ++iw,w+=dw)
			lnvec[iw] = log(w/wref)*temp;
	}

	/* loop over rays */
	for (ire=0; ire<nre; ++ire) {

		/* determine index of ray end */
		kend = re[ire].kend;

		/* if not the end index we want, skip this ray */
		if (kend!=krecord) continue;

		nref = re[ire].nref;

		/* if ray was not reflected */
		if (nref!=prim && prim!=INT_MAX) continue;

		px = re[ire].px;
		v = 1.0/sqrt(re[ire].se);

		/* if not within aperture, skip this ray */
		if (ABS(px*v)>aperture) continue;

		/* ray end parameters */
		sigma = 1/sqrt(re[ire].sigma);
		x = re[ire].x;
		z = re[ire].z;
		t = re[ire].t;
		pz = re[ire].pz;
		q1 = re[ire].q1;
		p1 = re[ire].p1;
		q2 = re[ire].q2;
		p2 = re[ire].p2;
		kmah = re[ire].kmah;
		atten = re[ire].atten;
		ampli = re[ire].ampli;
		ampliphase = re[ire].ampliphase;
		dangle = re[ire].dangle;
		dsdx = re[ire].dsdxe;
		dsdz = re[ire].dsdze;
		v0 = 1.0/sqrt(re[ire].sb);

		/* compute ray dependant constants */
		v2 = v*v;
		v4 = v2*v2;
		dvds = -0.5*v4*(dsdx*px+dsdz*pz);
		dvdn = -0.5*v4*(dsdx*pz-dsdz*px);

		scale = sigma*dangle*sqrt(v/(v0*2*PI));

		/* complex beam epsilon */
		if (bw!=0.0) {
			eps1 = 0.0;
			eps2 = -0.5*wpeak*bw*bw;
		} else {
			temp = c * ABS(p2/q2) + eps;
			eps1 = -q2/q1 * (p2*p1/(q2*q1) + temp*temp);
			eps1 = eps1/(p1*p1/(q1*q1) + temp*temp);
			eps2 = -temp/(p1*p1+q1*q1*temp*temp);		
		}
		ceps = cmplx(eps1,eps2);

		/* complex p, q, and p/q */
		cp = crmul(ceps,p1);  cp.r += p2;
		cq = crmul(ceps,q1);  cq.r += q2;
		cpoq = cdiv(cp,cq);
		cpoqr = cpoq.r;
		cpoqi = cpoq.i;

		/* if cpoqi negative, skip ray */
		/* (cpoqi<0 leads to exponential growth!) */
		if (cpoqi<0.0) {
			continue;
		}

		/* complex q, adjusted for reflections */
		cqr = (nref%2) ? cneg(cq) : cq;

		/* frequency- and ray-independent amplitude factor */
		phase = -PI*((kmah+1)/2);
		camp = cwp_csqrt(cdiv(ceps,cqr));

		/* correction for cmplex reflection/transmission */
		if (reftrans) {
			phase += ampliphase;
			camp = crmul(camp,ampli);
		}

                /* correction for 2.5-D Spreading */
        	phase += PI/4.0;

                camp = crmul(camp,scale);
		camp = cmul(camp,cmplx(cos(phase),sin(phase)));
		campr = camp.r;
		campi = camp.i;

		/* save computing time */
     		if (natten == 2)
			for (iw=0; iw<nw; ++iw)
				lnvec[iw] = atten*lnvec[iw];

		/* loop over receiver locations */
		for (ig=0; ig<ng; ++ig) {

			/* delta x and delta z */
			dx = xg[ig]-x;
			dz = zg[ig]-z;
			disq = dx*dx+dz*dz;

			/* delta t and delta s */
			n = v*(pz*dx-px*dz);
			nn = n*n;

			/* do not extrapolate too far */
			if (lscale!=FLT_MAX && disq>sqlscale) continue;

			dels = v*(px*dx+pz*dz)*(1-dvdn/v*n);
			delt = dels/v-0.5*dvds*dels*dels/v2;

			/* adjusted time */
			ta = t+delt+0.5*cpoqr*nn-ft;

			/* skip if outside time window */
			if (ta<0.0 || ta>(nt-1)*dt) continue;

			/* compute cos and sin increments */
			cosd = cos(dw*ta);
			sind = sin(dw*ta);

			/* compute exp increment */
                        if (natten==0)
				/* no attenuation */
				expd = exp(-0.5*dw*cpoqi*nn);
			else
	                        /* include attenuation term */
				expd = exp(-0.5*dw*(cpoqi*nn+atten));

			/* initialize cos, sin, and exp */
			cosw = 1.0;
			sinw = 0.0;
			expw = 1.0;

			/* if no attenuation or noncausal attenuation */
			if (natten==0 || natten==1) {

				/* loop over frequencies */
				for (iw=0; iw<nw; ++iw) {

					/* accumulate beam */
					csyn[ig][iw].r += expw
						*(campr*cosw-campi*sinw);
					csyn[ig][iw].i += expw
						*(campr*sinw+campi*cosw);

					/* update cos, sin, and exp */
					tempd = cosw*cosd-sinw*sind;
					sinw = cosw*sind+sinw*cosd;
					cosw = tempd;
					expw *= expd;
				
					/* if amplitude too small, break */
					if (expw<0.01) break;

				}

			/* else, causal attenuation */
			} else {

				/* loop over frequencies */
				for (iw=0,w=0.0; iw<nw; ++iw,w+=dw) {

					/* compute cos and sin */
					tempd = w*(ta-lnvec[iw]);
					cosw = cos(tempd);
					sinw = sin(tempd);					

					/* accumulate beam */
					csyn[ig][iw].r += expw
						*(campr*cosw-campi*sinw);
					csyn[ig][iw].i += expw
						*(campr*sinw+campi*cosw);

					/* update exp */
					expw *= expd;

					/* if amplitude too small, break */
					if (expw<0.01) break;
				}
			}
		}
	}

	/* make complex ricker wavelet */
	for (iw=0,w=0.0; iw<nw; ++iw,w+=dw) {
		cwave[iw] = cricker(w,wpeak,delay);
		cwave[iw] = crmul(cwave[iw],sqrt(w));
	}

	/* apply wavelet to synthetics and inverse Fourier transform */
	for (ig=0; ig<ng; ++ig) {
		for (iw=0; iw<nw; ++iw) {
			csyn[ig][iw] = cmul(csyn[ig][iw],cwave[iw]);
		}
		pfacr(-1,ntfft,csyn[ig],syn);
		fwrite(syn,sizeof(float),nt,stdout);
	}
	
	/* free workspace */
	free1float(syn);
	free1float(lnvec);
	free1complex(cwave);
	free2complex(csyn);
}

static complex cricker (float w, float wpeak, float delay)
/*****************************************************************************
Compute Fourier transform of Ricker wavelet - complex function of frequency
******************************************************************************
Input:
w		frequency at which to evaluate transform
wpeak		peak (dominant) frequency in radians
delay		time shift (used for an approximately causal wavelet)
******************************************************************************
Notes:
The amplitude of the Ricker wavelet at a frequency of 2.5*wpeak is 
approximately 4 percent of that at the dominant frequency wpeak.
The Ricker wavelet effectively begins at time t = -2*PI/wpeak.  Therefore,
for practical purposes, a causal wavelet may be obtained by a time delay
of 2*PI/wpeak.
The Ricker wavelet has the shape of the second derivative of a Gaussian.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 02/28/91
******************************************************************************/
{
	return crmul(cwp_cexp(cmplx(-pow(w/wpeak,2.0),delay*w)),
		4.0*w*w*sqrt(PI)/pow(wpeak,3.0));
}

static int insideModel (Model *m, float x, float z)
/* return 1 if (x,z) inside model; 0 otherwise */
{
	return (m->ymin<=x && x<=m->ymax && m->xmin<=z && z<=m->xmax);
}

static void shootRays (Model *m, float xs, float zs,
	int nangle, float dangle, float fangle,
	int kstop, int kreflect,
	RayEnd re[])
/*****************************************************************************
Shoot rays via dynamic ray tracing for a sloth model
******************************************************************************
Input:
m		trianglulated sloth model
xs		horizontal coordinate of source
zs		vertical coordinate (depth) of source
nangle		number of ray takeoff angles
dangle		increment in ray takeoff angle
fangle		first ray takeoff angle
kstop		index of edge at which to stop ray
kreflect	index of edge at which to reflect ray

Output:
re		array[nangle] of RayEnds
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 02/28/91
modified: Andreas Rueger, Colorado School of Mines, 09/13/93
******************************************************************************/
{
	int iangle,dec;
	float angle,sb,zmax,xmax,eps;
	Tri *tris;
	FaceAttributes *fa;
	EdgeAttributes *ea;
	RayStep *rs,*rslast,*rsnext,*rs1,*rs2;

	/* determine triangle in model containing source location */
	tris = insideTriInModel(m,NULL,zs,xs);

        /* if source is on edge, move it by eps */
	dec = checkIfSourceOnEdge(tris,zs,xs);
	if (dec!=0) {
		zmax = m->xmax;
		eps = m->eps;
		if (dec==2) eps *= 5;
		zs = (zs<zmax-eps) ? zs+eps : zs-eps;

		/* if we are still on an edge, move x value */
		if (checkIfSourceOnEdge(tris,zs,xs) != 0) {
			xmax=m->ymax;
			xs = (xs<xmax-eps) ? xs+eps : xs-eps;
		}

		/* find new triangle */
		tris = insideTriInModel(m,NULL,zs,xs);
        }

	/* determine sloth at source location (beginning of ray) */
	fa = tris->fa;
	sb = fa->s00+fa->dsdx*xs+fa->dsdz*zs;

	/* allocate space for ray steps */
	rs1 = (RayStep*)malloc(sizeof(RayStep));
	rs2 = (RayStep*)malloc(sizeof(RayStep));
	
	/* loop over takeoff angles */
	for (iangle=0,angle=fangle; iangle<nangle; ++iangle,angle+=dangle) {
	
		/* initialize ray step pointers */
		rs = rs1;
		rsnext = rs2;
	
		/* initialize linked list of ray steps */
		rs->sigma = 0.0;
		rs->x = xs;
		rs->z = zs;
		rs->px = sin(angle)*sqrt(sb);
		rs->pz = cos(angle)*sqrt(sb);
		rs->q1 = 1.0;
		rs->p1 = 0.0;
		rs->q2 = 0.0;
		rs->p2 = 1.0;
		rs->t = 0.0;
		rs->kmah = 0;
		rs->nref = 0;
		rs->eu = NULL;
		rs->f = tris;
		rs->ampli = 1;
                rs->ampliphase = 0;
                rs->atten = 0;

		/* trace ray */
		do {
			/* trace ray in triangle */
			traceRayInTri(rs,rsnext);
			
			/* remember last ray step */
			rslast = rs;
			rs = rsnext;
			rsnext = rslast;
			
			/* edge attributes */
			ea = rs->eu->e->ea;
			
			/* if ray at stopping edge, stop */
			if (ea!=NULL && ea->k==kstop) {
				rslast = rs;
				break;
				
			/* else if ray at reflecting edge, reflect */
			} else if (ea!=NULL && ea->k==kreflect) {
				reflectRayFromEdge(rs,rsnext);
			
			/* else attempt to trace ray across edge */
			} else if (!traceRayAcrossEdge(rs,rsnext)) {
					rsnext = NULL;
			}
			
			/* remember last ray step */
			rslast = rs;
			rs = rsnext;
			rsnext = rslast;
			
		} while(rs!=NULL);
		
		/* save ray end parameters */
		fa = rslast->f->fa;
		re[iangle].sigma = rslast->sigma;
		re[iangle].x = rslast->x;
		re[iangle].z = rslast->z;
		re[iangle].px = rslast->px;
		re[iangle].pz = rslast->pz;
		re[iangle].t = rslast->t;
		re[iangle].q1 = rslast->q1;
		re[iangle].p1 = rslast->p1;
		re[iangle].q2 = rslast->q2;
		re[iangle].p2 = rslast->p2;
		re[iangle].kmah = rslast->kmah;
		re[iangle].nref = rslast->nref;
                re[iangle].ampli = rslast->ampli;
		if (rslast->ampliphase<-3.0 && kstop==1 && rslast->nref==0)
			fprintf(stderr,"phase=%f    ampli=%f\n",
				rslast->ampliphase,rslast->ampli);
                re[iangle].ampliphase = rslast->ampliphase;
                re[iangle].atten = rslast->atten;
		re[iangle].sb = sb;
		re[iangle].se = fa->s00+fa->dsdx*rslast->x+fa->dsdz*rslast->z;
		re[iangle].dsdxe = fa->dsdx;
		re[iangle].dsdze = fa->dsdz;
		re[iangle].ab = angle;
		re[iangle].kend = (rs==NULL) ? -1 : kstop;
		re[iangle].dangle = dangle;
	}
}

static void traceRayInTri (RayStep *rs, RayStep *rsnew)
/* Trace ray in triangle.  Return new RayStep. */
{
	int kmah,nref;
	float x,z,px,pz,t,q1,p1,q2,p2,ddt,
		s00,dsdx,dsdz,xa,za,xb,zb,dx,dz,a,b,c,
		ds,q,sigma,dsigma,dsigma1,dsigma2,small,frac;
	float ampli,ampliphase,atten,qfac;
	EdgeUse *eu,*eut,*eusmall,*euzero=NULL;
	Face *f;
	FaceAttributes *fa;

	/* get input parameters */
	sigma =	rs->sigma;
	x = rs->x;
	z = rs->z;
	px = rs->px;
	pz = rs->pz;
	t = rs->t;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	kmah = rs->kmah;
	nref = rs->nref;
	eu = rs->eu;
	f = rs->f;
	ampli = rs->ampli;
	ampliphase = rs->ampliphase;
	atten = rs->atten;

	/* determine sloth function */
	fa = f->fa;
	s00 = fa->s00;
	dsdx = fa->dsdx;
	dsdz = fa->dsdz;

	/* determine Q-factor */
	qfac = fa->qfac;

	/* determine edge intersection with smallest positive dsigma */
	eusmall = NULL;
	small = FLT_MAX;
	eut = f->eu;
	do {
		/* edge endpoints */
		xa = eut->vu->v->y;  za = eut->vu->v->x;
		xb = eut->euCW->vu->v->y;  zb = eut->euCW->vu->v->x;

		/* coefficients b and c of equation to be solved for dsigma */
		dx = xb-xa;  dz = zb-za;
		b = dx*pz-dz*px;
		c = (eut==eu) ? 0.0 : dx*(z-za)-dz*(x-xa); 

		/* if sloth constant, solve linear equation */
		if (dsdx==0.0 && dsdz==0.0) {
			if (b!=0.0)
				dsigma1 = -c/b;
			else
				dsigma1 = FLT_MAX;
			dsigma2 = FLT_MAX;
		
		/* else, if sloth not constant, solve quadratic equation */
		} else {
			a = 0.25*(dx*dsdz-dz*dsdx);
			ds = b*b-4.0*a*c;
			if (ds<0.0) {
				dsigma1 = dsigma2 = FLT_MAX;
			} else {
				q = -0.5*((b<0.0)?b-sqrt(ds):b+sqrt(ds));
				if (a!=0.0)
					dsigma1 = q/a;
				else
					dsigma1 = FLT_MAX;
				if (q!=0.0)
					dsigma2 = c/q;
				else
					dsigma2 = FLT_MAX;
			}
		}
		
		/* remember edge with smallest positive dsigma */
		if (0.0<dsigma1 && dsigma1<small) {
			small = dsigma1;
			eusmall = eut;
		}
		if (0.0<dsigma2 && dsigma2<small) {
			small = dsigma2;
			eusmall = eut;
		}

		/* remember edge for which dsigma is zero */
		if (dsigma1==0.0) euzero = eut;
		if (dsigma2==0.0) euzero = eut;

		/* next edge use */
		eut = eut->euCW;

	} while (eut!=f->eu);

	/* ray exits at edge with smallest positive dsigma */
	if (eusmall!=NULL) {
		dsigma = small;
		eu = eusmall;

	/* but if no dsigma>0, choose the edge we are up against */
	} else {
		dsigma = 0.0;
		eu = euzero;
	}

	/* update dynamic ray parameters */
	evaluateDynamic(dsigma,px,pz,dsdx,dsdz,&q1,&p1,&q2,&p2,&kmah);	

	/* update ray parameters */
	sigma += dsigma;
	ddt = dsigma*(s00+dsdx*x+dsdz*z
		+dsigma*(0.5*(dsdx*px+dsdz*pz)
		+dsigma*(0.0833333*(dsdx*dsdx+dsdz*dsdz))));
	t += ddt;
	x += dsigma*(px+0.25*dsdx*dsigma);
	z += dsigma*(pz+0.25*dsdz*dsigma);
	px += 0.5*dsdx*dsigma;
	pz += 0.5*dsdz*dsigma;

	/* don't let ray exit too close to a vertex */
	xa = eu->vu->v->y;  dx = eu->euCW->vu->v->y-xa;
	za = eu->vu->v->x;  dz = eu->euCW->vu->v->x-za;
	frac = (ABS(dx)>ABS(dz))?(x-xa)/dx:(z-za)/dz;
	if (frac<0.0001) {
		x = xa+0.0001*dx;
		z = za+0.0001*dz;
	} else if (frac>0.9999) {
		x = xa+0.9999*dx;
		z = za+0.9999*dz;
	}

	/* compute contribution due to attenuation */
	if (qfac!=FLT_MAX)
		atten += ddt/qfac;
	else
		atten = 0;

	/* return new raystep */
	rsnew->sigma = sigma;
	rsnew->x = x;
	rsnew->z = z;
	rsnew->px = px;
	rsnew->pz = pz;
	rsnew->t = t;
	rsnew->q1 = q1;
	rsnew->p1 = p1;
	rsnew->q2 = q2;
	rsnew->p2 = p2;
	rsnew->kmah = kmah;
	rsnew->nref = nref;
	rsnew->eu = eu;
	rsnew->f = f;
	rsnew->atten = atten;
	rsnew->ampli = ampli;
	rsnew->ampliphase = ampliphase;
}

static int traceRayAcrossEdge (RayStep *rs, RayStep *rsnew)
/* Trace ray across edge. */
/* If successful, return 1, along with new RayStep. */
/* If unsuccessful, return 0. */
/* Failure to trace across an edge is because: */
/* (1) the ray was incident with angle greater than the critical angle, or */
/* (2) the ray is incident at a boundary edge */
{
	int kmah,nref;
	float sigma,x,z,px,pz,t,q1,p1,q2,p2,
		s00,s1,ds1dx,ds1dz,s2,ds2dx,ds2dz,
		px1,pz1,px1r,pz1r,px2r,pz2r,pz2rs,
		c1ov1,c2ov2,oc2s,g,cterm,iterm,
		fac1,fac2,dv1dl,dv2dl,dv1dm,dv2dm,
		scale,gx,gz,hx,h_z,frac,dx,dz;
        float ampli,atten,dens1,dens2,ampliphase,coeff;
	float coeff1, coeff2, taper;
	EdgeUse *eu,*eum;
	EdgeUseAttributes *eua,*euma;
	Face *f;
	FaceAttributes *fa;

	/* get input parameters */
	sigma = rs->sigma;
	x = rs->x;
	z = rs->z;
	px = rs->px;
	pz = rs->pz;
	t = rs->t;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	kmah = rs->kmah;
	nref = rs->nref;
	eu = rs->eu;
	f = rs->f;
	atten = rs->atten;
	ampli = rs->ampli;
	ampliphase = rs->ampliphase;

	/* check for boundary */
	if (eu->euMate->f==NULL) return 0;

	/* determine sloth on this side of edge */
	fa = f->fa;
	s00 = fa->s00;
	ds1dx = fa->dsdx;
	ds1dz = fa->dsdz;
	s1 = s00+ds1dx*x+ds1dz*z;

        /* determine density on this side */
        dens1 = fa->dens;

	/* determine sloth on other side of edge */
	eum = eu->euMate;
	f = eum->f;
	fa = f->fa;
	s00 = fa->s00;
	ds2dx = fa->dsdx;
	ds2dz = fa->dsdz;
	s2 = s00+ds2dx*x+ds2dz*z;

	/* determine density on other side of the edge */
	dens2 = fa->dens;

        if (dens1==FLT_MAX) dens1 = 1.0;
        if (dens2==FLT_MAX) dens2 = 1.0;

	/* if sloth function not same on both sides of edge */
	if (s1!=s2 || ds1dx!=ds2dx || ds1dz!=ds2dz || dens1!=dens2) {

		/* edge vector */
		dx = eum->vu->v->y-eu->vu->v->y;
		dz = eum->vu->v->x-eu->vu->v->x;

		/* fractional distance along edge */
		frac = (ABS(dx)>ABS(dz))
			? (x-eu->vu->v->y)/dx : (z-eu->vu->v->x)/dz;

		/* linearly interpolate unit vector g tangent to edge */
		eua = eu->eua;
		euma = eum->eua;
		if (eua!=NULL && euma!=NULL) {
			gx = frac*euma->tx-(1.0-frac)*eua->tx;
			gz = frac*euma->tz-(1.0-frac)*eua->tz;
		} else {
			gx = -dx;
			gz = -dz;
		}
		scale = 1.0/sqrt(gx*gx+gz*gz);
		gx *= scale;
		gz *= scale;

		/* unit vector h normal to edge */
		hx = -gz;
		h_z = gx;

		/* remember ray parameters on this side */
		px1 = px;
		pz1 = pz;

		/* rotated ray parameters on this side */
		px1r = px*h_z-pz*hx;
		pz1r = px*hx+pz*h_z;

		/* rotated ray parameters on other side */
		px2r = px1r;
		pz2rs = s2-px2r*px2r;

		/* post-critical */
		if (pz2rs<=0.0) return 0;

		/* grazing incidence */
		if (pz1r*pz1r <= 0.008*s1) return 0;

		pz2r = sqrt(pz2rs);

		/* ray parameters on other side */
		px = px2r*h_z+pz2r*hx;
		pz = pz2r*h_z-px2r*hx;

		/* curvature term */
		c1ov1 = pz1r;
		c2ov2 = pz2r;
		oc2s = s2/pz2rs;
		if (eua!=NULL && euma!=NULL) {
			g = frac*euma->c-(1.0-frac)*eua->c;
			cterm = g*oc2s*(c1ov1-c2ov2);
		} else {
			cterm = 0.0;
		}

		/* update dynamic ray parameters */
		scale = (pz2r*sqrt(s1))/(pz1r*sqrt(s2));
		q1 = q1*scale;
		p1 = p1/scale+cterm*q1;
		q2 = q2*scale;
		p2 = p2/scale+cterm*q2;

		/* velocity derivatives tangent and normal to ray */
		fac1 = -0.5/(s1*s1);
		fac2 = -0.5/(s2*s2);
		dv1dl = fac1*(px1*ds1dx+pz1*ds1dz);
		dv2dl = fac2*(px*ds2dx+pz*ds2dz);
		dv1dm = fac1*(pz1*ds1dx-px1*ds1dz);
		dv2dm = fac2*(pz*ds2dx-px*ds2dz);

		/* inhomogeneity term */
		iterm = -px1r*oc2s
			*(2.0*(dv1dm*c1ov1-dv2dm*c2ov2)+px1r*(dv1dl-dv2dl));

		/* update dynamic ray parameters */
		p1 += iterm*q1;
		p2 += iterm*q2;

                /* transmission effects on amplitudes */
		if (s1!=s2 || dens1!=dens2) {
			taper = (px1r*px1r)/s2;

			/* if close to critical*/
			taper = (taper>TAPER)?cos((taper-TAPER)*5.0*PI):1.0;

			coeff1 = dens2/dens1; 
			coeff2 = pz2r/pz1r; 
			coeff = 1.0+(coeff1-coeff2)/(coeff1+coeff2);

			/* complete amplitude coeff*/
			ampli *= coeff*sqrt(pz2r/pz1r)*taper;
               }
	}

	/* return new raystep */
	rsnew->sigma = sigma;
	rsnew->x = x;
	rsnew->z = z;
	rsnew->px = px;
	rsnew->pz = pz;
	rsnew->t = t;
	rsnew->q1 = q1;
	rsnew->p1 = p1;
	rsnew->q2 = q2;
	rsnew->p2 = p2;
	rsnew->kmah = kmah;
	rsnew->nref = nref;
	rsnew->eu = eum;
	rsnew->f = f;
        rsnew->ampli = ampli;
        rsnew->ampliphase = ampliphase;
        rsnew->atten = atten;

	return 1;
}

static void reflectRayFromEdge (RayStep *rs, RayStep *rsnew)
/* Reflect ray from edge and return new RayStep. */
{
	int kmah,nref;
	float sigma,x,z,px,pz,t,q1,p1,q2,p2,
		s00,dsdx,dsdz,s,
		px1,pz1,pxr,pzr,
		c1ov1,c2ov2,oc2s,g,cterm,iterm,
		dv1dl,dv2dl,dv1dm,dv2dm,
		scale,gx,gz,hx,h_z,frac,dx,dz;
        float atten,ampli,ampliphase,dens1,dens2,s2,
               ds2dx,ds2dz,temp1,temp2;
	EdgeUse *eu,*eum;
	EdgeUseAttributes *eua,*euma;
	Face *f,*fn;
	FaceAttributes *fa;

	/* get input parameters */
	sigma = rs->sigma;
	x = rs->x;
	z = rs->z;
	px = rs->px;
	pz = rs->pz;
	t = rs->t;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	kmah = rs->kmah;
	nref = rs->nref;
	eu = rs->eu;
	f = rs->f;
	atten = rs->atten;
	ampli = rs->ampli;
	ampliphase = rs->ampliphase;

	/* determine sloth on incident side of edge */
	fa = f->fa;
	s00 = fa->s00;
	dsdx = fa->dsdx;
	dsdz = fa->dsdz;
	s = s00+dsdx*x+dsdz*z;

	/* determine dens on incident side of edge */
        dens1 = fa->dens;

	/* edge vector */
	eum = eu->euMate;

	/* determine sloth on other side of edge */
	fn = eum->f;
	fa = fn->fa;
	s00 = fa->s00;
	ds2dx = fa->dsdx;
	ds2dz = fa->dsdz;
	s2 = s00+ds2dx*x+ds2dz*z;

        /* determine density on other side of the edge */
        dens2 = fa->dens;

        if (dens1==FLT_MAX) dens1 = 1.0;
        if (dens2==FLT_MAX) dens2 = 1.0;

	dx = eum->vu->v->y-eu->vu->v->y;
	dz = eum->vu->v->x-eu->vu->v->x;

	/* fractional distance along edge */
	frac = (ABS(dx)>ABS(dz)) ? (x-eu->vu->v->y)/dx : (z-eu->vu->v->x)/dz;

	/* linearly interpolate unit vector g tangent to edge */
	eua = eu->eua;
	euma = eum->eua;
	gx = frac*euma->tx-(1.0-frac)*eua->tx;
	gz = frac*euma->tz-(1.0-frac)*eua->tz;
	scale = 1.0/sqrt(gx*gx+gz*gz);
	gx *= scale;
	gz *= scale;

	/* unit vector h normal to edge */
	hx = -gz;
	h_z = gx;

	/* remember incident ray parameters */
	px1 = px;
	pz1 = pz;

	/* rotated incident ray parameters */
	pxr = px*h_z-pz*hx;
	pzr = px*hx+pz*h_z;

	/* rotated reflected ray parameters */
	pxr = pxr;
	pzr = -pzr;

	/* reflected ray parameters */
	px = pxr*h_z+pzr*hx;
	pz = pzr*h_z-pxr*hx;

	/* curvature term */
	c1ov1 = c2ov2 = -pzr;
	oc2s = s/(pzr*pzr);
	g = frac*euma->c-(1.0-frac)*eua->c;
	cterm = g*oc2s*(c1ov1+c2ov2);

	/* update dynamic ray parameters */
	scale = -c2ov2/c1ov1;
	q1 = q1*scale;
	p1 = p1/scale+cterm*q1;
	q2 = q2*scale;
	p2 = p2/scale+cterm*q2;

	/* if sloth not constant */
	if (dsdx!=0.0 || dsdz!=0.0) {

		/* velocity derivatives tangent and normal to ray */
		scale = -0.5/(s*s);
		dv1dl = scale*(px1*dsdx+pz1*dsdz);
		dv2dl = scale*(px*dsdx+pz*dsdz);
		dv1dm = scale*(pz1*dsdx-px1*dsdz);
		dv2dm = scale*(pz*dsdx-px*dsdz);
	
		/* inhomogeneity term */
		iterm = -pxr*oc2s*
			(2.0*(dv1dm*c1ov1+dv2dm*c2ov2)+
			pxr*(dv1dl-dv2dl));
		
		/* update dynamic ray parameters */
		p1 += iterm*q1;
		p2 += iterm*q2;
	}

	/* increment number of reflections */
	++nref;

        /* pre-critical */
	if (s2-pxr*pxr>=0) {
		temp1 = -pzr*dens2/dens1/sqrt(s);
		temp2 = sqrt(s2/s-(pxr*pxr/s));
		ampli *= (temp1-temp2)/(temp1+temp2);  
	} else {
		ampliphase -= 2*atan(-sqrt(pxr*pxr-s2)*dens1/(dens2*pzr));
	}

	/* return new raystep */
	rsnew->sigma = sigma;
	rsnew->x = x;
	rsnew->z = z;
	rsnew->px = px;
	rsnew->pz = pz;
	rsnew->t = t;
	rsnew->q1 = q1;
	rsnew->p1 = p1;
	rsnew->q2 = q2;
	rsnew->p2 = p2;
	rsnew->kmah = kmah;
	rsnew->nref = nref;
	rsnew->eu = eu;
	rsnew->f = f;
	rsnew->ampli = ampli;
	rsnew->ampliphase = ampliphase;
	rsnew->atten = atten;
}

static void evaluateDynamic (float sigma, 
	float px, float pz, float dsdx, float dsdz,
	float *q1, float *p1, float *q2, float *p2, int *kmah)
/* evaluate dynamic ray parameters via modified midpoint method */
{
	double s0,s1,s2,ss,scale,a,b,c,d,e,
		q1i,p1i,q2i,p2i,q1o,p1o,q2o,p2o;

	/* get input dynamic ray parameters */
	q1i = *q1;  p1i = *p1;  q2i = *q2;  p2i = *p2;

	/* trivial case:  constant sloth or ray parallel to sloth gradient */
	if (pz*dsdx==px*dsdz) {
		q1o = q1i+p1i*sigma;
		q2o = q2i+p2i*sigma;
		p2o = p2i;
		p1o = p1i;

	/* else, general case */
	} else {

		/* constants */
		s0 = px*px+pz*pz;
		s1 = px*dsdx+pz*dsdz;
		s2 = 0.25*(dsdx*dsdx+dsdz*dsdz);
		ss = (s2*sigma+s1)*sigma+s0;
		scale = 1.0/sqrt(s0*ss);
		a = s0+0.5*s1*sigma;
		b = (0.25*s1*s1/s0-s2)*sigma;
		c = s0+s1*sigma;
		d = 0.5*s1+2.0*b;
		b *= sigma;
		e = (0.5*s1+s2*sigma)/ss;

		/* update q1 and q2 */
		q1o = scale*(a*(q1i+p1i*sigma)+b*q1i);
		q2o = scale*(a*(q2i+p2i*sigma)+b*q2i);
		
		/* update p1 and p2 */
		p1o = scale*(c*p1i+d*q1i)-e*q1o;
		p2o = scale*(c*p2i+d*q2i)-e*q2o;
	}

	/* update kmah index */
	if (q2i*q2o>=0.0 && p2i*p2o<0.0 && q2i*p2i<0.0) {
		*kmah += 2;
	} else if (q2o==0.0 || q2i*q2o<0.0 ||
		(q2i==0.0 && p2i*p2o<0.0 && q2o*p2o>0.0)) {
		*kmah += 1;
	}

	/* return updated dynamic ray parameters */
	*q1 = q1o;  *p1 = p1o;  *q2 = q2o;  *p2 = p2o;
}

int checkIfSourceOnEdge (Face *tris, float zs, float xs)
{
	float x1,x2,x3,z1,z2,z3,m12,m13,m23,b12,b13,b23,eps;
	EdgeUse *eu;

	eu = tris->eu;
	eps = tris->m->eps;

	/* get vertices */
	x1 = eu->vu->v->y;
	z1 = eu->vu->v->x;
	x2 = eu->euCW->vu->v->y;
	z2 = eu->euCW->vu->v->x;
	x3 = eu->euCCW->vu->v->y;
	z3 = eu->euCCW->vu->v->x;

	/* source is sitting on vertex */
	if ((xs-x1)*(xs-x1)+(zs-z1)*(zs-z1)<eps
		|| (xs-x2)*(xs-x2)+(zs-z2)*(zs-z2)<eps
		|| (xs-x3)*(xs-x3)+(zs-z3)*(zs-z3)<eps) return 2;

	/* check special cases and compute slope of edges */
	if (ABS(x1-x2)<0.1*eps) {
		if (ABS(xs-x1)<0.1*eps) {
			return 1;
		} else {
			m12 = 0.01*FLT_MAX;
		}
	} else {
		m12 = (z1-z2)/(x1-x2);
	}
	if (ABS(x1-x3)<0.1*eps) {
		if (ABS(xs-x1)<0.1*eps) {
			return 1;
		} else {
			m13 = 0.01*FLT_MAX;
		}
	} else {
		m13 = (z1-z3)/(x1-x3);
	}
	if (ABS(x2-x3)<0.1*eps) {
		if (ABS(xs-x2)<0.1*eps) {
			return 1;
		} else {
			m23 = 0.01*FLT_MAX;
		}
	} else {
		m23 = (z2-z3)/(x2-x3);
	}
	b12 = z1-m12*x1;
	b13 = z1-m13*x1;
	b23 = z2-m23*x2;

	/* source on edge? */
	if (ABS(zs-m12*xs-b12)<0.1*eps
		|| ABS(zs-m13*xs-b13)<0.1*eps
		|| ABS(zs-m23*xs-b23)<0.1*eps) return 1;

	return 0;
}
