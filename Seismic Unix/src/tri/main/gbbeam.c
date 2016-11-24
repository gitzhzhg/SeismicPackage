/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* GBBEAM: $Test Release: 1.1 $ ; $Date: 2011/11/21 16:56:25 $	*/

#include "par.h"
#include "Triangles/tri.h"
#include "Triangles/sloth.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" GBBEAM - Gaussian beam synthetic seismograms for a sloth model 	",
"									",
" gbbeam <rayends >syntraces xg= zg= [optional parameters]		",
"									",
" Required Parameters:							",
" xg=              x coordinates of receiver surface			",
" zg=              z coordinates of receiver surface			",
"									",
" Optional Parameters:							",
" ng=101           number of receivers (uniform distributed along surface)",
" krecord=1        integer index of receiver surface (see notes below)	",
" ang=0.0          array of angles corresponding to amplitudes in amp	",
" amp=1.0          array of amplitudes corresponding to angles in ang	",
" bw=0             beamwidth at peak frequency				",
" nt=251           number of time samples				",
" dt=0.004         time sampling interval				",
" ft=0.0           first time sample					",
" reftrans=0       =1 complex refl/transm. coefficients considered	",
" prim             =1, only single-reflected rays are considered	",     
"                  =0, only direct hits are considered			",
" atten=0          =1 add noncausal attenuation				",
"                  =2 add causal attenuation				",
" lscale=          if defined restricts range of extrapolation		",
" aperture=        maximum angle of receiver aperture			",
" fpeak=0.1/dt     peak frequency of ricker wavelet			",
" infofile         ASCII-file to store useful information		",
" NOTES:								",
" Only rays that terminate with index krecord will contribute to the	",
" synthetic seismograms at the receiver (xg,zg) locations.  The		",
" receiver locations are determined by cubic spline interpolation	",
" of the specified (xg,zg) coordinates.					",
"									",
NULL};

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 02/09/91
 * MODIFIED:  Andreas Rueger, Colorado School of Mines, 08/18/93
 *	Modifications include: 2.5-D amplitudes, computation of reflection/
 *			transmission losses, attenuation,
 *			timewindow, lscale, aperture, beam width, etc.
 */
/**************** end self doc ***********************************/

#define C 0.99
#define EPS 0.0001

/* prototypes for functions defined and used internally */
static void makexzs (int nu, float *xu, float *zu, 
	int ns, float *xs, float *zs);
static void makesyn (int prim, int nre, int reftrans, RayEnd *re,
	int nang, float *ang, float *amp, float lscale, int krecord,
	int ng, float *xg, float *zg, float fpeak, float aperture, float bw,
	int nt, float dt, float ft, int natten, FILE *infofp);
static complex cricker (float w, float wpeak, float delay);

/* the main program */
int main (int argc, char **argv)
{
	int nxg,nzg,nxz,ng,krecord,nt,nre,nrealloc,nang,iang,namp,reftrans;
	int natten,prim;
	float *xg,*zg,*ang,*amp;
	float bw,dt,ft,lscale,fpeak,aperture;
	RayEnd *re;	
	char *infofile;
	FILE *infofp=NULL;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* get parameters */
	nxg = countparval("xg");
	nzg = countparval("zg");
	if (nxg==0 || nzg==0) err("must specify both xg and zg");
	if (nxg!=nzg) err("number of xg must equal number of zg");
	nxz = nxg;
	if (!getparint("ng",&ng)) ng = 101;
	xg = ealloc1float(MAX(nxz,ng));
	zg = ealloc1float(MAX(nxz,ng));
	getparfloat("xg",xg);
	getparfloat("zg",zg);

	if (getparstring("infofile",&infofile)) infofp = efopen(infofile,"w");
	if (!getparfloat("lscale",&lscale)) lscale = FLT_MAX;
	if (!getparfloat("bw",&bw)) bw = 0.0;
	if (!getparint("krecord",&krecord)) krecord = 1;
	if (!getparint("nt",&nt)) nt = 251;
	if (!getparfloat("dt",&dt)) dt = 0.004;
	if (!getparfloat("ft",&ft)) ft = 0.0;
	if (!getparint("reftrans",&reftrans)) reftrans = 0;
	if (!getparint("atten",&natten)) natten = 0;
	if (!getparfloat("aperture",&aperture)) aperture = FLT_MAX;
	if (aperture != FLT_MAX) aperture = asin(aperture*PI/180.);
	if (!getparfloat("fpeak",&fpeak)) fpeak = 0.1/dt;
	if (!getparint("prim",&prim)) prim = INT_MAX;

	if (natten!=0 && natten!=1 && natten!=2)
		err("unknown attenuation specified:  atten=%d\n",natten);
	if (0.5/dt<2.0*fpeak) {
		fprintf(stderr,"WARNING: ALIASING POSSIBLE\n");
		fprintf(stderr,"decrease dt or reduce fpeak\n");
	}

	if ((nang=countparval("ang"))==0) nang = 1;
	if ((namp=countparval("amp"))==0) namp = 1;
	if (nang!=namp) err("number of angs must equal number of amps");
	ang = ealloc1float(nang);
	amp = ealloc1float(namp);
	if (!getparfloat("ang",ang)) ang[0] = 0.0;
	if (!getparfloat("amp",amp)) amp[0] = 1.0;
        checkpars();

	for (iang=0; iang<nang; ++iang) {
		if (iang<nang-1 && ang[iang]>ang[iang+1])
			err("angs must increase monotonically");
		ang[iang] *= PI/180.0;
	}

        if (infofp!=NULL) 
		fprintf(infofp,"THIS FILE CONTAINS BEAM INFORMATION\n\n"); 		

	/* read rayends */
	nre = 0;
	nrealloc = 200;
	re = ealloc1(nrealloc,sizeof(RayEnd));
	while (efread(&re[nre],sizeof(RayEnd),1,stdin)==1) {
		nre++;
		if (nre==nrealloc) {
			nrealloc += 200;
			re = erealloc1(re,nrealloc,sizeof(RayEnd));
		}
	}

	/* compute receiver coordinates uniformly sampled in distance s */
	makexzs(nxz,xg,zg,ng,xg,zg);

	/* make and write synthetic seismograms */
	makesyn(prim,nre,reftrans,re,nang,ang,amp,lscale,krecord,
		ng,xg,zg,fpeak,aperture,bw,nt,dt,ft,natten,infofp);

	return EXIT_SUCCESS;
}

static void makexzs (int nu, float *xu, float *zu, 
	int ns, float *xs, float *zs)
/* interpolate (x,z) coordinates uniformly sampled in distance s */
{
	int iu,nuu,iuu;
	float x,z,xlast,zlast,dx,dz,duu,uu,ds,
		*u,*s,(*xud)[4],(*zud)[4],*us;

	xud = (float(*)[4])ealloc1float(4*nu);
	zud = (float(*)[4])ealloc1float(4*nu);
	u = ealloc1float(nu);
	for (iu=0; iu<nu; ++iu)
		u[iu] = iu;
	csplin(nu,u,xu,xud);
	csplin(nu,u,zu,zud);
	nuu = 20*nu;
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
	ds = (s[nuu-1]-s[0])/((ns>1)?ns-1:1);
	yxtoxy(nuu,duu,0.0,s,ns,ds,0.0,0.0,(float)(nu-1),us);
	intcub(0,nu,u,xud,ns,us,xs);
	intcub(0,nu,u,zud,ns,us,zs);
	free1float(us);
	free1float(s);
	free1float(u);
	free1float((float*)xud);
	free1float((float*)zud);
}

static void makesyn (int prim, int nre, int reftrans, RayEnd *re,
	int nang, float *ang, float *amp, float lscale, int krecord,
	int ng, float *xg, float *zg, float fpeak, float aperture, float bw,
	int nt, float dt, float ft, int natten, FILE *infofp)
/* make and write synthetic seismograms */
{
	int ntfft,nw,iw,ig,ire,kend,kmah,nref,*count;
	float dw,w,wpeak,delay,ampa,sigma,scale,dsdx,dsdz,
		t,x,z,px,pz,q1,p1,q2,p2,v0,v,a0,dvds,dvdn,disq,
		dx,dz,delt,dels,nn,phase,ta,cpoqr,cpoqi,v4,temp,
		*syn,*lnvec,*attenvec,wref,sqlscale,n,v2;
        float ampli,ampliphase,atten,dangle,eps1,eps2,c,eps;
	double campr,campi,cosw,sinw,expw,cosd,sind,expd,tempd;
	complex ceps,cq,cp,cpoq,cqr,camp,
		*cwave,**csyn;

	/* constants */
	ntfft = npfaro(nt,2*nt);
	nw = ntfft/2+1;
	dw = 2.0*PI/(ntfft*dt);
	wpeak = 2*PI*fpeak;
	delay = 0.0;
	wref = 1000.0;
	sqlscale = lscale*lscale;
	c = C;
	eps = EPS;

	/* allocate workspace */
	syn = ealloc1float(ntfft);
	cwave = ealloc1complex(nw);
	csyn = ealloc2complex(nw,ng);
	lnvec = ealloc1float(nw);
	attenvec = ealloc1float(nw);
	count = ealloc1int(ng);

	/* initialize synthetics */
	for (ig=0; ig<ng; ++ig) {
		count[ig] = 0;
		for (iw=0; iw<nw; ++iw)
			csyn[ig][iw] = cmplx(0.0,0.0);
        }

	/* precomputing causal attenuation term */
        if (natten==2) {
		temp = 1.0/PI;
		lnvec[0] = -1.0;
		for (iw=1,w=dw; iw<nw; ++iw,w+=dw)
			lnvec[iw] = log(w/wref)*temp;
	}
	
	/* print useful information */
	if (infofp!=NULL)
		fprintf(infofp,
			"***** %d rays in rayendfile.\n" 		
			"***** %s attenuation is simulated.\n"
			"***** Geometrical spreading is "
			"two-and-one-half dimensional.\n",
			nre,
			(natten==0)?"No":((natten==1)?"Noncausal":"Causal"));

	/* loop over rays */
	for (ire=0; ire<nre; ++ire) {

		/* determine index of ray end */
		kend = re[ire].kend;

		/* if not the end index we want, skip this ray */
		if (kend!=krecord) continue;

		nref = re[ire].nref;

		/* if ray was not reflected */
		if (nref != prim && prim != INT_MAX) continue;

		px = re[ire].px;
		v = 1.0/sqrt(re[ire].se);

		/* if not within aperture, skip this ray */
		if (ABS(px*v)>aperture) {
			if (infofp!=NULL) fprintf(infofp,
				"Ray %d is out of the aperature\n",ire);
			continue;
		}

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
		a0 = re[ire].ab;

		/* compute ray dependant constants */
		v2 = v*v;
		v4 = v2*v2;
		dvds = -0.5*v4*(dsdx*px+dsdz*pz);
		dvdn = -0.5*v4*(dsdx*pz-dsdz*px);

		/* useful ray dependant information */
                if (infofp!=NULL) {
			fprintf(infofp,
				"\n***Ray emerges at (x=%g,z=%g) "
				"on interface %d.\n",x,z,kend); 		
			fprintf(infofp,"   Emergence angle=%g    time=%g\n",
				asin(v*px)*180.0/PI,t); 		
			fprintf(infofp,"   v=%g    dsdx=%g    dsdz=%g\n",
				v,dsdx,dsdz); 		
			fprintf(infofp,"   dvds=%g    dvdn=%g\n",dvds,dvdn); 
                	fprintf(infofp,"   ampli=%g    ampliphase=%g\n",
				ampli,ampliphase);
		}

		if (dangle==0.0) dangle = 1.0;
		scale = sigma*dangle*sqrt(v/(v0*2*PI));

		/* angularly dependent amplitude */
		intlin(nang,ang,amp,amp[0],amp[nang-1],1,&a0,&ampa);

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
			if (infofp!=NULL)
			 fprintf(infofp,"cpoqi negative, skipping ray!");
			continue;
		}

		/* complex q, adjusted for reflections */
		cqr = (nref%2) ? cneg(cq) : cq;
		
		/* frequency-independent amplitude factor */
		phase = -PI*((kmah+1)/2);
		camp = cwp_csqrt(cdiv(ceps,cqr));

                if (infofp!=NULL) {
			temp = sqrt(2*((eps1*q1+q2)*(eps1*q1+q2)
				+ eps2*eps2*q1*q1)/(-wpeak*eps2));
			fprintf(infofp,"   Gaussian beam information:\n"); 
			fprintf(infofp,"   Re(M)=%g \t Im(M)=%g\n",
				cpoqr,cpoqi); 		
			fprintf(infofp,"   const beam phase=%g degrees\n",
				180.0/PI*atan(camp.i/(camp.r+eps)));
			fprintf(infofp,"   epsilon1=%g \t epsilon2=%g\n",
				eps1,eps2); 	
			fprintf(infofp,"   ray-end beam width=%g\n",temp); 		
		}

		/* complex reflection/transmission coefficient */
		if (reftrans) {
			phase += ampliphase;
			camp = crmul(camp,ampli);
		}

		/* phase correction for 2.5-D spreading */
		phase += PI/4.0;

                camp = crmul(camp,scale*ampa);
		camp = cmul(camp,cmplx(cos(phase),sin(phase)));
		campr = camp.r;
		campi = camp.i;

		/* precompute causal attenuation */
		if (natten==2) {
			for (iw=0; iw<nw; ++iw)
				attenvec[iw] = atten*lnvec[iw];
		}

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

			/* count beams that influence this receiver */
			++count[ig];

			dels = v*(px*dx+pz*dz)*(1.0-dvdn/v*n);
			delt = dels/v-0.5*dvds*dels*dels/v2;

			/* adjusted time */
			ta = t+delt+0.5*cpoqr*nn-ft;

			/* uncomment for extrapolation information */
/*			if (infofp!=NULL)
				fprintf(infofp,
					"   n=%g   ds=%g   dt=%g   tt=%g\n",
					n,dels,delt,ta);
*/

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
					tempd = w*(ta-attenvec[iw]);
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
		for (iw=0; iw<nw; ++iw)
			csyn[ig][iw] = cmul(csyn[ig][iw],cwave[iw]);
		pfacr(-1,ntfft,csyn[ig],syn);
		if (efwrite(syn,sizeof(float),nt,stdout)!=nt)
			err("error writing output traces!\n");
	}

	/* receiver contibution check */
        if (infofp!=NULL) {
		fprintf(infofp,"\nBeam contribution to each receiver "
				"*************************\n");
		fprintf(infofp,"Receiver No\t x_coord. \t z_coord. \t "
				"No. of beams\n");
	        for (ig=0; ig<ng; ++ig) 
			fprintf(infofp,"%d \t\t %g \t\t %g \t\t %d\n",
				ig,xg[ig],zg[ig],count[ig]);
        }

	/* free workspace */
        free1float(syn);
        free1float(lnvec);
        free1float(attenvec);
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
