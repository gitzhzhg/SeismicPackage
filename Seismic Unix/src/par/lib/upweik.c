/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "par.h"

/*********************** self documentation **********************/
/*****************************************************************************
UPWEIK - Upwind Finite Difference Eikonal Solver

eikpex - Eikonal equation extrapolation of times and derivatives in 
         polar coordinates
ray_theoretic_sigma - difference equation extrapolation of "ray_theoretic_sigma" in polar coordinates
ray_theoretic_beta - difference equation extrapolation of "ray_theoretic_beta" in polar coordinates
eiktam - Compute traveltimes t(x,z) and  propagation angle a(x,z) via 
         eikonal equation, and ray_theoretic_sigma sig(x,z), incident angle bet(x,z) 
         via Crank-Nicolson Method
******************************************************************************
Function Prototypes:
void eikpex (int na, float da, float r, float dr, 
	float sc[], float uc[], float wc[], float tc[],
	float sn[], float un[], float wn[], float tn[]);
void ray_theoretic_sigma (int na, float da, float r, float dr, 
	float uc[], float wc[], float sc[],
	float un[], float wn[], float sn[]);
void ray_theoretic_beta (int na, float da, float r, float dr, 
	float uc[], float wc[], float bc[],
	float un[], float wn[], float bn[]);
void eiktam (float xs, float zs, 
	int nz, float dz, float fz, int nx, float dx, float fx, float **vel,
	float **time, float **angle, float **sig, float **bet)
******************************************************************************
eikpex:
Input:
na		number of a samples
da		a sampling interval
r		current radial distance r
dr		radial distance to extrapolate
sc		array[na] of slownesses at current r
uc		array[na] of dt/dr at current r
wc		array[na] of dt/da at current r
tc		array[na] of times t at current r
sn		array[na] of slownesses at next r

Output:
un		array[na] of dt/dr at next r (may be equivalenced to uc)
wn		array[na] of dt/da at next r (may be equivalenced to wc)
tn		array[na] of times t at next r (may be equivalenced to tc)

******************************************************************************
ray_theoretic_sigma:
Input:
na		number of a samples
da		a sampling interval
r		current radial distance r
dr		radial distance to extrapolate
uc		array[na] of dt/dr at current r
wc		array[na] of dt/da at current r
sc		array[na] of ray_theoretic_sigma  at current r
un		array[na] of dt/dr at next r
wn		array[na] of dt/da at next r

Output:
sn		array[na] of ray_theoretic_sigma at next r 
******************************************************************************
ray_theoretic_beta:
Input:
na		number of a samples
da		a sampling interval
r		current radial distance r
dr		radial distance to extrapolate
uc		array[na] of dt/dr at current r
wc		array[na] of dt/da at current r
bc		array[na] of ray_theoretic_beta  at current r
un		array[na] of dt/dr at next r
wn		array[na] of dt/da at next r

Output:
bn		array[na] of ray_theoretic_beta at next r 
******************************************************************************
eiktam:
Input:
xs		x coordinate of source (must be within x samples)
zs		z coordinate of source (must be within z samples)
nz		number of z samples
dz		z sampling interval
fz		first z sample
nx		number of x samples
dx		x sampling interval
fx		first x sample
vel		array[nx][nz] containing velocities

Output:
time		array[nx][nz] containing first-arrival times
angle		array[nx][nz] containing propagation angles
sig  		array[nx][nz] containing ray_theoretic_sigmas
bet		array[nx][nz] containing ray_theoretic_betas
******************************************************************************
Notes:
eikpex:
If na*da==2*PI, then the angular coordinate is wrapped around (periodic). 

This function implements the finite-difference method described by Bill
Symes (Rice University) and Jos van Trier (Stanford University) in a
(1990) preprint of a paper submitted to Geophysics.

ray_theoretic_sigma:
This routine implements the Crank-Nicolson finite-difference method with
boundary conditions dray_theoretic_sigma/da=0.

ray_theoretic_beta:
This function implements the Crank-Nicolson finite-difference 
method, with boundary conditions dray_theoretic_beta/da=1. 

eiktam:
The actual computation of times and ray_theoretic_sigmas is done in polar coordinates,
with bilinear interpolation used to map to/from rectangular coordinates.

******************************************************************************
Authors: CWP: Zhenuye Liu, based on code by Dave Hale, 1992.
******************************************************************************/

/**************** end self doc ********************************/

#define TINY 1.0e-3	/* avoid divide by zero */
#define CFL 0.98	/* Courant/Friedrichs/Lewy stability factor */

void eikpex (int na, float da, float r, float dr, 
	float sc[], float uc[], float wc[], float tc[],
	float sn[], float un[], float wn[], float tn[])
/*****************************************************************************
eikpex - Eikonal equation extrapolation of times and derivatives in 
         polar coordinates
******************************************************************************
Input:
na		number of a samples
da		a sampling interval
r		current radial distance r
dr		radial distance to extrapolate
sc		array[na] of slownesses at current r
uc		array[na] of dt/dr at current r
wc		array[na] of dt/da at current r
tc		array[na] of times t at current r
sn		array[na] of slownesses at next r

Output:
un		array[na] of dt/dr at next r (may be equivalenced to uc)
wn		array[na] of dt/da at next r (may be equivalenced to wc)
tn		array[na] of times t at next r (may be equivalenced to tc)
******************************************************************************
Notes:
If na*da==2*PI, then the angular coordinate is wrapped around (periodic). 

This function implements the finite-difference method described by Bill
Symes (Rice University) and Jos van Trier (Stanford University) in a
(1990) preprint of a paper submitted to Geophysics.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/16/90
******************************************************************************/
{
	int i,wrap;
	float drleft,drorig,frac,cmax,umaxl,uminr,uminm,umaxm,
		uu,unew,uold,ueol,ueor,wor,or,*wtemp,*s;
	
	/* allocate workspace */
	wtemp = alloc1float(na);
	s = alloc1float(na);
	
	/* remember the step size */
	drleft = drorig = dr;
	
	/* initialize slownesses to values at current r */
	for (i=0; i<na; ++i)
		s[i] = sc[i];
	
	/* copy inputs to output */
	for (i=0; i<na; ++i) {
		un[i] = uc[i];
		wn[i] = wc[i];
		tn[i] = tc[i];
	}
	
	/* determine if angular coordinate wraps around */
	wrap = ABS(na*da-2.0*PI)<0.01*ABS(da);
	
	/* loop over intermediate steps with adaptive stepsize */
	while (drleft>0.0) {
		
		/* determine adaptive step size according to CFL condition */
		for (i=0,cmax=TINY; i<na; ++i) {
			if (r*ABS(un[i])<TINY*ABS(wn[i]))
				cmax = 1.0/TINY;
			else
				cmax = MAX(cmax,ABS(wn[i]/(r*un[i])));
		}
		dr = MIN(drleft,CFL/cmax*r*da);
		
		/* if angles wrap around */
		if (wrap) {
			umaxl = (wn[na-1]>0.0 ? un[na-1] : s[0]);
			if (wn[0]>0.0) {
				uminm = s[0];
				umaxm = un[0];
			} else {
				uminm = un[0];
				umaxm = s[0];
			}
			uminr = (wn[1]>0.0 ? s[0] : un[1]);
			ueol = uminm+umaxl;
			ueor = uminr+umaxm;
			wtemp[0] = wn[0]+dr*(ueor-ueol)/da;
			umaxl = (wn[na-2]>0.0 ? un[na-2] : s[na-1]);
			if (wn[na-1]>0.0) {
				uminm = s[na-1];
				umaxm = un[na-1];
			} else {
				uminm = un[na-1];
				umaxm = s[na-1];
			}
			uminr = (wn[0]>0.0 ? s[na-1] : un[0]);
			ueol = uminm+umaxl;
			ueor = uminr+umaxm;
			wtemp[na-1] = wn[na-1]+dr*(ueor-ueol)/da;
		
		/* else, if angles do not wrap around */
		} else {
			if (wn[0]<=0.0)
				wtemp[0] = wn[0] + 
					dr*(un[1]-un[0])/da; 
			else
				wtemp[0] = 0.0;
			if (wn[na-1]>=0.0) 
				wtemp[na-1] = wn[na-1] +
					dr*(un[na-1]-un[na-2])/da;
			else
				wtemp[na-1] = 0.0;
		}
		
		/* update interior w values via Enquist/Osher scheme */
		for (i=1; i<na-1; ++i) {
			umaxl = (wn[i-1]>0.0 ? un[i-1] : s[i]);
			if (wn[i]>0.0) {
				uminm = s[i];
				umaxm = un[i];
			} else {
				uminm = un[i];
				umaxm = s[i];
			}
			uminr = (wn[i+1]>0.0 ? s[i] : un[i+1]);
			ueol = uminm+umaxl;
			ueor = uminr+umaxm;
			wtemp[i] = wn[i]+dr*(ueor-ueol)/da;
		}
		
		/* decrement the size of step left to do */
		drleft -= dr;
		
		/* update radial coordinate and its inverse */
		r += dr;
		or = 1.0/r;
		
		/* linearly interpolate slowness for new r */
		frac = drleft/drorig;
		for (i=0; i<na; ++i)
			s[i] = frac*sc[i]+(1.0-frac)*sn[i];
		
		/* update w and u; integrate u to get t */
		for (i=0; i<na; i++) {
			wn[i] = wtemp[i];
			wor = wn[i]*or;
			uu = (s[i]-wor)*(s[i]+wor);
			if(uu<=0) err("\tRaypath has a too large curvature!\n\t A smoother velocity is required. \n");
 			unew = sqrt(uu); 
			uold = un[i];
			un[i] = unew;
			tn[i] += 0.5*dr*(unew+uold);
		}
	}
	
	/* free workspace */
	free1float(wtemp);
	free1float(s);
}

void ray_theoretic_sigma (int na, float da, float r, float dr, 
	float uc[], float wc[], float sc[],
	float un[], float wn[], float sn[])
/*****************************************************************************
ray_theoretic_sigma - difference equation extrapolation of "ray_theoretic_sigma" in polar coordinates
******************************************************************************
Input:
na		number of a samples
da		a sampling interval
r		current radial distance r
dr		radial distance to extrapolate
uc		array[na] of dt/dr at current r
wc		array[na] of dt/da at current r
sc		array[na] of ray_theoretic_sigma  at current r
un		array[na] of dt/dr at next r
wn		array[na] of dt/da at next r

Output:
sn		array[na] of ray_theoretic_sigma at next r 
******************************************************************************

This function implements the Crank-Nicolson finite-difference method with
boundary conditions dray_theoretic_sigma/da=0.
******************************************************************************
Author:  Zhenyue Liu, Colorado School of Mines, 07/8/92
******************************************************************************/
{
	int i;
	float r1,*d,*b,*c,*e;
	
	/* allocate workspace */
	d = alloc1float(na-2);
	b = alloc1float(na-2);
	c = alloc1float(na-2);
	e = alloc1float(na-2);
	
	r1 = r+dr;
 	
	/* Crank-Nicolson */
 	for (i=0; i<na-2; ++i) {
		d[i] = (uc[i+1]+un[i+1])/(2.0*dr);
		e[i] = (wn[i+1]/(r1*r1)+wc[i+1]/(r*r))/(8.0*da);
		b[i] = 1.0-(sc[i+2]-sc[i])*e[i]
			+d[i]*sc[i+1];
		c[i] = -e[i];
	} 
	d[0] += c[0];
	d[na-3] += e[na-3]; 
	
	tripp(na-2,d,e,c,b);
	for(i=0;i<na-2; ++i) sn[i+1]=b[i];
	sn[0] = sn[1];
	sn[na-1] = sn[na-2];
	
	
	/* free workspace */
	free1float(d);
	free1float(c);
	free1float(e);
	free1float(b);
}

void ray_theoretic_beta (int na, float da, float r, float dr, 
	float uc[], float wc[], float bc[],
	float un[], float wn[], float bn[])
/*****************************************************************************
ray_theoretic_beta - difference equation extrapolation of "ray_theoretic_beta" in polar coordinates
******************************************************************************
Input:
na		number of a samples
da		a sampling interval
r		current radial distance r
dr		radial distance to extrapolate
uc		array[na] of dt/dr at current r
wc		array[na] of dt/da at current r
bc		array[na] of ray_theoretic_beta  at current r
un		array[na] of dt/dr at next r
wn		array[na] of dt/da at next r

Output:
bn		array[na] of ray_theoretic_beta at next r 
******************************************************************************
Notes: This function implements the Crank-Nicolson finite-difference 
method, with boundary conditions dray_theoretic_beta/da=1. 
******************************************************************************
author:  Zhenyue Liu, Colorado School of Mines, 07/8/92
******************************************************************************/
{
	int i;
	float r1,*d,*b,*c,*e;
	
	/* allocate workspace */
	d = alloc1float(na-2);
	b = alloc1float(na-2);
	c = alloc1float(na-2);
	e = alloc1float(na-2);
	
	r1 = r+dr;
	/* Crank-Nicolson */
   	for (i=0; i<na-2; ++i) {
		d[i] = uc[i+1]*r*r+un[i+1]*r1*r1;
		e[i] = (wn[i+1]+wc[i+1])*dr/(4.0*da);
		b[i] = -(bc[i+2]-bc[i])*e[i]
			+d[i]*bc[i+1];
		c[i] = -e[i];
	}   
	d[0] += c[0];
	d[na-3] += e[na-3]; 
	b[0] += da*c[0];
	b[na-3] -= da*e[na-3];
	
	tripp(na-2,d,e,c,b);
	for(i=0;i<na-2; ++i) bn[i+1]=b[i];
	bn[0] = bn[1]-da;
	bn[na-1] = bn[na-2]+da;
	
	
	/* free workspace */
	free1float(d);
	free1float(c);
	free1float(e);
	free1float(b);
}

/* functions defined and used internally */
void eiktam (float xs, float zs, 
	int nz, float dz, float fz, int nx, float dx, float fx, float **vel,
	float **time, float **angle, float **sig, float **bet)
/*****************************************************************************
eiktam - Compute traveltimes t(x,z) and  propagation angle a(x,z) via 
         eikonal equation, and ray_theoretic_sigma sig(x,z), incident angle bet(x,z) 
         via Crank-Nicolson Method
******************************************************************************
Input:
xs		x coordinate of source (must be within x samples)
zs		z coordinate of source (must be within z samples)
nz		number of z samples
dz		z sampling interval
fz		first z sample
nx		number of x samples
dx		x sampling interval
fx		first x sample
vel		array[nx][nz] containing velocities

Output:
time		array[nx][nz] containing first-arrival times
angle		array[nx][nz] containing propagation angles
sig  		array[nx][nz] containing ray_theoretic_sigmas
bet		array[nx][nz] containing ray_theoretic_betas
******************************************************************************
Notes:
The actual computation of times and ray_theoretic_sigmas is done in polar coordinates,
with bilinear interpolation used to map to/from rectangular coordinates.
******************************************************************************
Revisor:  Zhenyue Liu, Colorado School of Mines, 7/8/92
******************************************************************************/
{
	int ix,iz,ia,ir,na,nr;
	float ss,a,r,da,dr,fa,fr,ex,ez,ea,rmax,rmaxs,
		**s,**sp,**tp,**up,**wp,**ap;

	/* shift coordinates so source is at (x=0,z=0) */
	fx -= xs;
	fz -= zs;
	ex = fx+(nx-1)*dx;
	ez = fz+(nz-1)*dz;
	
	/* determine polar coordinate sampling */
	rmaxs = fx*fx+fz*fz;
	rmaxs = MAX(rmaxs,fx*fx+ez*ez);
	rmaxs = MAX(rmaxs,ex*ex+ez*ez);
	rmaxs = MAX(rmaxs,ex*ex+fz*fz);
	rmax = sqrt(rmaxs);
	dr = MIN(ABS(dx),ABS(dz));
	nr = 1+NINT(rmax/dr);
	dr = rmax/(nr-1);
	fr = 0.0;
	if (fx==0.0 && fz==0.0) {
		fa = 0.0;  ea = PI/2.0;
	} else if (fx<0.0 && fz==0.0) {
		fa = -PI/2.0;  ea = PI/2.0;
	} else if (fx==0.0 && fz<0.0) {
		fa = 0.0;  ea = PI;
	} else {
		fa = -PI;  ea = PI;
	}
	da = dr/rmax;
	na = 1+NINT((ea-fa)/da);
	da = (ea-fa)/(na-1);
	if (fa==-PI && ea==PI)
		na = na-1;
	
	/* allocate space */
	s = alloc2float(nz,nx);
	sp = alloc2float(na,nr);
	tp = alloc2float(na,nr);
	up = alloc2float(na,nr);
	wp = alloc2float(na,nr);
	ap = alloc2float(na,nr);
	
	/* compute slownesses */
	for (ix=0; ix<nx; ++ix)
		for (iz=0; iz<nz; ++iz)
			s[ix][iz] = 1.0/vel[ix][iz];
	
	/* convert from rectangular to polar coordinates */
	recttopolar(nz,dz,fz,nx,dx,fx,s,na,da,fa,nr,dr,fr,sp);
	
	/* average the slownesses in source region */
	for (ir=0,ss=0.0; ir<2; ++ir)
		for (ia=0; ia<na; ++ia)
			ss += sp[ir][ia];
	ss /= 2*na;

	/* compute traveltimes and derivatives in source region */
	for (ir=0,r=0; ir<2; ++ir,r+=dr) {
		for (ia=0; ia<na; ++ia) {
			up[ir][ia] = ss;
			wp[ir][ia] = 0.0;
			tp[ir][ia] = r*ss;
		}
	}

/* 	tt=cpusec();   */
	/* solve eikonal equation for remaining times and derivatives */
	for (ir=1,r=dr; ir<nr-1; ++ir,r+=dr) {
		eikpex(na,da,r,dr,
			sp[ir],up[ir],wp[ir],tp[ir],
			sp[ir+1],up[ir+1],wp[ir+1],tp[ir+1]);
	}
	
	/* convert times from polar to rectangular coordinates */
	polartorect(na,da,fa,nr,dr,fr,tp,nz,dz,fz,nx,dx,fx,time);

/*  	fprintf(stderr,"\t CPU time for traveltimes= %f \n",cpusec()-tt); 
 	tt=cpusec();   */
	
	/* compute propagation angles in polar and convert */
	for (ia=0,a=fa; ia<na; ++ia,a+=da)
		ap[0][ia] = a;
	for (ir=1,r=fr+dr; ir<nr; ++ir,r+=dr)
		for (ia=0,a=fa; ia<na; ++ia,a+=da){
		    ap[ir][ia] = a+asin(wp[ir][ia]/(sp[ir][ia]*r));
		}
	polartorect(na,da,fa,nr,dr,fr,ap,nz,dz,fz,nx,dx,fx,angle);
/*  	fprintf(stderr,"\t CPU time for propagation angles= %f\n", 	
		cpusec()-tt); 
	tt=cpusec();   */
	
	/* compute ray_theoretic_sigmas  for initial values */
	for (ir=0,r=0; ir<2; ++ir,r+=dr) 
		for (ia=0; ia<na; ++ia) tp[ir][ia] = r/ss;

	/* solve diffrence equation for remaining ray_theoretic_sigmas */
	for (ir=1,r=dr; ir<nr-1; ++ir,r+=dr) 
 		ray_theoretic_sigma(na,da,r,dr,up[ir],wp[ir],tp[ir],
			up[ir+1],wp[ir+1],tp[ir+1]);  
	polartorect(na,da,fa,nr,dr,fr,tp,nz,dz,fz,nx,dx,fx,sig);

/* 	fprintf(stderr,"\t CPU time for sigmas= %f \n",cpusec()-tt); 
	tt=cpusec(); */
	
	/* compute ray_theoretic_betas for initial values */
	for (ir=0; ir<2; ++ir) 
		for (ia=0,a=fa; ia<na; ++ia,a+=da) tp[ir][ia] = a;

	/* solve diffrence equation for remaining ray_theoretic_betas */
	for (ir=1,r=dr; ir<nr-1; ++ir,r+=dr) 
 		ray_theoretic_beta(na,da,r,dr,up[ir],wp[ir],tp[ir],
			up[ir+1],wp[ir+1],tp[ir+1]);  
	polartorect(na,da,fa,nr,dr,fr,tp,nz,dz,fz,nx,dx,fx,bet);
	
/* 	fprintf(stderr,"\t CPU time for incident angles= %f \n",
		cpusec()-tt); */
	
	/* free space */
	free2float(s);
	free2float(sp);
	free2float(tp);
	free2float(up);
	free2float(wp);
	free2float(ap);
}

