/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* MODELING: $Revision: 1.5 $ ; $Date: 2011/10/20 21:08:05 $	*/

/*********************** self documentation **********************/
/*************************************************************************
MODELING - Seismic Modeling Subroutines for SUSYNLV and clones

decodeReflectors	parse reflectors parameter string
decodeReflector		decode a particular reflector
breakReflectors		break up reflectors duplicating interior (x,z) points
makeref			make piecewise cubic reflectors		
raylv2		 	Trace ray between two points,
				for linear velocity v = v00+dvdx*x+dvdz*z

addsinc		Add sinc wavelet to trace at specified time and
			with specified amplitude

makericker	make a Ricker wavelet

**************************************************************************
Function Prototypes:
void decodeReflectors (int *nrPtr,
	float **aPtr, int **nxzPtr, float ***xPtr, float ***zPtr);
int decodeReflector (char *string,
	float *aPtr, int *nxzPtr, float **xPtr, float **zPtr);
void breakReflectors (int *nr, float **ar, 
	int **nu, float ***xu, float ***zu);
void makeref (float dsmax, int nr, float *ar, 
	int *nu, float **xu, float **zu, Reflector **r);
void raylv2 (float v00, float dvdx, float dvdz,
	float x0, float z0, float x, float z,
	float *c, float *s, float *t, float *q);
void addsinc (float time, float amp,
	int nt, float dt, float ft, float *trace);
void makericker (float fpeak, float dt, Wavelet **w);
	
**************************************************************************
Notes:
Typedefs used by Hale's modeling
typedef struct ReflectorSegmentStruct {
	float x;	* x coordinate of segment midpoint *
	float z;	* z coordinate of segment midpoint *
	float s;	* x component of unit-normal-vector *
	float c;	* z component of unit-normal-vector *
} ReflectorSegment;
typedef struct ReflectorStruct {
	int ns;			* number of reflector segments *
	float ds;		* segment length *
	float a;		* amplitude of reflector *
	ReflectorSegment *rs;	* array[ns] of reflector segments *
} Reflector;
typedef struct WaveletStruct {
	int lw;			* length of wavelet *
	int iw;			* index of first wavelet sample *
	float *wv;		* wavelet sample values *
} Wavelet;

These are items used in SUSYNLV, SUSYNVXZ, SUSYNLVCW.

**************************************************************************
Author: Dave Hale, Colorado School of Mines, 09/17/91
**************************************************************************/
/**************** end self doc ********************************/

#include "par.h"

/*
 * Credits: CWP Dave Hale, 09/17/91,  Colorado School of Mines
 */


void decodeReflectors (int *nrPtr,
	float **aPtr, int **nxzPtr, float ***xPtr, float ***zPtr)
/*************************************************************************
decodeReflectors - parse reflectors parameter string
**************************************************************************
Output:
nrPtr		pointer to nr an int specifying number of reflectors
aPtr		pointer to a specifying reflector amplitudes
nxzPtr		pointer to nxz specifying number of (x,z) pairs defining the
		reflectors
xPtr		pointer to array[x][nr] of x values for the entire model
zPtr		array[z][nr] of z values for the entire model

***************************************************************************
Author: Dave Hale, Colorado School of Mines, 09/17/91
**************************************************************************/
{
	int nr,*nxz,ir;
	float *a,**x,**z;
	char t[1024],*s;

	/* count reflectors */
	nr = countparname("ref");
	if (nr==0) nr = 1;
	
	/* allocate space */
	a = ealloc1(nr,sizeof(float));
	nxz = ealloc1(nr,sizeof(int));
	x = ealloc1(nr,sizeof(float*));
	z = ealloc1(nr,sizeof(float*));

	/* get reflectors */
	for (ir=0; ir<nr; ++ir) {
		if (!getnparstring(ir+1,"ref",&s)) s = "1:1,2;4,2";
		strcpy(t,s);
		if (!decodeReflector(t,&a[ir],&nxz[ir],&x[ir],&z[ir]))
			err("Reflector number %d specified "
				"incorrectly!\n",ir+1);
	}

	/* set output parameters before returning */
	*nrPtr = nr;
	*aPtr = a;
	*nxzPtr = nxz;
	*xPtr = x;
	*zPtr = z;
}

/* parse one reflector specification; return 1 if valid, 0 otherwise */
int decodeReflector (char *string,
	float *aPtr, int *nxzPtr, float **xPtr, float **zPtr)
/**************************************************************************
decodeReflector - parse one reflector specification
***************************************************************************
Input:
string		string representing reflector

Output:
aPtr		pointer to amplitude value
nxzPtr		pointer to number of x,z pairs
xPtr		array of x values for one reflector
zPtr		array of z values for one reflector
***************************************************************************
Author: Dave Hale, Colorado School of Mines, 09/17/91
**************************************************************************/
{
	int nxz,ixz;
	float a,*x,*z;
	char *s,*t;

	/* if specified, get reflector amplitude; else, use default */
	s = string;
	if (strchr(s,':')==NULL) {
		a = 1.0;
		s = strtok(s,",;\0");
	} else {
		/* if (strcspn(s,":")>=strcspn(s,",;\0")) return 0;*/
		if (((unsigned int) strcspn(s,":")) >=
		    ((unsigned int) strcspn(s,",;\0"))) return 0;
		a = atof(s=strtok(s,":"));
		s = strtok(NULL,",;\0");
	}

	/* count x and z values, while splitting string into tokens */
	for (t=s,nxz=0; t!=NULL; ++nxz)
		t = strtok(NULL,",;\0");
	
	/* total number of values must be even */
	if (nxz%2) return 0;

	/* number of (x,z) pairs */
	nxz /= 2;

	/* 2 or more (x,z) pairs are required */
	if (nxz<2) return 0;

	/* allocate space */
	x = ealloc1(nxz,sizeof(float));
	z = ealloc1(nxz,sizeof(float));

	/* convert (x,z) values */
	for (ixz=0; ixz<nxz; ++ixz) {
		x[ixz] = atof(s);
		s += strlen(s)+1;
		z[ixz] = atof(s);
		s += strlen(s)+1;
	}

	/* set output parameters before returning */
	*aPtr = a;
	*nxzPtr = nxz;
	*xPtr = x;
	*zPtr = z;
	return 1;
}

/* Break up reflectors by duplicating interior (x,z) points */
void breakReflectors (int *nr, float **ar, 
	int **nu, float ***xu, float ***zu)
{
	int nri,nro,*nui,*nuo,ir,jr,iu;
	float *ari,*aro,**xui,**zui,**xuo,**zuo;

	/* input reflectors */
	nri = *nr;
	ari = *ar;
	nui = *nu;
	xui = *xu;
	zui = *zu;

	/* number of output reflectors */
	for (ir=0,nro=0; ir<nri; ++ir)
		nro += nui[ir]-1;

	/* make output reflectors and free space for input reflectors */
	aro = ealloc1float(nro);
	nuo = ealloc1int(nro);
	xuo = ealloc1(nro,sizeof(float*));
	zuo = ealloc1(nro,sizeof(float*));
	for (ir=0,jr=0; ir<nri; ++ir) {
		for (iu=0; iu<nui[ir]-1; ++iu,++jr) {
			aro[jr] = ari[ir];
			nuo[jr] = 2;
			xuo[jr] = ealloc1float(2);
			zuo[jr] = ealloc1float(2);
			xuo[jr][0] = xui[ir][iu];
			zuo[jr][0] = zui[ir][iu];
			xuo[jr][1] = xui[ir][iu+1];
			zuo[jr][1] = zui[ir][iu+1];
		}
		free1float(xui[ir]);
		free1float(zui[ir]);
	}
	free1float(ari);
	free1int(nui);
	free1(xui);
	free1(zui);

	/* output reflectors */
	*nr = nro;
	*ar = aro;
	*nu = nuo;
	*xu = xuo;
	*zu = zuo;
}

void makeref (float dsmax, int nr, float *ar, 
	int *nu, float **xu, float **zu, Reflector **r)
/*****************************************************************************
Make piecewise cubic reflectors
******************************************************************************
Input:
dsmax		maximum length of reflector segment
nr		number of reflectors
ar		array[nr] of reflector amplitudes
nu		array[nr] of numbers of (x,z) pairs; u = 0, 1, ..., nu[ir]
xu		array[nr][nu[ir]] of reflector x coordinates x(u)
zu		array[nr][nu[ir]] of reflector z coordinates z(u)

Output:
r		array[nr] of reflectors
******************************************************************************
Notes:
Space for the ar, nu, xu, and zu arrays is freed by this function, since
they are only used to construct the reflectors.

This function is meant to be called only once, so it need not be very
efficient.  Once made, the reflectors are likely to be used many times, 
so the cost of making them is insignificant.
*****************************************************************************/
{
	int ir,iu,nuu,iuu,ns,is;
	float x,z,xlast,zlast,dx,dz,duu,uu,ds,fs,rsx,rsz,rsxd,rszd,
		*u,*s,(*xud)[4],(*zud)[4],*us;
	ReflectorSegment *rs;
	Reflector *rr;
	
	/* allocate space for reflectors */
	*r = rr = ealloc1(nr,sizeof(Reflector));

	/* loop over reflectors */
	for (ir=0; ir<nr; ++ir) {

		/* compute cubic spline coefficients for uniformly sampled u */
		u = ealloc1float(nu[ir]);
		for (iu=0; iu<nu[ir]; ++iu)
			u[iu] = iu;
		xud = (float(*)[4])ealloc1float(4*nu[ir]);
		csplin(nu[ir],u,xu[ir],xud);
		zud = (float(*)[4])ealloc1float(4*nu[ir]);
		csplin(nu[ir],u,zu[ir],zud);

		/* finely sample x(u) and z(u) and compute length s(u) */
		nuu = 20*nu[ir];
		duu = (u[nu[ir]-1]-u[0])/(nuu-1);
		s = ealloc1float(nuu);
		s[0] = 0.0;
		xlast = xu[ir][0];
		zlast = zu[ir][0];
		for (iuu=1,uu=duu; iuu<nuu; ++iuu,uu+=duu) {
			intcub(0,nu[ir],u,xud,1,&uu,&x);
			intcub(0,nu[ir],u,zud,1,&uu,&z);
			dx = x-xlast;
			dz = z-zlast;
			s[iuu] = s[iuu-1]+sqrt(dx*dx+dz*dz);
			xlast = x;
			zlast = z;
		}

		/* compute u(s) from s(u) */
		ns = 1+s[nuu-1]/dsmax;
		ds = s[nuu-1]/ns;
		fs = 0.5*ds;
		us = ealloc1float(ns);
		yxtoxy(nuu,duu,0.0,s,ns,ds,fs,0.0,(float)(nu[ir]-1),us);

		/* compute reflector segments uniformly sampled in s */
		rs = ealloc1(ns,sizeof(ReflectorSegment));
		for (is=0; is<ns; ++is) {
			intcub(0,nu[ir],u,xud,1,&us[is],&rsx);
			intcub(0,nu[ir],u,zud,1,&us[is],&rsz);
			intcub(1,nu[ir],u,xud,1,&us[is],&rsxd);
			intcub(1,nu[ir],u,zud,1,&us[is],&rszd);
			rs[is].x = rsx;
			rs[is].z = rsz;
			rs[is].c = rsxd/sqrt(rsxd*rsxd+rszd*rszd);
			rs[is].s = -rszd/sqrt(rsxd*rsxd+rszd*rszd);
		}
		
		/* fill in reflector structure */
		rr[ir].ns = ns;
		rr[ir].ds = ds;
		rr[ir].a = ar[ir];
		rr[ir].rs = rs;

		/* free workspace */
		free1float(us);
		free1float(s);
		free1float(u);
		free1float((float*)xud);
		free1float((float*)zud);

		/* free space replaced by reflector segments */
		free1(xu[ir]);
		free1(zu[ir]);
	}

	/* free space replaced by reflector segments */
	free1(nu);
	free1(xu);
	free1(zu);
}

void raylv2 (float v00, float dvdx, float dvdz,
	float x0, float z0, float x, float z,
	float *c, float *s, float *t, float *q)
/*****************************************************************************
Trace ray between two points, for linear velocity v = v00+dvdx*x+dvdz*z
******************************************************************************
Input:
v00		velocity at (x=0,z=0)
dvdx		derivative dv/dx of velocity with respect to x
dvdz		derivative dv/dz of velocity with respect to z
x0		x coordinate at beginning of ray
z0		z coordinate at beginning of ray
x		x coordinate at end of ray
z		z coordinate at end of ray

Output:
c		cosine of propagation angle at end of ray
s		sine of propagation angle at end of ray
t		time along raypath
q		integral of velocity along raypath
*****************************************************************************/
{
	float a,oa,v0,v,cr=0.0,sr=0.0,r,or,c0,mx,temp;
	
	/* if (x,z) same as (x0,z0) */
	if (x==x0 && z==z0) {
		*c = 1.0;
		*s = 0.0;
		*t = 0.0;
		*q = 0.0;;
		return;
	}

	/* if constant velocity */
	if (dvdx==0.0 && dvdz==0.0) {
		x -= x0;
		z -= z0;
		r = sqrt(x*x+z*z);
		or = 1.0/r;
		*c = z*or;
		*s = x*or;
		*t = r/v00;
		*q = r*v00;
		return;
	}

	/* if necessary, rotate coordinates so that v(x,z) = v0+a*(z-z0) */
	if (dvdx!=0.0) {
		a = sqrt(dvdx*dvdx+dvdz*dvdz);
		oa = 1.0/a;
		cr = dvdz*oa;
		sr = dvdx*oa;
		temp = cr*x0-sr*z0;
		z0 = cr*z0+sr*x0;
		x0 = temp;
		temp = cr*x-sr*z;
		z = cr*z+sr*x;
		x = temp;
	} else {
		a = dvdz;
	}

	/* velocities at beginning and end of ray */
	v0 = v00+a*z0;
	v = v00+a*z;
	
	/* if either velocity not positive */
	if (v0<0.0 || v<0.0) {
		*c = 1.0;
		*s = 0.0;
		*t = FLT_MAX;
		*q = FLT_MAX;
		return;
	}

	/* translate (x,z) */
	x -= x0;
	z -= z0;
	
	/* if raypath is parallel to velocity gradient */
	if (x*x<0.000001*z*z) {

		/* if ray is going down */
		if (z>0.0) {
			*s = 0.0;
			*c = 1.0;
			*t = log(v/v0)/a;
			*q = 0.5*z*(v+v0);
		
		/* else, if ray is going up */
		} else {
			*s = 0.0;
			*c = -1.0;
			*t = -log(v/v0)/a;
			*q = -0.5*z*(v+v0);
		}
	
	/* else raypath is circular with finite radius */
	} else {

		/* save translated -x to avoid roundoff error below */
		mx = -x;
		
		/* translate to make center of circular raypath at (0,0) */
		z0 = v0/a;
		z += z0;
		x0 = -(x*x+z*z-z0*z0)/(2.0*x);
		x += x0;
		
		/* signed radius of raypath; if ray going clockwise, r > 0 */
		if ( (a>0.0 && mx>0.0) || (a<0.0 && mx<0.0) )
			r = sqrt(x*x+z*z);
		else
			r = -sqrt(x*x+z*z);
		
		/* cosine at (x0,z0), cosine and sine at (x,z) */
		or = 1.0/r;
		c0 = x0*or;
		*c = x*or;
		*s = -z*or;

		/* time along raypath */
		*t = log((v*(1.0+c0))/(v0*(1.0+(*c))))/a;
		if ((*t)<0.0) *t = -(*t);

		/* integral of velocity along raypath */
		*q = a*r*mx;
	}

	/* if coordinates were rotated, unrotate cosine and sine */
	if (dvdx!=0.0) {
		temp = cr*(*s)+sr*(*c);
		*c = cr*(*c)-sr*(*s);
		*s = temp;
	}
}

void addsinc (float time, float amp,
	int nt, float dt, float ft, float *trace)
/*****************************************************************************
Add sinc wavelet to trace at specified time and with specified amplitude
******************************************************************************
Input:
time		time at which to center sinc wavelet
amp		peak amplitude of sinc wavelet
nt		number of time samples
dt		time sampling interval
ft		first time sample
trace		array[nt] containing sample values

Output:
trace		array[nt] with sinc added to sample values
*****************************************************************************/
{
	static float sinc[101][8];
	static int nsinc=101,madesinc=0;
	int jsinc;
	float frac;
	int itlo,ithi,it,jt;
	float tn,*psinc;

	/* if not made sinc coefficients, make them */
	if (!madesinc) {
		for (jsinc=1; jsinc<nsinc-1; ++jsinc) {
			frac = (float)jsinc/(float)(nsinc-1);
			mksinc(frac,8,sinc[jsinc]);
		}
		for (jsinc=0; jsinc<8; ++jsinc)
			sinc[0][jsinc] = sinc[nsinc-1][jsinc] = 0.0;
		sinc[0][3] = 1.0;
		sinc[nsinc-1][4] = 1.0;
		madesinc = 1;
	}
	tn = (time-ft)/dt;
	jt = tn;
	jsinc = (tn-jt)*(nsinc-1);
	itlo = jt-3;
	ithi = jt+4;
	if (itlo>=0 && ithi<nt) {
		psinc = sinc[jsinc];
		trace[itlo] += amp*psinc[0];
		trace[itlo+1] += amp*psinc[1];
		trace[itlo+2] += amp*psinc[2];
		trace[itlo+3] += amp*psinc[3];
		trace[itlo+4] += amp*psinc[4];
		trace[itlo+5] += amp*psinc[5];
		trace[itlo+6] += amp*psinc[6];
		trace[itlo+7] += amp*psinc[7];
	} else if (ithi>=0 && itlo<nt) {
		if (itlo<0) itlo = 0;
		if (ithi>=nt) ithi = nt-1;
		psinc = sinc[jsinc]+itlo-jt+3;
		for (it=itlo; it<=ithi; ++it)
			trace[it] += amp*(*psinc++);
	}
}

void makericker (float fpeak, float dt, Wavelet **w)
/*****************************************************************************
Make Ricker wavelet
******************************************************************************
Input:
fpeak		peak frequency of wavelet
dt		time sampling interval

Output:
w		Ricker wavelet
*****************************************************************************/
{
	int iw,lw,it,jt;
	float t,x,*wv;
	
	iw = -(1+1.0/(fpeak*dt));
	lw = 1-2*iw;
	wv = ealloc1float(lw);
	for (it=iw,jt=0,t=it*dt; jt<lw; ++it,++jt,t+=dt) {
		x = PI*fpeak*t;
		x = x*x;
		wv[jt] = exp(-x)*(1.0-2.0*x);
	}
	*w = ealloc1(1,sizeof(Wavelet));
	(*w)->lw = lw;
	(*w)->iw = iw;
	(*w)->wv = wv;
}
