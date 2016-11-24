/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTTOZ: $Revision: 1.20 $ ; $Date: 2011/11/16 23:21:55 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUTTOZ - resample from time to depth					",
"									",
" suttoz <stdin >stdout [optional parms]				",
"									",
" Optional Parameters:							",
" nz=1+(nt-1)*dt*vmax/(2.0*dz)   number of depth samples in output	",
" dz=vmin*dt/2		depth sampling interval (defaults avoids aliasing)",
" fz=v(ft)*ft/2		first depth sample				",
" t=0.0,...		times corresponding to interval velocities in v",
" v=1500.0,...		interval velocities corresponding to times in v",
" vfile=		  binary (non-ascii) file containing velocities v(t)",
" verbose=0		>0 to print depth sampling information		",
"									",
" Notes:								",
" Default value of nz set to avoid aliasing				",
" The t and v arrays specify an interval velocity function of time.	",
"									",
" Note that t and v are given  as arrays of floats separated by commas,  ",
" for example:								",
" t=0.0,0.01,.2,... v=1500.0,1720.0,1833.5,... with the number of t values",
" equaling the number of v values. The velocities are linearly interpolated",
" to make a continuous, piecewise linear v(t) profile.			",
"									",
" Linear interpolation and constant extrapolation is used to determine	",
" interval velocities at times not specified.  Values specified in t	",
" must increase monotonically.						",
"									",
" Alternatively, interval velocities may be stored in a binary file	",
" containing one velocity for every time sample.  If vfile is specified,",
" then the t and v arrays are ignored.					",
"									",
" see selfdoc of suztot  for depth to time conversion			",
"									",
" Trace header fields accessed:  ns, dt, and delrt			",
" Trace header fields modified:  trid, ns, d1, and f1			",
"									",
NULL};

/* Credits:
 *	CWP: Dave Hale c. 1992
 *
 */
/**************** end self doc *******************************************/

/* functions defined and used internally */
static void maketz (int nt, float dt, float ft, float v[], 
	int nz, float dz, float fz, float t[]);
static void zttz(int nt, float dt, float ft, float zt[], float vft, float vlt, 
	int nz, float dz, float fz, float tz[]);

segy tr;

int
main(int argc, char **argv)
{
	int nt;		/* number of time samples */
	int it;		/* counter */
	int nz;		/* numer of depth samples */

	int ntpar;	/* number of getparred time values for velocities */
	int nvpar;	/* number of getparred velocity values */
	int itpar;	/* counter */

	int verbose;	/* verbose flag, =0 silent, =1 chatty */
	float dt;	/* time sampling interval */
	float ft;	/* first time value */
	float dz;	/* depth sampling interval for velocities */
	float fz;	/* first depth value */
	float t;	/* timevalues for velocities */
	float vmin;	/* minimum velocity */
	float vmax;	/* maximum velocity */
	float *tpar=NULL;	/* values of t getparred */
	float *vpar=NULL;	/* values of v getparred */
	float *vt=NULL;		/* v(t) velocity as a function of t */
	float *tz=NULL;		/* t(z) time as a function of z */
	float *temp=NULL;	/* temporary storage array */
	char *vfile="";		/* name of the velocity file */

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get time sampling from first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	ft = tr.delrt/1000.0;

	/* determine velocity function v(t) */
	vt = ealloc1float(nt);
	if (!getparstring("vfile",&vfile)) {
		ntpar = countparval("t");
		if (ntpar==0) ntpar = 1;
		tpar = ealloc1float(ntpar);
		if (!getparfloat("t",tpar)) tpar[0] = 0.0;
		nvpar = countparval("v");
		if (nvpar==0) nvpar = 1;
		if (nvpar!=ntpar)err("number of t and v values must be equal");
		vpar = ealloc1float(nvpar);
		if (!getparfloat("v",vpar)) vpar[0] = 1500.0;
		for (itpar=1; itpar<ntpar; ++itpar)
			if (tpar[itpar]<=tpar[itpar-1])
				err("tpar must increase monotonically");
		for (it=0,t=0.0; it<nt; ++it,t+=dt)
			intlin(ntpar,tpar,vpar,vpar[0],vpar[ntpar-1],
				1,&t,&vt[it]);
	} else {
		if (fread(vt,sizeof(float),nt,fopen(vfile,"r"))!=nt)
			err("cannot read %d velocities from file %s",nt,vfile);
	}

	/* determine minimum and maximum velocities */
	for (it=1,vmin=vmax=vt[0]; it<nt; ++it) {
		if (vt[it]<vmin) vmin = vt[it];
		if (vt[it]>vmax) vmax = vt[it];
	}

	/* get parameters */
	if (!getparfloat("dz",&dz)) dz = vmin*dt/2.0;
	if (!getparfloat("fz",&fz)) fz = vt[0]*ft/2.0;
	if (!getparint("nz",&nz)) nz = 1+(nt-1)*dt*vmax/(2.0*dz);
	if (!getparint("verbose",&verbose)) verbose = 0;
        checkpars();
	CHECK_NT("nz",nz);

	/* if requested, print depth sampling */
	if (verbose) {
		warn("Input:");
		warn("\tnumber of time samples = %d",nt);
		warn("\ttime sampling interval = %g",dt);
		warn("\tfirst time sample = %g",ft);
		warn("Output:\n");
		warn("\tnumber of depth samples = %d",nz);
		warn("\tdepth sampling interval = %g",dz);
		warn("\tfirst depth sample = %g",fz);
	}

	/* allocate workspace */
	tz = ealloc1float(nz);
	temp = ealloc1float(nt);

	/* make t(z) function */
	maketz(nt,dt,ft,vt,nz,dz,fz,tz);
	
	/* loop over traces */
	do {
		/* update header fields */
		tr.trid = TRID_DEPTH;
		tr.ns = nz;
		tr.d1 = dz;
		tr.f1 = fz;

		/* resample */
		memcpy((void *) temp, (const void *) tr.data,nt*sizeof(float));
		ints8r(nt,dt,ft,temp,0.0,0.0,nz,tz,tr.data);

		/* put this trace before getting another */
		puttr(&tr);

	} while(gettr(&tr));

	return(CWP_Exit());
}

static void maketz (int nt, float dt, float ft, float v[], 
	int nz, float dz, float fz, float t[])
/************************************************************************
maketz - compute t(z) from v(t)
*************************************************************************
Input:
nt	number of time samples
dt	time sampling interval
ft	first time sample
v[]	array of velocities as a function of time t
nz	number of z values (output)
dz	depth sampling interval (output)
fz	first depth value (output)
Output:
t[]	array of t values as a function of z
*************************************************************************
Author: CWP: Dave Hale (c. 1992)
*************************************************************************/
{
	int it;			/* counter */
	float vft;		/* velocity at the first time sample */
	float vlt;		/* velocity at the last time sample */
	float *z=NULL;		/* array of depth values as a function of t */

	/* allocate space */
	z = ealloc1float(nt);

	/* calculate z(t) from v(t) */
	z[0] = 0.5*ft*v[0];
	for (it=1; it<nt; it++)
		z[it] = z[it-1]+0.5*dt*v[it-1];
	vft = v[0];
	vlt = v[nt-1];

	/* compute t(z) from z(t) */
	zttz(nt,dt,ft,z,vft,vlt,nz,dz,fz,t);
	free1float(z);
}

static void zttz(int nt, float dt, float ft, float zt[], float vft, float vlt, 
	int nz, float dz, float fz, float tz[])
/************************************************************************
zttz - compute t(z) from z(t)
*************************************************************************
Input:
nt	number of time samples
dt	time sampling interval
ft	first time value
zt[]	array of z values as a function of time
vft	velocity at first time sample
vlt	velocity at last time sample
nz	number of z values  (output)
dz	depth sampling interval (output)
fz	first z value
Output:
tz[] 	array of time values as a function of depth
*************************************************************************
Author: CWP: Dave Hale (c. 1992)
************************************************************************/
{
	int iz;				/* depth counter */
	float z;			/* depth */
	float lt=ft+(nt-1)*dt;		/* last time */
	float lz=fz+(nz-1)*dz;		/* last depth */

	/* switch from z(t) to t(z) */
	yxtoxy(nt,dt,ft,zt,nz,dz,fz,0.0,0.0,tz);

	/* for z values before fz, use first velocity to calculate t(z) */
	for (iz=0,z=fz; z<=zt[0]; iz++,z+=dz)
		tz[iz] = 2.0*z/vft;

	/* for z values from lz down to fz, calculate t(z) */
	for (iz=nz-1,z=lz; z>=zt[nt-1]; iz--,z-=dz)
		tz[iz] = lt+2.0*(z-zt[nt-1])/vlt;
}
