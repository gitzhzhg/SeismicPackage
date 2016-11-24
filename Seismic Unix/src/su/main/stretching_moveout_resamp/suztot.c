/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUZTOT: $Revision: 1.10 $ ; $Date: 2015/02/09 15:33:07 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUZTOT - resample from depth to time					",
"									",
" suztot <stdin >stdout [optional parms]				",
"									",
" Optional Parameters:							",
" nt=1+(nz-1)*2.0*dz/(vmax*dt)    number of time samples in output	",
" dt=2*dz/vmin		time sampling interval (defaults avoids aliasing)",
" ft=2*fz/v(fz)		first time sample				",
" z=0.0,...		depths corresponding to interval velocities in v",
" v=1500.0,...		interval velocities corresponding to depths in v",
" vfile=		binary (non-ascii) file containing velocities v(z)",
" verbose=0		>0 to print depth sampling information		",
"									",
" Notes:								",
" Default value of nt set to avoid aliasing				",
" The z and v arrays specify an interval velocity function of depth.	",
"									",
" Note that z and v are given  as arrays of floats separated by commas,  ",
" for example:								",
" z=0.0,100,200,... v=1500.0,1720.0,1833.5,... with the number of z values",
" equaling the number of v values. The velocities are linearly interpolated",
" to produce a piecewise linear v(z) profile. This fact must be taken into",
" account when attempting to use this program as the inverse of suttoz.	",
"									",
" Linear interpolation and constant extrapolation is used to determine	",
" interval velocities at times not specified.  Values specified in z	",
" must increase monotonically.						",
"									",
" Alternatively, interval velocities may be stored in a binary file	",
" containing one velocity for every time sample.  If vfile is specified,",
" then the z and v arrays are ignored.					",
"									",
" see the selfdoc of   suttoz  for time to depth conversion		",
" Trace header fields accessed:  ns, dt, and delrt			",
" Trace header fields modified:  trid, ns, d1, and f1			",
"									",
NULL};

/* Credits:
 *	CWP: John Stockwell, 2005, 
 *            based on suttoz.c written by Dave Hale c. 1992
 *
 */
/**************** end self doc *******************************************/

/* functions defined and used internally */
static void makezt (int nz, float dz, float fz, float v[], 
	int nt, float dt, float ft, float z[]);
static void tzzt(int nz, float dz, float fz, float tz[], float vfz, float vlz, 
	int nt, float dt, float ft, float zt[]);

segy tr;

int
main(int argc, char **argv)
{
	int nz;		/* numer of depth samples */
	int iz;		/* counter */
	int nt;		/* number of time samples */

	int nzpar;	/* number of getparred depth values for velocities */
	int nvpar;	/* number of getparred velocity values */
	int izpar;	/* counter */

	int verbose;	/* verbose flag, =0 silent, =1 chatty */

	float dz=0.0;	/* depth sampling interval */
	float fz=0.0;	/* first depth value */
	float dt=0.0;	/* time sampling interval for velocities */
	float ft=0.0;	/* first time value */
	float z=0.0;	/* depth values for times */
	float vmin=0.0;	/* minimum velocity */
	float vmax=0.0;	/* maximum velocity */

	float *zpar=NULL;	/* values of z getparred */
	float *vpar=NULL;	/* values of v getparred */
	float *vz=NULL;		/* v(z) velocity as a function of z */
	float *zt=NULL;		/* z(t) depth as a function of t */
	float *temp=NULL;	/* temporary storage array */
	char *vfile="";		/* name of the velocity file */

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get time sampling from first header */
	if (!gettr(&tr)) err("can't get first trace");
	nz = tr.ns;

	/* get depth sampling */
	if (!getparfloat("dz",&dz)) dz = ((float) tr.d1);

	/* determine velocity function v(t) */
	vz = ealloc1float(nz);
	if (!getparstring("vfile",&vfile)) {
		nzpar = countparval("z");
		if (nzpar==0) nzpar = 1;
		zpar = ealloc1float(nzpar);
		if (!getparfloat("z",zpar)) zpar[0] = 0.0;
		nvpar = countparval("v");
		if (nvpar==0) nvpar = 1;
		if (nvpar!=nzpar)err("number of t and v values must be equal");
		vpar = ealloc1float(nvpar);
		if (!getparfloat("v",vpar)) vpar[0] = 1500.0;
		for (izpar=1; izpar<nzpar; ++izpar)
			if (zpar[izpar]<=zpar[izpar-1])
				err("zpar must increase monotonically");
		for (iz=0,z=0.0; iz<nz; ++iz,z+=dz)
			intlin(nzpar,zpar,vpar,vpar[0],vpar[nzpar-1],
				1,&z,&vz[iz]);
	} else { /* read from a file */
		if (fread(vz,sizeof(float),nz,fopen(vfile,"r"))!=nz)
			err("cannot read %d velocities from file %s",nz,vfile);
	}

	/* determine minimum and maximum velocities */
	for (iz=1,vmin=vmax=vz[0]; iz<nz; ++iz) {
		if (vz[iz]<vmin) vmin = vz[iz];
		if (vz[iz]>vmax) vmax = vz[iz];
	}

	/* get parameters */
	if (!getparfloat("dt",&dt)) dt = 2.0*dz/vmin;
	if (!getparfloat("ft",&ft)) ft = 2.0*ft/vz[0];
	if (!getparfloat("fz",&fz)) fz = 0.0;
	if (!getparint("nt",&nt)) nt = 1+(nz-1)*dz*2.0/(dt*vmax);
	if (!getparint("verbose",&verbose)) verbose = 0;
        checkpars();
	CHECK_NT("nt",nt);

	/* if requested, print time sampling, etc */
	if (verbose) {
		warn("Input:");
		warn("\tnumber of depth samples = %d",nz);
		warn("\tdepth sampling interval = %g",dz);
		warn("\tfirst depth sample = %g",fz);
		warn("Output:");
		warn("\tnumber of time samples = %d",nt);
		warn("\ttime sampling interval = %g",dt);
		warn("\tfirst time sample = %g",ft);
	}

	/* allocate workspace */
	zt = ealloc1float(nt);
	temp = ealloc1float(nz);

	/* make z(t) function */
	makezt(nz,dz,fz,vz,nt,dt,ft,zt);
	
	/* loop over traces */
	do {
		/* update header fields */
		tr.trid = TREAL;
		tr.ns = nt;
		tr.dt = dt*1000000.0;
		tr.f1 = ft;
		tr.d1 = 0.0;

		/* resample */
		memcpy((void *) temp, (const void *) tr.data,nz*sizeof(float));
		ints8r(nz,dz,fz,temp,0.0,0.0,nt,zt,tr.data);

		/* put this trace before getting another */
		puttr(&tr);

	} while(gettr(&tr));

	return(CWP_Exit());
}

static void makezt (int nz, float dz, float fz, float v[], 
	int nt, float dt, float ft, float z[])
/************************************************************************
makezt - compute z(t) from v(z)
*************************************************************************
Input:
nz	number of z values (output)
dz	depth sampling interval (output)
fz	first depth value (output)
v[]	array of velocities as a function of time t
nt	number of time samples
dt	time sampling interval
ft	first time sample
Output:
z[]	array of z values as a function of t
*************************************************************************
Author: CWP: based on maketz by Dave Hale (c. 1992)
*************************************************************************/
{
	int iz;			/* counter */
	float vfz;		/* velocity at the first depth sample */
	float vlz;		/* velocity at the last depth sample */
	float *t=NULL;		/* array of time values as a function of z */

	/* allocate space */
	t = ealloc1float(nz);

	/* calculate t(z) from v(z) */
	t[0] = 2.0*fz/v[0];
	for (iz=1; iz<nz; ++iz)
		t[iz] = t[iz-1]+2.0*dz/v[iz-1];
	vfz = v[0];
	vlz = v[nz-1];

	/* compute z(t) from t(z) */
	tzzt(nz,dz,fz,t,vfz,vlz,nt,dt,ft,z);
	free1float(t);
}

static void tzzt(int nz, float dz, float fz, float tz[], float vfz, float vlz, 
	int nt, float dt, float ft, float zt[])
/************************************************************************
tzzt - compute z(t) from t(z)
*************************************************************************
Input:
nz	number of z values
dz	depth sampling interval
fz	first z value
tz[]	array of z values as a function of time
vfz	velocity at first time sample
vlz	velocity at last time sample
nt	number of time samples
dt	time sampling interval
ft	first time value
Output:
zt[] 	array of time values as a function of depth
*************************************************************************
Author: CWP: based on zttz by Dave Hale (c. 1992)
************************************************************************/
{
	int it;				/* depth counter */
	float t;			/* time */
	float lt=ft+(nt-1)*dt;		/* last time */

	/* switch from t(z) to z(t) */
	yxtoxy(nz,dz,fz,tz,nt,dt,ft,0.0,0.0,zt);

	/* for t values before ft, use first velocity to calculate z(t) */
	for (it=0,t=ft; t<=tz[0]; ++it,t+=dt)
		zt[it] = vfz*t/2.0;

	/* for t values from lt down to ft, calculate z(t) */
	for (it=nt-1,t=lt; t>=tz[nz-1]; it--,t-=dt)
		zt[it] = lt*vlz/2.0 + vlz*(t-tz[nz-1])/2.0;
}
