/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUFDMOD2: $Revision: 1.29 $ ; $Date: 2015/06/02 20:15:23 $        */

/* sufdmod2 - finite difference modeling by simple 2nd order explict method */

#include "par.h"
#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUFDMOD2 - Finite-Difference MODeling (2nd order) for acoustic wave equation",
" 									",
" sufdmod2 <vfile >wfile nx= nz= tmax= xs= zs= [optional parameters]	",
" 									",
" Required Parameters:							",
" <vfile		file containing velocity[nx][nz]		",
" >wfile		file containing waves[nx][nz] for time steps	",
" nx=			number of x samples (2nd dimension)		",
" nz=			number of z samples (1st dimension)		",
" xs=			x coordinates of source, or, alternatively, the name",
"			of a file that contains the x- and z-coordinates,",
"			with the number of pairs as the first record and",
"			the actual pairs of (x,z) locations following.  ",
" zs=			z coordinates of source				",
" tmax=			maximum time					",
" 									",
" Optional Parameters:							",
" sstrength=1.0		strength of source				",
" pw=0			use point or extended source geometry parameters",
" 			=1  use horizontal plane wave source 		",
" pwt=20		amp taper on ends of line src (in grid points)  ",
" mono=0		use ricker wavelet as source function 		",
" 			=1  use single frequency src (freq=2*fpeak)	",
" nt=1+tmax/dt		number of time samples (dt determined for stability)",
" mt=1			number of time steps (dt) per output time step	",
" 									",
" dx=1.0		x sampling interval				",
" fx=0.0		first x sample					",
" dz=1.0		z sampling interval				",
" fz=0.0		first z sample					",
" 									",
" fmax = vmin/(10.0*h)	maximum frequency in source 			",
" fpeak=0.5*fmax	peak frequency in ricker wavelet		",
" 									",
" dfile=		input file containing density[nx][nz]		",
" vsx=			x coordinate of vertical line of seismograms	",
" hsz=			z coordinate of horizontal line of seismograms	",
" vsfile=		output file for vertical line of seismograms[nz][nt]",
" hsfile=		output file for horizontal line of seismograms[nx][nt]",
" ssfile=		output file for source point seismograms[nt]	",
" verbose=0		=1 for diagnostic messages, =2 for more		",
" abs=1,1,1,1		absorbing boundary conditions on top,left,bottom,right",
"			sides of the model. 				",
" 			=0,1,1,1 for free surface condition on the top	",
" 									",
" Notes:								",
" 									",
" This program uses the traditional explicit second order differencing	",
" method. 								",
" 									",
NULL};

/*
 * Authors:  CWP:Dave Hale
 *           CWP:modified for SU by John Stockwell, 1993.
 *           U Houston: added plane wave and monochromatic wave 
 *                        source options.  Chris Liner, 2010
 *
 *
 * Trace header fields set: sx, gx, ns, delrt, tracl, tracr, offset, d1, d2,
 *                          sdepth, trid
 *
 * Modifications: Tony Kocurko (TK:)
 *                Memorial University in Newfoundland and Labrador
 *                - Allow user to supply the name of a file containing
 *                  shot point locations, rather than supplying them
 *                  as values to the xs= and zs= command line arguments.
 *                - Correct the calculation of izs[is].
 *
 * Technical reference:
 * Kelly, K. R., R. W. Ward, S. Treitel, and R. M. Alford (1976),
 * Synthetic Seismograms: A finite-difference approach, 
 * Geophysics, Vol. 41. No. I (February, 1976), p. 2-27.
 *
 */
/**************** end self doc ********************************/

#define	ABS0	1
#define	ABS1	1
#define	ABS2	1
#define	ABS3	1

void ptsrc (float sstrength, float xs, float zs,
	int nx, float dx, float fx,
	int nz, float dz, float fz,
	float dt, float t, float fmax, float fpeak, float tdelay, int mono, float **s);
void exsrc (int ns, float *xs, float *zs,
	int nx, float dx, float fx,
	int nz, float dz, float fz,
	float dt, float t, float fmax, int pwt, int mono, float **s);
void tstep2 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp, int *abs);
static float ricker (float t, float fpeak, int mono);

segy cubetr; 	/* data cube traces */
segy srctr;	/* source seismogram traces */
segy horiztr;	/* horizontal line seismogram traces */
segy verttr;	/* vertical line seismogram traces */

int
main(int argc, char **argv)
{
	int ix,iz,it,is;	/* counters */
	int nx,nz,nt,mt;	/* x,z,t,tsizes */

	int verbose;		/* is verbose? */
	int nxs;		/* number of source x coordinates */
	int nzs;		/* number of source y coordinates */
	int ns;			/* total number of sources ns=nxs=nxz */
	int pw;			/* plane wave source flag 0=no 1=yes */
	float ztmp=0.0;		/* tmp var to hold src depth in pw=1 case */

	int vs2;		/* depth in samples of horiz rec line */
	int hs1;		/* horiz sample of vert rec line */

	float fx;		/* first x value */
	float dx;		/* x sample interval */

	float fz;		/* first z value */
	float dz;		/* z sample interval */
	float h;		/* minumum spatial sample interval */

	float hsz;		/* z position of horiz receiver line */
	float vsx;		/* x position of vertical receiver line */

	float dt;		/* time sample interval */
	float fmax;		/* maximum temporal frequency allowable */
	float fpeak;		/* peak frequency of ricker wavelet */
	float tdelay=0.0;	/* time delay of source beginning */

	float vmin;		/* minimum wavespeed in vfile */
	float vmax;		/* maximum wavespeed in vfile */

	float dmin=0.0;		/* minimum density in dfile */
	float dmax=0.0;		/* maximum density in dfile */

	float sstrength=0.0;	/* source strength */

	float tmax;		/* maximum time to compute */
	float t;		/* time */
	float *xs=NULL;		/* array of source x coordinates */
	float *zs=NULL;		/* array of source z coordinates */
	int *ixs=NULL;		/* array of source x sample locations */
	int *izs=NULL;		/* array of source z sample locations */
	float **s=NULL;		/* array of source pressure values */
	float **dvv=NULL;	/* array of velocity values from vfile */
	float **od=NULL;	/* array of density values from dfile */


	/* pressure field arrays */
	float **pm=NULL;	/* pressure field at t-dt */
	float **p=NULL;		/* pressure field at t */
	float **pp=NULL;		/* pressure field at t+dt */
	float **ptemp=NULL;		/* temp pressure array */

	/* output data arrays */
	float **ss=NULL;	/* source point seismogram array */
	float **hs=NULL;	/* seismograms from horiz receiver line */
	float **vs=NULL;	/* seismograms from vert receiver line */

	/* file names */
	char *dfile="";		/* density file name */
	char *vsfile="";	/* vert receiver seismogram line file  name */
	char *hsfile="";	/* horiz receiver seismogram line file name */
	char *ssfile="";	/* source point seismogram file name */
	char *spfile="";	/* TK: source point file name */

	/* input file pointers */
	FILE *velocityfp=stdin; /* pointer to input velocity data */
	FILE *densityfp=NULL;	/* pointer to input density data file */
	FILE *srpntfp=NULL;	/* TK: pointer to source point location file.*/

	/* output file pointers */
	FILE *hseisfp=NULL;	/* pointer to output horiz rec line file  */
	FILE *vseisfp=NULL;	/* pointer to output vert rec line file  */	
	FILE *sseisfp=NULL;	/* pointer to source point seis output file */

	/* SEGY fields */
	long tracl=0;		/* trace number within a line */
	long tracr=0;		/* trace number within a reel */

	/* Absorbing boundary conditions related stuff*/
	int abs[4];		/* absorbing boundary cond. flags */
	int nabs;		/* number of values given */

	int pwt;		/* taper length in grid points */
	int mono;		/* single fequency src flag */

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* get required parameters */
	if (!getparint("nx",&nx)) err("must specify nx!\n");
	if (!getparint("nz",&nz)) err("must specify nz!\n");
	if (!getparfloat("tmax",&tmax)) err("must specify tmax!\n");

	/*--------------------------------------------------------------------*\
	  TK:
	    First, see if the value supplied to the xs= command line switch is
	  actually the name of a file of source point locations. If a file
	  by that name can be opened and the number of (x,z) source locations
	  read, then we will use that file for the source location information.
	    Otherwise, we will fall back to the default behaviour and read the
	  shot point location x-coordinates from the command line argument
	  itself.
	\*--------------------------------------------------------------------*/
	getparstring("xs",&spfile); /* See if the user specified a source
                                       point file name. */
	srpntfp = fopen (spfile, "r");
	if ( srpntfp != (FILE *)NULL ) {
		if ( (is = fscanf (srpntfp, "%d", &nxs)) != 1 ) {
			fclose (srpntfp);
			err("error reading nxs from spfile=%s\n", spfile);
		}
		nzs = nxs;
	} else {
		nxs = countparval("xs");
		nzs = countparval("zs");
	}
	if (nxs!=nzs)
		err("number of xs = %d must equal number of zs = %d\n",
			nxs,nzs);
	ns = nxs;

	/* plane wave src */
	if (!getparint("pw",&pw)) pw=0;
	if (pw==1) ns = nx;

	if (ns==0) err("must specify xs and zs!\n");
	xs = alloc1float(ns);
	zs = alloc1float(ns);
	ixs = alloc1int(ns);
	izs = alloc1int(ns);
	/*--------------------------------------------------------------------*\
	  TK:
	    If the user supplied the name of a file for the shot locations,
	  then the points are read from that file. Recall that the first record
	  of that file is the number of such locations and that the actual (x,z)
	  points follow int the file.
	    If the user didn't supply the name of a file for the shot locations,
	  then the coordinates are read from the xs= and zs= command line
	  arguments as usual.
	\*--------------------------------------------------------------------*/
	if ( srpntfp != (FILE *)NULL ) {
		for (it = 0; it < ns; it++) {
		if ((is = fscanf (srpntfp, "%f %f", &xs[it], &zs[it])) != 2 ) {
			fclose (srpntfp);
	      		err("error reading xs[%d], zs[%d] from spfile=%s\n",it,it,spfile);
		}
	}
		fclose (srpntfp);
	} else {
	  getparfloat("xs",xs);
	  getparfloat("zs",zs);
	}
	if (!getparfloat("sstrength",&sstrength))	sstrength =1.0;

	if (pw==1) ztmp = zs[0];
	
	nabs = countparval("abs");
	if (nabs==4) {
		getparint("abs", abs);	
	} else {
		abs[0] = ABS0;
		abs[1] = ABS1;
		abs[2] = ABS2;
		abs[3] = ABS3;

		if (!((nabs==4) || (nabs==0)) ) 
			warn("Number of abs %d, using abs=1,1,1,1",nabs);
	}
	
	/* get optional parameters */
	if (!getparint("nt",&nt)) nt = 0;
	if (!getparint("mt",&mt)) mt = 1;
	if (!getparfloat("dx",&dx)) dx = 1.0;
	if (!getparfloat("fx",&fx)) fx = 0.0;
	if (!getparfloat("dz",&dz)) dz = 1.0;
	if (!getparfloat("fz",&fz)) fz = 0.0;
	if (!getparint("pwt",&pwt)) pwt = 20;
	if (!getparint("mono",&mono)) mono = 0;

	/* source coordinates in samples */
	for (is=0 ; is < ns ; ++is) {
		ixs[is] = NINT( ( xs[is] - fx )/dx );
		izs[is] = NINT( ( zs[is] - fz )/dz ); /* TK: dz had been dx */
	}

	if (!getparfloat("hsz",&hsz)) hsz = 0.0;
	hs1 = NINT( (hsz - fz)/dz );

	if (!getparfloat("vsx",&vsx)) vsx = 0.0;
	vs2 = NINT((vsx - fx)/dx );
	
	if (!getparint("verbose",&verbose)) verbose = 0;
	getparstring("dfile",&dfile);
	getparstring("hsfile",&hsfile);
	getparstring("vsfile",&vsfile);
	getparstring("ssfile",&ssfile);
	
	/* allocate space */
	s = alloc2float(nz,nx);
	dvv = alloc2float(nz,nx);
	od = alloc2float(nz,nx);
	pm = alloc2float(nz,nx);
	p = alloc2float(nz,nx);
	pp = alloc2float(nz,nx);
	
	/* read velocities */
	fread(dvv[0],sizeof(float),nx*nz,velocityfp);
	
	/* determine minimum and maximum velocities */
	vmin = vmax = dvv[0][0];
	for (ix=0; ix<nx; ++ix) {
		for (iz=0; iz<nz; ++iz) {
			vmin = MIN(vmin,dvv[ix][iz]);
			if (verbose==1 && dvv[ix][iz]==0) {
				warn("v=0 at (ix,iz)=(%i,%i)",ix,iz);
			}
			vmax = MAX(vmax,dvv[ix][iz]);
		}
	}
	
	/* determine mininum spatial sampling interval */
	h = MIN(ABS(dx),ABS(dz));
	
	/* determine time sampling interval to ensure stability */
	dt = h/(2.0*vmax);
	warn("stable dt=%g",dt);
	
	/* determine maximum temporal frequency to avoid dispersion */
	if (!getparfloat("fmax", &fmax))	fmax = vmin/(10.0*h);

	/* compute or set peak frequency for ricker wavelet */
	if (!getparfloat("fpeak", &fpeak))	fpeak = 0.5*fmax;

	/* determine number of time steps required to reach maximum time */
	if (nt==0) nt = 1+tmax/dt;

	/* if requested, open file and allocate space for seismograms */
	if (*hsfile!='\0') {
		if((hseisfp=fopen(hsfile,"w"))==NULL)
			err("cannot open hsfile=%s\n",hsfile);
		hs = alloc2float(nt,nx);
	} else {
		hs = NULL;
	}

	if (*vsfile!='\0') {
		if((vseisfp=fopen(vsfile,"w"))==NULL)
			err("cannot open vsfile=%s\n",vsfile);
		vs = alloc2float(nt,nz);
	} else {
		vs = NULL;
	}

	if (*ssfile!='\0') {
		if((sseisfp=fopen(ssfile,"w"))==NULL)
			err("cannot open ssfile=%s\n",ssfile);
		ss = alloc2float(nt,ns);
	} else {
		ss = NULL;
	}
	
	/* if specified, read densities */
	if (*dfile!='\0') {
		if((densityfp=fopen(dfile,"r"))==NULL)
			err("cannot open dfile=%s\n",dfile);
		if (fread(od[0],sizeof(float),nx*nz,densityfp)!=nx*nz)
			err("error reading dfile=%s\n",dfile);
		fclose(densityfp);
		dmin = dmax = od[0][0];
		for (ix=0; ix<nx; ++ix) {
			for (iz=0; iz<nz; ++iz) {
				dmin = MIN(dmin,od[ix][iz]);
				dmax = MAX(dmax,od[ix][iz]);
			}
		}
	}
	
	/* if densities not specified or constant, make densities = 1 */
	if (*dfile=='\0' || dmin==dmax ) {
		for (ix=0; ix<nx; ++ix)
			for (iz=0; iz<nz; ++iz)
				od[ix][iz] = 1.0;
		dmin = dmax = 1.0;
	}
	
	/* compute density*velocity^2 and 1/density and zero time slices */	
	for (ix=0; ix<nx; ++ix) {
		for (iz=0; iz<nz; ++iz) {
			dvv[ix][iz] = od[ix][iz]*dvv[ix][iz]*dvv[ix][iz];
			od[ix][iz] = 1.0/od[ix][iz];
			pp[ix][iz] = p[ix][iz] = pm[ix][iz] = 0.0;
		}
	}
	
	/* if densities constant, free space and set NULL pointer */
	if (dmin==dmax) {
		free2float(od);
		od = NULL;
	}
	
	/* if verbose, print parameters */
	if (verbose) {
		fprintf(stderr,"nx = %d\n",nx);
		fprintf(stderr,"dx = %g\n",dx);
		fprintf(stderr,"nz = %d\n",nz);
		fprintf(stderr,"dz = %g\n",dz);
		fprintf(stderr,"nt = %d\n",nt);
		fprintf(stderr,"dt = %g\n",dt);
		fprintf(stderr,"tmax = %g\n",tmax);
		fprintf(stderr,"fmax = %g\n",fmax);
		fprintf(stderr,"fpeak = %g\n",fpeak);
		fprintf(stderr,"vmin = %g\n",vmin);
		fprintf(stderr,"vmax = %g\n",vmax);
		fprintf(stderr,"mt = %d\n",mt);
		if (dmin==dmax) {
			fprintf(stderr,"constant density\n");
		} else {
			fprintf(stderr,"dfile=%s\n",dfile);
			fprintf(stderr,"dmin = %g\n",dmin);
			fprintf(stderr,"dmax = %g\n",dmax);
		}
	}

	/* if plane wave src */
	if (pw==1) {
		for (ix=0; ix<nx; ++ix) {
			xs[ix]  = ix*dx;
			ixs[ix] = ix;
			zs[ix]  = ztmp;
			izs[ix] = ztmp/dz;
		}
	}
	

	/* loop over time steps */
	for (it=0,t=0.0; it<nt; ++it,t+=dt) {
	
		/* if verbose, print time step */
		if (verbose>1) fprintf(stderr,"it=%d  t=%g\n",it,t);
	
		/* update source function */
		if (ns==1)
			ptsrc(sstrength,xs[0],zs[0],nx,dx,fx,nz,dz,fz,dt,t,
			      fmax,fpeak,tdelay,mono,s);
		else
			exsrc(ns,xs,zs,nx,dx,fx,nz,dz,fz,dt,t,fmax,pwt,mono,s);
		
		/* do one time step */
		tstep2(nx,dx,nz,dz,dt,dvv,od,s,pm,p,pp,abs);
		
		/* write waves */
	/* if (it%mt==0) fwrite(pp[0],sizeof(float),nx*nz,stdout); */
		if (it%mt==0) {

			cubetr.sx = xs[0];
			cubetr.sdepth = zs[0];
			cubetr.trid = 30 ;
			cubetr.ns = nz ;
			cubetr.d1 = dz ;
			cubetr.d2 = dx ;
			/* account for delay in source starting time */
			cubetr.delrt = - 1000.0 * tdelay;

			tracl = 0 ;

			for (ix=0 ; ix < nx ; ++ix) {
				++tracl;
				++tracr;
				

				cubetr.offset = ix * dx - xs[0];
				cubetr.gx = ix * dx ;
				cubetr.tracl = (int) tracl;
				cubetr.tracr = (int) tracr;

				for (iz=0 ; iz < nz ; ++iz) {	
					cubetr.data[iz] = pp[ix][iz];
				}
				fputtr(stdout, &cubetr);
			}
		}

		/* if requested, save horizontal line of seismograms */
		if (hs!=NULL) {
			for (ix=0; ix<nx; ++ix)
				hs[ix][it] = pp[ix][hs1];
		}

		/* if requested, save vertical line of seismograms */
		if (vs!=NULL) {
			for (iz=0; iz<nz; ++iz)
				vs[iz][it] = pp[vs2][iz];
		}

                /* if requested, save seismograms at source locations */
                if (ss!=NULL) {
                        for (is=0; is<ns; ++is)
                                ss[is][it] = pp[ixs[is]][izs[is]];
                }

		/* roll time slice pointers */
		ptemp = pm;
		pm = p;
		p = pp;
		pp = ptemp;
	}

	/* if requested, write horizontal line of seismograms */
	if (hs!=NULL) {

		horiztr.sx = xs[0];
		horiztr.sdepth = zs[0];
		horiztr.trid = 1;
		horiztr.ns = nt ;
		horiztr.dt = 1000000 * dt ;
		horiztr.d2 = dx ;

		/* account for delay in source starting time */
		horiztr.delrt = -1000.0 * tdelay ; 

		tracl = tracr = 0;

		for (ix=0 ; ix < nx ; ++ix){
			++tracl;
			++tracr;

			/* offset from first source location */
			horiztr.offset = ix * dx - xs[0];
			horiztr.gx = ix * dx;

			horiztr.tracl = (int) tracl;
			horiztr.tracr = (int) tracr;

			for (it = 0 ; it < nt ; ++it){
				horiztr.data[it] = hs[ix][it];
			}
			
			fputtr(hseisfp , &horiztr);
		}

			
		fclose(hseisfp);
	}

	/* if requested, write vertical line of seismograms */
	if (vs!=NULL) {

		verttr.trid = 1;
		verttr.ns = nt ;
		verttr.sx = xs[0];
		verttr.sdepth = zs[0];
		verttr.dt = 1000000 * dt ;
		verttr.d2 = dx ;
		/* account for delay source starting time */
		verttr.delrt = -1000.0 * tdelay ;

		tracl = tracr = 0;
		for (iz=0 ; iz < nz ; ++iz){
			++tracl;
			++tracr;

			/* vertical line implies offset in z */
			verttr.offset = iz * dz - zs[0];

			verttr.tracl = (int) tracl;
			verttr.tracr = (int) tracr;

			for (it = 0 ; it < nt ; ++it){
				verttr.data[it] = vs[iz][it];
			}
			
			fputtr(vseisfp , &verttr);
		}

		fclose(vseisfp);
	}

	/* if requested, write seismogram at source position */
	if (ss!=NULL) {

		srctr.trid = 1;
		srctr.ns = nt ;
		srctr.dt = 1000000 * dt ;
		srctr.d2 = dx ;
		srctr.delrt = -1000.0 * tdelay ;

		tracl = tracr = 0;
		for (is=0 ; is < ns ; ++is){
			++tracl;
			++tracr;

			srctr.sx = xs[is];
			srctr.sdepth = zs[is];
			srctr.tracl = (int) tracl;
			srctr.tracr = (int) tracr;

			for (it = 0 ; it < nt ; ++it){
				srctr.data[it] = ss[is][it];
			}
			
			fputtr(sseisfp , &srctr);
		}

		fclose(sseisfp);
	}

	
	/* free space before returning */
	free2float(s);
	free2float(dvv);
	free2float(pm);
	free2float(p);
	free2float(pp);
	
	if (od!=NULL) free2float(od);
	if (hs!=NULL) free2float(hs);
	if (vs!=NULL) free2float(vs);
	if (ss!=NULL) free2float(ss);
	
	return(CWP_Exit());
}


void exsrc (int ns, float *xs, float *zs,
	int nx, float dx, float fx,
	int nz, float dz, float fz,
	float dt, float t, float fmax, int pwt, int mono, float **s)
/*****************************************************************************
update source pressure function for an extended source
******************************************************************************
Input:
ns		number of x,z coordinates for extended source
xs		array[ns] of x coordinates of extended source
zs		array[ns] of z coordinates of extended source
nx		number of x samples
dx		x sampling interval
fx		first x sample
nz		number of z samples
dz		z sampling interval
fz		first z sample
dt		time step (ignored)
t		time at which to compute source function
fmax		maximum frequency
pwt		src taper in grid points
mono		=0 use ricker src... =1 use monofreq src (2*fpeak)

Output:
s		array[nx][nz] of source pressure at time t+dt
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 03/01/90
******************************************************************************/
{
	int ix,iz,ixv,izv,is;
	float ts,xn,zn,v,xv,zv,dxdv,dzdv,xvn,zvn;
	float amp,dv,dist,distprev;
	static float *vs,(*xsd)[4],(*zsd)[4];
	static int made=0;
	float a, pio2, opwt;
	float fpeak, tdelay;
	
	/* if not already made, make spline coefficients */
	if (!made) {
		vs = alloc1float(ns);
		xsd = (float(*)[4])alloc1float(ns*4);
		zsd = (float(*)[4])alloc1float(ns*4);
		for (is=0; is<ns; ++is)
			vs[is] = is;
		cmonot(ns,vs,xs,xsd);
		cmonot(ns,vs,zs,zsd);
		made = 1;
	}
	
	/* zero source array */
	for (ix=0; ix<nx; ++ix)
		for (iz=0; iz<nz; ++iz)
			s[ix][iz] = 0.0 *dt ;
	
	/* compute time-dependent part of source function */
	if (mono==0) {
		/* ricker src */
		fpeak = 0.5*fmax;
	} else {
		/* mono src */
		fpeak = fmax;
	}
	tdelay = 1.0/fpeak;
	if (t>2.0*tdelay && mono==0) return;
	ts = ricker(t-tdelay,fpeak,mono);
	
	/* loop over extended source locations */
	for (v=vs[0],distprev=0.0,dv=1.0; dv!=0.0; distprev=dist,v+=dv) {
		
		/* determine x(v), z(v), dx/dv, and dz/dv along source */
		intcub(0,ns,vs,xsd,1,&v,&xv);
		intcub(0,ns,vs,zsd,1,&v,&zv);
		intcub(1,ns,vs,xsd,1,&v,&dxdv);
		intcub(1,ns,vs,zsd,1,&v,&dzdv);
		
		/* determine increment along extended source */
		if (dxdv==0.0)
			dv = dz/ABS(dzdv);
		else if (dzdv==0.0)
			dv = dx/ABS(dxdv);
		else
			dv = MIN(dz/ABS(dzdv),dx/ABS(dxdv));
		if (v+dv>vs[ns-1]) dv = vs[ns-1]-v;
		dist = dv*sqrt(dzdv*dzdv+dxdv*dxdv)/sqrt(dx*dx+dz*dz);
		
		/* determine source amplitude */
		amp = (dist+distprev)/2.0;
		
		/* let source contribute within limited distance */
		xvn = (xv-fx)/dx;
		zvn = (zv-fz)/dz;
		ixv = NINT(xvn); 
		izv = NINT(zvn);
		pio2 = 3.1415926/2.0;
		opwt = 1.0 / (float) pwt;
		for (ix=0; ix<nx; ++ix) {
			for (iz=MAX(0,izv-3); iz<=MIN(nz-1,izv+3); ++iz) {
				xn = ix-xvn;
				zn = iz-zvn;
				a = 1.0;
				if (ix < pwt) {
				  a = sin( pio2*((float) ix * opwt ));
				} 
				if (ix > nx-pwt) {
				  a = cos((ix - (nx-pwt))*pio2*opwt); 
				}
				s[ix][iz] += a*ts*amp*exp(-xn*xn-zn*zn);
			}
		}
	}
}

void ptsrc (float sstrength, float xs, float zs,
	int nx, float dx, float fx,
	int nz, float dz, float fz,
	float dt, float t, float fmax, float fpeak, float tdelay, int mono, float **s)
/*****************************************************************************
update source pressure function for a point source
******************************************************************************
Input:
xs		x coordinate of point source
zs		z coordinate of point source
nx		number of x samples
dx		x sampling interval
fx		first x sample
nz		number of z samples
dz		z sampling interval
fz		first z sample
dt		time step (ignored)
t		time at which to compute source function
fmax		maximum frequency (ignored)
fpeak		peak frequency

Output:
tdelay		time delay of beginning of source function
s		array[nx][nz] of source pressure at time t+dt
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 03/01/90
******************************************************************************/
{
	int ix,iz,ixs,izs;
	float ts,xn,zn,xsn,zsn;
	
	/* zero source array */
	for (ix=0; ix<nx; ++ix)
		for (iz=0; iz<nz; ++iz)
			s[ix][iz] = 0.0 * dt*fmax;
	
	/* compute time-dependent part of source function */
	/* fpeak = 0.5*fmax;  this is now getparred */
	tdelay = 1.0/fpeak;
	if (t>2.0*tdelay && mono==0) return;
	ts = ricker(t-tdelay,fpeak,mono);
	
	/* let source contribute within limited distance */
	xsn = (xs-fx)/dx;
	zsn = (zs-fz)/dz;
	ixs = NINT(xsn);
	izs = NINT(zsn);
	for (ix=MAX(0,ixs-3); ix<=MIN(nx-1,ixs+3); ++ix) {
		for (iz=MAX(0,izs-3); iz<=MIN(nz-1,izs+3); ++iz) {
			xn = ix-xsn;
			zn = iz-zsn;
			s[ix][iz] = sstrength*ts*exp(-xn*xn-zn*zn);
		}
	}
}

static float ricker (float t, float fpeak, int mono)
/*****************************************************************************
Compute Ricker wavelet as a function of time
******************************************************************************
Input:
t		time at which to evaluate Ricker wavelet
fpeak		peak (dominant) frequency of wavelet
mono		=0 use ricker... =1 use single frequency (2*fpeak)  CLL 11/27/06
******************************************************************************
Notes:
The amplitude of the Ricker wavelet at a frequency of 2.5*fpeak is 
approximately 4 percent of that at the dominant frequency fpeak.
The Ricker wavelet effectively begins at time t = -1.0/fpeak.  Therefore,
for practical purposes, a causal wavelet may be obtained by a time delay
of 1.0/fpeak.
The Ricker wavelet has the shape of the second derivative of a Gaussian.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 04/29/90
******************************************************************************/
{
	float x,xx;
	
	x = PI*fpeak*t;
	xx = x*x;
	/* return (-6.0+24.0*xx-8.0*xx*xx)*exp(-xx); */
	/* return PI*fpeak*(4.0*xx*x-6.0*x)*exp(-xx); */
	/* return exp(-xx)*(1.0-2.0*xx); */
	if (mono==0) {
		return exp(-xx)*(1.0-2.0*xx);
	} else {
		return sin(4*x);
	}
}

/* 2D finite differencing subroutine */

/* functions declared and used internally */
static void star1 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp);
static void star2 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp);
static void star3 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp);
static void star4 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp);
static void absorb (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **pm, float **p, float **pp,
	int *abs);

void tstep2 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp, int *abs)
/*****************************************************************************
One time step of FD solution (2nd order in space) to acoustic wave equation
******************************************************************************
Input:
nx		number of x samples
dx		x sampling interval
nz		number of z samples
dz		z sampling interval
dt		time step
dvv		array[nx][nz] of density*velocity^2
od		array[nx][nz] of 1/density (NULL for constant density=1.0)
s		array[nx][nz] of source pressure at time t+dt
pm		array[nx][nz] of pressure at time t-dt
p		array[nx][nz] of pressure at time t

Output:
pp		array[nx][nz] of pressure at time t+dt
******************************************************************************
Notes:
This function is optimized for special cases of constant density=1 and/or
equal spatial sampling intervals dx=dz.  The slowest case is variable
density and dx!=dz.  The fastest case is density=1.0 (od==NULL) and dx==dz.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 03/13/90
******************************************************************************/
{
	/* convolve with finite-difference star (special cases for speed) */
	if (od!=NULL && dx!=dz) {
		star1(nx,dx,nz,dz,dt,dvv,od,s,pm,p,pp);
	} else if (od!=NULL && dx==dz) {
		star2(nx,dx,nz,dz,dt,dvv,od,s,pm,p,pp);
	} else if (od==NULL && dx!=dz) {
		star3(nx,dx,nz,dz,dt,dvv,od,s,pm,p,pp);
	} else {
		star4(nx,dx,nz,dz,dt,dvv,od,s,pm,p,pp);
	}
	
	/* absorb along boundaries */
	absorb(nx,dx,nz,dz,dt,dvv,od,pm,p,pp,abs);
}

/* convolve with finite-difference star for variable density and dx!=dz */
static void star1 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp)
{
	int ix,iz;
	float xscale1,zscale1,xscale2,zscale2;
		
	/* determine constants */
	xscale1 = (dt*dt)/(dx*dx);
	zscale1 = (dt*dt)/(dz*dz);
	xscale2 = 0.25*xscale1;
	zscale2 = 0.25*zscale1;
	
	/* do the finite-difference star */
	for (ix=1; ix<nx-1; ++ix) {
		for (iz=1; iz<nz-1; ++iz) {
			pp[ix][iz] = 2.0*p[ix][iz]-pm[ix][iz] +
				dvv[ix][iz]*(
					od[ix][iz]*(
						xscale1*(
							p[ix+1][iz]+
							p[ix-1][iz]-
							2.0*p[ix][iz]
						) +
						zscale1*(
							p[ix][iz+1]+
							p[ix][iz-1]-
							2.0*p[ix][iz]
						)
					) +
					(
						xscale2*(
							(od[ix+1][iz]-
							od[ix-1][iz]) *
							(p[ix+1][iz]-
							p[ix-1][iz])
						) +
						zscale2*(
							(od[ix][iz+1]-
							od[ix][iz-1])*
							(p[ix][iz+1]-
							p[ix][iz-1])
						)
					)
				) +
				s[ix][iz];
		}
	}
}

/* convolve with finite-difference star for variable density and dx==dz */
static void star2 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp)
{
	int ix,iz;
	float scale1,scale2;
	
	if ( dx != dz ) 
		fprintf(stderr,"ASSERT FAILED: dx != dz in star2\n");
	/* determine constants */
	scale1 = (dt*dt)/(dx*dx);
	scale2 = 0.25*scale1;
	
	/* do the finite-difference star */
	for (ix=1; ix<nx-1; ++ix) {
		for (iz=1; iz<nz-1; ++iz) {
			pp[ix][iz] = 2.0*p[ix][iz]-pm[ix][iz] +
				dvv[ix][iz]*(
					od[ix][iz]*(
						scale1*(
							p[ix+1][iz]+
							p[ix-1][iz]+
							p[ix][iz+1]+
							p[ix][iz-1]-
							4.0*p[ix][iz]
						)
					) +
					(
						scale2*(
							(od[ix+1][iz]-
							od[ix-1][iz]) *
							(p[ix+1][iz]-
							p[ix-1][iz]) +
							(od[ix][iz+1]-
							od[ix][iz-1]) *
							(p[ix][iz+1]-
							p[ix][iz-1])
						)
					)
				) +
				s[ix][iz];
		}
	}
}

/* convolve with finite-difference star for density==1.0 and dx!=dz */
static void star3 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp)
{
	int ix,iz;
	float xscale,zscale;
		
	if ( od != ((float **) NULL) ) 
		fprintf(stderr,"ASSERT FAILED: od !=  NULL in star3\n");
	/* determine constants */
	xscale = (dt*dt)/(dx*dx);
	zscale = (dt*dt)/(dz*dz);
	
	/* do the finite-difference star */
	for (ix=1; ix<nx-1; ++ix) {
		for (iz=1; iz<nz-1; ++iz) {
			pp[ix][iz] = 2.0*p[ix][iz]-pm[ix][iz] +
				dvv[ix][iz]*(
					xscale*(
						p[ix+1][iz]+
						p[ix-1][iz]-
						2.0*p[ix][iz]
					) +
					zscale*(
						p[ix][iz+1]+
						p[ix][iz-1]-
						2.0*p[ix][iz]
					)
				) +
				s[ix][iz];
		}
	}
}

/* convolve with finite-difference star for density==1.0 and dx==dz */
static void star4 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp)
{
	int ix,iz;
	float scale;
	
	/* determine constants */
	if ( od != ((float **) NULL) ) 
		fprintf(stderr,"ASSERT FAILED: od !=  NULL in star4\n");
	if ( dz != dx ) 
		fprintf(stderr,"ASSERT FAILED: dz !=  dx in star4\n");
	scale = (dt*dt)/(dx*dz);
	
	/* do the finite-difference star */
	for (ix=1; ix<nx-1; ++ix) {
		for (iz=1; iz<nz-1; ++iz) {
			pp[ix][iz] = 2.0*p[ix][iz]-pm[ix][iz] +
				scale*dvv[ix][iz]*(
					p[ix+1][iz]+
					p[ix-1][iz]+
					p[ix][iz+1]+
					p[ix][iz-1]-
					4.0*p[ix][iz]
				) +
				s[ix][iz];
		}
	}
}

static void absorb (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **pm, float **p, float **pp,
	int *abs)
{
	int ix,iz;
	float ov,ovs,cosa,beta,gamma,dpdx,dpdz,dpdt,dpdxs,dpdzs,dpdts;

	/* solve for upper boundary */
	iz = 1;
	for (ix=0; ix<nx; ++ix) {

		if (abs[0]!=0) {

			if (od!=NULL)
				ovs = 1.0/(od[ix][iz]*dvv[ix][iz]);
			else
				ovs = 1.0/dvv[ix][iz];
			ov = sqrt(ovs);
			if (ix==0)
				dpdx = (p[1][iz]-p[0][iz])/dx;
			else if (ix==nx-1)
				dpdx = (p[nx-1][iz]-p[nx-2][iz])/dx;
			else
				dpdx = (p[ix+1][iz]-p[ix-1][iz])/(2.0*dx);
			dpdt = (pp[ix][iz]-pm[ix][iz])/(2.0*dt);
			dpdxs = dpdx*dpdx;
			dpdts = dpdt*dpdt;
			if (ovs*dpdts>dpdxs)
				cosa = sqrt(1.0-dpdxs/(ovs*dpdts));
			else 
				cosa = 0.0;
			beta = ov*dz/dt*cosa;
			gamma = (1.0-beta)/(1.0+beta);

			pp[ix][iz-1] = gamma*(pp[ix][iz]-p[ix][iz-1])+p[ix][iz];
		} else {
			pp[ix][iz-1] = 0.0;
		}
	}


	/* extrapolate along left boundary */
	ix = 1;
	for (iz=0; iz<nz; ++iz) {
		if (abs[1]!=0) {
			if (od!=NULL)
				ovs = 1.0/(od[ix][iz]*dvv[ix][iz]);
			else
				ovs = 1.0/dvv[ix][iz];
			ov = sqrt(ovs);
			if (iz==0)
				dpdz = (p[ix][1]-p[ix][0])/dz;
			else if (iz==nz-1)
				dpdz = (p[ix][nz-1]-p[ix][nz-2])/dz;
			else
				dpdz = (p[ix][iz+1]-p[ix][iz-1])/(2.0*dz);
			dpdt = (pp[ix][iz]-pm[ix][iz])/(2.0*dt);
			dpdzs = dpdz*dpdz;
			dpdts = dpdt*dpdt;
			if (ovs*dpdts>dpdzs)
				cosa = sqrt(1.0-dpdzs/(ovs*dpdts));
			else
				cosa = 0.0;
			beta = ov*dx/dt*cosa;
			gamma = (1.0-beta)/(1.0+beta);
			pp[ix-1][iz] = gamma*(pp[ix][iz]-p[ix-1][iz])+p[ix][iz];
		} else {
			pp[ix-1][iz] = 0.0;
		}
	}

	/* extrapolate along lower boundary */
	iz = nz-2;
	for (ix=0; ix<nx; ++ix) {
		if (abs[2]!=0) {
			if (od!=NULL)
				ovs = 1.0/(od[ix][iz]*dvv[ix][iz]);
			else
				ovs = 1.0/dvv[ix][iz];
			ov = sqrt(ovs);
			if (ix==0)
				dpdx = (p[1][iz]-p[0][iz])/dx;
			else if (ix==nx-1)
				dpdx = (p[nx-1][iz]-p[nx-2][iz])/dx;
			else
				dpdx = (p[ix+1][iz]-p[ix-1][iz])/(2.0*dx);
			dpdt = (pp[ix][iz]-pm[ix][iz])/(2.0*dt);
			dpdxs = dpdx*dpdx;
			dpdts = dpdt*dpdt;
			if (ovs*dpdts>dpdxs)
				cosa = sqrt(1.0-dpdxs/(ovs*dpdts));
			else
				cosa = 0.0;
			beta = ov*dz/dt*cosa;
			gamma = (1.0-beta)/(1.0+beta);

			pp[ix][iz+1] = gamma*(pp[ix][iz]-p[ix][iz+1])+p[ix][iz];
		} else {
			pp[ix][iz+1] = 0.0;
		}
	}

	/* extrapolate along right boundary */
	ix = nx-2;
	for (iz=0; iz<nz; ++iz) {
		if (abs[3]!=0) {
			if (od!=NULL)
				ovs = 1.0/(od[ix][iz]*dvv[ix][iz]);
			else
				ovs = 1.0/dvv[ix][iz];
			ov = sqrt(ovs);
			if (iz==0)
				dpdz = (p[ix][1]-p[ix][0])/dz;
			else if (iz==nz-1)
				dpdz = (p[ix][nz-1]-p[ix][nz-2])/dz;
			else
				dpdz = (p[ix][iz+1]-p[ix][iz-1])/(2.0*dz);
			dpdt = (pp[ix][iz]-pm[ix][iz])/(2.0*dt);
			dpdzs = dpdz*dpdz;
			dpdts = dpdt*dpdt;
			if (ovs*dpdts>dpdzs)
				cosa = sqrt(1.0-dpdzs/(ovs*dpdts));
			else
				cosa = 0.0;
			beta = ov*dx/dt*cosa;
			gamma = (1.0-beta)/(1.0+beta);
			pp[ix+1][iz] =gamma*(pp[ix][iz]-p[ix+1][iz])+p[ix][iz];
		} else {
			pp[ix+1][iz] = 0.0;
		}
	}
}
