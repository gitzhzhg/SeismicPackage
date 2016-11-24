/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUFDMOD2_PML: $Revision: 1.9 $ ; $Date: 2011/11/12 00:40:42 $	*/


#include "par.h"
#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUFDMOD2_PML - Finite-Difference MODeling (2nd order) for acoustic wave",
"    equation with PML absorbing boundary conditions.			",
" Caveat: experimental PML absorbing boundary condition version,	",
"may be buggy!								",
" 									",
" sufdmod2_pml <vfile >wfile nx= nz= tmax= xs= zs= [optional parameters]",
" 									",
" Required Parameters:							",
" <vfile		file containing velocity[nx][nz]		",
" >wfile		file containing waves[nx][nz] for time steps	",
" nx=			number of x samples (2nd dimension)		",
" nz=			number of z samples (1st dimension)		",
" xs=			x coordinates of source				",
" zs=			z coordinates of source				",
" tmax=			maximum time					",
" 									",
" Optional Parameters:							",
" nt=1+tmax/dt		number of time samples (dt determined for stability)",
" mt=1			number of time steps (dt) per output time step	",
" 									",
" dx=1.0		x sampling interval				",
" fx=0.0		first x sample					",
" dz=1.0		z sampling interval				",
" fz=0.0		first z sample					",
" 									",
" fmax = vmin/(10.0*h)	maximum frequency in source wavelet		",
" fpeak=0.5*fmax	peak frequency in ricker wavelet		",
" 									",
" dfile=		input file containing density[nx][nz]		",
" vsx=			x coordinate of vertical line of seismograms	",
" hsz=			z coordinate of horizontal line of seismograms	",
" vsfile=		output file for vertical line of seismograms[nz][nt]",
" hsfile=		output file for horizontal line of seismograms[nx][nt]",
" ssfile=		output file for source point seismograms[nt]	",
" verbose=0		=1 for diagnostic messages, =2 for more		",
" 									",
" abs=1,1,1,1		Absorbing boundary conditions on top,left,bottom,right",
" 			sides of the model. 				",
" 		=0,1,1,1 for free surface condition on the top		",
"                                                                       ",
" ...PML parameters....                                                 ",
" pml_max=1000.0        PML absorption parameter                        ",
" pml_thick=0           half-thickness of pml layer (0 = do not use PML)",
" 									",
" Notes:								",
" This program uses the traditional explicit second order differencing	",
" method. 								",
" 									",
" Two different absorbing boundary condition schemes are available. The ",
" first is a traditional absorbing boundary condition scheme created by ",
" Hale, 1990. The second is based on the perfectly matched layer (PML)	",
" method of Berenger, 1995.						",
" 									",
NULL};

/*
 * Authors:  CWP:Dave Hale
 *           CWP:modified for SU by John Stockwell, 1993.
 *           CWP:added frequency specification of wavelet: Craig Artley, 1993
 *           TAMU:added PML absorbing boundary condition: 
 *               Michael Holzrichter, 1998
 *           CWP/WesternGeco:corrected PML code to handle density variations:
 *               Greg Wimpey, 2006
 *
 * References: (Hale's absobing boundary conditions)
 * Clayton, R. W., and Engquist, B., 1977, Absorbing boundary conditions
 * for acoustic and elastic wave equations, Bull. Seism. Soc. Am., 6,
 *	1529-1540. 
 *
 * Clayton, R. W., and Engquist, B., 1980, Absorbing boundary conditions
 * for wave equation migration, Geophysics, 45, 895-904.
 *
 * Hale, D.,  1990, Adaptive absorbing boundaries for finite-difference
 * modeling of the wave equation migration, unpublished report from the
 * Center for Wave Phenomena, Colorado School of Mines.
 *
 * Richtmyer, R. D., and Morton, K. W., 1967, Difference methods for
 * initial-value problems, John Wiley & Sons, Inc, New York.
 *
 * Thomee, V., 1962, A stable difference scheme for the mixed boundary problem
 * for a hyperbolic, first-order system in two dimensions, J. Soc. Indust.
 * Appl. Math., 10, 229-245.
 *
 * Toldi, J. L., and Hale, D., 1982, Data-dependent absorbing side boundaries,
 * Stanford Exploration Project Report SEP-30, 111-121.
 *
 * References: (PML boundary conditions)
 * Jean-Pierre Berenger, ``A Perfectly Matched Layer for the Absorption of
 * Electromagnetic Waves,''  Journal of Computational Physics, vol. 114,
 * pp. 185-200.
 *
 * Hastings, Schneider, and Broschat, ``Application of the perfectly
 * matched layer (PML) absorbing boundary condition to elastic wave
 * propogation,''  Journal of the Accoustical Society of America,
 * November, 1996.
 *
 * Allen Taflove, ``Electromagnetic Modeling:  Finite Difference Time
 * Domain Methods'', Baltimore, Maryland: Johns Hopkins University Press,
 * 1995, chap. 7, pp. 181-195.
 *
 *
 * Trace header fields set: ns, delrt, tracl, tracr, offset, d1, d2,
 *                          sdepth, trid
 */
/**************** end self doc ********************************/

#define	ABS0	1
#define	ABS1	1
#define	ABS2	1
#define	ABS3	1

/* Prototypes for PML absorbing boundary conditions */
static void pml_init (int nx, int nz, float dx, 
		float dz, float dt, float **dvv, float **od, int verbose);
static void pml_absorb (int nx, float dx, int nz, float dz, float dt,
        float **dvv, float **od, float **pm, float **p, float **pp,
        int *abs);

/* PML related global variables */
float pml_max=0;
int pml_thick=0;
int pml_thickness=0;

float **cax_b, **cax_r;
float **cbx_b, **cbx_r;
float **caz_b, **caz_r;
float **cbz_b, **cbz_r;
float **dax_b, **dax_r;
float **dbx_b, **dbx_r;
float **daz_b, **daz_r;
float **dbz_b, **dbz_r;

float **ux_b,  **ux_r;
float **uz_b,  **uz_r;
float **v_b,   **v_r;
float **w_b,   **w_r;

float dvv_0, dvv_1, dvv_2, dvv_3;
float sigma, sigma_ex, sigma_ez, sigma_mx, sigma_mz;


/* Prototypes for finite differencing */
void ptsrc (float xs, float zs,
	int nx, float dx, float fx,
	int nz, float dz, float fz,
	float dt, float t, float fmax, float fpeak, float tdelay, float **s);
void exsrc (int ns, float *xs, float *zs,
	int nx, float dx, float fx,
	int nz, float dz, float fz,
	float dt, float t, float fmax, float **s);
void tstep2 (int nx, float dx, int nz, float dz, float dt,
	float **dvv, float **od, float **s,
	float **pm, float **p, float **pp, int *abs);

/* Globals for trace manipulation */
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
	float tdelay=0.;	/* time delay of source beginning */

	float vmin;		/* minimum wavespeed in vfile */
	float vmax;		/* maximum wavespeed in vfile */

	float dmin=0.0;		/* minimum density in dfile */
	float dmax=0.0;		/* maximum density in dfile */

	float tmax;		/* maximum time to compute */
	float t;		/* time */
	float *xs;		/* array of source x coordinates */
	float *zs;		/* array of source z coordinates */
	int *ixs;		/* array of source x sample locations */
	int *izs;		/* array of source z sample locations */
	float **s;		/* array of source pressure values */
	float **dvv;		/* array of velocity values from vfile */
	float **od;		/* array of density values from dfile */

	/* pressure field arrays */
	float **pm;		/* pressure field at t-dt */
	float **p;		/* pressure field at t */
	float **pp;		/* pressure field at t+dt */
	float **ptemp;		/* temp pressure array */

	/* output data arrays */
	float **ss;		/* source point seismogram array */
	float **hs;		/* seismograms from horiz receiver line */
	float **vs;		/* seismograms from vert receiver line */

	/* file names */
	char *dfile="";		/* density file name */
	char *vsfile="";	/* vert receiver seismogram line file  name */
	char *hsfile="";	/* horiz receiver seismogram line file name */
	char *ssfile="";	/* source point seismogram file name */

	/* input file pointers */
	FILE *velocityfp=stdin; /* pointer to input velocity data */
	FILE *densityfp;	/* pointer to input density data file */

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

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* get required parameters */
	/* get dimensions of model, maximum duration */
	if (!getparint("nx",&nx)) err("must specify nx!");
	if (!getparint("nz",&nz)) err("must specify nz!");
	if (!getparfloat("tmax",&tmax)) err("must specify tmax!");

	/* get source information, coordinates */
	nxs = countparval("xs");
	nzs = countparval("zs");
	if (nxs!=nzs)
		err("number of xs = %d must equal number of zs = %d",
			nxs,nzs);
	ns = nxs;

	if (ns==0) err("must specify xs and zs!");
	xs = alloc1float(ns);
	zs = alloc1float(ns);
	ixs = alloc1int(ns);
	izs = alloc1int(ns);
	getparfloat("xs",xs);
	getparfloat("zs",zs);
	
	/* Get absorbing boundary information */
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

        if (!getparfloat("pml_max",&pml_max)) pml_max = 1000.0;
        if (!getparint("pml_thick",&pml_thick)) pml_thick = 0;
        pml_thickness = 2 * pml_thick;

	/* source coordinates in samples */
	for (is=0 ; is < ns ; ++is) {
		ixs[is] = NINT( ( xs[is] - fx )/dx );
		izs[is] = NINT( ( zs[is] - fz )/dx );
	}

	/* z-coorinate of horizontal line of detectors */
	if (!getparfloat("hsz",&hsz)) hsz = 0.0;
	hs1 = NINT( (hsz - fz)/dz );

	/* x-coordinate of vertical line of detectors */
	if (!getparfloat("vsx",&vsx)) vsx = 0.0;
	vs2 = NINT((vsx - fx)/dx );
	
	if (!getparint("verbose",&verbose)) verbose = 0;

	/* Input and output file information */
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
			vmax = MAX(vmax,dvv[ix][iz]);
		}
	}
	
	/* determine mininum spatial sampling interval */
	h = MIN(ABS(dx),ABS(dz));
	
	/* determine time sampling interval to ensure stability */
	dt = h/(2.0*vmax);
	
	/* determine maximum temporal frequency to avoid dispersion */
	if (!getparfloat("fmax", &fmax))	fmax = vmin/(10.0*h);

	/* compute or set peak frequency for ricker wavelet */
	if (!getparfloat("fpeak", &fpeak))	fpeak = 0.5*fmax;

	/* determine number of time steps required to reach maximum time */
	if (nt==0) nt = 1+tmax/dt;

	/* if requested, open file and allocate space for seismograms */
	/* ... horizontal line of seismograms */
	if (*hsfile!='\0') {
		if((hseisfp=fopen(hsfile,"w"))==NULL)
			err("cannot open hsfile=%s",hsfile);
		hs = alloc2float(nt,nx);
	} else {
		hs = NULL;
	}

	/* ... vertical line of seismograms */
	if (*vsfile!='\0') {
		if((vseisfp=fopen(vsfile,"w"))==NULL)
			err("cannot open vsfile=%s",vsfile);
		vs = alloc2float(nt,nz);
	} else {
		vs = NULL;
	}

	/* ...  seismograms at the source point */
	if (*ssfile!='\0') {
		if((sseisfp=fopen(ssfile,"w"))==NULL)
			err("cannot open ssfile=%s",ssfile);
		ss = alloc2float(nt,ns);
	} else {
		ss = NULL;
	}
	
	/* if specified, read densities */
	if (*dfile!='\0') {
		if((densityfp=fopen(dfile,"r"))==NULL)
			err("cannot open dfile=%s",dfile);
		if (fread(od[0],sizeof(float),nx*nz,densityfp)!=nx*nz)
			err("error reading dfile=%s",dfile);
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
		warn("nx = %d",nx);
		warn("dx = %g",dx);
		warn("nz = %d",nz);
		warn("dz = %g",dz);
		warn("nt = %d",nt);
		warn("dt = %g",dt);
		warn("tmax = %g",tmax);
		warn("fmax = %g",fmax);
		warn("fpeak = %g",fpeak);
		warn("vmin = %g",vmin);
		warn("vmax = %g",vmax);
		warn("mt = %d",mt);
                warn("pml_max = %g",pml_max);
                warn("pml_half = %d",pml_thick);
                warn("pml_thickness = %d",pml_thickness);
		if (dmin==dmax) {
			warn("constant density");
		} else {
			warn("dfile=%s",dfile);
			warn("dmin = %g",dmin);
			warn("dmax = %g",dmax);
		}
	}


        if (pml_thick != 0)
		pml_init (nx, nz, dx, dz, dt, dvv, od, verbose);


	/* loop  ver time steps */
	for (it=0,t=0.0; it<nt; ++it,t+=dt) {
	
		/* if verbose, print time step */
		if (verbose>1) warn("it=%d  t=%g",it,t);
	
		/* update source function */
		if (ns==1)
			ptsrc(xs[0],zs[0],nx,dx,fx,nz,dz,fz,dt,t,
			      fmax,fpeak,tdelay,s);
		else
			exsrc(ns,xs,zs,nx,dx,fx,nz,dz,fz,dt,t,fmax,s);
		
		/* do one time step */
		tstep2(nx,dx,nz,dz,dt,dvv,od,s,pm,p,pp,abs);
		
		/* write waves */
		if (it%mt==0) {

			/* set selected trace header fields for all traces */
			cubetr.sx = xs[0];
			cubetr.sdepth = zs[0];
			cubetr.trid = TRID_DEPTH;
			cubetr.ns = nz ;
			cubetr.d1 = dz ;
			cubetr.d2 = dx ;

			/* account for delay in source starting time */
			cubetr.delrt = - 1000.0 * tdelay;

			tracl = 0 ;

			/* set selected trace header fields trace by trace */
			for (ix=0 ; ix < nx ; ++ix) {
				++tracl;
				++tracr;

				cubetr.offset = ix * dx - xs[0];
				cubetr.tracl = (int) tracl;
				cubetr.tracr = (int) tracr;

				for (iz=0 ; iz < nz ; ++iz) {	
					cubetr.data[iz] = pp[ix][iz];
				}
				/* output traces of data cube */
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
	float dt, float t, float fmax, float **s)
/*****************************************************************************
exsrc - update source pressure function for an extended source
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

Output:
s		array[nx][nz] of source pressure at time t+dt
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 03/01/90
******************************************************************************/
{
	int ix,iz,ixv,izv,is;
	float sigma,tbias,ascale,tscale,ts,xn,zn,
		v,xv,zv,dxdv,dzdv,xvn,zvn,amp,dv,dist,distprev;
	static float *vs,(*xsd)[4],(*zsd)[4];
	static int made=0;
	
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
	sigma = 0.25/fmax;
	tbias = 3.0*sigma;
	ascale = -exp(0.5)/sigma;
	tscale = 0.5/(sigma*sigma);
	if (t>2.0*tbias) return;
	ts = ascale*(t-tbias)*exp(-tscale*(t-tbias)*(t-tbias));
	
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
		for (ix=MAX(0,ixv-3); ix<=MIN(nx-1,ixv+3); ++ix) {
			for (iz=MAX(0,izv-3); iz<=MIN(nz-1,izv+3); ++iz) {
				xn = ix-xvn;
				zn = iz-zvn;
				s[ix][iz] += ts*amp*exp(-xn*xn-zn*zn);
			}
		}
	}
}

/* prototype of subroutine used internally */
static float ricker (float t, float fpeak);

void ptsrc (float xs, float zs,
	int nx, float dx, float fx,
	int nz, float dz, float fz,
	float dt, float t, float fmax, float fpeak, float tdelay, float **s)
/*****************************************************************************
ptsrc - update source pressure function for a point source
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
	if (t>2.0*tdelay) return;
	ts = ricker(t-tdelay,fpeak);
	
	/* let source contribute within limited distance */
	xsn = (xs-fx)/dx;
	zsn = (zs-fz)/dz;
	ixs = NINT(xsn);
	izs = NINT(zsn);
	for (ix=MAX(0,ixs-3); ix<=MIN(nx-1,ixs+3); ++ix) {
		for (iz=MAX(0,izs-3); iz<=MIN(nz-1,izs+3); ++iz) {
			xn = ix-xsn;
			zn = iz-zsn;
			s[ix][iz] = ts*exp(-xn*xn-zn*zn);
		}
	}
}

static float ricker (float t, float fpeak)
/*****************************************************************************
ricker - Compute Ricker wavelet as a function of time
******************************************************************************
Input:
t		time at which to evaluate Ricker wavelet
fpeak		peak (dominant) frequency of wavelet
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
	return exp(-xx)*(1.0-2.0*xx);
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
        if (pml_thick == 0) {
	   absorb(nx,dx,nz,dz,dt,dvv,od,pm,p,pp,abs);
        } else {
           pml_absorb(nx,dx,nz,dz,dt,dvv,od,pm,p,pp,abs);
	}
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
		warn("ASSERT FAILED: dx != dz in star2");
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
		warn("ASSERT FAILED: od !=  NULL in star3");
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
		warn("ASSERT FAILED: od !=  NULL in star4");
	if ( dz != dx ) 
		warn("ASSERT FAILED: dz !=  dx in star4");
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
/*****************************************************************************
absorb - absorbing boundary conditions 
*****************************************************************************
Input:
nx	number of samples in x direction
dx	spatial sampling interval in x direction
nz	number of samples in z direction
dz	spatial sampling interval in z direction
dt	time sampling interval
dvv	array of velocity values from vfile
od	array of density values from dfile
pm	pressure field at time t-1
p	pressure field at time t
pp	pressure field at t+dt
abs	flag indicating to absorb or not to absorb

*****************************************************************************
Notes:
This method is an improvement on the method of Clayton and Engquist, 1977
and 1980. The method is described in Hale, 1990.

*****************************************************************************
References:
Clayton, R. W., and Engquist, B., 1977, Absorbing boundary conditions
for acoustic and elastic wave equations, Bull. Seism. Soc. Am., 6, 1529-1540. 

Clayton, R. W., and Engquist, B., 1980, Absorbing boundary conditions
for wave equation migration, Geophysics, 45, 895-904.

Hale, D.,  1990, Adaptive absorbing boundaries for finite-difference
modeling of the wave equation migration, unpublished report from the
Center for Wave Phenomena, Colorado School of Mines.

Richtmyer, R. D., and Morton, K. W., 1967, Difference methods for
initial-value problems, John Wiley & Sons, Inc, New York.

Thomee, V., 1962, A stable difference scheme for the mixed boundary problem
for a hyperbolic, first-order system in two dimensions, J. Soc. Indust.
Appl. Math., 10, 229-245.

Toldi, J. L., and Hale, D., 1982, Data-dependent absorbing side boundaries,
Stanford Exploration Project Report SEP-30, 111-121.

*****************************************************************************
Author: CWP: Dave Hale 1990 
******************************************************************************/
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


static void pml_absorb (int nx, float dx, int nz, float dz, float dt,
        float **dvv, float **od, float **pm, float **p, float **pp,
        int *abs)
/**************************************************************************
  pml_absorb - uses the perfectly matched layer absorbing boundary condition.
***************************************************************************
Notes:
   The PML formulation is specialized to the acoustic case.


   Array        Size        Location of [0][0] with respect to p [0][0]
   -----   ---------------  -------------------------------------------
   ux_b    (nx+pml, pml+2)  (0, nz-1)
   uz_b         "               "
   dax_b        "               "
   dbx_b        "               "
   daz_b        "               "
   dbz_b        "               "
 
   ux_r    (pml+2, nz)      (nx-1, 0)
   uz_r         "               "
   dax_r        "               "
   dbx_r        "               "
   daz_r        "               "
   dbz_r        "               "

   v_b     (nx+pml, pml+3)  (0, nz-1.5)
   cax_b        "               "
   cbx_b        "               "

   w_b     (nx+pml, pml+2)  (-0.5, nz-1)
   caz_b        "               "
   cbz_b        "               "

   v_r     (pml+2, nz)      (nx-1, -0.5)
   cax_r        "               "
   cbx_r        "               "

   w_r     (pml+3, nz)      (nx-1.5, 0)
   caz_r        "               "
   cbz_r        "               "
***************************************************************************
References:
   Jean-Pierre Berenger, ``A Perfectly Matched Layer for the Absorption of
   Electromagnetic Waves,''  Journal of Computational Physics, vol. 114,
   pp. 185-200.

   Hastings, Schneider, and Broschat, ``Application of the perfectly
   matched layer (PML) absorbing boundary condition to elastic wave
   propogation,''  Journal of the Accoustical Society of America,
   November, 1996.

   Allen Taflove, ``Electromagnetic Modeling:  Finite Difference Time
   Domain Methods'', Baltimore, Maryland: Johns Hopkins University Press,
   1995, chap. 7, pp. 181-195.

   The PML ABC is implemented by extending the modeled region on
   the bottom and right sides and treating the modeled region as
   periodic.

   In the extended region, the differential equations of PML are
   modeled.  The extension is accomplished by using additional
   arrays which record the state in the extended regions.  The
   result is a nasty patchwork of arrays.  (It is possible to use
   the PML differential equations to model the absorbing and
   non-absorbing regions.  This greatly simplifies things at the
   expense of memory.)

   The size of the new arrays and the location of their (0,0)
   element in the coordinate space of the main p arrays are:
*************************************************************************
   Author:  TAMU: Michael Holzrichter, 1998
*************************************************************************/
{
        int ix, iz, jx, jz;

   /* Calculate v for bottom pad above and below main domain */

   for (ix=0, jz=pml_thickness+2; ix<nx; ++ix) {
      v_b[ix][ 0] = cax_b [ix][ 0] * v_b [ix][ 0] +
                      cbx_b [ix][ 0] * (ux_b [ix][   0] + uz_b [ix][   0]
                                       -((abs[2]!=0) ? p [ix][nz-2] : 0.0));

      v_b[ix][jz] = cax_b [ix][jz] * v_b [ix][jz] +
                      cbx_b [ix][jz] * (((abs[0]!=0) ? p [ix][   1] : 0.0)
                                       -ux_b [ix][jz-1] - uz_b [ix][jz-1]);
   }	

   /* Calculate v for bottom pad above and below right pad */

   for (ix=nx, jx=1, jz=pml_thickness+2; ix<nx+pml_thickness; ++ix, ++jx) {
      v_b  [ix][ 0] = cax_b [ix][ 0] * v_b [ix][ 0] +
                      cbx_b [ix][ 0] * (ux_b [ix][   0] + uz_b [ix][   0]
                                       -ux_r [jx][nz-2] - uz_r [jx][nz-2]);

      v_b  [ix][jz] = cax_b [ix][jz] * v_b [ix][jz] +
                      cbx_b [ix][jz] * (ux_r [jx][   1] + uz_r [jx][   1]
                                       -ux_b [ix][jz-1] - uz_b [ix][jz-1]);
   }


   /* Calculate v for main part of bottom pad */

   for (ix=0; ix<nx+pml_thickness; ++ix) {
      for (iz=1; iz<pml_thickness+2; ++iz) {
         v_b [ix][iz] = cax_b [ix][iz] * v_b [ix][iz] +
                        cbx_b [ix][iz] * (ux_b [ix][iz  ] + uz_b [ix][iz  ]
                                         -ux_b [ix][iz-1] - uz_b [ix][iz-1]);
      }
   }


   /* Calculate w for left edge of bottom pad */

   for (iz=0, ix=nx+pml_thickness-1; iz<pml_thickness+2; ++iz) {
      w_b [ 0][iz] = caz_b [ 0][iz] * w_b [ 0][iz] +
                     cbz_b [ 0][iz] * (ux_b [ix][iz] + uz_b [ix][iz]
                                      -ux_b [ 0][iz] - uz_b [ 0][iz]);
   }


   /* Calculate w for main part of bottom pad */

   for (ix=1; ix<nx+pml_thickness; ++ix) {
      for (iz=0; iz<pml_thickness+2; ++iz) {
         w_b [ix][iz] = caz_b [ix][iz] * w_b [ix][iz] +
                        cbz_b [ix][iz] * (ux_b [ix-1][iz] + uz_b [ix-1][iz]
                                         -ux_b [ix  ][iz] - uz_b [ix  ][iz]);
      }
   }


   /* Calculate v along top and bottom edge of right pad */

   for (ix=0, jx=nx-1, jz=pml_thickness; ix<pml_thickness+2; ++ix, ++jx) {
      if (jx == nx+pml_thickness) jx = 0;

      v_r [ix][   0] = cax_r [ix][   0] * v_r [ix][   0] +
                       cbx_r [ix][   0] * (ux_r [ix][ 0] + uz_r [ix][ 0]
                                          -ux_b [jx][jz] - uz_b [jx][jz]);

      v_r [ix][nz-1] = cax_r [ix][nz-1] * v_r [ix][nz-1] +
                       cbx_r [ix][nz-1] * (ux_b [jx][   0] + uz_b [jx][   0]
                                          -ux_r [ix][nz-2] - uz_r [ix][nz-2]);
   }


   /* Calculate v in rest of right pad */

   for (ix=0; ix<pml_thickness+2; ++ix) {
      for (iz=1; iz<nz-1; ++iz) {
         v_r [ix][iz] = cax_r [ix][iz] * v_r [ix][iz] +
                        cbx_r [ix][iz] * (ux_r [ix][iz  ] + uz_r [ix][iz  ]
                                         -ux_r [ix][iz-1] - uz_r [ix][iz-1]);
      }
   }


   /* Calculate w along left and right sides of right pad */

   for (iz=0, jx=pml_thickness+2; iz<nz; ++iz) {
      w_r [ 0][iz] = caz_r [ 0][iz] * w_r [ 0][iz] +
                     cbz_r [ 0][iz] * (((abs[3]!=0) ? p [nx-2][iz] : 0.0)
                                      -ux_r [   0][iz] - uz_r [   0][iz]);

      w_r [jx][iz] = caz_r [jx][iz] * w_r [jx][iz] +
                     cbz_r [jx][iz] * (ux_r [jx-1][iz] + uz_r [jx-1][iz]
                                      -((abs[1]!=0) ? p [1][iz] : 0.0));
   }


   /* Calculate w in main part of right pad */

   for (ix=1; ix<pml_thickness+2; ++ix) {
      for (iz=0; iz<nz; ++iz) {
         w_r [ix][iz] = caz_r [ix][iz] * w_r [ix][iz] +
                        cbz_r [ix][iz] * (ux_r [ix-1][iz] + uz_r [ix-1][iz]
                                         -ux_r [ix  ][iz] - uz_r [ix  ][iz]);
      }
   }


   /* Calculate ux and uz in bottom pad */

   for (ix=0; ix<nx+pml_thickness-1; ++ix) {
      for (iz=0; iz<pml_thickness+2; ++iz) {
         ux_b [ix][iz] = dax_b [ix][iz] * ux_b [ix  ][iz] +
                         dbx_b [ix][iz] * (w_b [ix][iz  ] - w_b [ix+1][iz]);
      }
   }

   for (ix=nx+pml_thickness-1, iz=0; iz<pml_thickness+2; ++iz) {
      ux_b [ix][iz] = dax_b [ix][iz] * ux_b [ix  ][iz] + 
                      dbx_b [ix][iz] * (w_b [ix][iz  ] - w_b [   0][iz]);
   }


   for (ix=0; ix<nx+pml_thickness; ++ix) {
      for (iz=0; iz<pml_thickness+2; ++iz) {
         uz_b [ix][iz] = daz_b [ix][iz] * uz_b [ix][iz  ] +
                         dbz_b [ix][iz] * (v_b [ix][iz+1] - v_b [ix][iz  ]);
      }
   }


   /* Calculate ux and uz in right pad */

   for (ix=0; ix<pml_thickness+2; ++ix) {
      for (iz=0; iz<nz; ++iz) {
         ux_r [ix][iz] = dax_r [ix][iz] * ux_r [ix  ][iz] +
                         dbx_r [ix][iz] * (w_r [ix  ][iz] - w_r [ix+1][iz]);
      }
   }

   for (ix=0; ix<pml_thickness+2; ++ix) {
      for (iz=0; iz<nz-1; ++iz) {
         uz_r [ix][iz] = daz_r [ix][iz] * uz_r [ix][iz  ] +
                         dbz_r [ix][iz] * (v_r [ix][iz+1] - v_r [ix][iz  ]);
      }
   }

   for (ix=0, iz=nz-1, jx=nx-1; ix<pml_thickness+2; ++ix, ++jx) {
      if (jx == nx+pml_thickness) jx = 0;

      uz_r [ix][ 0] = uz_b [jx][pml_thickness+1];
      uz_r [ix][iz] = uz_b [jx][              0];
   }


   /* Update top and bottom edge of main grid with new field values */

   for (ix=0, jz=pml_thickness+1; ix<nx; ++ix) {
      if (abs [0] != 0) pp [ix][0] = ux_b [ix][jz] + uz_b [ix][jz];
      if (abs [2] != 0) pp [ix][nz-1] = ux_b [ix][ 0] + uz_b [ix][ 0];
   }


   /* Update left and right edges of main grid with new field values */

   for (iz=1, jx=pml_thickness+1; iz<nz-1; ++iz) {
      if (abs [1] != 0) pp [0][iz] = ux_r [jx][iz] + uz_r [jx][iz];
      if (abs [3] != 0) pp [nx-1][iz] = ux_r [ 0][iz] + uz_r [ 0][iz];
   }
}


static void pml_init (int nx, int nz, float dx, float dz, float dt,
		      float **dvv, float **od, int verbose)
{
   int ix, iz;

   /* Allocate arrays for pad on right */

   cax_r = alloc2float (nz, pml_thickness+2);
   cbx_r = alloc2float (nz, pml_thickness+2);
   caz_r = alloc2float (nz, pml_thickness+3);
   cbz_r = alloc2float (nz, pml_thickness+3);
   dax_r = alloc2float (nz, pml_thickness+2);
   dbx_r = alloc2float (nz, pml_thickness+2);
   daz_r = alloc2float (nz, pml_thickness+2);
   dbz_r = alloc2float (nz, pml_thickness+2);

   ux_r  = alloc2float (nz, pml_thickness+2);
   uz_r  = alloc2float (nz, pml_thickness+2);
   v_r   = alloc2float (nz, pml_thickness+2);
   w_r   = alloc2float (nz, pml_thickness+3);


   /* Zero out arrays for pad on right */

   for (ix=0; ix<pml_thickness+2; ++ix) {
      for (iz=0; iz<nz; ++iz) {
         ux_r  [ix][iz] = uz_r  [ix][iz] = 0.0;
         v_r   [ix][iz] = w_r   [ix][iz] = 0.0;

         cax_r [ix][iz] = cbx_r [ix][iz] = 0.0;
         caz_r [ix][iz] = cbz_r [ix][iz] = 0.0;
         dax_r [ix][iz] = dbx_r [ix][iz] = 0.0;
         daz_r [ix][iz] = dbz_r [ix][iz] = 0.0;
      }
   }


   /* Zero out extra bit on right pad */

   for (ix=pml_thickness+2, iz=0; iz<nz; ++iz) {
      caz_r [ix][iz] = cbz_r [ix][iz] = 0.0;
      w_r   [ix][iz] = 0.0;
   }


   /* Allocate arrays for pad on bottom */

   cax_b = alloc2float (pml_thickness+3, nx + pml_thickness);
   cbx_b = alloc2float (pml_thickness+3, nx + pml_thickness);
   caz_b = alloc2float (pml_thickness+2, nx + pml_thickness);
   cbz_b = alloc2float (pml_thickness+2, nx + pml_thickness);
   dax_b = alloc2float (pml_thickness+2, nx + pml_thickness);
   dbx_b = alloc2float (pml_thickness+2, nx + pml_thickness);
   daz_b = alloc2float (pml_thickness+2, nx + pml_thickness);
   dbz_b = alloc2float (pml_thickness+2, nx + pml_thickness);

   ux_b  = alloc2float (pml_thickness+2, nx + pml_thickness);
   uz_b  = alloc2float (pml_thickness+2, nx + pml_thickness);
   v_b   = alloc2float (pml_thickness+3, nx + pml_thickness);
   w_b   = alloc2float (pml_thickness+2, nx + pml_thickness);
	

   /* Zero out arrays for pad on bottom */

   for (ix=0; ix<nx+pml_thickness; ++ix) {
      for (iz=0; iz<pml_thickness+2; ++iz) {
         ux_b  [ix][iz] = uz_b  [ix][iz] = 0.0;
         v_b   [ix][iz] = w_b   [ix][iz] = 0.0;

         cax_b [ix][iz] = cbx_b [ix][iz] = 0.0;
         caz_b [ix][iz] = cbz_b [ix][iz] = 0.0;
         dax_b [ix][iz] = dbx_b [ix][iz] = 0.0;
         daz_b [ix][iz] = dbz_b [ix][iz] = 0.0;
      }
   }


   /* Zero out extra bit on bottom pad */

   for (ix=0, iz=pml_thickness+2; ix<nx+pml_thickness; ++ix) {
      cax_b [ix][iz] = cbx_b [ix][iz] = 0.0;
      v_b   [ix][iz] = 0.0;
   }


   /* Initialize cax & cbx arrays */

   for (ix=0; ix<pml_thickness+2; ++ix) {
      for (iz=0; iz<nz; ++iz) {
         sigma_ez = 0.0;

         cax_r [ix][iz] = (2.0 - (sigma_ez * dt)) / (2.0 + (sigma_ez * dt));
         cbx_r [ix][iz] = (2.0 * dt / dz)         / (2.0 + (sigma_ez * dt));
      }
   }

   for (ix=0; ix<nx+pml_thickness; ++ix) {
      for (iz=0; iz<pml_thickness+3; ++iz) {
         if ((iz == 0) || (iz == pml_thickness + 2)) {
            sigma_ez = 0.0;
         } else {
            sigma_ez = pml_max * 0.5 * (1.0 - cos (2*PI*(iz-0.5)/(pml_thickness+1)));
         }

         cax_b [ix][iz] = (2.0 - (sigma_ez * dt)) / (2.0 + (sigma_ez * dt));
         cbx_b [ix][iz] = (2.0 * dt / dz)         / (2.0 + (sigma_ez * dt));
      }
   }


   /* Initialize caz & cbz arrays */

   for (ix=0; ix<pml_thickness+3; ++ix) {
      for (iz=0; iz<nz; ++iz) {
         if ((ix == 0) || (ix == pml_thickness+2)) {
            sigma_ex = 0.0;
         } else {
            sigma_ex = pml_max * 0.5 * (1.0 - cos (2*PI*(ix-0.5)/(pml_thickness+1)));
         }

         caz_r [ix][iz] = (2.0 - (sigma_ex * dt)) / (2.0 + (sigma_ex * dt));
         cbz_r [ix][iz] = (2.0 * dt / dz)         / (2.0 + (sigma_ex * dt));
      }
   }


   for (ix=0; ix<nx+pml_thickness; ++ix) {
      for (iz=0; iz<pml_thickness+2; ++iz) {
         if (ix == 0) {
            sigma_ex = pml_max * 0.5 * (1.0 - cos (2*PI*(0.5)/(pml_thickness+1)));
         } else if (ix < nx) {
            sigma_ex = 0.0;
         } else {
            sigma_ex = pml_max * 0.5 * (1.0 - cos (2*PI*(ix-nx+0.5)/(pml_thickness+1)));
         }

         caz_b [ix][iz] = (2.0 - (sigma_ex * dt)) / (2.0 + (sigma_ex * dt));
         cbz_b [ix][iz] = (2.0 * dt / dz)         / (2.0 + (sigma_ex * dt));
      }
   }


   /* Initialize right pad's dax dbx, daz, & dbz arrays */

   for (ix=0; ix<pml_thickness+2; ++ix) {
      for (iz=0; iz<nz; ++iz) {

         /* Determine sigma_mx and sigma_mz */

         if ((ix == 0) || (ix == pml_thickness+1)) {
            sigma_mx = 0.0;
         } else {
            sigma_mx = pml_max * 0.5 * (1.0 - cos (2*PI*(ix)/(pml_thickness+1)));
         }

         sigma_mz = 0.0;


         /* Determine velocity, interpolate */

	 if (od!=NULL) {
	    /*	    if (verbose) warn("initializing right pml with density");*/
	    if (ix == 0) {
	       dvv_0 = sqrt (dvv [nx-1][iz] * od [nx-1][iz]);
	    } else if (ix == pml_thickness+1) {
	       dvv_0 = sqrt (dvv [   0][iz] * od [   0][iz]);
	    } else {
	       dvv_0 = sqrt (dvv [nx-1][iz] * od [nx-1][iz]);
	       dvv_1 = sqrt (dvv [   0][iz] * od [   0][iz]);

	       dvv_0 = ((ix) * dvv_1 + (1+pml_thickness-ix)*dvv_0);
	       dvv_0 /= (pml_thickness+1);
	    }
	 }
	 else {
	    if (ix == 0) {
	       dvv_0 = sqrt (dvv [nx-1][iz]);
	    } else if (ix == pml_thickness+1) {
	       dvv_0 = sqrt (dvv [   0][iz]);
	    } else {
	       dvv_0 = sqrt (dvv [nx-1][iz]);
	       dvv_1 = sqrt (dvv [   0][iz]);

	       dvv_0 = ((ix) * dvv_1 + (1+pml_thickness-ix)*dvv_0);
	       dvv_0 /= (pml_thickness+1);
	    }
	 }

         dvv_0 = dvv_0 * dvv_0;


         dax_r [ix][iz] = (2.0 - (sigma_mx * dt)) / (2.0 + (sigma_mx * dt));
         dbx_r [ix][iz] = (2.0 * dt * dvv_0 / dx) / (2.0 + (sigma_mx * dt));

         daz_r [ix][iz] = (2.0 - (sigma_mz * dt)) / (2.0 + (sigma_mz * dt));
         dbz_r [ix][iz] = (2.0 * dt * dvv_0 / dz) / (2.0 + (sigma_mz * dt));
      }
   }


   /* Initialize bottom pad's dax, dbx, daz, & dbz arrays */

   for (ix=0; ix<nx+pml_thickness; ++ix) {
      for (iz=0; iz<pml_thickness+2; ++iz) {

         /* Determine sigma_mx and sigma_mz */

         if (ix < nx) {
            sigma_mx = 0.0;
         } else {
            sigma_mx = pml_max * 0.5 * (1.0 - cos (2*PI*(ix-nx+1)/(pml_thickness+1)));
         }

         if ((iz == 0) || (iz == pml_thickness+1)) {
            sigma_mz = 0.0;
         } else {
            sigma_mz = pml_max * 0.5 * (1.0 - cos (2*PI*(iz)/(pml_thickness+1)));
         }


         /* Determine velocity, interpolate */
	 if (od!=NULL) {
	    /*	    if (verbose) warn("initializing bottom pml with density");*/
	    if (ix < nx) {
	       if (iz == 0) {
		  dvv_0 = sqrt (dvv [ix][nz-1] * od [ix][nz-1]);
	       } else if (iz == pml_thickness+1) {
		  dvv_0 = sqrt (dvv [ix][   0] * od [ix][   0]);
	       } else {
		  dvv_0 = sqrt (dvv [ix][nz-1] * od [ix][nz-1]);
		  dvv_1 = sqrt (dvv [ix][   0] * od [ix][   0]);

		  dvv_0 = ((iz) * dvv_1 + (1+pml_thickness-iz)*dvv_0);
		  dvv_0 /= (pml_thickness+1);
	       }
	    } else {
	       if (iz == 0) {
		  dvv_0 = sqrt (dvv [nx-1][nz-1] * od [nx-1][nz-1]);
		  dvv_1 = sqrt (dvv [   0][nz-1] * od [   0][nz-1]);
	       } else if (iz == pml_thickness+1) {
		  dvv_0 = sqrt (dvv [nx-1][   0] * od [nx-1][   0]);
		  dvv_1 = sqrt (dvv [   0][   0] * od [   0][   0]);
	       } else {
		  dvv_2 = sqrt (dvv [nx-1][nz-1] * od [nx-1][nz-1]);
		  dvv_3 = sqrt (dvv [nx-1][   0] * od [nx-1][   0]);

		  dvv_0 = ((iz) * dvv_3 + (1+pml_thickness-iz)*dvv_2);
		  dvv_0 /= (pml_thickness+1);

		  dvv_2 = sqrt (dvv [0][nz-1] * od [0][nz-1]);
		  dvv_3 = sqrt (dvv [0][0] * od [0][0]);

		  dvv_1 = ((iz) * dvv_3 + (1+pml_thickness-iz)*dvv_2);
		  dvv_1 /= (pml_thickness+1);
	       }

	       dvv_0 = ((ix-nx+1) * dvv_1 + (nx+pml_thickness-ix)*dvv_0);
	       dvv_0 /= (pml_thickness+1);
	    }
	 }

	 else {
	    if (ix < nx) {
	       if (iz == 0) {
		  dvv_0 = sqrt (dvv [ix][nz-1]);
	       } else if (iz == pml_thickness+1) {
		  dvv_0 = sqrt (dvv [ix][   0]);
	       } else {
		  dvv_0 = sqrt (dvv [ix][nz-1]);
		  dvv_1 = sqrt (dvv [ix][   0]);

		  dvv_0 = ((iz) * dvv_1 + (1+pml_thickness-iz)*dvv_0);
		  dvv_0 /= (pml_thickness+1);
	       }
	    } else {
	       if (iz == 0) {
		  dvv_0 = sqrt (dvv [nx-1][nz-1]);
		  dvv_1 = sqrt (dvv [   0][nz-1]);
	       } else if (iz == pml_thickness+1) {
		  dvv_0 = sqrt (dvv [nx-1][   0]);
		  dvv_1 = sqrt (dvv [   0][   0]);
	       } else {
		  dvv_2 = sqrt (dvv [nx-1][nz-1]);
		  dvv_3 = sqrt (dvv [nx-1][   0]);

		  dvv_0 = ((iz) * dvv_3 + (1+pml_thickness-iz)*dvv_2);
		  dvv_0 /= (pml_thickness+1);

		  dvv_2 = sqrt (dvv [0][nz-1]);
		  dvv_3 = sqrt (dvv [0][0]);

		  dvv_1 = ((iz) * dvv_3 + (1+pml_thickness-iz)*dvv_2);
		  dvv_1 /= (pml_thickness+1);
	       }

	       dvv_0 = ((ix-nx+1) * dvv_1 + (nx+pml_thickness-ix)*dvv_0);
	       dvv_0 /= (pml_thickness+1);
	    }
	 }

         dvv_0 = dvv_0 * dvv_0;

         dax_b [ix][iz] = (2.0 - (sigma_mx * dt)) / (2.0 + (sigma_mx * dt));
         dbx_b [ix][iz] = (2.0 * dt * dvv_0 / dx) / (2.0 + (sigma_mx * dt));

         daz_b [ix][iz] = (2.0 - (sigma_mz * dt)) / (2.0 + (sigma_mz * dt));
         dbz_b [ix][iz] = (2.0 * dt * dvv_0 / dz) / (2.0 + (sigma_mz * dt));
      }
   }
}
