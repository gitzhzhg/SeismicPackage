/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUFCTANISMOD: $Revision: 1.6 $ ; $Date: 2015/06/02 20:15:23 $	*/

#include "par.h"
#include "su.h"
#include "segy.h"

/************************** self documentation *************************/
char *sdoc[] = {
"									",
" SUFCTANISMOD - Flux-Corrected Transport correction applied to the 2D",
"	  elastic wave equation for finite difference modeling in 	",
"	  anisotropic media						",
"									",
" sufctanismod > outfile [optional parameters]				",
"		outfile is the final wavefield snapshot x-component	",
"		x-component of wavefield snapshot is in snapshotx.data	",
"		y-component of wavefield snapshot is in snapshoty.data	",
"		z-component of wavefield snapshot is in snapshotz.data	",
" 									",
" Optional Output Files:						",
" reflxfile=	reflection seismogram file name for x-component		",
"		no output produced if no name specified	 		",
" reflyfile=	reflection seismogram file name for y-component		",
"		no output produced if no name specified	 		",
" reflzfile=	reflection seismogram file name for z-component		",
"		no output produced if no name specified	 		",
" vspxfile=	VSP seismogram file name for x-component		",
"		no output produced if no name specified	 		",
" vspyfile=	VSP seismogram file name for y-component		",
"		no output produced if no name specified	 		",
" vspzfile=	VSP seismogram file name for z-component		",
"		no output produced if no name specified	 		",
"									",
" suhead=1      To get SU-header output seismograms (else suhead=0)	",
"									",
" New parameter:							",
"	               							",     
" Optional Parameters:							",
" mt=1          number of time steps per output snapshot  		",
" dofct=1 	1 do the FCT correction					",
"		0 do not do the FCT correction 				",
" FCT Related parameters:						",
" eta0=0.03	diffusion coefficient					",
"		typical values ranging from 0.008 to 0.06		",
"		about 0.03 for the second-order method 			",
"		about 0.012 for the fourth-order method 		",
" eta=0.04	anti-diffusion coefficient 				",
"		typical values ranging from 0.008 to 0.06		",
"		about 0.04 for the second-order method  		",
"		about 0.015 for the fourth-order method 		",
" fctxbeg=0 	x coordinate to begin applying the FCT correction	",
" fctzbeg=0 	z coordinate to begin applying the FCT correction	",
" fctxend=nx 	x coordinate to stop applying the FCT correction	",
" fctzend=nz 	z coordinate to stop applying the FCT correction	",
" 									",
" deta0dx=0.0	gradient of eta0 in x-direction  d(eta0)/dx		",
" deta0dz=0.0	gradient of eta0 in z-direction  d(eta0)/dz		",
" detadx=0.0	gradient of eta in x-direction 	 d(eta)/dx		",
" detadz=0.0	gradient of eta in z-direction 	 d(eta)/dz		",
"									",
" General Parameters:							",
" order=2	2 second-order finite-difference 			",
"		4 fourth-order finite-difference 			",
"									",
" nt=200        number of time steps 			 		",
" dt=0.004	time step  						",
"									",
" nx=100 	number of grid points in x-direction 			",
" nz=100 	number of grid points in z-direction 			",
"									",
" dx=0.02	spatial step in x-direction 				",
" dz=0.02	spatial step in z-direction 				",
"									",
" sx=nx/2	source x-coordinate (in gridpoints)			",
" sz=nz/2	source z-coordinate (in gridpoints)			",
"									",
" fpeak=20	peak frequency of the wavelet 				",
"									",
" receiverdepth=sz  depth of horizontal receivers (in gridpoints)      ",
" vspnx=sx			x grid loc of vsp				",
"									",
" verbose=0     silent operation							",
"				=1 for diagnostic messages, =2 for more		",
"									",
" wavelet=1	1 AKB wavelet						",
" 		2 Ricker wavelet 					",
"		3 impulse 						",
"		4 unity 						",
"									",
" isurf=2	1 absorbing surface condition 				",
"		2 free surface condition 				",
"		3 zero surface condition 				",
"									",
" source=1	1 point source 						",
" 		2 sources are located on a given refelector 	        ", 
"			(two horizontal and one dipping reflectors) 	",
" 		3 sources are located on a given dipping refelector     ", 
"									",
" sfile= 	the name of input source file, if no name specified then",
"		use default source location. (source=1 or 2) 		",
"									",
" Density and Elastic Parameters:					",
" dfile= 	the name of input density file,                         ",
"               if no name specified then                             ",
"		assume a linear density profile with ...		",
" rho00=2.0	density at (0, 0) 					",
" drhodx=0.0	density gradient in x-direction  d(rho)/dx		",
" drhodz=0.0	density gradient in z-direction  d(rho)/dz		",
"									",
" afile= 	name of input elastic param.  (c11) aa file, if no name ",
"		specified then, assume a linear profile with ...	",
" aa00=2.0	elastic parameter at (0, 0) 				",
" daadx=0.0	parameter gradient in x-direction  d(aa)/dx		",
" daadz=0.0	parameter gradient in z-direction  d(aa)/dz		",
"									",
" cfile= 	name of input elastic param. (c33)  cc file, if no name ",
"		specified then, assume a linear profile with ...	",
" cc00=2.0	elastic parameter at (0, 0) 				",
" dccdx=0.0	parameter gradient in x-direction  d(cc)/dx		",
" dccdz=0.0	parameter gradient in z-direction  d(cc)/dz		",
"									",
" ffile= 	name of input elastic param.  (c13) ff file, if no name ",
"		specified then, assume a linear profile with ...	",
" ff00=2.0	elastic parameter at (0, 0) 				",
" dffdx=0.0	parameter gradient in x-direction  d(ff)/dx		",
" dffdz=0.0	parameter gradient in z-direction  d(ff)/dz		",
"									",
" lfile= 	name of input elastic param.  (c44) ll file, if no name ",
"		specified then, assume a linear profile with ...	",
" ll00=2.0	elastic parameter at (0, 0) 				",
" dlldx=0.0	parameter gradient in x-direction  d(ll)/dx		",
" dlldz=0.0	parameter gradient in z-direction  d(ll)/dz		",
"									",
" nfile= 	name of input elastic param. (c66)  nn file, if no name ",
"		specified then, assume a linear profile with ...	",
" nn00=2.0	elastic parameter at (0, 0) 				",
" dnndx=0.0	parameter gradient in x-direction  d(nn)/dx		",
" dnndz=0.0	parameter gradient in z-direction  d(nn)/dz		",
"									",
" Optimizations:							",
" The moving boundary option permits the user to restrict the computations",
" of the wavefield to be confined to a specific range of spatial coordinates.",
" The boundary of this restricted area moves with the wavefield		",
" movebc=0	0 do not use moving boundary optimization		",
"		1 use moving boundaries					",
"									",
NULL};

/*
 * Author: Tong Fei,	Center for Wave Phenomena, 
 *		Colorado School of Mines, Dec 1993
 * Some additional features by: Stig-Kyrre Foss, CWP
 *		Colorado School of Mines, Oct 2001
 * New features (Oct 2001): 
 * - setting receiver depth
 * - outputfiles with SU-headers
 * - additional commentary
 * Modifications (Mar 2010) Chris Liner, U Houston
 * - added snapshot mt param to parallel sufdmod2d functionality
 * - added verbose and some basic info echos
 * - error check that source loc is in grid
 * - dropped mbx1 etc from selfdoc (they were internally computed)
 * - moved default receiver depth to source depth
 * - added vspnx to selfdoc and moved default vspnx to source x
 * - changed sy in selfdoc to sz (typo)
 * - fixed bug in vsp file(s) allocation: was [nt,nx] now is [nt,nz]
 */

/* 
Notes:
	This program performs seismic modeling for elastic anisotropic 
	media with vertical axis of symmetry.  
	The finite-difference method with the FCT correction is used.

	Stability condition:	vmax*dt /(sqrt(2)*min(dx,dz)) < 1
	
	Two major stages are used in the algorithm:
	(1) conventional finite-difference wave extrapolation
	(2) followed by an FCT correction 

References:
	The detailed algorithm description is given in the article
	"Elimination of dispersion in finite-difference modeling 
	and migration"	in CWP-137, project review, page 155-174.

	Original reference to the FCT method:
	Boris, J., and Book, D., 1973, Flux-corrected transport. I.
	SHASTA, a fluid transport algorithm that works: 
	Journal of Computational Physics, vol. 11, p. 38-69.

*/
/********************** end self doc ***********************************/

/* function prototypes for subroutines used internally */
void 	read_parameter(int nx, int nz, float dx, float dz, 
		       float p00, float dpdx, float dpdz, char *file, float **pp);
void	anis_solver2(int it, float **u, float **v, float **w, 
		     float **e11, float **e33, float **e23, float **e12, float **e13,  
		     float **aa, float **cc, float **ff, float **ll, float **nn, 
		     float **rho, float **xzsource, float *fx, float *fy, float *fz, 
		     float dx, float dz, float dt, int  nx, int  nz, 
		     int dofct, int isurf, float eta0, float eta, 
		     float deta0dx, float deta0dz, float detadx, float detadz, 
		     int mbx1, int mbx2, int mbz1, int mbz2);
void	absorb1(int it, int  nx,  int  nz, float dx, 
		float dz, float dt, 
		float **u, float **u1, float **cc, float **rho, 
		int isurf, int mbx1, int mbx2, int mbz1, int mbz2, 
		int side, int tb);
void	absorb2(int it, int  nx,  int  nz, float dx, 
		float dz, float dt, 
		float **u, float **u1, float **cc, float **rho, 
		int isurf, int mbx1, int mbx2, int mbz1, int mbz2, 
		int side, int tb);
void	boundary_vel(float **cc, float **rho, 
		     float *vell, float *velr, float *velt, float *velb, 
		     int nx, int nz, int mbx1, int mbz1, int mbx2, int mbz2);
void	locate_source(int nx, int nz, int sx, int sz, 
		      float **xzsource, int source);
void tforce_ricker(int n, float *tforce, float dt, float fpeak);
void tforce_akb(int n, float *tforce, float dt, float fpeak);
void tforce_spike(int n, float *tforce, float dt, float fpeak);
void tforce_unit(int n, float *tforce, float dt, float fpeak);
void	moving_bc (int it, int nx, int nz, int sx, int sz, 
		   float dx, float dz, int impulse, int movebc, 
		   float *t, float vmax, int *mbx1, int *mbz1, int *mbx2, int *mbz2);
void	moving_fctbc (int mbx1, int mbz1, int mbx2, int mbz2, 
		      int nxcc1, int nzcc1, int nxcc2, int nzcc2, 
		      int *fctxbeg, int *fctzbeg, int *fctxend, int *fctzend);
void	strain2_x(int mbx1, int mbx2, int mbz1, int mbz2, 
		  float dx, float **a, float **da);
void	strain2_z(int mbx1, int mbx2, int mbz1, int mbz2, 
		  float dz, float **a, float **da);
void	strain2_xy(int mbx1, int mbx2, int mbz1, int mbz2, 
		   float dx, float **ay, float **da);
void	strain2_xz(int mbx1, int mbx2, int mbz1, int mbz2, 
		   float dx, float dz, float **ax, float **az, float **da);
void	strain2_yz(int mbx1, int mbx2, int mbz1, int mbz2, 
		   float dz, float **ay, float **da);
void	strain4_x(int mbx1, int mbx2, int mbz1, int mbz2, 
		  float dx, float **a, float **da);
void	strain4_z(int mbx1, int mbx2, int mbz1, int mbz2, 
		  float dz, float **a, float **da);
void	strain4_xy(int mbx1, int mbx2, int mbz1, int mbz2, 
		   float dx, float **ay, float **da);
void	strain4_xz(int mbx1, int mbx2, int mbz1, int mbz2, 
		   float dx, float dz, float **ax, float **az, float **da);
void	strain4_yz(int mbx1, int mbx2, int mbz1, int mbz2, float dz, float **ay, float **da);
void	difference(int mbx1, int mbx2, int mbz1, int mbz2, int it, 
		   float **u1, float **u, 
		   float **xzsource, float *f, float dt, float rdxdz); 
void	difference_2x(int mbx1, int mbx2, int mbz1, int mbz2, int shift, 
		      float dtdz, float **u, float **e, float **c);
void	difference_2z(int mbx1, int mbx2, int mbz1, int mbz2, int shift, 
		      float dtdz, float **u, float **e, float **c);
void	difference_4x(int mbx1, int mbx2, int mbz1, int mbz2, int shift, 
		      float dtdz, float **u, float **e, float **c);
void	difference_4z(int mbx1, int mbx2, int mbz1, int mbz2, int shift, 
		      float dtdz, float **u, float **e, float **c);

void	fct2d1o(float **u, float **u1, int nx, int nz, 
		float eta0, float eta, float deta0dx, float deta0dz, 
		float detadx, float detadz, float dx, float dz, 
		int mbx1, int mbx2, int mbz1, int mbz2, 
		float **f0, float **f1);

void    su_output(int nx, int sx, int sdepth, int ns, float dx, 
		  float dt, FILE *outputfile, float **data);


int
main (int  argc,  char  **argv)
{

  int receiverdepth,suhead;
  int	ix, iz, it, nx, nz,  
    nt, sx, sz, vspnx;
  int	isurf, impulse, dofct, mbx1, mbx2, mbz1, mbz2;
  int 	fctxbeg, fctxend, fctzbeg, fctzend,
    nxcc1=0, nxcc2=0, nzcc1=0, nzcc2=0;
  int 	indexux, indexuy, indexuz, wavelet, movebc;
  int 	source, order; 
  int   mt, verbose;
  float	dx, dz, dt, fpeak, rho00, vmax;
  float	eta, eta0;
  float	daadx, daadz, 
    dccdx, dccdz, dffdx, dffdz, dlldx, dlldz, 
    dnndx, dnndz, drhodx, drhodz; 
  float	deta0dx, deta0dz, detadx, detadz;
  float 	aa00, cc00, ff00, ll00, nn00;
  float	**u=NULL,  **v=NULL,  **w=NULL,   
    **e11=NULL, **e33=NULL, **e12=NULL, **e13=NULL, **e23=NULL, 
    **xzsource=NULL, **aa=NULL, **cc=NULL, **ff=NULL, **ll=NULL, **nn=NULL, **rho=NULL, 
    *fx=NULL, *fy=NULL, *fz, *t=NULL, 
    **refl_x=NULL, **refl_y=NULL, **refl_z=NULL,
    **vsp_x=NULL, **vsp_y=NULL, **vsp_z=NULL;
  FILE	*outfp=stdout;
  FILE	*outfpx=NULL, *outfpy=NULL, *outfpz=NULL;
  FILE	*outreflx=NULL, *outrefly=NULL, *outreflz=NULL,  
    *outvspx=NULL, *outvspy=NULL, *outvspz=NULL; 
  /* input file names */
  char *sfile="";		/* source file name */
  char *dfile="";		/* density file name */
  char *afile="";		/* file name for elastic parameter aa*/
  char *cfile="";		/* file name for elastic parameter cc*/
  char *ffile="";		/* file name for elastic parameter ff*/
  char *lfile="";		/* file name for elastic parameter ll*/
  char *nfile="";		/* file name for elastic parameter nn*/
  /* output file names */
  char *reflxfile="";	/* reflection seismogram file name, x-comp */
  char *reflyfile="";	/* reflection seismogram file name, y-comp */
  char *reflzfile="";	/* reflection seismogram file name, z-comp */
  char *vspxfile="";	/* VSP seismogram file name, x-comp */
  char *vspyfile="";	/* VSP seismogram file name, y-comp */
  char *vspzfile="";	/* VSP seismogram file name, z-comp */
  
  initargs (argc, argv);
  requestdoc(0);
  
  /* get required parameters  */
  if (!getparint("nt", &nt)) nt=250;
  if (!getparint("nx", &nx)) nx=300;
  if (!getparint("nz", &nz)) nz=250;
  if (!getparint("sx", &sx)) sx=nx/2;
	if (sx>nx) err("Source sx=%i is greater than nx=%i. Exit",sx,nx);
  if (!getparint("sz", &sz)) sz=nz/2;
	if (sz>nz) err("Source sz=%f is greater than nz=%i. Exit",sz,nx);
	if (!getparint("receiverdepth", &receiverdepth)) receiverdepth=sz;
  if (!getparint("vspnx", &vspnx)) vspnx=sx;
  if (!getparint("impulse", &impulse)) impulse=0;
  if (!getparint("source", &source)) source=0;
  if (!getparint("isurf", &isurf)) isurf=1;
  if (!getparint("dofct", &dofct)) dofct=0;
  if (!getparint("fctxbeg", &fctxbeg)) fctxbeg=0;
  if (!getparint("fctzbeg", &fctzbeg)) fctzbeg=0;
  if (!getparint("fctxend", &fctxend)) fctxend=nx;
  if (!getparint("fctzend", &fctzend)) fctzend=nz;
  if (!getparint("indexux", &indexux)) indexux=0;
  if (!getparint("indexuy", &indexuy)) indexuy=0;
  if (!getparint("indexuz", &indexuz)) indexuz=0;
  if (!getparint("wavelet", &wavelet)) wavelet=1;
  if (!getparint("movebc", &movebc)) movebc=0;
  if (!getparint("order", &order)) order=2;
  if (!getparint("suhead",&suhead)) suhead=1;
  if (!getparint("mt",&mt)) mt=1;
  if (!getparint("verbose",&verbose)) verbose=0;
  if (!getparfloat("dx", &dx)) dx=0.02;
  if (!getparfloat("dz", &dz)) dz=0.02;
  if (!getparfloat("dt", &dt)) dt=0.002;
  if (!getparfloat("fpeak", &fpeak)) fpeak=20.0;
  if (!getparfloat("aa00", &aa00)) aa00=2.0;
  if (!getparfloat("cc00", &cc00)) cc00=2.0;
  if (!getparfloat("ff00", &ff00)) ff00=2.0;
  if (!getparfloat("ll00", &ll00)) ll00=2.0;
  if (!getparfloat("nn00", &nn00)) nn00=2.0;
  if (!getparfloat("daadx", &daadx)) daadx=0.0;
  if (!getparfloat("daadz", &daadz)) daadz=0.0;
  if (!getparfloat("dccdx", &dccdx)) dccdx=0.0;
  if (!getparfloat("dccdz", &dccdz)) dccdz=0.0;
  if (!getparfloat("dffdx", &dffdx)) dffdx=0.0;
  if (!getparfloat("dffdz", &dffdz)) dffdz=0.0;
  if (!getparfloat("dlldx", &dlldx)) dlldx=0.0;
  if (!getparfloat("dlldz", &dlldz)) dlldz=0.0;
  if (!getparfloat("dnndx", &dnndx)) dnndx=0.0;
  if (!getparfloat("dnndz", &dnndz)) dnndz=0.0;
  if (!getparfloat("drhodx", &drhodx)) drhodx=0.0;
  if (!getparfloat("drhodz", &drhodz)) drhodz=0.0;
  if (!getparfloat("rho00", &rho00)) rho00=1.0;
  if (!getparfloat("eta0", &eta0)) eta0=0.03;
  if (!getparfloat("eta", &eta)) eta=0.04;
  if (!getparfloat("deta0dx", &deta0dx)) deta0dx=0.0;
  if (!getparfloat("deta0dz", &deta0dz)) deta0dz=0.0;
  if (!getparfloat("detadx", &detadx)) detadx=0.0;
  if (!getparfloat("detadz", &detadz)) detadz=0.0;
  getparstring("sfile",&sfile);
  getparstring("dfile",&dfile);
  getparstring("afile",&afile);
  getparstring("cfile",&cfile);
  getparstring("ffile",&ffile);
  getparstring("lfile",&lfile);
  getparstring("nfile",&nfile);
  getparstring("nfile",&nfile);
  getparstring("reflxfile",&reflxfile);
  getparstring("reflyfile",&reflyfile);
  getparstring("reflzfile",&reflzfile);
  getparstring("vspxfile",&vspxfile);
  getparstring("vspyfile",&vspyfile);
  getparstring("vspzfile",&vspzfile);
	
	
  /*   allocate space	*/
  u = alloc2float(nz, nx);
  v = alloc2float(nz,nx);
  w = alloc2float(nz,nx);
  e11 = alloc2float(nz,nx);
  e33 = alloc2float(nz,nx);
  e23 = alloc2float(nz,nx);
  e12 = alloc2float(nz,nx);
  e13 = alloc2float(nz,nx);
  xzsource = alloc2float(nz,nx);	
  aa = alloc2float(nz,nx);	
  cc = alloc2float(nz,nx);	
  ff = alloc2float(nz,nx);	
  ll = alloc2float(nz,nx);	
  nn = alloc2float(nz,nx);	
  rho = alloc2float(nz,nx);	
  fx = alloc1float(nt);
  fy = alloc1float(nt);
  fz = alloc1float(nt);
  t = alloc1float(nt);

 	
  /* allocate optional space for store reflection and VSP seismogram */
  if (*reflxfile != '\0') {  /* allocate space for refl_x  */
    refl_x = alloc2float(nt, nx); 
  }
  if (*reflyfile != '\0') {  /* allocate space for refl_y  */
    refl_y = alloc2float(nt, nx); 
  }
  if (*reflzfile != '\0') {  /* allocate space for refl_z  */
    refl_z = alloc2float(nt, nx); 
  }
  if (*vspxfile != '\0') {  /* allocate space for vsp_x  */
    vsp_x = alloc2float(nt, nz); 
  }
  if (*vspyfile != '\0') {  /* allocate space for vsp_y  */
    vsp_y = alloc2float(nt, nz); 
  }
  if (*vspzfile != '\0') {  /* allocate space for vsp_z  */
    vsp_z = alloc2float(nt, nz); 
  }

	warn("Memory usage approximately %f MB", (float) (15*nz*nx*4+4*nt*4+6*nt*nx*4)/(1024*1024));

  /*   initial condition  */
  for (ix=0; ix < nx; ix++)
    for (iz=0; iz < nz; iz++)
      {
	u[ix][iz]=0.0;
	v[ix][iz]=0.0;
	w[ix][iz]=0.0;
	xzsource[ix][iz]=0.0;
      }
		
  for (ix=0; ix<nx; ix++)
    for (iz=0; iz<nz; iz++)
      {
	e11[ix][iz]=0.0;
	e33[ix][iz]=0.0;
	e23[ix][iz]=0.0;
	e13[ix][iz]=0.0;
	e12[ix][iz]=0.0;
      }

  /*   get time response of the source function */
  for (it=0; it<nt; it++) 
    {
      t[it]=it*dt;
      fx[it]=0.0;
      fy[it]=0.0;
      fz[it]=0.0;
    }

  if (indexux) {		/*  x-component of the force  */
    if (wavelet == 1)
      tforce_akb(nt, fx, dt, fpeak);
    if (wavelet == 2)
      tforce_ricker(nt, fx, dt, fpeak);
    if (wavelet == 3) 	
      tforce_spike(nt, fx, dt, fpeak);
    if (wavelet == 4) 	
      tforce_unit(nt, fx, dt, fpeak);
  }
  if (indexuy) {		/*  y-component of the force  */
    if (wavelet == 1)
      tforce_akb(nt, fy, dt, fpeak);
    if (wavelet == 2)
      tforce_ricker(nt, fy, dt, fpeak);
    if (wavelet == 3) 	
      tforce_spike(nt, fy, dt, fpeak);
    if (wavelet == 4) 	
      tforce_unit(nt, fy, dt, fpeak);
  }
  if (indexuz) {		/*  z-component of the force  */
    if (wavelet == 1)
      tforce_akb(nt, fz, dt, fpeak);
    if (wavelet == 2)
      tforce_ricker(nt, fz, dt, fpeak);
    if (wavelet == 3) 	
      tforce_spike(nt, fz, dt, fpeak);
    if (wavelet == 4) 	
      tforce_unit(nt, fz, dt, fpeak);
  }

  warn("Source function set");


  /*    obtain density and elastic parameters  */
  vmax = 0.0;
  read_parameter(nx, nz, dx, dz, rho00, drhodx, drhodz, dfile, rho); 
  read_parameter(nx, nz, dx, dz, aa00, daadx, daadz, afile, aa); 
  read_parameter(nx, nz, dx, dz, cc00, dccdx, dccdz, cfile, cc); 
  read_parameter(nx, nz, dx, dz, ff00, dffdx, dffdz, ffile, ff); 
  read_parameter(nx, nz, dx, dz, ll00, dlldx, dlldz, lfile, ll); 
  read_parameter(nx, nz, dx, dz, nn00, dnndx, dnndz, nfile, nn); 
	
	warn("Parameter grids read");

    /*  compute density inverse and maximum velocity  */	
	for (ix=0; ix < nx; ix++)   {
		for (iz=0; iz < nz; iz++) {
			rho[ix][iz]=1.0/rho[ix][iz];
			if ( cc[ix][iz]*rho[ix][iz] > vmax ) vmax = cc[ix][iz]*rho[ix][iz];
		}
	}
    vmax = sqrt(vmax);
	
	warn("vmax = %f", vmax);
	if (dz <= dx) {
		warn("stability check:  vmax*dt/(sqrt(2)*min(dx,dz)) = %f (should be < 1)", vmax*dt /(sqrt(2)*dz));
	} else {
		warn("stability check:  vmax*dt/(sqrt(2)*min(dx,dz)) = %f (should be < 1)", vmax*dt /(sqrt(2)*dx));
	}
	
  /*  give source location  */
  if (*sfile != '\0') {
    read_parameter(nx, nz, dx, dz, aa00, daadx, daadz, sfile, xzsource); 
  } else {
    locate_source(nx, nz, sx, sz, xzsource, source);
  }

  /* Debugging */
  nxcc2=nx;
  nzcc2=nz;
  /* end debugging */

  /*     evolve in time    */
  outfpx = fopen("snapshotx.data", "w");
  outfpy = fopen("snapshoty.data", "w");
  outfpz = fopen("snapshotz.data", "w");
	
	warn("Snapshot files open");

  /* begin time loop */
  for (it=0; it < nt; it++) {
      
	  if (verbose==2) fprintf (stderr,"it= %d\n", it);

      moving_bc (it, nx, nz, sx, sz, 
		 dx, dz, impulse, movebc, t, vmax, 
		 &mbx1, &mbz1, &mbx2, &mbz2);

      /* FCT correction is localized to the area bounded by */
      /* (fctxend - fctxbeg) by (fctzend-fctzbeg) */
      /* contain the FCT correction boundary by the model boundary */
      /* computed by moving_bc */
      moving_fctbc (mbx1, mbz1, mbx2, mbz2, 
		    nxcc1, nzcc1, nxcc2, nzcc2, 
		    &fctxbeg, &fctzbeg, &fctxend, &fctzend);

      anis_solver2(it, u, v, w, e11,  
		   e33, e23, e12, e13, aa, cc, ff, ll, nn, 
		   rho, xzsource, fx, fy, fz, 
		   dx, dz, dt, nx, nz, dofct, isurf, 
		   eta0, eta, deta0dx, deta0dz, 
		   detadx, detadz, mbx1, mbx2, mbz1, mbz2);

      /*  get reflection seismogram  */
      if (*reflxfile != '\0') {  /* get refl_x  */
		  for (ix=0; ix<nx; ix++) {
			  refl_x[ix][it]=u[ix][receiverdepth];
		  }
      }
	  
      if (*reflyfile != '\0') {  /* get refl_y  */
		  for (ix=0; ix<nx; ix++) {
			  refl_y[ix][it]=v[ix][receiverdepth];
		  }
      }	/*  end get reflection seismogram  */
	  
      if (*reflzfile != '\0') {  /* get refl_z  */
		  for (ix=0; ix<nx; ix++) {
			  if (w[ix][receiverdepth]>1){
				 warn("problems at it=%d x=%d \n",it, ix);
			  }
			  refl_z[ix][it]=w[ix][receiverdepth];
		  }
      }
	 /* warn("got here 1, vspnx=%i vspxfile=%s",vspnx,vspxfile); */

      /*  get VSP seismogram  */
      if ( vspnx >= 0 && vspnx < nx) {   /*position not out of range*/
		
	
		  if (*vspxfile != '\0') {  /* get vsp_x  */
			  for (iz=0; iz<nz; iz++) {
				  vsp_x[iz][it]=u[vspnx][iz];
				  /* warn("got here 2, iz=%i, u[vspnx][iz]=%f",iz,u[vspnx][iz]);	*/
			  }
		  }
		  if (*vspyfile != '\0') {  /* get vsp_x  */
			  for (iz=0; iz<nz; iz++) {
				  vsp_y[iz][it]=v[vspnx][iz];
			  }
		  }
		  
		  if (*vspzfile != '\0') {  /* get vsp_x  */
			  for (iz=0; iz<nz; iz++) {
				  vsp_z[iz][it]=w[vspnx][iz];
			  }
		  }

		  
      }  /*  end get VSP seismogram  */

	  /* write snaps every mt time steps */
	  if (it%mt==0) {
		  fwrite(u[0], sizeof(float), nz*nx, outfpx);	
		  fwrite(v[0], sizeof(float), nz*nx, outfpy);	
		  fwrite(w[0], sizeof(float), nz*nx, outfpz);
		  if (verbose>=1) warn("snap at it=%i",it);
	  }

    }	/* end time loop */

	/* write last x-component snap to std out */
	fwrite(u[0], sizeof(float), nz*nx, outfp);	
	
	warn("Source function set");

	/*  output seismogram  */
	if (*reflxfile != '\0') {  /* write refl_x */
	if ((outreflx = fopen(reflxfile, "w"))==NULL) {
	  fprintf(stderr, "Cannot open file=%s\n", reflxfile); 
	  exit(1);
	}
	if (suhead==1){
	  su_output(nx,sx,sz,nt,dx,dt,outreflx,refl_x);
	}
	else
	  {
	fwrite(refl_x[0], sizeof(float), nx*nt, outreflx);
	  }
	fclose(outreflx);
	}
	if (*reflyfile != '\0') {  /* write refl_y */
	if ((outrefly = fopen(reflyfile, "w"))==NULL) {
	  fprintf(stderr, "Cannot open file=%s\n", reflyfile); 
	  exit(1);
	}
	if (suhead==1){
	  su_output(nx,sx,sz,nt,dx,dt,outrefly,refl_y);
	}
	else
	  {
	fwrite(refl_y[0], sizeof(float), nx*nt, outrefly);
	  }
	fclose(outrefly);
	}
	if (*reflzfile != '\0') {  /* write refl_z */
	if ((outreflz = fopen(reflzfile, "w"))==NULL) {
	  fprintf(stderr, "Cannot open file=%s\n", reflzfile); 
	  exit(1);
	}
	if (suhead==1){
	  su_output(nx,sx,sz,nt,dx,dt,outreflz,refl_z);
	}
	else
	  {
	fwrite(refl_z[0], sizeof(float), nx*nt, outreflz);
	  }
	fclose(outreflz);
	}
	if (*vspxfile != '\0') {  /* write vsp_x */
	if ((outvspx = fopen(vspxfile, "w"))==NULL) {
	  fprintf(stderr, "Cannot open file=%s\n", vspxfile); 
	  exit(1);
	}
	if (suhead==1){
	  su_output(nz,sx,sz,nt,dx,dt,outvspx,vsp_x);
	}
	else
	  {
	fwrite(vsp_x[0], sizeof(float), nz*nt, outvspx);
	  }
	fclose(outvspx);
	}
	if (*vspyfile != '\0') {  /* write vsp_x */
	if ((outvspy = fopen(vspyfile, "w"))==NULL) {
	  fprintf(stderr, "Cannot open file=%s\n", vspyfile); 
	  exit(1);
	}	
	if (suhead==1){
	  su_output(nz,sx,sz,nt,dx,dt,outvspy,vsp_y);
	}
	else
	  {
	fwrite(vsp_y[0], sizeof(float), nz*nt, outvspy);
	  }
	}
	if (*vspzfile != '\0') {  /* write vsp_z */
	if ((outvspz = fopen(vspzfile, "w"))==NULL) {
	  fprintf(stderr, "Cannot open file=%s\n", vspzfile); 
	  exit(1);
	}
	if (suhead==1){
	  su_output(nz,sx,sz,nt,dx,dt,outvspz,vsp_z);
	}
	else
	  {
	fwrite(vsp_z[0], sizeof(float), nz*nt, outvspz);
	  }
	}

	return(CWP_Exit());
}
/*------------------ end of main program ----------------------------*/

/************************************************************************
* read_parameter --  Obtain elastic parameter, either read from a file 
*			or assume linear variation
*************************************************************************
*************************************************************************
* Input: 
*	int nx		number of grids in x direction 
*	int nz		number of grids in z direction
*	float dx	spatial step in x-direction 	
*	float dz	spatial step in z-direction 	
*	float p00=2.0	elastic parameter at (0, 0) 
*	float dpdx=0.0	parameter gradient in x-direction  d(p)/dx
*	float dpdz=0.0	parameter gradient in z-direction  d(p)/dz 
*	char *file	file name store the elastic parameter
*	
*************************************************************************
* Output: float **pp	elastic parameter array
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void 	read_parameter(int nx, int nz, float dx, float dz, 
		       float p00, float dpdx, float dpdz, char *file, float **pp)
{
  int 	ix, iz;
  FILE	*infp;

  /*    obtain  elastic parameter  */
  if (*file != '\0') {  /* open requested parameter file */
    if ((infp = fopen(file, "r"))==NULL) {
      fprintf(stderr, "Cannot open file=%s\n", file); 
      exit(1);
    }

    /* read elastic parameter into the array pp[nz][nx] */
    fread(pp[0], sizeof(float), nz*nx, infp);
  } else { /* assume a linear parameter profile */
    for (ix=0; ix < nx; ix++)
      for (iz=0; iz < nz; iz++)
	{
	  pp[ix][iz]=p00+dpdx*(ix-1)*dx
	    +dpdz*(iz-1)*dz;
	}
  } 
}



/************************************************************************
*
* anis_solver2 ---  2nd-order 2-D finite-difference solver for the 
*		first order elastic wave equation
*
*************************************************************************
*************************************************************************
* Input:
*
*	for the elastic wave equation
*	float **u	x-component, solution at previous time level
*	float **v	y-component, solution at previous time level
*	float **w	z-component, solution at previous time level
*	float **aa	elastic parameter
*	float **cc	elastic parameter
*	float **ff	elastic parameter
*	float **ll	elastic parameter
*	float **nn	elastic parameter
*	float **rho	reverse of density field
*	float *fx	time response of the source function, x-component
*	float *fy	time response of the source function, y-component
*	float *fz	time response of the source function, z-component
*	float **xzsource	source location
*
*	int it		current time step
*	float dx	spacing in x-direction
*	float dz	spacing in z-direction
*	int nx		number of grids in x-direction
*	int mbx1	sample start in x-direction
*	int mbx2	sample end in x-direction
*	int nz		number of grids in z-direction
*	int mbz1	sample start in z-direction
*	int mbz2	sample end in z-direction
*	int fctxbeg	left boundary for the FCT correction
*	int fctxend	right boundary for the FCT correction
*	int fctzbeg	top boundary for the FCT correction
*	int fctzend	bottom boundary for the FCT correction 
*	int dofct	FCT correction index
*			=1 do FCT
*			=0 not do the FCT
*	int isurf	index for surface boundary condition
*	float eta0	diffusion coefficient
*	float deta0dx	gradient of eta0 in x-direction
*	float deta0dz	gradient of eta0 in z-direction
*	float eta	anti-diffusion coefficient
*	float detadx	gradient of eta in x-direction
*	float detadz	gradient of eta in z-direction
*
************************************************************************
* Output:
*	float **u	solution of x-component at next time level
*	float **v	solution of y-component at next time level
*	float **w	solution of z-component at next time level
************************************************************************
* Notes: this is a 2nd order finite-difference routine on staggered 
*	with optional FCT correction, and boundary conditions included.
************************************************************************
* Author:	Tong Fei (1993), Colorado School of Mines
************************************************************************/
void	anis_solver2(int it, float **u, float **v, float **w, 
		     float **e11, float **e33, float **e23, float **e12, float **e13,  
		     float **aa, float **cc, float **ff, float **ll, float **nn, 
		     float **rho, float **xzsource, float *fx, float *fy, float *fz, 
		     float dx, float dz, float dt, int  nx, int  nz, 
		     int dofct, int isurf, float eta0, float eta, 
		     float deta0dx, float deta0dz, float detadx, float detadz, 
		     int mbx1, int mbx2, int mbz1, int mbz2)
{
  int 	ix, iz;
  float	dtdx=dt/dx, dtdz=dt/dz, rdxdz=1.0/dx/dz;

  float	**u1, **f0, **f1;

  /*   allocate temporery space	*/
  u1 = alloc2float(nz, nx);
  f0 = alloc2float(nz, nx);
  f1 = alloc2float(nz, nx);


  /*  solve u at next time step  */
  difference(mbx1, mbx2, mbz1, mbz2, it, u1, u, 
	     xzsource, fx, dt, rdxdz); 
  /*  finite-difference to compute the contribution of e11 to u */
  difference_2x(mbx1, mbx2, mbz1, mbz2, -1, dtdx, u1, e11, aa); 
  /*  finite-difference to compute the contribution of e33 to u */
  difference_2x(mbx1, mbx2, mbz1, mbz2, -1, dtdx, u1, e33, ff); 
  /*  finite-difference to compute the contribution of e13 to u */
  difference_2z(mbx1, mbx2, mbz1, mbz2, -1, 2.0*dtdz, u1, e13, ll); 

  if (dofct) 
    {
      fct2d1o(u, u1, nx, nz, eta0, eta, deta0dx, deta0dz, 
	      detadx, detadz, dx, dz, mbx1, mbx2, mbz1, mbz2, 
	      f0, f1);
    }

	/*  apply the boundary condition  */
  absorb1(it, nx, nz, dx, dz, dt, u, u1, cc, rho, 
	  isurf, mbx1, mbx2, mbz1, mbz2, 1, 1);
  absorb1(it, nx, nz, dx, dz, dt, u, u1, ll, rho, 
	  isurf, mbx1, mbx2, mbz1, mbz2, 1, 1);

  /*  update the solution  */
  for (ix=mbx1; ix < mbx2; ix++)
    for (iz=mbz1; iz < mbz2; iz++)
      {
	u[ix][iz]=u1[ix][iz];
      }

  /*  solve w at next time step  */
  difference(mbx1-1, mbx2, mbz1, mbz2-1, it, u1, w, 
	     xzsource, fz, dt, rdxdz); 
  /*  finite-difference to compute the contribution of e13 to w */
  difference_2x(mbx1-1, mbx2, mbz1, mbz2-1, 0, 2.0*dtdx, u1, e13, ll); 
  /*  finite-difference to compute the contribution of e11 to w */
  difference_2z(mbx1-1, mbx2, mbz1, mbz2-1, 0, dtdx, u1, e11, ff); 
  /*  finite-difference to compute the contribution of e33 to w */
  difference_2z(mbx1-1, mbx2, mbz1, mbz2-1, 0, dtdz, u1, e33, cc); 

  if (dofct) 
    {
      fct2d1o(w, u1, nx, nz, eta0, eta, deta0dx, deta0dz, 
	      detadx, detadz, dx, dz, mbx1, mbx2-1, mbz1, mbz2-1, 
	      f0, f1);
    }

  /*  apply the boundary condition  */
  absorb1(it, nx, nz, dx, dz, dt, w, u1, cc, rho, 
	  isurf, mbx1, mbx2, mbz1, mbz2-1, 0, 1);

  /*  update the solution  */
  for (ix=mbx1; ix < mbx2-1; ix++)
    for (iz=mbz1; iz < mbz2-1; iz++)
      {
	w[ix][iz]=u1[ix][iz];
      }

  /*  solve v at next time step  */
  difference(mbx1-1, mbx2, mbz1, mbz2-1, it, u1, v, 
	     xzsource, fy, dt, rdxdz); 
  /*  finite-difference to compute the contribution of e12 to v */
  difference_2x(mbx1-1, mbx2, mbz1, mbz2-1, 0, 2.0*dtdx, u1, e12, nn); 
  /*  finite-difference to compute the contribution of e23 to v */
  difference_2z(mbx1-1, mbx2, mbz1, mbz2-1, 0, 2.0*dtdz, u1, e23, ll); 

  if (dofct) 
    {
      fct2d1o(v, u1, nx, nz, eta0, eta, deta0dx, deta0dz, 
	      detadx, detadz, dx, dz, mbx1, mbx2-1, mbz1, mbz2-1, 
	      f0, f1);
    }

  /*  apply the boundary condition  */
  absorb1(it, nx, nz, dx, dz, dt, v, u1, ll, rho, 
	  isurf, mbx1, mbx2-1, mbz1, mbz2-1, 0, 1);

  /*  update the solution  */
  for (ix=mbx1; ix < mbx2-1; ix++)
    for (iz=mbz1; iz < mbz2-1; iz++)
      {
	v[ix][iz]=u1[ix][iz];
      }

  /*  compute strain tensor  */
  /*  solve e11 at next time step  */
  difference(mbx1-1, mbx2, mbz1-1, mbz2+1, it, u1, e11, 
	     xzsource, fz, 0.0, 0.0); 
  difference_2x(mbx1-1, mbx2, mbz1-1, mbz2+1, 0, dtdx, u1, u, rho); 
  /*  no BC applied to e11, size (nx-1)*nz  */
  /*  update the solution e11 */
  for (ix=mbx1; ix < mbx2-1; ix++)
    for (iz=mbz1; iz < mbz2; iz++)
      {
	e11[ix][iz]=u1[ix][iz];
      }

  /*  solve e33 at next time step  */
  difference(mbx1-1, mbx2, mbz1, mbz2, it, u1, e33, 
	     xzsource, fz, 0.0, 0.0); 
  difference_2z(mbx1-1, mbx2, mbz1, mbz2, -1, dtdx, u1, w, rho); 
  /*  apply BC to top and bottom  for e33  */
  /*	absorb1(it, nx, nz, dx, dz, dt, e33, u1, cc, rho, 
	isurf, mbx1, mbx2-1, mbz1, mbz2, 0, 1);
  */	/*  update the solution e33 */
  for (ix=mbx1; ix < mbx2-1; ix++)
    for (iz=mbz1+1; iz < mbz2-1; iz++)
      {
	e33[ix][iz]=u1[ix][iz];
      }

  /*  solve e23 at next time step  */
  difference(mbx1-1, mbx2, mbz1, mbz2, it, u1, e23, 
	     xzsource, fz, 0.0, 0.0); 
  difference_2z(mbx1-1, mbx2, mbz1, mbz2, -1, 0.5*dtdx, u1, v, rho); 
  /*  apply BC to top and bottom for e23  */
  /*	absorb1(it, nx, nz, dx, dz, dt, e23, u1, ll, rho, 
	isurf, mbx1, mbx2-1, mbz1, mbz2, 0, 1);
  */	/*  update the solution e23 */
  for (ix=mbx1; ix < mbx2-1; ix++)
    for (iz=mbz1+1; iz < mbz2-1; iz++)
      {
	e23[ix][iz]=u1[ix][iz];
      }

  /*  solve e12 at next time step  */
  difference(mbx1, mbx2, mbz1-1, mbz2, it, u1, e12, 
	     xzsource, fz, 0.0, 0.0); 
  difference_2x(mbx1, mbx2, mbz1-1, mbz2, -1, 0.5*dtdx, u1, v, rho); 
  /*  apply BC to sides for e12  */
  absorb1(it, nx, nz, dx, dz, dt, e12, u1, ll, rho, 
	  isurf, mbx1, mbx2, mbz1, mbz2-1, 1, 0);
  /*  update the solution e12 */
  for (ix=mbx1; ix < mbx2; ix++)
    for (iz=mbz1; iz < mbz2-1; iz++)
      {
	e12[ix][iz]=u1[ix][iz];
      }

  /*  solve e13 at next time step  */
  difference(mbx1, mbx2, mbz1-1, mbz2, it, u1, e13, 
	     xzsource, fz, 0.0, 0.0); 
  difference_2x(mbx1, mbx2, mbz1-1, mbz2, -1, 0.5*dtdx, u1, w, rho); 
  difference_2z(mbx1, mbx2, mbz1-1, mbz2, 0, 0.5*dtdx, u1, u, rho); 
  /*  apply BC to sides for e13  */
  absorb1(it, nx, nz, dx, dz, dt, e13, u1, cc, rho, 
	  isurf, mbx1, mbx2, mbz1, mbz2-1, 1, 0);
  /*  update the solution e13 */
  for (ix=mbx1; ix < mbx2; ix++)
    for (iz=mbz1; iz < mbz2-1; iz++)
      {
	e13[ix][iz]=u1[ix][iz];
      }

  /*  free the temporery space  */
  free2float(u1);
  free2float(f0);	
  free2float(f1);	
}	


/************************************************************************
* absorb1 -- function to apply boundary condition on one boundary layer,
*	 absorbing boundary condition applied on both sides
*		and bottom.
*************************************************************************
* Notes: 
*		"isurf" can be used to select surface boundary condition	
*
* 		Absorbing boundary conditions on the sides are determined
*	 by the method of Clayton and Engquist, 1980.
*************************************************************************
*************************************************************************
* Input: 
*	int it		time index 
*	int nx		number of grids in x direction 
*	int nz		number of grids in z direction
*	float dx	spatial step in x direction 
*	float dz	spatial step in z direction 
*	float dt	time step	
*	float **u	wavefield at previous time step
*	float **u1	wavefield at current time step
*	float **cc	elastic parameter profile
*	float **rho	density reverse
*	float isurf	1 absorbing BC on surface
*			2 zero gradient BC on surface
*			3 zero value BC on surface
*	int mbx1		left boundary position
*	int mby1		top boundary position
*	int mbx2		right boundary postion 
*	int mby2		bottom boundary position
*	
*************************************************************************
* Output: float **u1	wavefield after applying the boundary condition 
*
*************************************************************************
* Reference: Clayton, R., and Engquist, B., 1980, Absorbing side boundary
*		condition for wave equation migration: Geophysics, vol. 45,
*		p. 895-904.
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	absorb1(int it, int  nx,  int  nz, float dx, 
		float dz, float dt, 
		float **u, float **u1, float **cc, float **rho, 
		int isurf, int mbx1, int mbx2, int mbz1, int mbz2, 
		int side, int tb)
{
  int	i, j;
  float	k, dtdz=dt/dz, dtdx=dt/dx;
  float 	*vell, *velr, *velt, *velb;

  vell = alloc1float(nz);
  velr = alloc1float(nz);
  velt = alloc1float(nx);
  velb = alloc1float(nx);

  /*  compute velocity at the boundary  */
  boundary_vel(cc, rho, vell, velr, velt, velb, 
	       nx, nz, mbx1, mbz1, mbx2, mbz2);

  
  if (tb) {
    for (i=mbx1; i<mbx2; i++)
      {
	k=-dtdz*velt[i];
	if ((k>1)||(k<-1))
	  warn("WARNING:Big k-values.");
	if (isurf == 1)  /* absorbing surface boundary condition */	  {
	    u1[i][mbz1]=u[i][mbz1]-k*(u[i][mbz1+1]-u[i][mbz1]);
	  }
	else if (isurf == 2)  /* zero gradient BC */
	  {
	    u1[i][mbz1]=u1[i][mbz1+1];
	  }	
	else if (isurf == 3)  /*  zero value BC  */
	  {
	    u1[i][mbz1]=0.0;
	  }	

	/* absorbing surface boundary condition */
	k=dtdz*velb[i];
	u1[i][mbz2-1]=u[i][mbz2-1]-k*(u[i][mbz2-1]-u[i][mbz2-2]);
      }
  }
	
  if (side) {
    /* absorbing surface boundary condition */
    for (j=mbz1; j<mbz2; j++)
      {
	k=dtdx*velr[j];
	u1[mbx2-1][j]=u[mbx2-1][j]-k*(u[mbx2-1][j]-u[mbx2-2][j]);	
	k=-dtdx*vell[j];
	u1[mbx1][j]=u[mbx1][j]-k*(u[mbx1+1][j]-u[mbx1][j]);
      }
  }

  free1float(vell);
  free1float(velr);
  free1float(velt);
  free1float(velb);
	
}


/************************************************************************
* absorb2 -- function to apply boundary condition on two boundary layers,
*	 absorbing boundary condition applied on both sides
*		and bottom.
*************************************************************************
* Notes: 
*		"isurf" can be used to select surface boundary condition	
*
* 		Absorbing boundary conditions on the sides are determined
*	 by the method of Clayton and Engquist, 1980.
*************************************************************************
*************************************************************************
* Input: 
*	int it		time index 
*	int nx		number of grids in x direction 
*	int nz		number of grids in z direction
*	float dx	spatial step in x direction 
*	float dz	spatial step in z direction 
*	float dt	time step	
*	float **u	wavefield at previous time step
*	float **u1	wavefield at current time step
*	float **cc	elastic parameter profile
*	float **rho	density reverse
*	float isurf	1 absorbing BC on surface
*			2 zero gradient BC on surface
*			3 zero value BC on surface
*	int mbx1		left boundary position
*	int mby1		top boundary position
*	int mbx2		right boundary postion 
*	int mby2		bottom boundary position
*	
*************************************************************************
* Output: float **u1	wavefield after applying the boundary condition 
*
*************************************************************************
* Reference: Clayton, R., and Engquist, B., 1980, Absorbing side boundary
*		condition for wave equation migration: Geophysics, vol. 45,
*		p. 895-904.
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	absorb2(int it, int  nx,  int  nz, float dx, 
		float dz, float dt, 
		float **u, float **u1, float **cc, float **rho, 
		int isurf, int mbx1, int mbx2, int mbz1, int mbz2, 
		int side, int tb)
{
  int	i, j;
  float	k, dtdz=dt/dz, dtdx=dt/dx;
  float 	*vell, *velr, *velt, *velb;

  vell = alloc1float(nz);
  velr = alloc1float(nz);
  velt = alloc1float(nx);
  velb = alloc1float(nx);

  /*  compute velocity at the boundary  */
  boundary_vel(cc, rho, vell, velr, velt, velb, 
	       nx, nz, mbx1, mbz1, mbx2, mbz2);

  if (tb) {
    for (i=mbx1+2; i<mbx2-2; i++)
      {
	k=-dtdz*velt[i];
	if (isurf == 1)  /* absorbing surface boundary condition */
	  {
	    u1[i][mbz1]=u[i][mbz1]-k*(u[i][mbz1+1]-u[i][mbz1]);
	    u1[i][mbz1+1]=u[i][mbz1+1]-k*(u[i][mbz1+2]-u[i][mbz1+1]);
	  }
	else if (isurf == 2)  /* zero gradient surface BC */
	  {
	    u1[i][mbz1]=u1[i][mbz1+1];
	    u1[i][mbz1+1]=u[i][mbz1+1]-k*(u[i][mbz1+2]-u[i][mbz1+1]);
	  }	
	else if (isurf == 3)  /* zero value surface BC */
	  {
	    u1[i][mbz1]=0.0;
	    u1[i][mbz1+1]=u[i][mbz1+1]-k*(u[i][mbz1+2]-u[i][mbz1+1]);
	  }	
	/*	absorbing bottom boundary condition	*/	
	k=dtdz*velb[i];
	u1[i][mbz2-1]=u[i][mbz2-1]-k*(u[i][mbz2-1]-u[i][mbz2-2]);
	u1[i][mbz2-2]=u[i][mbz2-2]-k*(u[i][mbz2-2]-u[i][mbz2-3]);
      }
  }
	
  /*	absorbing side boundary condition	*/
  if (side) {	
    for (j=mbz1; j<mbz2; j++)
      {
	k=dtdx*velr[j];
	u1[mbx2-1][j]=u[mbx2-1][j]-k*(u[mbx2-1][j]-u[mbx2-2][j]);	
	u1[mbx2-2][j]=u[mbx2-2][j]-k*(u[mbx2-2][j]-u[mbx2-3][j]);	
	k=-dtdx*vell[j];
	u1[mbx1][j]=u[mbx1][j]-k*(u[mbx1+1][j]-u[mbx1][j]);
	u1[mbx1+1][j]=u[mbx1+1][j]-k*(u[mbx1+2][j]-u[mbx1+1][j]);
      }
  }	

  free1float(vell);
  free1float(velr);
  free1float(velt);
  free1float(velb);
	
}

/************************************************************************
* boundary_vel -- function to compute the velocity at the boundary
*************************************************************************
*************************************************************************
* Input: 
*	int nx		number of grids in x direction 
*	int nz		number of grids in z direction
*	float **cc	elastic parameter profile
*	float **rho	density reverse
*
*	int mbx1		left boundary position
*	int mby1		top boundary position
*	int mbx2		right boundary postion 
*	int mby2		bottom boundary position
*	
*************************************************************************
* Output: 
*	float *vell	velocity at the left boundary 
*	float *velr	velocity at the right boundary 
*	float *velt	velocity at the top boundary 
*	float *velb	velocity at the bottom boundary 
*
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	boundary_vel(float **cc, float **rho, 
		     float *vell, float *velr, float *velt, float *velb, 
		     int nx, int nz, int mbx1, int mbz1, int mbx2, int mbz2)
{
  int 	i, k;

  /* velocity at side boundary */
  /* note:  here rho = 1/rho  */
  for (k=0; k<nz; ++k) {
    vell[k] = sqrt(cc[mbx1][k]*rho[mbx1][k]);
    velr[k] = sqrt(cc[mbx2-1][k]*rho[mbx2-1][k]);
  }

  /* velocity at top and bottom boundary */
  for (i=0; i<nx; ++i) {
    velt[i] = sqrt(cc[i][mbz1]*rho[i][mbz1]);
    velb[i] = sqrt(cc[i][mbz2-1]*rho[i][mbz2-1]);
  }
}




/********************************************************************
* fct2d1o - 2-D FCT correction for the FIRST-ORDER wave equation:
********************************************************************
* Note: second order accuracy in space
********************************************************************
* Input:
*
*	for the 2nd-order wave equation
*	float **u	solution at previous time level
*	float **u1	solution at next time level
*
*	float dx	spacing in x-direction
*	float dz	spacing in z-direction
*	int	mbx1	sample start in x-direction
*	int	mbx2	sample end in x-direction
*	int	mbz1	sample start in z-direction
*	int	mbz2	sample end in z-direction
*	float eta0	diffusion coefficient
*	float deta0dx	gradient of eta0 in x-direction
*	float deta0dz	gradient of eta0 in z-direction
*	float eta	anti-diffusion coefficient
*	float detadx	gradient of eta in x-direction
*	float detadz	gradient of eta in z-direction
*
********************************************************************
* Output:
*
*	for 2nd-order wave equation
*	float **u1	solution after the FCT correction
*
*	Working Arrays 
*	f0[][]	
*	f1[][]
*	 
********************************************************************
* References:
*	The detailed algorithm description is given in the article
*	"Elimination of dispersion in finite-difference modeling 
*	and migration"	in CWP-137, project review, page 155-174.
*
*	Original reference to FCT method:
*	Boris, J., and Book, D., 1973, Flux-corrected transport. I.
*	SHASTA, a fluid transport algorithm that works: 
*	Journal of Computational Physics, vol. 11, p. 38-69.
*
********************************************************************
* Author: Tong Fei, Colorado School of Mines, 1993.
********************************************************************/
void	fct2d1o(float **u, float **u1, int nx, int nz, 
		float eta0, float eta, float deta0dx, float deta0dz, 
		float detadx, float detadz, float dx, float dz, 
		int mbx1, int mbx2, int mbz1, int mbz2, 
		float **f0, float **f1)
{
  int	ix, iz;
  float	sf, eta00, eta11, min;
  float	deta0dxdx=deta0dx*dx, deta0dzdz=deta0dz*dz, 
    detadxdx=detadx*dx, detadzdz=detadz*dz;
	
  /*  diffusive fluxes from first differences of u[][] and u1[][] */
  for (ix=mbx1; ix<mbx2-1; ix++)	
    for (iz=mbz1; iz<mbz2-1; iz++)
      {
	eta00=eta0*(1.0+deta0dxdx*(ix-1)+deta0dzdz*(iz-1));
	eta11=eta*(1.0+detadxdx*(ix-1)+detadzdz*(iz-1));
	f0[ix][iz]=eta00*(u[ix+1][iz]-u[ix][iz]);
	f1[ix][iz]=eta11*(u1[ix+1][iz]-u1[ix][iz]);
      }
  for (ix=mbx1; ix<mbx2-1; ix++)	
    {
      eta00=eta0*(1.0+deta0dxdx*(ix-1)+deta0dzdz*(mbz2-1));
      eta11=eta*(1.0+detadxdx*(ix-1)+detadzdz*(mbz2-1));
      f0[ix][mbz2-1]=eta00*(u[ix+1][mbz2-1]-u[ix][mbz2-1]);
      f1[ix][mbz2-1]=eta11*(u1[ix+1][mbz2-1]-u1[ix][mbz2-1]);
    }

  /*  diffusive fluxes from first differences of u1[][]  */

  /* diffuse the first transported u[][] using saved first differences */
  for (ix=mbx1+1; ix<mbx2-1; ix++)
    for (iz=mbz1+1; iz<mbz2-1; iz++)
      u1[ix][iz]=u1[ix][iz]+(f0[ix][iz]-f0[ix-1][iz]);

  /*  take first differences of the diffused u[][]  */
  for (ix=mbx1; ix<mbx2-1; ix++)
    for (iz=mbz1; iz<mbz2-1; iz++)
      {
	f0[ix][iz]=u1[ix+1][iz]-u1[ix][iz];
      }
  for (ix=mbx1; ix<mbx2-1; ix++)
    f0[ix][mbz2-1]=u1[ix+1][mbz2-1]-u1[ix][mbz2-1];
			

  /*  limit the antidiffusive fluxes  */

  for (ix=mbx1+1; ix<mbx2-2; ix++)
    for (iz=mbz1; iz<mbz2; iz++)
      {
	/*		sf=SIGN(f1[ix][iz]);
			f1[ix][iz]=sf*MAX(0.0, MIN(MIN(sf*f0[ix-1][iz], 
			sf*f1[ix][iz]), sf*f0[ix+1][iz]));
	*/
	if (f1[ix][iz] > 0) {
	  sf=1.0;
	} else {
	  sf=-1.0;
	}

	if (sf*f0[ix-1][iz] > sf*f1[ix][iz]) {
	  min = sf*f1[ix][iz];
	} else {
	  min = sf*f0[ix-1][iz];
	}

	if (min > sf*f0[ix+1][iz]) {
	  min = sf*f0[ix+1][iz];
	} 
			
	if (min > 0.0) {
	  f1[ix][iz]=sf*min;
	} else {
	  f1[ix][iz]=0.0;
	}
      }
		  
  for (iz=mbz1; iz<mbz2; iz++)
    {
      /*	sf=SIGN(f1[mbx1][iz]);
		f1[mbx1][iz]=sf*MAX(0.0, MIN(sf*f1[mbx1][iz], 
		sf*f0[mbx1+1][iz])); 
		sf=SIGN(f1[mbx2-2][iz]);
		f1[mbx2-2][iz]=sf*MAX(0.0, MIN(sf*f0[mbx2-3][iz], 
		sf*f1[mbx2-2][iz]));  
      */
      if (f1[mbx1][iz] > 0.0) {
	sf=1.0;
      } else {
	sf=-1.0;
      }

      if (sf*f1[mbx1][iz] > sf*f0[mbx1+1][iz]) {
	min = sf*f0[mbx1+1][iz];
      } else {
	min = sf*f1[mbx1][iz];
      }

      if (min > 0.0) {
	f1[mbx1][iz]=sf*min;
      } else {
	f1[mbx1][iz]=0.0;
      }

      if (f1[mbx2-2][iz] > 0.0) {
	sf=1.0;
      } else {
	sf=-1.0;
      }

      if (sf*f0[mbx2-3][iz] > sf*f1[mbx2-2][iz]) {
	min = sf*f1[mbx2-2][iz];
      } else {
	min = sf*f0[mbx2-3][iz];
      } 

      if (min > 0.0) {
	f1[mbx2-2][iz]=sf*min;
      } else {
	f1[mbx2-2][iz]=0.0;
      }
    }
	
			  
  /*  antidiffuse with the limited fluxes	  */
  for (ix=mbx1+1; ix<mbx2-1; ix++)
    for (iz=mbz1+1; iz<mbz2-1; iz++)
      u1[ix][iz]=u1[ix][iz]-(f1[ix][iz]-f1[ix-1][iz]);

  /*  diffusive fluxes from first differences of u[][] and u1[][] */
  for (ix=mbx1; ix<mbx2-1; ix++)	
    for (iz=mbz1; iz<mbz2-1; iz++)
      {
	eta00=eta0*(1.0+deta0dxdx*(ix-1)+deta0dzdz*(iz-1));
	eta11=eta*(1.0+detadxdx*(ix-1)+detadzdz*(iz-1));
	f0[ix][iz]=eta00*(u[ix][iz+1]-u[ix][iz]);
	f1[ix][iz]=eta11*(u1[ix][iz+1]-u1[ix][iz]);
      }
  for (iz=mbz1; iz<mbz2-1; iz++)
    {	
      eta00=eta0*(1.0+deta0dxdx*(mbx2-1)+deta0dzdz*(mbz2-1));
      eta11=eta*(1.0+detadxdx*(mbx2-1)+detadzdz*(mbz2-1));
      f0[mbx2-1][iz]=eta00*(u[mbx2-1][iz+1]-u[mbx2-1][iz]);
      f1[mbx2-1][iz]=eta11*(u1[mbx2-1][iz+1]-u1[mbx2-1][iz]);
    }	

  /*  diffusive fluxes from first differences of u1[][]  */

  /* diffuse the first transported u[][] using saved first differences */
  for (ix=mbx1+1; ix<mbx2-1; ix++)
    for (iz=mbz1+1; iz<mbz2-1; iz++)
      u1[ix][iz]=u1[ix][iz]+(f0[ix][iz]-f0[ix][iz-1]);

  /*  take first differences of the diffused u[][]  */
  for (ix=mbx1; ix<mbx2-1; ix++)
    for (iz=mbz1; iz<mbz2-1; iz++)
      {
	f0[ix][iz]=u1[ix][iz+1]-u1[ix][iz]; 
      }
  for (iz=mbx1; iz<mbz2-1; iz++)
    f0[mbx2-1][iz]=u1[mbx2-1][iz+1]-u1[mbx2-1][iz]; 
			

  /*  limit the antidiffusive fluxes  */

	
  for (ix=mbx1; ix<mbx2; ix++)
    for (iz=mbz1+1; iz<mbz2-2; iz++)
      {
	/*		sf=SIGN(f1[ix][iz]);
			f1[ix][iz]=sf*MAX(0.0, MIN(MIN(sf*f0[ix][iz-1], 
			sf*f1[ix][iz]), sf*f0[ix][iz+1]));
	*/
	if (f1[ix][iz] > 0) {
	  sf=1.0;
	} else {
	  sf=-1.0;
	}

	if (sf*f0[ix][iz-1] > sf*f1[ix][iz]) {
	  min = sf*f1[ix][iz];
	} else {
	  min = sf*f0[ix][iz-1];
	}

	if (min > sf*f0[ix][iz+1]) {
	  min = sf*f0[ix][iz+1];
	} 
			
	if (min > 0.0) {
	  f1[ix][iz]=sf*min;
	} else {
	  f1[ix][iz]=0.0;
	}

      }
		
  for (ix=mbx1; ix<mbx2; ix++)
    {
      /*	sf=SIGN(f1[ix][mbz1]);
		f1[ix][mbz1]=sf*MAX(0.0, MIN(sf*f1[ix][mbz1], 
		sf*f0[ix][mbz1+1])); 
		sf=SIGN(f1[ix][mbz2-2]);
		f1[ix][mbz2-2]=sf*MAX(0.0, MIN(sf*f0[ix][mbz2-3], 
		sf*f1[ix][mbz2-2])); 
      */
      if (f1[ix][mbz1] > 0.0) {
	sf=1.0;
      } else {
	sf=-1.0;
      }

      if (sf*f1[ix][mbz1] > sf*f0[ix][mbz1+1]) {
	min = sf*f0[ix][mbz1+1];
      } else {
	min = sf*f1[ix][mbz1];
      }

      if (min > 0.0) {
	f1[ix][mbz1]=sf*min;
      } else {
	f1[ix][mbz1]=0.0;
      }

      if (f1[ix][mbz2-2] > 0.0) {
	sf=1.0;
      } else {
	sf=-1.0;
      }

      if (sf*f0[ix][mbz2-3] > sf*f1[ix][mbz2-2]) {
	min = sf*f1[ix][mbz2-2];
      } else {
	min = sf*f0[ix][mbz2-3];
      } 

      if (min > 0.0) {
	f1[ix][mbz2-2]=sf*min;
      } else {
	f1[ix][mbz2-2]=0.0;
      }
 
    }
			  
  /*  antidiffuse with the limited fluxes	  */
  for (ix=mbx1+1; ix<mbx2-1; ix++)
    for (iz=mbz1+1; iz<mbz2-1; iz++)
      u1[ix][iz]=u1[ix][iz]-(f1[ix][iz]-f1[ix][iz-1]);

}

/************************************************************************
* tforce_ricker -- Compute the	time response of a source function as
*	a Ricker wavelet with peak frequency "fpeak" Hz.	
*************************************************************************
*************************************************************************
* Input: 
*	int nt		number of time step
*	float dt 	time step
*	float fpeak	peak frequency of the Ricker wavelet
*	
*************************************************************************
* Output: float *tforce		source array
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void tforce_ricker(int nt, float *tforce, float dt, float fpeak)
{
  int	it;
  float	t1, t0; 

  t0=1.0/fpeak;

  for (it=0; it<nt; it++) {
    t1=it*dt;
    tforce[it] = exp(-PI*PI*fpeak*fpeak*(t1-t0)*(t1-t0))*
      (1.0-2.*PI*PI*fpeak*fpeak*(t1-t0)*(t1-t0));
  }
}

/************************************************************************
* tforce_akb -- Compute the	time response of a source function as
*	a wavelet based on a wavelet used by Alford, Kelly, and Boore.
*************************************************************************
*************************************************************************
* Input: 
*	int nt		number of time step
*	float dt 	time step
*	float fpeak	peak frequency of the wavelet
*	
*************************************************************************
* Output: float *tforce		source array
*
*************************************************************************
* Reference:
*	Alford, R., Kelly, K., and Boore, D., 1974,
*	Accuracy of finite-difference modeling of the acoustic wave
*	 equation: Geophysics, vol. 39, p. 834-842.
*
* The waveform here differs from the one in Alford, Kelly, and Boore in
* that there is a time shift and an arbitrary amplitude scaling factor of 60.
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void tforce_akb(int nt, float *tforce, float dt, float fpeak)
{
  int	it;
  float	t1; 
  float 	t0=1.8/fpeak;

  for (it=0; it<nt; it++) {
    t1=it*dt;
    tforce[it] = -60.0*(t1-t0)*exp(-2.0*fpeak*fpeak
				   *(t1-t0)*(t1-t0));
  }
}

/************************************************************************
* tforce_spike -- Compute the	time response of a source function as
*	a spike.	
*************************************************************************
*************************************************************************
* Input: 
*	int nt		number of time step
*	float dt 	time step
*	float fpeak	(not used) peak frequency
*	
*************************************************************************
* Output: float *tforce		source array
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void tforce_spike(int nt, float *tforce, float dt, float fpeak)
{
  int	it;
  float	t1;

  for (it=0; it<nt; it++) {
    t1=it*dt;
    tforce[it] = 0.0;
  }

  tforce[1]=1.0;
}


/************************************************************************
* tforce_unit -- Compute the	time response of a source function as
*	a constant unit shift.	
*************************************************************************
*************************************************************************
* Input: 
*	int nt		number of time step
*	float dt 	time step
*	float fpeak	(not used) peak frequency 
*	
*************************************************************************
* Output: float *tforce		source array
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void tforce_unit(int nt, float *tforce, float dt, float fpeak)
{
  int	it;
  float	t1;

  for (it=0; it<nt; it++) {
    t1=it*dt;
    tforce[it] = 1.0;
  }

}


/************************************************************************
* locate_source --	specify the source location	
*			user can change to give many source locations
*************************************************************************
*************************************************************************
* Input: 
*	int nx		number of grids in x direction 
*	int nz		number of grids in z direction
*	int sx		single source x location
*	int sz		single source z location
*	int source 	1 for single source
*			2 many sources 
*	
*************************************************************************
* Output: float **xzsource	source positions array
*
*************************************************************************
* Note: 
*	User may modify this function to re-arrange the source locations
*	for their purpose
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	locate_source(int nx, int nz, int sx, int sz, 
		      float **xzsource, int source)
{
  int	ix, iz;

  for (ix=0; ix<nx; ix++)
    for (iz=0; iz<nz; iz++)
      { 
	xzsource[ix][iz]=0.0;
      }

  if( source==1 ) 
    xzsource[sx][sz]=1.0;

  if( source==2 ) {
    for(ix=0; ix<nx/14; ix++)
      xzsource[ix][nz/3]=1.0;
    for(ix=nx/14; ix<nx*17/70; ix++)
      xzsource[ix][(int)(nz/3+tan(PI/4.)*(ix-nx/14.))]=1.0;
    for(ix=17*nx/70; ix<80*nx/80; ix++)
      xzsource[ix][22*nz/30]=1.0;
    /*		for(ix=nx*63/80; ix<nx*15/16; ix++)
		xzsource[ix][(int)(nz*22/30-tan(PI/4.)
		*(ix-nx*63/80.))]=1.0;
		for(ix=15*nx/16; ix<nx; ix++)
		xzsource[ix][nz/3]=1.0;
    */	}

  if( source==3 ) {
    for(ix=0; ix<nx/14; ix++)
      xzsource[ix][nz/3]=1.0;
    for(ix=nx/14; ix<nx*34/70; ix++)
      xzsource[ix][(int)(nz/3+tan(PI/18.)*(ix-nx/14.))]=1.0;
    for(ix=17*nx/70; ix<80*nx/80; ix++)
      xzsource[ix][22*nz/30]=1.0;
  }
}



/************************************************************************
* moving_bc -- Computes the boundary containing the wavefield.
*		Finite difference computations are performed only inside
*		this boundary.
*************************************************************************
* Input: 
*	int it		time index
*	int nx		number of x values
*	int nz		number of z values
*	int sx		source x location
* 	int sz 		source z location
*	float dx	x increment
*	float dz	z increment
*	int impulse	1 if single source  
*	int movebc 	1 for moving boundary, 0 no moving boundary
*	float *t 	time array
*	float vmax	maximum velocity
*	
*************************************************************************
* Output: 	int *mbx1	right boundary
*		int *mbx2	left boundary
*		int *mbz1	top boundary
*		int *mbz2	bottom boundary
* These delimit the boundary within which the wavefield is computed.
*************************************************************************
* Notes: the moving boundary is used to improve the efficiency of the
*	modeling or migration, by constraining the area over which
*	the finite-differencing is computed.
* Caveat: this improves computational speed for forward modeling of
*		i.e. generating snapshots, this does not save much time
*		for the migration.
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	moving_bc (int it, int nx, int nz, int sx, int sz, 
		   float dx, float dz, int impulse, int movebc, 
		   float *t, float vmax, int *mbx1, int *mbz1, int *mbx2, int *mbz2)
{
  float	temp, dzdx=dz/dx;

  vmax *= 1.5;

  temp=vmax*t[it]/dz;

  if (movebc) {
    if (impulse) {
      if (0 > sz - temp - 0.04*nz) {
	/*endring foer: mbz1=0*/
	*mbz1=0;
      } else {
	*mbz1 = sz - temp - 0.04*nz;
      }

      if (nz > sz+temp+0.04*nz) {
	*mbz2=sz+temp+0.04*nz;
      } else {
	*mbz2=nz;
      }

      if (0 > sx - temp*dzdx - 0.04*nx) {
	*mbx1=0;
      } else {
	*mbx1 = sx - temp*dzdx - 0.04*nx;
      } 

      if (nx > sx + temp*dzdx + 0.04*nx) {
	*mbx2=sx + temp*dzdx + 0.04*nx; 
      } else {
	*mbx2 = nx;
      }

    } else {
      /*endring mbz1=0*/
      *mbz1=0;
      if (nz > temp+0.04*nz) {
	*mbz2=temp+0.04*nz;
      } else {
	*mbz2=nz;
      }

      *mbx1=0;
      *mbx2=nx;
    }

  } else {
    *mbx1=0;
    *mbx2=nx;
    *mbz1=0;
    *mbz2=nz;
  }

}


/************************************************************************
* moving_fctbc -- Computes the boundary containing the area where do
*	       the FCT correction.
*	       FCT correction are performed only inside
*	       this boundary.
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
* 	  int nxcc1	right boundary for doing FCT
*	  int nxcc2	left boundary for doing FCT
*	  int nzcc1	top boundary for doing FCT
*	  int nzcc2	bottom boundary for doing FCT
*	
*************************************************************************
* Output: int *fctxbeg	right boundary
*	  int *fctxend	left boundary
*	  int *fctzbeg	top boundary
*	  int *fctzend	bottom boundary
* These delimit the boundary within which the FCT is applied.
*************************************************************************
* Notes: the moving boundary is used to improve the efficiency of the
*       modeling or migration, by constraining the area over which
*       the finite-differencing is computed.
* Caveat: this improves computational speed for forward modeling of
*	  i.e. generating snapshot 
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	moving_fctbc (int mbx1, int mbz1, 
		      int mbx2, int mbz2, 
		      int nxcc1, int nzcc1, 
		      int nxcc2, int nzcc2, 
		      int *fctxbeg, int *fctzbeg, 
		      int *fctxend, int *fctzend)
{
  /* FCT correction is localized to the area bounded by */
  /* (fctxend - fctxbeg) by (fctzend-fctzbeg) */
  /* contain the FCT correction boundary by the model boundary */
  if (mbx1 > nxcc1) {  /* left boundary */
    *fctxbeg=mbx1;
  } else {
    *fctxbeg=nxcc1;
  }
  if (mbx2 < nxcc2) {  /* right boundary */
    *fctxend=mbx2;
  } else {
    *fctxend=nxcc2;
  }
  if (mbz1 > nzcc1) {  /* top boundary */
    *fctzbeg=mbz1;
  } else {
    *fctzbeg=nzcc1;
  }
  if (mbz2 < nzcc2) {  /* bottom boundary */
    *fctzend=mbz2;
  } else {
    *fctzend=nzcc2;
  }
}




/************************************************************************
* strain2_x --  use 2nd-order FD to compute the derivative in x-direction
*		(the strain tensor e11 from input wavefield). 
*		e11 has the size (nx-1)*nz
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dx	spatial step in x-direction 	
*	  float **a	x-component wavefield;  size nx*nz
*	
*************************************************************************
* Output: float **da	derivative field (tensor e11)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain2_x(int mbx1, int mbx2, int mbz1, int mbz2, 
		  float dx, float **a, float **da)
{
  int 	i, k;
  int	rdx = 1.0/dx;

  for (i=mbx1; i < mbx2-1; i++)
    for (k=mbz1; k < mbz2; k++)
      {
	da[i][k]=rdx*(a[i+1][k]-a[i][k]);
      }

}

/************************************************************************
* strain2_z --  use 2nd-order FD to compute the derivative in z-direction
*		(the strain tensor e33 from input wavefield). 
*		e33 has the size (nx-1)*nz
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dx	spatial step in x-direction 	
*	  float **a	z-component wavefield;  size (nx-1)*(nz-1)
*	
*************************************************************************
* Output: float **da	derivative field (tensor e33)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain2_z(int mbx1, int mbx2, int mbz1, int mbz2, 
		  float dz, float **a, float **da)
{
  int 	i, k;
  int	rdz = 1.0/dz;

  for (i=mbx1; i < mbx2-1; i++)
    for (k=mbz1+1; k < mbz2-1; k++)
      {
	da[i][k]=rdz*(a[i][k]-a[i][k-1]);
      }

}

/************************************************************************
* strain2_xy --  use 2nd-order FD to compute the derivative in x-direction
*		(the strain tensor e12 from input wavefield). 
*		e12 has the size nx*(nz-1)
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dx	spatial step in x-direction 	
*	  float **ay	y-component wavefield;  size (nx-1)*(nz-1)
*	
*************************************************************************
* Output: float **da	derivative field (tensor e12)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain2_xy(int mbx1, int mbx2, int mbz1, int mbz2, 
		   float dx, float **ay, float **da)
{
  int 	i, k;
  int	rdx = 0.5/dx;

  for (i=mbx1+1; i < mbx2-1; i++)
    for (k=mbz1; k < mbz2-1; k++)
      {
	da[i][k]=rdx*(ay[i][k]-ay[i-1][k]);
      }

}

/************************************************************************
* strain2_xz --  use 2nd-order FD to compute the derivative in z-direction
*		(the strain tensor e13 from input wavefield). 
*		e13 has the size nx*(nz-1)
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dx	spatial step in x-direction 	
*	  float dz	spatial step in z-direction 	
*	  float **ax	x-component wavefield;  size nx*nz
*	  float **az	z-component wavefield;  size (nx-1)*(nz-1)
*	
*************************************************************************
* Output: float **da	derivative field (tensor e13)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain2_xz(int mbx1, int mbx2,  int mbz1, int mbz2, 
		   float dx, float dz, float **ax, float **az, float **da)
{
  int 	i, k;
  int	rdx = 0.5/dx, rdz=0.5/dz;

  for (i=mbx1+1; i < mbx2-1; i++)
    for (k=mbz1; k < mbz2-1; k++)
      {
	da[i][k]=rdx*(az[i][k]-az[i-1][k])
	  +rdz*(ax[i][k+1]-ax[i][k]);
      }

}


/************************************************************************
* strain2_yz --  use 2nd-order FD to compute the derivative in z-direction
*		(the strain tensor e23 from input wavefield). 
*		e23 has the size (nx-1)*nz
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dz	spatial step in z-direction 	
*	  float **ay	y-componentwavefield;  size (nx-1)*(nz-1)
*	
*************************************************************************
* Output: float **da	derivative field (tensor e23)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain2_yz(int mbx1, int mbx2,  int mbz1, int mbz2, 
		   float dz, float **ay,  float **da)
{
  int 	i, k;
  int	rdz=0.5/dz;

  for (i=mbx1; i < mbx2-1; i++)
    for (k=mbz1+1; k < mbz2-1; k++)
      {
	da[i][k]=rdz*(ay[i][k]-ay[i][k-1]);
      }
}



/************************************************************************
* strain4_x --  use 2nd-order FD to compute the derivative in x-direction
*		(the strain tensor e11 from input wavefield). 
*		e11 has the size (nx-1)*nz
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dx	spatial step in x-direction 	
*	  float **a	x-component wavefield;  size nx*nz
*	
*************************************************************************
* Output: float **da	derivative field (tensor e11)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain4_x(int mbx1, int mbx2, int mbz1, int mbz2, 
		  float dx, float **a, float **da)
{
  int 	i, k;
  int	rdx = 1.0/dx;
  float	twelveth = 1.0/24.0;
  float	eighth = 9.0/8.0;

  for (i=mbx1+1; i < mbx2-2; i++)
    for (k=mbz1; k < mbz2; k++)
      {
	da[i][k]=rdx*(eighth*(a[i+1][k]-a[i][k])
		      -twelveth*(a[i+2][k]-a[i-1][k]));
      }

  for (k=mbz1; k < mbz2; k++)
    {
      da[mbx1][k]=rdx*(a[mbx1+1][k]-a[mbx1][k]);
      da[mbx2-2][k]=rdx*(a[mbx2-1][k]-a[mbx2-2][k]);
    }
}


/************************************************************************
* strain4_z --  use 2nd-order FD to compute the derivative in z-direction
*		(the strain tensor e33 from input wavefield). 
*		e33 has the size (nx-1)*nz
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dx	spatial step in x-direction 	
*	  float **a	z-component wavefield;  size (nx-1)*(nz-1)
*	
*************************************************************************
* Output: float **da	derivative field (tensor e33)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain4_z(int mbx1, int mbx2, int mbz1, int mbz2, 
		  float dz, float **a, float **da)
{
  int 	i, k;
  int	rdz = 1.0/dz;
  float	twelveth = 1.0/24.0;
  float	eighth = 9.0/8.0;

  for (i=mbx1; i < mbx2-1; i++)
    for (k=mbz1+2; k < mbz2-2; k++)
      {
	da[i][k]=rdz*(eighth*(a[i][k]-a[i][k-1])
		      -twelveth*(a[i][k+1]-a[i][k-2]));
      }

  for (i=mbx1; i < mbx2-1; i++)
    {
      da[i][mbz1+1]=rdz*(a[i][mbz1+1]-a[i][mbz1]);
      da[i][mbz2-2]=rdz*(a[i][mbz2-2]-a[i][mbz2-3]);
    }

}

/************************************************************************
* strain4_xy --  use 2nd-order FD to compute the derivative in x-direction
*		(the strain tensor e12 from input wavefield). 
*		e12 has the size nx*(nz-1)
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dx	spatial step in x-direction 	
*	  float **ay	y-component wavefield;  size (nx-1)*(nz-1)
*	
*************************************************************************
* Output: float **da	derivative field (tensor e12)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain4_xy(int mbx1, int mbx2, int mbz1, int mbz2, 
		   float dx, float **ay, float **da)
{
  int 	i, k;
  int	rdx = 0.5/dx;
  float	twelveth = 1.0/24.0;
  float	eighth = 9.0/8.0;

  for (i=mbx1+2; i < mbx2-2; i++)
    for (k=mbz1; k < mbz2-1; k++)
      {
	da[i][k]=rdx*(eighth*(ay[i][k]-ay[i-1][k])
		      -twelveth*(ay[i+1][k]-ay[i-2][k]));
      }

  for (k=mbz1; k < mbz2-1; k++)
    {
      da[mbx1+1][k]=rdx*(ay[mbx1+1][k]-ay[mbx1][k]);
      da[mbx2-2][k]=rdx*(ay[mbx2-2][k]-ay[mbx2-3][k]);
    }

}

/************************************************************************
* strain4_xz --  use 2nd-order FD to compute the derivative in z-direction
*		(the strain tensor e13 from input wavefield). 
*		e13 has the size nx*(nz-1)
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dx	spatial step in x-direction 	
*	  float dz	spatial step in z-direction 	
*	  float **ax	x-component wavefield;  size nx*nz
*	  float **az	z-component wavefield;  size (nx-1)*(nz-1)
*	
*************************************************************************
* Output: float **da	derivative field (tensor e13)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain4_xz(int mbx1, int mbx2, int mbz1, int mbz2, 
		   float dx, float dz, float **ax, float **az, float **da)
{
  int 	i, k;
  int	rdx = 0.5/dx, rdz=0.5/dz;
  float	twelveth = 1.0/24.0;
  float	eighth = 9.0/8.0;

  for (i=mbx1+2; i < mbx2-2; i++)
    for (k=mbz1+1; k < mbz2-2; k++)
      {
	da[i][k]=rdx*(eighth*(az[i][k]-az[i-1][k])
		      -twelveth*(az[i+1][k]-az[i-2][k]))
	  +rdz*(eighth*(ax[i][k+1]-ax[i][k])
		-twelveth*(ax[i][k+2]-ax[i][k-1]));
      }

  for (i=mbx1+1; i < mbx2-1; i++)
    {
      da[i][mbz1]=rdx*(az[i][mbz1]-az[i-1][mbz1])
	+rdz*(ax[i][mbz1+1]-ax[i][mbz1]);
      da[i][mbz2-2]=rdx*(az[i][mbz2-2]-az[i-1][mbz2-2])
	+rdz*(ax[i][mbz2-1]-ax[i][mbz2-2]);
    }

  for (k=mbz1+1; k < mbz2-2; k++)
    {
      da[mbx1+1][k]=rdx*(az[mbx1+1][k]-az[mbx1][k])
	+rdz*(ax[mbx1+1][k+1]-ax[mbx1+1][k]);
      da[mbx2-2][k]=rdx*(az[mbx2-2][k]-az[mbx2-3][k])
	+rdz*(ax[mbx2-2][k+1]-ax[mbx2-2][k]);
    }

}

/************************************************************************
* strain4_yz --  use 2nd-order FD to compute the derivative in z-direction
*		(the strain tensor e23 from input wavefield). 
*		e23 has the size (nx-1)*nz
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  float dz	spatial step in z-direction 	
*	  float **ay	y-componentwavefield;  size (nx-1)*(nz-1)
*	
*************************************************************************
* Output: float **da	derivative field (tensor e23)
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	strain4_yz(int mbx1, int mbx2, int mbz1, int mbz2, 
		   float dz, float **ay, float **da)
{
  int 	i, k;
  int	rdz=0.5/dz;
  float	twelveth = 1.0/24.0;
  float	eighth = 9.0/8.0;

  for (i=mbx1; i < mbx2-1; i++)
    for (k=mbz1+2; k < mbz2-2; k++)
      {
	da[i][k]=rdz*(eighth*(ay[i][k]-ay[i][k-1])
		      -twelveth*(ay[i][k+1]-ay[i][k-2]));
      }

  for (i=mbx1; i < mbx2-1; i++)
    {
      da[i][mbz1+1]=rdz*(ay[i][mbz1+1]-ay[i][mbz1]);
      da[i][mbz2-2]=rdz*(ay[i][mbz2-2]-ay[i][mbz2-3]);
    }

}


/************************************************************************
* difference --  use 2nd-order FD in time to wavefield in time,
*		also include the source function  
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  int it	time index
*	  float **u	solution at previous time level
*	  float *f	time response of the source function
*	  float **xzsource	source location
*	  float dt	time step 	
*	  float rdxdz	1/(dx*dz)
*	
*************************************************************************
* Output: float  **u1	solution at next time level
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	difference(int mbx1, int mbx2, int mbz1, int mbz2, int it, 
		   float **u1, float **u, 
		   float **xzsource, float *f, float dt, float rdxdz) 
{
  int	i, k;

  for (i=mbx1+1; i < mbx2-1; i++)
    for (k=mbz1+1; k < mbz2-1; k++)
      {
	u1[i][k]=u[i][k]+f[it]*xzsource[i][k]*dt*rdxdz;
      }
}

/************************************************************************
* difference_2x --  2nd-order FD on staggered grid to approximate 
*		1st-order derivative (compute the contribution of 
*		strain tensor to the wavefield).
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  int shift	spatial shift
*	  float **e	component of strain tensor
*	  float **c	elastic parameter
*	  float dtdx	dt/dx 	
*	
*************************************************************************
* Output: float  **u	updated solution at next time level
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	difference_2x(int mbx1, int mbx2, int mbz1, int mbz2, int shift, 
		      float dtdx, float **u, float **e, float **c) 
{
  int	i, k;

  /*  finite-difference to compute the contribution of e to u */
  /*  for derivative in x-direction */
  for (i=mbx1+1; i < mbx2-1; i++)
    for (k=mbz1+1; k < mbz2-1; k++)
      {
	u[i][k]
	  +=dtdx*c[i][k]*(e[i+1+shift][k]
			  -e[i+shift][k]);
      }
}

/************************************************************************
* difference_2z --  2nd-order FD on staggered grid to approximate 
*		1st-order derivative (compute the contribution of 
*		strain tensor to the wavefield).
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  int shift	spatial shift
*	  float **e	component of strain tensor
*	  float **c	elastic parameter
*	  float dtdz	dt/dz 	
*	
*************************************************************************
* Output: float  **u	updated solution at next time level
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	difference_2z(int mbx1, int mbx2, int mbz1, int mbz2, int shift, 
		      float dtdz, float **u, float **e, float **c)
{ 
  int	i, k;

  /*  finite-difference to compute the contribution of e to u */
  /*  for derivative in z-direction */
  for (i=mbx1+1; i < mbx2-1; i++)
    for (k=mbz1+1; k < mbz2-1; k++)
      {
	u[i][k] 
	  +=dtdz*c[i][k]*(e[i][k+1+shift]
			  -e[i][k+shift]);
      }
}


/************************************************************************
* difference_4x --  4th-order FD on staggered grid to approximate 
*		1st-order derivative (compute the contribution of 
*		strain tensor to the wavefield).
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  int shift	spatial shift
*	  float **e	component of strain tensor
*	  float **c	elastic parameter
*	  float dtdx	dt/dx 	
*	
*************************************************************************
* Output: float  **u	updated solution at next time level
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	difference_4x(int mbx1, int mbx2, int mbz1, int mbz2, int shift, 
		      float dtdx, float **u, float **e, float **c) 
{
  int	i, k;
  float	twelveth = 1.0/24.0;
  float	eighth = 9.0/8.0;

  /*  finite-difference to compute the contribution of e to u */
  /*  for derivative in x-direction */
  for (i=mbx1+2; i < mbx2-2; i++)
    for (k=mbz1+2; k < mbz2-2; k++)
      {
	u[i][k]
	  +=dtdx*c[i][k]*(eighth*(e[i+1+shift][k]-e[i+shift][k])
			  -twelveth*(e[i+2+shift][k]-e[i-1+shift][k]));
      }
}

/************************************************************************
* difference_4z --  4th-order FD on staggered grid to approximate 
*		1st-order derivative (compute the contribution of 
*		strain tensor to the wavefield).
*************************************************************************
*************************************************************************
* Input: 
* 	  int mbx1	right boundary containing the wavefield
*	  int mbx2	left boundary containing the wavefield
*	  int mbz1	top boundary containing the wavefield
*	  int mbz2	bottom boundary containing the wavefield
*	  int shift	spatial shift
*	  float **e	component of strain tensor
*	  float **c	elastic parameter
*	  float dtdz	dt/dz 	
*	
*************************************************************************
* Output: float  **u	updated solution at next time level
*
*************************************************************************
*************************************************************************
* Author: Tong Fei, 1993, Colorado School of Mines.
*************************************************************************/
void	difference_4z(int mbx1, int mbx2, int mbz1, int mbz2, int shift, 
		      float dtdz, float **u, float **e, float **c)
{ 
  int	i, k;
  float	twelveth = 1.0/24.0;
  float	eighth = 9.0/8.0;

  /*  finite-difference to compute the contribution of e to u */
  /*  for derivative in z-direction */
  for (i=mbx1+2; i < mbx2-2; i++)
    for (k=mbz1+2; k < mbz2-2; k++)
      {
	u[i][k] 
	  +=dtdz*c[i][k]*(eighth*(e[i][k+1+shift]-e[i][k+shift])
			  -twelveth*(e[i][k+2+shift]-e[i][k-1+shift]));
      }
}

/************************************************************************
* su_output --  Given seismic data this rutine will create output with 
*               the proper SU header set.
*************************************************************************
*************************************************************************
* Input: 
* nx - number of gridpoints in x-direction
* dx - gridsize x - direction
* sx - source x-coord
* sdepth - source depth
* ns - number of timesteps
* dt - steplength in time
* outputfile - file to contain output with SU header set
* data - the data to be stored
************************************************************************
*  type of data: 
*     (1) horizontal seismograms 	
*     (2) vertical seismograms (nx -> nz)
*************************************************************************
*************************************************************************
* Author: Stig-Kyrre Foss, CWP 2001
*************************************************************************/
void su_output(int nx, int sx, int sdepth, int ns, float dx, 
	       float dt, FILE *outputfile, float **data)
{ 
  segy sudata;
  int	tracl, tracr, ix, it;


  /* Setting su-data */
  sudata.trid   = 1; /* Data is seismic data (id-code)*/
  sudata.sx     = sx;
  sudata.sdepth = sdepth;
  sudata.ns     = ns;
  sudata.dt     = (short)(1000000*dt);
  sudata.d2     = dx;

  /*Initializing trace counters*/	
  tracl=tracr=0;
	
  for (ix=0; ix<nx; ++ix)
    {
      ++tracr;
      ++tracl;
      sudata.offset = ix*dx - sx;
      sudata.tracl = tracl;
      sudata.tracr = tracr;

      for (it=0; it<ns; ++it)
	{
	  sudata.data[it] = data[ix][it];
	}
	    
      fputtr(outputfile,&sudata);
    }
}
	
