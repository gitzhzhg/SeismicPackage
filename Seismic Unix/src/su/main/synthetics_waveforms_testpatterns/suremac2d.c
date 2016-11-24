/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                */

/* SUREMAC2D: $Revision: 1.7 $ ; $Date: 2015/08/11 22:52:55 $         */

/* 
  Acoustic 2D Fourier method modeling with REM time integration
*/

#include <stdio.h>
#include <fcntl.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                        ",
" SUREMAC2D - Acoustic 2D Fourier method modeling with high accuracy     ",
"             Rapid Expansion Method (REM) time integration              ",
"                                                                        ",
" suremac2d [parameters]                                                 ",
"                                                                        ",
" Required parameters:                                                   ",
"                                                                        ",
" opflag=     0: variable density wave equation                          ",
"             1: constant density wave equation                          ",
"             2: non-reflecting wave equation                            ",
"                                                                        ",
" nx=         number of grid points in horizontal direction              ",
" nz=         number of grid points in vertical direction                ",
" nt=         number of time samples                                     ",
" dx=         spatial increment in horizontal direction                  ",
" dz=         spatial increment in vertical direction                    ",
" dt=         time sample interval in seconds                            ",
" isx=        grid point # of horizontal source positions                ",
" isz=        grid point # of vertical source positions                  ",
"                                                                        ",
" Optional parameters:                                                   ",
" fx=0.0      first horizontal coordinate                                ",
" fz=0.0      first vertical coordinate                                  ",
" irx=        horizontal grid point # of vertical receiver lines         ",
" irz=        vertical grid point # of horizontal receiver lines         ",
" w=0.1       width of spatial source distribution (see notes)           ",
" sflag=2     source time function                                       ",
"             0: user supplied source function                           ",
"             1: impulse (spike at t=0)                                  ",
"             2: Ricker wavelet                                          ",
" fmax=       maximum frequency of Ricker (default) wavelet              ",
" amps=1.0    amplitudes of sources                                      ",
" prec=0      1: precompute Bessel coefficients b_k (see notes)          ",
"             2: use precomputed Bessel coefficients b_k                 ",
" fsflag=0    1: perform run with free surface b.c.                      ",
" vmaxu=      user-defined maximum velocity                              ",
" dtsnap=0.0  time interval in seconds of wave field snapshots           ",
" iabso=1     apply absorbing boundary conditions (0: none)              ",
" abso=0.1    damping parameter for absorbing boundaries                 ",
" nbwx=20     horizontal width of absorbing boundary                     ",
" nbwz=20     vertical width of absorbing boundary                       ",
" verbose=0   1: show parameters used                                    ",
"             2: print maximum amplitude at every expansion term         ",
"                                                                        ",
" velfile=vel          velocity filename                                 ",
" densfile=dens        density filename                                  ",
" sname=wavelet.su     user supplied source time function filename       ",
" sepxname=sectx.su    x-direction pressure sections filename            ",
" sepzname=sectz.su    z-direction pressure sections filename            ",
" snpname=snap.su      pressure snapshot filename                        ",
" jpfile=stderr        diagnostic output                                 ",
"                                                                        ",
" Notes:                                                                 ",
"  0. The combination of the Fourier method with REM time integration    ",
"     allows the computation of synthetic seismograms which are free     ",
"     of numerical grid dispersion. REM has no restriction on the        ",
"     time step size dt. The Fourier method requires at least two        ",
"     grid points per shortest wavelength.                               ",
"  1. nx and nz must be valid numbers for pfafft transform lengths.      ",
"     nx and nz must be odd numbers (unless opflag=1). For valid         ",
"     numbers see e.g. numbers in structure 'nctab' in source file       ",
"     $CWPROOT/src/cwp/lib/pfafft.c.                                     ",
"  2. Velocities (and densities) are stored as plain C style files       ",
"     of floats where the fast dimension is along the z-direction.       ",
"  3. Units must be consistent, e.g. m, s and m/s.                       ",
"  4. A 20 grid points wide border at the sides and the bottom of        ",
"     the modeling grid is used for sponge boundary conditions           ",
"     (default: iabso=1).                                                ",
"     Source and receiver lines should be placed some (e.g. 10) grid     ",
"     points away from the absorbing boundaries in order to reduce       ",
"     reflections due to obliquely incident wavefronts.                  ",
"  5. Dominant frequency is about fmax/2 (sflag=2), absolute maximum     ",
"     is delayed by 3/fmax from beginning of wavelet.                    ",
"  6. If opflag!=1 the source should be not a spike in space; the        ",
"     parameter w determines at which distance (in grid points) from     ",
"     the source's center the Gaussian weight decays to 10 percent       ",
"     of its maximum. w=2 may be a reasonable choice; however, the       ",
"     waveform will be distorted.                                        ",
"  7. Horizontal and vertical receiver line sections are written to      ",
"     separate files. Each file can hold more than one line.             ",
"  8. Parameter vmaxu may need to be chosen larger than the highest      ",
"     propagation velocity if the modeling run becomes unstable.         ",
"     This happens if the largest eigenvalue of the modeling             ",
"     operator L is larger than estimated from the largest velocity      ",
"     due to variations of the density.                                  ",
"     In particular if using the variable density acoustic wave          ",
"     equation the eigenvalues depend also on the density and it is      ",
"     impossible to estimated the largest eigenvalue analytically.       ",
"  9. Bessel coefficients can be precomputed (prec=1) and stored on      ",
"     disk to save CPU time when several shots need to be run.           ",
"     In this case computation of Bessel coefficients can be skipped     ",
"     and read from disk file for reuse (prec=2).                        ",
"     For reuse of Bessel coefficients the user may need to define       ",
"     the overall maximum velocity (vmaxu).                              ",
" 10. If snapshots are not required, a spike source (sflag=1) may be     ",
"     applied and the resulting impulse response seismograms can be      ",
"     convolved later with a desired wavelet.                            ",
" 11. The free surface (fsflag=1) does not coincide with the first       ",
"     vertical grid index (0). It appears to be half a grid spacing      ",
"     above that position.                                               ",
"                                                                        ",
NULL};

/*
  Acoustic 2D Fourier method modeling with REM time integration

  Reference: 
  Kosloff, D., Fihlo A.Q., Tessmer, E. and Behle, A., 1989,
    Numerical solution of the acoustic and elastic wave equations by a
    new rapid expansion method, Geophysical Prospecting, 37, 383-394
  
 * Credits:
 *      University of Hamburg: Ekkehart Tessmer, October 2012
 */
/**************** end self doc ***********************************/

FILE *snp, *sepx, *sepz, *vel, *dens, *jpfp, *sfp, *fpbes;

segy tri, tro, sno;

int main(int argc, char *argv[])
{
  int i, k, l, il, it; /* loop indices */
  int indx, indz;
  int opflag;  /* selection of wave equation type */
  int sflag;   /* source function selection (Ricker/spike) */
  int fsflag;  /* flag for modeling with free surface b.c. */
  int prec;    /* flag for precomputing Bessel coefficients */
  int m;       /* number of Bessel coefficients computed */
  int m0;
  int m2;      /* number of Bessel coefficients used in expansion */
  int nx;      /* number of gridpoints in horizontal direction */
  int nz;      /* number of gridpoints in vertical direction */
  int nzpad;   /* number of gridpoints in vertical direction with zero-padding */
  int nt;      /* number of time samples */
  int nt0;
  int nbwx;    /* width of absorbing boundary in horizontal direction */
  int nbwz;    /* width of absorbing boundary in vertical direction */
  int nsnap;   /* number of snapshots */
  int nsnap0;
  int nsectx;  /* number of horizontal sections */
  int nsectz;  /* number of vertical sections */
  int nsourc;  /* number of sources */
  int iabso;   /* flag for using absorbing boundaries */
  int nwav=0;  /* number of samples for user source function */
  int verbose;
  int *irx=NULL; /* horizontal gridpoint indices of vertical receiver lines */
  int *irz=NULL; /* vertical gridpoint indices of horizontal receiver lines */
  int *isx;    /* horizontal gridpoint indices of sources */
  int *isz;    /* vertical gridpoint indices of sources */
  float abso;  /* damping value for absorbing boundaries */
  float dx;    /* grid spacing in horizontal direction */
  float dz;    /* grid spacing in vertical direction */
  float dt;    /* time step size or sample rate */
  float dt0;   
  float dtsnap; /* time increment between snapshots */
  float dtsnap0;
  float dtwave;/* time interval of user supplied wavelet (sflag=0) */
  float fx;    /* first horizontal coordinate */
  float fz;    /* first vertical coordinate */
  float t;     /* time */
  float tmax;  /* maximum time */
  float w;     /* width of source's spatial Gaussian distribution */
  float fmax;  /* maximum frequency of source time function */
  float vmax;  /* maximum subsurface velocity */
  float vmax0;
  float vmin;  /* minimum subsurface velocity */
  float vmaxu; /* user defined maximum velocity */
  float r;     /* largest eigenvalue of modeling operator L */
  float r0;
  float r2;    /* r^2 */
  float amx,amn;   /* maximum amplitude in numerical grid */
  float pi;
  float tmp;   /* temporary storage */
  float *amps;     /* amplitudes of sources */
  float *bwx;      /* weighting function values in absorbing zone (hor.) */
  float *bwz;      /* weighting function values in absorbing zone (vert.) */
  float *wave=NULL; /* user source function array */
  float **a1;       /* pressure wave field */
  float **a2;       /* pressure wave field */
  float **c2rho;    /* velocity structure c, c*c or c*c*rho (see opflag) */
  float **rhoinv=NULL; /* inverse of density */
  float **aux1;        /* auxiliary array */
  float **aux2=NULL;   /* auxiliary array */
  float **gbox=NULL;   /* Gaussian source box */
  float ***sectx=NULL; /* time sections along horizontal direction */
  float ***sectz=NULL; /* time sections along vertical direction */
  float ***snap=NULL;  /* snapshots */
  double **bessn, **bestr, *btemp; /* Bessel coefficients */
  char* jpfile;        /* file for information output */
  cwp_String velfile;  /* velocity filename */
  cwp_String densfile; /* density filename */
  cwp_String sname;    /* source function filename */
  cwp_String sepxname; /* x-direction pressure sections filename */
  cwp_String sepzname; /* z-direction pressure sections filename */
  cwp_String snpname;  /* pressure snapshot filename */
  struct rusage cput;
  long user_bk, user_s_bk=0, user_u_bk=0, user_mod, user_s_mod=0, user_u_mod=0; 

  /* function prototypes */
  void stru(int nx, int nz, float **c2rho, float **rhoinv, int opflag, float *vmin, float *vmax);
  void difx(float **vin, float **vout, float **bulk, int nx, int nz, float dx, int iadd);
  void difz(float **vin, float **vout, float **bulk, int nx, int nz, int nzpad, float dz, int iadd, int iload);
  void difx2(float **vin, float **vout, float **bulk, int nx, int nz, float dx, int iadd);
  void difz2(float **vin, float **vout, float **bulk, int nx, int nz, int nzpad, float dz, int iadd);
  void bessel_jn(double x, int n, double *bes);
  void lintgr(float t, float r, float dt, int nt, int m, 
              float *wave, double *sum);
  float fwave(float t, float fmax);
  void damp(int nbw, float abso, float *bw);
  void tbc2d(float *a, float *bwx, float *bwz, int nbwx, int nbwz,
	     int nx, int nz);
  void g2d(float w, int nx, int nz, float **g);

  /* defaults */
#define FX         0.0
#define FZ         0.0
#define IABSO      1
#define ABSO       0.1
#define NBWX      20
#define NBWZ      20
#define DTSNAP     0.0
#define SFLAG      2
#define NXB       21
#define NZB       21
#define W          0.1
#define PREC       0
#define FSFLAG     0

  initargs(argc, argv);
  requestdoc(1);

  /* get parameters */
  if (!getparint ("opflag" , &opflag)) err("opflag required!\n");
  if (!getparint ("nx" , &nx)) err("nx required!\n");
  if (!getparint ("nz" , &nz)) err("nz required!\n");
  if (!getparint ("nt" , &nt)) err("nt required!\n");
  if (!getparfloat ("dx" , &dx)) err("dx required!\n");
  if (!getparfloat ("dz" , &dz)) err("dz required!\n");
  if (!getparfloat ("dt" , &dt)) err("dt required!\n");
  
  nsourc = countparval("isx");
  if (nsourc != countparval("isz"))
    err("isx and isz must have the same number of coordinates.\n");
  isx = ealloc1int(nsourc);
  isz = ealloc1int(nsourc);
  amps = ealloc1float(nsourc);
  for (l=0; l<nsourc; l++) amps[l] = 1.;
  if (!getparint ("isx" , isx)) err("isx required!\n");
  if (!getparint ("isz" , isz)) err("isz required!\n");
  
  /* get optional parameters */
  if (!getparfloat ("dtsnap" , &dtsnap)) dtsnap = DTSNAP;
  if (!getparint ("sflag" , &sflag)) sflag = SFLAG;
  if (!getparfloat ("fmax" , &fmax) && sflag==2) 
    err("fmax required if Ricker wavelet selected!\n");
  if (!getparint ("prec" , &prec)) prec = PREC;
  if (!getparint ("fsflag" , &fsflag)) fsflag = FSFLAG;
  nsectx = countparval("irz");
  if (nsectx) {
    irz = ealloc1int(nsectx);
    getparint("irz", irz);
  }
  nsectz = countparval("irx");
  if (nsectz) {
    irx = ealloc1int(nsectz);
    getparint("irx", irx);
  }
  if (!getparint ("iabso" , &iabso)) iabso = IABSO;
  if (!getparfloat ("abso" , &abso)) abso = ABSO;
  if (!getparint ("verbose" , &verbose)) verbose=0;
  if (!getparint ("nbwx" , &nbwx)) nbwx = NBWX;
  if (!getparint ("nbwz" , &nbwz)) nbwz = NBWZ;
  if (!getparfloat ("w" , &w)) w = W;
  if (!getparfloat ("fx" , &fx)) fx = FX;
  if (!getparfloat ("fz" , &fz)) fz = FZ;
  if (!getparfloat ("vmaxu" , &vmaxu)) vmaxu = 0.;
  getparfloat("amps", amps);

  if (!getparstring("jpfile",&jpfile)) {
    jpfp = stderr;
  } else { 
    jpfp = fopen(jpfile,"w");
  }

  if (!getparstring("velfile",&velfile)) velfile = "vel"; 
  if (!getparstring("densfile",&densfile)) densfile = "dens"; 
  if (!getparstring("snpname",&snpname)) snpname = "snap.su"; 
  if (!getparstring("sepxname",&sepxname)) sepxname = "sectx.su"; 
  if (!getparstring("sepzname",&sepzname)) sepzname = "sectz.su"; 
  if (!getparstring("sname",&sname)) sname = "wavelet.su"; 

  nzpad = nz;
  if (fsflag == 1) {
    nzpad = npfa((int)(nz * 1.5));
    if (opflag == 1) {
      nzpad = npfa(nzpad);
    } else {
      while (!(nzpad % 2)) nzpad = npfa(++nzpad); /* force nzpad odd */
    }
  }
  
  if (verbose) {
    fprintf(jpfp,"opflag=%d\n",opflag);
    fprintf(jpfp,"nx=%d  dx=%f\n",nx,dx);
    fprintf(jpfp,"nz=%d  dz=%f\n",nz,dz);
    fprintf(jpfp,"fsflag=%d\n",fsflag);
    if (fsflag == 1) fprintf(jpfp,"nzpad=%d\n",nzpad);
    fprintf(jpfp,"nt=%d  dt=%f\n",nt,dt);
    fprintf(jpfp,"fx=%f  fz=%f\n",fx,fz);
    fprintf(jpfp,"w=%f\n",w);
    if (nsourc == 1) 
      fprintf(jpfp,"isx=%d  isz=%d\n",isx[0],isz[0]);
    else {
      fprintf(jpfp,"sources: nsourc=%d\n",nsourc);
      fprintf(jpfp,"  coordinates  amplitudes\n");
      for (il=0; il<nsourc; il++)
	fprintf(jpfp,"  (%4d,%4d)  %e\n",isx[il],isz[il],amps[il]);
    }

    fprintf(jpfp,"sflag=%d\n",sflag);
    fprintf(jpfp,"fmax=%e\n",fmax);
    fprintf(jpfp,"prec=%d\n",prec);

    if (nsectx) {
      fprintf(jpfp,"x-receiver lines: nsectx=%d\n",nsectx);
      fprintf(jpfp,"  z-coordinates:");
      for (il=0; il<nsectx; il++) fprintf(jpfp,"  %d",irz[il]);
      fprintf(jpfp,"\n");
    }
    if (nsectz) {
      fprintf(jpfp,"z-receiver lines: nsectx=%d\n",nsectx);
      fprintf(jpfp,"  x-coordinates:");
      for (il=0; il<nsectz; il++) fprintf(jpfp,"  %d",irx[il]);
      fprintf(jpfp,"\n");
    }
    if (vmaxu > 0.) fprintf(jpfp,"vmaxu=%f\n",vmaxu);
  }

  if (npfa(nx) != nx) err("Error: Invalid nx !\n");
  if (! nx % 2 && opflag != 1) err("Error: nx must be odd!\n");
  if (npfa(nz) != nz) err("Error: Invalid nz !");
  if (! nz % 2 && opflag != 1) err("Error: nz must be odd!\n");

  for (l=0; l<nsourc; l++) {
    if (isx[l] - (NXB+1)/2 < 0 && isx[l] + (NXB+1)/2 > nx-1) {
      err("Error: (%d) source box beyond model's boundary in x-direction!\n",l);
    }
    if (isz[l] - (NZB+1)/2 < 0 && isz[l] + (NZB+1)/2 > nz-1) {
      err("Error: (%d) source box beyond model's boundary in z-direction!\n",l);
    }
  }

  tmax = nt * dt;
  if (dtsnap < 0.) err("stopped: dtsnap must be >= 0!\n");
  if (dtsnap > 0.)
    nsnap = NINT(tmax / dtsnap);
  else
    nsnap = 0;

  if (verbose) {
    if (nsectx) fprintf(jpfp,"sepxname=%s\n",sepxname);
    if (nsectz) fprintf(jpfp,"sepzname=%s\n",sepzname);
    fprintf(jpfp,"nsnap=%d\n",nsnap);
    if (nsnap) {
      fprintf(jpfp,"dtsnap=%e\n",dtsnap);
      fprintf(jpfp,"snpname=%s\n",snpname);
    }
  }
  
  if (prec == 2) 
    fprintf(jpfp,"sflag and fmax are ignored if precomputed Bessel coefficients are used (prec=2).\n");

  if (nt < 1) err("stopped: nt must be > 0!\n");

  bwx    = ealloc1float(nbwx);
  bwz    = ealloc1float(nbwz);
  a1     = ealloc2float(nx,nz);
  a2     = ealloc2float(nx,nz);
  c2rho  = ealloc2float(nx,nz);
  aux1   = ealloc2float(nx,nzpad);
  gbox   = ealloc2float(NXB,NZB);

  if (opflag != 1) aux2 = ealloc2float(nx,nzpad);
  if (opflag == 0) rhoinv = ealloc2float(nx,nz);
  if (nsnap > 0) snap = ealloc3float(nx,nz,nsnap);
  if (nsectx > 0) sectx = ealloc3float(nx,nt,nsectx);
  if (nsectz > 0) sectz = ealloc3float(nz,nt,nsectz);

  dtwave = 0.;
  if (sflag == 0) {
    if (!(sfp = fopen(sname,"r")))
      err("Could not open user wavelet file\n");
    fgettr(sfp,&tri);
    if (tri.ns == 0)
      err("user wavelet: tri.ns must be > 0!\n");
    if (tri.dt == 0)
      err("user wavelet: tri.dt must be > 0!\n");
    nwav = tri.ns;
    dtwave = tri.dt * 1.e-6;
    wave = ealloc1float(nwav);
    for (it=0; it<nwav; it++) wave[it] = tri.data[it];
    efclose(sfp);
    fprintf(jpfp,"nwav=%5d\n",nwav);
  }

  if (sflag == 2) {
    dtwave = dt;
    nwav = 8./(dt*fmax);
    wave = ealloc1float(nwav);
    for (it=0; it<nwav; it++) wave[it] = fwave((it+1)*dt,fmax);
  }

  /* read velocity (and density) file */
  if (!(vel = fopen(velfile,"r")))
    err("Could not open velocity file\n");
  if (opflag == 0) {
    if (!(dens = fopen(densfile,"r")))
      err("Could not open density file\n");
  }
  stru(nx,nz,c2rho,rhoinv,opflag,&vmin,&vmax);

  /* change absorbing zone for no reflections at top */
  if (iabso) {
    for (k=0; k<nbwz; k++) {
      for (i=0; i<nx; i++) c2rho[nz-1-k][i] = c2rho[0][i];
      if (opflag == 0)
	for (i=0; i<nx; i++) rhoinv[nz-1-k][i] = rhoinv[0][i];
    }
  }

  efclose(vel);
  if (opflag == 0) efclose(dens);

  if (verbose) fprintf(jpfp,"max., min. velocities: %e, %e\n",vmax,vmin);
  
  /* setup aborbing boundaries */
  if (iabso) {
    damp(nbwx,abso,bwx);
    damp(nbwz,abso,bwz);
  }

  if (vmax == 0.) err("stopped: vmax is zero!\n");
  
  if ((dx > dz ? dx : dz) > vmin/(2.*fmax))
    err("stopped: fmax is too high for spatial sampling!\n");
  
  /* prepare Bessel coefficients convolved with wavelet */
  if (vmaxu > 0.) {
    if (vmax > vmaxu) err("vmaxu is smaller than vmax!\n");
    vmax = vmaxu; 
  }
  /* largest EV of operator L */
  pi = 4.*atan(1.);
  r = 1.1 * pi * vmax * sqrt(1./(dx*dx) + 1./(dz*dz));
  r2 = r*r;

  m = tmax * r; /* highest index of terms in expansion */
  m2 = m/2; /* number of expansion terms */

  if (verbose) fprintf(jpfp,"r=%e  tmax*r=%e  m=%d\n",r,tmax*r,m);
  bessn = ealloc2double(m2,nsnap);
  bestr = ealloc2double(m2,nt);
  btemp = ealloc1double(m+50);

  /* Bessel-coefficients */
  if (verbose) {
    getrusage(RUSAGE_SELF, &cput);
    user_s_bk = cput.ru_utime.tv_sec;
    user_u_bk = cput.ru_utime.tv_usec;
  }

  switch(prec) {
  case 0:
  case 1:
    if (verbose) fprintf(jpfp,"computing Bessel coefficients ...");
    for (it=0; it<nsnap; it++) {
      t = (it+1)*dtsnap;
      if (sflag == 1) {
	bessel_jn(t*r,m+50,btemp);
	for (l=0; l<m2; l++) bessn[it][l] = btemp[2*l+1];
      } else {
	lintgr(t,r,dtwave,nwav,m,wave,bessn[it]);
      }
    }

    for (it=0; it<nt; it++) {
      t = (it+1)*dt;
      if (sflag == 1) {
	bessel_jn(t*r,m+50,btemp);
	for (l=0; l<m2; l++) bestr[it][l] = btemp[2*l+1];
      } else {
	lintgr(t,r,dtwave,nwav,m,wave,bestr[it]);
      }
    }

    for (k=0; k<m2; k++) {
      amx=0.; amn=0.;
      for (it=0; it<nt; it++) {
	amx = (amx > bestr[it][k] ? amx : bestr[it][k]);
	amn = (amn < bestr[it][k] ? amn : bestr[it][k]);
      }
    }

    if (verbose) fprintf(jpfp," done.\n");
    if (prec == 0) break;
    fpbes = efopen("b_k","w");
    efwrite(&vmax,sizeof(float),1,fpbes);
    efwrite(&r,sizeof(float),1,fpbes);
    efwrite(&m,sizeof(int),1,fpbes);
    efwrite(&nsnap,sizeof(int),1,fpbes);
    efwrite(&dtsnap,sizeof(float),1,fpbes);
    efwrite(&nt,sizeof(int),1,fpbes);
    efwrite(&dt,sizeof(float),1,fpbes);
    for (it=0; it<nsnap; it++) efwrite(bessn[it],sizeof(double),m2,fpbes);
    for (it=0; it<nt; it++) efwrite(bestr[it],sizeof(double),m2,fpbes);
    fclose(fpbes);
    break;
  case 2:
    if (!(fpbes = fopen("b_k","r")))
      err("Could not open Bessel coefficients file (b_k)!\n",
	  "Coefficients must be precomputed (prec=1).\n");
    efread(&vmax0,sizeof(float),1,fpbes);
    efread(&r0,sizeof(float),1,fpbes);
    efread(&m0,sizeof(int),1,fpbes);
    efread(&nsnap0,sizeof(int),1,fpbes);
    efread(&dtsnap0,sizeof(float),1,fpbes);
    efread(&nt0,sizeof(int),1,fpbes);
    efread(&dt0,sizeof(float),1,fpbes);
    if (vmax != vmax0 || r != r0 || m != m0 || nsnap != nsnap0 ||
	dtsnap != dtsnap0 || nt != nt0 || dt != dt0 )
      err("Wrong set of Bessel coefficients in file b_k!\n");
    for (it=0; it<nsnap; it++) efread(bessn[it],sizeof(double),m2,fpbes);
    for (it=0; it<nt; it++) efread(bestr[it],sizeof(double),m2,fpbes);
    fclose(fpbes);
  }

  if (verbose) {
    getrusage(RUSAGE_SELF, &cput);
    user_bk  = (cput.ru_utime.tv_sec  - user_s_bk) * 1000000;
    user_bk += (cput.ru_utime.tv_usec - user_u_bk);

    user_s_mod = cput.ru_utime.tv_sec;
    user_u_mod = cput.ru_utime.tv_usec;
    fprintf(jpfp,"CPU time for Bessel coefficients: %.3f seconds\n",
	    (float)user_bk/1000000.);
  }

  if (verbose) fprintf(jpfp,"starting modeling.\n");

  for (k=0; k<nz; k++) {
    for (i=0; i<nx; i++) a1[k][i] = a2[k][i] = 0.;
  }

  /* Gaussian source box */
  g2d(w, NXB, NZB, gbox);

  /* source distribution */
  for (l=0; l<nsourc; l++) {
    for (k=0; k<NZB; k++) {
      indz = isz[l]+k-NZB/2;
      for (i=0; i<NXB; i++) {
	indx = isx[l]+i-NXB/2;
	if (gbox[k][i] > 1.e-2) {
	  if (indx<0 || indx>nx-1 || indz<0 || indz>nz-1)
	    err("source box beyond grid boundary!\n");
	  a1[indz][indx] = gbox[k][i] * amps[l];
	}
      }
    }
  }

  /* initialize first two expansion terms Q_1 and Q_3 */
  switch(opflag) {
  case 0:
    difx(a1,aux1,rhoinv,nx,nz,dx,2);
    difx(aux1,aux1,NULL,nx,nz,dx,0);

    difz(a1,aux2,rhoinv,nx,nz,nzpad,dz,2,0);
    difz(aux2,aux1,NULL,nx,nz,nzpad,dz,1,1);
    
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	aux1[k][i] = c2rho[k][i]*aux1[k][i]/r2;
	a2[k][i] = 4.*aux1[k][i] + 3.*a1[k][i];
      }
    }
    break;
  case 1:
    difx2(a1,aux1,NULL,nx,nz,dx,0);
    difz2(a1,aux1,NULL,nx,nz,nzpad,dz,1);
    
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	aux1[k][i] = c2rho[k][i]*aux1[k][i]/r2;
	a2[k][i] = 4.*aux1[k][i] + 3.*a1[k][i];
      }
    }
    break;
  case 2:
    difx(a1,aux1,c2rho,nx,nz,dx,2);
    difx(aux1,aux1,NULL,nx,nz,dx,0);

    difz(a1,aux2,c2rho,nx,nz,nzpad,dz,2,0);
    difz(aux2,aux1,NULL,nx,nz,nzpad,dz,1,1);
    
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	aux1[k][i] = c2rho[k][i]*aux1[k][i]/r2;
	a2[k][i] = 4.*aux1[k][i] + 3.*a1[k][i];
      }
    }
    break;
  default:
    err("no such opflag!\n");
    break;
  } /* end switch */

  if (nsnap > 0) {
    for (it=0; it<nsnap; it++) { /* first two terms for snapshots */
      for (k=0; k<nz; k++) {
	for (i=0; i<nx; i++) {
	  snap[it][k][i] = 2.*a1[k][i]*bessn[it][0] + 2.*a2[k][i]*bessn[it][1];
	}
      }
    }
  }
  
  if (nsectx > 0) {
    for (il=0; il<nsectx; il++) {
      for (it=0; it<nt; it++) { /* first two terms for x-sections */
	for (i=0; i<nx; i++) {
	  sectx[il][it][i] = 2.*a1[irz[il]][i]*bestr[it][0] + 2.*a2[irz[il]][i]*bestr[it][1]; 
	}
      }
    }
  }
  
  if (nsectz > 0) {
    for (il=0; il<nsectz; il++) {
      for (it=0; it<nt; it++) { /* first two terms for z-sections */
	for (k=0; k<nz; k++) {
	  sectz[il][it][k] = 2.*a1[k][irx[il]]*bestr[it][0] + 2.*a2[k][irx[il]]*bestr[it][1]; 
	}
      }
    }
  }

  if (iabso) {
    tbc2d(a1[0],bwx,bwz,nbwx,nbwz,nx,nz);
    tbc2d(a2[0],bwx,bwz,nbwx,nbwz,nx,nz);
  }
  
  /* calculate all other odd expansion terms Q_2n+1 */
  for (l=2; l<m2; l++) {
    if (verbose == 2) {
      amx = 0.;
      for (k=0; k<nz; k++) {
	for (i=0; i<nx; i++) {
	  tmp = fabs(a1[k][i]);
	  amx = (amx > tmp ? amx : tmp);
	}
      }
      fprintf(jpfp,"l=%4d  max= %e\n",l,amx);
    }

    switch(opflag) {
    case 0:
      difx(a2,aux1,rhoinv,nx,nz,dx,2);
      difx(aux1,aux1,NULL,nx,nz,dx,0);

      difz(a2,aux2,rhoinv,nx,nz,nzpad,dz,2,0);
      difz(aux2,aux1,NULL,nx,nz,nzpad,dz,1,1);

      for (k=0; k<nz; k++) {
	for (i=0; i<nx; i++) {
	  aux1[k][i] = c2rho[k][i]*aux1[k][i]/r2;
	  aux1[k][i] = 4.*aux1[k][i] + 2.*a2[k][i] - a1[k][i];
	  
	  a1[k][i] = a2[k][i];
	  a2[k][i] = aux1[k][i];
	}
      }
      break;
    case 1:
      difx2(a2,aux1,NULL,nx,nz,dx,0);
      difz2(a2,aux1,NULL,nx,nz,nzpad,dz,1);

      for (k=0; k<nz; k++) {
	for (i=0; i<nx; i++) {
	  aux1[k][i] = c2rho[k][i]*aux1[k][i]/r2;
	  aux1[k][i] = 4.*aux1[k][i] + 2.*a2[k][i] - a1[k][i];
	  
	  a1[k][i] = a2[k][i];
	  a2[k][i] = aux1[k][i];
	}
      }
      break;
    case 2:
      difx(a2,aux1,c2rho,nx,nz,dx,2);
      difx(aux1,aux1,NULL,nx,nz,dx,0);

      difz(a2,aux2,c2rho,nx,nz,nzpad,dz,2,0);
      difz(aux2,aux1,NULL,nx,nz,nzpad,dz,1,1);

      for (k=0; k<nz; k++) {
	for (i=0; i<nx; i++) {
	  aux1[k][i] = c2rho[k][i]*aux1[k][i]/r2;
	  aux1[k][i] = 4.*aux1[k][i] + 2.*a2[k][i] - a1[k][i];
	  
	  a1[k][i] = a2[k][i];
	  a2[k][i] = aux1[k][i];
	}
      }
      break;
    default:
      err("no such opflag!\n");
      break;
    } /* end switch */

    if (nsnap > 0) {
      for (it=0; it<nsnap; it++) {
	for (k=0; k<nz; k++) { /* add term for snapshots */
	  for (i=0; i<nx; i++) {
	    snap[it][k][i] += 2.*a2[k][i]*bessn[it][l];
	  }
	}
      }
    }
      
    if (nsectx > 0) {	/* add term for x-sections */
      for (il=0; il<nsectx; il++) {
	for (it=0; it<nt; it++) {
	  for (i=0; i<nx; i++) {
	    sectx[il][it][i] += 2.*a2[irz[il]][i]*bestr[it][l];
	  }
	}
      }
    }
      
    if (nsectz > 0) { /* add term for z-sections */
      for (il=0; il<nsectz; il++) {
	for (it=0; it<nt; it++) {
	  for (k=0; k<nz; k++) {
	    sectz[il][it][k] += 2.*a2[k][irx[il]]*bestr[it][l];
	  }
	}
      }
    }

    if (iabso) {
      tbc2d(a1[0],bwx,bwz,nbwx,nbwz,nx,nz);
      tbc2d(a2[0],bwx,bwz,nbwx,nbwz,nx,nz);
    }

  } /* end loop over expansion terms */

  if (verbose) {
    getrusage(RUSAGE_SELF, &cput);
    user_mod  = (cput.ru_utime.tv_sec  - user_s_mod) * 1000000;
    user_mod += (cput.ru_utime.tv_usec - user_u_mod);
    if (verbose) fprintf(jpfp,"CPU time for modeling: %.3f seconds\n",
			 (float)user_mod/1000000.);
  }

  /* --- output results to files --- */
  /* common trace headers */
  tro.trid = 1;
  tro.scalco = -10;
  tro.scalel = -10;
  tro.sx = NINT((fx+isx[0]*dx)*10.);
  tro.sdepth = NINT((fz+isz[0]*dz)*10.);
  tro.ns = nt;
  tro.dt = NINT(dt*1000000.);
  tro.d2 = dx;

  /* write x-direction sections */
  if (nsectx) {
    sepx = efopen(sepxname,"w");
    for (il=0; il<nsectx; il++) {
      tro.fldr = il+1;
      for (i=0; i<nx; i++) {
	tro.tracl = i;
	tro.tracf = i;
	tro.gx = NINT((fx+i*dx)*10.);
	for (it=0; it<nt; it++) tro.data[it] = sectx[il][it][i];
	fvputtr(sepx,&tro);
      }
    }
    efclose(sepx);
  }

  /* write z-direction sections */
  if (nsectz) {
    sepz = efopen(sepzname,"w");
    for (il=0; il<nsectz; il++) {
      tro.fldr = il+1;
      for (k=0; k<nz; k++) {
	tro.tracl = k;
	tro.tracf = k;
	tro.gx = NINT((fz+k*dz)*10.);
	for (it=0; it<nt; it++) tro.data[it] = sectz[il][it][k];
	fvputtr(sepz,&tro);
      }
    }
    efclose(sepz);
  }

  /* write snapshots */
  if (nsnap) {
    snp = efopen(snpname,"w");
    sno.ns = nz;
    sno.scalco = -10;
    sno.trid = 130; /* depth range */
    sno.scalco = -10;
    sno.scalel = -10;
    sno.sx = NINT((fx+isx[0]*dx)*10.);
    sno.sdepth = NINT((fz+isz[0]*dz)*10.);
    sno.dt = NINT(dtsnap*1000000.);
    sno.d1 = dz;
    sno.d2 = dx;
    for (l=0; l<nsnap; l++) {
      sno.fldr = l+1;
      for (i=0; i<nx; i++) {
	sno.tracl = i;
	sno.tracf = i;
	sno.gx = NINT((fx+i*dx)*10.);
	sno.offset = NINT((sno.gx-sno.sx)/10.);
	for (k=0; k<nz; k++) sno.data[k] = snap[l][k][i];
	fvputtr(snp,&sno);
      }
    }
    efclose(snp);
  }

  if (verbose) fprintf(jpfp,"finished modeling.\n");
  efclose(jpfp);

  return(CWP_Exit());
}
/**********************************************************************/
void stru(int nx, int nz, float **c2rho, float **rhoinv, int opflag, float *vmin, float *vmax)
/*
  read structure velocities (and densities) file

  input:
    nx:      number of gridpoints in horizontal direction
    nz:      number of gridpoints in vertical direction
    opflag:  flag for wave equation type 

  output:
    c2rho:   array of c*c*rho or c*c or c values
    rhoinv:  array of 1/rho
    vmin:    minimum velocity of grid
    vmax:    maximum velocity of grid
*/
  {
  int i, k;
  float tmp;
  float *bufz;

  bufz = ealloc1float(nz);

  for (i=0; i<nx; i++) {
    efread(bufz,sizeof(float),nz,vel); 
    for (k=0; k<nz; k++) c2rho[k][i] = bufz[k];
  }

  switch(opflag) {
  case 0:
    /* variable density case */
    for (i=0; i<nx; i++) {
      efread(bufz,sizeof(float),nz,dens); 
      for (k=0; k<nz; k++) {
	rhoinv[k][i] = 1./bufz[k];
	c2rho[k][i] = c2rho[k][i]*c2rho[k][i]*bufz[k];
      }
    }
    break;
  case 1:
    /* constant density case */
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	c2rho[k][i] = c2rho[k][i]*c2rho[k][i];
      }
    }
    break;
  case 2:
    /* non-reflecting wave equation */
    break;
  }

  /* find maximum and minimum velocity */
  *vmax = 0.;
  *vmin = 1.e20;
  switch(opflag) {
  case 0:
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	tmp = sqrt(c2rho[k][i]*rhoinv[k][i]);
	*vmax = (tmp > *vmax ? tmp : *vmax);
	*vmin = (tmp < *vmin ? tmp : *vmin);
      }
    }
    break;
  case 1:
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	tmp = sqrt(c2rho[k][i]);
	*vmax = (tmp > *vmax ? tmp : *vmax);
	*vmin = (tmp < *vmin ? tmp : *vmin);
      }
    }
    break;
  case 2:
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	tmp = c2rho[k][i];
	*vmax = (tmp > *vmax ? tmp : *vmax);
	*vmin = (tmp < *vmin ? tmp : *vmin);
      }
    }
    break;
  }
  free1float(bufz);
}
/**********************************************************************/
void g2d(float w, int nx, int nz, float **g)
/*
  2-D Gaussian weights for spatial distribution of source 

  input:
    w:     width in grid points where Gaussian is one tenth of its maximum
    nx:    number of gridpoints in horizontal direction
    nz:    number of gridpoints in vertical direction

  output:
    g:     weights for spatial distribution of source
*/
{
  int i, i0, k, k0;
  float pi, sigma, fac, sum, r2, arg;

  pi = 4.*atan(1.);

  i0 = nx/2;
  k0 = nz/2;

  if (w < 0.1) w = 0.1; /* spatial spike */

  /* w is width where Gaussian is one tenth of its maximum */
  sigma = sqrt(-w*w/(2.*log(0.1)));

  fac = 1./(sigma*sqrt(2.*pi));

  sum = 0.;
  for (k=0; k<nz; k++) {
    for (i=0; i<nx; i++) {
      r2 = (i-i0)*(i-i0)+(k-k0)*(k-k0);
      arg = r2/(2.*sigma*sigma);
      g[k][i] = 0.;
      if (arg < 80.) g[k][i] = fac*exp(-arg);
      sum += g[k][i];
    }
  }
  /* normalization */
  for (k=0; k<nz; k++) {
    for (i=0; i<nx; i++) g[k][i] /= sum;
  }
}
/**********************************************************************/
void difx(float **vin, float **vout, float **bulk, int nx, int nz, float dx, 
	  int iadd)
/* 
   compute 1st derivative along x-direction 

   input:
     vin:    2D input array
     bulk:   2D array for multiplication with output array
     nx:     length of array in x-direction
     nz:     length of array in z-direction
     dx:     grid spacing in x-direction
     iadd:   flag

   output:
     vout:   2D output array

     iadd=0: set result in vout
     iadd=1: add result to vout
     iadd=2: set result multiplied by bulk in vout
     iadd=3: add result multiplied by bulk to vout
*/
   
{
  static int i, k;
  float  tmp;
  static float *a, *rkx;
  static int icall=0, ieo=0, m1=-1, p1=1 ;
  static int num;

  void rk(float *ak, int n, float d, int ind);
 
  if (icall == 0 ) {
    icall = 1;
    ieo = nz % 2;
    num = ((nz + 1) / 2) * 2;

    a   = ealloc1float(2*nx);
    rkx = ealloc1float(nx);
    rk(rkx,nx,dx,1);
  }

  for (k=0; k<num; k+=2) {
	
    /* Load data from vin */
    for (i=0; i<nx; i++) {
      a[2*i] = vin[k][i];
      if (!ieo || !(k==num-2)) {
	a[2*i+1] = vin[k+1][i];
      }
      else {
	a[2*i+1] = 0.;
      }
    }

    /* Perform derivative */
    pfacc(m1, nx, (complex*)a);
	
    for (i=0; i<nx; i++) {
      tmp      =  a[2*i]   * rkx[i];
      a[2*i]   = -a[2*i+1] * rkx[i];
      a[2*i+1] =  tmp;
    }
	
    pfacc(p1, nx, (complex*)a);

    if (iadd == 0) {

      /* Set data into vout */
      for (i=0; i<nx; i++) {
	vout[k][i] = a[2*i];
	if (!ieo || !(k==num-2)) vout[k+1][i] = a[2*i+1];
      }
    } else if (iadd == 1) {

      /* Add data into vout */
      for (i=0; i<nx; i++) {
	vout[k][i] += a[2*i];
	if (!ieo || !(k==num-2)) vout[k+1][i] += a[2*i+1];
      }
    } else if (iadd == 2) {

      /* Set data multiplied by bulk into vout */
      for (i=0; i<nx; i++) {
	vout[k][i] = a[2*i] * bulk[k][i];
	if (!ieo || !(k==num-2)) vout[k+1][i] = a[2*i+1] * bulk[k+1][i];
      }
    } else if (iadd == 3) {

      /* Add data multiplied by bulk into vout */
      for (i=0; i<nx; i++) {
	vout[k][i] += a[2*i] * bulk[k][i];;
	if (!ieo || !(k==num-2)) vout[k+1][i] += a[2*i+1] * bulk[k+1][i];
      }
    }
  }
}
/**********************************************************************/
void difx2(float **vin, float **vout, float **bulk, int nx, int nz, float dx, 
   int iadd)
/* 
   compute 2nd derivative along x-direction 

   input:
     vin:    2D input array
     bulk:   2D array for multiplication with output array
     nx:     length of array in x-direction
     nz:     length of array in z-direction
     dx:     grid spacing in x-direction
     iadd:   flag

   output:
     vout:   2D output array

   iadd=0: set result in vout
   iadd=1: add result to vout
   iadd=2: set result multiplied by bulk in vout
   iadd=3: add result multiplied by bulk to vout
*/

{
  static int i, k;
  static float *a, *rkx;
  static int icall=0, ieo=0, m1=-1, p1=1 ;
  static int num;

  void rk(float *ak, int n, float d, int ind);
 
  if (icall == 0 ) {
    icall = 1;
    ieo = nz % 2;
    num = ((nz + 1) / 2) * 2;

    a   = ealloc1float(2*nx);
    rkx = ealloc1float(nx);
    rk(rkx,nx,dx,2);
  }

  for (k=0; k<num; k+=2) {
	
    /* Load data from vin */
    for (i=0; i<nx; i++) {
      a[2*i] = vin[k][i];
      if (!ieo || !(k==num-2)) {
	a[2*i+1] = vin[k+1][i];
      }
      else {
	a[2*i+1] = 0.;
      }
    }

    /* Perform derivative */
    pfacc(m1, nx, (complex*)a);
	
    for (i=0; i<nx; i++) {
      a[2*i]   *= rkx[i];
      a[2*i+1] *= rkx[i];
    }
	
    pfacc(p1, nx, (complex*)a);

    if (iadd == 0) {

      /* Set data into vout */
      for (i=0; i<nx; i++) {
	vout[k][i] = a[2*i];
	if (!ieo || !(k==num-2)) vout[k+1][i] = a[2*i+1];
      }
    } else if (iadd == 1) {

      /* Add data into vout */
      for (i=0; i<nx; i++) {
	vout[k][i] += a[2*i];
	if (!ieo || !(k==num-2)) vout[k+1][i] += a[2*i+1];
      }
    } else if (iadd == 2) {

      /* Set data multiplied by bulk into vout */
      for (i=0; i<nx; i++) {
	vout[k][i] = a[2*i] * bulk[k][i];
	if (!ieo || !(k==num-2)) vout[k+1][i] = a[2*i+1] * bulk[k+1][i];
      }
    } else if (iadd == 3) {

      /* Add data multiplied by bulk into vout */
      for (i=0; i<nx; i++) {
	vout[k][i] += a[2*i] * bulk[k][i];;
	if (!ieo || !(k==num-2)) vout[k+1][i] += a[2*i+1] * bulk[k+1][i];
      }
    }
  }
}
/**********************************************************************/
void difz(float **vin, float **vout, float **bulk, int nx, int nz, int nzpad, float dz,
	  int iadd, int iload)
/* 
   compute 1st derivative along z-direction 

   input:
     vin:    2D input array
     bulk:   2D array for multiplication with output array
     nx:     length of array in x-direction
     nz:     length of array in z-direction
     dz:     grid spacing in z-direction
     iadd:   flag

   output:
     vout:   2D output array

   iadd=0: set result in vout
   iadd=1: add result to vout
   iadd=2: set result multiplied by bulk in vout
   iadd=3: add result multiplied by bulk to vout

   iload=0: load nz rows and zero-pad, unload nzpad rows
   iload=1: load nzpad rows, unload nz rows
*/

{
  static int i, k, nzunl;
  float  tmp;
  static float *a, *rkz;
  static int icall=0, ieo=0, m1=-1, p1=1 ;
  static int num;

  void rk(float *ak, int n, float d, int ind);

  if (icall == 0 ) {
    icall = 1;
    ieo = nx % 2;
    num = ((nx + 1) / 2) * 2;

    a   = ealloc1float(2*nzpad);
    rkz = ealloc1float(nzpad);
    rk(rkz,nzpad,dz,1);
  }

  for (i=0; i<num; i+=2) {

    if (iload == 0) {
      /* Load data from vin */
      for (k=0; k<nz; k++) {
	a[2*k] = vin[k][i];
	if (!ieo || !(i==num-2)) {
	  a[2*k+1] = vin[k][i+1];
	}
	else {
	  a[2*k+1] = 0.;
	}
      }
      /* zero padding for free surface */
      for (k=nz; k<nzpad; k++) {
	a[2*k]   = 0.;
	a[2*k+1] = 0.;
      }      
      nzunl = nzpad;
    } else {
      for (k=0; k<nzpad; k++) {
	a[2*k] = vin[k][i];
	if (!ieo || !(i==num-2)) {
	  a[2*k+1] = vin[k][i+1];
	}
	else {
	  a[2*k+1] = 0.;
	}
      }
      nzunl = nz;
    }

    /* Perform derivative */
    pfacc(m1, nzpad, (complex*)a);
	
    for (k=0; k<nzpad; k++) {
      tmp      =  a[2*k]   * rkz[k];
      a[2*k]   = -a[2*k+1] * rkz[k];
      a[2*k+1] =  tmp;
    }
	
    pfacc(p1, nzpad, (complex*)a);

    if (iadd == 0) {
	
      /* Set data into vout */
      for (k=0; k<nzunl; k++) {
	vout[k][i] = a[2*k];
	if (!ieo || !(i==num-2)) vout[k][i+1] = a[2*k+1];
      }
    } else if (iadd == 1) {

      /* Add data into vout */
      for (k=0; k<nzunl; k++) {
	vout[k][i] += a[2*k];
	if (!ieo || !(i==num-2)) vout[k][i+1] += a[2*k+1];
      }
    } else if (iadd == 2) {

      /* Set data multiplied by bulk into vout */
      for (k=0; k<nz; k++) {
	vout[k][i] = a[2*k] * bulk[k][i];
	if (!ieo || !(i==num-2)) vout[k][i+1] = a[2*k+1] * bulk[k][i+1];
      }
      if (nzunl == nzpad) {
	for (k=nz; k<nzpad; k++) {
	  vout[k][i] = a[2*k] * bulk[0][i];
	  if (!ieo || !(i==num-2)) vout[k][i+1] = a[2*k+1] * bulk[0][i+1];
	}
      }
    } else if (iadd == 3) {

      /* Add data multiplied by bulk into vout */
      for (k=0; k<nz; k++) {
	vout[k][i] += a[2*k] * bulk[k][i];
	if (!ieo || !(i==num-2)) vout[k][i+1] += a[2*k+1] * bulk[k][i+1];
      }
      if (nzunl == nzpad) {
	for (k=nz; k<nzpad; k++) {
	  vout[k][i] += a[2*k] * bulk[0][i];
	  if (!ieo || !(i==num-2)) vout[k][i+1] += a[2*k+1] * bulk[0][i+1];
	}
      }
    }
  }
}
/**********************************************************************/
void difz2(float **vin,float **vout, float **bulk, int nx, int nz, int nzpad, float dz,
	   int iadd)
/* 
   compute 2ndq derivative along z-direction 

   input:
     vin:    2D input array
     bulk:   2D array for multiplication with output array
     nx:     length of array in x-direction
     nz:     length of array in z-direction
     dz:     grid spacing in z-direction
     iadd:   flag

   output:
     vout:   2D output array

   iadd=0: set result in vout
   iadd=1: add result to vout
   iadd=2: set result multiplied by bulk in vout
   iadd=3: add result multiplied by bulk to vout
*/

{
  static int i, k;
  static float *a, *rkz;
  static int icall=0, ieo=0, m1=-1, p1=1 ;
  static int num;

  void rk(float *ak, int n, float d, int ind);

  if (icall == 0 ) {
    icall = 1;
    ieo = nx % 2;
    num = ((nx + 1) / 2) * 2;

    a   = ealloc1float(2*nzpad);
    rkz = ealloc1float(nzpad);
    rk(rkz,nzpad,dz,2);
  }

  for (i=0; i<num; i+=2) {
	
    /* Load data from vin */
    for (k=0; k<nz; k++) {
      a[2*k] = vin[k][i];
      if (!ieo || !(i==num-2)) {
	a[2*k+1] = vin[k][i+1];
      }
      else {
	a[2*k+1] = 0.;
      }
    }

    /* zero padding for free surface */
    for (k=nz; k<nzpad; k++) {
      a[2*k]   = 0.;
      a[2*k+1] = 0.;
    }      

    /* Perform derivative */
    pfacc(m1, nzpad, (complex*)a);
	
    for (k=0; k<nzpad; k++) {
      a[2*k]   *= rkz[k];
      a[2*k+1] *= rkz[k];
    }
	
    pfacc(p1, nzpad, (complex*)a);

    if (iadd == 0) {
	
      /* Set data into vout */
      for (k=0; k<nzpad; k++) {
	vout[k][i] = a[2*k];
	if (!ieo || !(i==num-2)) vout[k][i+1] = a[2*k+1];
      }
    } else if (iadd == 1) {

      /* Add data into vout */
      for (k=0; k<nzpad; k++) {
	vout[k][i] += a[2*k];
	if (!ieo || !(i==num-2)) vout[k][i+1] += a[2*k+1];
      }
    } else if (iadd == 2) {

      /* Set data multiplied by bulk into vout */
      for (k=0; k<nz; k++) {
	vout[k][i] = a[2*k] * bulk[k][i];
	if (!ieo || !(i==num-2)) vout[k][i+1] = a[2*k+1] * bulk[k][i+1];
      }
    } else if (iadd == 3) {

      /* Add data multiplied by bulk into vout */
      for (k=0; k<nz; k++) {
	vout[k][i] += a[2*k] * bulk[k][i];
	if (!ieo || !(i==num-2)) vout[k][i+1] += a[2*k+1] * bulk[k][i+1];
      }
    }
  }
}
 
/**********************************************************************/
void rk(float *ak, int n, float d, int ind)
/*
  wave-numbers for Fourier transform

  input:
    n:      length of Fourier transform
    d:      grid spacing
    ind:    order of derivative

  output:
    ak:     array of wave numbers
*/
{
  int i,isign,n2;
  float pi, dn, c; 
  
  pi = 4.*atan(1.);
  isign = (int)(-pow((-1.),(float)ind));
  n2 = n/2;
  dn = 2.*pi/(n*d);
  
  for (i=0; i<n; i++) {
    c=dn*i;
    if (i > n2) c = -dn*(n-i);
    ak[i] = pow(c,(float)ind)*isign/n;
  }
}
/**********************************************************************/
void bessel_jn(double x, int n, double *bes)
/*
  compute Bessel function values J0(x), J1(x), ...

  input:
    x:    argument
    n:    highest order of Bessel function

  output:
    bes:  function values of n orders of Bessel functions at argument x

  Notes:  1.  n must be larger than x
          2.  for accuracy of higher order Bessel function  choose n 
              larger (e.g. +50) than needed
*/
{
  int i, j;
  double sum;
  double eps=1.0e-130;

  if (x > (double)n)
    err("bessel_jn: n(%d) must be larger than x(%e)!\n",n,x);

  if (fabs(x) > eps) {

    bes[n-1] = 0.;
    bes[n-2] = 1.;
    
    sum = 0.;
    for (i=n-3; i>=0; i--) {
      bes[i] = (2*(i+1)/x)*bes[i+1] - bes[i+2];
      
      if (i%2 == 0) {
	sum = sum + 2. * bes[i];
	if (fabs(sum) > 1.e30) {
	  for (j=i; j<n; j++) bes[j] /= sum;
	  sum = 1.;
	}
      }
    }
    
    sum = bes[0];
    for (i=2; i<n; i+=2) sum += 2. * bes[i];
    for (i=0; i<n; i++) bes[i] /= sum;
  } else { /* if fabs(x) very small assume x=0. */
    bes[0] = 1.;
    for (i=1; i<n; i++) bes[i] = 0.;
  }
}

/*
  Routines for numerical convolution of Bessel functions with wavelet
  using Gauss-Legendre quadrature
*/

void null(int n, double *x0, double *p, double *pm, double eps, 
	  void (*func)(int, double, double*, double*, double*));
void legpol(int n, double x, double *p, double *pm, double *ps);
void legcoeffs(int n, double *x, double *a);
void bessel_jn(double x, int n, double *bes);

/*----------------------------------------------------------------------*/
void legcoeffs(int n, double *x, double *a)
/*
  Abscissas and corresponding weights for Gauss-Legendre quadrature
  (abscissas are zeros of Legendre polynomial)

  only positive zeros need to be estimated since they come in +/- pairs;
  they are arranged in descending order;

  Input:
    n  polynomial degree

  Output:
    x  abscissas
    a  weights
*/
{
  int i, n2;
  double eps, con, p, pm, ps;

  eps = DBL_EPSILON;

  n2 = n/2;

  /* approximate location of zeros */
  con = 1.0 - 1.0/(8.0*n*n) + 1.0/(8.0*n*n*n);
  
  for (i=0; i<n2; i++)
    x[i] = con * cos(PI*(4.0*(i+1)-1.0)/(4.0*n+2.0));
  
  /* accurate location of zeros using Newton-Raphson */
  for (i=0; i<n2; i++) {
    null(n, &x[i], &p, &pm, eps, legpol);
    a[i] = 2.0 * (1.0-x[i]*x[i])/(n*n*pm*pm);
  }
  
  if (n % 2) { /* if degree is odd */
    i = n2;
    x[i] = 0.;
    legpol(n, x[i], &p, &pm, &ps);
    a[i] = 2./(n*n*pm*pm);
  }

  /* generate negative zeros and corresponding weights */
  for (i=0; i<n2; i++) {
    x[n-1-i] = -x[i];
    a[n-1-i] =  a[i];
  }

  return;
}
/*----------------------------------------------------------------------*/
void null(int n, double *x0, double *p, double *pm, double eps,
	  void (*func)(int, double, double*, double*, double*))
/*
  Newton-Raphson method

  Input:
    n     degree of function func
    x0    approximate extimate of zero
    eps   termination accuracy criterion epsilon
    func  function to be evaluated

  Output:
    p     value of polynomial of degree n
    pm    value of polynomial of degree n-1

*/
{
  int iter;
  double d, ps, x1;

  iter = 0;
  d = 2.*eps;
  while (d > eps) {
    iter++;
    if (iter > 10) {
      fprintf(stderr,"stop in null: too many iterations!\n");
      exit (1);
    }
    func(n, *x0, p, pm, &ps);
    x1 = *x0 - *p/ps;
    d = fabs(x1 - *x0);
    *x0 = x1;
  }
  return;
}
/*----------------------------------------------------------------------*/
void legpol(int n, double x, double *p, double *pm, double *ps)
/*
  recursive computation of Legendre polynomials

  input:
    n   polynomial degree
    x   argument

  output:
    p   polynomial value (degree n)
    pm  polynomial value (degree n-1)
    ps  first derivative of p (degree n)
*/
{
  int i;
  double p0, p1, tmp;
  
  p0 = 1.0;
  p1 = x;
  for (i=1; i<n; i++) {
    tmp = ((2*i+1) * x * p1 - i * p0) / (i+1);
    p0 = p1;
    p1 = tmp;
  }

  *p = p1;
  *pm = p0;
  *ps = n * (x * p1 - p0) / (x * x - 1.);
  
  return;
}
/*----------------------------------------------------------------------*/
void lintgr(float t, float r, float dt, int nt, int m, float *wave, double* sum)
/*
  integrate J_n(tR) h(t-tau) using Gauss-Legendre quadrature

  input:
    t      time
    r      parameter used in Bessel function argument
    dt     sample increment of wavelet stored in wave
    nt     number of wavelet samples 
    m      highest order of Bessel functions
    wave   array of wavelet sample values

  output:
    sum    quadrature result
*/
{
  int i, j, m2, mm, n;
  static float *u, *xout, *yout;
  double arg, f, wmax;
  double *x, *a, *bes;

  n = t*r; /* number of quadrature points */
  n /= 2;
  if (n < 20) n = 20;

  m2 = m/2;
  mm = m + 50;  /* added indices for better accuracy of high J_n(x) */

  bes  = ealloc1double(mm+1);
  x    = ealloc1double(n);
  a    = ealloc1double(n);
  u    = ealloc1float(n);
  xout = ealloc1float(n);
  yout = ealloc1float(n);

  legcoeffs(n, x, a);

  for (j=0; j<m2; j++) sum[j] = 0.;

  for (i=0; i<n; i++) {
    /* map interval [-1,1] to [0,t] */
    u[i] = 0.5 * t * (x[i] + 1.);
    xout[i] = t - u[i];
  }

  wmax = 0.0;
  for (i=0; i<nt; i++)
    if (fabs(wave[i]) > wmax) wmax = fabs(wave[i]);
  wmax *= 1.0e-6;

  /* sinc interpolation of wavelet to quadrature points */
  ints8r(nt,dt,dt,wave,0.,0.,n,xout,yout);
  for (i=0; i<n; i++) {
    /* ignore very small contributions */
    if (fabs(yout[i]) < wmax) continue;

    f = yout[i] * a[i];

    arg = u[i]*r;
    bessel_jn(arg,mm,bes);

    for (j=0; j<m2; j++) sum[j] += bes[2*j+1] * f;
  }

  /* normalize by interval length */
  for (j=0; j<m2; j++) sum[j] *= 0.5 * t;
  
  free(yout);
  free(xout);
  free(bes);
  free(u);
  free(a);
  free(x);

  return;
} /* end function lintgr */
/**********************************************************************/
float fwave(float t, float fmax)
/*
!
! --- Ricker wavelet ---
!
! t    : time
! fmax : maximum frequency of wavelet spectrum
!
! returns time sample at time t of a Ricker-wavelet with 
! maximum frequency fmax
!
*/
{
  static int ienter=0;
  static float pi, pi2, agauss, tcut, s, res;

  if (ienter == 0) {
    ienter = 1;
    pi = 4.*atan(1.);
    pi2 = 2.*pi;
    agauss = 0.5*fmax;
    tcut = 1.5/agauss;
  }

  s = (t-tcut) * agauss;

  if (fabs(s) < 4.) {
    res = exp(-2.*s*s) * cos(pi2*s);
  } else {
    res = 0.;
  }

  return res;
} /* end function fwave */
/**********************************************************************/
void damp(int nbw, float abso, float *bw)
{
/* 
    compute weights for aborbing boundary region

    input:
      nbw:    number of gridpoints of boundary width
      abso:   strength of absorption

    output:
      bw:     array of absorption weights

*/
  int i;
  float pi, delta;

  pi = 4. * atan(1.);
  delta = pi / nbw;
  
  for (i=0; i<nbw; i++) {
    bw[i] = 1.0 - abso * (1.0 + cos(i*delta)) * 0.5;
  }
 
  return;
}
/**********************************************************************/
void tbc2d(float *a, float *wbx, float *wbz, int nwbx, int nwbz,
           int nx, int nz)
/*
   2D absorbing boundary condition

   input:
   a:     array to be treated at boundaries
   wbx:   array of weights for absorbing boundaries in horizontal direction
   wbz:   array of weights for absorbing boundaries in vertical direction
   nbwx:  number of grid points of absorbing zone in x-direction
   nbwz:  number of grid points of absorbing zone in z-direction
   nx:    number of grid points of computational area in x-direction
   nz:    number of grid points of computational area in z-direction

   output:
   a:    array after absorbing boundaries treatment

   Note: The top absorbing region is placed below the bottom absorbing
         region. This is possible due to the cyclic nature of the
         discrete Fourier transform.

     (free) surface
   +----------------+
   |*              *|     * - absorbing region
   |*              *|
   |*              *|
   |*              *|
   |****************|
   |****************|
   +----------------+
         bottom
*/
{
  static int *ist;
  static int ienter = 0;
  int i, k;
  int ifl, ihv, istart, is, iz;
  int ind1, ind2, nwb, ng1, ng2, nz0;   
  float wb;
  
  if (ienter == 0) {
    ienter = 1;
    ist = ealloc1int(nz);
    ng1 = nwbx;
    ng2 = nwbz;
    ihv = 0;
    if (ng1 > ng2) {
      ng1 = nwbz;
      ng2 = nwbx;
      ihv = 1;
    }
    ifl = -ng2;
    istart = nwbx;
    iz = 0;
    if (ihv == 1) ist[0] = nwbx;
    for (i=0; i<ng2; i++) {
      if (ifl >= 0) {
	ifl = ifl-ng2;
	iz++;
	if (ihv == 1) ist[iz] = nwbx-i;
	istart--;
      }
      ifl = ifl + ng1;
      if (ihv == 0) ist[i] = istart;
      /* printf("i= %d ist= %d\n",i,ist[i]); */
    }
  }
/*
   +------------------------+
   |   Treatment of sides   |
   +------------------------+
*/
  nz0 = nz - 2*nwbz;
  for (k=0; k<nz; k++) {
    nwb = nwbx;
    if (k >= nz0+nwbz) nwb = ist[nz-k-1];
    if (k >= nz0 && k < nz0+nwbz) nwb = ist[k-nz0];

    for (i=0; i<nwb; i++) {
      wb = wbx[i];
      ind1 = i + k*nx;
      a[ind1] *= wb;
      ind2 = (nx-i-1) + k*nx;
      a[ind2] *= wb;
    }
  }

/*
   +-------------------------+
   |   Treatment of bottom   |
   +-------------------------+
*/
  for (k=0; k<nwbz; k++) {
    is = ist[k];
    wb = wbz[nwbz-k-1];

    for (i=is; i<nx-is; i++) {
      ind1 = i + (k+nz0)*nx;
      ind2 = i + (nz-k-1)*nx;
      a[ind1] *= wb;
      a[ind2] *= wb;
    }
  }
}
