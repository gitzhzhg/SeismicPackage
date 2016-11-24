/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                */

/* SUREMEL2DAN: $Revision: 1.4 $ ; $Date: 2015/08/11 22:52:55 $         */


/* 
  Elastic anisotropic 2D Fourier method modeling with REM time integration
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
" SUREMEL2DAN - Elastic anisotropic 2D Fourier method modeling with high ",
"               accuracy Rapid Expansion Method (REM) time integration   ",
"                                                                        ",
" suremel2dan [parameters]                                               ",
"                                                                        ",
" Required parameters:                                                   ",
"                                                                        ",
" nx=         number of grid points in horizontal direction              ",
" nz=         number of grid points in vertical direction                ",
" nt=         number of time samples                                     ",
" dx=         spatial increment in horizontal direction                  ",
" dz=         spatial increment in vertical direction                    ",
" dt=         time sample interval in seconds                            ",
" isx=        grid point # of horizontal source positions                ",
" isz=        grid point # of vertical source positions                  ",
" styp=       source types (pressure, shear, single forces)              ",
" samp=       amplitudes of sources                                      ",
" amode=      0: isotropic,  1: anisotropic                              ",
" vmax=       global maximum velocity (only if amode=1)                  ",
" vmin=       global minimum velocity (only if amode=1)                  ",
"                                                                        ",
" Optional parameters:                                                   ",
" fx=0.0      first horizontal coordinate                                ",
" fz=0.0      first vertical coordinate                                  ",
" irx=        horizontal grid point # of vertical receiver lines         ",
" irz=        vertical grid point # of horizontal receiver lines         ",
" rxtyp=      types of horizontal receiver lines                         ",
" rztyp=      types of vertical receivers lines                          ",
" sntyp=      types of snapshots                                         ",
"             0: P,  1: S,  2: UX,  3: UZ                                ",
" w=0.1       width of spatial source distribution (see notes)           ",
" sflag=2     source time function                                       ",
"             0: user supplied source function                           ",
"             1: impulse (spike at t=0)                                  ",
"             2: Ricker wavelet                                          ",
" fmax=       maximum frequency of Ricker (default) wavelet              ",
" amps=1.0    amplitudes of sources                                      ",
" prec=0      1: precompute Bessel coefficients b_k (see notes)          ",
"             2: use precomputed Bessel coefficients b_k                 ",
" vmaxu=      user-defined maximum velocity                              ",
" dtsnap=0.0  time interval in seconds of wave field snapshots           ",
" iabso=1     apply absorbing boundary conditions (0: none)              ",
" abso=0.1    damping parameter for absorbing boundaries                 ",
" nbwx=20     horizontal width of absorbing boundary                     ",
" nbwz=20     vertical width of absorbing boundary                       ",
" verbose=0   1: show parameters used                                    ",
"             2: print maximum amplitude at every expansion term         ",
"                                                                        ",
" c11file=c11       c11 filename                                         ",
" c13file=c13       c13 filename                                         ",
" c15file=c15       c15 filename                                         ",
" c33file=c33       c33 filename                                         ",
" c35file=c35       c35 filename                                         ",
" c55file=c55       c55 filename                                         ",
" vpfile=vp         P-velocity filename                                  ",
" vsfile=vs         S-velocity filename                                  ",
" densfile=dens     density filename                                     ",
"                                                                        ",
" sname=wavelet.su  user supplied source time function filename          ",
"                                                                        ",
" Basenames of seismogram and snapshot files:                            ",
" xsect=xsect_     x-direction section files basename                    ",
" zsect=zsect_     z-direction section files basename                    ",
" snap=snap_       snapshot files basename                               ",
"                                                                        ",
" jpfile=stderr        diagnostic output                                 ",
"                                                                        ",
" Notes:                                                                 ",
"  0. The combination of the Fourier method with REM time integration    ",
"     allows the computation of synthetic seismograms which are free     ",
"     of numerical grid dispersion. REM has no restriction on the        ",
"     time step size dt. The Fourier method requires at least two        ",
"     grid points per shortest wavelength.                               ",
"  1. nx and nz must be valid numbers for pfafft transform lengths.      ",
"     nx and nz must be odd numbers. For valid numbers see e.g.          ",
"     numbers in structure 'nctab' in source file                        ",
"     $CWPROOT/src/cwp/lib/pfafft.c.                                     ",
"  2. Velocities and densities are stored as plain C style files         ",
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
"  6. If source is not single force (i.e. pressure or shear source)      ",
"     it should be not a spike in space; the parameter w determines      ",
"     at which distance (in grid points) from the source's center        ",
"     the Gaussian weight decays to 10 percent of its maximum.           ",
"     w=2 may be a reasonable choice; however, the waveform will be      ",
"     distorted.                                                         ",
"  7. Horizontal and vertical receiver line sections are written to      ",
"     separate files. Each file can hold more than one line.             ",
"  8. Parameter vmaxu may need to be chosen larger than the highest      ",
"     propagation velocity if the modeling run becomes unstable.         ",
"     This happens if the largest eigenvalue of the modeling             ",
"     operator L is larger than estimated from the largest velocity      ",
"     due to variations of the density.                                  ",
"  9. Bessel coefficients can be precomputed (prec=1) and stored on      ",
"     disk to save CPU time when several shots need to be run.           ",
"     In this case computation of Bessel coefficients can be skipped     ",
"     and read from disk file for reuse (prec=2).                        ",
"     For reuse of Bessel coefficients the user may need to define       ",
"     the overall maximum velocity (vmaxu).                              ",
" 10. If snapshots are not required, a spike source (sflag=1) may be     ",
"     applied and the resulting impulse response seismograms can be      ",
"     convolved later with a desired wavelet.                            ",
" 11. Output is written to SU style files.                               ", 
"     Basenames of seismogram and snapshot output files will be          ",
"     extended by the type of the data (p, s, ux, or uz).                ",
"     Additionally seismogram files will be consecutively numbered.      ",
"                                                                        ",
" Caveat:                                                                ",
"     Time sections and snapshots are kept entirely in memory during     ",
"     run time. Therefore, lots of time section and snapshots may        ",
"     eat up a large amount of memory.                                   ",
NULL};

/*
  Elastic anisotropic 2D Fourier method modeling with REM time integration

  Reference: 
  Kosloff, D., Fihlo A.Q., Tessmer, E. and Behle, A., 1989,
    Numerical solution of the acoustic and elastic wave equations by a
    new rapid expansion method, Geophysical Prospecting, 37, 383-394
  
 * Credits:
 *      University of Hamburg: Ekkehart Tessmer, July 2013
 */
/**************** end self doc ***********************************/

FILE *fpd=NULL, *fps=NULL, *fpp=NULL, *sfp=NULL, *jpfp;
FILE *fpc11, *fpc13, *fpc15, *fpc33, *fpc35, *fpc55;
FILE **trxfp, **trzfp, **snfp, *fpbes;

segy tri, tro, sno;

int main(int argc, char *argv[])
{
  int i, k, l, il, is, it; /* loop indices */
  int indx, indz;
  int amode;   /* isotropic/anisotropic input parameters flag */
  int sflag;   /* source function selection (Ricker/spike) */
  int prec;    /* flag for precomputing Bessel coefficients */
  int m;       /* number of Bessel coefficients computed */
  int m0;
  int m2;      /* number of Bessel coefficients used in expansion */
  int nx;      /* number of gridpoints in horizontal direction */
  int nz;      /* number of gridpoints in vertical direction */
  int nzpad;   /* number of gridpoints in vertical direction */
  int nt;      /* number of time samples */
  int nt0;
  int nbwx;    /* width of absorbing boundary in horizontal direction */
  int nbwz;    /* width of absorbing boundary in vertical direction */
  int nsnap;     /* number of snapshots */
  int nsnap0;    /* number of snapshots */
  int nsectx;    /* number of horizontal sections */
  int nsectz;    /* number of vertical sections */
  int nsourc;    /* number of sources */
  int nsntyp;    /* number of snapshot types */
  int iabso;     /* flag for using absorbing boundaries */
  int nwav=0;    /* number of samples for user source function */
  int verbose;
  int *irx=NULL; /* horizontal gridpoint indices of vertical receiver lines */
  int *irz=NULL; /* vertical gridpoint indices of horizontal receiver lines */
  int *rxtyp=NULL; /* receiver types in horizontal direction */
  int *rztyp=NULL; /* receiver types in vertical direction */
  int *sntyp=NULL; /* snapshot types */
  int *isx;        /* horizontal gridpoint indices of sources */
  int *isz;        /* vertical gridpoint indices of sources */
  int *styp;       /* source types */
  int *sntracl=NULL;    /* trace numbers of snapshots */
  float abso;   /* damping value for absorbing boundaries */
  float dx;     /* grid spacing in horizontal direction */
  float dz;     /* grid spacing in vertical direction */
  float dt;     /* time step size or sample rate */
  float dt0;   
  float dtsnap; /* time increment between snapshots */
  float dtsnap0;
  float dtwave; /* time interval of user supplied wavelet (sflag=0) */
  float fx;     /* first horizontal coordinate */
  float fz;     /* first vertical coordinate */
  float t;      /* time */
  float tmax;   /* maximum time */
  float w;      /* width of source's spatial Gaussian distribution */
  float fmax;   /* maximum frequency of source time function */
  float theta;     /* cubic dilatation */
  float tmp1, tmp2, tmp3; /* temporary storage */
  float vmax, vmin;    /* maximum & minimum velocities */
  float vmax0, vmin0;  /* user supplied maximum & minimum velocities */
  float vmaxu;  /* user defined maximum velocity */
  float r;      /* largest eigenvalue of modeling operator L */
  float r0;
  float amx;    /* maximum amplitude in numerical grid */
  float amz;    /* maximum amplitude in numerical grid */
  float pi;
  float *samp;     /* amplitudes of sources */
  float *bufz;     /* buffer */
  float *bwx;      /* weighting function values in absorbing zone (hor.) */
  float *bwz;      /* weighting function values in absorbing zone (vert.) */
  float *wave=NULL; /* user source function array */
  float **qux;      /* horizontal diplacement wave field (term Q_n) */
  float **quz;      /* vertical diplacement wave field (term Q_n)*/
  float **q2ux;     /* horizontal diplacement wave field (term Q_n+2) */
  float **q2uz;     /* vertical diplacement wave field (term Q_n+2)*/
  float **a1;       /* auxiliary array */
  float **a2;       /* auxiliary array */
  float **a3;       /* auxiliary array */
  float **c11=NULL;    /* anisotopy parameter (Voight notation) */
  float **c13=NULL;    /* anisotopy parameter (Voight notation) */
  float **c15=NULL;    /* anisotopy parameter (Voight notation) */
  float **c33=NULL;    /* anisotopy parameter (Voight notation) */
  float **c35=NULL;    /* anisotopy parameter (Voight notation) */
  float **c55=NULL;    /* anisotopy parameter (Voight notation) */
  float **lambda=NULL; /* Lame parameter */
  float **twmy=NULL;   /* twice the shear modulus */
  float **rhoinv;      /* inverse of density */
  float **gbox=NULL;   /* Gaussian source box */
  float ***xsect=NULL;    /* time sections along horizontal direction */
  float ***zsect=NULL;    /* time sections along vertical direction */
  float ****snap=NULL;    /* snapshots */
  double **bessn, **bestr, *btemp; /* Bessel coefficients */
  char* jpfile;           /* file for information output */
  char dum[4];
  cwp_String typ[]={"p","s","ux","uz"};    /* type of section or snapshot */
  cwp_String c11file;     /* c11 filename */
  cwp_String c13file;     /* c13 filename */
  cwp_String c15file;     /* c15 filename */
  cwp_String c33file;     /* c33 filename */
  cwp_String c35file;     /* c35 filename */
  cwp_String c55file;     /* c55 filename */
  cwp_String vpfile;      /* P-velocity filename */
  cwp_String vsfile;      /* S-velocity filename */
  cwp_String densfile;    /* density filename */
  cwp_String sname;       /* source function filename */
  cwp_String xsectb;      /* x-direction sections basename */
  cwp_String zsectb;      /* z-direction sections basename */
  cwp_String snapb;       /* snapshots basename */
  cwp_String *xsectfn=NULL; /* x-direction sections filenames */
  cwp_String *zsectfn=NULL; /* z-direction sections filenames */
  cwp_String *snapfn=NULL;  /* snapshots filenames */
  struct rusage cput;
  long user_bk, user_s_bk=0, user_u_bk=0, user_mod, user_s_mod=0, user_u_mod=0; 

  /* function prototypes */
  void difx(float **vin, float **vout, float **bulk, int nx, int nz,
	    float dx, int iadd);
  void difz(float **vin, float **vout, float **bulk, int nx, int nz, int nzpad,
	    float dz, int iadd, int iload);
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

  initargs(argc, argv);
  requestdoc(1);

  /* get parameters */
  if (!getparint ("amode", &amode)) err("amode required!\n");
  if (!getparint ("nx" , &nx)) err("nx required!\n");
  if (!getparint ("nz" , &nz)) err("nz required!\n");
  if (!getparint ("nt" , &nt)) err("nt required!\n");
  if (!getparfloat ("dx" , &dx)) err("dx required!\n");
  if (!getparfloat ("dz" , &dz)) err("dz required!\n");
  if (!getparfloat ("dt" , &dt)) err("dt required!\n");
  
  nzpad = nz;

  nsourc = countparval("isx");
  if (nsourc != countparval("isz"))
    err("isx and isz must have the same number of coordinates.\n");
  if (nsourc != countparval("styp"))
    err("isx and styp arrays must be of same size.\n");
  if (nsourc != countparval("samp"))
    err("isx and samp arrays must be of same size.\n");

  isx = ealloc1int(nsourc);
  isz = ealloc1int(nsourc);
  styp = ealloc1int(nsourc);
  samp = ealloc1float(nsourc);

  for (l=0; l<nsourc; l++) samp[l] = 1.;
  if (!getparint ("isx" , isx)) err("isx required!\n");
  if (!getparint ("isz" , isz)) err("isz required!\n");
  if (!getparint ("styp", styp)) err("styp required!\n");
  if (!getparfloat ("samp", samp)) err("samp required!\n");

  if (amode) {
    if (!getparfloat ("vmax", &vmax0)) err("vmax required!\n");
    if (!getparfloat ("vmin", &vmin0)) err("vmin required!\n");
  }

  /* get optional parameters */
  if (!getparfloat ("dtsnap" , &dtsnap)) dtsnap = DTSNAP;
  nsntyp = 0;
  if (dtsnap != 0.0) {
    nsntyp = countparval("sntyp");
    sntyp = ealloc1int(nsntyp);
    if (!getparint ("sntyp" , sntyp)) 
      err("sntyp required if dtsnap<>0!\n");
    getparint("sntyp", sntyp);
  }

  if (!getparint ("sflag" , &sflag)) sflag = SFLAG;
  if (!getparfloat ("fmax" , &fmax) && sflag==2) 
    err("fmax required if Ricker wavelet selected!\n");
  if (!getparint ("prec" , &prec)) prec = PREC;

  nsectx = countparval("irz");
  if (nsectx) {
    irz = ealloc1int(nsectx);
    getparint("irz", irz);
    if (countparval("rxtyp") == nsectx) {
      rxtyp = ealloc1int(nsectx);
      getparint("rxtyp", rxtyp);
    } else {
      err("irz and rxtyp arrays must be of same size.\n");
    }
  }

  nsectz = countparval("irx");
  if (nsectz) {
    irx = ealloc1int(nsectz);
    getparint("irx", irx);
    if (countparval("rztyp") == nsectz) {
      rztyp = ealloc1int(nsectz);
      getparint("rztyp", rztyp);
    } else {
      err("irx and rztyp must be of same size.\n");
    }
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
  getparfloat("samp", samp);

  if (!getparstring("jpfile",&jpfile)) {
    jpfp = stderr;
  } else { 
    jpfp = fopen(jpfile,"w");
  }

  if (!getparstring("c11file", &c11file)) c11file = "c11"; 
  if (!getparstring("c13file", &c13file)) c13file = "c13"; 
  if (!getparstring("c15file", &c15file)) c15file = "c15"; 
  if (!getparstring("c33file", &c33file)) c33file = "c33"; 
  if (!getparstring("c35file", &c35file)) c35file = "c35"; 
  if (!getparstring("c55file", &c55file)) c55file = "c55"; 
  if (!getparstring("vpfile",&vpfile)) vpfile = "vp"; 
  if (!getparstring("vsfile",&vsfile)) vsfile = "vs"; 
  if (!getparstring("densfile",&densfile)) densfile = "dens"; 

  if (!getparstring("xsect", &xsectb)) xsectb = "xsect_"; 
  if (!getparstring("zsect", &zsectb)) zsectb = "zsect_"; 
  if (!getparstring("snap",  &snapb))  snapb  = "snap_"; 

  if (!getparstring("sname",&sname)) sname = "wavelet.su"; 

  if (verbose) {
    fprintf(jpfp,"\namode=%d ",amode);
    if (amode)
      fprintf(jpfp,"(anisotropic elastic modeling)\n");
    else
      fprintf(jpfp,"(isotropic elastic modeling)\n");
    fprintf(jpfp,"\nnx=%d  dx=%f\n",nx,dx);
    fprintf(jpfp,"nz=%d  dz=%f\n",nz,dz);
    fprintf(jpfp,"nt=%d  dt=%f\n",nt,dt);
    fprintf(jpfp,"fx=%f  fz=%f\n",fx,fz);
    fprintf(jpfp,"\nw=%.3f\n",w);

    fprintf(jpfp,"sources: %d\n",nsourc);
    fprintf(jpfp,"  x-coordinates:");
    for (i=0; i<nsourc; i++) fprintf(jpfp,"%7d",isx[i]);
    fprintf(jpfp,"\n");
    fprintf(jpfp,"  z-coordinates:");
    for (i=0; i<nsourc; i++) fprintf(jpfp,"%7d",isz[i]);
    fprintf(jpfp,"\n");
    fprintf(jpfp,"  types        :");
    for (i=0; i<nsourc; i++) fprintf(jpfp,"%7s",typ[styp[i]]);
    fprintf(jpfp,"\n");
    fprintf(jpfp,"  amplitudes   :");
    for (i=0; i<nsourc; i++) fprintf(jpfp,"%7.3f",samp[i]);
    fprintf(jpfp,"\n");

    fprintf(jpfp,"\nsflag=%d\n",sflag);
    fprintf(jpfp,"fmax=%e\n",fmax);
    fprintf(jpfp,"prec=%d\n",prec);

    if (vmaxu > 0.) fprintf(jpfp,"vmaxu=%f\n",vmaxu);
  }

  if (npfa(nx) != nx) err("Error: Invalid nx !\n");
  if (! nx % 2) err("Error: nx must be odd!\n");
  if (npfa(nz) != nz) err("Error: Invalid nz !");
  if (! nz % 2) err("Error: nz must be odd!\n");

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
    if (nsectx) {
      fprintf(jpfp,"\nx-receiver lines: %d\n",nsectx);
      fprintf(jpfp,"  z-coordinates:");
      for (il=0; il<nsectx; il++) fprintf(jpfp,"%5d",irz[il]);
      fprintf(jpfp,"\n");
      fprintf(jpfp,"  section types:");
      for (il=0; il<nsectx; il++) fprintf(jpfp,"%5s",typ[rxtyp[il]]);
      fprintf(jpfp,"\n");
    }
    if (nsectz) {
      fprintf(jpfp,"z-receiver lines: %d\n",nsectz);
      fprintf(jpfp,"  x-coordinates:");
      for (il=0; il<nsectz; il++) fprintf(jpfp,"%5d",irx[il]);
      fprintf(jpfp,"\n");
      fprintf(jpfp,"  section types:");
      for (il=0; il<nsectz; il++) fprintf(jpfp,"%5s",typ[rztyp[il]]);
      fprintf(jpfp,"\n");
    }
    if (nsntyp) {
      fprintf(jpfp,"number of snapshot types: %d\n",nsntyp);
      fprintf(jpfp,"  nsnap=%d\n",nsnap);
      fprintf(jpfp,"  snapshot types:");
      for (is=0; is<nsntyp; is++) fprintf(jpfp,"%5s",typ[sntyp[is]]);
      fprintf(jpfp,"\n");
    }
  }
  
  if (prec == 2) 
    fprintf(jpfp,"sflag and fmax are ignored if precomputed Bessel coefficients are used (prec=2).\n");

  if (nt < 1) err("stopped: nt must be > 0!\n");

  bufz   = ealloc1float(nz);
  bwx    = ealloc1float(nbwx);
  bwz    = ealloc1float(nbwz);
  qux    = ealloc2float(nx,nz);
  quz    = ealloc2float(nx,nz);
  q2ux   = ealloc2float(nx,nz);
  q2uz   = ealloc2float(nx,nz);
  a1     = ealloc2float(nx,nz);
  a2     = ealloc2float(nx,nz);
  a3     = ealloc2float(nx,nz);
  rhoinv = ealloc2float(nx,nz);
  if (amode) {
    c11 =    ealloc2float(nx,nz);
    c13 =    ealloc2float(nx,nz);
    c15 =    ealloc2float(nx,nz);
    c33 =    ealloc2float(nx,nz);
    c35 =    ealloc2float(nx,nz);
    c55 =    ealloc2float(nx,nz);
  } else {
    lambda = ealloc2float(nx,nz);
    twmy   = ealloc2float(nx,nz);
  }
  gbox   = ealloc2float(NXB,NZB);

  if (nsnap > 0) snap = ealloc4float(nx,nz,nsnap,nsntyp);
  if (nsectx > 0) xsect = ealloc3float(nx,nt,nsectx);
  if (nsectz > 0) zsect = ealloc3float(nz,nt,nsectz);

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

  /* --- read subsurface structure --- */
  vmin = 1.0e30;
  vmax = 0.;
  
  if((fpd = fopen(densfile,"r")) == NULL)
    err("Could not open density file.");

  for (i=0; i<nx; i++) {
    if (fread(bufz,sizeof(float),nz,fpd) != nz)
      err("densfile has not enough items");
    for (k=0; k<nz; k++) rhoinv[k][i] = 1./bufz[k];
  }

  if (amode) {
    if((fpc11 = fopen(c11file,"r")) == NULL)
      err("Could not open c11 file.");

    if((fpc13 = fopen(c13file,"r")) == NULL)
      err("Could not open c13 file.");

    if((fpc15 = fopen(c15file,"r")) == NULL)
      err("Could not open c15 file.");

    if((fpc33 = fopen(c33file,"r")) == NULL)
      err("Could not open c33 file.");

    if((fpc35 = fopen(c35file,"r")) == NULL)
      err("Could not open c35 file.");

    if((fpc55 = fopen(c55file,"r")) == NULL)
      err("Could not open c55 file.");

    for (i=0; i<nx; i++) {
      if (fread(bufz,sizeof(float),nz,fpc11) != nz)
	err("c11file has not enough items");
      for (k=0; k<nz; k++) c11[k][i] = bufz[k];

      if (fread(bufz,sizeof(float),nz,fpc13) != nz)
	err("c13file has not enough items");
      for (k=0; k<nz; k++) c13[k][i] = bufz[k];

      if (fread(bufz,sizeof(float),nz,fpc15) != nz)
	err("c15file has not enough items");
      for (k=0; k<nz; k++) c15[k][i] = bufz[k];

      if (fread(bufz,sizeof(float),nz,fpc33) != nz)
	err("c33file has not enough items");
      for (k=0; k<nz; k++) c33[k][i] = bufz[k];

      if (fread(bufz,sizeof(float),nz,fpc35) != nz)
	err("c35file has not enough items");
      for (k=0; k<nz; k++) c35[k][i] = bufz[k];

      if (fread(bufz,sizeof(float),nz,fpc55) != nz)
	err("c55file has not enough items");
      for (k=0; k<nz; k++) c55[k][i] = bufz[k];
    }
    fclose(fpc11);
    fclose(fpc13);
    fclose(fpc15);
    fclose(fpc33);
    fclose(fpc35);
    fclose(fpc55);

    vmax = vmax0;
    vmin = vmin0;
  } else {

    if((fps = fopen(vsfile,"r")) == NULL)
      err("Could not open S-velocity file.");
    
    if((fpp = fopen(vpfile,"r")) == NULL)
      err("Could not open P-velocity file.");
    
    for (i=0; i<nx; i++) {
      if (fread(bufz,sizeof(float),nz,fps) != nz)
	err("vsfile has not enough items");
      for (k=0; k<nz; k++) {
	twmy[k][i] = 2.0*bufz[k]*bufz[k]/rhoinv[k][i];
	vmin = (bufz[k] < vmin ? bufz[k] : vmin);
      }	
      if (fread(bufz,sizeof(float),nz,fpp) != nz)
	err("vpfile has not enough items");
      for (k=0; k<nz; k++) {
	lambda[k][i] = bufz[k]*bufz[k]/rhoinv[k][i] - twmy[k][i];
	vmax = (bufz[k] > vmax ? bufz[k] : vmax);
      }
    }
    fclose(fps);
    fclose(fpp);
  }
  fclose(fpd);

  if (verbose) fprintf(jpfp,"max., min. velocities: %e, %e\n",vmax,vmin);
  if (vmax == 0.) err("stopped: vmax is zero!\n");
  if ((dx > dz ? dx : dz) > vmin/(2.*fmax))
    err("stopped: fmax is too high for spatial sampling!\n");
  
  /* change absorbing zone for no reflections at top */
  if (iabso) {
    if (amode) {
      for (i=0; i<nx; i++) {
	for (k=nz-nbwz; k<nz; k++) {
	  rhoinv[k][i] = rhoinv[0][i];
	  c11[k][i] = c11[0][i];
	  c13[k][i] = c13[0][i];
	  c15[k][i] = c15[0][i];
	  c33[k][i] = c33[0][i];
	  c35[k][i] = c35[0][i];
	  c55[k][i] = c55[0][i];
	}
      }
    } else {
      for (i=0; i<nx; i++) {
	for (k=nz-nbwz; k<nz; k++) {
	  rhoinv[k][i] = rhoinv[0][i];
	  twmy[k][i] = twmy[0][i];
	  lambda[k][i] = lambda[0][i];
	}
      }
    }
  }

  /* setup aborbing boundaries */
  if (iabso) {
    damp(nbwx,abso,bwx);
    damp(nbwz,abso,bwz);
  }

  /* prepare Bessel coefficients convolved with wavelet */
  if (vmaxu > 0.) {
    if (vmax > vmaxu) err("vmaxu is smaller than vmax!\n");
    vmax = vmaxu; 
  }

  /* largest EV of operator L */
  pi = 4.*atan(1.);
  r = 1.1 * pi * vmax * sqrt(1./(dx*dx) + 1./(dz*dz));

  m = tmax * r; /* highest index of terms in expansion */
  m2 = m/2; /* number of expansion terms */

  if (verbose) fprintf(jpfp,"r=%e  tmax*r=%e  m=%d\n",r,tmax*r,m);
  bessn = ealloc2double(m2,nsnap);
  bestr = ealloc2double(m2,nt);
  btemp = ealloc1double(m+50);

  if (verbose) {
    getrusage(RUSAGE_SELF, &cput);
    user_s_bk = cput.ru_utime.tv_sec;
    user_u_bk = cput.ru_utime.tv_usec;
  }

  /* Bessel-coefficients */
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
	err("Bessel coefficients must be precomputed (prec=1).\n");
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
    for (i=0; i<nx; i++) qux[k][i] = quz[k][i] = 0.;
    for (i=0; i<nx; i++) rhoinv[k][i] *= 4./(r*r);
  }

  /* Gaussian source box */
  g2d(w, NXB, NZB, gbox);

  /* initialize first expansion term Q_1 */
  /* source distribution */ 
  for (l=0; l<nsourc; l++) { /* pressure sources */
    if (styp[l] == 0) {
      for (k=0; k<NZB; k++) {
	indz = isz[l]+k-NZB/2;
	for (i=0; i<NXB; i++) {
	  indx = isx[l]+i-NXB/2;
	  a1[indz][indx] = -0.5 * gbox[k][i] * samp[l];
	}
      }
      difx(a1,qux,NULL,nx,nz,dx,1);
      difz(a1,quz,NULL,nx,nz,nzpad,dz,1,0);
    } else if (styp[l] == 1) { /* shear sources */
      for (k=0; k<NZB; k++) {
	indz = isz[l]+k-NZB/2;
	for (i=0; i<NXB; i++) {
	  indx = isx[l]+i-NXB/2;
	  a1[indz][indx] = -0.5 * gbox[k][i] * samp[l];
	  a2[indz][indx] = -a1[indz][indx];
	}
      }
      difx(a2,quz,NULL,nx,nz,dx,1);
      difz(a1,qux,NULL,nx,nz,nzpad,dz,1,0);
    } else if (styp[l] == 2) { /* single forces (x-direction) */
      for (k=0; k<NZB; k++) {
	indz = isz[l]+k-NZB/2;
	for (i=0; i<NXB; i++) {
	  indx = isx[l]+i-NXB/2;
	  qux[indz][indx] += gbox[k][i] * samp[l];
	}
      }
    } else if (styp[l] == 3) { /* single forces (z-direction) */
      for (k=0; k<NZB; k++) {
	indz = isz[l]+k-NZB/2;
	for (i=0; i<NXB; i++) {
	  indx = isx[l]+i-NXB/2;
	  quz[indz][indx] += gbox[k][i] * samp[l];
	}
      }
    }
  }

  for (k=0; k<nz; k++) {
    for (i=0; i<nx; i++) {
      qux[k][i] /= dx*dz;
      quz[k][i] /= dx*dz;
    }
  }

  /* initialize second expansion term Q_3 */
  /* strains */
  /* E_xx */
  difx(qux,a1,NULL,nx,nz,dx,0);
  /* E_zz */
  difz(quz,a2,NULL,nx,nz,nzpad,dz,0,0);
  /* E_xz */
  difx(quz,a3,NULL,nx,nz,dx,0);
  
  /* shear snapshots (part I) */
  for (il=0; il<nsntyp; il++) {
    if (sntyp[il] == 1) {
      for (it=0; it<nsnap; it++) {
	for (k=0; k<nz; k++) {
	  for (i=0; i<nx; i++)
	    snap[il][it][k][i] = 2.*a3[k][i]*bessn[it][0];
	}
      }
    }
  }

  /* shear sections (part I) */
  for (il=0; il<nsectx; il++) {
    if (rxtyp[il] == 1) {
      for (it=0; it<nt; it++) {
	for (i=0; i<nx; i++)
	  xsect[il][it][i] = 2.*a3[irz[il]][i]*bestr[it][0];
      }
    }
  }
  for (il=0; il<nsectz; il++) {
    if (rztyp[il] == 1) {
      for (it=0; it<nt; it++) {
	for (k=0; k<nz; k++)
	  zsect[il][it][k] = 2.*a3[k][irx[il]]*bestr[it][0];
      }
    }
  }

  difz(qux,a3,NULL,nx,nz,nzpad,dz,1,0);

  /* shear snapshots (part II) */
  for (il=0; il<nsntyp; il++) {
    if (sntyp[il] == 1) {
      for (it=0; it<nsnap; it++) {
	for (k=0; k<nz; k++) {
	  for (i=0; i<nx; i++)
	    snap[il][it][k][i] -= a3[k][i]*bessn[it][0];
	}
      }
    }
  }
  
  /* shear sections (part II) */
  for (il=0; il<nsectx; il++) {
    if (rxtyp[il] == 1) {
      for (it=0; it<nt; it++) {
	for (i=0; i<nx; i++)
	  xsect[il][it][i] -= a3[irz[il]][i]*bestr[it][0];
      }
    }
  }
  for (il=0; il<nsectz; il++) {
    if (rztyp[il] == 1) {
      for (it=0; it<nt; it++) {
	for (k=0; k<nz; k++)
	  zsect[il][it][k] -= a3[k][irx[il]]*bestr[it][0];
      }
    }
  }

#if 0
  /* displacement divergence snapshots */
  for (il=0; il<nsntyp; il++) {
    if (sntyp[il] == 0) {
      for (it=0; it<nsnap; it++) {
	for (k=0; k<nz; k++) {
	  for (i=0; i<nx; i++)
	    snap[il][it][k][i] = 0.5*(a1[k][i]+a2[k][i])*bessn[it][0];
	}
      }
    }
  }
  
  /* displacement divergence sections */
  for (il=0; il<nsectx; il++) {
    if (rxtyp[il] == 0) {
      for (it=0; it<nt; it++) {
	for (i=0; i<nx; i++)
	  xsect[il][it][i] = 0.5*(a1[irz[il]][i]+a2[irz[il]][i])*bestr[it][0];
      }
    }
  }
  for (il=0; il<nsectz; il++) {
    if (rztyp[il] == 1) {
      for (it=0; it<nt; it++) {
	for (k=0; k<nz; k++)
	  zsect[il][it][k] = 0.5*(a1[k][irx[il]]+a2[k][irx[il]])*bestr[it][0];
      }
    }
  }
#endif

  /* --- stress-strain relation --- */
  if (amode) {
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	tmp1 = a1[k][i]*c11[k][i] + a2[k][i]*c13[k][i] + a3[k][i]*c15[k][i];
	tmp2 = a1[k][i]*c13[k][i] + a2[k][i]*c33[k][i] + a3[k][i]*c35[k][i];
	tmp3 = a1[k][i]*c15[k][i] + a2[k][i]*c35[k][i] + a3[k][i]*c55[k][i];
	a1[k][i] = tmp1;
	a2[k][i] = tmp2;
	a3[k][i] = tmp3;
      }
    }
  } else {
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	theta = a1[k][i] + a2[k][i];
	a1[k][i] = lambda[k][i]*theta + twmy[k][i]*a1[k][i];
	a2[k][i] = lambda[k][i]*theta + twmy[k][i]*a2[k][i];
	a3[k][i] = twmy[k][i]*a3[k][i]*0.5;
      }
    }
  }
  
  /* pressure snapshots */
  for (il=0; il<nsntyp; il++) {
    if (sntyp[il] == 0) {
      for (it=0; it<nsnap; it++) {
	for (k=0; k<nz; k++) {
	  for (i=0; i<nx; i++)
	    snap[il][it][k][i] = 0.5*(a1[k][i]+a2[k][i])*bessn[it][0];
	}
      }
    }
  }
  
  /* pressure sections */
  for (il=0; il<nsectx; il++) {
    if (rxtyp[il] == 0) {
      for (it=0; it<nt; it++) {
	for (i=0; i<nx; i++)
	  xsect[il][it][i] = 0.5*(a1[irz[il]][i]+a2[irz[il]][i])*bestr[it][0];
      }
    }
  }
  for (il=0; il<nsectz; il++) {
    if (rztyp[il] == 1) {
      for (it=0; it<nt; it++) {
	for (k=0; k<nz; k++)
	  zsect[il][it][k] = 0.5*(a1[k][irx[il]]+a2[k][irx[il]])*bestr[it][0];
      }
    }
  }
  
  /* --- accellerations --- */
  difx(a1,q2ux,rhoinv,nx,nz,dx,2);
  difz(a3,q2ux,rhoinv,nx,nz,nzpad,dz,3,0);
  
  difz(a2,q2uz,rhoinv,nx,nz,nzpad,dz,2,0);
  difx(a3,q2uz,rhoinv,nx,nz,dx,3);

  if (iabso) {
    tbc2d(qux[0],bwx,bwz,nbwx,nbwz,nx,nz);
    tbc2d(quz[0],bwx,bwz,nbwx,nbwz,nx,nz);
    tbc2d(q2ux[0],bwx,bwz,nbwx,nbwz,nx,nz);
    tbc2d(q2uz[0],bwx,bwz,nbwx,nbwz,nx,nz);
  }

  /* generation of second term */
  for (k=0; k<nz; k++) {
    for (i=0; i<nx; i++) {
      q2ux[k][i] += 3.*qux[k][i];
      q2uz[k][i] += 3.*quz[k][i];
    }
  }

  /* displacement snapshots */
  for (il=0; il<nsntyp; il++) {
    if (sntyp[il] == 2) {
      for (it=0; it<nsnap; it++) {
	for (k=0; k<nz; k++) {
	  for (i=0; i<nx; i++)
	    snap[il][it][k][i] = qux[k][i]*bessn[it][0];
	}
      }
    }
    if (sntyp[il] == 3) {
      for (it=0; it<nsnap; it++) {
	for (k=0; k<nz; k++) {
	  for (i=0; i<nx; i++)
	    snap[il][it][k][i] = quz[k][i]*bessn[it][0];
	}
      }
    }
  }
  
  /* displacement sections */
  for (il=0; il<nsectx; il++) {
    if (rxtyp[il] == 2) {
      for (it=0; it<nt; it++) {
	for (i=0; i<nx; i++)
	  xsect[il][it][i] = qux[irz[il]][i]*bestr[it][0];
      }
    }
    if (rxtyp[il] == 3) {
      for (it=0; it<nt; it++) {
	for (i=0; i<nx; i++)
	  xsect[il][it][i] = quz[irz[il]][i]*bestr[it][0];
      }
    }
  }

  for (il=0; il<nsectz; il++) {
    if (rztyp[il] == 2) {
      for (it=0; it<nt; it++) {
	for (k=0; k<nz; k++)
	  zsect[il][it][k] = qux[k][irx[il]]*bestr[it][0];
      }
    }
    if (rztyp[il] == 3) {
      for (it=0; it<nt; it++) {
	for (k=0; k<nz; k++)
	  zsect[il][it][k] = quz[k][irx[il]]*bestr[it][0];
      }
    }
  }
  
  /* calculate all other odd expansion terms Q_2n+1 */
  for (l=1; l<m2; l++) {

    if (verbose == 2) {
      amx = 0.;
      amz = 0.;
      for (k=0; k<nz; k++) {
	for (i=0; i<nx; i++) {
	  amx = (fabs(q2ux[k][i]) > amx ? fabs(q2ux[k][i]) : amx);
	  amz = (fabs(q2uz[k][i]) > amz ? fabs(q2uz[k][i]) : amz);
	}
      }
      fprintf(stderr,"l=%5d    amx=%e  amz=%e\n",l,amx,amz);
    }

    /* --- recursion relation --- */
    for (k=0; k<nz; k++) {
      for (i=0; i<nx; i++) {
	a1[k][i] = q2ux[k][i];
	a2[k][i] = q2uz[k][i];

	q2ux[k][i] = 2.*q2ux[k][i] - qux[k][i];
	q2uz[k][i] = 2.*q2uz[k][i] - quz[k][i];

	qux[k][i] = a1[k][i];
	quz[k][i] = a2[k][i];
      }
    }

    /* strains */
    /* E_xx */
    difx(qux,a1,NULL,nx,nz,dx,0);
    /* E_zz */
    difz(quz,a2,NULL,nx,nz,nzpad,dz,0,0);
    /* E_xz */
    difx(quz,a3,NULL,nx,nz,dx,0);
    
    /* shear snapshots (part I) */
    for (il=0; il<nsntyp; il++) {
      if (sntyp[il] == 1) {
	for (it=0; it<nsnap; it++) {
	  for (k=0; k<nz; k++) {
	    for (i=0; i<nx; i++)
	      snap[il][it][k][i] += 2.*a3[k][i]*bessn[it][l];
	  }
	}
      }
    }
    
    /* shear sections (part I) */
    for (il=0; il<nsectx; il++) {
      if (rxtyp[il] == 1) {
	for (it=0; it<nt; it++) {
	  for (i=0; i<nx; i++)
	    xsect[il][it][i] += 2.*a3[irz[il]][i]*bestr[it][l];
	}
      }
    }
    for (il=0; il<nsectz; il++) {
      if (rztyp[il] == 1) {
	for (it=0; it<nt; it++) {
	  for (k=0; k<nz; k++)
	    zsect[il][it][k] += 2.*a3[k][irx[il]]*bestr[it][l];
	}
      }
    }
    
    difz(qux,a3,NULL,nx,nz,nzpad,dz,1,0);
    
    /* shear snapshots (part II) */
    for (il=0; il<nsntyp; il++) {
      if (sntyp[il] == 1) {
	for (it=0; it<nsnap; it++) {
	  for (k=0; k<nz; k++) {
	    for (i=0; i<nx; i++)
	      snap[il][it][k][i] -= a3[k][i]*bessn[it][l];
	  }
	}
      }
    }
    
    /* shear sections (part II) */
    for (il=0; il<nsectx; il++) {
      if (rxtyp[il] == 1) {
	for (it=0; it<nt; it++) {
	  for (i=0; i<nx; i++)
	    xsect[il][it][i] -= a3[irz[il]][i]*bestr[it][l];
	}
      }
    }
    for (il=0; il<nsectz; il++) {
      if (rztyp[il] == 1) {
	for (it=0; it<nt; it++) {
	  for (k=0; k<nz; k++)
	    zsect[il][it][k] -= a3[k][irx[il]]*bestr[it][l];
	}
      }
    }

#if 0    
    /* displacment divergence snapshots */
    for (il=0; il<nsntyp; il++) {
      if (sntyp[il] == 0) {
	for (it=0; it<nsnap; it++) {
	  for (k=0; k<nz; k++) {
	    for (i=0; i<nx; i++)
	      snap[il][it][k][i] += 0.5*(a1[k][i]+a2[k][i])*bessn[it][l];
	  }
	}
      }
    }
    
    /* displacment divergence sections */
    for (il=0; il<nsectx; il++) {
      if (rxtyp[il] == 0) {
	for (it=0; it<nt; it++) {
	  for (i=0; i<nx; i++)
	    xsect[il][it][i] += 0.5*(a1[irz[il]][i]+a2[irz[il]][i])*bestr[it][l];
	}
      }
    }
    for (il=0; il<nsectz; il++) {
      if (rztyp[il] == 1) {
	for (it=0; it<nt; it++) {
	  for (k=0; k<nz; k++)
	    zsect[il][it][k] += 0.5*(a1[k][irx[il]]+a2[k][irx[il]])*bestr[it][l];
	}
      }
    }
#endif

    /* --- stress-strain relation --- */
    if (amode) {
      for (k=0; k<nz; k++) {
	for (i=0; i<nx; i++) {
	  tmp1 = a1[k][i]*c11[k][i] + a2[k][i]*c13[k][i] + a3[k][i]*c15[k][i];
	  tmp2 = a1[k][i]*c13[k][i] + a2[k][i]*c33[k][i] + a3[k][i]*c35[k][i];
	  tmp3 = a1[k][i]*c15[k][i] + a2[k][i]*c35[k][i] + a3[k][i]*c55[k][i];
	  a1[k][i] = tmp1;
	  a2[k][i] = tmp2;
	  a3[k][i] = tmp3;
	  }
      }
    } else {
      for (k=0; k<nz; k++) {
	for (i=0; i<nx; i++) {
	  theta = a1[k][i] + a2[k][i];
	  a1[k][i] = lambda[k][i]*theta + twmy[k][i]*a1[k][i];
	  a2[k][i] = lambda[k][i]*theta + twmy[k][i]*a2[k][i];
	  a3[k][i] = twmy[k][i]*a3[k][i]*0.5;
	}
      }
    }
    
    /* pressure snapshots */
    for (il=0; il<nsntyp; il++) {
      if (sntyp[il] == 0) {
	for (it=0; it<nsnap; it++) {
	  for (k=0; k<nz; k++) {
	    for (i=0; i<nx; i++)
	      snap[il][it][k][i] += 0.5*(a1[k][i]+a2[k][i])*bessn[it][l];
	  }
	}
      }
    }
    
    /* pressure sections */
    for (il=0; il<nsectx; il++) {
      if (rxtyp[il] == 0) {
	for (it=0; it<nt; it++) {
	  for (i=0; i<nx; i++)
	    xsect[il][it][i] += 0.5*(a1[irz[il]][i]+a2[irz[il]][i])*bestr[it][l];
	}
      }
    }
    for (il=0; il<nsectz; il++) {
      if (rztyp[il] == 1) {
	for (it=0; it<nt; it++) {
	  for (k=0; k<nz; k++)
	    zsect[il][it][k] += 0.5*(a1[k][irx[il]]+a2[k][irx[il]])*bestr[it][l];
	}
      }
    }

    /* --- accellerations --- */
    difx(a1,q2ux,rhoinv,nx,nz,dx,3);
    difz(a3,q2ux,rhoinv,nx,nz,nzpad,dz,3,0);
    
    difz(a2,q2uz,rhoinv,nx,nz,nzpad,dz,3,0);
    difx(a3,q2uz,rhoinv,nx,nz,dx,3);

    if (iabso) {
      tbc2d(qux[0],bwx,bwz,nbwx,nbwz,nx,nz);
      tbc2d(quz[0],bwx,bwz,nbwx,nbwz,nx,nz);
      tbc2d(q2ux[0],bwx,bwz,nbwx,nbwz,nx,nz);
      tbc2d(q2uz[0],bwx,bwz,nbwx,nbwz,nx,nz);
    }

    /* displacement snapshots */
    for (il=0; il<nsntyp; il++) {
      if (sntyp[il] == 2) {
	for (it=0; it<nsnap; it++) {
	  for (k=0; k<nz; k++) {
	    for (i=0; i<nx; i++)
	      snap[il][it][k][i] += qux[k][i]*bessn[it][l];
	  }
	}
      }
      if (sntyp[il] == 3) {
	for (it=0; it<nsnap; it++) {
	  for (k=0; k<nz; k++) {
	    for (i=0; i<nx; i++)
	      snap[il][it][k][i] += quz[k][i]*bessn[it][l];
	  }
	}
      }
    }
    
    /* displacement sections */
    for (il=0; il<nsectx; il++) {
      if (rxtyp[il] == 2) {
	for (it=0; it<nt; it++) {
	  for (i=0; i<nx; i++)
	    xsect[il][it][i] += qux[irz[il]][i]*bestr[it][l];
	}
      }
      if (rxtyp[il] == 3) {
	for (it=0; it<nt; it++) {
	  for (i=0; i<nx; i++)
	    xsect[il][it][i] += quz[irz[il]][i]*bestr[it][l];
	}
      }
    }

    for (il=0; il<nsectz; il++) {
      if (rztyp[il] == 2) {
	for (it=0; it<nt; it++) {
	  for (k=0; k<nz; k++)
	    zsect[il][it][k] += qux[k][irx[il]]*bestr[it][l];
	}
      }
      if (rztyp[il] == 3) {
	for (it=0; it<nt; it++) {
	  for (k=0; k<nz; k++)
	    zsect[il][it][k] += quz[k][irx[il]]*bestr[it][l];
	}
      }
    }
  
  } /* end loop over expansion terms */
    
  if (verbose) {
    getrusage(RUSAGE_SELF, &cput);
    user_mod  = (cput.ru_utime.tv_sec  - user_s_mod) * 1000000;
    user_mod += (cput.ru_utime.tv_usec - user_u_mod);
    fprintf(jpfp,"CPU time for modeling: %.3f seconds\n",
	    (float)user_mod/1000000.);
  }

  /* compose time section file names */
  if (nsectx) {
    xsectfn = (char **) alloc2(100, nsectx, sizeof(char));
    trxfp = calloc(nsectx, sizeof(FILE*));

    for (il=0; il<nsectx; il++) {
      memset(xsectfn[il], 0, 1);
      strcat(xsectfn[il], xsectb);
      strcat(xsectfn[il], typ[rxtyp[il]]);
      sprintf(dum,"_%d.su", il);
      strcat(xsectfn[il], dum);
    }
  } /* endif */

  if (nsectz) {
    zsectfn = (char **) alloc2(100, nsectz, sizeof(char));
    trzfp = calloc(nsectz, sizeof(FILE*));
    
    for (il=0; il<nsectz; il++) {
      memset(zsectfn[il],0,1);
      strcat(zsectfn[il], zsectb);
      strcat(zsectfn[il], typ[rztyp[il]]);
      sprintf(dum,"_%d.su",il);
      strcat(zsectfn[il], dum);
    }
  } /* endif */

  /* compose snapshot file names */
  if (nsntyp) {
    snapfn = (char **) alloc2(100, nsntyp, sizeof(char));

    snfp = calloc(nsntyp, sizeof(FILE*));
    sntracl = ealloc1int(nsntyp);

    for (is=0; is<nsntyp; is++) {
      memset(snapfn[is],0,1);
      strcat(snapfn[is], snapb);
      strcat(snapfn[is], typ[sntyp[is]]);
      sprintf(dum,"_%d.su",is);
      strcat(snapfn[is], dum);
    }
  } /* endif */

  /* common trace headers */
  tro.trid = 1;
  tro.scalco = -10;
  tro.scalel = -10;
  tro.sx = NINT((fx+isx[0]*dx)*10.);
  tro.ns = nt;
  tro.dt = NINT(dt*1000000.);

  /* --- output results to files --- */
  /* write x-direction sections */
  tro.ntr = nx;
  tro.d2 = dx;
  for (il=0; il<nsectx; il++) {
    trxfp[il] = fopen(xsectfn[il],"w");
    tro.fldr = il+1;
    for (i=0; i<nx; i++) {
      tro.tracl = tro.tracr = i+1;
      tro.gx = NINT((fx+i*dx)*10.);
      tro.sdepth = NINT((fz+isz[0]*dz)*10.);
      tro.offset = NINT((tro.gx-tro.sx)/10.);
      for (it=0; it<nt; it++) tro.data[it] = xsect[il][it][i];
      fvputtr(trxfp[il],&tro);
    }
    efclose(trxfp[il]);
  }

  /* write z-direction sections */
  tro.ntr = nz;
  tro.d2 = dz;
  for (il=0; il<nsectz; il++) {
    trzfp[il] = fopen(zsectfn[il],"w");
    tro.fldr = il+1;
    for (k=0; k<nz; k++) {
      tro.tracl = tro.tracr = k+1;
      tro.gx = NINT((fz+k*dz)*10.);
      tro.sdepth = NINT((fz+isz[0]*dz)*10.);
      tro.offset = NINT((tro.gx-tro.sdepth)/10.);
      for (it=0; it<nt; it++) tro.data[it] = zsect[il][it][k];
      fvputtr(trzfp[il],&tro);
    }
    efclose(trzfp[il]);
  }

  /* write snapshots */
  if (nsntyp) {
    /* header for snapshots */
    sno.ns = nz;
    sno.ntr = nx;
    sno.scalco = -10;
    sno.trid = 130; /* depth range */
    sno.scalco = -10;
    sno.scalel = -10;
    sno.sx = NINT((fx+isx[0]*dx)*10.);
    sno.sdepth = NINT((fz+isz[0]*dz)*10.);
    sno.dt = NINT(dtsnap*1000000.);
    sno.d1 = dz;
    sno.d2 = dx;
    for (is=0; is<nsntyp; is++) {
      snfp[is] = fopen(snapfn[is],"w");
      sntracl[is] = 0;

      for (l=0; l<nsnap; l++) {
	sno.fldr = l+1;
	for (i=0; i<nx; i++) {
	  sno.tracl = sno.tracr = ++sntracl[is];
	  sno.gx = NINT((fx+i*dx)*10.);
	  sno.offset = NINT((sno.gx-sno.sx)/10.);
	  for (k=0; k<nz; k++) sno.data[k] = snap[is][l][k][i];
	  fvputtr(snfp[is],&sno);
	}
      }
      efclose(snfp[is]);
    }
  }

  if (verbose) fprintf(jpfp,"finished modeling.\n");
  efclose(jpfp);

  return(CWP_Exit());
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
      if (!ieo || !(k==num-2))
	a[2*i+1] = vin[k+1][i];
      else
	a[2*i+1] = 0.;
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
	if (!ieo || !(i==num-2))
	  a[2*k+1] = vin[k][i+1];
	else
	  a[2*k+1] = 0.;
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
	if (!ieo || !(i==num-2))
	  a[2*k+1] = vin[k][i+1];
	else
	  a[2*k+1] = 0.;
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
    x[i] = con * cos(PI*(4.0*(i+1)-1.0)/(4.0*n+2.));
  
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
