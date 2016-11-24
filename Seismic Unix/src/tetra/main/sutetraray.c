/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTETRARAY: $Revision: 1.5 $ ; $Date: 2011/11/21 16:53:31 $	*/

#include "su.h"
#include "segy.h"
#include "Tetra/tetra.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
"									",
" SUTETRARAY - 3-D TETRAhedral WaveFront construction RAYtracing	",
"									",
"   sutetray < tetrafile [parameters] > ttfile	 			",
" 									",
" 									",
" Optional Parameters:							",	
" nxgd=				number of x samples (fastest dimension)	",
" nygd=				number of y samples (second dimension) 	",
" fxgd=xmin(from tetrafile)	first x sample			  	",
" fygd=ymin(from tetrafile)	first y sample				",
" shootupward=1			=1 shooting upward, -1 downward		",
" dxgd=(xmax-xmin)/(nxgd-1)	x sampling interval		 	",
" dygd=(ymax-ymin)/(nygd-1)	y sampling interval			",
" takeoff=30.0			max takeoff angle for shooting aperture	",
" ntakeoff=5			number of samples in takeoff angle	",
" nazimuth=15			number of samples in azimuth		",
" maxntris=1000			maximum number of triangles allowed	",
" maxnrays=600			maximum number of rays allowed		",
" maxnsegs=100			maximum number of ray segments allowed 	",
" tmax=10			maximum traveltime traced		",
" dtwf=0.05			wavefront step size for raytracing	",
" ntwf=(int)(tmax/dtwf)+1	maximum number of wavefronts	  	",
" ntwf=MAX(ntwf,ifwf2dump+nwf2dump)				     	",
" edgemax=4			maximum triangle length (in dxgd) not to",
"				be split				",
" fsx=fxgd+nxgd/2*dxgd		first source in x			",
" dsx=dxgd			x increment in source			",
" nsx=1				number of source in x			",
" fsy=fygd+nygd/2*dygd		first source in y			",
" dsy=dygd			y increment in source		   	",
" nsy=1				number of source in y		   	",
" fsz=zmin+(zmax-zmin)/2	first source in z			",
" dsy=(zmax-zmin)/2		y increment in source		   	",
" nsy=1				number of source in y			",
" nxt=nxgd*2/2+1	number of x-samples in ttable for sukdmig3d	",	
" nyt=nygd*2/2+1	number of y-samples in ttable for sukdmig3d	",
" irefseq=nhz-1,...     reflector sequence				",
" crfile=NULL	   file for cosines and ray paths r (for sukdmig3d)	",
" sttfile=NULL	  file for surface traveltimes (for visualization)	",
" verbose=0		=1 print some useful information		",
"									",
"	The following two files are output for viewer3 to display   	",
" rayfile=NULL		ray path file		    			",
" wffile=NULL		wavefront file (then must give ifwf2dump)	",
" ifwf2dump=20		the first wavefront to dump  			",
" nwf2dump=1		the number of wavefronts to dump		",
" jpfile=stderr		job print (default: stderr)			",
"									",
" 									",
" Disclaimer:								",
" This is a research code that will take considerable work to get into  ",
" the form of a a production level 3D migration code. The code is       ",
" offered as is, along with tetramod and sukdmig3d, to provide a starting",
" point for researchers who wish to write their own 3D migration codes.",
" 									",
NULL};
/*
 *
 * Credits:
 *  	CWP: Zhaobo Meng, 1996
 *
 * Reference:
 * Zhaobo Meng and Norman Bleistein, Wavefront Construction (WF) Ray
 * Tracing in Tetrahedral Models -- Application to 3-D traveltime and
 * ray path computations, CWP report 251, 1997
 */
/**************** end self doc *******************************************/

/*#define DEBUG*/
#define OUT -99999		/* x and y value is the ray is out-of-domain */
#define EPS 0.00001		/* small number to avoid being divided by 0 */
#define TraveltimeInfinity 9.0  /* traveltime infinity */
#define DistanceInfinity 9000	/* distance infinity */
#define SmallReal 0.1e-5 	/* must be smaller than any slowness */
#define SmallSigma 0.0001	/* smallest sigma value for comparison */
#define SigmaInfinity 1.0e+10
#define MaxOnce 1000
#define Margin 0.1              /* some margin for which_tetra */

segy tro;

struct WF
{
      float x[3];   /* intersection */
      float p[3];   /* slowness vector at intersection */	
};

struct TRI
{
      int i1;	        /* index for first vertex */
      int i2;           /* index for second vertex */
      int i3;           /* index for third vertex */
      struct TRI *next;	/* pointer to next tri */
};

enum ICODE {NORMAL,SPOILED,HIT_SURFACE};

struct RAY
{
      enum ICODE icode;
      int itetra;     /* tetra in which this ray now travels */
      int lastfacet;  /* last facet that this ray penetrates */
      int ionce;      /* number of adjustments applied when cornered */
      int iref;       /* index for reflections */
      int shootupward;/* 1 upward, -1 downward */
      int nseg;       /* number of segments */
      float t;        /* current traveltime within dt */
      float x[3];     /* cuttent ray position */
      float ttotal;   /* total traveltime from source */
      float r;        /* total distance traveled from source */
      float v;        /* velocity at current point */
      float **xseg;   /* segment positions */
      float *tseg;
      float p[3];
};

enum RESULT {HIT_FACET_FIRST,
      HIT_WF_FIRST,
      HIT_SURFACE_FIRST,
      OUT_OF_MODEL};

enum CHECK_TRIS {PLEASE,PLEASE_DO_NOT};

enum DONE {ALREADY,NOT_YET};

enum IN_TRI{WF_YES,WF_NO};

enum ENOUGH_NTWF{WF_TRUE,WF_FALSE};
/* Prototypes of functions used internally */
void crossprod(float u1, float u2, float u3, float v1, float v2,
		 float v3, float *w1, float *w2, float *w3);
float tetrasolver(float ct[10], float p[3], float gs[3],
			float xst[3],float sg0); 
int in_which_tetra(float xmin, float ymin, float xmax,
			float ymax, float zmax, float x[3],
			 struct POINT *point, struct TETRA *tetra,
			 int ntetra);
float tri_intp(float x, float y, float x0, float y0, float t0,
		float x1, float y1, float t1, float x2, float y2, float t2); 
void tri_multi_intp( struct RAY *ray0, struct RAY *ray1, struct RAY *ray2,
		float fxgd, float fygd, float dxgd, float dygd,
		int nxgd, int nygd, float **ttable, float **ctable,
		 float **rtable);
enum IN_TRI in_tri(float x0, float y0, float x1, float y1, float x2,
		float y2, float x, float y, float areat);
float quadsolver(float a,float b,float c);
float cubicsolver(float d,float a,float b,float c);

enum RESULT trace1tetra( int maxnsegs, float xmin, float ymin, float xmax,
      float ymax, float zmax, struct RAY *ray, int npoint,
      struct POINT *point, int nfacet, struct FACET *facet,
      int ntetra, struct TETRA *tetra, int *irefseq,
      int nrefseq, FILE *jpfp, float dt);  

int    
oneshot(int shootupward, int maxnsegs, int npoint, struct POINT *point,
        int nfacet, struct FACET *facet, int ntetra, struct TETRA *tetra, 
        int *irefseq, int nrefseq, FILE *jpfp, float takeoff, float dtwf,
        int ntwf, int maxntris, int maxnrays, float edgemax, 
        int nxt, int nyt, int ixtf, int iytf, float source[3], int ntakeoff,
        int nazimuth, float fxgd, float dxgd, int nxgd, float fygd, 
        float dygd, int nygd, float xmin, float ymin, float xmax, float ymax,
        float zmax, FILE *rayfp, FILE *wffp, FILE *crfp, FILE *sttfp,
        int ifwf2dump, int nwf2dump);
/*******************************************************************
Inputs:
int shootupward flag: shootupward if =1 else downward
int maxnsegs    max number of ray segments
int npoint      number of point
struct *point   control points
int nfacet      number of facet
struct *facet   facet information
int ntetra      number of tetra
struct *tetra   tetra information
int *irefseq    reflector sequence
int nrefseq     number of reflectors
FILE *jpfp	file pointer to the job print
float takeoff   maximum takeoff angle
float dtwf        time step for wavefront updating
int ntwf          total number of time steps allowed
int maxntris    maximum number of triangles allowed
int maxnrays    maximum number of rays allowed
float edgemax   maximum edge length in dxgd
int nxt         number of x-samples in ttable for sukdmig3d
int nyt         number of y-samples in ttable for sukdmig3d
int nxtl        number of samples in ttable on left of source
int nytf        number of samples in ttable in front of source
float source[3] source position
int ntakeoff    number of samples in azimuth
int nazimuth    number of take-off angles
float fxgd      first sample in x
float dxgd      sample interval in x
int nxgd        number of sampels in x
float fygd      first sample in y
float dygd      sample interval in y
int nygd        number of sampels in y
FILE *rayfp     file pointer to rayfile
FILE *wffp      file pointer to wavefront file
int ifwf2dump   the first wavefront to dump
int nwf2dump    the number of wavefronts to dump

Output:         none
******************************************************************/

float f(float sigma, float s, float p, float a);
float zhorz( float xx, float yy, float n10, float n20, float n30,
      float x00, float y00, float z00);
void WF_normalize(float *n1,float *n2,float *n3);
void read_point( struct POINT *point, int npoint);
void read_facet( struct FACET *facet, int nfacet);
void read_tetra( struct TETRA *tetra, int ntetra);
float s_intp( struct POINT *point, struct TETRA *tetra, float x[3]);
float area( float x0, float y0, float x1, float y1, float x2, float y2);
void cross_slim_tetra( struct RAY *ray, struct POINT *point,
		 struct FACET *facet, struct TETRA *tetra, int *irefseq,
		int nrefseq, float sincid, FILE *jpfp); 
float facet_fit( struct POINT *point, float x[3], struct FACET *facet);
enum RESULT RTlaw( struct RAY *ray, struct POINT *point, struct FACET *facet,
      struct TETRA *tetra, int *irefseq, int nrefseq, int ifhit, float sincid,
      int itetranew, FILE *jpfp);
void s_normalize( float p[3], float s);

int 
main(int argc, char **argv)
{
      int shootupward;      /* flag: 1 upward, -1 downward */
      int npoint;           /* number of control points */
      struct POINT *point=NULL;/* control points */
      int nfacet;           /* number of facets */
      struct FACET *facet=NULL;/* facet information */
      int ntetra;           /* number of tetra */
      struct TETRA *tetra=NULL;/* tetra information */

      int nygd;    	    /* number of samples in y direction */
      int nxgd; 	    /* number of samples in x direction */

      /**********************************************************
      These four parameters are used for defining the ttable 
      size for sukdmig3d
      **********************************************************/
      int nxt;	      /* number of x-samples in ttable for sukdmig3d */
      int nyt;	      /* number of y-samples in ttable for sukdmig3d */
      int ixtf;       /* number of samples in ttable on left of source */
      int iytf;       /* number of samples in ttable in front of source */
      int ntakeoff;   /* number of samples in take-off */
      int nazimuth;   /* number of samples in azimuth */

      float fygd;     /* first sample in y */
      float fxgd;     /* first sample in x */

      float fsx;      /* first source in x */
      float fsy;      /* first source in y */
      float fsz;      /* first source in z */

      float dsx;      /* x increment of sources */
      float dsy;      /* y increment of sources */
      float dsz;      /* z increment of sources */

      int nsx;	      /* number of sources in x direction */
      int nsy;	      /* number of sources in y direction */ 
      int nsz;        /* number of sources in z direction */

      int *irefseq;   /* reflector sequence */

      float dygd;     /* y sample interval */
      float dxgd;     /* x sample interval */

      float dtwf;	      /* time step in raytracing */
      int ntwf;	      /* number of time steps in raytracing */
      float tmax;     /* maximum time traced */

      char *rayfile="";/* file for ray information */
      char *wffile="";
      char *crfile="";
      char *sttfile="";
      char *jpfile="";/* file for job print */

      FILE *rayfp=NULL;
      FILE *rayfp0=NULL;
      FILE *crfp=NULL;
      FILE *sttfp=NULL;
      FILE *wffp=NULL;
      FILE *jpfp=stderr;

      int ifwf2dump;
      int nwf2dump;

      int nrefseq;
      int ns;         /* number of sources */

      int maxntris;   /* maximum number of triangles allowed */
      int maxnrays;   /* maximum number of rays allowed */
      float edgemax;  /* maximum edge length in dxgd */
	
      int verbose;    /* if =1 print some useful information */

      float source[3];/* source position */

      int isx;	      /* source index for nsx */
      int isy;	      /* source index for nsy */
      int isz;        /* source index for nsz */

      int ixsf;       /* first source in gd */
      int ixsr;       /* source spacing and gd spacing ratio */
      int iysf;       /* first source in gd */
      int iysr;       /* source spacing and gd spacing ratio */

      float xmin,xmax,ymin,ymax,zmin,zmax;

      float takeoff;  /* max takeoff angle */

      int iref,maxnsegs;

      /* hook up getpar */
      initargs(argc,argv);
      requestdoc(1);

      /* get parameters */
      if (!getparint("verbose",&verbose)) verbose=0;

      if (!getparint("shootupward",&shootupward)) shootupward=1;

      if (getparstring("jpfile",&jpfile))
	    if ((jpfp=fopen(jpfile,"w"))==NULL)
		 jpfp=stderr;
		 
      if (!getparfloat("takeoff",&takeoff)) takeoff=30.0;
      takeoff=takeoff/180.0*PI;

      nrefseq=countparval("irefseq");
      irefseq=alloc1int(nrefseq);
      getparint("irefseq",irefseq);

      if (verbose)
            for (iref=0;iref<nrefseq;iref++)
                  fprintf(jpfp,"irefseq[%d]=%d\n",iref,irefseq[iref]);

      fscanf(stdin,"%f=xmin\n",&xmin);
      fscanf(stdin,"%f=xmax\n",&xmax);
      fscanf(stdin,"%f=ymin\n",&ymin);
      fscanf(stdin,"%f=ymax\n",&ymax);
      fscanf(stdin,"%f=zmin\n",&zmin);
      fscanf(stdin,"%f=zmax\n",&zmax);

      if (verbose)
	    fprintf(jpfp,"xmin=%f\nxmax=%f\nymin=%f\nymax=%f\nzmin=%f\nzmax=%f\n",
		  xmin,xmax,ymin,ymax,zmin,zmax);

      if (!getparint("nxgd",&nxgd)) nxgd=21;
      if (nxgd<2) err("nxgd too small");
      nxgd=(nxgd/2)*2+1;

      if (!getparfloat("fxgd",&fxgd)) fxgd=xmin;
      if (fxgd<xmin) err("fxgd too small");
      if (!getparfloat("dxgd",&dxgd)) dxgd=(xmax-xmin)/(nxgd-1);
      if (fxgd+(nxgd-1)*dxgd>xmax*1.01) {
            fprintf(stderr,"fxgd=%f nxgd=%d dxgd=%f xmax=%f\n",
            fxgd,nxgd,dxgd,xmax);
            err("fxgd+(nxgd-1)*dxgd too large");
      }

      if (!getparint("nygd",&nygd)) nygd=21;
      if (nygd<2) err("nygd too small");
      nygd=(nygd/2)*2+1;

      if (!getparfloat("fygd",&fygd)) fygd=ymin;
      if (fygd<ymin) err("fygd too small");
      if (!getparfloat("dygd",&dygd)) dygd=(ymax-ymin)/(nygd-1);
      if (fygd+(nygd-1)*dygd>ymax*1.01) err("fygd+(nygd-1)*dygd too large");

      if (verbose) {
	    fprintf(jpfp,"shootupward=%d\n",shootupward);
            fprintf(jpfp,"nxgd=%d\nnygd=%d\n",nxgd,nygd);
            fprintf(jpfp,"dxgd=%f\ndygd=%f\n",dxgd,dygd);
            fprintf(jpfp,"fxgd=%f\nfygd=%f\n",fxgd,fygd);
      }

      if (getparstring("rayfile",&rayfile))
	    if ((rayfp=fopen(rayfile,"w"))==NULL)
	      	  err("Can not open rayfile to write");

      if (getparstring("wffile",&wffile))
            if ((wffp=fopen(wffile,"w"))==NULL)
                  err("Can not open wffile to write");

      if (wffp!=NULL) {
	    if (!getparint("ifwf2dump",&ifwf2dump)) ifwf2dump=20;
            if (!getparint("nwf2dump",&nwf2dump)) nwf2dump=1;
      }

      if (verbose) 
	    fprintf(jpfp,"ifwf2dump=%d, nwf2dump=%d\n",
		  ifwf2dump,nwf2dump);

      if (getparstring("crfile",&crfile))
            if ((crfp=fopen(crfile,"w"))==NULL)
                  err("Can not open crfile to write");

      if (getparstring("sttfile",&sttfile))
            if ((sttfp=fopen(sttfile,"w"))==NULL)
                  err("Can not open sttfile to write");

      if (!getparint("maxntris",&maxntris))
	    maxntris=1000;
	
      if (!getparint("maxnrays",&maxnrays))
            maxnrays=600;

      if (!getparint("maxnsegs",&maxnsegs))
            maxnsegs=100;

      if (!getparfloat("edgemax",&edgemax))
	    edgemax=4;

      if (!getparfloat("fsx",&fsx)) fsx=fxgd+nxgd/2.0*dxgd;
      if (!getparfloat("fsy",&fsy)) fsy=fygd+nygd/2.0*dygd;

      if (!getparint("nsx",&nsx)) nsx=1;
      if (!getparint("nsy",&nsy)) nsy=1;
      if (!getparint("nsz",&nsz)) nsz=1;

      ns=nsx*nsy;

      if (!getparint("nxt",&nxt)) nxt=(nxgd/2)*2+1;
      if (!getparint("nyt",&nyt)) nyt=(nygd/2)*2+1;
      nxt=(nxt/2)*2+1;
      nyt=(nyt/2)*2+1;

      if (!getparfloat("dsx",&dsx)) dsx=dxgd;
      if (!getparfloat("dsy",&dsy)) dsy=dygd;
      if (!getparfloat("dsz",&dsz)) dsz=(zmax-zmin)/2;

      ixsf=(fsx-fxgd)/dxgd;
      if (ixsf<nxt/2) err("ixsf=%d should >= nxt/2=%d. Use larger fsx or smaller nxt",
	    ixsf,nxt/2);

      ixsr=MAX(1,dsx/dxgd);
      if (ixsf+ixsr*nsx+nxt/2 > nxgd) 
            err("ixsf+ixsr*nsx+nxt/2=%d should <= nxgd=%d",
                  ixsf+ixsr*nsx+nxt/2,nxgd);

      iysf=(fsy-fygd)/dygd;
      if (iysf<nyt/2) err("iysf=%d should >= nyt/2=%d. Use larger fsy or smaller nyt",
            iysf,nyt/2);

      iysr=MAX(1,dsy/dygd);
      if (iysf+iysr*nsy+nyt/2 > nygd) 
            err("iysf+iysr*nsy+nyt/2=%d should <= nygd=%d",
                  iysf+iysr*nsy+nyt/2,nygd);

      fsx=fxgd+ixsf*dxgd;
      fsy=fygd+iysf*dygd;
      fsx+=0.01*dxgd;
      fsy+=0.01*dygd;

      dsx=ixsr*dxgd;
      dsy=iysr*dygd;

      if (!getparfloat("fsz",&fsz)) fsz=zmin+(zmax-zmin)/2.0;

      if (!getparint("ntakeoff",&ntakeoff)) ntakeoff=5;
      if (!getparint("nazimuth",&nazimuth)) nazimuth=15;

      if (!getparfloat("tmax",&tmax)) tmax=10.0;

      if (!getparfloat("dtwf",&dtwf)) dtwf=0.05;

      if (!getparint("ntwf",&ntwf)) {
            ntwf=(int)(tmax/dtwf)+1;
            ntwf=MAX(200,ntwf);
      }

      checkpars();

      if (ntwf<ifwf2dump+nwf2dump) {
            fprintf(jpfp,"ntwf < ifwf2dump+nwf2dump, reset=%d\n",
                  ifwf2dump+nwf2dump);
            ntwf=ifwf2dump+nwf2dump;
      }

      if (ntwf>2000) err("ntwf>2000, too large");

      if (verbose) {
	    fprintf(jpfp,"nxgd=%d\nnygd=%d\ndxgd=%g\n",
		  nxgd,nygd,dxgd);
	    fprintf(jpfp,"dygd=%g\nntakeoff=%d\nnazimuth=%d\n",
		  dygd,ntakeoff,nazimuth);
            fprintf(jpfp,"takeoff=%f\n",takeoff);
	    fprintf(jpfp,
		  "fsx=%g,dsx=%g,nsx=%d\nfsy=%g,dsy=%g,nsy=%d\nfsz=%g,dsz=%g,nsz=%d\n",
		  fsx,dsx,nsx,fsy,dsy,nsy,fsz,dsz,nsz);
	    fprintf(jpfp,"nxt=%d\nnyt=%d\n",nxt,nyt);
	    fprintf(jpfp,"maxntris=%d\nmaxnrays=%d\nmaxnsegs=%d\nedgemax=%g\n",
		  maxntris,maxnrays,maxnsegs,edgemax);
	    fprintf(jpfp,"ntwf=%d,dtwf=%g,tmax=%g\n",ntwf,dtwf,tmax);
      }

      if (nxt<0 || nxt>nxgd) err("nxt must be >=0 <=nxgd");
      if (nyt<0 || nyt>nygd) err("nyt must be >=0 <=nygd");

      if ((fsx+(nsx-1)*dsx>fxgd+(nxgd-1)*dxgd) ||
          (fsy+(nsy-1)*dsy>fygd+(nygd-1)*dygd) ||
	  (fsx<fxgd) || (fsy<fygd)) 
	    err("Source out of medium");

      /* number of sources */
      tro.nhs=nsx;
      tro.nvs=nsy;
      tro.duse=nsz;

      /* first source */
      tro.sdel=ixsf;
      tro.gdel=iysf;
      tro.sdepth=(int)(fsz*1000.0);

      /* source spacing */
      tro.swdep=ixsr;
      tro.gwdep=iysr;
      tro.ep=(int)(dsz*1000.0);

      /* first sample */
      tro.f1=fxgd;
      tro.f2=fygd;

      /* model size: nxgd x nygd */
      tro.gelev=nygd;
      tro.selev=nxgd;

      /* traveltime sample spacing */
      tro.d1=dxgd;
      tro.d2=dygd;

      /* number of traces and samples in each trace */
      tro.ns=nxt;
      tro.ntr=nyt;

      fscanf(stdin,"%d=npoint : x[3] : s : n[3]\n",&npoint);
      fprintf(jpfp,"npoint=%d\n",npoint);
      point=(struct POINT *)malloc(npoint*sizeof(struct POINT));   
      read_point(
      point,
      npoint);
      fprintf(jpfp,"point %d read\n",npoint);

      fscanf(stdin,"%d=nfacet : area : ct[10] : ip[3] : cn[3] : itetra[2]\n",
	    &nfacet);
      fprintf(jpfp,"nfacet=%d\n",nfacet);
      facet=(struct FACET *)malloc(nfacet*sizeof(struct FACET));      
      read_facet(
            facet,
            nfacet);
            fprintf(jpfp,"facet %d read\n",nfacet);

      fscanf(stdin,"%d=ntetra : v : ip[4] : ifacet[4] : ireg : gs[3]\n",&ntetra);
      fprintf(jpfp,"ntetra=%d\n",ntetra);
      tetra=(struct TETRA *)malloc(ntetra*sizeof(struct TETRA));
      
      read_tetra(
            tetra,
            ntetra);
            fprintf(jpfp,"tetra %d read\n",ntetra);

      if (rayfp!=NULL)
            fprintf(rayfp,"%d =Number of shots\n",ns);

      for (isy=0;isy<nsy;isy++) {
	    source[1]=fsy+isy*dsy;
            iytf=iysf+iysr*isy-nyt/2;
	    for (isx=0;isx<nsx;isx++) {
		  source[0]=fsx+isx*dsx;
                  ixtf=ixsf+ixsr*isx-nxt/2;
                  for (isz=0;isz<nsz;isz++) {
	                source[2]=fsz+isz*dsz;
			if (isz<nsz-1) rayfp0=NULL;
			else rayfp0=rayfp;
			/*******************************************************
                        Usually it is not every good to put the source right on
                        a facet, this is because the raytracer may not stably
                        find the tetrahetron that the source lies in. So addition 
			of EPS will help.
			********************************************************/

			/* shooting one shot */
			oneshot(
			      shootupward,
                              maxnsegs,
                              npoint,
                              point,
                              nfacet,
			      facet,
			      ntetra,
			      tetra,
                              irefseq,/* reflector sequence */
                              nrefseq,/* number of reflectors */
			      jpfp,
			      takeoff,/* max takeoff angle */
			      dtwf,	/* time step */
			      ntwf,	/* maximum number of steps */
		              maxntris,/* maximum number of triangles allowed */
        		      maxnrays,/* maximum number of rays allowed */
        		      edgemax,/* maximum edge length in dxgd */
			      nxt,	/* # of x-samples in ttable for sukdmig3d */
			      nyt,	/* # of y-samples in ttable for sukdmig3d */
			      ixtf,	/* # of samples in ttable on left of source */
			      iytf,	/* # of samples in ttable in front of source */
			      source,   /* source position */
			      ntakeoff, /* number of samples in azimuth */
			      nazimuth, /* number of take-off angles */
			      fxgd,     /* first sample in x */
			      dxgd,	/* sample interval in x */
			      nxgd,	/* number of sampels in x */
			      fygd,	/* first sample in y */
			      dygd,	/* sample interval in y */
			      nygd,	/* number of sampels in y */
			      xmin,ymin,xmax,ymax,zmax,
			      rayfp0,	/* file pointer to rayfile */
			      wffp,
			      crfp,
			      sttfp,
			      ifwf2dump,
                              nwf2dump);

                        fprintf(stderr,"shot (%d %d %d) is written\n",
                              isx,isy,isz);
			if (wffp!=NULL) 
				fclose(wffp);
			wffp=NULL;
		  }
       	    }
      }

      if (rayfp!=NULL) fclose(rayfp);
      if (jpfp!=stderr) fclose(jpfp);

      return EXIT_SUCCESS;
}
void WF_normalize(float *n1,float *n2,float *n3)
/**********************************************************************
WF_normalize - normalize vector n1,n2,n3
**********************************************************************
Inputs/outputs:
float *n1	first component of the vector to be normalized to 1
float *n2       second component of the vector to be normalized to 1
float *n3       the third component of the vector to be normalized to 1
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
******************************************************************/
{
        float norm;
        norm=*n1**n1+*n2**n2+*n3**n3;
        norm=sqrt(norm);
        *n1=*n1/norm;
        *n2=*n2/norm;
        *n3=*n3/norm;
}

int    
oneshot(int shootupward, int maxnsegs, int npoint, struct POINT *point,
        int nfacet, struct FACET *facet, int ntetra, struct TETRA *tetra, 
        int *irefseq, int nrefseq, FILE *jpfp, float takeoff, float dtwf,
        int ntwf, int maxntris, int maxnrays, float edgemax, 
        int nxt, int nyt, int ixtf, int iytf, float source[3], int ntakeoff,
        int nazimuth, float fxgd, float dxgd, int nxgd, float fygd, 
        float dygd, int nygd, float xmin, float ymin, float xmax, float ymax,
        float zmax, FILE *rayfp, FILE *wffp, FILE *crfp, FILE *sttfp,
        int ifwf2dump, int nwf2dump)  
/*******************************************************************
Inputs:
int shootupward 1 upward, -1 downward
int maxnsegs    max number of ray segments
int npoint      number of point
struct *point   control points
int nfacet      number of facet
struct *facet   facet information
int ntetra      number of tetra
struct *tetra   tetra information
int *irefseq    reflector sequence
int nrefseq     number of reflectors
FILE *jpfp	file pointer to the job print
float takeoff   maximum takeoff angle
float dtwf        time step for wavefront updating
int ntwf          total number of time steps allowed
int maxntris    maximum number of triangles allowed
int maxnrays    maximum number of rays allowed
float edgemax   maximum edge length in dxgd
int nxt         number of x-samples in ttable for sukdmig3d
int nyt         number of y-samples in ttable for sukdmig3d
int ixtf        number of samples in ttable on left of source
int iytf        number of samples in ttable in front of source
float source[3] source position
int ntakeoff    number of samples in azimuth
int nazimuth    number of take-off angles
float fxgd      first sample in x
float dxgd      sample interval in x
int nxgd        number of sampels in x
float fygd      first sample in y
float dygd      sample interval in y
int nygd        number of sampels in y
FILE *rayfp     file pointer to rayfile
FILE *wffp      file pointer to wavefront file
int ifwf2dump   the first wavefront to dump
int nwf2dump    the number of wavefronts to dump

Output:         none
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
******************************************************************/
{
      struct WF *wf;

      /*******************************************************
      The traveltime table start from ixgdl, end with ixgdl+nxt-1 in x 
      and start with iygdf and end with iygdf+nyt-1
      *******************************************************/
      float azimuth;   	/* azimuth angle */
      float sintakeoff;	/* sin(takeoff) */
      float cosazimuth;	/* cos(azimuth) */
      float costakeoff;	/* cos(takeoff) */
      float sinazimuth;	/* sin(azimuth) */
      float s;	        /* slowness */
      int itakeoff;	/* index for azimuth */
      int iazimuth;	/* index for takeoff */

      float dazimuth;	/* azimuth increment */
      float dtakeoff;   /* takeoff increment */
      int ithis;	/* index for this ray */
      int inext;
      int itwf;		/* index for t */
      float distmax=0;	/* the maximum edge of the triangle */
      int iedgemax;	/* the index for the largest triangle */
      int iedgemax0;
      int iedge;	/* index for iedgemax */

      enum ENOUGH_NTWF enough_ntwf=WF_TRUE;

      int i1,i2,i3;	
      int ii1,ii2,ii3;
      int iray;	/* index for rays */

      struct TRI head;	        /* first triangle */
      struct TRI *headp=NULL;	/* pointer to the first usable triangle */
      struct TRI *trip;	        /* pointer to tris */
      struct TRI *trip0;        /* runing pointer */
      struct TRI *itrip;	/* index for trip */
      struct TRI *temtrip;	/* temporary pointer to triangles */

      int nrays=0;	        /* number of rays */
      int ngoodrays;
      int ntris=0;	        /* number of triangles */

      float dist[3];

      int ixgd,iygd;
      float x0,x1,x2;
      float y0,y1,y2;
      float z0,z1,z2;

      float xvertex0,xvertex1;
      float yvertex0,yvertex1;
      float zvertex0,zvertex1;

      int initreg;
      int ntr;
      int initetra;
      int tripi0,tripi1;

      float **ttable;        /* traveltime table */
      float **ctable;
      float **rtable;
      enum CHECK_TRIS check_tris=PLEASE;
      enum RESULT result;
      enum DONE done;
      struct RAY *ray;

      float temp,sqrts,tsum;

      int iseg;
      float adjust;     /* when interpolating, ajust this amount */
      float r,d,d2,sintheta,costheta;

      ray=(struct RAY *)
	    malloc(maxnrays*sizeof(struct RAY));

      fprintf(jpfp,"source at %f %f %f\n",source[0],source[1],source[2]);

      ttable=ealloc2float(nxgd,nygd);
      ctable=ealloc2float(nxgd,nygd);
      rtable=ealloc2float(nxgd,nygd);

      for (ixgd=0;ixgd<nxgd;ixgd++)
            for (iygd=0;iygd<nygd;iygd++)
                   ttable[iygd][ixgd]=TraveltimeInfinity;

      memset((void *)ctable[0],0,
            nxgd*nygd*sizeof(float));
      memset((void *)rtable[0],0,
            nxgd*nygd*sizeof(float));

      /* the ray information is bookkept on horizons */
      wf=(struct WF *) 
	    malloc(sizeof(struct WF)*maxnrays*ntwf);

      initetra=in_which_tetra(
            xmin,
            ymin,
            xmax,
            ymax,
            zmax,
	    source,    /* source position */
	    point,     /* control points */
            tetra,     /* tetra array */
            ntetra);   /* number of tetra */

      if (initetra<0) err("Source out of model");
      fprintf(jpfp,"source in tetra %d\n",initetra);

      initreg=tetra[initetra].ireg;
      fprintf(jpfp,"initreg=%d\n",initreg);
	
      /*********************************************
      Initialize the ray position
      to be the source position.
      ********************************************* */
      for (iray=0;iray<maxnrays;iray++) {
      	    wf[iray].x[0]=source[0];
	    wf[iray].x[1]=source[1];
	    wf[iray].x[2]=source[2];
            ray[iray].xseg=ealloc2float(3,maxnsegs);
            ray[iray].tseg=ealloc1float(maxnsegs);
            for (iseg=0;iseg<maxnsegs;iseg++)
                  ray[iray].tseg[iseg]=0.0;
	    ray[iray].icode=NORMAL;
            ray[iray].shootupward=shootupward;
	    ray[iray].itetra=initetra;
            ray[iray].lastfacet=-1;
            ray[iray].ionce=0;
            ray[iray].iref=0;
	    ray[iray].nseg=0;
            ray[iray].r=0.0;
            ray[iray].ttotal=0.0;
      }

      for (iray=0;iray<nazimuth*ntakeoff+1;iray++) {
            wf[iray].x[0]=source[0];
	    wf[iray].x[1]=source[1];
	    wf[iray].x[2]=source[2];
	    ray[iray].xseg[0][0]=source[0];
	    ray[iray].xseg[0][1]=source[1];
            ray[iray].xseg[0][2]=source[2];
      }

      for (itwf=1;itwf<ntwf-1;itwf++) {
	     for (iray=0;iray<maxnrays;iray++) {
		   ithis=itwf*maxnrays+iray;
		   wf[ithis].x[0]=OUT;
		   wf[ithis].x[1]=OUT;
		   wf[ithis].x[2]=OUT;
	     }
      }

      azimuth=2.0*3.1415926;
      dazimuth=azimuth/nazimuth;
      dtakeoff=takeoff/ntakeoff;

      /*********************************************************
      The maximum number of rays must not be more than maxnrays
      *********************************************************/
      if (ntakeoff*nazimuth>=maxnrays) 
      	    err("too many rays: (ntakeoff*nazimuth=)%d>=(maxnrays=%d",
		  ntakeoff*nazimuth,maxnrays);

      done=NOT_YET;

      s=s_intp(
	    point,             /* control points */
            &tetra[initetra],  /* this tetra */
            source);           /* the current position */

      #ifdef DUBUG
      fprintf(jpfp,"sloth at the source=%f\n",s);
      #endif

      sqrts=sqrt(s);

      for (itwf=0;itwf<ntwf;itwf++) {
	    ithis=itwf*maxnrays;
	    inext=(itwf+1)*maxnrays;

            if (done==ALREADY) break;

	    if (itwf==0) {
		  /*****************************************
		  Pointing to the first triangle
		  **************************************** */
		  headp=&head;
		  /********************************************
		  There are nazimuth*ntakeoff+1 rays starting for each shot
		  ********************************************/
		  for (iray=0;iray<=nazimuth*ntakeoff;iray++) {
			/************************************
	    	      	Ray 0 is the center; takeoff is the 
		       	take-off angle; azimuth is the azimuth
		       	************************************/
		       	if (iray==0) {
                              itakeoff=0;
			      sintakeoff=0.0;
                              costakeoff=1.0;
			} else {
                              itakeoff=1+(iray-1)/nazimuth;
                              temp=dtakeoff*itakeoff;
	       		      sintakeoff=sin(temp);
	       	              costakeoff=cos(temp);
                        }

		       	/***************************************
		       	Index for azimuth
		       	****************************************/
		       	if (iray==0) {
                              iazimuth=0;
			      sinazimuth=0.0;
                              cosazimuth=1.0;
		      	} else {	
                              iazimuth=(iray-1)%nazimuth;
	  		      temp=(iazimuth+0.5*(itakeoff-1))*dazimuth+0.1;
		              sinazimuth=sin(temp);
		       	      cosazimuth=cos(temp);
                        }

		       	/***************************************
		       	the equation of the slowness vector is
		       	px^2 + py^2 + pz^2 = s,
		       	there are two freedoms.
		       	***************************************/
		       	wf[iray].p[0]=sintakeoff*cosazimuth*sqrts;
		       	wf[iray].p[1]=sintakeoff*sinazimuth*sqrts;
                        /* If the rays are shot upward, put -1 here; 
                        otherwise, put +1 here */
			if (shootupward==1)
                              wf[iray].p[2]=-costakeoff*sqrts;
                        else
		      	      wf[iray].p[2]=costakeoff*sqrts;
	       	  }

		  /******************************************
		  At first step, we have nazimuth*ntakeoff+1 rays,
		  which form (2*ntakeoff-1)*nazimuth triangles.
			
		  		4   3   2	
				 \ 3|2 /
				  \ | /
				 4 \|/ 1
			      5-----0-----1
				 5 /|\ nazimuth-1
				  / | \ 
				 / 6|  \
				6    nazimuth
		  ******************************************/
		  head.i1=0;
		  head.i2=1;
		  head.i3=2;

		  trip=headp;
		  for (iray=2;iray<=nazimuth;iray++) {
		       	trip->next=(struct TRI*) 
		      	      malloc(sizeof(struct TRI));
			trip=trip->next;
		      	trip->i1=0;
		       	trip->i2=iray;
		       	trip->i3=iray%nazimuth+1;
		  }

		  for (iazimuth=0;iazimuth<ntakeoff-1;iazimuth++) {
		       	for (itakeoff=0;itakeoff<nazimuth;itakeoff++) {
		       	      trip->next=(struct TRI*) 
			      malloc(sizeof(struct TRI));
			      trip=trip->next;
			      trip->i1=itakeoff+1+iazimuth*nazimuth;
			      trip->i2=(itakeoff+1)%nazimuth+1
                                    +iazimuth*nazimuth;
			      trip->i3=trip->i1+nazimuth;
			      trip->next=(struct TRI*) 
			       	    malloc(sizeof(struct TRI));
			      trip=trip->next;
				    trip->i1=(itakeoff+1)%nazimuth+1
                                           +iazimuth*nazimuth;
			      trip->i2=itakeoff+1+(iazimuth+1)*nazimuth;
			      trip->i3=trip->i1+nazimuth;
		        }
		  }

		  trip->next=(struct TRI*)NULL;
		  nrays=nazimuth*ntakeoff+1;
		  ntris=(2*ntakeoff-1)*nazimuth;
		  fprintf(jpfp,"Starting: ntrips=%d nrays=%d\n",
		       	ntris,nrays);
	     } else {
	          /****************************************************
		  Test if the head is spoiled. If the first ray is spoiled,
                  the next ray will take the place; All triangles connected
                  with these rays need to be removed;
		  ****************************************************/
		  while ((ray[headp->i1].icode==SPOILED ||
			  ray[headp->i2].icode==SPOILED ||
			  ray[headp->i3].icode==SPOILED) && 
			       ntris>1) {
			  headp=headp->next;
			  ntris--;
			  fprintf(jpfp,
			       	"head spoiled, cut off: ntris=%d\n",ntris);
		  }

		  if (ntris<2) break;
			

                  /****************************************************
                  Need to test if all triangles are still good. Loop over
                  all triangle, to see is all three rays of which the
                  triangle is composed to be NORMAL.
	      	  ****************************************************/
		  for (trip=headp;;) { 

			/* make sure that next triangle is not the end*/
			if (trip->next==(struct TRI*)NULL)
		       	      break;

                        /* if next triangle is good, then continue to
                        test the next */
			if (  ray[trip->next->i1].icode!=SPOILED && 
		       	      ray[trip->next->i2].icode!=SPOILED && 
			      ray[trip->next->i3].icode!=SPOILED) {
				    trip=trip->next;
				    continue; 
			 }

                         /************************************************
                         if the program execuates here, it means that
                         triangle trip is good; but trip->next is bad.
                         We need to cut off this triangle. What could 
                         happen to the one or more rays of this this triangle
                         for this misfortune to happen?
                         ************************************************/
		      	 if (ntris<1) break;

			 ntris--;

                         /***********************************************
                         trip is good; but trip->next is bad. We need to see
                         if trip0->next->next is also bad, or even further.
                         In these case, we need to cut off all the consecutive
                         bad triangles.
                         ************************************************/
		       	 for (itrip=trip->next;;itrip=itrip->next) {

		               /* bad to the very end */
		       	       if (itrip->next==(struct TRI*)NULL)
			       	     break;	
			       /* good. the bad chain has stopped */
		               if (ray[itrip->next->i1].icode!=SPOILED &&
				   ray[itrip->next->i2].icode!=SPOILED &&
				   ray[itrip->next->i3].icode!=SPOILED) 
                                         break;

			       ntris--;
                               free(itrip);
		       	 }

                         /* now: both trip and trip->next are good ones */
		       	 trip->next=itrip->next;
		   }
                   /* from headp to the end, there are ntris triangles
                   they are all good ones. */	

                  /* to dump the wavefront */
		  if (wffp!=NULL && itwf==ifwf2dump)
		        fprintf(wffp,"%d = nwf2dump\n",nwf2dump);

	       	  /****************************************************
		  Test if we need to add some triangles
		  ****************************************************/
		  for (trip=headp;trip!=(struct TRI*)NULL;
		     	trip=trip->next) {

		        if (nrays>=maxnrays) break;
		        if (ntris>=maxntris) {
		       	      fprintf(jpfp,
		       	"ntris >= maxntris: wavefront after this step is spoiled\n");
		              break;
		       	}

		       	if (ray[trip->i1].icode==SPOILED ||
		       	    ray[trip->i2].icode==SPOILED ||
		       	    ray[trip->i3].icode==SPOILED) 
			       err("Triangle wrong: Impossible! But that's life!"); 

                        /* ray one */
		        x0=wf[ithis+trip->i1].x[0];
		        y0=wf[ithis+trip->i1].x[1];
			z0=wf[ithis+trip->i1].x[2];
                        
			/* ray two */
			x1=wf[ithis+trip->i2].x[0];
			y1=wf[ithis+trip->i2].x[1];
			z1=wf[ithis+trip->i2].x[2];

			/* ray three */
			x2=wf[ithis+trip->i3].x[0];
			y2=wf[ithis+trip->i3].x[1];
		        z2=wf[ithis+trip->i3].x[2];

		        /************************************************
		        Write the current wavefront to the wffile. This 
			can be visualized by viewer3
			************************************************/
			
                        if (check_tris==PLEASE) {
			      dist[0]=(x0-x1)*(x0-x1)+(y0-y1)*(y0-y1)
                                     +(z0-z1)*(z0-z1);
		      	      dist[1]=(x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)
                                     +(z2-z1)*(z2-z1);
			      dist[2]=(x2-x0)*(x2-x0)+(y2-y0)*(y2-y0)
                                     +(z2-z0)*(z2-z0);

		       	      distmax=dist[0];
			      iedgemax=0;
			      for (iedge=1;iedge<3;iedge++) {
			       	    if (dist[iedge]>distmax) {
				          iedgemax=iedge;
				      	  distmax=dist[iedge];
				    }
			      }

			      /* this triangle is ok. all edges within limits,
			      nothing needs to be done with this triangle */
			      if (distmax<edgemax*edgemax) 
			       	    continue; 

			      /* this triangle is too big. needs to be splitted 
			      into two triangles */
			      if (iedgemax==0) {	/* vertex 1 & 2 */
				    xvertex0=x0;
				    xvertex1=x1;
				    yvertex0=y0;
				    yvertex1=y1;
				    zvertex0=z0;
				    zvertex1=z1;
				    tripi0=trip->i1;
				    tripi1=trip->i2;
			      } else if (iedgemax==1) {/* vertex 2 & 3 */
			     	    xvertex0=x1;
				    xvertex1=x2;
				    yvertex0=y1;
				    yvertex1=y2;
				    zvertex0=z1;
				    zvertex1=z2;
				    tripi0=trip->i2;
				    tripi1=trip->i3;
			      } else {		/* vertex 1 & 3 */
			       	    xvertex0=x0;
				    xvertex1=x2;
				    yvertex0=y0;
				    yvertex1=y2;
				    zvertex0=z0;
				    zvertex1=z2;
				    tripi0=trip->i1;
				    tripi1=trip->i3;
			      }

			      if (ray[tripi0].shootupward!=ray[tripi1].shootupward)
                                    continue; /* better not add one here */

			      /***********************************************
			      Add one more ray: nrays: Interpolation of ray 
                              data, whatever you need, could be traveltime; 
                              amplitude; phase, and even more.
	      		      ***********************************************/
			      wf[ithis+nrays].x[0]=(xvertex0+xvertex1)/2.0;
			      wf[ithis+nrays].x[1]=(yvertex0+yvertex1)/2.0;
			      wf[ithis+nrays].x[2]=(zvertex0+zvertex1)/2.0;

			      /* Needs to determine which tetra it is in */
                              fprintf(jpfp,"Add one more ray\n");
		      	      ray[nrays].itetra=in_which_tetra(
                                    xmin,
                                    ymin,
                                    xmax,
                                    ymax,
                                    zmax,
                                    wf[ithis+nrays].x, 
				    point,
                                    tetra,
				    ntetra);

                              if (ray[nrays].itetra<0) {
				    ray[nrays].icode=SPOILED;
                                    fprintf(jpfp,"attempt to add this ray but failed\n");
			      }

			      ray[nrays].shootupward=ray[tripi0].shootupward;

			      #ifdef DEBUG
			      fprintf(jpfp,"add a ray (nray=%d) in tetra=%d\n",
			            nrays,ray[nrays].itetra);
			      fprintf(jpfp,"at x=%f %f %f\n",wf[ithis+nrays].x[0],
			            wf[ithis+nrays].x[1],wf[ithis+nrays].x[2]);

			      fprintf(jpfp,"this tetra: ip=%d %d %d %d\n",
			            tetra[ray[nrays].itetra].ip[0],
			            tetra[ray[nrays].itetra].ip[1],
			            tetra[ray[nrays].itetra].ip[2],
			            tetra[ray[nrays].itetra].ip[3]);
			      fprintf(jpfp,"ray[nrays].shootupward=%d\n",
				    ray[nrays].shootupward);
                              #endif

                              ray[nrays].r=(ray[tripi0].r+ray[tripi1].r)/2.0;
                              ray[nrays].tseg[0]=itwf*dtwf;
                              ray[nrays].ttotal=itwf*dtwf;

                              #ifdef DEBUG
			      fprintf(jpfp,"itwf=%d ray[%d].xseg=%f %f %f\n",
			            itwf,nrays,ray[nrays].xseg[0][0],
			            ray[nrays].xseg[0][1],ray[nrays].xseg[0][2]);
			      #endif

			      wf[ithis+nrays].p[0]=(wf[ithis+tripi0].p[0]+
			       	    wf[ithis+tripi1].p[0])/2.0;
			      wf[ithis+nrays].p[1]=(wf[ithis+tripi0].p[1]+
			       	    wf[ithis+tripi1].p[1])/2.0;
			      wf[ithis+nrays].p[2]=(wf[ithis+tripi0].p[2]+
			       	    wf[ithis+tripi1].p[2])/2.0;

			      s=s_intp(
				    point,              /* control points */
				    &tetra[initetra],   /* this tetra */
				    wf[ithis+nrays].x); /* the current position */

                              s_normalize(wf[ithis+nrays].p,s);
			      
                              r=(ray[tripi0].r+ray[tripi1].r)/2.0;
                              d2=(xvertex0-wf[ithis+nrays].x[0])*
                                 (xvertex0-wf[ithis+nrays].x[0])+
				 (yvertex0-wf[ithis+nrays].x[1])*
                                 (yvertex0-wf[ithis+nrays].x[1])+
				 (zvertex0-wf[ithis+nrays].x[2])*
                                 (zvertex0-wf[ithis+nrays].x[2]);
                              d=sqrt(d2);
                              sintheta=d/MAX(0.2,r);
                              sintheta=MIN(1.0,sintheta);
                              costheta=sqrt(1.0-sintheta*sintheta);
                              adjust=r-r*costheta;

			      #ifdef DEBUG
			      fprintf(jpfp,"adjust=%f,r=%f\n",adjust,r);
			      fprintf(jpfp,"p1,p2,p3=%f %f %f\n",
			      wf[ithis+nrays].p[0],
			      wf[ithis+nrays].p[1],
			      wf[ithis+nrays].p[2]);
			      #endif

                              wf[ithis+nrays].x[0]+=adjust*wf[ithis+nrays].p[0]/sqrt(s);
                              wf[ithis+nrays].x[1]+=adjust*wf[ithis+nrays].p[1]/sqrt(s);
			      wf[ithis+nrays].x[2]+=adjust*wf[ithis+nrays].p[2]/sqrt(s);

                              ray[nrays].xseg[0][0]=wf[ithis+nrays].x[0];
			      ray[nrays].xseg[0][1]=wf[ithis+nrays].x[1];
			      ray[nrays].xseg[0][2]=wf[ithis+nrays].x[2];

			      /* Needs to determine which tetra it is in */
		      	      ray[nrays].itetra=in_which_tetra(
                                    xmin,
                                    ymin,
                                    xmax,
                                    ymax,
                                    zmax,
                                    wf[ithis+nrays].x, 
				    point,
                                    tetra,
				    ntetra);

                              if (ray[nrays].itetra<0) {
				    ray[nrays].icode=SPOILED;
                                    fprintf(jpfp,"tried to adjust this ray, failed\n");
			      }

                              /********************************************
			      add 2 triangles.  Since any one edge is shared
                              by 2 triangles, we need to add one triangle on
                              one side, and the other one on the other side.
                              i.e. add one triangle to trip, and one to
                              trip0
                              ********************************************/

			      i1=trip->i1;
			      i2=trip->i2;
			      i3=trip->i3;

                              ii1=ii2=ii3=0;
                              iedgemax0=999;
                              /* find the next neighboring triangle */
		              for (trip0=headp;trip0!=(struct TRI*)NULL;
		     	            trip0=trip0->next) {
                                    if (trip0==trip) continue;
                                    ii1=trip0->i1;
                                    ii2=trip0->i2;
                                    ii3=trip0->i3;

                                    if ((ii1==i1 || ii1==i2 || ii1==i3) &&
                                        (ii2==i1 || ii2==i2 || ii2==i3)) {
                                          iedgemax0=0;
                                          break;
                                    } else if 
                                        ((ii2==i1 || ii2==i2 || ii2==i3) &&
                                         (ii3==i1 || ii3==i2 || ii3==i3)) {
                                          iedgemax0=1;
                                          break;
				    } else if 
                                        ((ii1==i1 || ii1==i2 || ii1==i3) &&
                                         (ii3==i1 || ii3==i2 || ii3==i3)) {
                                          iedgemax0=2;
                                          break;
                                    }
                              }

			      #ifdef DEBUG
                              fprintf(jpfp,
                              "ii1=%d ii2=%d ii3=%d i1=%d i2=%d i3=%d iedgemax0=%d\n",
                              ii1,ii2,ii3,i1,i2,i3,iedgemax0);
			      #endif

			      temtrip=trip->next;
			      if (iedgemax==0) {	/* vertex 1 & 2 */
			       	    trip->i2=nrays;
			            trip->next=(struct TRI *)
			       		  malloc(sizeof(struct TRI));
			            trip=trip->next;
				    trip->i1=nrays;
			            trip->i2=i2;
			      	    trip->i3=i3;
			      } else if (iedgemax==1) {/* vertex 2 & 3 */
			       	    trip->i3=nrays;
			            trip->next=(struct TRI *)
			       		  malloc(sizeof(struct TRI));
			            trip=trip->next;
			      	    trip->i1=i1;
			            trip->i2=nrays;
			       	    trip->i3=i3;
			      } else {		/* vertex 1 & 3 */
			       	    trip->i3=nrays;
			            trip->next=(struct TRI *)
			       		  malloc(sizeof(struct TRI));
			       	    trip=trip->next;
			            trip->i1=nrays;
			      	    trip->i2=i2;
			            trip->i3=i3;
			      }
	
			      trip->next=temtrip;
			      ntris++;

                              if (trip0==(struct TRI*)NULL || iedgemax0==999 ||
                                  ii1==ii2 || ii1==ii3 || ii2==ii3) {
                                    nrays++;
				    continue;
			      }

			      temtrip=trip0->next;
			      if (iedgemax0==0) {	/* vertex 1 & 2 */
			       	    trip0->i2=nrays;
			            trip0->next=(struct TRI *)
			       		  malloc(sizeof(struct TRI));
			            trip0=trip0->next;
				    trip0->i1=nrays;
			            trip0->i2=ii2;
			      	    trip0->i3=ii3;
			      } else if (iedgemax0==1) {/* vertex 2 & 3 */
			       	    trip0->i3=nrays;
			            trip0->next=(struct TRI *)
			       		  malloc(sizeof(struct TRI));
			            trip0=trip0->next;
			      	    trip0->i1=ii1;
			            trip0->i2=nrays;
			       	    trip0->i3=ii3;
			      } else {		/* vertex 1 & 3 */
			       	    trip0->i3=nrays;
			            trip0->next=(struct TRI *)
			       		  malloc(sizeof(struct TRI));
			       	    trip0=trip0->next;
			            trip0->i1=nrays;
			      	    trip0->i2=ii2;
			            trip0->i3=ii3;
			      }
	
			      trip0->next=temtrip;
			      ntris++;
                              nrays++;
			}
		  } /* end if check_tris */

		  /************************************************
	          Write the current wavefront to the wffile. This 
	      	  can be visualized by viewer3
		  ************************************************/
		  if (wffp!=NULL && itwf>=ifwf2dump && itwf<ifwf2dump+nwf2dump) {

		        for (trip=headp,ntr=0;trip!=(struct TRI*)NULL;
			     trip=trip->next) { 

			      if (wf[ithis+trip->i1].x[0]>0.0 &&
			          wf[ithis+trip->i1].x[1]>0.0 &&
			          wf[ithis+trip->i1].x[2]>0.0 &&
			          wf[ithis+trip->i2].x[0]>0.0 &&
			          wf[ithis+trip->i2].x[1]>0.0 &&
			          wf[ithis+trip->i2].x[2]>0.0 &&
			          wf[ithis+trip->i3].x[0]>0.0 &&
			          wf[ithis+trip->i3].x[1]>0.0 &&
			          wf[ithis+trip->i3].x[2]) ntr++;
                              else {
                                    ntr=0;
                                    break;
                              }
			}

                        fprintf(wffp,"%d = ntris\n",ntr);

			if (ntr>0) {
		              for (trip=headp;trip!=(struct TRI*)NULL;
		     	            trip=trip->next) 

			            fprintf(wffp,"%f %f %f %f %f %f %f %f %f\n",
                                          wf[ithis+trip->i1].x[0],
				          wf[ithis+trip->i1].x[1],
				          wf[ithis+trip->i1].x[2],
				          wf[ithis+trip->i2].x[0],
				          wf[ithis+trip->i2].x[1],
				          wf[ithis+trip->i2].x[2],
				          wf[ithis+trip->i3].x[0],
				          wf[ithis+trip->i3].x[1],
				          wf[ithis+trip->i3].x[2]);
                        }
                  }

		  if (ntris<2 || nrays<4) break;
            } /* end if it!=0 */

            if (itwf>=ntwf-1) break;

	    #ifdef DEBUG
            fprintf(jpfp,"Check done\n");
            #endif

            done=ALREADY;

	    for (iray=0;iray<nrays;iray++) {

	       	  if (ray[iray].icode==SPOILED || 
                        ray[iray].icode==HIT_SURFACE) continue;

                  done=NOT_YET;

      		  ray[iray].x[0]=wf[ithis+iray].x[0];
		  ray[iray].x[1]=wf[ithis+iray].x[1];
		  ray[iray].x[2]=wf[ithis+iray].x[2];

		  ray[iray].t=0.0;
		  ray[iray].p[0]=wf[ithis+iray].p[0];
		  ray[iray].p[1]=wf[ithis+iray].p[1];
		  ray[iray].p[2]=wf[ithis+iray].p[2];

		  /******************************************
		  Check if the current ray point source is out
		  of bounds
		  ******************************************/	

		  #ifdef DEBUG
                  fprintf(jpfp,"\nicode(iray=%d)=%d\n",
                        iray,ray[iray].icode);
		  #endif

		  while (HIT_FACET_FIRST==
                        (result=trace1tetra(
			maxnsegs,
		        xmin,ymin,xmax,ymax,zmax,
			ray+iray,/* this ray */
                        npoint,  /* number of points */
                        point,   /* control points */
                        nfacet,  /* number of facets */
                        facet,   /* facet information */
                        ntetra,  /* number of tetra */
                        tetra,   /* tetra information */
                        irefseq, /* reflector sequence */
                        nrefseq, /* number of reflectors */
		       	jpfp,
		       	dtwf)))	/* step size for t */
        	  {};

                  if (result==OUT_OF_MODEL) 
                        ray[iray].icode=SPOILED;

		  if (itwf<ntwf-1) {	
		       	wf[inext+iray].x[0]=ray[iray].x[0];
		       	wf[inext+iray].x[1]=ray[iray].x[1];
		       	wf[inext+iray].x[2]=ray[iray].x[2];
		       	wf[inext+iray].p[0]=ray[iray].p[0];
		       	wf[inext+iray].p[1]=ray[iray].p[1];
		       	wf[inext+iray].p[2]=ray[iray].p[2];
		  } else {
		        fprintf(jpfp,"ntwf is not enough\n");
		        enough_ntwf=WF_FALSE;
		  }

                  /* plot the rays wherever they go */
                  if (result==HIT_SURFACE_FIRST) {
                        check_tris=PLEASE_DO_NOT;
                        ray[iray].icode=HIT_SURFACE;

                        if (fabs(ray[iray].x[2])>0.0001) 
                              fprintf(jpfp,"x[2]!=0.0, =%f\n",ray[iray].x[2]);
                  }
	    }

            #ifdef DEBUG
            fprintf(jpfp,"itwf=%d done\n",itwf);
            #endif
      }/* next it */

      /******************************************************************
      Now, let's output the ray information for viewer3 to check if all
      the rays are correct. 
      ******************************************************************/
      if (rayfp!=NULL) {
            fprintf(rayfp,
                  "%d =Maximum number of segments\n",maxnsegs);

            ngoodrays=0;
            for (iray=0;iray<nrays;iray++)
                  if (ray[iray].icode!=SPOILED) ngoodrays++;

            fprintf(rayfp,
                  "%d =Number of rays\n",ngoodrays);

	    for (iray=0;iray<nrays;iray++) {
                  if (ray[iray].icode==SPOILED) continue;
                  fprintf(rayfp,"%d=nseg %f=ttotal\n",
                        ray[iray].nseg+1,ray[iray].ttotal);

                  tsum=0.0;
                  for (iseg=0;iseg<=ray[iray].nseg;iseg++) {
		        tsum+=ray[iray].tseg[iseg];
                        fprintf(rayfp,"%f %f %f %f %f\n",
                              ray[iray].xseg[iseg][0],
			      ray[iray].xseg[iseg][1],
			      ray[iray].xseg[iseg][2],
			      tsum,
			      ray[iray].tseg[iseg]);
                  }
            }
      }

      if (enough_ntwf==WF_FALSE) return 1;

      /* out the surface traveltime, only for visualization */
      if (sttfp!=NULL) {
	    for (trip=headp,ntr=0;trip!=(struct TRI*)NULL;
		  trip=trip->next) { 

	          if (ray[trip->i1].icode==SPOILED  ||
	    	      ray[trip->i2].icode==SPOILED  ||
	       	      ray[trip->i3].icode==SPOILED  ||
                      fabs(ray[trip->i1].x[2])>0.01 ||
		      fabs(ray[trip->i2].x[2])>0.01 ||
		      fabs(ray[trip->i3].x[2])>0.01) continue;

		  ntr++;
            }

            fprintf(stderr,"ntr=%d\n",ntr);

            fprintf(sttfp,"%d = ntris\n",ntr);

            if (ntr>0) {
                  for (trip=headp;trip!=(struct TRI*)NULL;
		       trip=trip->next) {

	                if (ray[trip->i1].icode==SPOILED  ||
	    	            ray[trip->i2].icode==SPOILED  ||
	       	            ray[trip->i3].icode==SPOILED  ||
                            fabs(ray[trip->i1].x[2])>0.01 ||
		            fabs(ray[trip->i2].x[2])>0.01 ||
		            fabs(ray[trip->i3].x[2])>0.01) continue;

	                fprintf(sttfp,"%f %f %f %f %f %f %f %f %f %f %f %f\n",
                              ray[trip->i1].x[0],
	                      ray[trip->i1].x[1],
			      ray[trip->i1].x[2],
			      ray[trip->i1].ttotal,
			      ray[trip->i2].x[0],
			      ray[trip->i2].x[1],
			      ray[trip->i2].x[2],
			      ray[trip->i2].ttotal,
			      ray[trip->i3].x[0],
			      ray[trip->i3].x[1],
			      ray[trip->i3].x[2],
			      ray[trip->i3].ttotal);
                  }
            }
            fclose(sttfp);
            sttfp=NULL;
      }

      /**************************************************
      To do the evaluation on the surface, for each 
      triangle, receivers inside this triangle will be
      evaluated. If a receiver is evaluated more than
      once, only the one with shortest traveltime
      is kept.
      *************************************************/
      for (trip=headp;trip!=NULL;trip=trip->next) {

	    if (ray[trip->i1].icode==SPOILED ||
	    	ray[trip->i2].icode==SPOILED ||
	       	ray[trip->i3].icode==SPOILED ) continue;

            tri_multi_intp(
                  &ray[trip->i1],
                  &ray[trip->i2],
                  &ray[trip->i3],
                  fxgd,
                  fygd,
                  dxgd,
                  dygd,
                  nxgd,
                  nygd,
		  ttable,
		  ctable,
		  rtable);
      }

      /******************************************************
      Now let us output the traveltime table to stdout, this
      will be used for sukdmig3d. Remember sx and sy are in 
      dxgd and dygd already.
      ******************************************************/
      for (iygd=iytf;iygd<iytf+nyt;iygd++) {
	    for (ixgd=ixtf;ixgd<ixtf+nxt;ixgd++) 
       		  tro.data[ixgd-ixtf]=MAX(0.0,ttable[iygd][ixgd]*1000.0);
            
       	    /* source left/front range */
       	    tro.gx=ixtf;
       	    tro.gy=iytf;

       	    /* source positions */
       	    tro.sx=(int)(source[0]*1000.0);
       	    tro.sy=(int)(source[1]*1000.0);
            tro.unscale=source[2];  /* this is a float */

       	    fputtr(stdout,&tro);

            if (crfp==NULL) continue;

            if (fwrite(&ctable[iygd][ixtf],sizeof(float),nxt,crfp)!=nxt)
                  err("Can not write to crfile");

            if (fwrite(&rtable[iygd][ixtf],sizeof(float),nxt,crfp)!=nxt)
                  err("Can not write to crfile");

      }

      fflush(stdout);
      fprintf(jpfp,"traveltimes for this shot written.\n");
      free2float(ttable);
	
      return 0;
}

float zhorz(float xx,float yy,float n10,float n20,
float n30,float x00,float y00,float z00)
/*****************************************************************
zhorz - linearly interpolation
*****************************************************************
Function prototype:
float zhorz(float xx,float yy,float n10,float n20,
float n30,float x00,float y00,float z00);
*****************************************************************
Inputs:
float xx 	x coordinate of the point where z coordinate will be returned
float yy        y coordinate of the point where z coordinate will be returned
float n10       x component of the normal to the plane
float n20       y component of the normal to the plane
float n30       z component of the normal to the plane
float x00       x coordinate of the point in the plane
float y00       y coordinate of the point in the plane
float z00       z coordinate of the point in the plane

Return:
the z coordinate of the point

Notes:
The equation of the plane is: 
	
(xx-x00)*n1ll+(yy-y00)*n2ll+(zz-z00)*n3ll=0

thus 

zz=z00-((xx-x00)*n1ll+(yy-y00)*n2ll)/n3ll;
*****************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
****************************************************************/
{
      if (fabs(n30)<0.001) return DistanceInfinity;
      return (z00-((xx-x00)*n10+(yy-y00)*n20)/n30);
}

float f(float sigma,float s,float p,float a)
/**********************************************************
f - from sigma to x
***********************************************************
Inputs:
float sigma	ray running parameter
float s         sloth
float p         ray parameter
float a         gradient of sloth

Return:
component of the ray position
Notices:
According to the formula (Cheveny 1987), the ray path satisfies:

x_i(sigma) = x_i(0) + p_i*sigma + 0.25*s_i*sigma^2

This function f will return the new cartesian coordinate:
***********************************************************
Author: CWP: Zhaobo Meng, Sept 1997
**********************************************************/
{
      return(s+p*sigma+0.25*a*sigma*sigma);
}

float cubicsolver(float d,float a,float b,float c)
/*****************************************************************
cubicsolver - solver for cubic equation
*****************************************************************
Function prototype:
float cubicsolver(float d,float a,float b,float c);
*****************************************************************
Inputs:
float d,a,b,c  coefficients of the cubic equation
return the solution for sigma
*****************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
******************************************************************/
{
      float sigma,fderi,fsigma;
      int MaxIterations=20;
      int iter;
      
      sigma=quadsolver(a,b,c);

      if (d==0.0) return sigma;

      if (sigma>0.5*SigmaInfinity) {
            sigma=1.0;
            MaxIterations=30;
      }

      for (iter=0;iter<MaxIterations;iter++) {
  	    /* fderi=3dx^2+2ax+b */

	    fsigma=((d*sigma+a)*sigma+b)*sigma+c;

	    if (fabs(fsigma)<EPS) break;

	    fderi=(3.0*d*sigma+2.0*a)*sigma+b;

            if (fabs(fderi)<EPS) break;

	    sigma-=fsigma/fderi;
      }
      
      if (sigma<0.0) return 0.0;
      
      return sigma;
}

float quadsolver(float a,float b,float c)
/**************************************************************
quadsolver - solver for quadratic equation ax^2+bx+c=0
**************************************************************
Function prototype:
float quadsolver(float a,float b,float c);
**************************************************************
Inputs:
float a,b,c  coefficients of the quadratic equation
return the solution for sigma
**************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
*************************************************************/

{
      float qdelta;
      float sigma,sigma1,sigma2;

      if (c==0.0) {
            if (a==0.0) return SigmaInfinity;
            sigma=-b/a;
            if (sigma<0.0) return SigmaInfinity;
            return sigma;
      }

      if (a==0.0) {
            if (b==0.0) return SigmaInfinity; 
            sigma=-c/b;
            if (sigma<0.0) sigma=SigmaInfinity;
            return sigma;
      } else {
            qdelta=b*b-4*a*c;
            if (qdelta<-EPS) return SigmaInfinity; 
            qdelta=MAX(0.0,qdelta);
            qdelta=sqrt(qdelta);
            sigma1=(qdelta-b)/(2.0*a);
            sigma2=(-qdelta-b)/(2.0*a);
            if (sigma1<-EPS) sigma1=SigmaInfinity;
            if (sigma2<-EPS) sigma2=SigmaInfinity;
            sigma=MIN(sigma1,sigma2);
            return MAX(0.0,sigma);
      }
}

int in_which_tetra(float xmin,float ymin,float xmax,float ymax,
float zmax,float x[3],struct POINT *point,struct TETRA *tetra,int ntetra)
/*****************************************************************
in_which_tetra - x falls in which tetra?
******************************************************************
Function prototype:
int in_which_tetra(float xmin,float ymin,float xmax,float ymax,
float zmax,float x[3],struct POINT *point,struct TETRA *tetra,int ntetra);
******************************************************************
inputs:
float xmin              x min
float ymin              y min              
float xmax              x max
float ymax              y max
float zmax              z max
float x[3]              coordinate of the point
struct POINT *point     control points
struct TETRA *tetra     tetra array
int ntetra              number of tetra
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
******************************************************************/
{
      int it;
      int ip0,ip1,ip2,ip3;
      float vv,vv0,vv1,vv2,vv3;
      float xxmin[4],xxmax[4];

      float vdiff;
      int itnearest;

      if (x[0]<xmin || x[0]>xmax ||
          x[1]<ymin || x[1]>ymax ||
          x[2]<0.0  || x[2]>zmax) return -3;

      itnearest=-1;
      vdiff=InfDistance;

      for (it=0;it<ntetra;it++) {

            vv=tetra[it].v;
            if (vv<0.0) continue;

	    ip0=tetra[it].ip[0];
	    ip1=tetra[it].ip[1];
            ip2=tetra[it].ip[2];
            ip3=tetra[it].ip[3];

            xxmin[0]=MIN(
                 point[ip0].x[0],MIN(
                 point[ip1].x[0],MIN(
                 point[ip2].x[0],
                 point[ip3].x[0])));

            xxmax[0]=MAX(
                 point[ip0].x[0],MAX(
                 point[ip1].x[0],MAX(
                 point[ip2].x[0],
                 point[ip3].x[0])));

            xxmin[1]=MIN(
                 point[ip0].x[1],MIN(
                 point[ip1].x[1],MIN(
                 point[ip2].x[1],
                 point[ip3].x[1])));

            xxmax[1]=MAX(
                 point[ip0].x[1],MAX(
                 point[ip1].x[1],MAX(
                 point[ip2].x[1],
                 point[ip3].x[1])));

            xxmin[2]=MIN(
                 point[ip0].x[2],MIN(
                 point[ip1].x[2],MIN(
                 point[ip2].x[2],
                 point[ip3].x[2])));

            xxmax[2]=MAX(
                 point[ip0].x[2],MAX(
                 point[ip1].x[2],MAX(
                 point[ip2].x[2],
                 point[ip3].x[2])));

            if (x[0]<xxmin[0]-Margin || x[0]>xxmax[0]+Margin ||
                x[1]<xxmin[1]-Margin || x[1]>xxmax[1]+Margin ||
		x[2]<xxmin[2]-Margin || x[2]>xxmax[2]+Margin) continue;

	    vv0=tetra_volume(x,
		 point[ip1].x,
		 point[ip2].x,
		 point[ip3].x);

            vv1=tetra_volume(point[ip0].x,
		 x,
		 point[ip2].x,
		 point[ip3].x);
	    
            vv2=tetra_volume(point[ip0].x,
		 point[ip1].x,
		 x,
		 point[ip3].x);

            vv3=tetra_volume(point[ip0].x,
		 point[ip1].x,
		 point[ip2].x,
		 x);

            if (vv0+vv1+vv2+vv3-vv < vdiff) {
	          vdiff=vv0+vv1+vv2+vv3-vv;
		  itnearest=it;
            }
      }
      if (itnearest==-1) 
	     err("x=%f %f %f not in any tetra: vdiff=%f\n",
                   x[0],x[1],x[2],vdiff);
      return itnearest;      
}

void crossprod(float u1,float u2,float u3,float v1,float v2,
float v3,float *w1,float *w2,float *w3)
/***************************************************************
crossprod - calculate the cross product
***************************************************************
Function prototype:
void crossprod(float u1,float u2,float u3,float v1,float v2,
float v3,float *w1,float *w2,float *w3);
***************************************************************
Inputs:
float u1,u2,u3 vector 1
float v1,v2,v3 vector 2
Output:
float* w1,w2,w3 cross product of vector u and vector v
***************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
***************************************************************/
{
      *w1=u2*u3-v2*v3;
      *w2=u3*v1-u1*v3;
      *w3=u1*v2-v1*u2;

      if (*w1==0.0 && *w2==0.0 && *w3==0.0) 
           *w3=1.0;
}

void tri_multi_intp(
struct RAY *ray0,
struct RAY *ray1,
struct RAY *ray2,
float fxgd,
float fygd,
float dxgd,
float dygd,
int nxgd,
int nygd,
float **ttable,
float **ctable,
float **rtable)
/**************************************************************
tri_multi_intp - interpolate the traveltime to form the traveltime table
**************************************************************
Function prototype:
void tri_multi_intp(
struct RAY *ray0,
struct RAY *ray1,
struct RAY *ray2,
float fxgd,
float fygd,
float dxgd,
float dygd,
int nxgd,
int nygd,
float **ttable,
float **ctable,
float **rtable);
**************************************************************
Inputs:
struct RAY *ray0 ray 0
struct RAY *ray1 ray 1
struct RAY *ray2 ray 2
float fxgd,fygd,dxgd,dygd first sample and grid spacing 
int nxgd,nygd number of grid points for traveltime table
Outputs:
float **ttable  traveltime table (for sukdmig3d)
float **ctable  cosine table (for sukdmig3d)
float **rtable  total ray path table (for geometric spreading)
**************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
************************************************************/
{
      float n1,n2,n3;   /* the normal to this triangle */
      int ixgd,iygd;
      float ttemp,ctemp,rtemp,x,y;
      float tmin,tmax;
      float xmin,xmax;
      float ymin,ymax;
      float areat;
      float sintheta,costheta;
      float v0,v1,v2;
      float r0,r1,r2;
      float c0,c1,c2;
      float x0,y0,t0;
      float x1,y1,t1;
      float x2,y2,t2;
      float dist0,dist1,dist2;

      v0=ray0->v;
      v1=ray1->v;
      v2=ray2->v;

      r0=ray0->r;
      r1=ray1->r;
      r2=ray2->r;

      c0=v0*ray0->p[2];
      c1=v1*ray1->p[2];
      c2=v2*ray2->p[2];

      x0=ray0->x[0];
      y0=ray0->x[1];
      t0=ray0->ttotal;

      x1=ray1->x[0];
      y1=ray1->x[1];
      t1=ray1->ttotal;

      x2=ray2->x[0];
      y2=ray2->x[1];
      t2=ray2->ttotal;

      areat=3.0*area(x0,y0,x1,y1,x2,y2);

      if (areat<EPS) return;

      tmin=MIN(t0,MIN(t1,t2));
      tmax=MAX(t0,MAX(t1,t2));
      xmin=MIN(x0,MIN(x1,x2));
      xmax=MAX(x0,MAX(x1,x2));
      ymin=MIN(y0,MIN(y1,y2));
      ymax=MAX(y0,MAX(y1,y2));

      crossprod(
            x1-x0,
            y1-y0,
            t1-t0,
            x2-x0,
            y2-y0,
            t2-t0,
            &n1,&n2,&n3);

      for (ixgd=MAX(0,(xmin-fxgd)/dxgd-1);
            ixgd<=MIN(nxgd-1,(xmax-fxgd)/dxgd+1);ixgd++) {
            x=fxgd+ixgd*dxgd;
            for (iygd=MAX(0,(ymin-fygd)/dygd-1);
                   iygd<=MIN(nygd-1,(ymax-fygd)/dygd+1);iygd++) {
		   y=fygd+iygd*dygd;
		   
                   if (WF_NO==in_tri(x0,y0,x1,y1,x2,y2,x,y,areat)) continue;

                   dist0=sqrt((x-x0)*(x-x0)+(y-y0)*(y-y0));
                   dist1=sqrt((x-x1)*(x-x1)+(y-y1)*(y-y1));
                   dist2=sqrt((x-x2)*(x-x2)+(y-y2)*(y-y2));

                   if (dist0<=dist1 && dist0<=dist2) {
                       ttemp=zhorz(x,y,n1,n2,n3,x0,y0,t0);
		       if (ttemp<tmin-EPS || ttemp>tmax+EPS) continue;
                       sintheta=dist0/(MAX(0.1,r0));
                       sintheta=MIN(1.0,sintheta);
                       costheta=sqrt(1.0-sintheta*sintheta);
                       ttemp-=r0*(1.0-costheta)/v0;
		       ctemp=zhorz(x,y,n1,n2,n3,x0,y0,c0);
		       rtemp=zhorz(x,y,n1,n2,n3,x0,y0,r0);
                   } else if (dist1<=dist0 && dist1<=dist2) {
                       ttemp=zhorz(x,y,n1,n2,n3,x1,y1,t1);
		       if (ttemp<tmin-EPS || ttemp>tmax+EPS) continue;
                       sintheta=dist1/(MAX(0.1,r1));
                       sintheta=MIN(1.0,sintheta);
                       costheta=sqrt(1.0-sintheta*sintheta);
                       ttemp-=r1*(1.0-costheta)/v1;
		       ctemp=zhorz(x,y,n1,n2,n3,x1,y1,c1);
		       rtemp=zhorz(x,y,n1,n2,n3,x1,y1,r1);
                   } else {
		       ttemp=zhorz(x,y,n1,n2,n3,x2,y2,t2);
		       if (ttemp<tmin-EPS || ttemp>tmax+EPS) continue;
                       sintheta=dist2/(MAX(0.1,r2));
                       sintheta=MIN(1.0,sintheta);
                       costheta=sqrt(1.0-sintheta*sintheta);
                       ttemp-=r2*(1.0-costheta)/v2;
		       ctemp=zhorz(x,y,n1,n2,n3,x2,y2,c2);
		       rtemp=zhorz(x,y,n1,n2,n3,x2,y2,r2);
                   }
                   ttable[iygd][ixgd]=MIN(ttable[iygd][ixgd],ttemp);
		   ctable[iygd][ixgd]=MAX(0.0,MIN(1.0,ctemp));
		   rtable[iygd][ixgd]=MAX(0.1,MIN(InfDistance,rtemp));
            }
      }
}

enum IN_TRI in_tri(float x0,float y0,float x1,float y1,
float x2,float y2,float x,float y,float areat)
/*************************************************************
in_tri - if this point falls in this triangle
**************************************************************
Function prototype:
enum IN_TRI in_tri(float x0,float y0,float x1,float y1,
float x2,float y2,float x,float y,float areat);
**************************************************************
Inputs:
float x0,y0   vertex 1
float x1,y1   vertex 2
float x2,y2   vertex 3
float x,y     testing point
Output:
float areat   relative area
return WF_YES/WF_NO x,y in this triangle or not
**************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
**************************************************************/
{
      float area0,area1,area2;

      area0=area(x,y,x1,y1,x2,y2);
      if (area0>areat) return WF_NO;

      area1=area(x0,y0,x,y,x2,y2);
      if (area0+area1>areat) return WF_NO;

      area2=area(x0,y0,x1,y1,x,y);
      if (area0+area1+area2>areat+0.1) return WF_NO;

      return WF_YES;
}

float tri_intp(float x,float y,float x0,float y0,float t0,
float x1,float y1,float t1,float x2,float y2,float t2)
/*************************************************************
tri_intp - linearly interpolate using 3 points
**************************************************************
Function prototype:
float tri_intp(float x,float y,float x0,float y0,float t0,
float x1,float y1,float t1,float x2,float y2,float t2);
**************************************************************
Inputs:
float x     x of the point
float y     y of the point
float x0    x of 1st point
float y0    y of 1st point
float t0    t value of 1st point 
float x1    x of 2nd point
float y1    y of 2nd point
float t1    t of 2nd point
float x2    x of 3rd point
float y2    y of 3rd point
float t2    t of 3rd point

Return  t at x,y determined by linear interpolation 
************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
************************************************************/
{
      float n1,n2,n3;   /* the normal to this triangle */

      crossprod(
            x1-x0,
            y1-y0,
            t1-t0,
            x2-x0,
            y2-y0,
            t2-t0,
            &n1,&n2,&n3);

      return (t0-((x-x0)*n1+(y-y0)*n2)/n3);      
}

void read_point(struct POINT *point,int npoint)
/*****************************************************************
read_point - read in point information
******************************************************************
Function prototype:
void read_point(struct POINT *point,int npoint);
******************************************************************
Input:
int npoint
Output:
struct POINT *point
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
**********************************************************************/
{
      int ip,ipp;    /* index for npoints */
   
      for (ip=0;ip<npoint;ip++) {
            fscanf(stdin,"%d : %f %f %f : %f : %f %f %f\n",
                  &ipp,
	          &point[ip].x[0],&point[ip].x[1],&point[ip].x[2],
                  &point[ip].s,
	          &point[ip].n[0],&point[ip].n[1],&point[ip].n[2]);
      }
}

void read_facet(struct FACET *facet,int nfacet)
/*****************************************************************
read-facet - read in facet information
******************************************************************
Function prototype:
void read_facet(struct FACET *facet,int nfacet);
******************************************************************
Input:
int nfacet
Output:
struct FACET *facet
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
*****************************************************************/
{
      int ifs,ifss;    /* index for nfacets */

      for (ifs=0;ifs<nfacet;ifs++) {
            fscanf(stdin,
"%d : %f : %f %f %f %f %f %f %f %f %f %f : %d %d %d : %f %f %f : %d %d\n",
                  &ifss,&facet[ifs].area,
		  &facet[ifs].ct[0],
		  &facet[ifs].ct[1],
		  &facet[ifs].ct[2],
		  &facet[ifs].ct[3],
		  &facet[ifs].ct[4],
		  &facet[ifs].ct[5],
                  &facet[ifs].ct[6],
		  &facet[ifs].ct[7],
		  &facet[ifs].ct[8],
                  &facet[ifs].ct[9],
	          &facet[ifs].ip[0],&facet[ifs].ip[1],&facet[ifs].ip[2],
	          &facet[ifs].cn[0],&facet[ifs].cn[1],&facet[ifs].cn[2],
	          &facet[ifs].itetra[0],&facet[ifs].itetra[1]);
      }
}

void read_tetra(struct TETRA *tetra,int ntetra)
/********************************************************************
read_tetra - read in the tetra information
*********************************************************************
Input:
int ntetra  number of tetra
Output:
struct TETRA *tetra tetra pointer
*********************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
*********************************************************************/
{
      int it,itt;    /* index for ntetra */

      for (it=0;it<ntetra;it++) {
            fscanf(stdin,"%d : %f : %d %d %d %d : %d %d %d %d : %d : %f %f %f\n",
                  &itt,&tetra[it].v,
		  &tetra[it].ip[0],&tetra[it].ip[1],&tetra[it].ip[2],&tetra[it].ip[3],
		  &tetra[it].ifacet[0],&tetra[it].ifacet[1],
	          &tetra[it].ifacet[2],&tetra[it].ifacet[3],
                  &tetra[it].ireg,
		  &tetra[it].gs[0],&tetra[it].gs[1],&tetra[it].gs[2]);
      }
}

float s_intp(struct POINT *point,struct TETRA *tetra,float x[3])
/*****************************************************************
s_intp - calculate s given the position
******************************************************************
Function prototype:
float s_intp(struct POINT *point,struct TETRA *tetra,float x[3]);
******************************************************************
Inputs:
struct POINT *point control points
struct POINT *point this tetra
float x[3] the current position
return
sloth linearly interpolated at x
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
*****************************************************************/
{
      float ss;
    
      ss=point[tetra->ip[0]].s+
         tetra->gs[0]*(x[0]-point[tetra->ip[0]].x[0])+
	 tetra->gs[1]*(x[1]-point[tetra->ip[0]].x[1])+
	 tetra->gs[2]*(x[2]-point[tetra->ip[0]].x[2]);

      return ss;
}

enum RESULT trace1tetra(
      int maxnsegs,
      float xmin,
      float ymin,
      float xmax,
      float ymax,
      float zmax,      
      struct RAY *ray,     /* ray information */
      int npoint,          /* number of points */
      struct POINT *point, /* control points */
      int nfacet,          /* number of facets */
      struct FACET *facet, /* facet information */
      int ntetra,          /* number of tetra */
      struct TETRA *tetra, /* tetra information */
      int *irefseq,        /* reflector sequences */
      int nrefseq,         /* number of reflectors */
      FILE *jpfp,
      float dtwf)          /* step size for t */
/***************************************************************
trace1tetra - ray tracing one tetra
*****************************************************************
Inputs:
int maxnsegs
float xmin
float ymin
float xmax
float ymax
float zmax      max z 
struct RAY *ray ray information
int npoint      number of points
struct *point   control points
int nfacet      number of facets
struct *facet   facet information
int ntetra      number of tetra
struct *tetra   tetra information
int *irefseq    reflector sequences
int nrefseq     number of reflectors
FILE *jpfp	pointer to job print
float dt        step size for traveltime and wavefront

Inputs/utputs:
struct RAY *ray ray information
*****************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
****************************************************************/
{
      float sgdt;	/* sigma value when intersecting with it*dtwf face */
      float sgf4min;    /* minimum sigma in the 4 solutions */
      int ifhit;        /* the min facet */
      float sgf4[4];

      float xst[3];	/* source position */

      float sx,sy,sz;   /* sloth gradient */

      float smst; 	/* smallest sigma */
      float smst0;      /* almost the smallest sigma */
      float sursg;      /* sigma needed to hit the surface */
      float sincid;
      float x0=DistanceInfinity;
      float y0=DistanceInfinity;
      float z0=DistanceInfinity;
      int ip;

      float cn0,cn1,cn2;
      float dist,distmax=0.0;
      float x00,y00,z00;
      int ip0,never,never0; 

      int itetranew;
      int ifacet,if0;
      float tadjust;
      float s,dlt;
      float xcenter,ycenter,zcenter;
      float alpha,alpha0;

      if (ray->itetra>ntetra-1 || ray->itetra<0) 
            err("ray->itetra out of range");

      s=s_intp(
	    point,                /* control points */
            &tetra[ray->itetra],  /* this tetra */
            ray->x);              /* the current position */

      s_normalize(ray->p,s);

      if (tetra[ray->itetra].v<0.0) {
            cross_slim_tetra(
                  ray,        /* ray information */
                  point,      /* control points */
                  facet,      /* facet information */
                  tetra,      /* tetra information */
                  irefseq,    /* reflector sequences */
                  nrefseq,    /* number of reflectors */
                  s,          /* incidence s */
                  jpfp);

            /* s needs to be reevaluated, because ray is changed */
            s=s_intp(
	          point,                /* control points */
                  &tetra[ray->itetra],  /* this tetra */
                  ray->x);              /* the current position */

            s_normalize(ray->p,s);
      }

      xst[0]=ray->x[0];
      xst[1]=ray->x[1];
      xst[2]=ray->x[2];
      ray->v=1.0/sqrt(s);

      #ifdef DEBUG
      fprintf(jpfp,"starting point=%f %f %f in itetra=%d, v=%f\n",
      xst[0],xst[1],xst[2],
      ray->itetra,tetra[ray->itetra].v);
      fprintf(jpfp,"starting point: p=%f %f %f\n",
      ray->p[0],ray->p[1],ray->p[2]);
      #endif

      /*************************************************************
      We need to calculate sgdt. The traveltime needed to reach the 
      next wavefront (from ray->t to dtwf).
      *************************************************************/

      /* sx,sy,sz is the sloth gradient */
      sx=tetra[ray->itetra].gs[0];
      sy=tetra[ray->itetra].gs[1];
      sz=tetra[ray->itetra].gs[2];

      #ifdef DEBUG
      fprintf(jpfp,"gs=%f %f %f : s=%f\n",sx,sy,sz,s);
      #endif

      sgdt=cubicsolver((sx*sx+sy*sy+sz*sz)/12.0,
             0.5*(sx*ray->p[0]+sy*ray->p[1]+sz*ray->p[2]),s,ray->t-dtwf);

      #ifdef DEBUG
      fprintf(jpfp,"to reach next dtwf: sgdt=%f; ray->t=%f; dtwf=%f\n",
      sgdt,ray->t,dtwf);
      #endif

      /*****************************************************
      The plane is: 

       	cn0*x+cn1*y+cn2*z=0,

      Suppose (x0,y0,z0) is a point on this plane.

      Then the linear transform from the old cartesian (x,y,z)
      to the now (x',y',z'), where cn0,cn1 are in plane, and
      cn2 is the normal to the plane, (x0,y0,z0) is the
      origin of the new cordinates, the transform can be written as:

		 x'       x - x0
		(y') = T (y - y0),	   (*)
		 z'       z - z0

      on the other hand, the ray can be written as 

		x=xst + px*sigma + 0.25*sx*sigma^2,
		y=yst + py*sigma + 0.25*sy*sigma^2,
		z=zst + pz*sigma + 0.25*sz*sigma^2,

      Take the third equation of (*) specifically,

      z' = cn0*(x-x0) + cn1*(y-y0) + cn2*(z-z0)
			
      When the ray incides, z'=0: =>

		0.25*(cn0*sx+cn1*sy+cn2*sz)*sg^2 
		+
		(cn0*px+cn1*py+cn2*pz)*sg
		+
		cn0*(xst-x0)+cn1*(ys-y0)+cn2*(zs-z0)=0	
		
      Solve for sghorz => the intersection point.	
      *****************************************************/

      for (if0=0;if0<4;if0++) {
	    /* for all facets of this tetra */
            ifacet=tetra[ray->itetra].ifacet[if0];

            if (ifacet>nfacet-1) 
                  err("ifacet out of range");

            if (ifacet==ray->lastfacet) {
                  sgf4[if0]=SigmaInfinity;
                  continue;
            }

            /* the normal to this facet */
	    cn0=facet[ifacet].cn[0];
	    cn1=facet[ifacet].cn[1];
	    cn2=facet[ifacet].cn[2];

	    #ifdef DEBUG
	    fprintf(jpfp,"ifacet=%d: cn=%f %f %f: ip=%d %d %d\n",
	    ifacet,cn0,cn1,cn2,facet[ifacet].ip[0],
	    facet[ifacet].ip[1],facet[ifacet].ip[2]);
	    #endif

            /* find the farthest vertex of this facet to the current point xst */
            distmax=0.0;
            for (ip0=0;ip0<3;ip0++) {
                  ip=facet[ifacet].ip[ip0];
                  if (ip>npoint-1) 
                        err("ip out of range");
                  x00=point[ip].x[0];
                  y00=point[ip].x[1];
                  z00=point[ip].x[2];
                  dist=(x00-xst[0])*(x00-xst[0])+
		       (y00-xst[1])*(y00-xst[1])+
		       (z00-xst[2])*(z00-xst[2]);
	          if (dist>distmax) {
                        x0=x00;
                        y0=y00;
                        z0=z00;
                        distmax=dist;
                  }
            } 

	    #ifdef DEBUG
	    fprintf(jpfp,"farthest vertex=%f %f %f distmax=%f\n",
		  x0,y0,z0,distmax);
            #endif

	    sgf4[if0]=quadsolver(
		  0.25*(cn0*sx+cn1*sy+cn2*sz),
		  cn0*ray->p[0]+cn1*ray->p[1]+cn2*ray->p[2],
	          cn0*(xst[0]-x0)+cn1*(xst[1]-y0)+cn2*(xst[2]-z0));

	    #ifdef DEBUG
	    fprintf(jpfp,"sgf4[%d]=%f, lastfacet=%d\n",
	    if0,sgf4[if0],ray->lastfacet);
            #endif

            if (tetra[facet[ifacet].itetra[0]].ireg==
                tetra[facet[ifacet].itetra[1]].ireg) continue;

            if (facet[ifacet].ct[0]==0.0 &&
                facet[ifacet].ct[1]==0.0 &&
                facet[ifacet].ct[2]==0.0 &&
                facet[ifacet].ct[3]==0.0 &&
                facet[ifacet].ct[4]==0.0 &&
                facet[ifacet].ct[5]==0.0) continue;

            sgf4[if0]=tetrasolver(
                  facet[ifacet].ct,
                  ray->p,
                  tetra[ray->itetra].gs,
                  xst,sgf4[if0]); 
      }

      sgf4min=SigmaInfinity;
      never0=1;
      never=-1;
      #ifdef DEBUG
      fprintf(jpfp,"shootupward=%d\n",ray->shootupward);
      #endif

      if (ray->shootupward==1) {
            ifhit=0;
            for (if0=0;if0<4;if0++) {

  	          if (sgf4[if0]==0.0) {
                        never0=0;
                        never=if0;
	          }

	          if (sgf4[if0]<sgf4min && sgf4[if0]>0.00001) {
                        sgf4min=sgf4[if0];
                        ifhit=tetra[ray->itetra].ifacet[if0];
                  }
	    }
      } else {
            ifhit=1;

	    /* do not go over lower boundary */
            if (xst[2]<zmax) 
                  sgf4[3]=MIN(sgf4[3],quadsolver(0.25*sz,
		        ray->p[2],xst[2]-zmax)); 

            for (if0=1;if0<4;if0++) {

  	          if (sgf4[if0]==0.0) {
                        never0=0;
                        never=if0;
	          }

	          if (sgf4[if0]<sgf4min && sgf4[if0]>0.00001) {
                        sgf4min=sgf4[if0];
                        ifhit=tetra[ray->itetra].ifacet[if0];
                  }
	    }
      }

      if (never0==0 && sgf4min>SigmaInfinity/2.0 && never>=0) {
            sgf4min=0.0;
            ifhit=never;
      }

      smst0=MIN(sgdt,sgf4min);

      /* because some tetra intersect the boundary, so it is possible
      for a ray to hit the surface without hitting a facet on
      the surface. If we suppose the surface is z=0 plane, then
      the following trick solves this problem */

      if (ray->shootupward==1) {
            sursg=quadsolver(0.25*sz,ray->p[2],xst[2]); /* sg needed to hit surface */
            smst=MIN(smst0,sursg);
      } else {
            smst=smst0;
            sursg=SigmaInfinity;
      }

      if (sgf4min>SigmaInfinity*0.5 && ray->ionce>=MaxOnce) {
            fprintf(jpfp,"Could not find any facet\n");
      }

      #ifdef DEBUG
      fprintf(jpfp,"smst0=%f smst=%f ",smst0,smst);
      fprintf(jpfp,"distmax=%f\n",distmax);
      #endif 

      if (ray->x[0]<xmin || ray->x[0]>xmax ||
          ray->x[1]<ymin || ray->x[1]>ymax) {
	     fprintf(jpfp,"this ray out of model\n");
             ray->icode=SPOILED;
             return OUT_OF_MODEL;
      }

      if ((smst>SigmaInfinity*0.5 || distmax<EPS) && ray->ionce<MaxOnce) {

            #ifdef DEBUG
	    fprintf(jpfp,"Going to be perturbed a little\n");
	    fprintf(jpfp,"old itetra=%d ip=%d %d %d %d v=%f\n",
	    ray->itetra,tetra[ray->itetra].ip[0],
	    tetra[ray->itetra].ip[1],tetra[ray->itetra].ip[2],
	    tetra[ray->itetra].ip[3],tetra[ray->itetra].v);
            #endif

            ray->itetra=in_which_tetra(
                  xmin,
                  ymin,
                  xmax,
                  ymax,
                  zmax,
	          ray->x,    
	          point,    
                  tetra,   
                  ntetra);

            xcenter=(point[tetra[ray->itetra].ip[0]].x[0]+
		     point[tetra[ray->itetra].ip[1]].x[0]+
		     point[tetra[ray->itetra].ip[2]].x[0]+
		     point[tetra[ray->itetra].ip[3]].x[0])/4.0;

            ycenter=(point[tetra[ray->itetra].ip[0]].x[1]+
		     point[tetra[ray->itetra].ip[1]].x[1]+
		     point[tetra[ray->itetra].ip[2]].x[1]+
		     point[tetra[ray->itetra].ip[3]].x[1])/4.0;

            zcenter=(point[tetra[ray->itetra].ip[0]].x[2]+
		     point[tetra[ray->itetra].ip[1]].x[2]+
		     point[tetra[ray->itetra].ip[2]].x[2]+
		     point[tetra[ray->itetra].ip[3]].x[2])/4.0;

            #ifdef DEBUG
	    fprintf(jpfp,"x,y,z center=%f %f %f\n",
	    xcenter,ycenter,zcenter);
	    fprintf(jpfp,"old xst: %f %f %f\n",
	    xst[0],xst[1],xst[2]);
	    #endif

            ray->ionce++;
            alpha=1.0-ray->ionce*0.5/(float)MaxOnce;
            alpha0=1.0-alpha;

	    ray->x[0]=alpha*xst[0]+alpha0*xcenter;
            ray->x[1]=alpha*xst[1]+alpha0*ycenter;
	    ray->x[2]=alpha*xst[2]+alpha0*zcenter;

            if (ray->x[0]<xmin || ray->x[0]>xmax ||
                  ray->x[1]<ymin || ray->x[1]>ymax) {
	          fprintf(jpfp,"this ray out of model\n");
                  ray->icode=SPOILED;
                  return OUT_OF_MODEL;
            }

	    #ifdef DEBUG
	    fprintf(jpfp,"new itetra=%d ip=%d %d %d %d v=%f\n",ray->itetra,
	    tetra[ray->itetra].ip[0],
	    tetra[ray->itetra].ip[1],
	    tetra[ray->itetra].ip[2],
	    tetra[ray->itetra].ip[3],
            tetra[ray->itetra].v); 
	    fprintf(jpfp,"new xst: %f %f %f\n",
	    ray->x[0],ray->x[1],ray->x[2]);
	    fprintf(jpfp,"3 SigmaInfinities\n");
            #endif

            ray->lastfacet=-1;
            return HIT_FACET_FIRST;
      }

      if (ray->ionce>0) {
            ray->nseg++;
            if (ray->nseg>maxnsegs) err("maxnsegs too small");
            ray->xseg[ray->nseg][0]=xst[0];
            ray->xseg[ray->nseg][1]=xst[1];
            ray->xseg[ray->nseg][2]=xst[2];

            if (ray->shootupward==1)
                  tadjust=-(xst[2]-ray->xseg[ray->nseg-1][2])/ray->v;
            else 
	          tadjust=(xst[2]-ray->xseg[ray->nseg-1][2])/ray->v;
            
            ray->tseg[ray->nseg]+=tadjust;
            ray->ttotal+=tadjust;
      }
      ray->ionce=0;

      #ifdef DEBUG
      fprintf(jpfp,"x goes from: %f %f %f\np[0],p[1],p[2]=%f %f %f in itetra=%d,v=%f\n",
      xst[0],xst[1],xst[2],ray->p[0],ray->p[1],ray->p[2],ray->itetra,
      tetra[ray->itetra].v);
      #endif

      ray->x[0]=xst[0]+smst*(ray->p[0] + 0.25*sx*smst);
      ray->x[1]=xst[1]+smst*(ray->p[1] + 0.25*sy*smst);
      ray->x[2]=xst[2]+smst*(ray->p[2] + 0.25*sz*smst);
      dlt=smst*(s+smst*(0.5*(ray->p[0]*sx+ray->p[1]*sy+ray->p[2]*sz)
	+(sx*sx+sy*sy+sz*sz)*smst/12.0));
      ray->t+=dlt;
      ray->r+=dlt/sqrt(s);

      if (ray->x[0]<xmin || ray->x[0]>xmax ||
          ray->x[1]<ymin || ray->x[1]>ymax) {
	     fprintf(jpfp,"this ray out of model\n");
             ray->icode=SPOILED;
             return OUT_OF_MODEL;
      }

      #ifdef DEBUG
      fprintf(jpfp,"min sg: x[0],x[1],x[2]=%f %f %f\n",
	    ray->x[0],ray->x[1],ray->x[2]);
      #endif

      /*****************************************************
      Once the shortest sigma is found, we need to adjust
      the slowness vector (px,py,pz) using

      p_(sg) = p_i0 + 0.5*s_i*sg

      *****************************************************/
      ray->p[0]+=0.5*sx*smst;
      ray->p[1]+=0.5*sy*smst;
      ray->p[2]+=0.5*sz*smst;

      sincid=s_intp(
	    point,                /* control points */
            &tetra[ray->itetra],  /* this tetra */
            ray->x);              /* the current position */

      s_normalize(ray->p,sincid);

      ray->nseg++;
      if (ray->nseg>maxnsegs) err("maxnsegs too small");
      ray->xseg[ray->nseg][0]=ray->x[0];
      ray->xseg[ray->nseg][1]=ray->x[1];
      ray->xseg[ray->nseg][2]=ray->x[2];
      ray->tseg[ray->nseg]+=dlt;
      ray->ttotal+=ray->tseg[ray->nseg];

      if (sgdt<sgf4min && sursg>=smst0) {
            #ifdef DEBUG
	    fprintf(jpfp,"Hit WF first\n");
            #endif
            ray->lastfacet=-1;
      	    return HIT_WF_FIRST; /* successfully touch WF */
      }

      /* Now, ray hits surface first */
      if (facet[ifhit].itetra[0]==-1 || facet[ifhit].itetra[1]==-1 
         || sursg<smst0) {

          #ifdef DEBUG
	  fprintf(jpfp,"Hit the surface at (%f %f %f) t=%f\n",
                ray->xseg[ray->nseg][0],
	        ray->xseg[ray->nseg][1],
                ray->xseg[ray->nseg][2],
	        ray->ttotal);
          fprintf(jpfp,"from (%f %f %f)\n",
                ray->xseg[0][0],
	        ray->xseg[0][1],
                ray->xseg[0][2]);

	  fprintf(jpfp,"ifhit=%d\n",ifhit);
	  fprintf(jpfp,"itetra[0] and [1]=%d %d\n",
	        facet[ifhit].itetra[0],facet[ifhit].itetra[1]);
	  fprintf(jpfp,"ip0,ip2=%d %d %d\n",
	        facet[ifhit].ip[0],facet[ifhit].ip[1],facet[ifhit].ip[2]);
          #endif

            ray->lastfacet=-1;
            return HIT_SURFACE_FIRST;
      }

      #ifdef DEBUG
      fprintf(jpfp,"try to cross facet from: itetra=%d: v=%f :",
      ray->itetra,tetra[ray->itetra].v);
      fprintf(jpfp,"ireg=%d\n",tetra[ray->itetra].ireg);
      #endif

      itetranew=facet[ifhit].itetra[0];
      if (ray->itetra==itetranew)
            itetranew=facet[ifhit].itetra[1];

      #ifdef DEBUG
      fprintf(jpfp,"try to come into itetra=%d: v=%f :",
      itetranew,tetra[itetranew].v);
      fprintf(jpfp,"ireg=%d\n",tetra[itetranew].ireg);
      #endif

      if (ray->itetra==itetranew)
            err("ray->itetra==itetranew");

      if (itetranew!=-2) {
            if (tetra[itetranew].ireg==tetra[ray->itetra].ireg) {
	          ray->itetra=itetranew;
                  ray->lastfacet=ifhit;
                  return HIT_FACET_FIRST;
            }
      }

      /* to apply Snell's law */
      return RTlaw(
            ray,        /* ray information */
            point,      /* control points */
            facet,      /* facet information */
            tetra,      /* tetra information */
            irefseq,    /* reflector sequences */
            nrefseq,    /* number of reflectors */
            ifhit,      /* last facet it hits */
            sincid,     /* sloth in last tetra */
            itetranew,  /* index for the new tetra */
            jpfp);
}

float area(float x0,float y0,float x1,float y1,float x2,float y2)
/******************************************************************
area - calculate the area given 3 2-D points
******************************************************************
Function prototype:
float area(float x0,float y0,float x1,float y1,float x2,float y2);
******************************************************************
float x0,y0 point 1
float x1,y1 point 2
float x2,y2 point 3
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
******************************************************************/
{
      float d1,d2,d3;
      d1=sqrt((x0-x1)*(x0-x1)+(y0-y1)*(y0-y1));
      d2=sqrt((x0-x2)*(x0-x2)+(y0-y2)*(y0-y2));
      d3=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));

      return sqrt((d1+d2+d3)*(d2+d3)*(d1+d3)*(d1+d2))/12.0;
}

void cross_slim_tetra(
struct RAY *ray,     /* ray information */
struct POINT *point, /* control points */
struct FACET *facet, /* facet information */
struct TETRA *tetra, /* tetra information */
int *irefseq,        /* reflector sequences */
int nrefseq,         /* number of reflectors */
float sincid,        /* incidence sloth */
FILE *jpfp)
/*****************************************************************
cross_slim_tetra - crossing a tetra with 0 volume
******************************************************************
Function prototype:
void cross_slim_tetra(
struct RAY *ray,
struct POINT *point,
struct FACET *facet,
struct TETRA *tetra,
int *irefseq,
int nrefseq, 
float sincid, 
FILE *jpfp);
******************************************************************
Inputs:
struct *point   control points
struct *facet   facet information
struct *tetra   tetra information
int *irefseq    reflector sequences
int nrefseq     number of reflectors
float sincid    incidence sloth
FILE *jpfp	pointer to job print

Inputs/utputs:
struct RAY *ray ray information pointer
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
******************************************************************/

{
      int ifhit;     /* last facet it hits */
      int ifs,newit,newifs;
      float fit_err;
      float ff;
      int ireg0;

      ireg0=tetra[ray->itetra].ireg;
      for (;;) {

            #ifdef DEBUG
            fprintf(jpfp,"itetra=%d\n",ray->itetra);
            fprintf(jpfp,"v=%f\n",tetra[ray->itetra].v);
            #endif

            fit_err=DistanceInfinity;
            newifs=-1;
            for (ifs=0;ifs<4;ifs++) {
                  ifhit=tetra[ray->itetra].ifacet[ifs];

                  #ifdef DEBUG
                  fprintf(jpfp,"ifs=%d ifhit=%d lastfacet=%d\n",
                  ifs,ifhit,ray->lastfacet);
                  #endif

                  if (ifhit==ray->lastfacet) continue;
                  ff=facet_fit(point,ray->x,&facet[ifhit]);

                  #ifdef DEBUG
                  fprintf(jpfp,"ff=%f\n",ff);
                  #endif

                  if (ff<fit_err) {
                        newifs=ifhit;
                        fit_err=ff;
                  }
            }
          
            if (newifs==-1) err("newifs not found");
            ray->lastfacet=newifs;

            newit=facet[newifs].itetra[0];
            if (newit==ray->itetra)
                  newit=facet[newifs].itetra[1];
            if (newit==ray->itetra)
                  err("newit wrong");

            /* need to apply Snell's law here */
            if (tetra[newit].ireg!=ireg0)
                  RTlaw(
                        ray,        /* ray information */
                        point,      /* control points */
                        facet,      /* facet information */
                        tetra,      /* tetra information */
                        irefseq,    /* reflector sequences */
                        nrefseq,    /* number of reflectors */
                        newifs,     /* last facet it hits */
                        sincid,     /* sloth in last tetra */
                        newit,  /* index for the new tetra */
                        jpfp);

            ray->itetra=newit;

            #ifdef DEBUG
            fprintf(jpfp,"newit=%d\n",newit);
            fprintf(jpfp,"new v=%f\n",tetra[ray->itetra].v);
            #endif

            if (ray->itetra==-1) break;          
            if (tetra[ray->itetra].v>0.0) break;
      }
}

float facet_fit(struct POINT *point,float x[3],struct FACET *facet)
/******************************************************************
facet_fit - to see if a point falls in the triangle
******************************************************************
Function prototype:
float facet_fit(struct POINT *point,float x[3],struct FACET *facet);
******************************************************************
struct POINT *point the control points
float x[3] the point x
struct FACET *facet this facet
Return:
Does this point falls in this facet?
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
******************************************************************/
{
      float areat,area0;

      area0=facet->area;

      areat=area3d(
            x,
            point[facet->ip[1]].x,
            point[facet->ip[2]].x);

      areat+=area3d(
	    point[facet->ip[0]].x,
            x,
            point[facet->ip[2]].x);

      areat+=area3d(
	    point[facet->ip[0]].x,
	    point[facet->ip[1]].x,
            x);

      if (area0<EPS) return DistanceInfinity;

      return areat/area0;
}

enum RESULT RTlaw(
      struct RAY *ray,     /* ray information */
      struct POINT *point, /* control points */
      struct FACET *facet, /* facet information */
      struct TETRA *tetra, /* tetra information */
      int *irefseq,        /* reflector sequences */
      int nrefseq,         /* number of reflectors */
      int ifhit,           /* last facet it hits */
      float sincid,        /* sloth in last tetra */
      int itetranew,       /* index for the new tetra */
      FILE *jpfp)
/*****************************************************************
RTlaw - apply Snell's law to penetrate a horizon
******************************************************************
Function prototype:
enum RESULT RTlaw(struct RAY *ray,struct POINT *point,struct FACET *facet,
struct TETRA *tetra,int *irefseq,int nrefseq,int ifhit,float sincid,
int itetranew,FILE *jpfp);
******************************************************************
Inputs:
struct *point   control points
struct *facet   facet information
struct *tetra   tetra information
int *irefseq    reflector sequences
int nrefseq     number of reflectors
int ifhit       last facet it hits
float sincid    sloth in last tetra
int itetranew   index for the new tetra
FILE *jpfp	pointer to job print

Inputs/utputs:
struct RAY *ray ray information pointer
******************************************************************
Author Zhaobo Meng, CWP, 1997
******************************************************************/
{
      float b;
      float strans;
      float crossp;

      float cn0,cn1,cn2;

      int iregnew;

      float cosa,cosb,sinb2; 
      float cosa2,sina2;

      /******************************************************************
      Now adjust the direction to penetrate the horizon

      By the R/T law:

		  P1  N 	
		___________________
	       /    \ |beta       /
	      /      \|	  	 /
	     /	      X		/
	    /	      |\       /
	   /	      | \alpha/
	   --------------------
		         P0

      As shown above, let P0 be the normalized initial slowness vector,
      while P0' be the normalized transmitted slowness vector,

      P0*N = cos(alpha)*sqrt(sincid), 

      and

      P1*N = cos(beta)*sqrt(strans).

      By Snell's law, 
	
      sin(alpha)/sin(beta) = vup/vdn;

      since P0' still lies in the same plane with P0 and N, suppose 

      P1 = a*P0 + b*N,

      b = cos(beta)*sqrt(strans) - a*cos(alpha)*sqrt(sincid)

      Since b*N = P1 - a*P0, thus

      b = P1*N - a*P0*N

      using sloth, we have P0*P0 = sincid, P1*P1 = strans, thus we have

      P0*P0*a^2 + 2P0*N*a*b + b^2 N*N = strans

      => sincid*a^2 + 2cos(alpha)*sqrt(sincid)*a*b + b^2 = strans

      => a^2 = 1

      since a is positive for transmitted waves,

      a=sqrt(a^2)

      So we get P1.

      For reflection, just do:

      p0-p1=a N

      p1=p0 - a N

      where N points to upward here.

      sincid = sincid + a^2 - 2 a sqrt(sincid) cos(alpha)

      a^2 = 2 a sqrt(sincid) cos(alpha)

      a = 2 sqrt(sincid) cos(alpha)

      p1[0]=p0[0]-a*cn0;
      p1[1]=p0[1]-a*cn1;
      p1[2]=p0[2]-a*cn2;

      The derivative of the curved tile is:
      (2ax+by+cx+n0,bx+dz+2ey+n1,cx+dy+2fz+n2)
      *************************************************/

      cn0=2.0*facet[ifhit].ct[0]*ray->x[0]
             +facet[ifhit].ct[1]*ray->x[1]
             +facet[ifhit].ct[2]*ray->x[0]
             +facet[ifhit].ct[6];

      cn1=    facet[ifhit].ct[1]*ray->x[0]
             +facet[ifhit].ct[3]*ray->x[2]
         +2.0*facet[ifhit].ct[4]*ray->x[1]
             +facet[ifhit].ct[7];
 
      cn2=    facet[ifhit].ct[2]*ray->x[0]
             +facet[ifhit].ct[3]*ray->x[1]
         +2.0*facet[ifhit].ct[5]*ray->x[2]
             +facet[ifhit].ct[8];

      #ifdef DEBUG
      fprintf(jpfp,"cn0,cn1,cn2=%f %f %f\n",cn0,cn1,cn2);
      #endif

      /* make the normal facing downward */
      if (cn2>0.0) {
            cn0=-cn0;
            cn1=-cn1;
            cn2=-cn2;
      }

      WF_normalize(&cn0,&cn1,&cn2);

      crossp=(ray->p[0]*cn0+ray->p[1]*cn1+ray->p[2]*cn2);

      cosa2=crossp*crossp/sincid;
      cosa=sqrt(cosa2);

      #ifdef DEBUG
      fprintf(jpfp,"cosa=%f\n",cosa);
      #endif

      sina2=1.0-cosa2;
      sina2=MAX(0.0,sina2);

      if (itetranew==-2) iregnew=-2;
      else iregnew=tetra[itetranew].ireg;

      if ((ray->iref<nrefseq && iregnew==irefseq[ray->iref]) || itetranew==-2) {
            fprintf(jpfp,"To be reflected: iregnew=%d\n",iregnew);
            fprintf(jpfp,"irefseq[%d]=%d\n",ray->iref,irefseq[ray->iref]);

            b=2.0*sqrt(sincid)*cosa;

            fprintf(jpfp,"b=%f p=%f %f %f\n",b,ray->p[0],ray->p[1],ray->p[2]);
            /* because n points downward */
            if (ray->shootupward==1) {
                  ray->p[0]-=b*cn0;
                  ray->p[1]-=b*cn1;
                  ray->p[2]-=b*cn2;
            } else {
                  ray->p[0]+=b*cn0;
                  ray->p[1]+=b*cn1;
                  ray->p[2]+=b*cn2;
            }

            fprintf(jpfp,"ref: p=%f %f %f\n",
            ray->p[0],ray->p[1],ray->p[2]);

            s_normalize(ray->p,sincid);
            ray->iref++;
            ray->shootupward=-ray->shootupward;
      } else {

            ray->itetra=itetranew;
   
            #ifdef DEBUG
            fprintf(jpfp,"Snell's law to be applied\n");
            #endif   

            strans=s_intp(
	          point,             /* control point */
                  &tetra[itetranew], /* this tetra */
                  ray->x);           /* the current position */

            ray->v=1.0/sqrt(strans);

	    #ifdef DEBUG
            fprintf(jpfp,"sincid,strans=%f,%f\n",sincid,strans);
            #endif

            sinb2=sina2*sincid/strans;   /* that's sloth, you silly! */

            /******************************************************
            if sinb2 > 1, it is overcritical, return SPOILED
            *******************************************************/

	    #ifdef DEBUG
            fprintf(jpfp,"ifhit=%d\n",ifhit);
	          fprintf(jpfp,"p=%f %f %f\n",ray->p[0],ray->p[1],ray->p[2]);
	          fprintf(jpfp,"n=%f %f %f\n",cn0,cn1,cn2);
	          fprintf(jpfp,"sina=%f sinb=%f\n",sqrt(sina2),sqrt(sinb2));
	          fprintf(jpfp,"sincid=%f strans=%f\n",sincid,strans);
	          fprintf(jpfp,"v0=%f v1=%f\n",1.0/sqrt(sincid),1.0/sqrt(strans));
	    #endif

            if (sinb2>=1.0) {
                  ray->icode=SPOILED;
                  fprintf(jpfp,"Overcritical\n");
                  ray->lastfacet=-1;
                  ray->icode=SPOILED;
                  return OUT_OF_MODEL;
            }

            cosb=sqrt(1.0-sinb2);

            b = cosb*sqrt(strans) - cosa*sqrt(sincid);

            ray->p[0]=ray->p[0]+b*cn0;
            ray->p[1]=ray->p[1]+b*cn1;
            ray->p[2]=ray->p[2]+b*cn2;

            s_normalize(ray->p,strans);
      }

      #ifdef DEBUG
      fprintf(jpfp,"new p=%f %f %f\n",
      ray->p[0],ray->p[1],ray->p[2]);
      fprintf(jpfp,"HIT_FACET_FIRST\n");
      #endif

      ray->lastfacet=ifhit;
      return HIT_FACET_FIRST;
}

void s_normalize(float p[3],float s)
/*******************************************************************
s_normalize - normalize and divided by s
********************************************************************
Function Prototype:
void s_normalize(float p[3],float s);
********************************************************************
Input:
float s sloth
Input/Output:
float p[3] ray vector
*********************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
********************************************************************/
{
      float norm;

      norm=sqrt((p[0]*p[0]+p[1]*p[1]+p[2]*p[2])/s);
	
      p[0]/=norm;
      p[1]/=norm;
      p[2]/=norm;
}

float tetrasolver(float ct[10],float p[3],float gs[3],float xst[3],
float sg0)
/******************************************************************
tetrasolver - solving the 4th order equation
*******************************************************************
Function prototype:
float tetrasolver(float ct[10],float p[3],float gs[3],float xst[3],
float sg0);
*******************************************************************
Inputs:
float ct[10] coefficients for quadratic tile
float p[3]   ray vector
float gs[3]  sloth gradient
float xst[3] starting point of the ray
float sg0    initial guess for sigma
Outputs:
return the optimal sigma (by Newton's method)
*******************************************************************
Author: CWP: Zhaobo Meng, Sept 1997
*******************************************************************/
{
      float sg,dfdsg,fsg,x0,y0,z0;
      int iter;
      int MaxIterations=6;
      sg=sg0;
      for (iter=0;iter<MaxIterations;iter++) {
            
            x0=xst[0]+(p[0]+0.25*gs[0]*sg)*sg;
            y0=xst[1]+(p[1]+0.25*gs[1]*sg)*sg;
            z0=xst[2]+(p[2]+0.25*gs[2]*sg)*sg;

            fsg=ct[0]*x0*x0+ct[1]*x0*y0+ct[2]*x0*z0+
                ct[3]*y0*z0+ct[4]*y0*y0+ct[5]*z0*z0+
                ct[6]*x0+ct[7]*y0+ct[8]*z0+ct[9];

            if (fabs(fsg)<EPS) break;

            dfdsg=p[0]*(2.0*ct[0]*x0+ct[1]*y0+ct[2]*x0+ct[6])
                 +p[1]*(ct[1]*y0+ct[3]*z0+2.0*ct[4]*y0+ct[7])
                 +p[2]*(ct[2]*x0+ct[3]*y0+2.0*ct[4]*z0+ct[8]); 

            if (fabs(dfdsg)<EPS) break;
            sg-=fsg/dfdsg;
      }
      if (sg<=0.0) return sg0;
      return sg;
}
