/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* TETRAMOD: $Revision: 1.5 $ ; $Date: 2011/11/21 16:53:31 $       */

#include "tetra.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
"						 			",
" TETRAMOD - TETRAhedron MODel builder. In each layer, velocity gradient",
"       is constant or a 2-D grid; horizons could be a uniform grid and/or",
"       added by a 2-D grid specified.   				",
"									",
" tetramod [parameters] > tetrafile		 			",
" 									",
" Required parameters:							",
"                                                                       ",
" nxhz=		number of samples (2nd dimension) for horizons		",
" nyhz=		number of samples (1st dimension) for horizons		",
" hzfile=	output xhz,yhz,zhz,v0hz,v1hz for viewer3                ",
"                                                                       ",
" Optional parameters:							",
"                                                                       ",
" xmin=0       	x of the lower left point in the model	        	",
" ymin=0        y of the lower left point in the model                  ",
" xmax=2        x of the upper right point in the model                 ",
" ymax=2        y of the upper right point in the model                 ",
" zmax=(max z)  max z in the model                                      ",
" blt=1.0       bottom layer thickness                                  ",
" nhz=1		number of layers in the model (except the model base)	",
" ficth=-1      ficticious horizons (no velocity interpolation based on them)",
" The following four numbers define the four corners for 3-D model;	",
" z00=0,0.6,1.2,... 	z at (xmin,ymin) on each horizon		",
" z01=0,0.6,1.2,... 	z at (xmin,ymax) on each horizon		",
" z10=0,0.6,1.2,... 	z at (xmax,ymin) on each horizon                ",
" z11=0,0.6,1.2,... 	z at (xmax,ymax) on each horizon                ",
" v00=1.0,2.0,3.0,...	v at (xmin,ymin) on each horizon		",
" v01=1.0,2.0,3.0,...	v at (xmin,ymax) on each horizon		",
" v10=1.0,2.0,3.0,...	v at (xmax,xmin) on each horizon                ",
" v11=1.0,2.0,3.0,...	v at (xmax,xmax) on each horizon                ",
" dvdz00=0,0,0,...   	dvdz at (xmin,ymin) on each horizon		",
" dvdz01=0,0,0,...   	dvdz at (xmin,ymax) on each horizon		",
" dvdz10=0,0,0,...   	dvdz at (xmax,xmin) on each horizon             ",
" dvdz11=0,0,0,...   	dvdz at (xmax,xmax) on each horizon             ",
" x0file=   	x grid for horizon 0 					",
" y0file=   	y grid for horizon 0 					",
" z0file=	z grid for horizon 0 added to 4-z interpolation   	",
" v0file=	v grid for horizon 0 added to 4-v interpolation   	",
" dvdz0file=	dvdz grid for horizon 00 added to 4-dvdz interpolation	",
"									",
" x1file=	x grid for horizon 1					",
" y1file=	y grid for horizon 1					",
" z1file=	z grid for horizon 1 added to 4-z interpolation  	",
" v1file=	v grid for horizon 1 added to 4-v interpolation  	",
" dvdz1file=	dvdz grid for horizon 1 added to 4-dvdz interpolation	",
" ...		for horizon  #2, #3 #4, etc... 				",
" verbose=0	=1 print some useful iinformation			",
"									",
" Remarks:								",
" TETRAMOD defines its own grammar to describe a 3-D model (including a ",
" tetra file for ray tracer SUTETRARAY and a horizon file for VIEWER3   ",
"									",
" Disclaimer:                                                           ",
" This is a research code that will take considerable work to get into  ",
" the form of a a production level 3D migration code. The code is       ",
" offered as is, along with tetramod and sukdmig3d, to provide a starting",
" point for researchers who wish to write their own 3D migration codes.",
"                                                                       ",
NULL};

/*
 * Credits:
 *  	CWP: Zhaobo Meng, 1996
 *
 *  Reference:
 *  Zhaobo Meng and Norman Bleistein, Wavefront Construction (WF) Ray
 * "Tracing in Tetrahedral Models -- Application to 3-D traveltime and
 *  ray path computations, CWP report 251, 1997
 *
 */
/**************** end self doc *******************************************/

#define EPS 0.0001
#define SmallReal 0.1e-10
#define SWAP(a,b) {float temp=(a);(a)=(b);(b)=temp;}
#define np 9
#define maxiterations 5

/* structures used internally */
void write_point(
/************************************************************
write_point - write control points to a file
************************************************************
Function Prototype:
void write_point(struct POINT *point,int npoint);
************************************************************
Input:
int npoint number of points
Ouput:
struct POINT *point control points
************************************************************/
      struct POINT *point,
      int npoint);

void write_facet(
/************************************************************
Input:
int nfacet number of facets
Ouput:
struct FACET *facet facet information
************************************************************/
      struct FACET *facet,
      int nfacet);

void write_tetra(
/************************************************************
Input:
int ntetra number of tetra
Ouput:
struct TETRA *tetra tetra information
************************************************************/
      struct TETRA *tetra,
      int ntetra);

void tm_normal(
/************************************************************
Inputs:
float x0,y0,z0 first point
float x1,y1,z1 second point
float x2,y2,z2 third point
outputs:
float *n1,*n2,*n3 the normal
************************************************************/
      float x0,float x1,float x2,
      float y0,float y1,float y2,
      float z0,float z1,float z2,
      float *n1,float *n2,float *n3);

void tm_normalize(
/************************************************************
Inputs/outputs:
float *n1,*n2,*n3 normalize this vector
************************************************************/
float *n1,float *n2,float *n3);

void gradient(
/************************************************************
Inputs:
float x0[3],x1[3],x2[3],x3[3] 4 vertices
float s0,s1,s2,s3 sloth at the 4 vertices
Outputs:
float gs[3] the gradient of sloth
************************************************************/
      float x0[3],  /*point0*/
      float s0,     /*sloth at point0*/
      float x1[3],  /*point1*/
      float s1,     /*sloth at point1*/
      float x2[3],  /*point2*/
      float s2,     /*sloth at point2*/
      float x3[3],  /*point3*/
      float s3,     /*sloth at point3*/
      float gs[3]);

int gaussj(
/************************************************************
Inputs:
float a[3][3] 
Input/Output:
float b[3] the soluation of a x = b
************************************************************/
      float a[3][3],
      float b[3]);

float norm2(float xy[],int n);

void ax(
/************************************************************
Inputs:
float a[np][np]
float x[np] 
Output:
float y[3] y=ax
************************************************************/
float a[np][np],
float x[np],
float y[np]);

void tetracgr(
/************************************************************
Inputs:
float a[np][np]
float rhs[np] 
Input/Output:
float ct[np+1] the soluation of a x = rhs, x->ct
************************************************************/
float a[np][np],
float ct[np+1],
float rhs[np]);

int 
main(int argc, char **argv)
{
      int nhz; 	      /*number of horizons except the surface*/
      int nxhz;       /*number of samples in x */
      int nyhz;       /*number of samples in y (1st dimension)*/

      float dxhz;    
      float dyhz;   

      float ***xhz;   /*x of upper horizon: fastest x; slowest ihz*/
      float ***yhz;   /*y of upper horizon: fastest x; slowest ihz*/
      float ***zhz;   /*z of upper horizon: fastest x; slowest ihz*/
      float ***v0hz;  /*v below upper horizon: fastest x; slowest y*/
      float ***v0hztem;/*an extra copy of v0hz*/
      float ***v1hz;  /*v at the bottom of this region*/
      float ***dvdzhz;/*v'z below upper horizon: fastest x; slowest y*/
      float ***dvdzhztem;

      float ***n1hz;  /*normals on horizon vertexes*/
      float ***n2hz;  /*normals on horizon vertexes*/
      float ***n3hz;  /*normals on horizon vertexes*/

      /*The following quatities are 1 smaller in size in each 
      dimension than n1hz, n2hz and n3hz. This is because the
      following quantities are the normals to the triangles*/
      float **n1ll;   /*n1ll[nyhz-1][nxhz-1]*/
      float **n1ur;   /*n1ur[nyhz-1][nxhz-1]*/
      float **n2ll;   /*n2ll[nyhz-1][nxhz-1]*/
      float **n2ur;   /*n2ur[nyhz-1][nxhz-1]*/
      float **n3ll;   /*n3ll[nyhz-1][nxhz-1]*/
      float **n3ur;   /*n3ur[nyhz-1][nxhz-1]*/

      int npoint;          /*number of control points*/
      struct POINT *point; /*control points of size npoints*/

      int nfacet;          /*number of facets*/
      struct FACET *facet; 

      int ntetra;          /*number of tetra*/
      struct TETRA *tetra; /*tetra of size: nhz*(nxhz-1)*(nyhz-1)*6*/

      /**********************************************************
      The users need to define horizons this way: first gives the
      z values at the four corners for each horizon (if not given,
      0 value will be assigned), the horizon will be linearly 
      interpolated using the four values; then the user could define a
      z grid, then this grid will be added to the interpolated 
      horizon (of course if not given, nothing will be added to the
      interpolated horizon.
      Likewise, v, dvdz are all processed this way.
      ***********************************************************/ 
      float *z00; 	/*z at (xmin,ymin) on each horizon*/
      float *z01; 	/*z at (xmin,ymax) on each horizon*/
      float *z10; 	/*z at (xmax,ymin) on each horizon*/
      float *z11; 	/*z at (xmax,ymax) on each horizon*/

      float *v00; 	/*v at (xmin,ymin) on each horizon*/
      float *v01; 	/*v at (xmin,ymax) on each horizon*/
      float *v10; 	/*v at (xmax,ymin) on each horizon*/
      float *v11; 	/*v at (xmax,ymax) on each horizon*/

      float *dvdz00; 	/*dvdz at (xmin,ymin) on each horizon*/
      float *dvdz01;	/*dvdz at (xmin,ymax) on each horizon*/
      float *dvdz10;    /*dvdz at (xmax,ymin) on each horizon*/
      float *dvdz11;    /*dvdz at (xmax,ymax) on each horizon*/

      char *xfile;      /*grid file for x0*/
      char *yfile;	/*grid file for y0*/
      char *zfile;	/*grid file for z0 (added to z)*/
      char *vfile;	/*grid file for v0 (added to v)*/
      char *dvdzfile;   /*grid file for dvdz (added to dvdz)*/

      /*********************************************************
      These are the output. Will be used by viewer3, suwfrayt3d
      and trip, etc.
      *********************************************************/
      char *hzfile; 	  /*horizon file for viewer3 and suwfrayt3d*/
      FILE *hzfp;

      float ***zflat;
      float vtemp;

      int *ficth,ifict;
      int nficth;

      /*define the four corners of the model*/
      float xmin,ymin;    /*first reference point*/
      float xmax,ymax;    /*second reference point*/
      float zmax=-999999.9;
      float zmin=0.0;

      float a[np][np],b[np];

      int ixhz,iyhz;
      int ihz; 	          /*index for nhz*/
      int *iihz,iiihz;           /*index for ihz*/
      int verbose;        /*=1 then print some useful iinformation*/
      float fz0,fz1,fv0,fv1,fdvdz0,fdvdz1;
      float beta,alpha;
      float dis0=0.0;
      float dis1=0.0;
      float dis2=0.0;
      int i000,i001,i010,i100,i110,i101,i011,i111;
      int if1,if2,if3,if4,if5,if6,if7,if8,if9,if10,if11,if12;
      int it1,it2,it3,it4,it5,it6;
      int ntrap,nhztrap,nhztrap1;
      int nhzfacetx,nfacetx;
      int nhzfacety,nfacety;
      int iwh,ip;
      int nhzpoint,ihz0=0;
      float blt;         /*bottom layer thickness*/
      int ip0,ip1,ip2,ip3;
      int itetra;
      int jumphz,if1p,if2p;
      int ifs;

      /* hook up getpar */
      initargs(argc,argv);
      requestdoc(1);

      xfile=(char *)malloc(80);
      yfile=(char *)malloc(80);
      zfile=(char *)malloc(80);
      vfile=(char *)malloc(80);
      dvdzfile=(char *)malloc(80);

      /* get parameters */
      if (!getparint("nhz",&nhz))	
       	    nhz=1;
      if (!getparint("nxhz",&nxhz))   
	    err("Must specify nxhz");
      if (!getparint("nyhz",&nyhz))   
	    err("Must specify nyhz");

      if (!getparstring("hzfile",&hzfile)) 	
       	    err("Must specify hzfile");
 
      if (!getparint("verbose",&verbose)) 	
       	    verbose=0;

      nficth=countparval("ficth");
      ficth=alloc1int(nficth);
      getparint("ficth",ficth);
      if (nficth==1 && ficth[0]==-1) nficth=0;

      if (verbose) {
	    fprintf(stderr,"nficth=%d\n",nficth);
            for (ifict=0;ifict<nficth;ifict++)
                  fprintf(stderr,"ficth[%d]=%d",ifict,ficth[ifict]);
      }

      if (!getparfloat("blt",&blt)) blt=1.0;

      if (verbose)
	    warn("hzfile=%s\n",hzfile);

      if (!getparfloat("xmin",&xmin)) xmin=0.0;
      if (!getparfloat("xmax",&xmax)) xmax=2.0;
      if (!getparfloat("ymin",&ymin)) ymin=0.0;
      if (!getparfloat("ymax",&ymax)) ymax=2.0;
      if (!getparfloat("zmax",&zmax)) zmax=0.0;

      if ((hzfp=fopen(hzfile,"w"))==NULL)
       	    err("Can not open hzfile");

      if (verbose) {
       	    warn("nhz=%d",nhz);
            warn("xmin=%g\nxmax=%g\nymin=%g\nymax=%g\n",xmin,xmax,ymin,ymax);
      }
	
      z00   =alloc1float(nhz);
      z01   =alloc1float(nhz);
      z10   =alloc1float(nhz);
      z11   =alloc1float(nhz);
      v00   =alloc1float(nhz);	
      v01   =alloc1float(nhz);
      v10   =alloc1float(nhz);
      v11   =alloc1float(nhz);
      dvdz00=alloc1float(nhz);
      dvdz01=alloc1float(nhz);
      dvdz10=alloc1float(nhz);
      dvdz11=alloc1float(nhz);

      iihz=ealloc1int(nhz);

      if (!getparfloat("z00",z00))
	    for(ihz=0;ihz<nhz;ihz++) 
	       	  z00[ihz] = ihz*0.6;
      if (!getparfloat("z01",z01))
	    for(ihz=0;ihz<nhz;ihz++) 
		  z01[ihz] = ihz*0.6;
      if (!getparfloat("z10",z10))
	    for(ihz=0;ihz<nhz;ihz++) 
		  z10[ihz] = ihz*0.6;
      if (!getparfloat("z11",z11))
	    for(ihz=0;ihz<nhz;ihz++) 
	       	  z11[ihz] = ihz*0.6;

      if (verbose) {	
       	    for (ihz=0;ihz<nhz;ihz++) 
	       	  warn("layer %d z: %f %f %f %f",ihz,
			z00[ihz],z01[ihz],z10[ihz],z11[ihz]);
      }

      /************************************************
      The default velocity is 2.0,3.0,...,in km/s
      ************************************************/
      if (!getparfloat("v00",v00))
	    for(ihz=0;ihz<nhz;ihz++) v00[ihz] = ihz+1;
      if (!getparfloat("v01",v01))
	    for(ihz=0;ihz<nhz;ihz++) v01[ihz] = ihz+1;
      if (!getparfloat("v10",v10))
	    for(ihz=0;ihz<nhz;ihz++) v10[ihz] = ihz+1;
      if (!getparfloat("v11",v11))
	    for(ihz=0;ihz<nhz;ihz++) v11[ihz] = ihz+1;

      if (verbose) 
	    for (ihz=0;ihz<nhz;ihz++) 
		  warn("layer %d v: %f %f %f %f",ihz,
		       v00[ihz],v01[ihz],v10[ihz],v11[ihz]);

      if (!getparfloat("dvdz00",dvdz00))
	    for(ihz=0;ihz<nhz;ihz++) dvdz00[ihz] = 0.0;
      if (!getparfloat("dvdz01",dvdz01))
	    for(ihz=0;ihz<nhz;ihz++) dvdz01[ihz] = 0.0;
      if (!getparfloat("dvdz10",dvdz10))
	    for(ihz=0;ihz<nhz;ihz++) dvdz10[ihz] = 0.0;
      if (!getparfloat("dvdz11",dvdz11))
	    for(ihz=0;ihz<nhz;ihz++) dvdz11[ihz] = 0.0;

      if (verbose) 
	    for (ihz=0;ihz<nhz;ihz++) 
		  warn("layer %d dvdz: %f %f %f %f",ihz,
		       dvdz00[ihz],dvdz01[ihz],dvdz10[ihz],dvdz11[ihz]);

      /*************************************************
      Get the size of the horizon (in triangles), which is
      not necessary to be equal to nx and ny, and these
      grids are not uniform 
      *************************************************/
      warn("nxhz=%d",nxhz);
      warn("nyhz=%d",nyhz);
      if (nxhz<=1) err("nxhz must >1");
      if (nyhz<=2) err("nyhz must >2");
      if (nyhz/2*2==nyhz) err("nyhz must be an odd number\n");

      n1hz=ealloc3float(nxhz,nyhz,nhz);
      n2hz=ealloc3float(nxhz,nyhz,nhz);
      n3hz=ealloc3float(nxhz,nyhz,nhz);

      n1ll=ealloc2float(nxhz-1,nyhz-1);
      n1ur=ealloc2float(nxhz-1,nyhz-1);
      n2ll=ealloc2float(nxhz-1,nyhz-1);
      n2ur=ealloc2float(nxhz-1,nyhz-1);
      n3ll=ealloc2float(nxhz-1,nyhz-1);
      n3ur=ealloc2float(nxhz-1,nyhz-1);

      xhz=ealloc3float(nxhz,nyhz,nhz);
      yhz=ealloc3float(nxhz,nyhz,nhz);
      zhz=ealloc3float(nxhz,nyhz,nhz+1);
      v0hz=ealloc3float(nxhz,nyhz,nhz);
      v0hztem=ealloc3float(nxhz,nyhz,nhz);
      v1hz=ealloc3float(nxhz,nyhz,nhz);
      dvdzhz=ealloc3float(nxhz,nyhz,nhz);
      dvdzhztem=ealloc3float(nxhz,nyhz,nhz);

      zflat=ealloc3float(nxhz,nyhz,nhz);

      /*these dxhz and dyhz will only be used if there is no xhz and yhz 
      provided*/

      dxhz=(xmax-xmin)/(nxhz-1);
      dyhz=(ymax-ymin)/(nyhz-1);

      for (ihz=0;ihz<nhz;ihz++) {

	    sprintf(xfile,"x%dfile",ihz);
	    sprintf(yfile,"y%dfile",ihz);
	    sprintf(zfile,"z%dfile",ihz);
	    sprintf(vfile,"v%dfile",ihz);
	    sprintf(dvdzfile,"dvdz%dfile",ihz);

	    if (verbose==1) 
		  warn("%s %s %s %s %s",xfile,
		       	yfile,zfile,vfile,dvdzfile);
		
	    if (getparstring(xfile,&xfile)) {
		  if (fread(xhz[ihz][0],sizeof(float),nxhz*nyhz,
		       	fopen(xfile,"r"))!=nxhz*nyhz)
		       	err("Can not read in %s\n",xfile);
		  warn("read in %s",xfile);
		  for (iyhz=0;iyhz<nyhz;iyhz++) {
		        if (fabs(xhz[ihz][iyhz][0]-xmin)>EPS) 
		              err("xhz[%d][%d][0] != xmin, not allowed",ihz,iyhz);
		        else
			      xhz[ihz][iyhz][0]=xmin;
			if (fabs(xhz[ihz][iyhz][nxhz-1]-xmax)>EPS)
			      err("xhz[%d][%d][%d]=%e != xmax=%e,not allowed",
			      ihz,iyhz,nxhz-1,xhz[ihz][iyhz][nxhz-1],xmax);
			else
			      xhz[ihz][iyhz][nxhz-1]=xmax;
		  }

	    } else { 
		  warn("Using uniform Cartesian grids for x-horizon%d",ihz);
		  for (ixhz=0;ixhz<nxhz;ixhz++)
		       	for (iyhz=0;iyhz<nyhz;iyhz++)
		       		xhz[ihz][iyhz][ixhz]=xmin+dxhz*ixhz; 
	    }

	    if (getparstring(yfile,&yfile)) {
	       	  if (fread(yhz[ihz][0],sizeof(float),nxhz*nyhz,
		       	fopen(yfile,"r"))!=nyhz*nxhz)
		       	      err("Can not read in %s\n",yfile);
		  warn("read in %s",yfile);
		  for (ixhz=0;ixhz<nxhz;ixhz++) {
		        if (fabs(yhz[ihz][0][ixhz]-ymin)>EPS) 
		      	      err("yhz[%d][0][%d]=%e != ymin=%e,not allowed",ihz,ixhz,
			      yhz[ihz][0][ixhz],ymin);
			else
			      yhz[ihz][0][ixhz]=ymin;
			if (fabs(yhz[ihz][nyhz-1][ixhz]-ymax)>EPS)
			      err("yhz[%d][%d][%d]=%e!=ymax=%e,not allowed",
			           ihz,nyhz-1,ixhz,yhz[ihz][nyhz-1][ixhz],ymax);
			else
			      yhz[ihz][nyhz-1][ixhz]=ymax;
		  }
	    } else {
		  warn("Using uniform Cartesian grids for y-horizon%d",ihz);
		  for (ixhz=0;ixhz<nxhz;ixhz++)
		        for (iyhz=0;iyhz<nyhz;iyhz++)
		      	      yhz[ihz][iyhz][ixhz]=ymin+iyhz*dyhz;
	    }

	    if (getparstring(zfile,&zfile)) {
		  if (fread(zhz[ihz][0],sizeof(float),nyhz*nxhz,
		        fopen(zfile,"r"))!=nyhz*nxhz)
		        err("Can not read in %s\n",zfile);
		  warn("read in %s",zfile);
	    } else {
	          warn("Using uniform cartesian grids for z-horizon%d",ihz);
	          for (ixhz=0;ixhz<nxhz;ixhz++)
	       	        for (iyhz=0;iyhz<nyhz;iyhz++)
			      zhz[ihz][iyhz][ixhz]=0;
	    }
	
	    if (getparstring(vfile,&vfile)) {       
	          if (fread(v0hz[ihz][0],sizeof(float),
	       	        nxhz*nyhz,fopen(vfile,"r"))!=
		       	      nxhz*nyhz)       
			      err("Can not read in %s\n",vfile);  
		  warn("read in %s",vfile);
	    } else {
		  warn("Using default for v-horizon %d",ihz);
	          for (ixhz=0;ixhz<nxhz;ixhz++)
		        for (iyhz=0;iyhz<nyhz;iyhz++)
		       	      v0hz[ihz][iyhz][ixhz]=0;
	    }

	    if (getparstring(dvdzfile,&dvdzfile)) {
	          if (fread(dvdzhz[ihz][0],sizeof(float),nyhz*nxhz,
			fopen(dvdzfile,"r"))!=nyhz*nxhz)       
		       	      err("Can not read in %s\n",dvdzfile);  
		  warn("read in %s",dvdzfile);
	    } else {
		  warn("Using default for dvdz-horizon %d",ihz);
		  for (ixhz=0;ixhz<nxhz;ixhz++)
			for (iyhz=0;iyhz<nyhz;iyhz++)
			      dvdzhz[ihz][iyhz][ixhz]=0;
	    }

            checkpars();

	    /*************************************************************
	    zhz is added to the plane determined by the four corners
	    *************************************************************/
	    for (ixhz=0;ixhz<nxhz;ixhz++){
	          for (iyhz=0;iyhz<nyhz;iyhz++) {
		        alpha=(xhz[ihz][iyhz][ixhz]-xmin)/(xmax-xmin);
		        fz0=alpha*z01[ihz]+(1-alpha)*z00[ihz];
		        fz1=alpha*z11[ihz]+(1-alpha)*z10[ihz];
		        beta =(yhz[ihz][iyhz][ixhz]-ymin)/(ymax-ymin);
		        zflat[ihz][iyhz][ixhz]=beta*fz1+(1-beta)*fz0;
			zhz[ihz][iyhz][ixhz]+=zflat[ihz][iyhz][ixhz];
		  }
	    }
      }

      for (iyhz=0;iyhz<nyhz;iyhz++)
            for (ixhz=0;ixhz<nxhz;ixhz++)
	          zmax=MAX(zmax,zhz[nhz-1][iyhz][ixhz]);

      zmax+=blt;
      for (iyhz=0;iyhz<nyhz;iyhz++)
            for (ixhz=0;ixhz<nxhz;ixhz++)
	          zhz[nhz][iyhz][ixhz]=zmax;

      for (ihz=0;ihz<nhz;ihz++) {
  	    if (nficth!=0 && ihz!=0) {
	          for (iiihz=ihz;iiihz>=0;iiihz--) {
		        for (ifict=0;ifict<nficth;ifict++) 
                              if (iiihz==ficth[ifict]) break;
                        if (ifict==nficth) break;
                  }
                  iihz[ihz]=iiihz;
                  if (iiihz==nhz) iihz[ihz]=ihz;
            } else
                  iihz[ihz]=ihz;

            /* iihz is the previous non-fictitious horizon */
            fprintf(stderr,"iihz[%d]=%d\n",ihz,iihz[ihz]);
      }

      for (ihz=0;ihz<nhz;ihz++)
	    for (ixhz=0;ixhz<nxhz;ixhz++)
	          for (iyhz=0;iyhz<nyhz;iyhz++) {
                        v0hztem[ihz][iyhz][ixhz]=v0hz[ihz][iyhz][ixhz];
			dvdzhztem[ihz][iyhz][ixhz]=dvdzhz[ihz][iyhz][ixhz];
	          }
      for (ihz=0;ihz<nhz;ihz++){
	    /*************************************************************
	    v0hz and dvdzhz are added to the velocity plane determined by 
	    velocity at the four corners
	    *************************************************************/
	    for (ixhz=0;ixhz<nxhz;ixhz++){
	          for (iyhz=0;iyhz<nyhz;iyhz++){

                        iiihz=iihz[ihz];
                        if (ihz<nhz-1) {
			      if (zhz[ihz][iyhz][ixhz]>zhz[ihz+1][iyhz][ixhz]) { 
			            SWAP(zhz[ihz][iyhz][ixhz],zhz[ihz+1][iyhz][ixhz]);
                                    iiihz=iihz[ihz+1];
			      }
                        }

	     	      	alpha=(xhz[ihz][iyhz][ixhz]-xmin)/(xmax-xmin);
	       	        fv0=alpha*v01[iiihz]+(1-alpha)*v00[iiihz];
	       	        fv1=alpha*v11[iiihz]+(1-alpha)*v10[iiihz];
	       	        fdvdz0=alpha*dvdz01[iiihz]+(1-alpha)*dvdz00[iiihz];
	       	        fdvdz1=alpha*dvdz11[iiihz]+(1-alpha)*dvdz10[iiihz];
	       	        beta=(yhz[ihz][iyhz][ixhz]-ymin)/(ymax-ymin);

			vtemp=v0hztem[iiihz][iyhz][ixhz]+
	       	              beta*fv1+(1-beta)*fv0;
	       	        dvdzhz[ihz][iyhz][ixhz]=dvdzhztem[iiihz][iyhz][ixhz]+
	       	              beta*fdvdz1+(1-beta)*fdvdz0;

			v0hz[ihz][iyhz][ixhz]=vtemp+
			      dvdzhz[ihz][iyhz][ixhz]*(zhz[ihz][iyhz][ixhz]-
			      zflat[iiihz][iyhz][ixhz]);

                        v1hz[ihz][iyhz][ixhz]=vtemp+
                              dvdzhz[ihz][iyhz][ixhz]*(
                              zhz[ihz+1][iyhz][ixhz]-
                              zflat[iiihz][iyhz][ixhz]);

                        #ifdef DEBUG
                        if (ixhz==0 && iyhz==0) {
                              fprintf(stderr,"iiihz=%d v0hz=%f v1hz=%f \n",
                              iiihz,v0hz[ihz][iyhz][ixhz],v1hz[ihz][iyhz][ixhz]);
                              fprintf(stderr,"vtemp=%f\n",vtemp);
                              fprintf(stderr,"zflat=%f zhz=%f\n",
                              zflat[iiihz][iyhz][ixhz],zhz[ihz][iyhz][ixhz]);
                        }
                        #endif
		  }
	    }

	    /**********************************************************
	    Output the normals for suwfrayt3d
	    All triangles look like:

		   (ix,iy+1) (ix+1,iy+1)	
			________
			|\     |
			| \  1 |
			|  \   |
			|   \  | 
			| 0  \ |
			|     \|
			--------  
		    (ix,iy) (ix+1,iy) 

	    **********************************************************/
	    for (ixhz=0;ixhz<nxhz-1;ixhz++) {
		   for (iyhz=0;iyhz<nyhz-1;iyhz++) {
		 	  tm_normal(xhz[ihz][iyhz][ixhz],xhz[ihz][iyhz][ixhz+1],
			        xhz[ihz][iyhz+1][ixhz],
			        yhz[ihz][iyhz][ixhz],yhz[ihz][iyhz][ixhz+1],
       		                yhz[ihz][iyhz+1][ixhz],
		                zhz[ihz][iyhz][ixhz],zhz[ihz][iyhz][ixhz+1],
       		          	zhz[ihz][iyhz+1][ixhz],n1ll[iyhz]+ixhz,
				n2ll[iyhz]+ixhz,n3ll[iyhz]+ixhz);

			  tm_normal(xhz[ihz][iyhz][ixhz+1],xhz[ihz][iyhz+1][ixhz+1],
				xhz[ihz][iyhz+1][ixhz],
			       	yhz[ihz][iyhz][ixhz+1],yhz[ihz][iyhz+1][ixhz+1],
				yhz[ihz][iyhz+1][ixhz],
			        zhz[ihz][iyhz][ixhz+1],zhz[ihz][iyhz+1][ixhz+1],
				zhz[ihz][iyhz+1][ixhz],n1ur[iyhz]+ixhz,
                                n2ur[iyhz]+ixhz,n3ur[iyhz]+ixhz);
	       	   }
       	    }

	    /****************************************
	    Using linear interpolate the normals:
	    Four corners first; four edges second;
	    interior last
	    ****************************************/
	    n1hz[ihz][0][0]=n1ll[0][0];
	    n2hz[ihz][0][0]=n2ll[0][0];
	    n3hz[ihz][0][0]=n3ll[0][0];
	    n1hz[ihz][0][nxhz-1]=n1ur[0][nxhz-2];
	    n2hz[ihz][0][nxhz-1]=n2ur[0][nxhz-2];
	    n3hz[ihz][0][nxhz-1]=n3ur[0][nxhz-2];
	    n1hz[ihz][nyhz-1][0]=n1ll[nyhz-2][0];
	    n2hz[ihz][nyhz-1][0]=n2ll[nyhz-2][0];
	    n3hz[ihz][nyhz-1][0]=n3ll[nyhz-2][0];
	    n1hz[ihz][nyhz-1][nxhz-1]=n1ur[nyhz-2][nxhz-2];
	    n2hz[ihz][nyhz-1][nxhz-1]=n2ur[nyhz-2][nxhz-2];
	    n3hz[ihz][nyhz-1][nxhz-1]=n3ur[nyhz-2][nxhz-2];
       
            for (ixhz=1;ixhz<nxhz-1;ixhz++) {
	       	  n1hz[ihz][0][ixhz]=n1ll[0][ixhz-1]
	      		+n1ur[0][ixhz-1]+n1ll[0][ixhz];
	       	  n2hz[ihz][0][ixhz]=n2ll[0][ixhz-1]
		        +n2ur[0][ixhz-1]+n2ll[0][ixhz];       
		  n3hz[ihz][0][ixhz]=n3ll[0][ixhz-1]
		        +n3ur[0][ixhz-1]+n1ll[0][ixhz];       

		  n1hz[ihz][nyhz-1][ixhz]=n1ur[nyhz-2][ixhz-1]
                        +n1ll[nyhz-2][ixhz]+n1ur[nyhz-2][ixhz]; 
		  n2hz[ihz][nyhz-1][ixhz]=n2ur[nyhz-2][ixhz-1]
                        +n2ll[nyhz-2][ixhz]+n2ur[nyhz-2][ixhz];  
		  n3hz[ihz][nyhz-1][ixhz]=n3ur[nyhz-2][ixhz-1]
                        +n3ll[nyhz-2][ixhz]+n3ur[nyhz-2][ixhz];  
	    }

	    for (iyhz=1;iyhz<nyhz-1;iyhz++) {
	       	  n1hz[ihz][iyhz][0]=n1ll[iyhz-1][0]
                        +n1ur[iyhz-1][0]+n1ll[iyhz][0];
	          n2hz[ihz][iyhz][0]=n2ll[iyhz-1][0]
                        +n2ur[iyhz-1][0]+n2ll[iyhz][0];
		  n3hz[ihz][iyhz][0]=n3ll[iyhz-1][0]
                        +n3ur[iyhz-1][0]+n3ll[iyhz][0];
                  n1hz[ihz][iyhz][nxhz-1]=n1ur[iyhz-1][nxhz-2]
                        +n1ll[iyhz][nxhz-2]+n1ur[iyhz][nxhz-2];
		  n2hz[ihz][iyhz][nxhz-1]=n2ur[iyhz-1][nxhz-2]
                        +n2ll[iyhz][nxhz-2]+n2ur[iyhz][nxhz-2];
	          n3hz[ihz][iyhz][nxhz-1]=n3ur[iyhz-1][nxhz-2]
                        +n3ll[iyhz][nxhz-2]+n3ur[iyhz][nxhz-2];
            }
		
            for (iyhz=1;iyhz<nyhz-1;iyhz++) {
	     	  for (ixhz=1;ixhz<nxhz-1;ixhz++) {
	       	        n1hz[ihz][iyhz][ixhz]
		              =n1ll[iyhz][ixhz-1]
			      +n1ur[iyhz][ixhz-1]
		     	      +n1ll[iyhz][ixhz]
	       		      +n1ur[iyhz-1][ixhz]
	       		      +n1ll[iyhz-1][ixhz]
	       		      +n1ur[iyhz-1][ixhz-1];
	       	        n2hz[ihz][iyhz][ixhz]
                              =n2ll[iyhz][ixhz-1]
                              +n2ur[iyhz][ixhz-1]
                              +n2ll[iyhz][ixhz]
                              +n2ur[iyhz-1][ixhz]
                              +n2ll[iyhz-1][ixhz]
                              +n2ur[iyhz-1][ixhz-1];
		        n3hz[ihz][iyhz][ixhz]
                              =n3ll[iyhz][ixhz-1]
                              +n3ur[iyhz][ixhz-1]
                              +n3ll[iyhz][ixhz]
                              +n3ur[iyhz-1][ixhz]
                              +n3ll[iyhz-1][ixhz]
                              +n3ur[iyhz-1][ixhz-1];
		 }
	  }

	  for (iyhz=0;iyhz<nyhz;iyhz++) 
		 for (ixhz=0;ixhz<nxhz;ixhz++) 
	            	tm_normalize(&n1hz[ihz][iyhz][ixhz],
		              &n2hz[ihz][iyhz][ixhz],
		       	      &n3hz[ihz][iyhz][ixhz]);
      }

      /******************************************
      Write model parameters to hzfile
      ******************************************/
      fwrite(&nhz,sizeof(int),1,hzfp);
      fwrite(&nxhz,sizeof(int),1,hzfp);
      fwrite(&nyhz,sizeof(int),1,hzfp);
      fwrite(&xmin,sizeof(float),1,hzfp);
      fwrite(&xmax,sizeof(float),1,hzfp);
      fwrite(&ymin,sizeof(float),1,hzfp);
      fwrite(&ymax,sizeof(float),1,hzfp);
      fwrite(&zmin,sizeof(float),1,hzfp);
      fwrite(&zmax,sizeof(float),1,hzfp);
      
      /**********************************************************
      Output the horizon information to file hzfile:
      **********************************************************/	
      for (ihz=0;ihz<nhz;ihz++) {
       	    if (fwrite(xhz[ihz][0],sizeof(float),nxhz*nyhz,hzfp)!=nxhz*nyhz)
		  err("Can not write xhz to hzfp");	

	    if (fwrite(yhz[ihz][0],sizeof(float),nxhz*nyhz,hzfp)!=nxhz*nyhz)
	          err("Can not write yhz to hzfp");

       	    if (fwrite(zhz[ihz][0],sizeof(float),nxhz*nyhz,hzfp)!=nxhz*nyhz)
		  err("Can not write zhz to hzfp");

	    if (fwrite(v0hz[ihz][0],sizeof(float),nxhz*nyhz,hzfp)!=
	          nxhz*nyhz)
	          err("Can not write v0hz to hzfp");

	    if (fwrite(v1hz[ihz][0],sizeof(float),nxhz*nyhz,hzfp)!=
	          nxhz*nyhz)
	          err("Can not write v1hz to hzfp");		
      }

      /**********************************************
      For even iyhz: Lower left   Upper right
                       2             1__2
                       |\            \  |
                       | \            \ |
                       |__\            \|
                       0   1            0

      For odd iyhz: Lower left    Upper right
                       1  2             2
                       |--/            /|
                       | /            / |
                       |/            /__|
                       0             0   1
      ***********************************************/
 
      ntrap=(nxhz-1)*(nyhz-1);         /*number of trapzoids per hz*/
      nhztrap=nhz*ntrap;               /*total number of trapzoids*/
      nhztrap1=(nhz+1)*ntrap;
      nfacetx=(nyhz-1)*nxhz;
      nhzfacetx=nhz*nfacetx;
      nfacety=nyhz*(nxhz-1);
      nhzfacety=nhz*nfacety;
      nhzpoint=nhz*nxhz*nyhz;

      if (verbose) {
	    warn("ntrap=%d nhztrap=%d nhztrap1=%d",ntrap,nhztrap,nhztrap1);
	    warn("nfacetx=%d nhzfacetx=%d",nfacetx,nhzfacetx);
	    warn("nfacety=%d nfacety=%d",nfacety,nhzfacety);
      }

      npoint=2*nhzpoint;
      point=(struct POINT *)malloc(npoint*sizeof(struct POINT));
      if (verbose) fprintf(stderr,"npoint=%d\n",npoint);

      nfacet=2*nhztrap1+             /*facet on horizons: if1 and if2*/
            6*nhztrap+               /*slant: if3 if4 if5 if6 if7 and if8*/
            2*nhzfacetx+             /*normal to x*/
            2*nhzfacety;             /*normal to y*/

      facet=(struct FACET *)malloc(nfacet*sizeof(struct FACET));
      if (verbose) fprintf(stderr,"nfacet=%d\n",nfacet);

      ntetra=6*nhztrap;
      tetra=(struct TETRA *)malloc(ntetra*sizeof(struct TETRA));
      if (verbose) fprintf(stderr,"ntetra=%d\n",ntetra);

      /*Assign the control point information*/
      for (ihz=0;ihz<nhz;ihz++) {
	    fprintf(stderr,"processing: ihz=%d\n",ihz);
            ihz0=MIN(nhz-1,ihz+1);
            for (iyhz=0;iyhz<nyhz;iyhz++) {
                  for (ixhz=0;ixhz<nxhz;ixhz++) {
                        ip=ihz*nxhz*nyhz+iyhz*nxhz+ixhz;
                        point[ip].x[0]=xhz[ihz][iyhz][ixhz];
		        point[ip].x[1]=yhz[ihz][iyhz][ixhz];
		        point[ip].x[2]=zhz[ihz][iyhz][ixhz];
		        point[ip].s=1.0/v0hz[ihz][iyhz][ixhz]/v0hz[ihz][iyhz][ixhz];
                        point[ip].n[0]=n1hz[ihz][iyhz][ixhz];
                        point[ip].n[1]=n2hz[ihz][iyhz][ixhz];
			point[ip].n[2]=n3hz[ihz][iyhz][ixhz];

                        ip+=nhzpoint;
                        point[ip].x[0]=xhz[ihz0][iyhz][ixhz];
		        point[ip].x[1]=yhz[ihz0][iyhz][ixhz];
		        point[ip].x[2]=zhz[ihz+1][iyhz][ixhz];
		        point[ip].s=1.0/v1hz[ihz][iyhz][ixhz]/v1hz[ihz][iyhz][ixhz];
                        if (ihz==nhz-1) {
			      point[ip].n[0]=0.0;
                              point[ip].n[1]=0.0;
			      point[ip].n[2]=1.0;
                        } else {
			      point[ip].n[0]=n1hz[ihz+1][iyhz][ixhz];
                              point[ip].n[1]=n2hz[ihz+1][iyhz][ixhz];
			      point[ip].n[2]=n3hz[ihz+1][iyhz][ixhz];
                        }
                  }
            }
      }

      for (ihz=0;ihz<nhz;ihz++) {
            ihz0=MIN(nhz-1,ihz+1);
            /*For each trapzoid*/
            for (iyhz=0;iyhz<nyhz-1;iyhz++) {
	          for (ixhz=0;ixhz<nxhz-1;ixhz++) {

                        if1=ihz*ntrap+iyhz*(nxhz-1)+ixhz; /*left facet on hz*/
                        iwh=nhztrap1;
                        if2=iwh+if1;                      /*right facet on hz*/
                        iwh=nhztrap1*2;
                        if3=iwh+if1;                      /*upper diagonal facet*/
                        iwh=nhztrap1*2+nhztrap;
                        if4=iwh+if1;                      /*lower diagonal facet*/
                        iwh=nhztrap1*2+nhztrap*2;
                        if5=iwh+if1;                      /*left slant1 facet*/
		        iwh=nhztrap1*2+nhztrap*3;
		        if6=iwh+if1;                      /*right slant1 facet*/
		        iwh=nhztrap1*2+nhztrap*4;
                        if7=iwh+if1;                      /*left slant2 facet*/
		        iwh=nhztrap1*2+nhztrap*5;
		        if8=iwh+if1;                      /*right slant2 facet*/
		        iwh=nhztrap1*2+nhztrap*6;
                        if9=iwh+ihz*nfacetx+iyhz*nxhz+ixhz;/*upper facet facing x*/
                        if10=nhzfacetx+if9;               /*lower facet facing x*/
		        iwh=nhztrap1*2+nhztrap*6+nhzfacetx*2;
                        if11=iwh+ihz*nfacety+iyhz*(nxhz-1)+ixhz;/*upper facet facing y*/
                        if12=nhzfacety+if11;              /*lower facet facing y*/

                        it1=ihz*ntrap+iyhz*(nxhz-1)+ixhz; /*left top tetra*/
                        it2=nhztrap+it1;                  /*left middle tetra*/
                        it3=nhztrap*2+it1;                /*left bottom tetra*/
                        it4=nhztrap*3+it1;                /*right top tetra*/
                        it5=nhztrap*4+it1;                /*right middle tetra*/
                        it6=nhztrap*5+it1;                /*right bottom tetra*/

                        i000=ihz*nxhz*nyhz+iyhz*nxhz+ixhz;/*upper 4 control point*/
                        i010=i000+nxhz;
                        i100=i000+1;
                        i110=i000+nxhz+1;

                        i001=nhzpoint+i000;    /*lower 4 control point*/
                        i011=nhzpoint+i010;
                        i101=nhzpoint+i100;
                        i111=nhzpoint+i110;
                           
		        tetra[it1].ireg=ihz;
		        tetra[it2].ireg=ihz;
		        tetra[it3].ireg=ihz;
		        tetra[it4].ireg=ihz;
		        tetra[it5].ireg=ihz;
		        tetra[it6].ireg=ihz;

                        /*normals to other facet will be calculated later*/
                        if (iyhz%2==0) {
                              tetra[it1].ip[0]=i000;     /*left top tetra*/
                              tetra[it1].ip[1]=i010;
                              tetra[it1].ip[2]=i100;
                              tetra[it1].ip[3]=i101;

                              tetra[it2].ip[0]=i000;     /*left middle tetra*/
                              tetra[it2].ip[1]=i010;
                              tetra[it2].ip[2]=i001;
                              tetra[it2].ip[3]=i101;

                              tetra[it3].ip[0]=i001;     /*left bottom tetra*/
                              tetra[it3].ip[1]=i011;
                              tetra[it3].ip[2]=i101;
                              tetra[it3].ip[3]=i010;

                              tetra[it4].ip[0]=i010;     /*right top tetra*/
                              tetra[it4].ip[1]=i110;
                              tetra[it4].ip[2]=i100;
                              tetra[it4].ip[3]=i101;

                              tetra[it5].ip[0]=i010;     /*right middle tetra*/
                              tetra[it5].ip[1]=i011;
                              tetra[it5].ip[2]=i101;
                              tetra[it5].ip[3]=i110;

                              tetra[it6].ip[0]=i011;     /*right bottom tetra*/
                              tetra[it6].ip[1]=i101;
                              tetra[it6].ip[2]=i111;
                              tetra[it6].ip[3]=i110;

                              tetra[it1].ifacet[0]=if1;  /*upper left tetra*/
                              tetra[it1].ifacet[1]=if3;
                              tetra[it1].ifacet[2]=if11;        
                              tetra[it1].ifacet[3]=if5;

                              tetra[it2].ifacet[0]=if5;  /*middle left tetra*/
                              tetra[it2].ifacet[1]=if9;
                              tetra[it2].ifacet[2]=if12;        
                              tetra[it2].ifacet[3]=if7;

                              tetra[it3].ifacet[0]=if7;  /*lower left tetra*/
                              tetra[it3].ifacet[1]=if10;
                              tetra[it3].ifacet[2]=if4;        
                              tetra[it3].ifacet[3]=if1+ntrap;

                              tetra[it4].ifacet[0]=if2;  /*upper right tetra*/
                              tetra[it4].ifacet[1]=if3;
                              tetra[it4].ifacet[2]=if9+1;        
                              tetra[it4].ifacet[3]=if6;

                              tetra[it5].ifacet[0]=if6;  /*middle right tetra*/
                              tetra[it5].ifacet[1]=if11+nxhz-1;
                              tetra[it5].ifacet[2]=if4;        
                              tetra[it5].ifacet[3]=if8;

                              tetra[it6].ifacet[0]=if8;  /*lower right tetra*/
                              tetra[it6].ifacet[1]=if10+1;
                              tetra[it6].ifacet[2]=if12+nxhz-1;        
                              tetra[it6].ifacet[3]=if2+ntrap;

			      /*centered normal for hz (c1): left hz facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz][iyhz][ixhz+1],
				    xhz[ihz][iyhz+1][ixhz],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz][iyhz][ixhz+1],
       			            yhz[ihz][iyhz+1][ixhz],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz][iyhz][ixhz+1],
       			      	    zhz[ihz][iyhz+1][ixhz],
				    &facet[if1].cn[0],
				    &facet[if1].cn[1],
				    &facet[if1].cn[2]);

			      /*centered normal for hz (c1): right hz facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz+1][ixhz+1],
                                    xhz[ihz][iyhz][ixhz+1],
				    xhz[ihz][iyhz+1][ixhz],
			            yhz[ihz][iyhz+1][ixhz+1],
                                    yhz[ihz][iyhz][ixhz+1],
       			            yhz[ihz][iyhz+1][ixhz],
			            zhz[ihz][iyhz+1][ixhz+1],
                                    zhz[ihz][iyhz][ixhz+1],
       			      	    zhz[ihz][iyhz+1][ixhz],
				    &facet[if2].cn[0],
				    &facet[if2].cn[1],
				    &facet[if2].cn[2]);

			      /*centered normal for if3: upper diagonal facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz+1][ixhz],
                                    xhz[ihz][iyhz][ixhz+1],
				    xhz[ihz0][iyhz][ixhz+1],
			            yhz[ihz][iyhz+1][ixhz],
                                    yhz[ihz][iyhz][ixhz+1],
       			            yhz[ihz0][iyhz][ixhz+1],
			            zhz[ihz][iyhz+1][ixhz],
                                    zhz[ihz][iyhz][ixhz+1],
       			      	    zhz[ihz+1][iyhz][ixhz+1],
				    &facet[if3].cn[0],
				    &facet[if3].cn[1],
				    &facet[if3].cn[2]);

			      /*centered normal for if4: lower diagonal facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz+1][ixhz],
                                    xhz[ihz0][iyhz+1][ixhz],
				    xhz[ihz0][iyhz][ixhz+1],
			            yhz[ihz][iyhz+1][ixhz],
                                    yhz[ihz0][iyhz+1][ixhz],
       			            yhz[ihz0][iyhz][ixhz+1],
			            zhz[ihz][iyhz+1][ixhz],
                                    zhz[ihz+1][iyhz+1][ixhz],
       			      	    zhz[ihz+1][iyhz][ixhz+1],
				    &facet[if4].cn[0],
				    &facet[if4].cn[1],
				    &facet[if4].cn[2]);

			      /*centered normal for if5: left slant1 facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz][iyhz+1][ixhz],
				    xhz[ihz0][iyhz][ixhz+1],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz][iyhz+1][ixhz],
       			            yhz[ihz0][iyhz][ixhz+1],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz][iyhz+1][ixhz],
       			      	    zhz[ihz+1][iyhz][ixhz+1],
				    &facet[if5].cn[0],
				    &facet[if5].cn[1],
				    &facet[if5].cn[2]);

			      /*centered normal for if6: right slant1 facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz+1][ixhz+1],
                                    xhz[ihz][iyhz+1][ixhz],
				    xhz[ihz0][iyhz][ixhz+1],
			            yhz[ihz][iyhz+1][ixhz+1],
                                    yhz[ihz][iyhz+1][ixhz],
       			            yhz[ihz0][iyhz][ixhz+1],
			            zhz[ihz][iyhz+1][ixhz+1],
                                    zhz[ihz][iyhz+1][ixhz],
       			      	    zhz[ihz+1][iyhz][ixhz+1],
				    &facet[if6].cn[0],
				    &facet[if6].cn[1],
				    &facet[if6].cn[2]);

			      /*centered normal for if7: left slant 2 facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz+1][ixhz],
                                    xhz[ihz0][iyhz][ixhz],
				    xhz[ihz0][iyhz][ixhz+1],
			            yhz[ihz][iyhz+1][ixhz],
                                    yhz[ihz0][iyhz][ixhz],
       			            yhz[ihz0][iyhz][ixhz+1],
			            zhz[ihz][iyhz+1][ixhz],
                                    zhz[ihz+1][iyhz][ixhz],
       			      	    zhz[ihz+1][iyhz][ixhz+1],
				    &facet[if7].cn[0],
				    &facet[if7].cn[1],
				    &facet[if7].cn[2]);

			      /*centered normal for if8: right slant 2 facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz+1][ixhz+1],
                                    xhz[ihz0][iyhz+1][ixhz],
				    xhz[ihz0][iyhz][ixhz+1],
			            yhz[ihz][iyhz+1][ixhz+1],
                                    yhz[ihz0][iyhz+1][ixhz],
       			            yhz[ihz0][iyhz][ixhz+1],
			            zhz[ihz][iyhz+1][ixhz+1],
                                    zhz[ihz+1][iyhz+1][ixhz],
       			      	    zhz[ihz+1][iyhz][ixhz+1],
				    &facet[if8].cn[0],
				    &facet[if8].cn[1],
				    &facet[if8].cn[2]);

                              /*control point of left facet on hz*/
			      facet[if1].ip[0]=i000;
			      facet[if1].ip[1]=i100;
			      facet[if1].ip[2]=i010;
                        
			      /*control point of right facet on hz*/
			      facet[if2].ip[0]=i010;
			      facet[if2].ip[1]=i110;
			      facet[if2].ip[2]=i100;

                              /*upper diagonal facet*/
			      facet[if3].ip[0]=i010;
			      facet[if3].ip[1]=i100;
			      facet[if3].ip[2]=i101;
                        
			      /*lower diagonal facet*/
			      facet[if4].ip[0]=i010;
			      facet[if4].ip[1]=i011;
			      facet[if4].ip[2]=i101;

			      /*left slant1 facet*/
			      facet[if5].ip[0]=i000;
			      facet[if5].ip[1]=i010;
			      facet[if5].ip[2]=i101;

			      /*right slant1 facet*/
			      facet[if6].ip[0]=i010;
			      facet[if6].ip[1]=i110;
			      facet[if6].ip[2]=i101;

			      /*left slant2 facet*/
			      facet[if7].ip[0]=i001;
			      facet[if7].ip[1]=i010;
			      facet[if7].ip[2]=i101;

			      /*right slant2 facet*/
			      facet[if8].ip[0]=i011;
			      facet[if8].ip[1]=i110;
			      facet[if8].ip[2]=i101;

                        } else {

			      /*left top tetra*/
                              tetra[it1].ip[0]=i000;
                              tetra[it1].ip[1]=i010;
                              tetra[it1].ip[2]=i110;
                              tetra[it1].ip[3]=i111;

		      	      /*left middle tetra*/
                              tetra[it2].ip[0]=i000;    
                              tetra[it2].ip[1]=i010;
                              tetra[it2].ip[2]=i011;
                              tetra[it2].ip[3]=i111;

			      /*left bottom tetra*/
                              tetra[it3].ip[0]=i001;
                              tetra[it3].ip[1]=i011;
                              tetra[it3].ip[2]=i111;
                              tetra[it3].ip[3]=i000;

			      /*right top tetra*/
                              tetra[it4].ip[0]=i000;
                              tetra[it4].ip[1]=i100;
                              tetra[it4].ip[2]=i110;
                              tetra[it4].ip[3]=i111;

			      /*right middle tetra*/
                              tetra[it5].ip[0]=i000;
                              tetra[it5].ip[1]=i001;
                              tetra[it5].ip[2]=i111;
                              tetra[it5].ip[3]=i100;

			      /*right bottom tetra*/
                              tetra[it6].ip[0]=i100;
                              tetra[it6].ip[1]=i001;
                              tetra[it6].ip[2]=i111;
                              tetra[it6].ip[3]=i101;

			      /*upper left tetra*/
                              tetra[it1].ifacet[0]=if1;
                              tetra[it1].ifacet[1]=if3;
                              tetra[it1].ifacet[2]=if11+nxhz-1;        
                              tetra[it1].ifacet[3]=if5;

			      /*middle left tetra*/
                              tetra[it2].ifacet[0]=if5;
                              tetra[it2].ifacet[1]=if9;
                              tetra[it2].ifacet[2]=if12+nxhz-1;        
                              tetra[it2].ifacet[3]=if7;

			      /*lower left tetra*/
                              tetra[it3].ifacet[0]=if7;
                              tetra[it3].ifacet[1]=if10;
                              tetra[it3].ifacet[2]=if4;        
                              tetra[it3].ifacet[3]=if1+ntrap;

			      /*upper right tetra*/
                              tetra[it4].ifacet[0]=if2;
                              tetra[it4].ifacet[1]=if3;
                              tetra[it4].ifacet[2]=if9+1;        
                              tetra[it4].ifacet[3]=if6;

			      /*middle right tetra*/
                              tetra[it5].ifacet[0]=if6;
                              tetra[it5].ifacet[1]=if4;
                              tetra[it5].ifacet[2]=if11;        
                              tetra[it5].ifacet[3]=if8;

			      /*lower right tetra*/
                              tetra[it6].ifacet[0]=if8;
                              tetra[it6].ifacet[1]=if10+1;
                              tetra[it6].ifacet[2]=if12;        
                              tetra[it6].ifacet[3]=if2+ntrap;

			      /*centered normal for hz (c1): left hz facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz][iyhz+1][ixhz+1],
				    xhz[ihz][iyhz+1][ixhz],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz][iyhz+1][ixhz+1],
       			            yhz[ihz][iyhz+1][ixhz],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz][iyhz+1][ixhz+1],
       			      	    zhz[ihz][iyhz+1][ixhz],
				    &facet[if1].cn[0],
				    &facet[if1].cn[1],
				    &facet[if1].cn[2]);

			      /*centered normal for hz (c1): right hz facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz+1][ixhz+1],
                                    xhz[ihz][iyhz][ixhz+1],
				    xhz[ihz][iyhz][ixhz],
			            yhz[ihz][iyhz+1][ixhz+1],
                                    yhz[ihz][iyhz][ixhz+1],
       			            yhz[ihz][iyhz][ixhz],
			            zhz[ihz][iyhz+1][ixhz+1],
                                    zhz[ihz][iyhz][ixhz+1],
       			      	    zhz[ihz][iyhz][ixhz],
				    &facet[if2].cn[0],
				    &facet[if2].cn[1],
				    &facet[if2].cn[2]);

			      /*centered normal for if3: upper diagonal facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz][iyhz+1][ixhz+1],
				    xhz[ihz0][iyhz+1][ixhz+1],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz][iyhz+1][ixhz+1],
       			            yhz[ihz0][iyhz+1][ixhz+1],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz][iyhz+1][ixhz+1],
       			      	    zhz[ihz+1][iyhz+1][ixhz+1],
				    &facet[if3].cn[0],
				    &facet[if3].cn[1],
				    &facet[if3].cn[2]);

			      /*centered normal for if4: lower diagonal facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz0][iyhz][ixhz],
				    xhz[ihz0][iyhz+1][ixhz+1],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz0][iyhz][ixhz],
       			            yhz[ihz0][iyhz+1][ixhz+1],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz+1][iyhz][ixhz],
       			      	    zhz[ihz+1][iyhz+1][ixhz+1],
				    &facet[if4].cn[0],
				    &facet[if4].cn[1],
				    &facet[if4].cn[2]);

			      /*centered normal for if5: left slant1 facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz][iyhz+1][ixhz],
				    xhz[ihz0][iyhz+1][ixhz+1],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz][iyhz+1][ixhz],
       			            yhz[ihz0][iyhz+1][ixhz+1],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz][iyhz+1][ixhz],
       			      	    zhz[ihz+1][iyhz+1][ixhz+1],
				    &facet[if5].cn[0],
				    &facet[if5].cn[1],
				    &facet[if5].cn[2]);

			      /*centered normal for if6: right slant1 facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz][iyhz][ixhz+1],
				    xhz[ihz0][iyhz+1][ixhz+1],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz][iyhz][ixhz+1],
       			            yhz[ihz0][iyhz+1][ixhz+1],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz][iyhz][ixhz+1],
       			      	    zhz[ihz+1][iyhz+1][ixhz+1],
				    &facet[if6].cn[0],
				    &facet[if6].cn[1],
				    &facet[if6].cn[2]);

			      /*centered normal for if7: left slant 2 facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz0][iyhz][ixhz],
				    xhz[ihz0][iyhz+1][ixhz+1],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz0][iyhz+1][ixhz],
       			            yhz[ihz0][iyhz+1][ixhz+1],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz+1][iyhz+1][ixhz],
       			      	    zhz[ihz+1][iyhz+1][ixhz+1],
				    &facet[if7].cn[0],
				    &facet[if7].cn[1],
				    &facet[if7].cn[2]);

			      /*centered normal for if8: right slant 2 facet*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz+1],
                                    xhz[ihz0][iyhz][ixhz],
				    xhz[ihz0][iyhz+1][ixhz+1],
			            yhz[ihz][iyhz][ixhz+1],
                                    yhz[ihz0][iyhz][ixhz],
       			            yhz[ihz0][iyhz+1][ixhz+1],
			            zhz[ihz][iyhz][ixhz+1],
                                    zhz[ihz+1][iyhz][ixhz],
       			      	    zhz[ihz+1][iyhz+1][ixhz+1],
				    &facet[if8].cn[0],
				    &facet[if8].cn[1],
				    &facet[if8].cn[2]);

			      /*control point for left facet on hz*/
			      facet[if1].ip[0]=i000;
			      facet[if1].ip[1]=i010;
			      facet[if1].ip[2]=i110;

			      /*control point of right facet on hz*/
			      facet[if2].ip[0]=i000;
			      facet[if2].ip[1]=i100;
			      facet[if2].ip[2]=i110;

                              /*upper diagonal facet*/
			      facet[if3].ip[0]=i000;
			      facet[if3].ip[1]=i110;
			      facet[if3].ip[2]=i111;
                        
			      /*lower diagonal facet*/
			      facet[if4].ip[0]=i000;
			      facet[if4].ip[1]=i001;
			      facet[if4].ip[2]=i111;

			      /*left slant1 facet*/
			      facet[if5].ip[0]=i000;
			      facet[if5].ip[1]=i010;
			      facet[if5].ip[2]=i111;

			      /*right slant1 facet*/
			      facet[if6].ip[0]=i000;
			      facet[if6].ip[1]=i100;
			      facet[if6].ip[2]=i111;

			      /*left slant2 facet*/
			      facet[if7].ip[0]=i000;
			      facet[if7].ip[1]=i011;
			      facet[if7].ip[2]=i111;

			      /*right slant2 facet*/
			      facet[if8].ip[0]=i100;
			      facet[if8].ip[1]=i001;
			      facet[if8].ip[2]=i111;
			}

                        if (ihz==0) {
			      facet[if1].itetra[0]=-1;
			      facet[if2].itetra[0]=-1;
                        } else {
                              facet[if1].itetra[0]=    /*left facet on hz*/
                                    nhztrap*2+(ihz-1)*ntrap+iyhz*(nxhz-1)+ixhz;
			      facet[if2].itetra[0]=    /*right facet on hz*/
                                    facet[if1].itetra[0]+3*nhztrap;
                        }
                        facet[if1].itetra[1]=it1;       /*left facet on hz*/
                        facet[if2].itetra[1]=it4;       /*right facet on hz*/
                        facet[if3].itetra[0]=it1;       /*upper diagonal facet*/
                        facet[if3].itetra[1]=it4;       /*upper diagonal facet*/
                        facet[if4].itetra[0]=it3;       /*lower diagonal facet*/
                        facet[if4].itetra[1]=it5;       /*lower diagonal facet*/
                        facet[if5].itetra[0]=it1;       /*left slant1 facet*/
                        facet[if5].itetra[1]=it2;       /*left slant1 facet*/
                        facet[if6].itetra[0]=it4;       /*right slant1 facet*/
                        facet[if6].itetra[1]=it5;       /*right slant1 facet*/
                        facet[if7].itetra[0]=it2;       /*left slant2 facet*/
                        facet[if7].itetra[1]=it3;       /*left slant2 facet*/
                        facet[if8].itetra[0]=it5;       /*right slant2 facet*/
                        facet[if8].itetra[1]=it6;       /*right slant3 facet*/
	          }
	    }
      }
      for (ihz=0;ihz<nhz;ihz++) {
            for (iyhz=0;iyhz<nyhz-1;iyhz++) {
                  for (ixhz=0;ixhz<nxhz;ixhz++) {

		        iwh=nhztrap1*2+nhztrap*6;
                        if9=iwh+ihz*nfacetx+iyhz*nxhz+ixhz;/*upper facet facing x*/
                        if10=nhzfacetx+if9;                /*lower facet facing x*/

                        it1=ihz*ntrap+iyhz*(nxhz-1)+ixhz; /*left top tetra*/
                        it2=nhztrap+it1;                  /*left middle tetra*/
                        it3=nhztrap*2+it1;                /*left bottom tetra*/
                        it4=nhztrap*3+it1;                /*right top tetra*/
                        it5=nhztrap*4+it1;                /*right middle tetra*/
                        it6=nhztrap*5+it1;                /*right bottom tetra*/

                        i000=ihz*nxhz*nyhz+iyhz*nxhz+ixhz;/*upper 4 control point*/
                        i010=i000+nxhz;
                        i100=i000+1;
                        i110=i000+nxhz+1;

                        i001=nhzpoint+i000;    /*lower 4 control point*/
                        i011=nhzpoint+i010;
                        i101=nhzpoint+i100;
                        i111=nhzpoint+i110;
                           
                        if (iyhz%2==0) {

			      /*centered normal for if9: upper facet facing x*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz0][iyhz][ixhz],
				    xhz[ihz][iyhz+1][ixhz],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz0][iyhz][ixhz],
       			            yhz[ihz][iyhz+1][ixhz],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz+1][iyhz][ixhz],
       			      	    zhz[ihz][iyhz+1][ixhz],
				    &facet[if9].cn[0],
				    &facet[if9].cn[1],
				    &facet[if9].cn[2]);

			      /*centered normal for if10: lower facet facing x*/
		 	      tm_normal( 
                                    xhz[ihz0][iyhz+1][ixhz],
                                    xhz[ihz0][iyhz][ixhz],
				    xhz[ihz][iyhz+1][ixhz],
			            yhz[ihz0][iyhz+1][ixhz],
                                    yhz[ihz0][iyhz][ixhz],
       			            yhz[ihz][iyhz+1][ixhz],
			            zhz[ihz+1][iyhz+1][ixhz],
                                    zhz[ihz+1][iyhz][ixhz],
       			      	    zhz[ihz][iyhz+1][ixhz],
				    &facet[if10].cn[0],
				    &facet[if10].cn[1],
				    &facet[if10].cn[2]);

                              /*upper facet facing x*/
			      facet[if9].ip[0]=i000;
			      facet[if9].ip[1]=i010;
			      facet[if9].ip[2]=i001;

                              /*lower facet facing x*/
			      facet[if10].ip[0]=i010;
			      facet[if10].ip[1]=i001;
			      facet[if10].ip[2]=i011;
                        } else {

			      /*centered normal for if9: upper facet facing x*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz0][iyhz+1][ixhz],
				    xhz[ihz][iyhz+1][ixhz],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz0][iyhz+1][ixhz],
       			            yhz[ihz][iyhz+1][ixhz],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz+1][iyhz+1][ixhz],
       			      	    zhz[ihz][iyhz+1][ixhz],
				    &facet[if9].cn[0],
				    &facet[if9].cn[1],
				    &facet[if9].cn[2]);

			      /*centered normal for if10: lower facet facing x*/
		 	      tm_normal( 
                                    xhz[ihz0][iyhz][ixhz],
                                    xhz[ihz0][iyhz+1][ixhz],
				    xhz[ihz][iyhz][ixhz],
			            yhz[ihz0][iyhz][ixhz],
                                    yhz[ihz0][iyhz+1][ixhz],
       			            yhz[ihz][iyhz][ixhz],
			            zhz[ihz+1][iyhz][ixhz],
                                    zhz[ihz+1][iyhz+1][ixhz],
       			      	    zhz[ihz][iyhz][ixhz],
				    &facet[if10].cn[0],
				    &facet[if10].cn[1],
				    &facet[if10].cn[2]);

                              /*upper facet facing x*/
		              facet[if9].ip[0]=i000;
		      	      facet[if9].ip[1]=i010;
			      facet[if9].ip[2]=i011;

                              /*lower facet facing x*/
			      facet[if10].ip[0]=i000;
			      facet[if10].ip[1]=i001;
			      facet[if10].ip[2]=i011;                

                        }

                        if (ixhz==0) {
                              facet[if9].itetra[0]=-1;/*upper facet facing x*/
		              facet[if10].itetra[0]=-1;/*lower facet facing x*/
                        } else {
                              facet[if9].itetra[0]=it4-1;
      		              facet[if10].itetra[0]=it6-1;
                        }

                        if (ixhz==nxhz-1) {
			      facet[if9].itetra[1]=-1;
			      facet[if10].itetra[1]=-1;
                        } else {
                              facet[if9].itetra[1]=it2;    /*upper facet facing x*/
                              facet[if10].itetra[1]=it3;   /*lower facet facing x*/
                        }
                  }
            }
            for (iyhz=0;iyhz<nyhz;iyhz++) {
                  for (ixhz=0;ixhz<nxhz-1;ixhz++) {

       	                iwh=nhztrap1*2+nhztrap*6+nhzfacetx*2;
                        if11=iwh+ihz*nfacety+iyhz*(nxhz-1)+ixhz;/*upper facet facing y*/
                        if12=nhzfacety+if11;          /*lower facet facing y*/

                        it1=ihz*ntrap+iyhz*(nxhz-1)+ixhz; /*left top tetra*/
                        it2=nhztrap+it1;                  /*left middle tetra*/
                        it3=nhztrap*2+it1;                /*left bottom tetra*/
                        it4=nhztrap*3+it1;                /*right top tetra*/
                        it5=nhztrap*4+it1;                /*right middle tetra*/
                        it6=nhztrap*5+it1;                /*right bottom tetra*/

                        i000=ihz*nxhz*nyhz+iyhz*nxhz+ixhz;/*upper 4 control point*/
                        i010=i000+nxhz;
                        i100=i000+1;
                        i110=i000+nxhz+1;

                        i001=nhzpoint+i000;    /*lower 4 control point*/
                        i011=nhzpoint+i010;
                        i101=nhzpoint+i100;
                        i111=nhzpoint+i110;

                        if (iyhz%2==0) {

			      /*centered normal for if11: upper facet facing y*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz0][iyhz][ixhz+1],
				    xhz[ihz][iyhz][ixhz+1],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz0][iyhz][ixhz+1],
       			            yhz[ihz][iyhz][ixhz+1],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz+1][iyhz][ixhz+1],
       			      	    zhz[ihz][iyhz][ixhz+1],
				    &facet[if11].cn[0],
				    &facet[if11].cn[1],
				    &facet[if11].cn[2]);

			      /*centered normal for if12: lower facet facing y*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz0][iyhz][ixhz+1],
				    xhz[ihz0][iyhz][ixhz],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz0][iyhz][ixhz+1],
       			            yhz[ihz0][iyhz][ixhz],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz+1][iyhz][ixhz+1],
       			      	    zhz[ihz+1][iyhz][ixhz],
				    &facet[if12].cn[0],
				    &facet[if12].cn[1],
				    &facet[if12].cn[2]);

                              /*upper facet facing y*/
		              facet[if11].ip[0]=i000;
		      	      facet[if11].ip[1]=i100;
			      facet[if11].ip[2]=i101;

                              /*lower facet facing y*/
			      facet[if12].ip[0]=i000;
			      facet[if12].ip[1]=i001;
			      facet[if12].ip[2]=i101;

                              if (iyhz==0) {
			            facet[if11].itetra[0]=-1;/*upper facet facing y*/
			            facet[if12].itetra[0]=-1;/*lower facet facing y*/
                              } else {
                                    facet[if11].itetra[0]=it1-(nxhz-1);
			            facet[if12].itetra[0]=it2-(nxhz-1);
                              }

                              if (iyhz==nyhz-1) {
                                    facet[if11].itetra[1]=-1;
                                    facet[if12].itetra[1]=-1;
                              } else {
                                    facet[if11].itetra[1]=it1;/*upper facet facing y*/
                                    facet[if12].itetra[1]=it2;/*lower facet facing y*/
                              }

                        } else {
            
			      /*centered normal for if11: upper facet facing y*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz],
                                    xhz[ihz0][iyhz][ixhz],
				    xhz[ihz][iyhz][ixhz+1],
			            yhz[ihz][iyhz][ixhz],
                                    yhz[ihz0][iyhz][ixhz],
       			            yhz[ihz][iyhz][ixhz+1],
			            zhz[ihz][iyhz][ixhz],
                                    zhz[ihz+1][iyhz][ixhz],
       			      	    zhz[ihz][iyhz][ixhz+1],
				    &facet[if11].cn[0],
				    &facet[if11].cn[1],
				    &facet[if11].cn[2]);

			      /*centered normal for if12: lower facet facing y*/
		 	      tm_normal( 
                                    xhz[ihz][iyhz][ixhz+1],
                                    xhz[ihz0][iyhz][ixhz+1],
				    xhz[ihz0][iyhz][ixhz],
			            yhz[ihz][iyhz][ixhz+1],
                                    yhz[ihz0][iyhz][ixhz+1],
       			            yhz[ihz0][iyhz][ixhz],
			            zhz[ihz][iyhz][ixhz+1],
                                    zhz[ihz+1][iyhz][ixhz+1],
       			      	    zhz[ihz+1][iyhz][ixhz],
				    &facet[if12].cn[0],
				    &facet[if12].cn[1],
				    &facet[if12].cn[2]);

                              /*upper facet facing y*/
		              facet[if11].ip[0]=i000;
		      	      facet[if11].ip[1]=i100;
			      facet[if11].ip[2]=i001;

                              /*lower facet facing y*/
			      facet[if12].ip[0]=i100;
			      facet[if12].ip[1]=i001;
			      facet[if12].ip[2]=i101;    

                              facet[if11].itetra[0]=it5-(nxhz-1);
			      facet[if12].itetra[0]=it6-(nxhz-1);
                              if (iyhz==nyhz-1) {
				    facet[if11].itetra[1]=-1;
				    facet[if12].itetra[1]=-1;
                              } else {
                                    facet[if11].itetra[1]=it5;/*upper facet facing y*/
                                    facet[if12].itetra[1]=it6;/*lower facet facing y*/
                              }
                        }
                  }
            }
      }

      /*For the bottom facet*/
      for (iyhz=0;iyhz<nyhz-1;iyhz++) {
            for (ixhz=0;ixhz<nxhz-1;ixhz++) {
                  if1=nhz*ntrap+iyhz*(nxhz-1)+ixhz; /*left facet nhz_hz*/
                  if2=nhztrap1+if1;                 /*right facet on nhz_hz*/

                  i000=nhzpoint+(nhz-1)*nxhz*nyhz+iyhz*nxhz+ixhz;
                  i010=i000+nxhz;
                  i100=i000+1;
                  i110=i000+nxhz+1;

                  if (iyhz%2==0) {
		      	/*control point of right facet on hz*/
		       	facet[if1].ip[0]=i000;
		       	facet[if1].ip[1]=i010;
		       	facet[if1].ip[2]=i100;

                        /*upper diagonal facet*/
		       	facet[if2].ip[0]=i010;
		      	facet[if2].ip[1]=i100;
		       	facet[if2].ip[2]=i110;
                  } else {
		       	/*control point for left facet on hz*/
		       	facet[if1].ip[0]=i000;
		        facet[if1].ip[1]=i010;
			facet[if1].ip[2]=i110;

		       	/*control point of right facet on hz*/
		       	facet[if2].ip[0]=i000;
		       	facet[if2].ip[1]=i100;
		       	facet[if2].ip[2]=i110;
                  }

                  /*normals to the bottom*/
                  facet[if1].cn[0]=0.0;
	          facet[if1].cn[1]=0.0;
	          facet[if1].cn[2]=-1.0;

	          /*3 normals for hz (c1): right hz facet*/
	          facet[if2].cn[0]=0.0;
	          facet[if2].cn[1]=0.0;
	          facet[if2].cn[2]=-1.0;

                  it3=nhztrap*2+(nhz-1)*ntrap+iyhz*(nxhz-1)+ixhz; /*left top tetra*/
                  it6=nhztrap*3+it3;                /*right bottom tetra*/

                  facet[if1].itetra[0]=it3;         /*left facet on hz*/
                  facet[if1].itetra[1]=-2;          /*left facet on hz*/
                  facet[if2].itetra[0]=it6;         /*right facet on hz*/
                  facet[if2].itetra[1]=-2;          /*right facet on hz*/
            }
      }

      for (itetra=0;itetra<ntetra;itetra++) {
	    /*the four control point*/
	    ip0=tetra[itetra].ip[0];
            ip1=tetra[itetra].ip[1];
	    ip2=tetra[itetra].ip[2];
            ip3=tetra[itetra].ip[3];
	    gradient(
                  point[ip0].x,
		  point[ip0].s,
                  point[ip1].x,
		  point[ip1].s,
                  point[ip2].x,
		  point[ip2].s,
                  point[ip3].x,
		  point[ip3].s,
                  tetra[itetra].gs);
      }

      fprintf(stdout,"%f=xmin\n",xmin);
      fprintf(stdout,"%f=xmax\n",xmax);
      fprintf(stdout,"%f=ymin\n",ymin);
      fprintf(stdout,"%f=ymax\n",ymax);
      fprintf(stdout,"%f=zmin\n",zmin);
      fprintf(stdout,"%f=zmax\n",zmax);

      write_point(point,npoint);

      /*Now check if some layers are smeared out*/
      for (ixhz=0;ixhz<nxhz-1;ixhz++) { /*if1*/
	    for (iyhz=0;iyhz<nyhz-1;iyhz+=2) {
  	           for (ihz=0;ihz<nhz;ihz++) {
		         for (iiihz=ihz+1;iiihz<=nhz;iiihz++) {
                               dis0=fabs(zhz[ihz][iyhz][ixhz]-zhz[iiihz][iyhz][ixhz]);
                               dis1=fabs(zhz[ihz][iyhz+1][ixhz]-zhz[iiihz][iyhz+1][ixhz]);
                               dis2=fabs(zhz[ihz][iyhz][ixhz+1]-zhz[iiihz][iyhz][ixhz+1]);

                               if (dis0>EPS || dis1>EPS || dis2>EPS) break;
                         }
			 jumphz=iiihz-1-ihz;
                         if (jumphz==0) continue;

                         if (dis0<EPS && dis1<EPS && dis2<EPS) {
                               /*Now, distance from zhz[ihz] to zhz[iiihz-1] is zero*/
                               if1=ihz*ntrap+iyhz*(nxhz-1)+ixhz; /*left facet*/
			       if1p=if1+(iiihz-1-ihz)*ntrap;
                               facet[if1].itetra[1]=facet[if1p].itetra[1];
			       facet[if1p].itetra[0]=facet[if1].itetra[0];
			 }
                   }
            }
      }

      for (ixhz=0;ixhz<nxhz-1;ixhz++) { /*if2*/
	    for (iyhz=0;iyhz<nyhz-1;iyhz+=2) {
  	           for (ihz=0;ihz<nhz;ihz++) {
		         for (iiihz=ihz+1;iiihz<=nhz;iiihz++) {
			       dis0=fabs(zhz[ihz][iyhz+1][ixhz+1]
					-zhz[iiihz][iyhz+1][ixhz+1]);
			       dis1=fabs(zhz[ihz][iyhz+1][ixhz]
					-zhz[iiihz][iyhz+1][ixhz]);
			       dis2=fabs(zhz[ihz][iyhz][ixhz+1]
					-zhz[iiihz][iyhz][ixhz+1]);

                               if (dis0>EPS || dis1>EPS || dis2>EPS) break;
                         }
			 jumphz=iiihz-1-ihz;
                         if (jumphz==0) continue;

                         if (dis0<EPS && dis1<EPS && dis2<EPS) {
                               /*Now, distance from zhz[ihz] to zhz[iiihz-1] is zero*/
			       if2=nhztrap1+ihz*ntrap+iyhz*(nxhz-1)+ixhz; /*right facet*/
			       if2p=if2+jumphz*ntrap;
                               facet[if2].itetra[1]=facet[if2p].itetra[1];
			       facet[if2p].itetra[0]=facet[if2].itetra[0];
			 }
                   }
            }
      }

      /*iyhz%2==1*/
      for (ixhz=0;ixhz<nxhz-1;ixhz++) { /*if1*/
	    for (iyhz=1;iyhz<nyhz-1;iyhz+=2) {
  	           for (ihz=0;ihz<nhz;ihz++) {
		         for (iiihz=ihz+1;iiihz<=nhz;iiihz++) {
			       dis0=fabs(zhz[ihz][iyhz][ixhz]-
					 zhz[iiihz][iyhz][ixhz]);
			       dis1=fabs(zhz[ihz][iyhz+1][ixhz]-
					 zhz[iiihz][iyhz+1][ixhz]);
			       dis2=fabs(zhz[ihz][iyhz+1][ixhz+1]-
				         zhz[iiihz][iyhz+1][ixhz+1]);

                               if (dis0>EPS || dis1>EPS || dis2>EPS) break;
                         }
			 jumphz=iiihz-1-ihz;
                         if (jumphz==0) continue;

			 if (dis0<EPS && dis1<EPS && dis2<EPS) {
                         /*Now, distance from zhz[ihz] to zhz[iiihz-1] is zero*/
                         if1=ihz*ntrap+iyhz*(nxhz-1)+ixhz; /*left facet*/
			 if1p=if1+(iiihz-1-ihz)*ntrap;
                         facet[if1].itetra[1]=facet[if1p].itetra[1];
			 facet[if1p].itetra[0]=facet[if1].itetra[0];
			 }
                   }
            }
      }

      for (ixhz=0;ixhz<nxhz-1;ixhz++) { /*if2*/
	    for (iyhz=1;iyhz<nyhz-1;iyhz+=2) {
  	           for (ihz=0;ihz<nhz;ihz++) {
		         for (iiihz=ihz+1;iiihz<=nhz;iiihz++) {
			       dis0=fabs(zhz[ihz][iyhz+1][ixhz+1]-
				         zhz[iiihz][iyhz+1][ixhz+1]);
			       dis1=fabs(zhz[ihz][iyhz][ixhz]-
					 zhz[iiihz][iyhz][ixhz]);
			       dis2=fabs(zhz[ihz][iyhz][ixhz+1]-
					 zhz[iiihz][iyhz][ixhz+1]);

                               if (dis0>EPS || dis1>EPS || dis2>EPS) break;
                         }
			 jumphz=iiihz-1-ihz;
                         if (jumphz==0) continue;

			 if (dis0<EPS && dis1<EPS && dis2<EPS) {
                               /*Now, distance from zhz[ihz] to zhz[iiihz-1] is zero*/
                               if2=nhztrap1+ihz*ntrap+iyhz*(nxhz-1)+ixhz; /*right facet*/
			       if2p=if2+(iiihz-1-ihz)*ntrap;
                               facet[if2].itetra[1]=facet[if2p].itetra[1];
			       facet[if2p].itetra[0]=facet[if2].itetra[0];
                         }
                   }
            }
      }

      for (ifs=0;ifs<nfacet;ifs++) 
            facet[ifs].area=area3d(
                  point[facet[ifs].ip[0]].x,   /*first point*/
                  point[facet[ifs].ip[1]].x,   /*second point*/
                  point[facet[ifs].ip[2]].x);  /*third point*/

      for (ifs=0;ifs<nfacet;ifs++) {
	    if (facet[ifs].cn[2]>0.0) {
                  facet[ifs].cn[0]=-facet[ifs].cn[0];
	          facet[ifs].cn[1]=-facet[ifs].cn[1];
	          facet[ifs].cn[2]=-facet[ifs].cn[2];
            }
            if (facet[ifs].cn[2]==0.0) {
	          if (facet[ifs].cn[0]<0.0) {
                        facet[ifs].cn[0]=-facet[ifs].cn[0];
	                facet[ifs].cn[1]=-facet[ifs].cn[1];
	                facet[ifs].cn[2]=-facet[ifs].cn[2];
	          } else if (facet[ifs].cn[0]==0.0) { 
		        if (facet[ifs].cn[1]<0.0) {
                              facet[ifs].cn[0]=-facet[ifs].cn[0];
	                      facet[ifs].cn[1]=-facet[ifs].cn[1];
	                      facet[ifs].cn[2]=-facet[ifs].cn[2];
                        }
                  } 
            }
      }

      for (ifs=0;ifs<nfacet;ifs++) {

            facet[ifs].ct[0]=0.0;
            facet[ifs].ct[1]=0.0;
            facet[ifs].ct[2]=0.0;
            facet[ifs].ct[3]=0.0;
            facet[ifs].ct[4]=0.0;
            facet[ifs].ct[5]=0.0;
            facet[ifs].ct[6]=facet[ifs].cn[0];
            facet[ifs].ct[7]=facet[ifs].cn[1];
            facet[ifs].ct[8]=facet[ifs].cn[2];
            facet[ifs].ct[9]=-point[facet[ifs].ip[0]].x[0]*facet[ifs].cn[0]
                             -point[facet[ifs].ip[0]].x[1]*facet[ifs].cn[1]
	                     -point[facet[ifs].ip[0]].x[2]*facet[ifs].cn[2];

            if (tetra[facet[ifs].itetra[0]].ireg==
                tetra[facet[ifs].itetra[1]].ireg ||
                fabs(facet[ifs].cn[2])<0.02) 
                   continue;

            a[0][0]=2.0*point[facet[ifs].ip[0]].x[0]*point[facet[ifs].ip[0]].n[2];
            a[0][1]=    point[facet[ifs].ip[0]].x[1]*point[facet[ifs].ip[0]].n[2];
            a[0][2]=    point[facet[ifs].ip[0]].x[0]*(point[facet[ifs].ip[0]].n[2]
                                                     -point[facet[ifs].ip[0]].n[0]);
            a[0][3]=   -point[facet[ifs].ip[0]].x[1]*point[facet[ifs].ip[0]].n[0];
            a[0][4]=0.0;
            a[0][5]=-2.0*point[facet[ifs].ip[0]].x[2]*point[facet[ifs].ip[0]].n[0];
            a[0][6]=point[facet[ifs].ip[0]].n[2];
            a[0][7]=0.0;
            a[0][8]=0.0;
            
            a[1][0]=0.0;
            a[1][1]=    point[facet[ifs].ip[0]].x[0]*point[facet[ifs].ip[0]].n[2];
            a[1][2]=   -point[facet[ifs].ip[0]].x[0]*point[facet[ifs].ip[0]].n[1];
            a[1][3]=    point[facet[ifs].ip[0]].x[2]*point[facet[ifs].ip[0]].n[2]
                       -point[facet[ifs].ip[0]].x[1]*point[facet[ifs].ip[0]].n[1];
            a[1][4]=2.0*point[facet[ifs].ip[0]].x[1]*point[facet[ifs].ip[0]].n[2];
            a[1][5]=-2.0*point[facet[ifs].ip[0]].x[2]*point[facet[ifs].ip[0]].n[1];
            a[1][6]=0.0;
            a[1][7]=point[facet[ifs].ip[0]].n[2];
            a[1][8]=0.0;

            a[2][0]=2.0*point[facet[ifs].ip[1]].x[0]*point[facet[ifs].ip[1]].n[2];
            a[2][1]=    point[facet[ifs].ip[1]].x[1]*point[facet[ifs].ip[1]].n[2];
            a[2][2]=    point[facet[ifs].ip[1]].x[0]
                      *(point[facet[ifs].ip[1]].n[2]-point[facet[ifs].ip[1]].n[0]);
            a[2][3]=   -point[facet[ifs].ip[1]].x[1]*point[facet[ifs].ip[1]].n[0];
            a[2][4]=0.0;
            a[2][5]=-2.0*point[facet[ifs].ip[1]].x[2]*point[facet[ifs].ip[1]].n[0];
            a[2][6]=point[facet[ifs].ip[1]].n[2];
            a[2][6]=0.0;
            a[2][8]=0.0;

            a[3][0]=0.0;
            a[3][1]=    point[facet[ifs].ip[1]].x[0]*point[facet[ifs].ip[1]].n[2];
            a[3][2]=   -point[facet[ifs].ip[1]].x[0]*point[facet[ifs].ip[1]].n[1];
            a[3][3]=    point[facet[ifs].ip[1]].x[2]*point[facet[ifs].ip[1]].n[2]
                       -point[facet[ifs].ip[1]].x[1]*point[facet[ifs].ip[1]].n[1];
            a[3][4]=2.0*point[facet[ifs].ip[1]].x[1]*point[facet[ifs].ip[1]].n[2];
            a[3][5]=-2.0*point[facet[ifs].ip[1]].x[2]*point[facet[ifs].ip[1]].n[1];
            a[3][6]=0.0;
            a[3][7]=point[facet[ifs].ip[1]].n[2];
            a[3][8]=0.0;

            a[4][0]=2.0*point[facet[ifs].ip[2]].x[0]*point[facet[ifs].ip[2]].n[2];
            a[4][1]=    point[facet[ifs].ip[2]].x[1]*point[facet[ifs].ip[2]].n[2];
            a[4][2]=    point[facet[ifs].ip[2]].x[0]
                      *(point[facet[ifs].ip[2]].n[2]-point[facet[ifs].ip[2]].n[0]);
            a[4][3]=-   point[facet[ifs].ip[2]].x[1]*point[facet[ifs].ip[2]].n[0];
            a[4][4]=0.0;
            a[4][5]=-2.0*point[facet[ifs].ip[2]].x[2]*point[facet[ifs].ip[2]].n[0];
            a[4][6]=point[facet[ifs].ip[2]].n[2];
            a[4][7]=0.0;
            a[4][8]=0.0;

            a[5][0]=0.0;
            a[5][1]=    point[facet[ifs].ip[2]].x[0]*point[facet[ifs].ip[2]].n[2];
            a[5][2]=   -point[facet[ifs].ip[2]].x[2]*point[facet[ifs].ip[2]].n[1];
            a[5][3]=    point[facet[ifs].ip[2]].x[2]*point[facet[ifs].ip[2]].n[2]
                       -point[facet[ifs].ip[2]].x[1]*point[facet[ifs].ip[2]].n[1];
            a[5][4]=2.0*point[facet[ifs].ip[2]].x[1]*point[facet[ifs].ip[2]].n[2];
            a[5][5]=-2.0*point[facet[ifs].ip[2]].x[2]*point[facet[ifs].ip[2]].n[1];
            a[5][6]=0.0;
            a[5][7]=point[facet[ifs].ip[2]].n[2];
            a[5][8]=0.0;

            a[6][0]=point[facet[ifs].ip[0]].x[0]*point[facet[ifs].ip[0]].x[0];
            a[6][1]=point[facet[ifs].ip[0]].x[0]*point[facet[ifs].ip[0]].x[1];
            a[6][2]=point[facet[ifs].ip[0]].x[0]*point[facet[ifs].ip[0]].x[2];
            a[6][3]=point[facet[ifs].ip[0]].x[1]*point[facet[ifs].ip[0]].x[2];
            a[6][4]=point[facet[ifs].ip[0]].x[1]*point[facet[ifs].ip[0]].x[1];
            a[6][5]=point[facet[ifs].ip[0]].x[2]*point[facet[ifs].ip[0]].x[2];
            a[6][6]=point[facet[ifs].ip[0]].x[0];
            a[6][7]=point[facet[ifs].ip[0]].x[1];
            a[6][8]=1.0;
   
            a[7][0]=point[facet[ifs].ip[1]].x[0]*point[facet[ifs].ip[1]].x[0];
            a[7][1]=point[facet[ifs].ip[1]].x[0]*point[facet[ifs].ip[1]].x[1];
            a[7][2]=point[facet[ifs].ip[1]].x[0]*point[facet[ifs].ip[1]].x[2];
            a[7][3]=point[facet[ifs].ip[1]].x[1]*point[facet[ifs].ip[1]].x[2];
            a[7][4]=point[facet[ifs].ip[1]].x[1]*point[facet[ifs].ip[1]].x[1];
            a[7][5]=point[facet[ifs].ip[1]].x[2]*point[facet[ifs].ip[1]].x[2];
            a[7][6]=point[facet[ifs].ip[1]].x[0];
            a[7][7]=point[facet[ifs].ip[1]].x[1];
            a[7][8]=1.0;

            a[8][0]=point[facet[ifs].ip[2]].x[0]*point[facet[ifs].ip[2]].x[0];
            a[8][1]=point[facet[ifs].ip[2]].x[0]*point[facet[ifs].ip[2]].x[1];
            a[8][2]=point[facet[ifs].ip[2]].x[0]*point[facet[ifs].ip[2]].x[2];
            a[8][3]=point[facet[ifs].ip[2]].x[1]*point[facet[ifs].ip[2]].x[2];
            a[8][4]=point[facet[ifs].ip[2]].x[1]*point[facet[ifs].ip[2]].x[1];
            a[8][5]=point[facet[ifs].ip[2]].x[2]*point[facet[ifs].ip[2]].x[2];
            a[8][6]=point[facet[ifs].ip[2]].x[0];
            a[8][7]=point[facet[ifs].ip[2]].x[1];
            a[8][8]=1.0;

            b[0]=point[facet[ifs].ip[0]].n[0];
            b[1]=point[facet[ifs].ip[0]].n[1];
            b[2]=point[facet[ifs].ip[1]].n[0];
            b[3]=point[facet[ifs].ip[1]].n[1];
            b[4]=point[facet[ifs].ip[2]].n[0];
            b[5]=point[facet[ifs].ip[2]].n[1];
            b[6]=point[facet[ifs].ip[0]].x[2];
            b[7]=point[facet[ifs].ip[1]].x[2];
            b[8]=point[facet[ifs].ip[2]].x[2];

            tetracgr(a,facet[ifs].ct,b);
      }

      write_facet(facet,nfacet);

      for (itetra=0;itetra<ntetra;itetra++) {
            tetra[itetra].v=tetra_volume(
                  point[tetra[itetra].ip[0]].x,    /*first point*/
		  point[tetra[itetra].ip[1]].x,    /*second point*/
		  point[tetra[itetra].ip[2]].x,    /*third point*/
                  point[tetra[itetra].ip[3]].x);   /*fourth point*/
            if (tetra[itetra].v<EPS) tetra[itetra].v=-9999.9;
      }

      write_tetra(tetra,ntetra);

      free3float(xhz);
      free3float(yhz);
      free3float(zhz);
      free3float(v0hz);
      free3float(v1hz);
      free3float(n1hz);
      free3float(n2hz);
      free3float(n3hz);
      free3float(dvdzhz);
 
      fclose(hzfp);
      return EXIT_SUCCESS;
}

void write_point(struct POINT *point,int npoint)
/************************************************************
write_point - write point information to a file
*************************************************************
Function prototype:
void write_point(struct POINT *point,int npoint);
*************************************************************
Input:
int npoint number of points
Ouput:
struct POINT *point control points
*************************************************************
Author: Zhaobo Meng, CWP 1996
************************************************************/
{
      int ip;    /*index for npoint*/

      for (ip=0;ip<npoint;ip++) {
	    if (point[ip].n[2]>0.0) {
                  point[ip].n[0]=-point[ip].n[0];
	          point[ip].n[1]=-point[ip].n[1];
	          point[ip].n[2]=-point[ip].n[2];
            }
            if (point[ip].n[2]==0.0) {
	          if (point[ip].n[0]<0.0) {
                        point[ip].n[0]=-point[ip].n[0];
	                point[ip].n[1]=-point[ip].n[1];
	                point[ip].n[2]=-point[ip].n[2];
	          } else if (point[ip].n[0]==0.0) { 
		        if (point[ip].n[1]<0.0) {
                              point[ip].n[0]=-point[ip].n[0];
	                      point[ip].n[1]=-point[ip].n[1];
	                      point[ip].n[2]=-point[ip].n[2];
			}
                  } 
            }
      }

      fprintf(stdout,"%d=npoint : x[3] : s : n[3]\n",npoint);
      for (ip=0;ip<npoint;ip++) {
            fprintf(stdout,"%d : %f %f %f : %f : %f %f %f\n",
                  ip,
		  point[ip].x[0],point[ip].x[1],point[ip].x[2],
                  point[ip].s,
                  point[ip].n[0],point[ip].n[1],point[ip].n[2]);
      }
      fprintf(stderr,"%d point written\n",npoint);
}

void write_facet(struct FACET *facet,int nfacet)
/***********************************************************
write_facet - write facet information to a file
************************************************************
Function prototype:
void write_facet(struct FACET *facet,int nfacet);
************************************************************
Input:
int nfacet number of facets
Ouput:
struct FACET *facet facet information
************************************************************
Author: CWP: Zhaobo Meng, Sept 1996
************************************************************/
{
      int ifs;    /*index for nfacet*/

      fprintf(stdout,"%d=nfacet : area : ct[10] : ip[3] : cn[3] : itetra[2]\n",
	    nfacet);
      for (ifs=0;ifs<nfacet;ifs++) {
            fprintf(stdout,
"%d : %f : %f %f %f %f %f %f %f %f %f %f : %d %d %d : %f %f %f : %d %d\n",
                  ifs,facet[ifs].area,
                  facet[ifs].ct[0],
		  facet[ifs].ct[1],
		  facet[ifs].ct[2],
		  facet[ifs].ct[3],
		  facet[ifs].ct[4],
		  facet[ifs].ct[5],
                  facet[ifs].ct[6],
		  facet[ifs].ct[7],
		  facet[ifs].ct[8],
                  facet[ifs].ct[9],
		  facet[ifs].ip[0],facet[ifs].ip[1],facet[ifs].ip[2],
		  facet[ifs].cn[0],facet[ifs].cn[1],facet[ifs].cn[2],
		  facet[ifs].itetra[0],facet[ifs].itetra[1]);
      }
      fprintf(stderr,"%d facet written\n",nfacet);
}

void write_tetra(struct TETRA *tetra,int ntetra)
/*****************************************************************
write_tetra - write tetra information to a file
*****************************************************************
Function prototype:
void write_tetra(struct TETRA *tetra,int ntetra);
*****************************************************************
Input:
int ntetra number of tetra
Ouput:
struct TETRA *tetra tetra information
*****************************************************************
Author: CWP: Zhaobo Meng, Sept 1996
*****************************************************************/
{
      int it;    /*index for ntetra*/

      fprintf(stdout,"%d=ntetra : v : ip[4] : ifacet[4] : ireg : gs[3]\n",ntetra);
      for (it=0;it<ntetra;it++) {
            fprintf(stdout,"%d : %f : %d %d %d %d : %d %d %d %d : %d : %f %f %f\n",
                  it,tetra[it].v,
		  tetra[it].ip[0],tetra[it].ip[1],tetra[it].ip[2],tetra[it].ip[3],
		  tetra[it].ifacet[0],tetra[it].ifacet[1],
		  tetra[it].ifacet[2],tetra[it].ifacet[3],
                  tetra[it].ireg,
		  tetra[it].gs[0],tetra[it].gs[1],tetra[it].gs[2]);
      }
      fprintf(stderr,"%d tetra written\n",ntetra);
}

void tm_normal(float x0,float x1,float x2,
float y0,float y1,float y2,
float z0,float z1,float z2,
float *n1,float *n2,float *n3)
/************************************************************
tm_normal - calculate the normal to the plane defined by the 
             triangle specified by its 3 vertex point.
************************************************************
Function prototype:
void tm_normal(float x0,float x1,float x2,
float y0,float y1,float y2,
float z0,float z1,float z2,
float *n1,float *n2,float *n3);
************************************************************
Inputs:
float x0,y0,z0 first point
float x1,y1,z1 second point
float x2,y2,z2 third point
outputs:
float *n1,*n2,*n3 the normal
****************************************************************
Notes:
3 points (x0,y0,z0), (x1,y1,z1) and (x2,y2,z2) can determine a 
plane. To determine the normal, we use two conditions: i.e. 
the normal must be perpendicular to the intersecting diagonals:

	  (x2,y2,z2)
	       /\
	      /  \
	     /    \
	    /      \
	   /________\
     (x0,y0,z0) (x1,y1,z1)

        V1=[x1-x0,y1-y0,z1-z0]
        V2=[x2-x0,y2-y0,z2-z0]

        N=V1xV2

        n1=(y1-y0)*(z2-z0)-(y2-y0)*(z1-z0)
        n2=(x2-x0)*(z1-z0)-(x1-x0)*(z2-z0)
        n3=(x1-x0)*(y2-y0)-(y1-y0)*(x2-x0)

*****************************************************************
Author: CWP: Zhaobo Meng, Sept 1996
******************************************************************/
{
      *n1=(y1-y0)*(z2-z0)-(y2-y0)*(z1-z0);
      *n2=(x2-x0)*(z1-z0)-(x1-x0)*(z2-z0);
      *n3=(x1-x0)*(y2-y0)-(y1-y0)*(x2-x0);

      tm_normalize(n1,n2,n3);
}

void tm_normalize(float *n1,float *n2,float *n3)
/******************************************************************
tm_normalize - normalize the vector
******************************************************************
Function prototype 
void tm_normalize(float *n1,float *n2,float *n3);
******************************************************************
Inputs/outputs:
float *n1,*n2,*n3 normalize this vector
tm_normalize - normalize the vector whose components are (n1,n2,n3)
                to 1.
*******************************************************************
Author: CWP: Zhaobo Meng, Sept 1996
******************************************************************/
{
      float norm;
      norm=*n1**n1+*n2**n2+*n3**n3;
      norm=sqrt(norm);
      if (norm!=0.0) {	
            *n1=*n1/norm;
            *n2=*n2/norm;
            *n3=*n3/norm;
      } else {
            *n1=0.0;
            *n2=0.0;
            *n3=-1.0;
      }
}

void gradient(float x0[3],float s0,float x1[3],float s1,
float x2[3],float s2,float x3[3],float s3,float gs[3])
/************************************************************
gradient - calculate the gradient of s using s at 4 points
************************************************************
Function prototype: 
void gradient(float x0[3],float s0,float x1[3],float s1,
float x2[3],float s2,float x3[3],float s3,float gs[3]);
************************************************************
Inputs:
float x0[3],x1[3],x2[3],x3[3] 4 vertices
float s0,s1,s2,s3 sloth at the 4 vertices
Outputs:
float gs[3] the gradient of sloth
************************************************************
Author: CWP: Zhaobo Meng, Sept 1996
************************************************************/
{
      float a[3][3];

      a[0][0]=x1[0]-x0[0];
      a[0][1]=x1[1]-x0[1];
      a[0][2]=x1[2]-x0[2];

      a[1][0]=x2[0]-x0[0];
      a[1][1]=x2[1]-x0[1];
      a[1][2]=x2[2]-x0[2];

      a[2][0]=x3[0]-x0[0];
      a[2][1]=x3[1]-x0[1];
      a[2][2]=x3[2]-x0[2];

      gs[0]=s1-s0;
      gs[1]=s2-s0;
      gs[2]=s3-s0;

      gaussj(a,gs);
}

#define N 3
/*Be careful: the fastest in a is row*/

int gaussj(float a[N][N],float b[N])
/********************************************************************
gaussj - Gaussian Jordan solver for linear equation
********************************************************************
Function Prototype:
int gaussj(float a[N][N],float b[N]);
********************************************************************
Inputs:
float a[3][3] 
Input/Output:
float b[3] the soluation of a x = b
*******************************************************************
Author: CWP: Zhaobo Meng, Sept 1996
******************************************************************/
{
      int i,j,k,l,ll;
      float big,dum,pivinv;
      int icol=0;
      int irow=0;

      int indxc[N];
      int indxr[N];
      int ipiv[N];

      for (j=0;j<N;j++) ipiv[j]=0;
      for (i=0;i<N;i++) {
            big=0.0;
            for (j=0;j<N;j++)
                  if (ipiv[j] != 1)
                        for (k=0;k<N;k++) {
                              if (ipiv[k] == 0) {
                                    if (fabs(a[j][k]) >= big) {
                                          big=fabs(a[j][k]);
                                          irow=j;
                                          icol=k;
                                    }
                              } else if (ipiv[k] > 1) {
				    /*fprintf(stderr,"Singular Matrixhz-1");*/
                                    for (l=0;l<N;l++) b[l]=0;
                                    return -1;
                              }
                        }
            ++(ipiv[icol]);
            if (irow != icol) {
                  for (l=0;l<N;l++) SWAP(a[irow][l],a[icol][l])
                  SWAP(b[irow],b[icol])
            }
            indxr[i]=irow;
            indxc[i]=icol;
            if (a[icol][icol] == 0.0) {
	           /*fprintf(stderr,"Singular Matrix-2");*/
                   for (l=0;l<N;l++) b[l]=0;
                   return -2;
            }

            pivinv=1.0/a[icol][icol];
            a[icol][icol]=1.0;
            for (l=0;l<N;l++) a[icol][l] *= pivinv;
            b[icol] *= pivinv;
            for (ll=0;ll<N;ll++)
                  if (ll != icol) {
                        dum=a[ll][icol];
                        a[ll][icol]=0.0;
                        for (l=0;l<N;l++) a[ll][l] -= a[icol][l]*dum;
                        b[ll] -= b[icol]*dum;
                  }
      }
      for (l=N-1;l>=0;l--) {
            if (indxr[l] != indxc[l])
                  for (k=0;k<N;k++)
                        SWAP(a[k][indxr[l]],a[k][indxc[l]]);
      }
      return 1;
}
#undef SWAP

void tetracgr(float a[np][np],float ct[np+1],float rhs[np])
/******************************************************************
tetracgr - conjugate gradient solver for linear equation
******************************************************************
Function Prototype:
void tetracgr(float a[np][np],float ct[np+1],float rhs[np]);
******************************************************************
Inputs:
float a[np][np]
float rhs[np] 
Input/Output:
float ct[np+1] the soluation of a x = rhs
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1996
******************************************************************/
{
      float x[np],y[np],s[np],p[np],r[np],q[np];
      int ix,iy,iter;
      float normold,normnew;
      float work,beta,alpha,error;

      for (ix=0;ix<6;ix++) x[ix]=0.0;
      x[6]=ct[6];
      x[7]=ct[7];
      x[8]=ct[9]; 

      ax(a,x,y);

      for (iy=0;iy<np;iy++)
            s[iy]=rhs[iy]-y[iy];

      ax(a,s,p);

      for (iy=0;iy<np;iy++)
            r[iy]=p[iy];

      normold=norm2(r,np);

      iter=0;
      while (normold>EPS) {
            iter++;

            /* if there are too many iterations, fail */
            if (iter==maxiterations) {
                  for (ix=0;ix<6;ix++)
		        x[ix]=0.0;
                  break;
            }

            ax(a,p,q);
            alpha=normold/norm2(q,np);

            for (ix=0;ix<np;ix++)
                  x[ix]+=alpha*p[ix];

            ax(a,x,y);

            error=0;
            for (ix=0;ix<np;ix++) {
                  work=y[ix]-rhs[ix];
                  error+=work*work;
            }  

            #ifdef DEBUG
            for (ix=0;ix<np;ix++)
                  fprintf(stderr,"%f ",x[ix]); 
            fprintf(stderr,"\nerror=%e at step %d\n",error,iter);
            #endif

            if (error<EPS) break;

            for (ix=0;ix<np;ix++)
                  s[ix]+=-alpha*q[ix];
            ax(a,s,r);
            normnew=norm2(r,np);
            beta=normnew/normold;
            for (ix=0;ix<np;ix++)
                  p[ix]=r[ix]+beta*p[ix];
            normold=normnew;
      }
      normnew=sqrt(x[6]*x[6]+x[7]*x[7]+1.0);
      for (ix=0;ix<8;ix++)
            ct[ix]=x[ix]/normnew;
      ct[8]=-1.0/normnew;
      ct[9]=x[8]/normnew;
}

void ax(float a[np][np],float x[np],float y[np])
/*****************************************************************
ax - production of matrix a and vector x
******************************************************************
Funtion Prototype:
void ax(float a[np][np],float x[np],float y[np]);
******************************************************************
Inputs:
float a[np][np]
float x[np] 
Output:
float y[np] y=ax
******************************************************************
Author: CWP: Zhaobo Meng, Sept 1996
******************************************************************/
{
      int ix,iy;
      for (iy=0;iy<np;iy++) {
           y[iy]=0;
           for (ix=0;ix<np;ix++) 
                 y[iy]+=a[iy][ix]*x[ix];
      }
}

float norm2(float xy[],int n)
/***********************************************************************
norm2 - Calculate the norm of a vector. Needs the length of vector 
***********************************************************************
Function Prototype:
float norm2(float xy[],int n);
***********************************************************************
Input:
float xy[] array
int n size of the array

Returns: the norm of the vector
***********************************************************************
Author: CWP: Zhaobo Meng, Sept 1996
**********************************************************************/
{
      int i;
      float sum=0;
      for (i=0;i<n;i++) sum+=xy[i]*xy[i];
      return(sum);
}
