/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSCUBECONTOUR: $Revision: 1.4 $ ; $Date: 2011/11/17 00:10:53 $	*/


#include "par.h"
#include "psplot.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" PSCCONTOUR - PostScript Contour plot of a data CUBE		        ",
" 									",
" pscubecontour n1= n2= n3= [optional parameters] <binaryfile >postscriptfile	",
"    or									",
" pscubecontour n1= n2= n3= front= side= top= [optional parameters] >postscriptfile",
" 									",
" Data formats supported:						",
"	1. Entire cube read from stdin (n1*n2*n3 floats) [default format]",
"	2. Faces read from stdin (n1*n2 floats for front, followed by n1*n3",
"	   floats for side, and n2*n3 floats for top) [specify faces=1]	",
"	3. Faces read from separate data files [specify filenames]	",
" 									",
" Required Parameters:							",
" n1                     number of samples in 1st (fastest) dimension	",
" n2                     number of samples in 2nd dimension		",
" n3                     number of samples in 3rd (slowest) dimension	",
" 									",
" Optional Parameters:							",
" front                  name of file containing front panel		",
" side                   name of file containing side panel		",
" top                    name of file containing top panel		",
" faces=0                =1 to read faces from stdin (data format 2)	",
" d1=1.0                 sampling interval in 1st dimension		",
" f1=0.0                 first sample in 1st dimension			",
" d2=1.0                 sampling interval in 2nd dimension		",
" f2=0.0                 first sample in 2nd dimension			",
" d3=1.0                 sampling interval in 3rd dimension		",
" f3=0.0                 first sample in 3rd dimension			",
" d1s=1.0                factor by which to scale d1 before imaging	",
" d2s=1.0                factor by which to scale d2 before imaging	",
" d3s=1.0                factor by which to scale d3 before imaging	",
" nc=5                   number of contour values			",
" dc=(zmax-zmin)/nc      contour interval				",
" fc=min+dc              first contour					",
" c=fc,fc+dc,...         array of contour values			",
" cwidth=1.0,...         array of contour line widths			",
" cgray=0.0,...          array of contour grays (0.0=black to 1.0=white)",
" ccolor=none,...        array of contour colors; none means use cgray	",
" cdash=0.0,...          array of dash spacings (0.0 for solid)		",
" labelcf=1              first labeled contour (1,2,3,...)		",
" labelcper=1            label every labelcper-th contour		",
" nlabelc=nc             number of labeled contours (0 no contour label)",
" nplaces=6              number of decimal places in contour label      ",
" xbox=1.5               offset in inches of left side of axes box	",
" ybox=1.5               offset in inches of bottom side of axes box	",
" size1=4.0              size in inches of 1st axes (vertical)		",
" size2=4.0              size in inches of 2nd axes (horizontal)	",
" size3=3.0              size in inches of 3rd axes (projected)		",
" angle=45               projection angle of cube in degrees (0<angle<90)",
"                        (angle between 2nd axis and projected 3rd axis)",
" x1end=x1max            value at which axis 1 ends			",
" d1num=0.0              numbered tic interval on axis 1 (0.0 for automatic)",
" f1num=x1min            first numbered tic on axis 1 (used if d1num not 0.0)",
" n1tic=1                number of tics per numbered tic on axis 1	",
" grid1=none             grid lines on axis 1 - none, dot, dash, or solid",
" label1=                label on axis 1				",
" x2beg=x2min            value at which axis 2 begins			",
" d2num=0.0              numbered tic interval on axis 2 (0.0 for automatic)",
" f2num=x2min            first numbered tic on axis 2 (used if d2num not 0.0)",
" n2tic=1                number of tics per numbered tic on axis 2	",
" grid2=none             grid lines on axis 2 - none, dot, dash, or solid",
" label2=                label on axis 2				",
" x3end=x3max            value at which axis 3 ends			",
" d3num=0.0              numbered tic interval on axis 3 (0.0 for automatic)",
" f3num=x3min            first numbered tic on axis 3 (used if d3num not 0.0)",
" n3tic=1                number of tics per numbered tic on axis 3	",
" grid3=none             grid lines on axis 3 - none, dot, dash, or solid",
" label3=                label on axis 3				",
" labelfont=Helvetica    font name for axes labels			",
" labelsize=18           font size for axes labels			",
" title=                 title of plot					",
" titlefont=Helvetica-Bold font name for title				",
" titlesize=24           font size for title				",
" titlecolor=black       color of title					",
" labelcfont=Helvetica-Bold font name for contour labels		",
" labelcsize=6           font size of contour labels   			",
" labelccolor=black      color of contour labels   			",
" axescolor=black        color of axes					",
" gridcolor=black        color of grid					",
" 									",
" All color specifications may also be made in X Window style Hex format",
" example:   axescolor=#255						",
"									",
" Note: The values of x1beg=x1min, x2end=x2max and x3beg=x3min cannot   ",
" be changed.								",
"									",
" Legal font names are:							",
" AvantGarde-Book AvantGarde-BookOblique AvantGarde-Demi AvantGarde-DemiOblique"
" Bookman-Demi Bookman-DemiItalic Bookman-Light Bookman-LightItalic ",
" Courier Courier-Bold Courier-BoldOblique Courier-Oblique ",
" Helvetica Helvetica-Bold Helvetica-BoldOblique Helvetica-Oblique ",
" Helvetica-Narrow Helvetica-Narrow-Bold Helvetica-Narrow-BoldOblique ",
" Helvetica-Narrow-Oblique NewCentrySchlbk-Bold"
" NewCenturySchlbk-BoldItalic NewCenturySchlbk-Roman Palatino-Bold  ",
" Palatino-BoldItalic Palatino-Italics Palatino-Roman ",
" SanSerif-Bold SanSerif-BoldItalic SanSerif-Roman ",
" Symbol Times-Bold Times-BoldItalic ",
" Times-Roman Times-Italic ZapfChancery-MediumItalic ",
" 									",
NULL};

/*
 * (Original codes pscontour and pscube)
 *
 * AUTHOR:  Craig Artley, Colorado School of Mines, 03/12/93
 * NOTE:  Original written by Zhiming Li & Dave Hale, CSM, 07/01/90
 *	  Completely rewritten, the code now bears more similarity to
 *	  psimage than the previous pscube.  Faces of cube now rendered
 *	  as three separate images, rather than as a single image.  The
 *	  output no longer suffers from stretching artifacts, and the
 *	  code is simpler.  -Craig
 * MODIFIED:  Craig Artley, Colorado School of Mines, 12/17/93
 * 	  Added color options.
 *
 * PSCCONTOUR: mashed together from pscube and pscontour 
 * to generate 3d contour plots by Claudia Vanelle, Institute of Geophysics,
 * University of Hamburg, Germany somewhen in 2000
 *
 * PSCUBE was "merged" with PSCONTOUR to create PSCUBECONTOUR 
 * by Claudia Vanelle, Applied Geophysics Group Hamburg
 * somewhen in 2000
 */
/**************** end self doc ********************************/

/* maximum number of contours */
#define NCMAX 200


int main (int argc, char **argv)
{
  int n1,n2,n3,i1,i2,i3,
    n1tic,n2tic,n3tic,grid1,grid2,grid3,nz,iz,
    faces,style=SEISMIC,bbox[4],
    npar,nc,nplaces,ic;
  int  labelcf, nlabelc, labelcper;
  float d1,d2,d3,f1,f2,f3,size1,size2,size3,xbox,ybox,angle,
    x1min,x1max,x2min,x2max,x3min,x3max,
    x1beg,x1end,x2beg,x2end,x3beg,x3end,
    d1num,f1num,d2num,f2num,d3num,f3num,
    p1beg,p1end,p2beg,p2end,p3beg,p3end,
    labelsize,titlesize,
    *z,*zfront,*zside,*ztop,*temp,matrix[6],
    cwidth[NCMAX],cgray[NCMAX],cdash[NCMAX],
    dc,fc,*x1,*x2,*x3,c[NCMAX],zmin,zmax,
    x1scale,x2scale,x3scale;
  float labelcsize,lcsize,*w,*wfront,*wside,*wtop;
  char *label1="",*label2="",*label3="",*title="",
    *labelfont="Helvetica",*titlefont="Helvetica-Bold",
    *labelcfont="Helvetica-Bold",*labelccolor="black",
    *grid1s="none",*grid2s="none",*grid3s="none",
    *titlecolor="black",*axescolor="black",*gridcolor="black",
    *frontf,*sidef,*topf,
    *scolor="none",*ccolor[NCMAX];
  FILE *infp=stdin,*frontfp,*sidefp,*topfp;
  
  /* initialize getpar */
  initargs(argc,argv);
  requestdoc(1);
  
  /* get parameters describing 1st dimension sampling */
  if (!getparint("n1",&n1)) err("must specify n1!\n");
  if (!getparfloat("d1",&d1)) d1 = 1.0;
  if (!getparfloat("f1",&f1)) f1 = 0.0;
  x1 = ealloc1float(n1);
  for (i1=0; i1<n1; i1++)
    x1[i1] = f1+i1*d1;
  for (i1=1,x1min=x1max=x1[0]; i1<n1; i1++) {
    x1min = MIN(x1min,x1[i1]);
    x1max = MAX(x1max,x1[i1]);
  }

  /* get parameters describing 2nd dimension sampling */
  if (!getparint("n2",&n2)) err("must specify n2!\n");
  if (!getparfloat("d2",&d2)) d2 = 1.0;
  if (!getparfloat("f2",&f2)) f2 = 0.0;
  x2 = ealloc1float(n2);
  for (i2=0; i2<n2; i2++)
    x2[i2] = f2+i2*d2;
  for (i2=1,x2min=x2max=x2[0]; i2<n2; i2++) {
    x2min = MIN(x2min,x2[i2]);
    x2max = MAX(x2max,x2[i2]);
  }

  /* get parameters describing 3rd dimension sampling */
  if (!getparint("n3",&n3)) err("must specify n3!\n");
	if (n3<2)err("must have n3>=2!");
  if (!getparfloat("d3",&d3)) d3 = 1.0;
  if (!getparfloat("f3",&f3)) f3 = 0.0;
  x3 = ealloc1float(n3);
  for (i3=0; i3<n3; i3++)
    x3[i3] = f3+i3*d3;
  for (i3=1,x3min=x3max=x3[0]; i3<n3; i3++) {
    x3min = MIN(x3min,x3[i3]);
    x3max = MAX(x3max,x3[i3]);
  }

  /* determine input type */
  if (!getparint("faces",&faces)) faces = 0;
  
  /* allocate space */
  nz = n1*n2+n1*n3+n2*n3;
  z = ealloc1float(nz);
  zfront = z;
  zside = zfront+n1*n2;
  ztop = zside+n1*n3;
  
  /* read data */
  if (getparstring("front",&frontf)
      && getparstring("side",&sidef)
      && getparstring("top",&topf)) {
    
    /* read face files */
    if ((frontfp = fopen(frontf,"r")) == NULL)
      err("error opening front file!\n");
    if (fread(zfront,sizeof(float),n1*n2,frontfp)!=n1*n2)
      err("error reading front file!\n");
    if ((sidefp = fopen(sidef,"r")) == NULL)
      err("error opening side file!\n");
    if (fread(zside,sizeof(float),n1*n3,sidefp)!=n1*n3)
      err("error reading side file!\n");
    if ((topfp = fopen(topf,"r")) == NULL)
      err("error opening top file!\n");
    if (fread(ztop,sizeof(float),n2*n3,topfp)!=n2*n3)
      err("error reading top file!\n");
    
  } else if (getparstring("front",&frontf)
	     || getparstring("side",&sidef)
	     || getparstring("top",&topf)) {
    
    err("must specify all or none of face, side, and top!\n");
    
  } else if (faces) {
    /* read faces from stdin */
    if (fread(zfront,sizeof(float),n1*n2,infp)!=n1*n2)
      err("error reading front from input!\n");
    if (fread(zside,sizeof(float),n1*n3, infp)!=n1*n3)
      err("error reading side from input!\n");
    if (fread(ztop,sizeof(float),n2*n3, infp)!=n2*n3)
      err("error reading top from input!\n");
  } else {
    /* read cube from stdin, pick off faces */
    temp = ealloc1float(n1);
    for (i3=0; i3<n3; i3++) {
      for (i2=0; i2<n2; i2++) {
	if (fread(temp,sizeof(float),n1,infp)!=n1)
	  err("error reading cube from input!\n");
	if (i3==0) 
	  for (i1=0; i1<n1; i1++)
	    zfront[i1+i2*n1] = temp[i1];
	if (i2==n2-1)
	  for (i1=0; i1<n1; i1++)
	    zside[i1+i3*n1] = temp[i1];
	ztop[i2+i3*n2] = temp[0];
      }
    }
    free1float(temp);
  }
  
  /* zero w array for contour labeling	*/	
  w = ealloc1float(nz);
  wfront = w;
  wside = wfront+n1*n2;
  wtop = wside+n1*n3;
  for(iz=0; iz<nz; iz++)
    w[iz] = 0.;
	
  /* determine data min and max */
  for (iz=0,zmin=zmax=z[0]; iz<nz; iz++){
    zmin = MIN(zmin,z[iz]);
    zmax = MAX(zmax,z[iz]);
  }
  
  /* get contouring parameters */
  if ((nc=getparfloat("c",c))==0) {
    nc = 5;  getparint("nc",&nc);
    dc = (zmax-zmin)/nc;  getparfloat("dc",&dc);
    fc = zmin+dc;  getparfloat("fc",&fc);
    for (ic=0; ic<nc; ic++)
      c[ic] = fc+ic*dc;
  }
  for (ic=0; ic<nc; ic++) {
    cwidth[ic] = 1.0;
    cgray[ic] = 0.0;
    cdash[ic] = 0.0;
    ccolor[ic] = scolor;
  }
  if ((npar=getparfloat("cwidth",cwidth))!=0)
    for (ic=npar; ic<nc; ic++)
      cwidth[ic] = cwidth[npar-1];
  if ((npar=getparfloat("cgray",cgray))!=0)
    for (ic=npar; ic<nc; ic++)
      cgray[ic] = cgray[npar-1];
  if ((npar=getparfloat("cdash",cdash))!=0)
    for (ic=npar; ic<nc; ic++)
      cdash[ic] = cdash[npar-1];
  if (getparstring("ccolor",&scolor)) {
    int i,j;  char *s;
    for (i=0,s=strtok(scolor,","); s!=NULL; ++i,s=strtok(NULL,","))
      ccolor[i] = s;
    for (j=i-1; i<nc; ++i)
      ccolor[i] = ccolor[j];
  }
  labelcf = 1; getparint("labelcf",&labelcf);
  labelcper = 1; getparint("labelcper",&labelcper);
  nlabelc = nc; getparint("nlabelc",&nlabelc);
  labelcsize = 6; getparfloat("labelcsize",&labelcsize);
  getparstring("labelcfont",&labelcfont);
  getparstring("labelccolor",&labelccolor);
  if (!getparint("nplaces",&nplaces))		nplaces = 6;
  
  /* get axes parameters, convert to points */
  if(!getparfloat("size1",&size1)) size1 = 4.0;
  if(!getparfloat("size2",&size2)) size2 = 4.0;
  if(!getparfloat("size3",&size3)) size3 = 3.0;
  if (!getparfloat("xbox",&xbox)) xbox = 1.5;
  if (!getparfloat("ybox",&ybox)) ybox = 1.5;
  size1 *= 72;
  size2 *= 72;
  size3 *= 72;
  xbox *= 72;
  ybox *= 72;
  
  /* get projection angle, convert to radians */
  if(!getparfloat("angle",&angle)) angle = 45.0;
  angle = MAX(angle,0.00001);
  angle = MIN(angle,90.0);
  angle *= PI/180.0;
  
  /* get axis1 parameters */
  x1beg = x1min; 
  x1end = x1max; getparfloat("x1end",&x1end);
  d1num = 0.0; getparfloat("d1num",&d1num);
  f1num = x1min; getparfloat("f1num",&f1num);
  n1tic = 1; getparint("n1tic",&n1tic);
  getparstring("grid1",&grid1s);
  if (STREQ("dot",grid1s)) grid1 = DOT;
  else if (STREQ("dash",grid1s)) grid1 = DASH;
  else if (STREQ("solid",grid1s)) grid1 = SOLID;
  else grid1 = NONE;
  getparstring("label1",&label1);
  
  /* get axis2 parameters */
  x2beg = x2min; getparfloat("x2beg",&x2beg);
  x2end = x2max;
  d2num = 0.0; getparfloat("d2num",&d2num);
  f2num = x2min; getparfloat("f2num",&f2num);
  n2tic = 1; getparint("n2tic",&n2tic);
  getparstring("grid2",&grid2s);
  if (STREQ("dot",grid2s)) grid2 = DOT;
  else if (STREQ("dash",grid2s)) grid2 = DASH;
  else if (STREQ("solid",grid2s)) grid2 = SOLID;
  else grid2 = NONE;
  getparstring("label2",&label2);

  /* get axis3 parameters */
  x3beg = x3min;
  x3end = x3max; getparfloat("x3end",&x3end);
  d3num = 0.0; getparfloat("d3num",&d3num);
  f3num = x3min; getparfloat("f3num",&f3num);
  n3tic = 1; getparint("n3tic",&n3tic);
  getparstring("grid3",&grid3s);
  if (STREQ("dot",grid3s)) grid3 = DOT;
  else if (STREQ("dash",grid3s)) grid3 = DASH;
  else if (STREQ("solid",grid3s)) grid3 = SOLID;
  else grid3 = NONE;
  getparstring("label3",&label3);
  
  /* get additional font parameters */
  getparstring("labelfont",&labelfont);
  labelsize = 18.0; getparfloat("labelsize",&labelsize);
  getparstring("title",&title);
  getparstring("titlefont",&titlefont);
  titlesize = 24.0; getparfloat("titlesize",&titlesize);
  getparstring("titlecolor",&titlecolor);
  getparstring("axescolor",&axescolor);
  getparstring("gridcolor",&gridcolor);
  style = SEISMIC;
  checkpars(); 
   
  /* determine axes pads */
  p1beg = (x1end>x1beg)?-fabs(d1)/2:fabs(d1)/2;
  p1end = (x1end>x1beg)?fabs(d1)/2:-fabs(d1)/2;
  p2beg = (x2end>x2beg)?-fabs(d2)/2:fabs(d2)/2;
  p2end = (x2end>x2beg)?fabs(d2)/2:-fabs(d2)/2;
  p3beg = (x3end>x3beg)?-fabs(d3)/2:fabs(d3)/2;
  p3end = (x3end>x3beg)?fabs(d3)/2:-fabs(d3)/2; 
  
  /* set bounding box */
  psAxesBBox(xbox,ybox,size2+cos(angle)*size3,size1+sin(angle)*size3,
	     labelfont,labelsize,titlefont,titlesize,style,bbox);
  boundingbox(bbox[0],bbox[1],bbox[2],bbox[3]);
  
  /* begin PostScript */
  begineps();
  
  /* save graphics state */
  gsave();
  
  /* translate coordinate system by box offset */
  translate(xbox,ybox);
  
  /* begin top */
  gsave();
  
  /* determine x2 and x3 scale factors */
  x2scale = size2/(x2end-x2beg);
  x3scale = size3*sin(angle)/(x3end-x3beg);
  
  /* scale x2 and x3 coordinates */
  for (i2=0; i2<n2; i2++)
    x2[i2] *= x2scale;
  for (i3=0; i3<n3; i3++)
    x3[i3] *= x3scale;

  /* transform and skew coordinates */
  matrix[0] = 1;  matrix[1] = 0;  matrix[2] = 1/tan(angle);
  matrix[3] = 1;  matrix[4] = 0;  matrix[5] = 0;
  translate(-(f2-x2min+x2beg)*x2scale-f3*x3scale/tan(angle),size1-f3*x3scale);
  concat(matrix);
  rectclip(x2beg*x2scale,f3*x3scale,size2,size3*sin(angle));
  
  /* draw contours */
  for (ic=0; ic<nc; ic++) {
    setlinewidth(cwidth[ic]);
    if (strcmp(ccolor[ic],"none"))
      setcolor(ccolor[ic]);
    else
      setgray(cgray[ic]);
    if (cdash[ic]!=0.0)
      setdash(&cdash[ic],1,0.0);
    else
      setdash(&cdash[ic],0,0.0);
    lcsize = 0.;
    if (nlabelc>0) {
      if((ic-labelcf+1)%labelcper==0 && ic>=labelcf-1  
	 && ic<labelcf-1+labelcper*nlabelc) {
	setlinewidth(cwidth[ic]);
	lcsize = labelcsize;
      }
      else { 
	lcsize = 0.;
	setlinewidth(0.25*cwidth[ic]);
      }
    }
    /* no labels -> lcsize=0 to psContour */
    psContour(c[ic],n2,x2,n3,x3,ztop,0,labelcfont,labelccolor,wtop,nplaces);
  }
  /* unscale x2 and x3 coordinates */
  for (i2=0; i2<n2; i2++)
    x2[i2] /= x2scale;
  for (i3=0; i3<n3; i3++)
    x3[i3] /= x3scale;

  translate((f2-x2min+x2beg)*x2scale+f3*x3scale/tan(angle),-size1+f3*x3scale);
  
  /* end top */
  grestore();

 /* begin front */
  gsave();
 
  /* determine x1 and x2 scale factors */
  x1scale = size1/(x1end-x1beg);
  x2scale = size2/(x2end-x2beg);
  
  /* scale x1 and x2 coordinates */
  for (i1=0; i1<n1; i1++)
    x1[i1] *= x1scale;
  for (i2=0; i2<n2; i2++)
    x2[i2] *= x2scale;


  rotate(-90);
  translate(-size1-f1*x1scale,-(f2-x2min+x2beg)*x2scale);
  rectclip(size1+f1*x1scale,(f2-x2min+x2beg)*x2scale,-size1,size2);
  
  /* draw contours */
  for (ic=0; ic<nc; ic++) {
    setlinewidth(cwidth[ic]);
    if (strcmp(ccolor[ic],"none"))
      setcolor(ccolor[ic]);
    else
      setgray(cgray[ic]);
    if (cdash[ic]!=0.0)
      setdash(&cdash[ic],1,0.0);
    else
      setdash(&cdash[ic],0,0.0);
    lcsize = 0.;
    if (nlabelc>0) {
      if((ic-labelcf+1)%labelcper==0 && ic>=labelcf-1  
	 && ic<labelcf-1+labelcper*nlabelc) {
	setlinewidth(cwidth[ic]);
	lcsize = labelcsize;
      }
      else { 
	lcsize = 0.;
	setlinewidth(0.25*cwidth[ic]);
      }
    }
    psContour(c[ic],n1,x1,n2,x2,z,lcsize,labelcfont,labelccolor,w,nplaces);
  }
 
  /* unscale x1 and x2 coordinates */
  for (i1=0; i1<n1; i1++)
    x1[i1] /= x1scale;
  for (i2=0; i2<n2; i2++)
    x2[i2] /= x2scale;
  
  translate(size1+f1*x1scale,(f2-x2min+x2beg)*x2scale);
  rotate(90);
  
  /* end front */
  grestore();

  /* begin side */
  gsave();
  
  /* determine x1 and x3 scale factors */
  x1scale = size1/(x1end-x1beg);
  x3scale = size3*cos(angle)/(x3end-x3beg);
  
  /* scale x1 and x3 coordinates */
  for (i1=0; i1<n1; i1++)
    x1[i1] *= x1scale;
  for (i3=0; i3<n3; i3++)
    x3[i3] *= x3scale;
  
  /* transform and skew coordinates */
  matrix[0] = 1;  matrix[1] = tan(angle);  matrix[2] = 0;
  matrix[3] = 1;  matrix[4] = 0;  matrix[5] = 0;
  
  translate(size2-f3*x3scale,size1+f1*x1scale-f3*x3scale*tan(angle));
  concat(matrix);
  rectclip(f3*x3scale,-x1end*x1scale,size3*cos(angle),size1);
  rotate(-90);
  
    /* draw contours */ 
  for (ic=0; ic<nc; ic++) {
    setlinewidth(cwidth[ic]);
    if (strcmp(ccolor[ic],"none"))
      setcolor(ccolor[ic]);
    else
      setgray(cgray[ic]);
    if (cdash[ic]!=0.0)
      setdash(&cdash[ic],1,0.0);
    else
      setdash(&cdash[ic],0,0.0);
    lcsize = 0.;
    if (nlabelc>0) {
      if((ic-labelcf+1)%labelcper==0 && ic>=labelcf-1  
	 && ic<labelcf-1+labelcper*nlabelc) {
	setlinewidth(cwidth[ic]);
	lcsize = labelcsize;
      }
      else { 
	lcsize = 0.;
	setlinewidth(0.25*cwidth[ic]);
      }
    }
    /* no labels -> lcsize=0 to psContour */
    psContour(c[ic],n1,x1,n3,x3,zside,0,labelcfont,labelccolor,wside,nplaces);
  }

  /* unscale x1 and x3 coordinates */
  for (i1=0; i1<n1; i1++)
    x1[i1] /= x1scale;
  for (i3=0; i3<n3; i3++)
    x3[i3] /= x3scale;
  
  rotate(90);
  translate(-size2+f3*x3scale,-size1-f1*x1scale+f3*x3scale*tan(angle));
  /* end side */
  grestore();
  
  
  /* restore graphics state */
  grestore();
  
  psCubeAxesBox(xbox,ybox,size1,size2,size3,angle,
		x1beg,x1end,p1beg,p1end,
		d1num,f1num,n1tic,grid1,label1,
		x2beg,x2end,p2beg,p2end,
		d2num,f2num,n2tic,grid2,label2,
		x3beg,x3end,p3beg,p3end,
		d3num,f3num,n3tic,grid3,label3,
		labelfont,labelsize,
		title,titlefont,titlesize,
		titlecolor,axescolor,gridcolor);
  
  /* end PostScript */
  showpage();
  endeps();
  
  return 0;
}
