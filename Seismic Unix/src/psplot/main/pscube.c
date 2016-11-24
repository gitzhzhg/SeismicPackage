/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSCUBE: $Revision: 1.15 $ ; $Date: 2011/11/17 00:10:53 $	*/


#include "par.h"
#include "psplot.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" PSCUBE - PostScript image plot with Legend of a data CUBE       ",
" 									",
" pscube n1= n2= n3= [optional parameters] <binaryfile >postscriptfile	",
"    or									",
" pscube n1= n2= n3= front= side= top= [optional parameters] >postscriptfile",
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
" perc=100.0             percentile used to determine clip		",
" clip=(perc percentile) clip used to determine bclip and wclip		",
" bperc=perc             percentile for determining black clip value	",
" wperc=100.0-perc       percentile for determining white clip value	",
" bclip=clip             data values outside of [bclip,wclip] are clipped",
" wclip=-clip            data values outside of [bclip,wclip] are clipped",
" brgb=0.0,0.0,0.0       red, green, blue values corresponding to black	",
" wrgb=1.0,1.0,1.0       red, green, blue values corresponding to white	",
" bhls=0.0,0.0,0.0       hue, lightness, saturation corresponding to black",
" whls=0.0,1.0,0.0       hue, lightness, saturation corresponding to white",
" bps=12                 bits per sample for color plots, either 12 or 24",
" d1s=1.0                factor by which to scale d1 before imaging	",
" d2s=1.0                factor by which to scale d2 before imaging	",
" d3s=1.0                factor by which to scale d3 before imaging	",
" verbose=1              =1 for info printed on stderr (0 for no info)	",
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
" axescolor=black        color of axes					",
" gridcolor=black        color of grid					",
" legend=0               =1 display the color scale                     ",
"                        if ==1, resize xbox,ybox,width,height          ",
" lstyle=vertleft       Vertical, axis label on left side               ",
"                        =vertright (Vertical, axis label on right side)",
"                        =horibottom (Horizontal, axis label on bottom) ",
" units=                 unit label for legend                          ",
" legendfont=times_roman10    font name for title                       ",
" following are defaults for lstyle=0. They are changed for other lstyles",
" lwidth=1.2             colorscale (legend) width in inches            ",
" lheight=height/3       colorscale (legend) height in inches           ",
" lx=1.0                 colorscale (legend) x-position in inches       ",
" ly=(height-lheight)/2+xybox    colorscale (legend) y-position in pixels",
" lbeg= lmin or wclip-5*perc    value at which legend axis begins       ",
" lend= lmax or bclip+5*perc    value at which legend axis ends         ",
" ldnum=0.0      numbered tic interval on legend axis (0.0 for automatic)",
" lfnum=lmin     first numbered tic on legend axis (used if d1num not 0.0)",
" lntic=1        number of tics per numbered tic on legend axis ",
" lgrid=none     grid lines on legend axis - none, dot, dash, or solid",
" 									",
" All color specifications may also be made in X Window style Hex format",
" example:   axescolor=#255						",
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
" Note: The values of x1beg=x1min, x2end=x2max and x3beg=x3min cannot   ",
" be changed.								",
" 									",
NULL};
/**************** end self doc ********************************/

/*
 * AUTHOR:  Craig Artley, Colorado School of Mines, 03/12/93
 * NOTE:  Original written by Zhiming Li & Dave Hale, CSM, 07/01/90
 *	  Completely rewritten, the code now bears more similarity to
 *	  psimage than the previous pscube.  Faces of cube now rendered
 *	  as three separate images, rather than as a single image.  The
 *	  output no longer suffers from stretching artifacts, and the
 *	  code is simpler.  -Craig
 * MODIFIED:  Craig Artley, Colorado School of Mines, 12/17/93
 * 	  Added color options.
 * MODIFIED: made up from pscube and psimage to generate 3-D plots 
 *  with legend  by Claudia Vanelle, Institute of Geophysics,
 *  University of Hamburg, Germany somewhen in 2000
 */

/* legend stuff added from psimage by Claudia Vanelle, 
 * Applied Geophysics Group Hamburg somewhen in 2000 */

/* color specification array indices for RGB and HLS */
#define R 0
#define G 1
#define B 2
#define H 0
#define L 1
#define S 2
#define FMAX(x,y) (float) (x) > (y) ? (x) : (y) 
#define FMIN(x,y) (float) (x) > (y) ? (y) : (x) 

/* functions defined and used internally */
static void drawimage(int hls, float colors[3][2], 
	int width, int height, int bps, float matrix[], unsigned char* z);
static float rgbvalue (float n1, float n2, float hue);
static void hlsrgb (float h, float l, float s, float *r, float *g, float *b);

int main (int argc, char **argv)
{
  int n1,n2,n3,n1s,n2s,n3s,n1c,n2c,n3c,i1,i2,i3,i1c,i2c,i3c,
    i1beg,i1end,i2beg,i2end,i3beg,i3end,i1step,i2step,i3step,
    n1tic,n2tic,n3tic,grid1,grid2,grid3,nz,iz,
    verbose,faces,hls,bps,style=SEISMIC,bbox[4],
    legend,ugrid=SOLID,lstyle=VERTLEFT,lz,lbegsup=0,lendsup=0,ln=256,
    lbbox[4];
  float d1,d2,d3,d1s,d2s,d3s,f1,f2,f3,size1,size2,size3,xbox,ybox,angle,
    x1min,x1max,x2min,x2max,x3min,x3max,
    x1beg,x1end,x2beg,x2end,x3beg,x3end,
    d1num,f1num,d2num,f2num,d3num,f3num,
    p1beg,p1end,p2beg,p2end,p3beg,p3end,
    clip,bclip,wclip,perc,bperc,wperc,
    zscale,zoffset,zi,labelsize,titlesize,
    *z,*zfront,*zside,*ztop,*temp,matrix[6],colors[3][2],
    lwidth,lheight,lx,ly,lbeg,lend,
    lmin=(float)FLT_MAX,lmax=(float)-FLT_MAX,
    ldnum,lfnum,ld,lf=0,labmatrix[6];
  unsigned char *czfront,*czside,*cztop,
    *szfront,*szside,*sztop,*czp,
    *data_legend=NULL;
  char *label1="",*label2="",*label3="",*title="",
    *labelfont="Helvetica",*titlefont="Helvetica-Bold",
    *grid1s="none",*grid2s="none",*grid3s="none",
    *titlecolor="black",*axescolor="black",*gridcolor="black",
    *frontf,*sidef,*topf,
    *units="", *legendfont="times_roman10", *lstyles="vertleft",*lgrids="none";
  FILE *infp=stdin,*frontfp,*sidefp,*topfp;
  
  /* initialize getpar */
  initargs(argc,argv);
  requestdoc(1);
  
  /* get parameters describing 1st dimension sampling */
  if (!getparint("n1",&n1)) err("must specify n1!\n");
  if (!getparfloat("d1",&d1)) d1 = 1.0;
  if (!getparfloat("f1",&f1)) f1 = 0.0;
  
  /* get parameters describing 2nd dimension sampling */
  if (!getparint("n2",&n2)) err("must specify n2!\n");
  if (!getparfloat("d2",&d2)) d2 = 1.0;
  if (!getparfloat("f2",&f2)) f2 = 0.0;
  
  /* get parameters describing 3rd dimension sampling */
  if (!getparint("n3",&n3)) err("must specify n3!\n");
  if (!getparfloat("d3",&d3)) d3 = 1.0;
  if (!getparfloat("f3",&f3)) f3 = 0.0;
  
  /* determine input type */
  if (!getparint("faces",&faces)) faces = 0;
  
  /* read color parameters */
  bps = 8;
  hls = 0;
  colors[R][0] = colors[G][0] = colors[B][0] = 0.0;
  colors[R][1] = colors[G][1] = colors[B][1] = 1.0;
  if (countparval("brgb") || countparval("wrgb")) {
    float brgb[3],wrgb[3];
    brgb[R] = brgb[G] = brgb[B] = 0.0;
    wrgb[R] = wrgb[G] = wrgb[B] = 1.0;
    getparfloat("brgb",&brgb[0]);
    getparfloat("wrgb",&wrgb[0]);
    brgb[R] = MAX(0.0,MIN(1.0,brgb[R]));
    wrgb[R] = MAX(0.0,MIN(1.0,wrgb[R]));
    brgb[G] = MAX(0.0,MIN(1.0,brgb[G]));
    wrgb[G] = MAX(0.0,MIN(1.0,wrgb[G]));
    brgb[B] = MAX(0.0,MIN(1.0,brgb[B]));
    wrgb[B] = MAX(0.0,MIN(1.0,wrgb[B]));
    colors[R][0] = brgb[R];  colors[R][1] = wrgb[R];
    colors[G][0] = brgb[G];  colors[G][1] = wrgb[G];
    colors[B][0] = brgb[B];  colors[B][1] = wrgb[B];
		if (!getparint("bps",&bps)) bps = 12;
		if (bps!=12 && bps!=24)
		  err("bps must equal 12 or 24 for color plots!\n");
  } else if (countparval("bhls") || countparval("whls")) {
    float bhls[3],whls[3];
    hls = 1;
    bhls[H] = whls[H] = 0.0;
    bhls[L] = 0.0;  whls[L] = 1.0;
    bhls[S] = whls[S] = 0.0;
    getparfloat("bhls",&bhls[0]);
    getparfloat("whls",&whls[0]);
    bhls[L] = MAX(0.0,MIN(1.0,bhls[L]));
    whls[L] = MAX(0.0,MIN(1.0,whls[L]));
    bhls[S] = MAX(0.0,MIN(1.0,bhls[S]));
    whls[S] = MAX(0.0,MIN(1.0,whls[S]));
    colors[H][0] = bhls[0];  colors[H][1] = whls[0];
    colors[L][0] = bhls[1];  colors[L][1] = whls[1];
    colors[S][0] = bhls[2];  colors[S][1] = whls[2];
    if (!getparint("bps",&bps)) bps = 12;
    if (bps!=12 && bps!=24)
      err("bps must equal 12 or 24 for color plots!\n");
  }
  
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
  
  /* if necessary, determine clips from percentiles */
  if (getparfloat("clip",&clip)) {
    bclip = clip;
    wclip = -clip;
  }
  if ((!getparfloat("bclip",&bclip) || !getparfloat("wclip",&wclip)) &&
      !getparfloat("clip",&clip)) {
    perc = 100.0;  getparfloat("perc",&perc);
    temp = ealloc1float(nz);
    for (iz=0; iz<nz; iz++)
      temp[iz] = z[iz];
    if (!getparfloat("bclip",&bclip)) {
      bperc = perc;	getparfloat("bperc",&bperc);
      iz = (nz*bperc/100.0);
      if (iz<0) iz = 0;
      if (iz>nz-1) iz = nz-1;
      qkfind(iz,nz,temp);
      bclip = temp[iz];
    }
    if (!getparfloat("wclip",&wclip)) {
      wperc = 100.0-perc;  getparfloat("wperc",&wperc);
      iz = (nz*wperc/100.0);
      if (iz<0) iz = 0;
      if (iz>nz-1) iz = nz-1;
      qkfind(iz,nz,temp);
      wclip = temp[iz];
    }
    free1float(temp);
  }
  if (!getparint("verbose",&verbose)) verbose = 1;
  if (verbose) warn("bclip=%g wclip=%g",bclip,wclip);
  
  /* get scaled sampling intervals */
  if (!getparfloat("d1s",&d1s)) d1s = 1.0;
  if (!getparfloat("d2s",&d2s)) d2s = 1.0;
  if (!getparfloat("d3s",&d3s)) d3s = 1.0;
  d1s = fabs(d1s);  d1s *= d1;
  d2s = fabs(d2s);  d2s *= d2;
  d3s = fabs(d3s);  d3s *= d3;
  
  /* get projection angle, convert to radians */
  if(!getparfloat("angle",&angle)) angle = 45.0;
  angle = MAX(angle,0.00001);
  angle = MIN(angle,90.0);
  angle *= PI/180.0;
  
  /* get axes parameters */
  if(!getparfloat("size1",&size1)) size1 = 4.0;
  if(!getparfloat("size2",&size2)) size2 = 4.0;
  if(!getparfloat("size3",&size3)) size3 = 3.0;
  if (!getparfloat("xbox",&xbox)) xbox = 1.5;
  if (!getparfloat("ybox",&ybox)) ybox = 1.5;
 
  /* compute extreme values */
  x1min = (d1>0.0)?f1:f1+(n1-1)*d1;
  x1max = (d1<0.0)?f1:f1+(n1-1)*d1;
  x2min = (d2>0.0)?f2:f2+(n2-1)*d2;
  x2max = (d2<0.0)?f2:f2+(n2-1)*d2;
  x3min = (d3>0.0)?f3:f3+(n3-1)*d3;
  x3max = (d3<0.0)?f3:f3+(n3-1)*d3;
  
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
  
 
  /* adjust x1beg and x1end to fall on sampled values */
  i1beg = NINT((x1beg-f1)/d1);
  i1beg = MAX(0,MIN(n1,i1beg));
  x1beg = f1+i1beg*d1;
  i1end = NINT((x1end-f1)/d1);
  i1end = MAX(0,MIN(n1-1,i1end));
  x1end = f1+i1end*d1;
  
  /* adjust x2beg and x2end to fall on sampled values */
  i2beg = NINT((x2beg-f2)/d2);
  i2beg = MAX(0,MIN(n2-1,i2beg));
  x2beg = f2+i2beg*d2;
  i2end = NINT((x2end-f2)/d2);
  i2end = MAX(0,MIN(n2-1,i2end));
  x2end = f2+i2end*d2;
  
  /* adjust x3beg and x3end to fall on sampled values */
  i3beg = NINT((x3beg-f3)/d3);
  i3beg = MAX(0,MIN(n3-1,i3beg));
  x3beg = f3+i3beg*d3;
  i3end = NINT((x3end-f3)/d3);
  i3end = MAX(0,MIN(n3-1,i3end));
  x3end = f3+i3end*d3;

  /* allocate space for image bytes */
  n1c = 1+abs(i1end-i1beg);
  n2c = 1+abs(i2end-i2beg);
  n3c = 1+abs(i3end-i3beg);
  czfront = ealloc1(n1c*n2c,sizeof(char));
  czside = ealloc1(n1c*n3c,sizeof(char));
  cztop = ealloc1(n2c*n3c,sizeof(char));
  
  /* compute conversion constants */
  zscale = (wclip!=bclip)?255.0/(wclip-bclip):1.0e10;
  zoffset = -bclip*zscale;
  i1step = (i1end>i1beg)?1:-1;
  i2step = (i2end>i2beg)?1:-1;
  i3step = (i3end>i3beg)?1:-1;
  
  /* convert front data to be imaged into unsigned characters */
  czp = czfront;
  for (i2c=0,i2=i2beg; i2c<n2c; i2c++,i2+=i2step) {
    for (i1c=0,i1=i1beg; i1c<n1c; i1c++,i1+=i1step) {
      zi = zoffset+zfront[i1+i2*n1]*zscale;
      if (zi<0.0) zi = 0.0;
      if (zi>255.0) zi = 255.0;
      *czp++ = (unsigned char)zi;
    }
  }
  
  /* convert side data to be imaged into unsigned characters */
  czp = czside;
  for (i3c=0,i3=i3beg; i3c<n3c; i3c++,i3+=i3step) {
    for (i1c=0,i1=i1beg; i1c<n1c; i1c++,i1+=i1step) {
      zi = zoffset+zside[i1+i3*n1]*zscale;
      if (zi<0.0) zi = 0.0;
      if (zi>255.0) zi = 255.0;
      *czp++ = (unsigned char)zi;
    }
  }

  /* convert top data to be imaged into unsigned characters */
  czp = cztop;
  for (i3c=0,i3=i3beg; i3c<n3c; i3c++,i3+=i3step) {
    for (i2c=0,i2=i2beg; i2c<n2c; i2c++,i2+=i2step) {
      zi = zoffset+ztop[i2+i3*n2]*zscale;
      if (zi<0.0) zi = 0.0;
      if (zi>255.0) zi = 255.0;
      *czp++ = (unsigned char)zi;
    }
  }
  /* free1float(z); */
  
  /* determine sampling after scaling */
  n1s = MAX(1,NINT(1+(n1c-1)*d1/d1s));
  d1s = (n1s>1)?d1*(n1c-1)/(n1s-1):d1;
  n2s = MAX(1,NINT(1+(n2c-1)*d2/d2s));
  d2s = (n2s>1)?d2*(n2c-1)/(n2s-1):d2;
  n3s = MAX(1,NINT(1+(n3c-1)*d3/d3s));
  d3s = (n3s>1)?d3*(n3c-1)/(n3s-1):d3;
  
  /* if necessary, interpolate front to scaled sampling intervals */
  if (n1s!=n1c || n2s!=n2c) {
    szfront = ealloc1(n1s*n2s,sizeof(char));
    intl2b(n1c,d1,0.0,n2c,d2,0.0,czfront,
	   n1s,d1s,0.0,n2s,d2s,0.0,szfront);
    free1(czfront);
  } else {
    szfront = czfront;
  }
  
  /* if necessary, interpolate side to scaled sampling intervals */
  if (n1s!=n1c || n3s!=n3c) {
    szside = ealloc1(n1s*n3s,sizeof(char));
    intl2b(n1c,d1,0.0,n3c,d3,0.0,czside,
	   n1s,d1s,0.0,n3s,d3s,0.0,szside);
    free1(czside);
  } else {
    szside = czside;
  }
  
  /* if necessary, interpolate top to scaled sampling intervals */
  if (n2s!=n2c || n3s!=n3c) {
    sztop = ealloc1(n2s*n3s,sizeof(char));
    intl2b(n2c,d2,0.0,n3c,d3,0.0,cztop,
	   n2s,d2s,0.0,n3s,d3s,0.0,sztop);
    free1(cztop);
  } else {
    sztop = cztop;
  }
  
  /* determine axes pads */
  p1beg = (x1end>x1beg)?-fabs(d1s)/2:fabs(d1s)/2;
  p1end = (x1end>x1beg)?fabs(d1s)/2:-fabs(d1s)/2;
  p2beg = (x2end>x2beg)?-fabs(d2s)/2:fabs(d2s)/2;
  p2end = (x2end>x2beg)?fabs(d2s)/2:-fabs(d2s)/2;
  p3beg = (x3end>x3beg)?-fabs(d3s)/2:fabs(d3s)/2;
  p3end = (x3end>x3beg)?fabs(d3s)/2:-fabs(d3s)/2;
  
  /* get legend specs BEREND, Schoenfelder */
  legend = 0; getparint("legend", &legend); /* BEREND, Schoenfelder */
  getparstring("units", &units); /* BEREND, Schoenfelder */
  getparstring("legendfont", &legendfont);     /* BEREND, Schoenfelder */
  
  /* Get or calc legend parameters */
  /* Legend min and max: Calc from data read in */
  if (legend) {
    for (lz=0;lz<nz;lz++) {
      lmin=FMIN(lmin,z[lz]);
      lmax=FMAX(lmax,z[lz]);
    }
    if (verbose==2) warn("lmin=%g lmax=%g",lmin,lmax);
  
    lbeg = lmin; if (getparfloat("lbeg",&lbeg)) lbegsup=1;
    lend = lmax; if (getparfloat("lend",&lend)) lendsup=1;
    /* Change wclip,bclip to be inside legend range */
    wclip = FMAX(lbeg,wclip); /* [wclip,bclip] has to be in [lbeg,lend] */
    bclip = FMIN(lend,bclip);
    if (lbegsup!=1) { /* Add white and black areas to show possible clipping */ 
      float rangeperc=(bclip-wclip)/20.;
      lbeg=wclip-rangeperc;
    }
    if (lendsup!=1) {
      float rangeperc=(bclip-wclip)/20.;
      lend=bclip+rangeperc;
    }
    
    lfnum = lmin; getparfloat("lfnum",&lfnum);
    
    getparstring("lstyle",&lstyles);
    if (STREQ("vertright",lstyles))
      lstyle = VERTRIGHT;
    else if (STREQ("horibottom",lstyles))
      lstyle = HORIBOTTOM;
    /* legend dimensions (BEREND), Schoenfelder */
    lwidth = 0.1 ;lheight = size1+sin(angle)*size3/2;
    if (lstyle==HORIBOTTOM) {
      lwidth=size2+cos(angle)*size3/1.2 ;lheight = 0.24;
    }
    getparfloat("lwidth",&lwidth);
    getparfloat("lheight",&lheight);
    
    lx=.8;ly = ybox+(size1+sin(angle)*size3-lheight)/2;
    if (lstyle==VERTRIGHT) {
      lx=xbox+size2+cos(angle)*size3+0.1;
    } else if (lstyle==HORIBOTTOM) {
      lx=xbox+(size2+cos(angle)*size3-lwidth)/2.0;ly = 1.0;
    }
    getparfloat("lx",&lx);
    getparfloat("ly",&ly);
    
    getparstring("lgrid",&lgrids);
    if (STREQ("dot",lgrids))
      ugrid = DOT;
    else if (STREQ("dash",lgrids))
      ugrid = DASH;
    else if (STREQ("solid",lgrids))
      ugrid = SOLID;
    else
      ugrid = NONE;
  }
  

  if (legend) {
    /* Make legend color values */
    int lll=0,lcount,perc5=13,ilbeg,ilend; /* color scale */
    if (lbegsup!=1) {
      ln+=perc5; /* white area */
    }
    if (lendsup!=1) {
      ln+=perc5; /* black area */
    }
    data_legend = ealloc1(ln,sizeof(char));
    if (lbegsup!=1) {
      for (lll=0;lll<perc5;lll++) data_legend[lll]=(char) 255; /* white area */
    }
    for (lcount=255;lcount>=0;lcount--,lll++) data_legend[lll]=(char) lcount;
    if (lendsup!=1) {
      for (;lll<ln;lll++) data_legend[lll]=(char) 0; /* black area */
    }
    lf=lbeg;ld=(lend-lbeg)/(ln-1);
    if (!(getparfloat("ldnum",&ldnum)))	ldnum=0.0;
    
    checkpars();

    /* adjust lbeg and lend to fall on sampled values */
    
    ilbeg = NINT((lbeg-lf)/ld);
    ilbeg = MAX(0,MIN(ln-1,ilbeg));
    lbeg = lf+ilbeg*ld;
    ilend = NINT((lend-lf)/ld);
    ilend = MAX(0,MIN(ln-1,ilend));
    lend = lf+ilend*ld;

    /* convert legend parameters to points */  
    lx *= 72.0; /* Schoenfelder */
    ly *= 72.0; /* Schoenfelder */
    lwidth *= 72.0; /* Schoenfelder */
    lheight *= 72.0; /* Schoenfelder */
  }

  /* convert axes parameters to points */  
  size1 *= 72;
  size2 *= 72;
  size3 *= 72;
  xbox *= 72;
  ybox *= 72;
  
  /* set bounding box */
  psAxesBBox(xbox,ybox,size2+cos(angle)*size3,size1+sin(angle)*size3,
	     labelfont,labelsize,titlefont,titlesize,style,bbox);
  if (legend) {
    psLegendBBox( /* Space for legend Schoenfelder */
		 lx,ly,lwidth,lheight,
		 labelfont,labelsize,
		 lstyle,lbbox);
    /* Include space for legend Schoenfelder */
    bbox[0]=MIN(bbox[0],lbbox[0]);
    bbox[1]=MIN(bbox[1],lbbox[1]);
    bbox[2]=MAX(bbox[2],lbbox[2]);
    bbox[3]=MAX(bbox[3],lbbox[3]);
  }
  boundingbox(bbox[0],bbox[1],bbox[2],bbox[3]);
  
  /* begin PostScript */
  begineps();
  
  /* save graphics state */
  gsave();
  
  /* translate coordinate system by box offset */
  translate(xbox,ybox);
  
  /* begin front */
  gsave();
  
  /* transform coordinates */
  translate(0,0);
  scale(size2,size1);
  
  /* determine image matrix */
  matrix[0] = 0;  matrix[1] = n2s;  matrix[2] = -n1s;
  matrix[3] = 0;  matrix[4] = n1s;  matrix[5] = 0;
  
  /* draw the image */
  drawimage(hls,colors,n1s,n2s,bps,matrix,szfront);
  
  /* end front */
  grestore();
  
  /* begin side */
  gsave();
  
  /* transform and skew coordinates */
  matrix[0] = 1;  matrix[1] = tan(angle);  matrix[2] = 0;
  matrix[3] = 1;  matrix[4] = 0;  matrix[5] = 0;
  translate(size2,0);
  concat(matrix);
  scale(size3*cos(angle),size1);
  
  /* determine image matrix */
  matrix[0] = 0;  matrix[1] = n3s;  matrix[2] = -n1s;
  matrix[3] = 0;  matrix[4] = n1s;  matrix[5] = 0;
  
  /* draw the image */
  drawimage(hls,colors,n1s,n3s,bps,matrix,szside);
  
  /* end side */
  grestore();
  
  /* begin top */
  gsave();
  
  
  /* transform and skew coordinates */
  matrix[0] = 1;  matrix[1] = 0;  matrix[2] = 1.0/tan(angle);
  matrix[3] = 1;  matrix[4] = 0;  matrix[5] = 0;
  translate(0,size1);
  concat(matrix);
  scale(size2,size3*sin(angle));
  
  /* determine image matrix */
  matrix[0] = n2s;  matrix[1] = 0;  matrix[2] = 0;
  matrix[3] = n3s;  matrix[4] = 0;  matrix[5] = 0;
  
  /* draw the image */
  drawimage(hls,colors,n2s,n3s,bps,matrix,sztop);
  
  /* end top */
  grestore();
  
  if (legend) {
    gsave();
  /* translate coordinate system by box offset */
  translate(-xbox,-ybox);
    translate(lx,ly);
    scale(lwidth,lheight);
    if ((lstyle==VERTLEFT) || (lstyle==VERTRIGHT)) {
      labmatrix[0] = 1;	 labmatrix[1] = 0;  labmatrix[2] = 0;
      labmatrix[3] = ln; labmatrix[4] = 0;  labmatrix[5] = 0;
      drawimage(hls,colors,1,ln,bps,labmatrix,data_legend);
    } else {
      labmatrix[0] = -1; labmatrix[1] = 0;  labmatrix[2] = 0;
      labmatrix[3] = ln; labmatrix[4] = 0;  labmatrix[5] = 0;
      rotate(-90);
      drawimage(hls,colors,1,ln,bps,labmatrix,data_legend);
      rotate(90);
    }
    grestore();
  }
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
  
	/* draw axes and title for legend Schoenfelder*/
	if (legend) {
	  float lpbeg,lpend;
	  int lntic=1;
	  gsave();
	  lpbeg = 0.0; /*(lend>lbeg)?-fabs(d1s)/2:fabs(d1s)/2;*/
	  lpend = 0.0; /*(lend>lbeg)?fabs(d1s)/2:-fabs(d1s)/2;*/
  
	  psLegendBox(
		    lx,ly,lwidth,lheight,
		    lbeg,lend,lpbeg,lpend,
		    ldnum,lf,lntic,ugrid,units,
		    labelfont,labelsize,
		    axescolor,gridcolor,
		    lstyle);
	  grestore();
	}
  /* end PostScript */
  showpage();
  endeps();
  
  return 0;
}

static void drawimage(int hls, float colors[3][2], 
	int width, int height, int bps, float matrix[], unsigned char* z)
{
	int row,col,rowbytes,hi,lo,zi,byte;
	unsigned char *rtab,*gtab,*btab,*rgb,*rgbp,*crgbp,*zp;

	/* handle gray scale as a special case */
	if ( (hls && 
		colors[L][0]==0.0 && 
		colors[L][1]==1.0 &&
		colors[S][0]==0.0 &&
		colors[S][1]==0.0) ||
		(colors[R][0]==0.0 && colors[R][1]==1.0 &&
		colors[G][0]==0.0 && colors[G][1]==1.0 &&
		colors[B][0]==0.0 && colors[B][1]==1.0) ) {
		image(width,height,8,matrix,z);
		return;
	}

	/* allocate space for rgb tables */
	rtab = ealloc1(256,sizeof(unsigned char));
	gtab = ealloc1(256,sizeof(unsigned char));
	btab = ealloc1(256,sizeof(unsigned char));

	/* if colors are hls values, map from hls to rgb tables */
	if (hls) {
		float hscale,lscale,sscale,hbase,lbase,sbase,h,l,s,r,g,b;
		hbase = colors[H][0];
		hscale = (colors[H][1]-colors[H][0])/255.0;
		lbase = colors[L][0];
		lscale = (colors[L][1]-colors[L][0])/255.0;
		sbase = colors[S][0];
		sscale = (colors[S][1]-colors[S][0])/255.0;
		for (zi=0; zi<256; ++zi) {
			h = hbase+zi*hscale;
			l = lbase+zi*lscale;
			s = sbase+zi*sscale;
			hlsrgb(h*360.0,l,s,&r,&g,&b);
			rtab[zi] = r*255.0;
			gtab[zi] = g*255.0;
			btab[zi] = b*255.0;
		}
	
	/* else colors are rgb values, map linearly to rgb tables */
	} else {
		float rscale,gscale,bscale,rbase,gbase,bbase;
		rbase = colors[0][0]*255.0;
		rscale = colors[0][1]-colors[0][0];
		gbase = colors[1][0]*255.0;
		gscale = colors[1][1]-colors[1][0];
		bbase = colors[2][0]*255.0;
		bscale = colors[2][1]-colors[2][0];
		for (zi=0; zi<256; ++zi) {
			rtab[zi] = rbase+zi*rscale;
			gtab[zi] = gbase+zi*gscale;
			btab[zi] = bbase+zi*bscale;
		}
	}

	/* convert unsigned char to rgb unsigned char */
	rgb = ealloc1(width*height*3,sizeof(unsigned char));
	for (row=0,rgbp=rgb,zp=z; row<height; ++row) {
		for (col=0; col<width; ++col) {
			zi = *zp++;
			*rgbp++ = (unsigned char)rtab[zi];
			*rgbp++ = (unsigned char)gtab[zi];
			*rgbp++ = (unsigned char)btab[zi];
		}
	}

	/* free tables */
	free1(rtab);
	free1(gtab);
	free1(btab);

	/* if necessary, compress 24 bit color to 12 bit color */
	if (bps==12) {
		rowbytes = 1+(width*12-1)/8;
		crgbp = rgbp = rgb;
		for (row=0; row<height; ++row) {
			for (byte=0; byte<rowbytes-1; ++byte) {
				hi = (*rgbp++)>>4;
				lo = (*rgbp++)>>4;
				*crgbp++ = (hi<<4)|lo;
			}
			if (width%2) {
				hi = (*rgbp++)>>4;
				*crgbp++ = hi<<4;
			} else {
				hi = (*rgbp++)>>4;
				lo = (*rgbp++)>>4;
				*crgbp++ = (hi<<4)|lo;
			}
		}
	}

	/* draw color image */
	rgbimage(width,height,bps/3,matrix,rgb);

	/* free workspace */
	free1(rgb);
}

/* internal functions to convert HLS to RGB (adapted from Foley & Van Dam) */
static float rgbvalue (float n1, float n2, float hue)
{
        while (hue>360.0) hue -= 360.0;
        while (hue<0.0) hue += 360.0;
        if (hue<60.0)
                return n1+(n2-n1)*hue/60.0;
        else if (hue<180.0)
                return n2;
        else if (hue<240.0)
                return n1+(n2-n1)*(240.0-hue)/60.0;
        else
                return n1;
}
static void hlsrgb (float h, float l, float s, float *r, float *g, float *b)
{
        float m1,m2;
        if (l<=0.5)
                m2 = l*(1.0+s);
        else
                m2 = l+s-l*s;
        m1 = 2*l-m2;
        if (s==0.0) {
                *r = *g = *b = l;
        } else {
                *r = rgbvalue(m1,m2,h+120.0);
                *g = rgbvalue(m1,m2,h);
                *b = rgbvalue(m1,m2,h-120.0);
        }
}
