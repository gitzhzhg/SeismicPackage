/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSIMAGE: $Revision: 1.32 $ ; $Date: 2011/11/17 00:10:53 $	*/

#include "par.h"
#include "psplot.h"
/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" PSIMAGE - PostScript IMAGE plot of a uniformly-sampled function f(x1,x2)",
"            with the option to display a second attribute		",
"									",
" psimage n1= [optional parameters] <binaryfile >postscriptfile	",
"									",
" Required Parameters:							",
" n1			 number of samples in 1st (fast) dimension	",
"									",
" Optional Parameters:							",
" d1=1.0		 sampling interval in 1st dimension		",
" f1=0.0		 first sample in 1st dimension			",
" n2=all		 number of samples in 2nd (slow) dimension	",
" d2=1.0		 sampling interval in 2nd dimension		",
" f2=0.0		 first sample in 2nd dimension			",
" perc=100.0		 percentile used to determine clip		",
" clip=(perc percentile) clip used to determine bclip and wclip		",
" bperc=perc		 percentile for determining black clip value	",
" wperc=100.0-perc	 percentile for determining white clip value	",
" bclip=clip		 data values outside of [bclip,wclip] are clipped",
" wclip=-clip		 data values outside of [bclip,wclip] are clipped",
"                        bclip and wclip will be set to be inside       ",
"                        [lbeg,lend] if lbeg and/or lend are supplied   ",
" threecolor=1		 supply 3 color values instead of only two,	",
"                        using not only black and white, but f.e. red,	",
"                        green and blue					",
" brgb=0.0,0.0,0.0	 red, green, blue values corresponding to black	",
" grgb=1.0,1.0,1.0	 red, green, blue values corresponding to grey	",
" wrgb=1.0,1.0,1.0	 red, green, blue values corresponding to white	",
" bhls=0.0,0.0,0.0	 hue, lightness, saturation corresponding to black",
" ghls=0.0,1.0,0.0	 hue, lightness, saturation corresponding to grey",
" whls=0.0,1.0,0.0	 hue, lightness, saturation corresponding to white",
" bps=12		 bits per sample for color plots, either 12 or 24",
" d1s=1.0		 factor by which to scale d1 before imaging	",
" d2s=1.0		 factor by which to scale d2 before imaging	",
" verbose=1		 =1 for info printed on stderr (0 for no info)	",
" xbox=1.5		 offset in inches of left side of axes box	",
" ybox=1.5		 offset in inches of bottom side of axes box	",
" width=6.0		 width in inches of axes box			",
" height=8.0		 height in inches of axes box			",
" x1beg=x1min		 value at which axis 1 begins			",
" x1end=x1max		 value at which axis 1 ends			",
" d1num=0.0		 numbered tic interval on axis 1 (0.0 for automatic)",
" f1num=x1min		 first numbered tic on axis 1 (used if d1num not 0.0)",
" n1tic=1		 number of tics per numbered tic on axis 1	",
" grid1=none		 grid lines on axis 1 - none, dot, dash, or solid",
" label1=		 label on axis 1				",
" x2beg=x2min		 value at which axis 2 begins			",
" x2end=x2max		 value at which axis 2 ends			",
" d2num=0.0		 numbered tic interval on axis 2 (0.0 for automatic)",
" f2num=x2min		 first numbered tic on axis 2 (used if d2num not 0.0)",
" n2tic=1		 number of tics per numbered tic on axis 2	",
" grid2=none		 grid lines on axis 2 - none, dot, dash, or solid",
" label2=		 label on axis 2				",
" labelfont=Helvetica	 font name for axes labels			",
" labelsize=18		 font size for axes labels			",
" title=		 title of plot					",
" titlefont=Helvetica-Bold font name for title				",
" titlesize=24		  font size for title				",
" titlecolor=black	 color of title					",
" axescolor=black	 color of axes					",
" gridcolor=black	 color of grid					",
" axeswidth=1            width (in points) of axes                      ",
" ticwidth=axeswidth     width (in points) of tic marks			",
" gridwidth=axeswidth    width (in points) of grid lines		",
" style=seismic		 normal (axis 1 horizontal, axis 2 vertical) or	",
"			 seismic (axis 1 vertical, axis 2 horizontal)	",
" legend=0	         =1 display the color scale			",
" lnice=0                =1 nice legend arrangement                     ",
"                        (overrides ybox,lx,width,height parameters)    ",
" lstyle=vertleft 	Vertical, axis label on left side   		",
"			 =vertright (Vertical, axis label on right side)",
"			 =horibottom (Horizontal, axis label on bottom)	",
" units=		 unit label for legend				",
" legendfont=times_roman10    font name for title			",
" following are defaults for lstyle=0. They are changed for other lstyles",
" lwidth=1.2		 colorscale (legend) width in inches 		",
" lheight=height/3     	 colorscale (legend) height in inches		",
" lx=1.0		 colorscale (legend) x-position in inches	",
" ly=(height-lheight)/2+xybox    colorscale (legend) y-position in pixels",
" lbeg= lmin or wclip-5*perc    value at which legend axis begins	",
" lend= lmax or bclip+5*perc    value at which legend axis ends        	",
" ldnum=0.0	 numbered tic interval on legend axis (0.0 for automatic)",
" lfnum=lmin	 first numbered tic on legend axis (used if d1num not 0.0)",
" lntic=1	 number of tics per numbered tic on legend axis ",
" lgrid=none	 grid lines on legend axis - none, dot, dash, or solid",
"									",
" curve=curve1,curve2,...  file(s) containing points to draw curve(s)   ",
" npair=n1,n2,n2,...            number(s) of pairs in each file         ",
" curvecolor=black,..	 color of curve(s)				",
" curvewidth=axeswidth	 width (in points) of curve(s)			",
" curvedash=0            solid curve(s), dash indices 1,...,11 produce  ",
"                        curve(s) with various dash styles              ",
"									",
" infile=none            filename of second attribute n1xn2 array       ",
"                        values must be from range 0.0 - 1.0            ",
"                        (plain unformatted C-style file)               ",
" bckgr=0.5              background gray value				",
"									",
" NOTES:								",
" The curve file is an ascii file with the points specified as x1 x2 	",
" pairs, one pair to a line.  A \"vector\" of curve files and curve	",
" colors may be specified as curvefile=file1,file2,etc.			",
" and curvecolor=color1,color2,etc, and the number of pairs of values   ",
" in each file as npair=npair1,npair2,... .				",
"									",
" You may eliminate the blocky appearance of psimages by adjusting the  ",
" d1s= and d2s= parameters, so that psimages appear similar to ximages.	",
"									",
" All color specifications may also be made in X Window style Hex format",
" example:   axescolor=#255						",
" 									",
" Some example colormap settings:					",
" red white blue: wrgb=1.0,0,0 grgb=1.0,1.0,1.0 brgb=0,0,1.0 		",
" white red blue: wrgb=1.0,1.0,1.0 grgb=1.0,0.0,0.0 brgb=0,0,1.0 	",
" blue red white: wrgb=0.0,0.0,1.0 grgb=1.0,0.0,0.0 brgb=1.0,1.0,1.0 	",
" red green blue: wrgb=1.0,0,0 grgb=0,1.0,0 brgb=0,0,1.0		",
" orange light-blue green: wrgb=1.0,.5,0 grgb=0,.7,1.0 brgb=0,1.0,0	",
" red light-blue dark blue: wrgb=0.0,0,1.0 grgb=0,1.0,1.0 brgb=0,0,1.0 	",
" 									",
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
"									",
NULL};

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 05/29/90
 * MODIFIED:  Craig Artley, Colorado School of Mines, 08/30/91
 *	    BoundingBox moved to top of PostScript output
 * MODIFIED:  Craig Artley, Colorado School of Mines, 12/16/93
 *	    Added color options (Courtesy of Dave Hale, Advance Geophysical).
 * Modified: Morten Wendell Pedersen, Aarhus University, 23/3-97
 *           Added ticwidth,axeswidth, gridwidth parameters 
 * MODIFIED: Torsten Schoenfelder, Koeln, Germany 006/07/97
 *          colorbar (legend) (as in ximage (by Berend Scheffers, Delft))
 * MODIFIED: Brian K. Macy, Phillips Petroleum, 01/14/99
 *	    Added curve plotting option
 * MODIFIED: Torsten Schoenfelder, Koeln, Germany 02/10/99
 *          color scale with interpolation of three colors
 * MODIFIED: Ekkehart Tessmer, University of Hamburg, Germany, 08/22/2007
 *          Added dashing option to curve plotting
 */
/**************** end self doc ********************************/

/* color specification array indices for RGB and HLS */
#define R 0
#define G 1
#define B 2
#define H 0
#define L 1
#define S 2
#define RIGHT 0
#define LEFT 1
#define FMAX(x,y) (float) (x) > (y) ? (x) : (y) 
#define FMIN(x,y) (float) (x) > (y) ? (y) : (x) 
#define EPS_PSPLOT .000001	/* epsilon value to stabilize legend */


/* functions defined and used internally */
static void drawimage(int hls, float colors[3][3], 
	int width, int height, int bps, float matrix[], unsigned char* z);
static void drawimage2(int hls, float colors[3][3], 
		       int width, int height, int bps, float matrix[], unsigned char* z, unsigned char* y, float bckgr);
static float rgbvalue (float n1, float n2, float hue);
static void hlsrgb (float h, float l, float s, float *r, float *g, float *b);

int 
main (int argc, char **argv)
{
	int n1,n2,n1tic,n2tic,nfloats,bbox[4],
	  i1,i2,grid1,grid2,style,
	  n1c,n2c,n1s,n2s,i1beg,i1end,i2beg,i2end,i1c,i2c,
	  nz,iz,i1step,i2step,verbose,hls,bps,
	  legend,ugrid=SOLID,lstyle=VERTLEFT,lz,lbegsup=0,lendsup=0,ln=256,
	  lbbox[4], threecolor=0; /* BEREND, Schoenfelder */
        int lnice; /* c liner */
	float labelsize,titlesize,perc,clip,bperc,wperc,bclip,wclip,
	  d1,f1,d2,f2,*z,*y=NULL,*temp,zscale,zoffset,zi,
		xbox,ybox,width,height,
		x1beg,x1end,x2beg,x2end,
		x1min,x1max,x2min,x2max,
		d1num,f1num,d2num,f2num,
		p1beg,p1end,p2beg,p2end,matrix[6],colors[3][3], /* for 3 color mode */
		d1s,d2s,
	  lwidth,lheight,lx,ly,lbeg,lend,lmin=(float) FLT_MAX,lmax=(float) -FLT_MAX,
	  ldnum,lfnum,ld,lf=0,labmatrix[6]; /* BEREND, Schoenfelder */
	float axeswidth, ticwidth, gridwidth;
	float infile_min, infile_max; /* E.T. */
	unsigned char *cz,*czp,*sz,*data_legend=NULL;
	unsigned char *cy=NULL,*cyp=NULL,*sy=NULL; /* E.T. */
	char *label1="",*label2="",*title="",*units="",
	  *legendfont="times_roman10",
	  *labelfont="Helvetica",*titlefont="Helvetica-Bold",
	  *styles="seismic",*grid1s="none",*grid2s="none",
	  *titlecolor="black",*axescolor="black",*gridcolor="black",
	  *lstyles="vertleft",*lgrids="none";
	FILE *infp=stdin, *in2fp=NULL; /* in2fp : E.T. */

	float **x1curve=NULL,**x2curve=NULL,*curvewidth=NULL;
	int i,j,curve=0,*npair=NULL,ncurvecolor=0,ncurvewidth=0,ncurvedash=0,*curvedash=NULL;
	char **curvecolor=NULL,**curvefile=NULL;
	char *infile=NULL; /* E.T. */
	FILE *curvefp=NULL;
	cwp_Bool is_curve = cwp_false;
	float bckgr; /* E.T. */

	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters describing 1st dimension sampling */
	if (!getparint("n1",&n1)) err("must specify n1!\n");
	d1 = 1.0;  getparfloat("d1",&d1);
	f1 = 0.0;  getparfloat("f1",&f1);
	x1min = (d1>0.0)?f1:f1+(n1-1)*d1;
	x1max = (d1<0.0)?f1:f1+(n1-1)*d1;

	/* get parameters describing 2nd dimension sampling */
	if (!getparint("n2",&n2)) {
		if (efseeko(infp,(off_t) 0,SEEK_END)!=0)
			err("must specify n2 if in a pipe!\n");
		nfloats = (int) (eftello(infp)/((off_t) sizeof(float)));
		efseeko(infp,(off_t) 0,SEEK_SET);
		n2 = nfloats/n1;
	}
	d2 = 1.0;  getparfloat("d2",&d2);
	f2 = 0.0;  getparfloat("f2",&f2);
	x2min = (d2>0.0)?f2:f2+(n2-1)*d2;
	x2max = (d2<0.0)?f2:f2+(n2-1)*d2;

	/* read color parameters */
	if (!getparint("threecolor",&threecolor)) threecolor=1;
	bps = 8;
	hls = 0;
	/* color[][0] is black, color[][2] is white in 2 color mode */
	colors[R][0] = colors[G][0] = colors[B][0] = 0.0;
	colors[R][1] = colors[G][1] = colors[B][1] = 0.5;
	colors[R][2] = colors[G][2] = colors[B][2] = 1.0;
	if (countparval("brgb") || countparval("wrgb")) {
		float brgb[3],grgb[3],wrgb[3];
		brgb[R] = brgb[G] = brgb[B] = 0.0;
		wrgb[R] = wrgb[G] = wrgb[B] = 1.0;
		getparfloat("brgb",&brgb[0]);
		getparfloat("wrgb",&wrgb[0]);
		grgb[R] = (brgb[R] + wrgb[R])/2.;
		grgb[G] = (brgb[G] + wrgb[G])/2.;
		grgb[B] = (brgb[B] + wrgb[B])/2.;
		if (threecolor==1)
		  getparfloat("grgb",&grgb[0]);
		brgb[R] = MAX(0.0,MIN(1.0,brgb[R]));
		grgb[R] = MAX(0.0,MIN(1.0,grgb[R]));
		wrgb[R] = MAX(0.0,MIN(1.0,wrgb[R]));
		brgb[G] = MAX(0.0,MIN(1.0,brgb[G]));
		grgb[G] = MAX(0.0,MIN(1.0,grgb[G]));
		wrgb[G] = MAX(0.0,MIN(1.0,wrgb[G]));
		brgb[B] = MAX(0.0,MIN(1.0,brgb[B]));
		grgb[B] = MAX(0.0,MIN(1.0,grgb[B]));
		wrgb[B] = MAX(0.0,MIN(1.0,wrgb[B]));
		colors[R][0] = brgb[R];	 colors[R][1] = grgb[R];  colors[R][2] = wrgb[R];
		colors[G][0] = brgb[G];	 colors[G][1] = grgb[G];  colors[G][2] = wrgb[G];
		colors[B][0] = brgb[B];	 colors[B][1] = grgb[B];  colors[B][2] = wrgb[B];
		if (!getparint("bps",&bps)) bps = 12;
		if (bps!=12 && bps!=24)
			err("bps must equal 12 or 24 for color plots!\n");
	} else if (countparval("bhls") || countparval("whls")) {
		float bhls[3],ghls[3],whls[3];
		hls = 1;
		bhls[H] = ghls[H] = whls[H] = 0.0;
		bhls[L] = 0.0;	ghls[L] = 0.5;	whls[L] = 1.0;
		bhls[S] = ghls[S] = whls[S] = 0.0;
		getparfloat("bhls",&bhls[0]);
		getparfloat("whls",&whls[0]);
		ghls[H] = (bhls[H] + whls[H])/2.;
		ghls[L] = (bhls[L] + whls[L])/2.;
		ghls[S] = (bhls[S] + whls[S])/2.;
		if (threecolor==1)
		  getparfloat("ghls",&ghls[0]);
		bhls[L] = MAX(0.0,MIN(1.0,bhls[L]));
		ghls[L] = MAX(0.0,MIN(1.0,ghls[L]));
		whls[L] = MAX(0.0,MIN(1.0,whls[L]));
		bhls[S] = MAX(0.0,MIN(1.0,bhls[S]));
		ghls[S] = MAX(0.0,MIN(1.0,ghls[S]));
		whls[S] = MAX(0.0,MIN(1.0,whls[S]));
		colors[H][0] = bhls[0];	 colors[H][1] = ghls[0];  colors[H][2] = whls[0];
		colors[L][0] = bhls[1];	 colors[L][1] = ghls[1];  colors[L][2] = whls[1];
		colors[S][0] = bhls[2];	 colors[S][1] = ghls[2];  colors[S][2] = whls[2];
		if (!getparint("bps",&bps)) bps = 12;
		if (bps!=12 && bps!=24)
			err("bps must equal 12 or 24 for color plots!\n");
	}

	/* get legend specs BEREND, Schoenfelder */
	legend = 0; getparint("legend", &legend); /* BEREND, Schoenfelder */
	getparstring("units", &units); /* BEREND, Schoenfelder */
	getparstring("legendfont", &legendfont);     /* BEREND, Schoenfelder */

	/* set up curve plotting */
	if ((curve=countparval("curve"))!=0) {
		curvefile=(char**)ealloc1(curve,sizeof(void*));
		getparstringarray("curve",curvefile);
		if ((x1curve=(float**)malloc(curve*sizeof(void*)))==NULL)
			err("Could not allocate x1curve pointers\n");
		if ((x2curve=(float**)malloc(curve*sizeof(void*)))==NULL)
			err("Could not allocate x2curve pointers\n");
		npair=ealloc1int(curve);
		getparint("npair",npair);
		is_curve = cwp_true;
	} else {
		npair=(int *)NULL;
		curvefile=(char **)NULL;
		x1curve=(float **)NULL;
		x2curve=(float **)NULL;
		is_curve = cwp_false;
	}
	if (is_curve) {
	 if ((ncurvecolor=countparval("curvecolor"))<curve) {
		curvecolor=(char**)ealloc1(curve,sizeof(void*));
		if (!getparstringarray("curvecolor",curvecolor)) {
			curvecolor[0]=(char *)cwp_strdup("black\0");
			ncurvecolor=1;
		}
		for (i=ncurvecolor; i<curve; i++)
			curvecolor[i]=(char *)cwp_strdup(curvecolor[ncurvecolor-1]);
	 } else if (ncurvecolor) {
		curvecolor=(char**)ealloc1(ncurvecolor,sizeof(void*));
		getparstringarray("curvecolor",curvecolor);
	 }
	 for (j=0; j<curve; j++) {
		curvefp=efopen(curvefile[j],"r");
		x1curve[j]=ealloc1float(npair[j]);
		x2curve[j]=ealloc1float(npair[j]);
		for (i=0; i<npair[j]; i++) {
			fscanf(curvefp,"%f",&x1curve[j][i]);
			fscanf(curvefp,"%f",&x2curve[j][i]);
		}
		efclose(curvefp);
	 }
	}

	/*** E.T. ***/
	getparstring("infile",&infile);
	if (infile != NULL) {
	  in2fp=fopen(infile,"r");
	  if (in2fp == NULL) {
	    fprintf(stderr,"error opening infile: %s\n",infile);
	    err("stopped.\n");
	  }
	}
	/******/

	/* read binary data to be plotted */
	nz = n1*n2;
	z = ealloc1float(nz);
	if (fread(z,sizeof(float),nz,infp)!=nz)
		err("error reading input file!\n");

	if (infile != NULL) { /* E.T. */
	  if (!getparfloat("bckgr",&bckgr)) bckgr = 0.5;
	  y = ealloc1float(nz);
	  if (fread(y,sizeof(float),nz,in2fp)!=nz)
		err("error reading modulating input file!\n");
	  
	  infile_min = 1.;
	  infile_max = 0.;
	  for (iz=0; iz<nz; iz++) {
	    infile_min = (infile_min < y[iz] ? infile_min : y[iz]);
	    infile_max = (infile_max > y[iz] ? infile_max : y[iz]);
	  }
	  if (infile_min < 0. || infile_max > 1.)
	    err("error: value of infile out of range 0..1!\n");
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
	verbose = 1;  getparint("verbose",&verbose);
	if (verbose) warn("bclip=%g wclip=%g",bclip,wclip);

	/* get scaled sampling intervals */
	d1s = 1.0;  getparfloat("d1s",&d1s);
	d2s = 1.0;  getparfloat("d2s",&d2s);
	d1s = fabs(d1s);  d1s *= d1;
	d2s = fabs(d2s);  d2s *= d2;

	/* get axes parameters */
	xbox = 1.5; getparfloat("xbox",&xbox); /* if psimage is called by ximage, it */
	ybox = 1.5; getparfloat("ybox",&ybox); /* will xbox=1.166 and ybox=1.167 */
	width = 6.0; getparfloat("wbox",&width); getparfloat("width",&width);
	height = 8.0;getparfloat("hbox",&height);getparfloat("height",&height);
         /* begin c liner */
	lnice = 0;  getparint("lnice",&lnice); 
        if (lnice==1) {
            ybox = 2.2;
            /* lx=8 is set below, after getpar on lx ... c liner */
            width = 5.4;
            height = 7.2;
        }
         /* end c liner */
	x1beg = x1min; getparfloat("x1beg",&x1beg);
	x1end = x1max; getparfloat("x1end",&x1end);
	d1num = 0.0; getparfloat("d1num",&d1num);
	f1num = x1min; getparfloat("f1num",&f1num);
	n1tic = 1; getparint("n1tic",&n1tic);
	getparstring("grid1",&grid1s);
	if (STREQ("dot",grid1s))
		grid1 = DOT;
	else if (STREQ("dash",grid1s))
		grid1 = DASH;
	else if (STREQ("solid",grid1s))
		grid1 = SOLID;
	else
		grid1 = NONE;
	getparstring("label1",&label1);
	x2beg = x2min; getparfloat("x2beg",&x2beg);
	x2end = x2max; getparfloat("x2end",&x2end);
	d2num = 0.0; getparfloat("d2num",&d2num);
	f2num = 0.0; getparfloat("f2num",&f2num);
	n2tic = 1; getparint("n2tic",&n2tic);
	getparstring("grid2",&grid2s);
	if (STREQ("dot",grid2s))
		grid2 = DOT;
	else if (STREQ("dash",grid2s))
		grid2 = DASH;
	else if (STREQ("solid",grid2s))
		grid2 = SOLID;
	else
		grid2 = NONE;
	getparstring("label2",&label2);
	getparstring("labelfont",&labelfont);
	labelsize = 18.0; getparfloat("labelsize",&labelsize);
	getparstring("title",&title);
	getparstring("titlefont",&titlefont);
	titlesize = 24.0; getparfloat("titlesize",&titlesize);
	getparstring("titlecolor",&titlecolor);
	getparstring("axescolor",&axescolor);
	getparstring("gridcolor",&gridcolor);

	/* axes and tic width */
        if(!getparfloat("axeswidth",&axeswidth)) axeswidth=1;
        if (!getparfloat("ticwidth",&ticwidth)) ticwidth=axeswidth;
        if(!getparfloat("gridwidth",&gridwidth)) gridwidth =axeswidth;

	if (is_curve) {
	 if ((ncurvewidth=countparval("curvewidth"))<curve) {
		curvewidth=ealloc1float(curve);
		if (!getparfloat("curvewidth",curvewidth)) {
			curvewidth[0]=axeswidth;
			ncurvewidth=1;
		}
		for (i=ncurvewidth; i<curve; i++)
			curvewidth[i]=curvewidth[ncurvewidth-1];
	 } else {
		curvewidth=ealloc1float(ncurvewidth);
		getparfloat("curvewidth",curvewidth);
	 }
	 if ((ncurvedash=countparval("curvedash"))<curve) {
		curvedash=ealloc1int(curve);
		if (!getparint("curvedash",curvedash)) {
		        curvedash[0]=0;
			ncurvedash=1;
		}
		for (i=ncurvedash; i<curve; i++)
			curvedash[i]=curvedash[ncurvedash-1];
	 } else {
		curvedash=ealloc1int(ncurvedash);
		getparint("curvedash",curvedash);
	 }
	}

	getparstring("style",&styles);

	if (STREQ("normal",styles))
		style = NORMAL;
	else
		style = SEISMIC;

	/* Get or calc legend parameters */
	/* Legend min and max: Calc from data read in */
	if (legend) {
	  for (lz=0;lz<nz;lz++) {
	    lmin=FMIN(lmin,z[lz]);
	    lmax=FMAX(lmax,z[lz]);
	  }
	  if (verbose==2) warn("lmin=%g lmax=%g",lmin,lmax);
	}

	if (legend) {
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
	  lwidth = 0.1 ;lheight = height/2;
	  if (lstyle==HORIBOTTOM) {
	    lwidth=width/1.2 ;lheight = 0.24;
	  }
	  getparfloat("lwidth",&lwidth);
	  getparfloat("lheight",&lheight);
	  
	  lx=.8;ly = ybox+(height-lheight)/2;
	  if (lstyle==VERTRIGHT) {
	    lx=xbox+width+0.1;
	  } else if (lstyle==HORIBOTTOM) {
	    lx=xbox+(width-lwidth)/2.0;ly = 1.0;
	  }
	  getparfloat("lx",&lx);
          if (lnice==1) lx = 8;   /* c liner */
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

	/* adjust x1beg and x1end to fall on sampled values */
	/* This will not allow to display an area greater than the data supplied */
	i1beg = NINT((x1beg-f1)/d1);
	i1beg = MAX(0,MIN(n1-1,i1beg));
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
	}
	/* allocate space for image bytes */
	n1c = 1+abs(i1end-i1beg);
	n2c = 1+abs(i2end-i2beg);
	cz = ealloc1(n1c*n2c,sizeof(char));
	if (infile != NULL) cy = ealloc1(n1c*n2c,sizeof(char)); /* E.T. */

	/* convert data to be imaged into unsigned characters */
	zscale = (wclip!=bclip)?255.0/(wclip-bclip):1.0e10;
	zoffset = -bclip*zscale;
	i1step = (i1end>i1beg)?1:-1;
	i2step = (i2end>i2beg)?1:-1;
	czp = cz;
	if (infile != NULL) cyp = cy; /* E.T. */

	for (i1c=0,i1=i1beg; i1c<n1c; i1c++,i1+=i1step) {
		for (i2c=0,i2=i2beg; i2c<n2c; i2c++,i2+=i2step) {
			zi = zoffset+z[i1+i2*n1]*zscale;
			if (zi<0.0) zi = 0.0;
			if (zi>255.0) zi = 255.0;
			*czp++ = (unsigned char)zi;
			if (infile != NULL) {
			  *cyp++ = (unsigned char)(y[i1+i2*n1]*255.0); /* E.T. */
			}
		}
	}
	free1float(z);
	if (infile != NULL) free1float(y); /* E.T. */

	/* determine sampling after scaling */
	n1s = MAX(1,NINT(1+(n1c-1)*d1/d1s));
	d1s = (n1s>1)?d1*(n1c-1)/(n1s-1):d1;
	n2s = MAX(1,NINT(1+(n2c-1)*d2/d2s));
	d2s = (n2s>1)?d2*(n2c-1)/(n2s-1):d2;

	/* if necessary, interpolate to scaled sampling intervals */
	if (n1s!=n1c || n2s!=n2c) {
		sz = ealloc1(n1s*n2s,sizeof(char));
		intl2b(n2c,d2,0.0,n1c,d1,0.0,cz,n2s,d2s,0.0,n1s,d1s,0.0,sz); /* Interpol array */
		free1(cz);
	} else {
		sz = cz;
	}

	if (infile != NULL) { /* E.T. */
	/* if necessary, interpolate to scaled sampling intervals */
	  if (n1s!=n1c || n2s!=n2c) {
	    sy = ealloc1(n1s*n2s,sizeof(char));
	    intl2b(n2c,d2,0.0,n1c,d1,0.0,cy,n2s,d2s,0.0,n1s,d1s,0.0,sy); /* Interpol array */
	    free1(cy);
	  } else {
	    sy = cy;
	  }
	}	  

	/* determine axes pads */
	p1beg = (x1end>x1beg)?-fabs(d1s)/2:fabs(d1s)/2;
	p1end = (x1end>x1beg)?fabs(d1s)/2:-fabs(d1s)/2;
	p2beg = (x2end>x2beg)?-fabs(d2s)/2:fabs(d2s)/2;
	p2end = (x2end>x2beg)?fabs(d2s)/2:-fabs(d2s)/2;

	/* convert axes box parameters from inches to points */
	xbox *= 72.0;
	ybox *= 72.0;
	width *= 72.0;
	height *= 72.0;
	if (legend) {
	  lx *= 72.0; /* Schoenfelder */
	  ly *= 72.0; /* Schoenfelder */
	  lwidth *= 72.0; /* Schoenfelder */
	  lheight *= 72.0; /* Schoenfelder */
	}

	/* set bounding box */
	psAxesBBox(
		   xbox,ybox,width,height,
		   labelfont,labelsize,
		   titlefont,titlesize,
		   style,bbox);
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

	/* determine image matrix */
	if (style==NORMAL) {
		matrix[0] = 0;	matrix[1] = n1s;  matrix[2] = n2s;
		matrix[3] = 0;	matrix[4] = 0;	matrix[5] = 0;
	} else {
		matrix[0] = n2s;  matrix[1] = 0;  matrix[2] = 0;
		matrix[3] = -n1s;  matrix[4] = 0;  matrix[5] = n1s;
	}

	scale(width,height);

	/* draw the image (before axes so grid lines are visible) */
	if (infile == NULL) { /* E.T. */
	  drawimage(hls,colors,n2s,n1s,bps,matrix,sz);
	} else {
	  drawimage2(hls,colors,n2s,n1s,bps,matrix,sz,sy,bckgr);
	}
	/***************************/
	/* main image has been drawn, restore graphics state */
	grestore();

	/* *********************************/
	/* draw the colorbar (before axes so grid lines are visible) Schoenfelder*/
	if (legend) {
	  gsave();
	  translate(lx,ly);
	  scale(lwidth,lheight);
	  if ((lstyle==VERTLEFT) || (lstyle==VERTRIGHT)) {
	    labmatrix[0] = 1;	 labmatrix[1] = 0;  labmatrix[2] = 0;
	    labmatrix[3] = ln; labmatrix[4] = 0;  labmatrix[5] = 0;
	    drawimage(hls,colors,1,ln,bps,labmatrix,data_legend);
	  } else {
	    labmatrix[0] = -1;	 labmatrix[1] = 0;  labmatrix[2] = 0;
	    labmatrix[3] = ln; labmatrix[4] = 0;  labmatrix[5] = 0;
	    rotate(-90);
	    drawimage(hls,colors,1,ln,bps,labmatrix,data_legend);
	    rotate(90);
	  }
	  
	  grestore();
	}

	/* draw curve */
	for (i=0; i<curve; i++) {
		gsave();
		psDrawCurve(
			xbox,ybox,width,height,
			x1beg,x1end,p1beg,p1end, 
			x2beg,x2end,p2beg,p2end,
			x1curve[i],x2curve[i],npair[i],
			curvecolor[i],curvewidth[i],curvedash[i],style);
		grestore();
	}


	gsave();
	/* draw axes and title */
	psAxesBox(
		  xbox,ybox,width,height,
		  x1beg,x1end,p1beg,p1end,
		  d1num,f1num,n1tic,grid1,label1,
		  x2beg,x2end,p2beg,p2end,
		  d2num,f2num,n2tic,grid2,label2,
		  labelfont,labelsize,
		  title,titlefont,titlesize,
		  titlecolor,axescolor,gridcolor,
		  ticwidth,axeswidth,gridwidth,
		  style);
	/* restore graphics state */
	grestore();

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

	if (curve) {
		free1int(npair);
		for (i=0; i<curve; i++) {
			free1float(x1curve[i]);
			free1float(x2curve[i]);
		}
		free1float(curvewidth);
		free1int(curvedash);
		free((void**)x1curve);
		free((void**)x2curve);
		free((void**)curvefile);
		free((void**)curvecolor);
	}

	return 0;
}
static void drawimage(int hls, float colors[3][3], 
	int width, int height, int bps, float matrix[], unsigned char* z)
{
	int row,col,rowbytes,hi,lo,zi,byte;
	unsigned char *rtab,*gtab,*btab,*rgb,*rgbp,*crgbp,*zp;

	/* handle gray scale as a special case */
	if ( (hls && 
		colors[L][0]==0.0 && 
		colors[L][1]==0.5 && 
		colors[L][2]==1.0 &&
		colors[S][0]==0.0 &&
		colors[S][1]==0.0 &&
		colors[S][2]==0.0) ||
		(colors[R][0]==0.0 && colors[R][1]==0.5 && colors[R][2]==1.0 &&
		colors[G][0]==0.0 && colors[G][1]==0.5 && colors[G][2]==1.0 &&
		colors[B][0]==0.0 && colors[B][1]==0.5 && colors[B][2]==1.0) ) {
		image(width,height,8,matrix,z);
		return;
	}

	/* allocate space for rgb tables */
	rtab = ealloc1(256,sizeof(unsigned char));
	gtab = ealloc1(256,sizeof(unsigned char));
	btab = ealloc1(256,sizeof(unsigned char));

	/* if colors are hls values, map from hls to rgb tables */
	if (hls) {
		float hscale_low,lscale_low,sscale_low,hbase_low,lbase_low,sbase_low,h,l,s,r,g,b;
		float hscale_hi,lscale_hi,sscale_hi,hbase_hi,lbase_hi,sbase_hi;
		hbase_low = colors[H][0];
		hscale_low = (colors[H][1]-colors[H][0])/127.0;
		lbase_low = colors[L][0];
		lscale_low = (colors[L][1]-colors[L][0])/127.0;
		sbase_low = colors[S][0];
		sscale_low = (colors[S][1]-colors[S][0])/127.0;
		hbase_hi = colors[H][1];
		hscale_hi = (colors[H][2]-colors[H][1])/127.0;
		lbase_hi = colors[L][1];
		lscale_hi = (colors[L][2]-colors[L][1])/127.0;
		sbase_hi = colors[S][1];
		sscale_hi = (colors[S][2]-colors[S][1])/127.0;
		for (zi=0; zi<128; ++zi) {
			h = hbase_low+zi*hscale_low;
			l = lbase_low+zi*lscale_low;
			s = sbase_low+zi*sscale_low;
			hlsrgb(h*360.0,l,s,&r,&g,&b);
			rtab[zi] = r*255.0;
			gtab[zi] = g*255.0;
			btab[zi] = b*255.0;
		}
		for (zi=128; zi<256; ++zi) {
			h = hbase_hi+(zi-128)*hscale_hi;
			l = lbase_hi+(zi-128)*lscale_hi;
			s = sbase_hi+(zi-128)*sscale_hi;
			hlsrgb(h*360.0,l,s,&r,&g,&b);
			rtab[zi] = r*255.0;
			gtab[zi] = g*255.0;
			btab[zi] = b*255.0;
		}
	
	/* else colors are rgb values, map linearly to rgb tables */
	} else {
	  float rscale_low,gscale_low,bscale_low,rbase_low,gbase_low,bbase_low,
	    rscale_hi,gscale_hi,bscale_hi,rbase_hi,gbase_hi,bbase_hi;

	  rbase_low = colors[0][0]*255.0;
	  rscale_low = 2.*(colors[0][1]-colors[0][0]);
	  gbase_low = colors[1][0]*255.0;
	  gscale_low = 2.*(colors[1][1]-colors[1][0]);
	  bbase_low = colors[2][0]*255.0;
	  bscale_low = 2.*(colors[2][1]-colors[2][0]);
	  rbase_hi = colors[0][1]*255.0;
	  rscale_hi = 2.*(colors[0][2]-colors[0][1]);
	  gbase_hi = colors[1][1]*255.0;
	  gscale_hi = 2.*(colors[1][2]-colors[1][1]);
	  bbase_hi = colors[2][1]*255.0;
	  bscale_hi = 2.*(colors[2][2]-colors[2][1]);
	  for (zi=0; zi<128; ++zi) {
	    rtab[zi] = rbase_low+zi*rscale_low;
	    gtab[zi] = gbase_low+zi*gscale_low;
	    btab[zi] = bbase_low+zi*bscale_low;
	  }
	  for (zi=128; zi<256; ++zi) {
	    rtab[zi] = rbase_hi+(zi-128)*rscale_hi;
	    gtab[zi] = gbase_hi+(zi-128)*gscale_hi;
	    btab[zi] = bbase_hi+(zi-128)*bscale_hi;
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

static void drawimage2(int hls, float colors[3][3], int width, int height,
		       int bps, float matrix[], unsigned char* z,
		       unsigned char* y, float bckgr) /* E.T. */
{
  int row,col,rowbytes,hi,lo,zi,yi,byte;
  unsigned char *rgb,*rgbp,*crgbp,*zp;
	float hval, lval, sval, rval, gval, bval;
	float *htab,*ltab,*stab;

	void rgb2hsv(float r, float g, float b, float *h, float *s, float *v);
	void hsv2rgb(float h, float s, float v, float *r, float *g, float *b);
	void rgbtohls(float r, float g, float b, float *h, float *l, float *s);
	void hlstorgb(float h, float l, float s, float *r, float *g, float *b);
	void rgbhls(float r, float g, float b, float *h, float *l, float *s);

	/* handle gray scale as a special case */
	if ( (hls && 
		colors[L][0]==0.0 && 
		colors[L][1]==0.5 && 
		colors[L][2]==1.0 &&
		colors[S][0]==0.0 &&
		colors[S][1]==0.0 &&
		colors[S][2]==0.0) ||
		(colors[R][0]==0.0 && colors[R][1]==0.5 && colors[R][2]==1.0 &&
		colors[G][0]==0.0 && colors[G][1]==0.5 && colors[G][2]==1.0 &&
		colors[B][0]==0.0 && colors[B][1]==0.5 && colors[B][2]==1.0) ) {
		image(width,height,8,matrix,z);
		return;
	}

	/* allocate space for rgb tables */
	htab = ealloc1(256,sizeof(float));
	ltab = ealloc1(256,sizeof(float));
	stab = ealloc1(256,sizeof(float));

	/* if colors are hls values, map from hls to rgb tables */
	if (hls) {
		float hscale_low,lscale_low,sscale_low,hbase_low,lbase_low,sbase_low;
		float hscale_hi,lscale_hi,sscale_hi,hbase_hi,lbase_hi,sbase_hi;
		hbase_low = colors[H][0];
		hscale_low = (colors[H][1]-colors[H][0])/127.0;
		lbase_low = colors[L][0];
		lscale_low = (colors[L][1]-colors[L][0])/127.0;
		sbase_low = colors[S][0];
		sscale_low = (colors[S][1]-colors[S][0])/127.0;
		hbase_hi = colors[H][1];
		hscale_hi = (colors[H][2]-colors[H][1])/127.0;
		lbase_hi = colors[L][1];
		lscale_hi = (colors[L][2]-colors[L][1])/127.0;
		sbase_hi = colors[S][1];
		sscale_hi = (colors[S][2]-colors[S][1])/127.0;
		for (zi=0; zi<128; ++zi) {
			htab[zi] = hbase_low+zi*hscale_low;
			ltab[zi] = lbase_low+zi*lscale_low;
			stab[zi] = sbase_low+zi*sscale_low;
		}
		for (zi=128; zi<256; ++zi) {
			htab[zi] = hbase_hi+(zi-128)*hscale_hi;
			ltab[zi] = lbase_hi+(zi-128)*lscale_hi;
			stab[zi] = sbase_hi+(zi-128)*sscale_hi;
		}
	
	/* else colors are rgb values, map linearly to rgb tables */
	} else {
	  float rscale_low,gscale_low,bscale_low,rbase_low,gbase_low,bbase_low,
	    rscale_hi,gscale_hi,bscale_hi,rbase_hi,gbase_hi,bbase_hi;

	  rbase_low = colors[0][0]*255.0;
	  rscale_low = 2.*(colors[0][1]-colors[0][0]);
	  gbase_low = colors[1][0]*255.0;
	  gscale_low = 2.*(colors[1][1]-colors[1][0]);
	  bbase_low = colors[2][0]*255.0;
	  bscale_low = 2.*(colors[2][1]-colors[2][0]);
	  rbase_hi = colors[0][1]*255.0;
	  rscale_hi = 2.*(colors[0][2]-colors[0][1]);
	  gbase_hi = colors[1][1]*255.0;
	  gscale_hi = 2.*(colors[1][2]-colors[1][1]);
	  bbase_hi = colors[2][1]*255.0;
	  bscale_hi = 2.*(colors[2][2]-colors[2][1]);
	  for (zi=0; zi<128; ++zi) {
	    rval = (rbase_low+zi*rscale_low)/255.;
	    gval = (gbase_low+zi*gscale_low)/255.;
	    bval = (bbase_low+zi*bscale_low)/255.;
	    rgbhls(rval,gval,bval,htab+zi,ltab+zi,stab+zi);
	    *(htab+zi) /= 360.;
	  }
	  for (zi=128; zi<256; ++zi) {
	    rval = (rbase_hi+(zi-128)*rscale_hi)/255.;
	    gval = (gbase_hi+(zi-128)*gscale_hi)/255.;
	    bval = (bbase_hi+(zi-128)*bscale_hi)/255.;
	    rgbhls(rval,gval,bval,htab+zi,ltab+zi,stab+zi);
	    *(htab+zi) /= 360.;
	  }
	}

	/* convert unsigned char to rgb unsigned char */
	rgb = ealloc1(width*height*3,sizeof(unsigned char));
	for (row=0,rgbp=rgb,zp=z,yi=0; row<height; ++row) {
	  for (col=0; col<width; ++col) {
	    zi = *zp++;
	    hval = htab[zi];
	    lval = bckgr+(ltab[zi]-bckgr)*y[yi]/255.;
	    sval = stab[zi] * y[yi]/255.;

	    yi++;
	    hlsrgb(hval*360.,lval,sval,&rval,&gval,&bval);

	    *rgbp++ = (unsigned char)(rval*255.);
	    *rgbp++ = (unsigned char)(gval*255.);
	    *rgbp++ = (unsigned char)(bval*255.);
	  }
	}

	/* free tables */
	free1(htab);
	free1(ltab);
	free1(stab);

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
/* RGB -> HLS  E.T. */
void rgbhls(float r, float g, float b, float *h, float *l, float *s)
/*
 r,g,b: [0.,1.]  h: [0.,360.]  l,s: [0.,1.]
*/
{
  float m1, m2, ddd, sss;

  *s = 0.;
  *h = 0.;

  m1 = r; 
  if (g > m1) m1 = g;
  if (b > m1) m1 = b;

  m2 = r; 
  if (g < m2) m2 = g;
  if (b < m2) m2 = b;

  sss = m1 + m2;
  ddd = m1 - m2;

  *l = sss * 0.5;

  if (ddd > 0.) {

    if (*l <= 0.5) *s = ddd/sss;
    else *s = ddd / (2. - sss);

    if (r == m1) {
      *h = (g-b) / ddd * 60.;
    } else if (g == m1) {
      *h = 120. + (b-r) / ddd * 60.;
    } else {
      *h = 240. + (r-g) / ddd * 60.;
     
      if (*h < 0.) *h += 360.;
    }
  }
}
