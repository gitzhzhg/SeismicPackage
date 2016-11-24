/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSCONTOUR: $Revision: 1.18 $ ; $Date: 2011/11/17 00:10:53 $	*/

#include "par.h"
#include "psplot.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" PSCONTOUR - PostScript CONTOURing of a two-dimensional function f(x1,x2)",
"									",
" pscontour n1= [optional parameters] <binaryfile >postscriptfile	",
"									",
" Required Parameters:							",
" n1                     number of samples in 1st (fast) dimension	",
"									",
" Optional Parameters:							",
" d1=1.0                 sampling interval in 1st dimension		",
" f1=d1                  first sample in 1st dimension			",
" x1=f1,f1+d1,...        array of monotonic sampled values in 1st dimension",
" n2=all                 number of samples in 2nd (slow) dimension	",
" d2=1.0                 sampling interval in 2nd dimension		",
" f2=d2                  first sample in 2nd dimension			",
" x2=f2,f2+d2,...        array of monotonic sampled values in 2nd dimension",
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
" wbox=6.0               width in inches of axes box			",
" hbox=8.0               height in inches of axes box			",
" x1beg=x1min            value at which axis 1 begins			",
" x1end=x1max            value at which axis 1 ends			",
" d1num=0.0              numbered tic interval on axis 1 (0.0 for automatic)",
" f1num=x1min            first numbered tic on axis 1 (used if d1num not 0.0)",
" n1tic=1                number of tics per numbered tic on axis 1	",
" grid1=none             grid lines on axis 1 - none, dot, dash, or solid",
" label1=                label on axis 1				",
" x2beg=x2min            value at which axis 2 begins			",
" x2end=x2max            value at which axis 2 ends			",
" d2num=0.0              numbered tic interval on axis 2 (0.0 for automatic)",
" f2num=x2min            first numbered tic on axis 2 (used if d2num not 0.0)",
" n2tic=1                number of tics per numbered tic on axis 2	",
" grid2=none             grid lines on axis 2 - none, dot, dash, or solid",
" label2=                label on axis 2				",
" labelfont=Helvetica    font name for axes labels			",
" labelsize=18           font size for axes labels			",
" title=                 title of plot					",
" titlefont=Helvetica-Bold font name for title				",
" titlesize=24           font size for title				",
" labelcfont=Helvetica-Bold font name for contour labels		",
" labelcsize=6           font size of contour labels   			",
" labelccolor=black      color of contour labels   			",
" titlecolor=black       color of title					",
" axescolor=black        color of axes					",
" gridcolor=black        color of grid					",
" axeswidth=1            width (in points) of axes			",
" ticwidth=axeswidth     width (in points) of tic marks		",
" gridwidth=axeswidth    width (in points) of grid lines		",
" style=seismic          normal (axis 1 horizontal, axis 2 vertical) or	",
"                        seismic (axis 1 vertical, axis 2 horizontal)	",
"									",
" Note.									",
" The line width of unlabeled contours is designed as a quarter of that	",
" of labeled contours. 							",
"									",
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
" type:   sudoc pscontour    for more information			",
NULL};

/*
 *Notes:
 *
 * For nice even-numbered contours, use the parameters  fc and dc
 *
 * Example: if the range of the z values of a data set range between
 * approximately -1000 and +1000, then use fc=-1000 nc=10 and dc=100
 * to get contours spaced by even 100's.
 */

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 05/29/90
 * MODIFIED:  Craig Artley, Colorado School of Mines, 08/30/91
 *            BoundingBox moved to top of PostScript output
 * MODIFIED:  Zhenyue Liu, Colorado School of Mines, 08/26/93
 *	      Values are labeled on contours  
 * MODIFIED:  Craig Artley, Colorado School of Mines, 12/16/93
 *            Added color options (Courtesy of Dave Hale, Advance Geophysical).
 * Modified: Morten Wendell Pedersen, Aarhus University, 23/3-97
 *           Added ticwidth,axeswidth, gridwidth parameters 
 */

/**************** end self doc ********************************/

/* maximum number of contours */
#define NCMAX 200

int main (int argc, char **argv)
{
	int n1,n2,nc,nplaces,n1tic,n2tic,nfloats,bbox[4],
		i1,i2,iz,ic,npar,grid1,grid2,style;
	float labelsize,titlesize,cwidth[NCMAX],cgray[NCMAX],cdash[NCMAX],
		d1,f1,d2,f2,dc,fc,*x1,*x2,c[NCMAX],*z,zmin,zmax,
		xbox,ybox,wbox,hbox,
		x1beg,x1end,x2beg,x2end,
		x1min,x1max,x2min,x2max,
		d1num,f1num,d2num,f2num,
		x1size,x2size,x1scale,x2scale;
	float labelcsize, lcsize, *w;
	float axeswidth, ticwidth, gridwidth;
	int  labelcf, nlabelc, labelcper;
	char *label1="",*label2="",*title="",
		*labelfont="Helvetica",*titlefont="Helvetica-Bold",
		*labelcfont="Helvetica-Bold",*labelccolor="black",
		*styles="seismic",*grid1s="none",*grid2s="none",
		*titlecolor="black",*axescolor="black",*gridcolor="black",
		*scolor="none",*ccolor[NCMAX];
	FILE *infp=stdin;

	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters describing 1st dimension sampling */
	if ((n1=countparval("x1"))==0 && !getparint("n1",&n1))
		err("Must specify n1!\n");
	x1 = ealloc1float(n1);
	if (!getparfloat("x1",x1)) {
		d1 = 1.0;  getparfloat("d1",&d1);
		f1 = d1;  getparfloat("f1",&f1);
		for (i1=0; i1<n1; i1++)
			x1[i1] = f1+i1*d1;
	}
	for (i1=1,x1min=x1max=x1[0]; i1<n1; i1++) {
		x1min = MIN(x1min,x1[i1]);
		x1max = MAX(x1max,x1[i1]);
	}

	/* get parameters describing 2nd dimension sampling */
	if ((n2=countparval("x2"))==0 && !getparint("n2",&n2)) {
			if (efseeko(infp,(off_t) 0,SEEK_END)!=0)
				err("must specify n2 if in a pipe!\n");
			nfloats = (int) (eftello(infp)/((off_t)sizeof(float)));
			efseeko(infp,(off_t) 0,SEEK_SET);
			n2 = nfloats/n1;
	}
	x2 = ealloc1float(n2);
	if (!getparfloat("x2",x2)) {
		d2 = 1.0;  getparfloat("d2",&d2);
		f2 = d2;  getparfloat("f2",&f2);
		for (i2=0; i2<n2; i2++)
			x2[i2] = f2+i2*d2;
	}
	for (i2=1,x2min=x2max=x2[0]; i2<n2; i2++) {
		x2min = MIN(x2min,x2[i2]);
		x2max = MAX(x2max,x2[i2]);
	}

	/* read binary data to be contoured */
	z = ealloc1float(n1*n2);
	if (fread(z,sizeof(float),n1*n2,infp)!=n1*n2)
		err("error reading input file!\n");

	/* zero w array for contour labeling	*/	
	w = ealloc1float(n1*n2);
	for(i2=0; i2<n2; i2++) {
		for(i1=0,iz=i2*n1; i1<n1; i1++,iz++) {
			w[iz] = 0.;
		}
	}
	
	/* determine data min and max */
	for (i2=0,zmin=zmax=z[0]; i2<n2; i2++) {
		for (i1=0,iz=i2*n1; i1<n1; i1++,iz++) {
			zmin = MIN(zmin,z[iz]);
			zmax = MAX(zmax,z[iz]);
		}
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
	if (!getparint("nplaces",&nplaces))	nplaces = 6;
 
	/* get axes parameters */
	xbox = 1.5; getparfloat("xbox",&xbox);
	ybox = 1.5; getparfloat("ybox",&ybox);
	wbox = 6.0; getparfloat("wbox",&wbox);
	hbox = 8.0; getparfloat("hbox",&hbox);
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

	if(!getparfloat("axeswidth",&axeswidth)) axeswidth=1;
	if (!getparfloat("ticwidth",&ticwidth)) ticwidth=axeswidth;
	if(!getparfloat("gridwidth",&gridwidth)) gridwidth =axeswidth;;

	getparstring("style",&styles);
        checkpars();

	if (STREQ("normal",styles))
		style = NORMAL;
	else
		style = SEISMIC;

	/* convert axes box parameters from inches to points */
	xbox *= 72.0;
	ybox *= 72.0;
	wbox *= 72.0;
	hbox *= 72.0;

	/* set bounding box */
	psAxesBBox(
		xbox,ybox,wbox,hbox,
		labelfont,labelsize,
		titlefont,titlesize,
		style,bbox);
	boundingbox(bbox[0],bbox[1],bbox[2],bbox[3]);

	/* begin PostScript */
	begineps();

	/* save graphics state */
	gsave();

	/* set clip */
	rectclip(xbox,ybox,wbox,hbox);

	/* determine axes sizes */
	x1size = (style==NORMAL)?wbox:hbox;
	x2size = (style==NORMAL)?hbox:wbox;

	/* translate coordinate system by box offset */
	translate(xbox,ybox);

	/* if style is not normal, rotate coordinate system */
	if (style!=NORMAL) {
		rotate(-90.0);
		translate(-hbox,0.0);
	}

	/* determine x1 and x2 scale factors */
	x1scale = x1size/(x1end-x1beg);
	x2scale = x2size/(x2end-x2beg);

	/* translate coordinate system by beginning axes values */
	translate(-x1beg*x1scale,-x2beg*x2scale);

	/* scale x1 and x2 coordinates */
	for (i1=0; i1<n1; i1++)
		x1[i1] *= x1scale;
	for (i2=0; i2<n2; i2++)
		x2[i2] *= x2scale;

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

	/* restore graphics state */
	grestore();

	/* draw axes and title */
	psAxesBox(
		xbox,ybox,wbox,hbox,
		x1beg,x1end,0.0,0.0,
		d1num,f1num,n1tic,grid1,label1,
		x2beg,x2end,0.0,0.0,
		d2num,f2num,n2tic,grid2,label2,
		labelfont,labelsize,
		title,titlefont,titlesize,
		titlecolor,axescolor,gridcolor,
		ticwidth,axeswidth,gridwidth,
		style);

	/* end PostScript */
	showpage();
	endeps();

	return 0;
}
