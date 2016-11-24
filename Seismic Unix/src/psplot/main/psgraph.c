/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSGRAPH: $Revision: 1.40 $ ; $Date: 2011/11/17 00:10:53 $	*/

#include "par.h"
#include "psplot.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" PSGRAPH - PostScript GRAPHer						",
" Graphs n[i] pairs of (x,y) coordinates, for i = 1 to nplot.		",
"									",
" psgraph n= [optional parameters] <binaryfile >postscriptfile		",
"									",
" Required Parameters:							",
" n                      array containing number of points per plot	",
"									",
" Data formats supported:						",
"	1.a. x1,y1,x2,y2,...,xn,yn					",
"	  b. x1,x2,...,xn,y1,y2,...,yn (must set pairs=0)		",
"	2.   y1,y2,...,yn (must give non-zero d1[]=)			",
"	3.   x1,x2,...,xn (must give non-zero d2[]=)			",
"	4.   nil (must give non-zero d1[]= and non-zero d2[]=)		",
"  The formats may be repeated and mixed in any order, but if		",
"  formats 2-4 are used, the d1 and d2 arrays must be specified including",
"  d1[]=0.0 d2[]=0.0 entries for any internal occurences of format 1.	",
"  Similarly, the pairs array must contain place-keeping entries for	",
"  plots of formats 2-4 if they are mixed with both formats 1.a and 1.b.",
"  Also, if formats 2-4 are used with non-zero f1[] or f2[] entries, then",
"  the corresponding array(s) must be fully specified including f1[]=0.0",
"  and/or f2[]=0.0 entries for any internal occurences of format 1 or	",
"  formats 2-4 where the zero entries are desired.			",
"									",
"  Available colors are all the common ones and many more. The complete	",
"  list of 68 colors is in the file $CWPROOT/src/psplot/basic.c.	",
"									",
" Optional Parameters:							",
" nplot=number of n's    number of plots				",
" d1=0.0,...             x sampling intervals (0.0 if x coordinates input)",
" f1=0.0,...             first x values (not used if x coordinates input)",
" d2=0.0,...             y sampling intervals (0.0 if y coordinates input)",
" f2=0.0,...             first y values (not used if y coordinates input)",
" pairs=1,...            =1 for data pairs in format 1.a, =0 for format 1.b",
" linewidth=1.0,...      line widths (in points) (0.0 for no lines)	",
" linegray=0.0,...       line gray levels (black=0.0 to white=1.0)	",
" linecolor=none,...     line colors; none means use linegray		",
"                        Typical use: linecolor=red,yellow,blue,...	",
" lineon=1.0,...         length of line segments for dashed lines (in points)",
" lineoff=0.0,...        spacing between dashes (0.0 for solid line)	",
" mark=0,1,2,3,...       indices of marks used to represent plotted points",
" marksize=0.0,0.0,...   size of marks (0.0 for no marks)		",
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
" titlecolor=black       color of title					",
" axescolor=black        color of axes					",
" gridcolor=black        color of grid					",
" axeswidth=1            width (in points) of axes			",
" ticwidth=axeswidth     width (in points) of tic marks		",
" gridwidth=axeswidth    width (in points) of grid lines		",
" style=normal           normal (axis 1 horizontal, axis 2 vertical) or	",
"                        seismic (axis 1 vertical, axis 2 horizontal)	",
" reverse=0              =1 to reverse sequence of plotting curves      ",             /* JGHACK */
" Note:	n1 and n2 are acceptable aliases for n and nplot, respectively.	",
"									",
" mark index:                                                           ",
" 1. asterisk                                                           ",
" 2. x-cross                                                            ",
" 3. open triangle                                                      ",
" 4. open square                                                        ",
" 5. open circle                                                        ",
" 6. solid triangle                                                     ",
" 7. solid square                                                       ",
" 8. solid circle                                                       ",
"									",
" All color specifications may also be made in X Window style Hex format",
" example:   axescolor=#255						",
"									",
" Example:								",
" psgraph n=50,100,20 d1=2.5,1,0.33 <datafile >psfile			",
"  plots three curves with equally spaced x values in one plot frame	",
"  x1-coordinates are x1(i) = f1+i*d1 for i = 1 to n (f1=0 by default)	",
"  number of x2's and then x2-coordinates for each curve are read	",
"  sequentially from datafile.						",
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
NULL};
/**************** end self doc ********************************/

/*
AUTHOR:  Dave Hale, Colorado School of Mines, 05/29/90
MODIFIED:  Jack K. Cohen 11/23/90 for different input data format
MODIFIED:  Lydia Deng 06/07/91 for dashed lines
MODIFIED:  Craig Artley, Colorado School of Mines, 08/30/91
           BoundingBox moved to top of PostScript output
           Added optional paired data sub-format
MODIFIED:  Dave Hale, Colorado School of Mines, 11/05/91
           Set PostScript line join to always use beveled joins.
MODIFIED:  John Stockwell, CSM, 21 August 1992, removed the info
	   in this comment block from the selfdoc to prevent sdoc
	   overflow error.
MODIFIED:   John Stockwell, CSM, 28 OCT 1992, new selfdoc strategy implemented.
MODIFIED:  Craig Artley, Colorado School of Mines, 12/16/93
           Added color options (Courtesy of Dave Hale, Advance Geophysical).
Modified: Morten Wendell Pedersen, Aarhus University, 23/3-97
          Added ticwidth,axeswidth, gridwidth parameters 
MODIFIED: James Gunning CSIRO, added reversing option
*/

#define NPMAX 5000	/* Arbitrary maximum number of plots allowed	*/

int main (int argc, char **argv)
{
	int nplot,n[NPMAX],nn,iplot,n1tic,n2tic,nd1,nf1,nd2,nf2,npairs,
		mark[NPMAX],pairs[NPMAX],grid1,grid2,style,npar,bbox[4];
	register int i;
	float labelsize,titlesize,
		linewidth[NPMAX],linegray[NPMAX],lineon[NPMAX],lineoff[NPMAX],
		marksize[NPMAX],d1[NPMAX],f1[NPMAX],d2[NPMAX],f2[NPMAX],
		x1beg,x2beg,x1end,x2end,xbox,ybox,wbox,hbox,dash[2],
		x1min,x1max,x2min,x2max, d1num,f1num,d2num,f2num,
		xsize,ysize,xscale,yscale;
	float axeswidth, ticwidth, gridwidth;
	char *label1="",*label2="",*title="",
		*labelfont="Helvetica",*titlefont="Helvetica-Bold",
		*styles="normal",*grid1s="none",*grid2s="none",
		*titlecolor="black",*axescolor="black",*gridcolor="black",
		*scolor="none",*linecolor[NPMAX];
	float **x1data, **x2data;
	
	int reverse=0,plot_direction=1;         /* JGHACK */
	int nplot_start, nplot_end;             /* JGHACK */

	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters needed to interpret datafile */
	for (i=0; i<NPMAX; i++) {
		pairs[i] = 1;
		d1[i] = 0.0;
		f1[i] = 0.0;
		d2[i] = 0.0;
		f2[i] = 0.0;
	}
	npairs = getparint("pairs",pairs);
	nd1 = getparfloat("d1",d1);
 	nf1 = getparfloat("f1",f1);
	nd2 = getparfloat("d2",d2);
 	nf2 = getparfloat("f2",f2);

	/* get plotting order JGHACK */
	if (getparint("reverse", &reverse) && (reverse==1)) plot_direction=-1; /* JGHACK */


	if (!(nn = getparint("n",n)))  nn = getparint("n1",n);
	if (nn==0)  err("Must specify n, the number of points per plot!");
	nplot = nn; getparint("n2",&nplot); getparint("nplot",&nplot);
	if (nplot > NPMAX)  err("too many plots");
	for (i=nn; i<nplot; ++i)
		n[i] = n[nn-1];
	if (npairs>0)
		for (i=npairs; i<nplot; ++i)
			pairs[i] = pairs[npairs-1];
	if (nd1>0)
		for (i=nd1; i<nplot; ++i)
			d1[i] = d1[nd1-1];
	if (nf1>0)
		for (i=nf1; i<nplot; ++i)
			f1[i] = f1[nf1-1];
	if (nd2>0)
		for (i=nd2; i<nplot; ++i)
			d2[i] = d2[nd2-1];
	if (nf2>0)
		for (i=nf2; i<nplot; ++i)
			f2[i] = f2[nf2-1];

	/* read, regularize and compute extreme values of data */
	x1data = (float **)ealloc1(nplot, sizeof(float*));
	x2data = (float **)ealloc1(nplot, sizeof(float*));
	x2max = x1max = -FLT_MAX;
	x2min = x1min =  FLT_MAX;
	for (iplot=0; iplot<nplot; ++iplot) {
		register int npoint = n[iplot];

		x1data[iplot] = ealloc1float(npoint);
		x2data[iplot] = ealloc1float(npoint);

		/* read data for this plot */
		if (d1[iplot] && d2[iplot]) { /* straight line */
			register int i;
			register float *px1data=x1data[iplot];
			register float *px2data=x2data[iplot];
			float x1,x2;

			for (i=0; i<npoint; ++i) {
				x1 = f1[iplot] + i*d1[iplot];
				x2 = f2[iplot] + i*d2[iplot];
				x1max = MAX(x1, x1max);
				x1min = MIN(x1, x1min);
				x2max = MAX(x2, x2max);
				x2min = MIN(x2, x2min);
				*px1data++ = x1;
				*px2data++ = x2;
			}
		} else if (d1[iplot]) { /* equally spaced x1's */
			register int i;
			register float *px1data=x1data[iplot];
			register float *px2data=x2data[iplot];
			float x1,x2;

			for (i=0; i<npoint; ++i) {
				if (efread(&x2, FSIZE, 1, stdin)!=1)
					err("Error reading input!\n");
				x1 = f1[iplot] + i*d1[iplot];
				x1max = MAX(x1, x1max);
				x1min = MIN(x1, x1min);
				x2max = MAX(x2, x2max);
				x2min = MIN(x2, x2min);
				*px1data++ = x1;
				*px2data++ = x2;
			}
		} else if (d2[iplot]) { /* equally spaced x2's */
			register int i;
			register float *px1data=x1data[iplot];
			register float *px2data=x2data[iplot];
			float x1,x2;

			for (i=0; i<npoint; ++i) {
				if (efread(&x1, FSIZE, 1, stdin)!=1)
					err("Error reading input!\n");
				x2 = f2[iplot] + i*d2[iplot];
				x1max = MAX(x1, x1max);
				x1min = MIN(x1, x1min);
				x2max = MAX(x2, x2max);
				x2min = MIN(x2, x2min);
				*px1data++ = x1;
				*px2data++ = x2;
			}
		} else { /* pairs */
			register int i;
			register float *px1data=x1data[iplot];
			register float *px2data=x2data[iplot];
			float x1,x2;

			if (pairs[iplot]) { /* x1,y1,x2,y2,...,xn,yn */
				for (i=0; i<npoint; ++i) {
					if (efread(&x1, FSIZE, 1, stdin)!=1)
						err("Error reading input!\n");
					if (efread(&x2, FSIZE, 1, stdin)!=1)
						err("Error reading input!\n");
					x1max = MAX(x1, x1max);
					x1min = MIN(x1, x1min);
					x2max = MAX(x2, x2max);
					x2min = MIN(x2, x2min);
					*px1data++ = x1;
					*px2data++ = x2;
				}

			} else { /* x1,x2,...,xn,y1,y2,...,yn */
				for (i=0; i<npoint; ++i) {
					if (efread(&x1, FSIZE, 1, stdin)!=1)
						err("Error reading input!\n");
					x1max = MAX(x1, x1max);
					x1min = MIN(x1, x1min);
					*px1data++ = x1;
				}
				for (i=0; i<npoint; ++i) {
					if (efread(&x2, FSIZE, 1, stdin)!=1)
						err("Error reading input!\n");
					x2max = MAX(x2, x2max);
					x2min = MIN(x2, x2min);
					*px2data++ = x2;
				}
			}
		}
	}

	/* cope with special cases */
	if (x1min==FLT_MAX) x1min = x1max = 0.0;
	if (x2min==FLT_MAX) x2min = x2max = 0.0;
	if (x1min == x1max) {
		x1min -= 1.0;
		x1max += 1.0;
	}
	if (x2min == x2max) {
		x2min -= 1.0;
		x2max += 1.0;
	}

	/* get plotting parameters */
	getparstring("label1",&label1);
	getparstring("label2",&label2);
	getparstring("title",&title);
	getparstring("style",&styles);
	if (STREQ("normal",styles))
		style = NORMAL;
	else
		style = SEISMIC;
	getparstring("labelfont",&labelfont);
	getparstring("titlefont",&titlefont);
	labelsize = 18.0; getparfloat("labelsize",&labelsize);
	titlesize = 24.0; getparfloat("titlesize",&titlesize);
	getparstring("titlecolor",&titlecolor);
	getparstring("axescolor",&axescolor);
	getparstring("gridcolor",&gridcolor);
	xbox = 1.5; getparfloat("xbox",&xbox);
	ybox = 1.5; getparfloat("ybox",&ybox);
	wbox = 6.0; getparfloat("wbox",&wbox);
	hbox = 8.0; getparfloat("hbox",&hbox);

	x1beg = x1min; getparfloat("x1beg",&x1beg);
	x1end = x1max; getparfloat("x1end",&x1end);
	x2beg = x2min; getparfloat("x2beg",&x2beg);
	x2end = x2max; getparfloat("x2end",&x2end);
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
	for (i=0; i<nplot; i++) {
		mark[i] = i%9;
		marksize[i] = 0.0;
		linewidth[i] = 1.0;
		lineon[i] = 1.0;
		lineoff[i] = 0.0;
		linegray[i] = 0.0;
		linecolor[i] = scolor;
	}
	if ((npar=getparint("mark",mark)))
		for (i=npar; i<nplot; ++i)  mark[i] = mark[npar-1];
	if ((npar=getparfloat("marksize",marksize)))
		for (i=npar; i<nplot; ++i)  marksize[i] = marksize[npar-1];
	if ((npar=getparfloat("linewidth",linewidth)))
		for (i=npar; i<nplot; ++i)  linewidth[i] = linewidth[npar-1];
	if ((npar=getparfloat("lineon",lineon)))
		for (i=npar; i<nplot; ++i)  lineon[i] = lineon[npar-1];
	if ((npar=getparfloat("lineoff",lineoff)))
		for (i=npar; i<nplot; ++i)  lineoff[i] = lineoff[npar-1];
	if ((npar=getparfloat("linegray",linegray)))
		for (i=npar; i<nplot; ++i)  linegray[i] = linegray[npar-1];
	if (getparstring("linecolor",&scolor)) {
		int i,j;  char *s;
		for (i=0,s=strtok(scolor,","); s!=NULL; ++i,s=strtok(NULL,","))
			linecolor[i] = s;
		for (j=i-1; i<nplot; ++i)
			linecolor[i] = linecolor[j];
	}
	if(!getparfloat("axeswidth",&axeswidth)) axeswidth=1;
	if (!getparfloat("ticwidth",&ticwidth)) ticwidth=axeswidth;
	if(!getparfloat("gridwidth",&gridwidth)) gridwidth =axeswidth;;

        checkpars();

	/* convert box parameters from inches to points */
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
	xsize = (style==NORMAL)?wbox:hbox;
	ysize = (style==NORMAL)?hbox:wbox;

	/* translate coordinate system by box offset */
	translate(xbox,ybox);

	/* if style is not normal, rotate coordinate system */
	if (style!=NORMAL) {
		rotate(-90.0);
		translate(-hbox,0.0);
	}
	
	/* always use beveled joins of line segments */
	setlinejoin(2);

	/* determine x and y scale factors */
	xscale = xsize/(x1end-x1beg);
	yscale = ysize/(x2end-x2beg);

	/* draw the plots */
	/* Plot in fwd or reverse order. JGHACK ......here...........*/
	if (plot_direction==1) {
	  nplot_start=0;
	  nplot_end=nplot-1;
	} else {
	  nplot_start=nplot-1;
	  nplot_end=0;
	}
	for (iplot=nplot_start; (plot_direction==1) ? (iplot<=nplot_end) : (iplot>=nplot_end); 
			   iplot+=plot_direction) {
	        /* ......to here */
	        register int j;
		register int ni = n[iplot];
		register float *px1data = x1data[iplot];
		register float *px2data = x2data[iplot];
		float *x1 = ealloc1float(ni);
		float *x2 = ealloc1float(ni);

		/* translate and scale */
		for (j=0; j<ni; ++j) {
			x1[j] = (*px1data++ - x1beg)*xscale;
			x2[j] = (*px2data++ - x2beg)*yscale;
		}

		/* plot */
		gsave();
		if (linewidth[iplot]!=0.0) {
			setlinewidth(linewidth[iplot]);
			if (lineoff[iplot]!=0.0){
				dash[0] = lineon[iplot];
				dash[1] = lineoff[iplot];
				setdash(dash,2,0.0);
			} else {
				setdash(dash,0,0.0);
			}
			if (strcmp(linecolor[iplot],"none"))
				setcolor(linecolor[iplot]);
			else
				setgray(linegray[iplot]);
			polyline(x1,x2,ni);
		}
		if (marksize[iplot]!=0.0) {
			if (strcmp(linecolor[iplot],"none"))
				setcolor(linecolor[iplot]);
			else
				setgray(linegray[iplot]);
			for (j=0; j<ni; ++j)
			    markto(x1[j],x2[j],mark[iplot],marksize[iplot]);
		}
		grestore();

		free1float(x1);
		free1float(x2);
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
