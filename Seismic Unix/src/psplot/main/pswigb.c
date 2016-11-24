/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSWIGB: $Revision: 1.29 $ ; $Date: 2011/11/17 00:10:53 $	*/

#include "par.h"
#include "psplot.h"


/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" PSWIGB - PostScript WIGgle-trace plot of f(x1,x2) via Bitmap		",
" Best for many traces.  Use PSWIGP (Polygon version) for few traces.	",
" 									",
" pswigb n1= [optional parameters] <binaryfile >postscriptfile		",
" 									",
" Required Parameters:							",
" n1                     number of samples in 1st (fast) dimension	",
" 									",
" Optional Parameters:							",
" d1=1.0                 sampling interval in 1st dimension		",
" f1=0.0                 first sample in 1st dimension			",
" n2=all                 number of samples in 2nd (slow) dimension	",
" d2=1.0                 sampling interval in 2nd dimension		",
" f2=0.0                 first sample in 2nd dimension			",
" x2=f2,f2+d2,...        array of sampled values in 2nd dimension	",
" bias=0.0               data value corresponding to location along axis 2",
" perc=100.0             percentile for determining clip		",
" clip=(perc percentile) data values < bias+clip and > bias-clip are clipped",
" xcur=1.0               wiggle excursion in traces corresponding to clip",
" wt=1                   =0 for no wiggle-trace; =1 for wiggle-trace	",
" va=1                   =0 for no variable-area; =1 for variable-area fill",
"                        =2 for variable area, solid/grey fill          ",
"                        SHADING: 2<= va <=5  va=2 lightgrey, va=5 black", 
" nbpi=72                number of bits per inch at which to rasterize	",
" verbose=1              =1 for info printed on stderr (0 for no info)	",
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
" style=seismic          normal (axis 1 horizontal, axis 2 vertical) or	",
"                        seismic (axis 1 vertical, axis 2 horizontal)	",
" interp=0		 no display interpolation			",
"			 =1 use 8 point sinc interpolation		",
" curve=curve1,curve2,...  file(s) containing points to draw curve(s)   ",
" npair=n1,n2,n2,...            number(s) of pairs in each file         ",
" curvecolor=black,..    color of curve(s)                              ",
" curvewidth=axeswidth   width (in points) of curve(s)                  ",
" curvedash=0            solid curve(s), dash indices 1,...,11 produce  ",
"                        curve(s) with various dash styles              ",
" 									",
" Notes: 								",
" The interp option may be useful for high nbpi values, however, it	",
" tacitly assumes that the data are purely oscillatory.	Non-oscillatory	",
" data will not be represented correctly when this option is set.	",
" 									",
" The curve file is an ascii file with the points specified as x1 x2	",
" pairs, one pair to a line.  A \"vector\" of curve files and curve	",
" colors may be specified as curvefile=file1,file2,etc. and 		",
" curvecolor=color1,color2,etc, and the number of pairs of values in each",
" file as npair=npair1,npair2,... .					",
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
"									",
NULL};
/**************** end self doc ********************************/

/*
 *
 * AUTHOR:  Dave Hale, Colorado School of Mines, 05/29/90
 * MODIFIED:  Craig Artley, Colorado School of Mines, 08/30/91
 *          BoundingBox moved to top of PostScript output
 * MODIFIED:  Craig Artley, Colorado School of Mines, 12/16/93
 *          Added color options (Courtesy of Dave Hale, Advance Geophysical).
 * Modified: Morten Wendell Pedersen, Aarhus University, 23/3-97
 *           Added ticwidth,axeswidth, gridwidth parameters 
 * MODIFIED:  Brian K. Macy, Phillips Petroleum, 01/14/99
 *            Added curve plotting option
 * MODIFIED:  P. Michaels, Boise State Univeristy  29 December 2000
 *            Added solid/grey color scheme for peaks/troughs
 * MODIFIED: Ekkehart Tessmer, University of Hamburg, Germany, 08/22/2007
 *          Added dashing option to curve plotting
 */

int main (int argc, char **argv)
{
	int n1,n2,nbpi,n1tic,n2tic,nfloats,wt,va,bbox[4],
		i2,grid1,grid2,style,
		n1bits,n2bits,nbpr,i1beg,i1end,if1r,n1r,
		b1fz,b1lz,n2in,nz,iz,i,nbytes,verbose,endian=CWPENDIAN, interp;
	float labelsize,titlesize,bias,perc,clip,xcur,
		d1,f1,d2,f2,*x2,*z,
		xbox,ybox,wbox,hbox,
		x1beg,x1end,x2beg,x2end,
		x1min,x1max,x2min,x2max,
		d1num,f1num,d2num,f2num,
		*temp,p2beg,p2end,matrix[6],
		bscale,boffset,bxcur,bx2;
	float axeswidth, ticwidth, gridwidth;
	unsigned char *bits;
	char *label1="",*label2="",*title="",
		*labelfont="Helvetica",*titlefont="Helvetica-Bold",
		*styles="seismic",*grid1s="none",*grid2s="none",
		*titlecolor="black",*axescolor="black",*gridcolor="black";
	FILE *infp=stdin;

	float **x1curve=NULL,**x2curve=NULL,*curvewidth=NULL;
	int j,curve=0,*npair=NULL,ncurvecolor=0,ncurvewidth=0,ncurvedash=0,*curvedash=NULL;
	char **curvecolor=NULL,**curvefile=NULL;
	FILE *curvefp=NULL;
	cwp_Bool is_curve = cwp_false;


	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters describing 1st dimension sampling */
	if (!getparint("n1",&n1))
		err("Must specify number of samples in 1st dimension!\n");
	d1 = 1.0;  getparfloat("d1",&d1);
	f1 = 0.0;  getparfloat("f1",&f1);
	x1min = (d1>0.0)?f1:f1+(n1-1)*d1;
	x1max = (d1<0.0)?f1:f1+(n1-1)*d1;

	/* get parameters describing 2nd dimension sampling */
	if ((n2=countparval("x2"))==0 && !getparint("n2",&n2)) {
			if (efseeko(infp,(off_t) 0,SEEK_END)!=0)
				err("must specify n2 if in a pipe!\n");
			nfloats =(int) (eftello(infp)/((off_t) sizeof(float)));
			efseeko(infp,(off_t) 0,SEEK_SET);
			n2 = nfloats/n1;
	}
	x2 = ealloc1float(n2);
	if (!getparfloat("x2",x2)) {
		d2 = 1.0;  getparfloat("d2",&d2);
		f2 = 0.0;  getparfloat("f2",&f2);
		for (i2=0; i2<n2; i2++)
			x2[i2] = f2+i2*d2;
	}
	for (i2=1,x2min=x2max=x2[0]; i2<n2; i2++) {
		x2min = MIN(x2min,x2[i2]);
		x2max = MAX(x2max,x2[i2]);
	}

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
	} else if (ncurvecolor){
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

	/* read binary data to be plotted */
	nz = n1*n2;
	z = ealloc1float(nz);
	if (fread(z,sizeof(float),nz,infp)!=nz)
		err("error reading input file!\n");

	/* if necessary, subtract bias */
	if (getparfloat("bias",&bias) && bias!=0.0)
		for (iz=0; iz<nz; iz++)
			z[iz] -= bias;

	/* if necessary, determine clip from percentile */
	if (!getparfloat("clip",&clip)) {
		perc = 100.0;  getparfloat("perc",&perc);
		temp = ealloc1float(nz);
		for (iz=0; iz<nz; iz++)
			temp[iz] = fabs(z[iz]);
		iz = (nz*perc/100.0);
		if (iz<0) iz = 0;
		if (iz>nz-1) iz = nz-1;
		qkfind(iz,nz,temp);
		clip = temp[iz];
		free1float(temp);
	}
	verbose = 1;  getparint("verbose",&verbose);
	if (verbose) warn("clip=%g\n",clip);

	/* get wiggle-trace-variable-area parameters */
	wt = 1;  getparint("wt",&wt);
	va = 1;  getparint("va",&va);

	/* set wt=va for va with solid/grey coloring  */
	if (va>=2) 
	{  wt=va; va=1; }

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

	/* determine bitmap dimensions and allocate space for bitmap */
	nbpi = 72;  getparint("nbpi",&nbpi);
	n1bits = nbpi*((style==NORMAL)?wbox:hbox);
	n2bits = nbpi*((style==NORMAL)?hbox:wbox);
	nbpr = 1+(n2bits-1)/8;
	bits = ealloc1(nbpr*n1bits,sizeof(unsigned char));
	for (i=0,nbytes=nbpr*n1bits; i<nbytes; i++)
		bits[i] = 0;

	/* determine number of traces that fall within axis 2 bounds */
	x2min = MIN(x2beg,x2end);
	x2max = MAX(x2beg,x2end);
	for (i2=0,n2in=0; i2<n2; i2++)
		if (x2[i2]>=x2min && x2[i2]<=x2max) n2in++;

	/* determine pads for wiggle excursion along axis 2 */
	xcur = 1.0;  getparfloat("xcur",&xcur);
	xcur = fabs(xcur);
	if (n2in>1) xcur *= (x2max-x2min)/(n2in-1);
	p2beg = (x2end>=x2beg)?-xcur:xcur;
	p2end = (x2end>=x2beg)?xcur:-xcur;

	/* set interpolation flag */
	if (!getparint("interp",&interp))	interp = 0;

        checkpars();

	/* determine scale and offset to map x2 units to bitmap units */
	bscale = (n2bits-1)/(x2end+p2end-x2beg-p2beg);
	boffset = -(x2beg+p2beg)*bscale;
	bxcur = xcur*bscale;

	/* adjust x1beg and x1end to fall on sampled values */
	i1beg = NINT((x1beg-f1)/d1);
	i1beg = MAX(0,MIN(n1-1,i1beg));
	x1beg = f1+i1beg*d1;
	i1end = NINT((x1end-f1)/d1);
	i1end = MAX(0,MIN(n1-1,i1end));
	x1end = f1+i1end*d1;

	/* determine first sample and number of samples to rasterize */
	if1r = MIN(i1beg,i1end);
	n1r = MAX(i1beg,i1end)-if1r+1;

	/* determine bits corresponding to first and last samples */
	b1fz = (x1end>x1beg)?0:n1bits-1;
	b1lz = (x1end>x1beg)?n1bits-1:0;

	/* rasterize traces */
	for (i2=0; i2<n2; i2++,z+=n1) {

		/* skip traces not in bounds */
		if (x2[i2]<x2min || x2[i2]>x2max) continue;

		/* determine bitmap coordinate of trace */
		bx2 = boffset+x2[i2]*bscale;

		/* rasterize one trace */
		if (interp==0) { /* no sinc interpolation */
			rfwtva(n1r,&z[if1r],-clip,clip,va?0:clip,
				(int)(bx2-bxcur),(int)(bx2+bxcur),b1fz,b1lz,
				wt,nbpr,bits,endian);
		} else { /* do sinc interpolation */
			rfwtvaint(n1r,&z[if1r],-clip,clip,va?0:clip,
				(int)(bx2-bxcur),(int)(bx2+bxcur),b1fz,b1lz,
				wt,nbpr,bits,endian);
		}
	}

	/* invert bitmap for PostScript (for which black=0, white=1) */
	for (i=0,nbytes=nbpr*n1bits; i<nbytes; i++) {
		if (endian==0) { /* little endian bitmap inversion */
			bits[i]=
				(bits[i]&((unsigned char) 128))>>7 |
				(bits[i]&((unsigned char) 64))>>5 |
				(bits[i]&((unsigned char) 32))>>3 |
				(bits[i]&((unsigned char) 16))>>1 |
				(bits[i]&8)<<1 | (bits[i]&4)<<3 |
				(bits[i]&2)<<5 | (bits[i]&1)<<7;
		}

		bits[i] = ~bits[i];
	}


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

	/* translate coordinate system by box offset */
	translate(xbox,ybox);

	/* determine image matrix */
	if (style==NORMAL) {
		matrix[0] = 0;  matrix[1] = n1bits;  matrix[2] = n2bits;
		matrix[3] = 0;  matrix[4] = 0;  matrix[5] = 0;
	} else {
		matrix[0] = n2bits;  matrix[1] = 0;  matrix[2] = 0;
		matrix[3] = -n1bits;  matrix[4] = 0;  matrix[5] = n1bits;
	}

	scale(wbox,hbox);

	/* draw the image (before axes so grid lines are visible) */
	image(n2bits,n1bits,1,matrix,bits);

	/* restore graphics state */
	grestore();

	/* draw curve */
	for (i=0; i<curve; i++) {
		gsave();
		psDrawCurve(
			xbox,ybox,wbox,hbox,
			x1beg,x1end,0.0,0.0,
			x2beg,x2end,p2beg,p2end,
			x1curve[i],x2curve[i],npair[i],
			curvecolor[i],curvewidth[i],curvedash[i],style);
		grestore();
	}

	/* draw axes and title */
	psAxesBox(
		xbox,ybox,wbox,hbox,
		x1beg,x1end,0.0,0.0,
		d1num,f1num,n1tic,grid1,label1,
		x2beg,x2end,p2beg,p2end,
		d2num,f2num,n2tic,grid2,label2,
		labelfont,labelsize,
		title,titlefont,titlesize,
		titlecolor,axescolor,gridcolor,
		ticwidth,axeswidth,gridwidth,
		style);

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
