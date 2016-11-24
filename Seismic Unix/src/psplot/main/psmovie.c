/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSMOVIE: $Revision: 1.16 $ ; $Date: 2011/11/17 00:10:53 $	*/

#include "par.h"
#include "psplot.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" PSMOVIE - PostScript MOVIE plot of a uniformly-sampled function f(x1,x2,x3)",
" 									",
" psmovie n1= [optional parameters] <binaryfile >postscriptfile		",
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
" perc=100.0             percentile used to determine clip		",
" clip=(perc percentile) clip used to determine bclip and wclip		",
" bperc=perc             percentile for determining black clip value	",
" wperc=100.0-perc       percentile for determining white clip value	",
" bclip=clip             data values outside of [bclip,wclip] are clipped",
" wclip=-clip            data values outside of [bclip,wclip] are clipped",
" d1s=1.0                factor by which to scale d1 before imaging	",
" d2s=1.0                factor by which to scale d2 before imaging	",
" verbose=1              =1 for info printed on stderr (0 for no info)	",
" xbox=1.0               offset in inches of left side of axes box	",
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
" style=seismic          normal (axis 1 horizontal, axis 2 vertical) or	",
"                        seismic (axis 1 vertical, axis 2 horizontal)	",
" n3=1                   number of samples in third dimension		",
" title2=                second title to annotate different frames	",
" loopdsp=3              display loop type (1=loop over n1; 2=loop over n2;",
"                                           3=loop over n3)		",
" d3=1.0                 sampling interval in 3rd dimension		",
" f3=d3                  first sample in 3rd dimension			",
" 									",
" NeXT: view movie via:   psmovie < infile n1= [optional params...] | open",
" Note: currently only the Preview Application can handle the multipage  ",
"       PostScript output by this program.				",
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
 * AUTHORS:  Dave Hale & Zhiming Li, Colorado School of Mines, 07/01/90
 * MODIFIED:  Craig Artley, Colorado School of Mines, 08/30/91
 *           BoundingBox moved to top of PostScript output
 */

int
main (int argc, char **argv)
{
	int n1,n2,n1tic,n2tic,nfloats,bbox[4],
		i1,i2,grid1,grid2,style,
		i3,n3,notitle2=0,
		n1c,n2c,n1s,n2s,i1beg,i1end,i2beg,i2end,i1c,i2c,
		n1dsp=0,n2dsp=0,n3dsp=0,loopdsp,
		nz,iz,i1step,i2step,verbose;
	float labelsize,titlesize,perc,clip,bperc,wperc,bclip,wclip,
		d1,f1,d2,f2,*z,*temp,zscale,zoffset,zi,
		f3,d3,f3s,
		xbox,ybox,wbox,hbox,
		x1beg,x1end,x2beg,x2end,
		x1min,x1max,x2min,x2max,
		d1num,f1num,d2num,f2num,
		p1beg,p1end,p2beg,p2end,matrix[6],
		d1s,d2s;
	unsigned char *cz,*czp,*sz;
	char *label1="",*label2="",*title="",*title2="",
		*labelfont="Helvetica",*titlefont="Helvetica-Bold",
		*styles="seismic",*grid1s="none",*grid2s="none";
	char title2s[80],c80[80];
	FILE *infp=stdin;

	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters describing 1st dimension sampling */
	if (!getparint("n1",&n1)) err("must specify n1!\n");
	if (!getparfloat("d1",&d1)) d1 = 1.0;
	if (!getparfloat("f1",&f1)) f1 = 0.0;

	/* get parameters describing 2nd dimension sampling */
	if (!getparint("n2",&n2)) {
		if (efseeko(infp,(off_t) 0,SEEK_END)!=0)
			err("must specify n2 if in a pipe!\n");
		nfloats = (int) (eftello(infp)/((off_t) sizeof(float)));
		efseeko(infp,(off_t) 0,SEEK_SET);
		n2 = nfloats/n1;
		n3 = 1;
	}
	if (!getparfloat("d2",&d2)) d2 = 1.0;
	if (!getparfloat("f2",&f2)) f2 = 0.0;

	/* get parameters describing 3rd dimension sampling */
	if (!getparint("n3",&n3)) {
		if (efseeko(infp,(off_t) 0,SEEK_END)!=0)
			err("must specify n3 if in a pipe!\n");
		nfloats = (int) (eftello(infp)/((off_t) sizeof(float)));
		efseeko(infp,(off_t) 0,SEEK_SET);
		n3 = nfloats/n1/n2;
	}
	if (!getparfloat("d3",&d3)) d3 = 1.0;
	if (!getparfloat("f3",&f3)) f3 = d3;

	/* set up desired looping mode */
	if (!getparint("loopdsp",&loopdsp)) loopdsp = 3;
	if (loopdsp == 3) {
		n1dsp = n1;
		n2dsp=n2;
		n3dsp=n3;
	} else if (loopdsp == 1) {
		n1dsp = n2;
		n2dsp=n3;
		n3dsp=n1;
	} else if (loopdsp == 2) {
		n1dsp = n1;
		n2dsp=n3;
		n3dsp=n2;
	}

	x1min = (d1>0.0)?f1:f1+(n1dsp-1)*d1;
	x1max = (d1<0.0)?f1:f1+(n1dsp-1)*d1;
	x2min = (d2>0.0)?f2:f2+(n2dsp-1)*d2;
	x2max = (d2<0.0)?f2:f2+(n2dsp-1)*d2;

	/* read binary data to be plotted */
	nz = n1*n2*n3;
	z = ealloc1float(nz);
	if (fread(z,sizeof(float),nz,infp)!=nz)
		err("error reading input file!\n");

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

	/* transpose data if needed */
	if (loopdsp == 1) {
		temp = ealloc1float(nz);
		for (iz=0;iz<nz;iz++) temp[iz] = z[iz];
		for (i3=0;i3<n3;i3++) {
			for (i2=0;i2<n2;i2++) {
		 		for(i1=0;i1<n1;i1++) {
		    			z[i2+i3*n2+i1*n2*n3]
						= temp[i1+i2*n1+i3*n1*n2];
				}
			}
		}
		free1float(temp);
	} else if (loopdsp == 2) {
		temp = ealloc1float(nz);
		for (iz=0;iz<nz;iz++) temp[iz] = z[iz];
		for (i3=0;i3<n3;i3++) {
			for (i2=0;i2<n2;i2++) {
				for(i1=0;i1<n1;i1++) {
					z[i1+i3*n1+i2*n1*n3]
						= temp[i1+i2*n1+i3*n1*n2];
				}
			}
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
	xbox = 1.0; getparfloat("xbox",&xbox);
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
	if (!getparstring("title2",&title2)) notitle2=1;
	getparstring("titlefont",&titlefont);
	titlesize = 24.0; getparfloat("titlesize",&titlesize);
	getparstring("style",&styles);
	if (STREQ("normal",styles))
		style = NORMAL;
	else
		style = SEISMIC;

        checkpars();

	/* adjust x1beg and x1end to fall on sampled values */

	i1beg = NINT((x1beg-f1)/d1);
	i1beg = MAX(0,MIN(n1dsp-1,i1beg));
	x1beg = f1+i1beg*d1;
	i1end = NINT((x1end-f1)/d1);
	i1end = MAX(0,MIN(n1dsp-1,i1end));
	x1end = f1+i1end*d1;

	/* adjust x2beg and x2end to fall on sampled values */
	i2beg = NINT((x2beg-f2)/d2);
	i2beg = MAX(0,MIN(n2dsp-1,i2beg));
	x2beg = f2+i2beg*d2;
	i2end = NINT((x2end-f2)/d2);
	i2end = MAX(0,MIN(n2dsp-1,i2end));
	x2end = f2+i2end*d2;

	/* allocate space for image bytes */
	n1c = 1+abs(i1end-i1beg);
	n2c = 1+abs(i2end-i2beg);

	/* convert data to be imaged into unsigned characters */
	zscale = (wclip!=bclip)?255.0/(wclip-bclip):1.0e10;
	zoffset = -bclip*zscale;
	i1step = (i1end>i1beg)?1:-1;
	i2step = (i2end>i2beg)?1:-1;

	/* determine sampling after scaling */
	n1s = MAX(1,NINT(1+(n1c-1)*d1/d1s));
	d1s = (n1s>1)?d1*(n1c-1)/(n1s-1):d1;
	n2s = MAX(1,NINT(1+(n2c-1)*d2/d2s));
	d2s = (n2s>1)?d2*(n2c-1)/(n2s-1):d2;

	/* convert axes box parameters from inches to points */
	xbox *= 72.0;
	ybox *= 72.0;
	wbox *= 72.0;
	hbox *= 72.0;

	/* set bounding box */
	psAxesBBox3(
		xbox,ybox,wbox,hbox,
		labelfont,labelsize,
		titlefont,titlesize,
		style,bbox);
	boundingbox(bbox[0],bbox[1],bbox[2],bbox[3]);

	/* begin PostScript */
	beginps();

	/* loop over n3 */
	for(i3=0;i3<n3dsp;i3++) {
		cz = ealloc1(n1c*n2c,sizeof(char));
		czp = cz;
		for (i1c=0,i1=i1beg; i1c<n1c; i1c++,i1+=i1step) {
			for (i2c=0,i2=i2beg; i2c<n2c; i2c++,i2+=i2step) {
				zi = zoffset
					+z[i1+i2*n1dsp+i3*n1dsp*n2dsp]*zscale;
				if (zi<0.0) zi = 0.0;
				if (zi>255.0) zi = 255.0;
				*czp++ = (unsigned char)zi;
			}
		}

		/* if necessary, interpolate to scaled sampling intervals */
		if (n1s!=n1c || n2s!=n2c) {
			sz = ealloc1(n1s*n2s,sizeof(char));
			intl2b(n2c,d2,0.0,n1c,d1,0.0,cz,
				n2s,d2s,0.0,n1s,d1s,0.0,sz);
			free1(cz); 
		} else {
			sz = cz;
		}

		/* determine axes pads */
		p1beg = (x1end>x1beg)?-fabs(d1s)/2:fabs(d1s)/2;
		p1end = (x1end>x1beg)?fabs(d1s)/2:-fabs(d1s)/2;
		p2beg = (x2end>x2beg)?-fabs(d2s)/2:fabs(d2s)/2;
		p2end = (x2end>x2beg)?fabs(d2s)/2:-fabs(d2s)/2;

		newpage("1",i3+1);

		/* save graphics state */
		gsave();

		/* translate coordinate system by box offset */
		translate(xbox,ybox);

		/* determine image matrix */
		if (style==NORMAL) {
			matrix[0] = 0;  matrix[1] = n1s;  matrix[2] = n2s;
			matrix[3] = 0;  matrix[4] = 0;  matrix[5] = 0;
		} else {
			matrix[0] = n2s;  matrix[1] = 0;  matrix[2] = 0;
			matrix[3] = -n1s;  matrix[4] = 0;  matrix[5] = n1s;
		}

		scale(wbox,hbox);

		/* draw the image (before axes so grid lines are visible) */
		image(n2s,n1s,8,matrix,sz);

		/* restore graphics state */
		grestore();

		/* update title2 only if n3>1*/
		strcpy(title2s,title2);
		if ( notitle2 == 0 ) {
			char *nullchar="";	
			f3s = f3 + i3 * d3;
			sprintf(c80,"= %.4g %s",f3s,nullchar);
			strcat(title2s,c80);
		}

		/* draw axes and title */
		psAxesBox3(
			xbox,ybox,wbox,hbox,
			x1beg,x1end,p1beg,p1end,
			d1num,f1num,n1tic,grid1,label1,
			x2beg,x2end,p2beg,p2end,
			d2num,f2num,n2tic,grid2,label2,
			labelfont,labelsize,
			title,titlefont,titlesize,
			style, title2s); 

		showpage();
	}

	/* end PostScript */
	endps();

	return EXIT_SUCCESS;
}

