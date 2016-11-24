#include "par.h"
#include "psplot.h"
#include "Triangles/tri.h"
#include "Triangles/sloth.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SPSPLOT - plot a triangulated sloth function s(x,z) via PostScript	",
"									",
" spsplot <modelfile >postscriptfile [optional parameters]		",
"									",
" Optional Parameters:							",
" gedge=0.0             gray to draw fixed edges (in interval [0.0,1.0])",
" gtri=1.0              gray to draw non-fixed edges of triangles 	",
" gmin=0.0              min gray to shade triangles (in interval [0.0,1.0])",
" gmax=1.0              max gray to shade triangles (in interval [0.0,1.0])",
" sgmin=minimum s(x,z)  s(x,y) corresponding to gmin 			",
" sgmax=maximum s(x,z)  s(x,y) corresponding to gmax 			",
" xbox=1.5              offset in inches of left side of axes box 	",
" ybox=1.5              offset in inches of bottom side of axes box	",
" wbox=6.0              width in inches of axes box			",
" hbox=8.0              height in inches of axes box			",
" xbeg=xmin             value at which x axis begins			",
" xend=xmax             value at which x axis ends			",
" dxnum=0.0             numbered tic interval on x axis (0.0 for automatic)",
" fxnum=xmin            first numbered tic on x axis (used if dxnum not 0.0)",
" nxtic=1               number of tics per numbered tic on x axis	",
" gridx=none            grid lines on x axis - none, dot, dash, or solid",
" labelx=               label on x axis					",
" zbeg=zmin             value at which z axis begins			",
" zend=zmax             value at which z axis ends			",
" dznum=0.0             numbered tic interval on z axis (0.0 for automatic)",
" fznum=zmin            first numbered tic on z axis (used if dynum not 0.0)",
" nztic=1               number of tics per numbered tic on z axis	",
" gridz=none            grid lines on z axis - none, dot, dash, or solid",
" labelz=               label on z axis					",
" labelfont=Helvetica   font name for axes labels			",
" labelsize=12          font size for axes labels			",
" title=                title of plot					",
" titlefont=Helvetica-Bold  font name for title				",
" titlesize=24          font size for title				",
" titlecolor=black      color of title					",
" axescolor=black       color of axes					",
" gridcolor=black       color of grid					",
" style=seismic         normal (z axis horizontal, x axis vertical) or	",
"                       seismic (z axis vertical, x axis horizontal)	",
"									",
" Note:  A value of gedge or gtri outside the interval [0.0,1.0]	",
" results in that class of edge not being drawn.			",
"									",
NULL};
/*
 *
 * AUTHOR:  Dave Hale, Colorado School of Mines, 10/18/90
 * MODIFIED: Craig Artley, Colorado School of Mines, 03/27/94
 *    Tweaks to improve PostScript header, add basic color support.
 *
 * NOTE:  Have observed errors in output when compiled with optimization
 *    under NEXTSTEP 3.1.  Caveat Emptor.
 *
 * Modified: Morten Wendell Pedersen, Aarhus University, 23/3-97
 *           Added ticwidth,axeswidth, gridwidth parameters 
 */
/**************** end self doc ***********************************/


/* prototypes for functions defined and used internally */
static void minmax (Model *m, float *smin, float *smax);
static void drawtriInterior (Tri *t, float xscale, float zscale,
	float gbase, float gscale);
static void drawtriUnfixedEdge (Tri *t, float xscale, float zscale);
static void drawtriFixedEdge (Tri *t, float xscale, float zscale);
static void psDrawShadedTri (float x1, float z1, float g1,
	float x2, float z2, float g2, 
	float x3, float z3, float g3);

/* the main program */
int main (int argc, char **argv)
{
	int bbox[4],style,nxtic,nztic,gridx,gridz;
	float gedge,gtri,gmin,gmax,sgmin,sgmax,gbase,gscale,smin,smax,
		xsize,zsize,xscale,zscale,
		xbeg,zbeg,xend,zend,xbox,ybox,wbox,hbox,
		labelsize,titlesize,dxnum,fxnum,dznum,fznum;
	float axeswidth, ticwidth, gridwidth;
	char *labelx="",*labelz="",*title="",
		*labelfont="Helvetica",*titlefont="Helvetica-Bold",
		*styles="seismic",*gridxs="none",*gridzs="none",
		*titlecolor="black",*axescolor="black",*gridcolor="black";
	Model *m;
	Face *f;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* read model */
	m = readModel(stdin);
	
	/* determine minimum and maximum s(x,y) */
	minmax(m,&smin,&smax);
	
	/* get optional parameters */
	gedge = 0.0; getparfloat("gedge",&gedge);
	gtri = 1.0; getparfloat("gtri",&gtri);
	gmin = 0.0; getparfloat("gmin",&gmin);
	gmax = 1.0; getparfloat("gmax",&gmax);
	sgmin = smin; getparfloat("sgmin",&sgmin);
	sgmax = smax; getparfloat("sgmax",&sgmax);
	getparstring("labelx",&labelx);
	getparstring("labelz",&labelz);
	getparstring("title",&title);
	getparstring("style",&styles);
	if (STREQ("normal",styles)) style = NORMAL;
	else style = SEISMIC;
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
	xbeg = m->ymin; getparfloat("xbeg",&xbeg);
	xend = m->ymax; getparfloat("xend",&xend);
	zbeg = m->xmin; getparfloat("zbeg",&zbeg);
	zend = m->xmax; getparfloat("zend",&zend);
	dxnum = 0.0; getparfloat("dxnum",&dxnum);
	fxnum = m->ymin; getparfloat("fxnum",&fxnum);
	nxtic = 1; getparint("nxtic",&nxtic);
	getparstring("gridx",&gridxs);
	if (STREQ("dot",gridxs)) gridx = DOT;
	else if (STREQ("dash",gridxs)) gridx = DASH;
	else if (STREQ("solid",gridxs)) gridx = SOLID;
	else gridx = NONE;
	dznum = 0.0; getparfloat("dznum",&dznum);
	fznum = m->xmin; getparfloat("fznum",&fznum);
	nztic = 1; getparint("nztic",&nztic);
	getparstring("gridz",&gridzs);
	if (STREQ("dot",gridzs)) gridz = DOT;
	else if (STREQ("dash",gridzs)) gridz = DASH;
	else if (STREQ("solid",gridzs)) gridz = SOLID;
	else gridz = NONE;
	if(!getparfloat("axeswidth",&axeswidth)) axeswidth=1;
	if (!getparfloat("ticwidth",&ticwidth)) ticwidth=axeswidth;
	if(!getparfloat("gridwidth",&gridwidth)) gridwidth =axeswidth;;

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
	xsize = (style==NORMAL)?hbox:wbox;
	zsize = (style==NORMAL)?wbox:hbox;

	/* translate coordinate system by box offset */
	translate(xbox,ybox);

	/* if style is not normal, rotate coordinate system */
	if (style!=NORMAL) {
		rotate(-90.0);
		translate(-hbox,0.0);
	}

	/* determine x and z scale factors */
	xscale = xsize/(xend-xbeg);
	zscale = zsize/(zend-zbeg);

	/* translate coordinate system by beginning axes values */
	translate(-zbeg*zscale,-xbeg*xscale);
	
	/* determine gray scale */
	if (sgmax==sgmin) sgmax += 1.0;
	gscale = (gmax-gmin)/(sgmax-sgmin);
	gbase = gmin-sgmin*gscale;
	
	/* draw triangles (faces) --- interior shading */
	gsave();
	f = m->f;
	do {
		drawtriInterior(f,xscale,zscale,gbase,gscale);
		f = f->fNext;
	} while (f!=m->f);
	grestore();

	/* draw triangles (faces) --- unfixed edges */
	if (gtri>=0.0 && gtri<=1.0) {
		gsave();
		setgray(gtri);
		f = m->f;
		do {
			drawtriUnfixedEdge(f,xscale,zscale);
			f = f->fNext;
		} while (f!=m->f);
		stroke();
		grestore();
	}

	/* draw triangles (faces) --- fixed edges */
	if (gedge>=0.0 && gedge<=1.0) {
		gsave();
		setgray(gedge);
		f = m->f;
		do {
			drawtriFixedEdge(f,xscale,zscale);
			f = f->fNext;
		} while (f!=m->f);
		stroke();
		grestore();
	}

	/* restore graphics state */
	grestore();

	/* draw axes and title ... see note below */
/*	psAxesBox(
		xbox,ybox,wbox,hbox,
		zbeg,zend,0.0,0.0,
		dznum,fznum,nztic,gridz,labelz,
		xbeg,xend,0.0,0.0,
		dxnum,fxnum,nxtic,gridx,labelx,
		labelfont,labelsize,
		title,titlefont,titlesize,
		style);
*/
/* 
   The (17 Dec 1993) version of the CWP/SU codes will implement
   color PostScript.  However, because most CWP sponsors will be
   using the old black and white PostScript code, this code
   is implemented with that scheme. When you upgrade to the new
   code, please substitute the paragraph below for the paragraph
   above.
*/
	psAxesBox(
		xbox,ybox,wbox,hbox,
		zbeg,zend,0.0,0.0,
		dznum,fznum,nztic,gridz,labelz,
		xbeg,xend,0.0,0.0,
		dxnum,fxnum,nxtic,gridx,labelx,
		labelfont,labelsize,
		title,titlefont,titlesize,
		titlecolor, axescolor, gridcolor,
		ticwidth,axeswidth,gridwidth,
		style);

	/* end PostScript */
	showpage();
	endeps();

	return EXIT_SUCCESS;
}

/* determine minimum and maximum s(x,z) in model */
static void minmax (Model *m, float *smin, float *smax)
{
	float s00,dsdx,dsdz,x,z,sxz;
	Face *f;
	FaceAttributes *fa;
	EdgeUse *eu;
	Vertex *v;
	
	/* initialize min and max values */
	*smin = FLT_MAX;
	*smax = -FLT_MAX;
	
	/* loop over faces */
	f = m->f;
	do {
		/* if face attributes exist */
		if ((fa=f->fa)!=NULL) {

			/* get face attributes */
			s00 = fa->s00;
			dsdx = fa->dsdx;
			dsdz = fa->dsdz;
			
			/* loop over vertexes */
			eu = f->eu;
			do {
				v = eu->vu->v;
				x = v->y;  z = v->x;
				sxz = s00+x*dsdx+z*dsdz;
				if (sxz<*smin) *smin = sxz;
				if (sxz>*smax) *smax = sxz;
				eu = eu->euCW;
			} while (eu!=f->eu);
		}
		
		/* next face */
		f = f->fNext;
		
	} while (f!=m->f);
}

/* draw triangle interior */
static void drawtriInterior (Tri *t, float xscale, float zscale,
	float gbase, float gscale)
{
	float x1,z1,s1,g1,x2,z2,s2,g2,x3,z3,s3,g3;
	FaceAttributes *fa;
		
	x1 = t->eu->vu->v->y;  z1 = t->eu->vu->v->x;
	x2 = t->eu->euCW->vu->v->y;  z2 = t->eu->euCW->vu->v->x;
	x3 = t->eu->euCCW->vu->v->y;  z3 = t->eu->euCCW->vu->v->x;

	if ((fa=t->fa)!=NULL) {
		s1 = fa->s00+x1*fa->dsdx+z1*fa->dsdz;
		s2 = fa->s00+x2*fa->dsdx+z2*fa->dsdz;
		s3 = fa->s00+x3*fa->dsdx+z3*fa->dsdz;
		g1 = gbase+s1*gscale;
		g2 = gbase+s2*gscale;
		g3 = gbase+s3*gscale;

		x1 *= xscale;  z1 *= zscale;
		x2 *= xscale;  z2 *= zscale;
		x3 *= xscale;  z3 *= zscale;

		psDrawShadedTri(z1,x1,g1,z2,x2,g2,z3,x3,g3);
	}
}

/* draw unfixed edges of triangle */
static void drawtriUnfixedEdge (Tri *t, float xscale, float zscale)
{
	float x1,z1,x2,z2,x3,z3;
		
	x1 = t->eu->vu->v->y;  z1 = t->eu->vu->v->x;
	x2 = t->eu->euCW->vu->v->y;  z2 = t->eu->euCW->vu->v->x;
	x3 = t->eu->euCCW->vu->v->y;  z3 = t->eu->euCCW->vu->v->x;

	x1 *= xscale;  z1 *= zscale;
	x2 *= xscale;  z2 *= zscale;
	x3 *= xscale;  z3 *= zscale;

	if (!t->eu->e->fixed) {
		moveto(z1,x1);
		lineto(z2,x2);
	}

	if (!t->eu->euCW->e->fixed) {
		moveto(z2,x2);
		lineto(z3,x3);
	}

	if (!t->eu->euCCW->e->fixed) {
		moveto(z3,x3);
		lineto(z1,x1);
	}
}

/* draw fixed edges of triangle */
static void drawtriFixedEdge (Tri *t, float xscale, float zscale)
{
	float x1,z1,x2,z2,x3,z3;
		
	x1 = t->eu->vu->v->y;  z1 = t->eu->vu->v->x;
	x2 = t->eu->euCW->vu->v->y;  z2 = t->eu->euCW->vu->v->x;
	x3 = t->eu->euCCW->vu->v->y;  z3 = t->eu->euCCW->vu->v->x;

	x1 *= xscale;  z1 *= zscale;
	x2 *= xscale;  z2 *= zscale;
	x3 *= xscale;  z3 *= zscale;

	if (t->eu->e->fixed) {
		moveto(z1,x1);
		lineto(z2,x2);
	}

	if (t->eu->euCW->e->fixed) {
		moveto(z2,x2);
		lineto(z3,x3);
	}

	if (t->eu->euCCW->e->fixed) {
		moveto(z3,x3);
		lineto(z1,x1);
	}
}

/* draw a shaded triangle via PostScript */
static void psDrawShadedTri (float x1, float y1, float g1,
	float x2, float y2, float g2, 
	float x3, float y3, float g3)
{
	int nlines,line;
	float a,b,c,dgdx,dgdy,angle,cangle,sangle,
		x1r,y1r,x2r,y2r,x3r,y3r,
		xmin,ymin,gmin,xmax,ymax,gmax,
		eps,gstep,xstep,gray,x,linewidth;
	
	/* save graphics state */
	gsave();
	
	/* set clip */
	newpath();
	moveto(x1,y1);
	lineto(x2,y2);
	lineto(x3,y3);
	clip();
	
	/* translate to make vertex 1 at (0,0) */
	translate(x1,y1);
	x2 -= x1;  y2 -= y1; 
	x3 -= x1;  y3 -= y1;
	x1 = 0.0;  y1 = 0.0;
	
	/* determine gradient of gray level */
	a = y2*(g3-g1)-y3*(g2-g1);
	b = x3*(g2-g1)-x2*(g3-g1);
	c = x2*y3-y2*x3;
	dgdx = (c!=0.0?-a/c:0.0);
	dgdy = (c!=0.0?-b/c:0.0);
	
	/* determine rotation angle and cos and sin */
	angle = (dgdx!=0.0 || dgdy!=0.0 ? atan2(dgdy,dgdx) : 0.0);
	cangle = cos(angle);
	sangle = sin(angle);
	
	/* rotate vertices */
	rotate(angle*180.0/3.141592654);
	x1r = 0.0;
	y1r = 0.0;
	x2r = x2*cangle+y2*sangle;
	y2r = -x2*sangle+y2*cangle;
	x3r = x3*cangle+y3*sangle;
	y3r = -x3*sangle+y3*cangle;
	
	/* determine rectangle */
	xmin = (x1r<x2r?x1r:x2r); xmin = (xmin<x3r?xmin:x3r);
	ymin = (y1r<y2r?y1r:y2r); ymin = (ymin<y3r?ymin:y3r);
	gmin = (xmin==x1r?g1:(xmin==x2r?g2:g3));
	xmax = (x1r>x2r?x1r:x2r); xmax = (xmax>x3r?xmax:x3r);
	ymax = (y1r>y2r?y1r:y2r); ymax = (ymax>y3r?ymax:y3r);
	gmax = (xmax==x1r?g1:(xmax==x2r?g2:g3));
	
	/* draw shaded rectangle */
	eps = 0.0166666;
	nlines = 1+ABS(gmax-gmin)/eps;
	if (nlines<2) nlines = 2;
	gstep = (gmax-gmin)/(nlines-1);
	xstep = (xmax-xmin)/(nlines-1);
	linewidth = 1.1*ABS(xstep);
	setlinewidth(linewidth);
	newpath();
	for (line=0,gray=gmin,x=xmin; line<nlines; ++line) {
		setgray(gray);
		moveto(x,ymin);
		lineto(x,ymax);
		stroke();
		gray += gstep;
		x += xstep;
	}
	
	/* restore graphics state */
	grestore();
}
