/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/* ELAPS: $Test Release: 1.1 $ ; $Date: 2011/11/21 16:58:10 $	*/

#include "par.h"
#include "psplot.h"
#include "tri.h"
#include "elastic.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" ELAPS - plot a triangulated function p(x,z) via PostScript 		",
" 									",
" elaps <modelfile >postscriptfile [optional parameters] 		",
" 									",
" Optional Parameters: 							",
" p=0			plot sqrt(a3333) (vertical P-wave velocity)	",	
" 		     =1 plot sqrt(a1313)(vertical S-wave velocity)	",
" 		     =2 plot (v_Ph-v_Pv)/v_Pv   			",
"                    =3 plot (a1212-a1313)/(2*a1313) = gamma            ",
" gedge=0.0		gray to draw fixed edges (in interval [0.0,1.0])",
" gtri=1.0		gray to draw non-fixed edges of triangles 	",
"			   put negative number for not drawing  	",
" gmin=0.0		min gray to shade triangles (in interval [0.0,1.0])",
" gmax=1.0		max gray to shade triangles (in interval [0.0,1.0])",
" pgmin=minimum p(x,z)	p(x,y)-value corresponding to gmin 		",
" pgmax=maximum p(x,z)	p(x,y)-value corresponding to gmax 		",
" xbox=1.5		offset in inches of left side of axes box 	",
" ybox=1.5		offset in inches of bottom side of axes box	",
" wbox=6.0		width in inches of axes box			",
" hbox=8.0		height in inches of axes box			",
" xbeg=xmin		value at which x axis begins			",
" xend=xmax		value at which x axis ends			",
" dxnum=0.0		numbered tic interval on x axis (0.0 for automatic)",
" fxnum=xmin		first numbered tic on x axis (used if dxnum not 0.0)",
" nxtic=1		number of tics per numbered tic on x axis	",
" gridx=none		grid lines on x axis - none, dot, dash, or solid",
" labelx=		label on x axis					",
" zbeg=zmin		value at which z axis begins			",
" zend=zmax		value at which z axis ends			",
" dznum=0.0		numbered tic interval on z axis (0.0 for automatic)",
" fznum=zmin		first numbered tic on z axis (used if dynum not 0.0)",
" nztic=1		number of tics per numbered tic on z axis	",
" gridz=none		grid lines on z axis - none, dot, dash, or solid",
" labelz=		label on z axis					",
" labelfont=Helvetica	font name for axes labels			",
" labelsize=12		font size for axes labels			",
" title=		title of plot					",
" titlecolor=black      color of title                                  ",
" axescolor=black       color of axes                                   ",
" gridcolor=black       color of grid                                   ",    
" titlefont=Helvetica-Bold font name for title				",
" titlesize=24		font size for title				",
" style=seismic	  normal (z axis horizontal, x axis vertical) or	",
"			seismic (z axis vertical, x axis horizontal)	",
"									",
NULL};
/*
 *
 * AUTHOR:  Dave Hale, Colorado School of Mines, 10/18/90
 * modified: Andreas Rueger, Colorado School of Mines, 01/25/94
 *
 */
/**************** end self doc ***********************************/


/* prototypes for functions defined and used internally */
static void minmax (Model *m, float *smin, float *smax, int p);

static void drawtri (Tri *t, float xscale, float zscale,
	float gedge, float gtri, float gbase, float gscale, int p);

static void psDrawShadedTri (float x1, float z1, float x2, float z2,
	float x3, float z3, float g);

/* the main program */
int main (int argc, char **argv)
{
	int bbox[4],style,nxtic,nztic,gridx,gridz,p;
	float gedge,gtri,gmin,gmax,pgmin,pgmax,gbase,gscale,pmin,pmax,
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
	
	if (!getparint("p",&p)) p = 0;

        if(p == 0){
	  fprintf(stderr,"\n **** v_P(x,z) field will be plotted ****\n\n");
	} else if(p == 1){
	  fprintf(stderr,"\n **** v_S(x,z) field will be plotted ****\n\n");
	} else if(p == 2){
	  fprintf(stderr,"\n **** epsilon field will be plotted ****\n\n");
	} else if(p == 3){
	  fprintf(stderr,"\n **** gamma field will be plotted ****\n\n");
	} else {
		err("ERROR: parameter <p> must be either 0,1,2 or 3 ");
	}


	/* determine minimum and maximum p(x,y) */
	minmax(m,&pmin,&pmax,p);

	fprintf(stderr,"\n minimum value = %g \n\n",pmin);
	fprintf(stderr,"\n maximum value = %g \n\n",pmax);

	
	/* get optional parameters */

	if(!getparfloat("gedge",&gedge)) gedge = 0.0;
	if(!getparfloat("gtri",&gtri)) gtri = 1.0; 
	if(!getparfloat("gmin",&gmin)) gmin = 0.0; 
	if(!getparfloat("gmax",&gmax)) gmax = 1.0; 
	if(!getparfloat("pgmin",&pgmin)) pgmin = pmin; 
	if(!getparfloat("pgmax",&pgmax)) pgmax = pmax; 
	getparstring("labelx",&labelx);
	getparstring("labelz",&labelz);
	getparstring("title",&title);
	getparstring("style",&styles);
	if (STREQ("normal",styles)) style = NORMAL;
	else style = SEISMIC;
	getparstring("labelfont",&labelfont);
	getparstring("titlefont",&titlefont);
        getparstring("titlecolor",&titlecolor);
        getparstring("axescolor",&axescolor);
        getparstring("gridcolor",&gridcolor);    
	if(!getparfloat("labelsize",&labelsize)) labelsize = 18.0;
	if(!getparfloat("titlesize",&titlesize))titlesize = 24.0; 
	if(!getparfloat("xbox",&xbox)) xbox = 1.5; 
	if(!getparfloat("ybox",&ybox)) ybox = 1.5; 
	if(!getparfloat("wbox",&wbox)) wbox = 6.0; 
	if(!getparfloat("hbox",&hbox)) hbox = 8.0; 
	if(!getparfloat("xbeg",&xbeg)) xbeg = m->ymin; 
	if(!getparfloat("xend",&xend)) xend = m->ymax; 
	if(!getparfloat("zbeg",&zbeg)) zbeg = m->xmin; 
	if(!getparfloat("zend",&zend)) zend = m->xmax; 
	if(!getparfloat("dxnum",&dxnum)) dxnum = 0.0; 
	if(!getparfloat("fxnum",&fxnum)) fxnum = m->ymin; 
	if(!getparint("nxtic",&nxtic)) nxtic = 1; 
	getparstring("gridx",&gridxs);
	if (STREQ("dot",gridxs)) gridx = DOT;
	else if (STREQ("dash",gridxs)) gridx = DASH;
	else if (STREQ("solid",gridxs)) gridx = SOLID;
	else gridx = NONE;
	if(!getparfloat("dznum",&dznum)) dznum = 0.0; 
	if(!getparfloat("fznum",&fznum)) fznum = m->xmin; 
	if(!getparint("nztic",&nztic)) nztic = 1; 
	getparstring("gridz",&gridzs);
	if (STREQ("dot",gridzs)) gridz = DOT;
	else if (STREQ("dash",gridzs)) gridz = DASH;
	else if (STREQ("solid",gridzs)) gridz = SOLID;
	else gridz = NONE;
        if(!getparfloat("axeswidth",&axeswidth)) axeswidth=1;
        if (!getparfloat("ticwidth",&ticwidth)) ticwidth=axeswidth;
        if(!getparfloat("gridwidth",&gridwidth)) gridwidth =axeswidth;

	/* begin PostScript */
	beginps();
	newpage("1",1);
  	
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
	
	/* draw axes and title ... see note below */
	/* psAxesBox(
		xbox,ybox,wbox,hbox,
		zbeg,zend,0.0,0.0,
		dznum,fznum,nztic,gridz,labelz,
		xbeg,xend,0.0,0.0,
		dxnum,fxnum,nxtic,gridx,labelx,
		labelfont,labelsize,
		title,titlefont,titlesize,
		titlecolor, axescolor, gridcolor,
		style);
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
	/* remember that g=0 is black; g=1 is white */

	if (pgmax==pgmin) pgmax += 1.0;
	gscale = (gmin-gmax)/(pgmax-pgmin);
	gbase = gmax-pgmin*gscale;
	
	/* draw triangles (faces) */
	f = m->f;
	do {
		drawtri(f,xscale,zscale,gedge,gtri,gbase,gscale,p);
		f = f->fNext;
	} while (f!=m->f);
	
	/* end PostScript */
	showpage();
	endps();

	return 1;
}

/* determine minimum and maximum p(x,z) in model */
static void minmax (Model *m, float *pmin, float *pmax, int p)
{
	float v=0.0;
	Face *f;
	FaceAttributes *fa;
	
	/* initialize min and max values */
	*pmin = FLT_MAX;
	*pmax = -FLT_MAX;
	
	/* loop over faces */
	f = m->f;
	do {
		/* if face attributes exist */
		if ((fa=f->fa)!=NULL) {

		/* get face attributes */
		if(p == 0){
	 		v = sqrt(fa->a3333);
		} else if(p == 1){
	 		v = sqrt(fa->a1313);
		} else if(p == 2){
			v = (sqrt(fa->a1111)-sqrt(fa->a3333))/fa->a3333;
		} else if(p == 3)
			v = 0.5*(fa->a1212 - fa->a1313)/fa->a1313;

		if (v<*pmin) *pmin = v;
		if (v>*pmax) *pmax = v;

		/* next face */
		f = f->fNext;

		}
	} while (f!=m->f);

}

/* draw triangle *********************************************************/
static void drawtri (Tri *t, float xscale, float zscale,
	float gedge, float gtri, float gbase, float gscale, int p)
{
	float x1,z1,x2,z2,x3,z3,g=0.0;
	FaceAttributes *fa;
		
	x1 = t->eu->vu->v->y;  z1 = t->eu->vu->v->x;
	x2 = t->eu->euCW->vu->v->y;  z2 = t->eu->euCW->vu->v->x;
	x3 = t->eu->euCCW->vu->v->y;  z3 = t->eu->euCCW->vu->v->x;
	if ((fa=t->fa)!=NULL) {
		if(p ==0){
			g = sqrt(fa->a3333);
		} else if(p == 1){
			g = sqrt(fa->a1313);
		} else if(p == 2){
			g = (sqrt(fa->a1111)-sqrt(fa->a3333))/fa->a3333;
		} else if(p == 3)
			g = ( fa->a1212 - fa->a1313 )/ fa->a1313 *0.5;

		g = gbase+g*gscale;
	}
	x1 *= xscale;  z1 *= zscale;
	x2 *= xscale;  z2 *= zscale;
	x3 *= xscale;  z3 *= zscale;

	gsave();

	if(fa!=NULL) psDrawShadedTri(z1,x1,z2,x2,z3,x3,g);
	

	if (t->eu->e->fixed && gedge>=0.0 && gedge<=1.0) {
		setgray(gedge);
		moveto(z1,x1);
		lineto(z2,x2);
		stroke();
	} else if (!t->eu->e->fixed && gtri>=0.0 && gtri<=1.0) {
		setgray(gtri);
		moveto(z1,x1);
		lineto(z2,x2);
		stroke();
	}
	if (t->eu->euCW->e->fixed && gedge>=0.0 && gedge<=1.0) {
		setgray(gedge);
		moveto(z2,x2);
		lineto(z3,x3);
		stroke();
	} else if (!t->eu->euCW->e->fixed && gtri>=0.0 && gtri<=1.0) {
		setgray(gtri);
		moveto(z2,x2);
		lineto(z3,x3);
		stroke();
	}
	if (t->eu->euCCW->e->fixed && gedge>=0.0 && gedge<=1.0) {
		setgray(gedge);
		moveto(z3,x3);
		lineto(z1,x1);
		stroke();
	} else if (!t->eu->euCCW->e->fixed && gtri>=0.0 && gtri<=1.0) {
		setgray(gtri);
		moveto(z3,x3);
		lineto(z1,x1);
		stroke();
	}
	grestore();
}

/* draw a shaded triangle via PostScript *************************************/
static void psDrawShadedTri (float x1, float y1, float x2, float y2,
	float x3, float y3, float g)
{
	/* save graphics state */
	gsave();
	
	newpath();
	moveto(x1,y1);
	lineto(x2,y2);
	lineto(x3,y3);
	closepath();
	setgray(g);
	fill();
	
	/* restore graphics state */
	grestore();
}
