/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2010.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2007.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1998.*/
/* All rights reserved.                       */

/* PSDRAWCURVE: $Revision: 1.4 $ ; $Date: 2011/11/17 00:08:10 $	*/

/*********************** self documentation **********************/
/*****************************************************************************
PSDRAWCURVE - Functions to draw a curve from a set of points

psDrawCurve	Draw a curve from a set of points via PostScript
******************************************************************************
Function Prototypes:
void psDrawCurve(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float x2Beg, float x2End, float p2Beg, float p2End,
	float *x1curve, float *x2curve, int ncurve,
	char *curveColor, float curvewidth, int curvedash, int style);
******************************************************************************
psDrawCurve:
Input:
x		x coordinate of lower left corner of box
y		y coordinate of lower left corner of box
width		width of box
height		height of box
x1Beg  		axis value at beginning of axis 1
x1End		axis value at end of axis 1
p1Beg  		pad value at beginning of axis 1
p1End		pad value at end of axis 1
x2Beg  		axis value at beginning of axis 2
x2End		axis value at end of axis 2
p2Beg  		pad value at beginning of axis 2
p2End		pad value at end of axis 2
x1curve		vector of x1 coordinates for points along curve
x2curve		vector of x2 coordinates for points along curve
ncurve		number of points along curve
curveColor	color to use for curve
curvewidth	width (in points) of curve
style		NORMAL (axis 1 on bottom, axis 2 on left) 
		SEISMIC (axis 1 on left, axis 2 on top)

*****************************************************************************
Author:		Brian Macy, Phillips Petroleum Co., 11/20/98
		(Adapted after Dave Hale and other's psAxesBox routine)
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"
#include "psplot.h"

typedef struct {
	float x;
	float y;
} LPoint;

void psDrawCurve(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float x2Beg, float x2End, float p2Beg, float p2End,
	float *x1curve, float *x2curve, int ncurve,
	char *curveColor, float curvewidth, int curvedash, int style)
/*****************************************************************************
Draw a curve from a set of points via PostScript
******************************************************************************
Input:
x		x coordinate of lower left corner of box
y		y coordinate of lower left corner of box
width		width of box
height		height of box
x1Beg  		axis value at beginning of axis 1
x1End		axis value at end of axis 1
p1Beg  		pad value at beginning of axis 1
p1End		pad value at end of axis 1
x2Beg  		axis value at beginning of axis 2
x2End		axis value at end of axis 2
p2Beg  		pad value at beginning of axis 2
p2End		pad value at end of axis 2
x1curve		vector of x1 coordinates for points along curve
x2curve		vector of x2 coordinates for points along curve
ncurve		number of points along curve
curveColor	color to use for curve
curvewidth	width (in points) of curve
style		NORMAL (axis 1 on bottom, axis 2 on left) 
		SEISMIC (axis 1 on left, axis 2 on top)
******************************************************************************
Author:		Brian Macy, Phillips Petroleum Co., 11/20/98
		(Adapted after Dave Hale and other's psAxesBox routine)
*****************************************************************************/
{
	float xbase,ybase,xscale,yscale,size1,size2;
	float *xcurve,*ycurve;
	LPoint *lpoints;   /* points for drawing line */
	int i,j,ndashes;

	/* setup dash styles */
	struct {
	  int ndash;
	  float dash[8];
	} dashes [] = {
	  {0, {0.}},                        /* solid line     */
	  {2, {10.,3.}},                    /* __ __ __ __    */
	  {2, {10.,5.}},                    /* __ __ __ __    */
	  {1, {5}},                         /* _ _ _ _ _ _ _  */
	  {2, {5.,10.}},                    /* _  _  _  _  _  */
	  {2, {1.,3.}},                     /* . . . . . . .  */
	  {2, {1.,4.}},                     /* .  .  .  .  .  */
	  {2, {1.,7.}},                     /* .   .   .   .  */
	  {4, {5.,3.,1.,3.}},               /* _ . _ . _ . _  */
	  {6, {5.,3.,1.,3.,1.,3.}},         /* _ .. _ .. _ .. */
	  {8, {5.,3.,1.,3.,1.,3.,1.,3.}},   /* _ ... _ ... _  */
	  {8, {1.,3.,1.,3.,5.,3.,5.,3.}}    /* .. __ .. __ .. */
	};

	/* adjust dash lengths to linewidth */
	ndashes = sizeof(dashes)/sizeof(dashes[0]);
	if (curvedash < 0 || curvedash > ndashes-1) {
		fprintf(stderr,"No such dash index: %d\n",curvedash);
		exit(-1);
	}
	  
	for (i=0; i<ndashes; i++) {
	  for (j=0; j<dashes[i].ndash; j++) dashes[i].dash[j] *= curvewidth;
	}
	
	/* allocate memory for lpoints */
	if ((lpoints=(LPoint *)malloc(ncurve*sizeof(LPoint)))==NULL) {
		fprintf(stderr,"Cannot allocate memory for lpoints\n");
		exit(-1);
	}

	/* determine sizes of axes 1 and 2 */
	if (style==NORMAL) {
		size1 = width;
		size2 = height;
	} else {
		size1 = height;
		size2 = width;
	}

	/* save graphics state */
	gsave();

	/* translate coordinate system, so that origin is at x,y */
	translate(x,y);

	/* if style is not NORMAL, rotate coordinate system */
	if (style!=NORMAL) {
		rotate(-90.0);
		translate(-height,0.0);
	}

	/* start a new path (just to be safe) */
	newpath();

	/* set up parameters */
	setcolor(curveColor);
	setlinewidth(curvewidth);
	setdash(dashes[curvedash].dash,dashes[curvedash].ndash,0.);
	xscale = size1/(x1End+p1End-x1Beg-p1Beg);
	xbase = -xscale*(x1Beg+p1Beg);
	xcurve=x1curve;
	yscale = size2/(x2End+p2End-x2Beg-p2Beg);
	ybase = -yscale*(x2Beg+p2Beg);
	ycurve=x2curve;

	/*
	 * Draw a curve from input data xcurve,ycurve. Clip
	 * the curve outside the axesbox.
	 */
	for (i=0; i<ncurve; ++i) {
		lpoints[i].x=xbase+xscale*xcurve[i];
		lpoints[i].y=ybase+yscale*ycurve[i];
	}
	rectclip(0,0,size1,size2);
	if (ncurve > 1) {
	        moveto(lpoints[0].x,lpoints[0].y);
		for (i=1; i<ncurve; i++) {
			lineto(lpoints[i].x,lpoints[i].y);
		}
		stroke();
	}


	/* restore graphics state */
	grestore();

	/* free resources before returning */
	free(lpoints);
}
