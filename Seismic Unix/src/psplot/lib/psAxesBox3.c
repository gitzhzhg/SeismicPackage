/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
PSAXESBOX3 -  Functions draw an axes box via PostScript, estimate bounding box
               these are versions of psAxesBox and psAxesBox3 for psmovie.

psAxesBox3	draw an axes box via PostScript
psAxesBBox3	estimate bounding box for an axes box drawn via psAxesBox3

******************************************************************************
Function Prototypes:
void psAxesBox3(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
	float x2Beg, float x2End, float p2Beg, float p2End,
	float d2Num, float f2Num, int n2Tic, int grid2, char *label2,
	char *labelFont, float labelSize,
	char *title, char *titleFont, float titleSize,
	int style, char *title2);
void psAxesBBox3(
	float x, float y, float width, float height,
	char *labelFont, float labelSize,
	char *titleFont, float titleSize,
	int style, int bbox[]);

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
d1Num		numbered tic increment for axis 1 (0.0 for automatic)
f1Num		first numbered tic for axis 1
n1Tic		number of horizontal tics per numbered tic for axis 1
grid1		grid code for axis 1:  NONE, DOT, DASH, or SOLID
label1		label for axis 1
x2Beg  		axis value at beginning of axis 2
x2End		axis value at end of axis 2
p2Beg  		pad value at beginning of axis 2
p2End		pad value at end of axis 2
d2Num		vertical numbered tic increment (0.0 for automatic)
f2Num		first numbered vertical tic
n2Tic		number of vertical tics per numbered tic
grid2		grid code for vertical axis:  NONE, DOT, DASH, or SOLID
label2		vertical axis label
labelFont	name of font to use for axes labels
labelSize	size of font to use for axes labels
title		axes box title
titleFont	name of font to use for title
titleSize	size of font to use for title
style		NORMAL (axis 1 on bottom, axis 2 on left) 
		  SEISMIC (axis 1 on left, axis 2 on top)
title2		second title

***************************************************************************
psAxesBBox3:
Input:
x		x coordinate of lower left corner of box
y		y coordinate of lower left corner of box
width		width of box
height		height of box
labelFont	name of font to use for axes labels
labelSize	size of font to use for axes labels
titleFont	name of font to use for title
titleSize	size of font to use for title
style		NORMAL (axis 1 on bottom, axis 2 on left) 
			  SEISMIC (axis 1 on left, axis 2 on top)
Output:
bbox		bounding box (bbox[0:3] = llx, lly, ulx, uly)

***************************************************************************
Notes:
psAxesBox3:
psAxesBox will determine the numbered tic increment and first
numbered tic automatically, if the specified increment is zero.

Pad values must be specified in the same units as the corresponding 
axes values.  These pads are useful when the contents of the axes box 
requires more space than implied by the axes values.  For example, 
the first and last seismic wiggle traces plotted inside an axes box 
will typically extend beyond the axes values corresponding to the 
first and last traces.  However, all tics will lie with the limits 
specified in the axes values (x1Beg, x1End, x2Beg, x2End).

psAxesBBox3:
psAxesBBox uses font sizes to estimate the bounding box for
an axes box drawn with psAxesBox.  To be on the safe side, 
psAxesBBox overestimates.

psAxesBBox assumes that the axes labels and titles do not extend
beyond the corresponding edges of the axes box.

******************************************************************************
References: 
(see references in basic.c)
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/27/89
	 modified by Zhiming Li, CSM, 7/1/90
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"
#include "psplot.h"

void psAxesBox3(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
	float x2Beg, float x2End, float p2Beg, float p2End,
	float d2Num, float f2Num, int n2Tic, int grid2, char *label2,
	char *labelFont, float labelSize,
	char *title, char *titleFont, float titleSize,
	int style, char *title2)
/*****************************************************************************
psAxesBox3 -  draw an axes box via PostScript
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
d1Num		numbered tic increment for axis 1 (0.0 for automatic)
f1Num		first numbered tic for axis 1
n1Tic		number of horizontal tics per numbered tic for axis 1
grid1		grid code for axis 1:  NONE, DOT, DASH, or SOLID
label1		label for axis 1
x2Beg  		axis value at beginning of axis 2
x2End		axis value at end of axis 2
p2Beg  		pad value at beginning of axis 2
p2End		pad value at end of axis 2
d2Num		vertical numbered tic increment (0.0 for automatic)
f2Num		first numbered vertical tic
n2Tic		number of vertical tics per numbered tic
grid2		grid code for vertical axis:  NONE, DOT, DASH, or SOLID
label2		vertical axis label
labelFont	name of font to use for axes labels
labelSize	size of font to use for axes labels
title		axes box title
titleFont	name of font to use for title
titleSize	size of font to use for title
style		NORMAL (axis 1 on bottom, axis 2 on left) 
		  SEISMIC (axis 1 on left, axis 2 on top)
title2		second title
******************************************************************************
Notes:
psAxesBox will determine the numbered tic increment and first
numbered tic automatically, if the specified increment is zero.

Pad values must be specified in the same units as the corresponding 
axes values.  These pads are useful when the contents of the axes box 
requires more space than implied by the axes values.  For example, 
the first and last seismic wiggle traces plotted inside an axes box 
will typically extend beyond the axes values corresponding to the 
first and last traces.  However, all tics will lie with the limits 
specified in the axes values (x1Beg, x1End, x2Beg, x2End).
******************************************************************************
AUTHOR:  Dave Hale, Colorado School of Mines, 06/27/89
UPDATE:
	 Zhiming Li, CSM, 7/1/90
	 1. added title2 option
******************************************************************************/
{
	int n1num,n2num,ntic,ndash=0,grided;
	unsigned int nchars1;
	float xa,ya,ticsize,dnum,fnum,dtic,amin,amax,azero,
		base,scale,anum,atic,
		xx,
		size1,size2,ticb,numb,labelb,dash[2],
		labelCH,labelCW,labelCA,labelCD,
		titleCH,titleCA,titleCD;
	char str[256];

	/* determine font dimensions */
	labelCH = fontheight(labelFont,labelSize);
	labelCW = fontwidth(labelFont,labelSize);
	labelCA = fontascender(labelFont,labelSize);
	labelCD = fontdescender(labelFont,labelSize);
	titleCH = fontheight(titleFont,titleSize);
	titleCA = fontascender(titleFont,titleSize);
	titleCD = fontdescender(titleFont,titleSize);

	/* determine sizes of axes 1 and 2 */
	if (style==NORMAL) {
		size1 = width;
		size2 = height;
	} else {
		size1 = height;
		size2 = width;
	}

	/* determine numbered tic intervals */
	if (d1Num==0.0) {
		n1num = size1/(4*labelCW);
		scaxis(x1Beg,x1End,&n1num,&d1Num,&f1Num);
	}
	if (d2Num==0.0) {
		n2num = size2/(4*labelCW);
		scaxis(x2Beg,x2End,&n2num,&d2Num,&f2Num);
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

	/* set font and character size */
	setfont(labelFont,labelSize);

	/* determine tic size */
	ticsize = 0.3*labelCH;

	/* draw axis 1 */
	amin = (x1Beg<x1End)?x1Beg:x1End;
	amax = (x1Beg>x1End)?x1Beg:x1End;
	azero = 0.0001*(amax-amin);
	dnum = d1Num;  fnum = f1Num;  ntic = n1Tic;
	scale = size1/(x1End+p1End-x1Beg-p1Beg);
	base = -scale*(x1Beg+p1Beg);
	ticb = -ticsize;
	numb = ticb-labelCA;
	labelb = numb-labelCH;
	nchars1 = 0;
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if (anum<amin) continue;
		xa = base+scale*anum;
		moveto(xa,0.0);  lineto(xa,ticb);
		if (anum>-azero && anum<azero)
			sprintf(str,"%.4g",0.0);
		else
			sprintf(str,"%.4g",anum);
		xx = xa + 0.3 *labelCH;
		moveto(xx,numb);
		rotate(90.);
		if (nchars1 < ((unsigned int) strlen(str)))
			nchars1 = (unsigned int) strlen(str);
		justshow(-0.5,str);
		rotate(-90.);
	}
	stroke();
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		xa = base+scale*atic;
		moveto(xa,0.0);  lineto(xa,ticb/2);
	}
	stroke();
	if (grid1==SOLID) {
		grided = 1;
		ndash = 0;
	} else if (grid1==DASH) {
		grided = 1;
		ndash = 1;  dash[0] = 10;
	} else if (grid1==DOT) {
		grided = 1;
		ndash = 2;  dash[0] = 1;  dash[1] = 5;
	} else
		grided = 0;
	if (grided) {
		for (anum=fnum; anum<=amax; anum+=dnum) {
			if (anum<amin) continue;
			xa = base+scale*anum;
			moveto(xa,0.0);  lineto(xa,size2);
		}
		setdash(dash,ndash,0.0);
		stroke();
		setdash(dash,0,0.0);
	}
	if (nchars1 > 4) nchars1 = 4; 
	labelb = numb - nchars1 * labelCW / 2. ;   
	moveto(size1/2.0,labelb);
	rotate(180.0); 
	justshow(-0.5,label1);
	rotate(-180.0); 

	/* draw axis 2 */
	amin = (x2Beg<x2End)?x2Beg:x2End;
	amax = (x2Beg>x2End)?x2Beg:x2End;
	azero = 0.0001*(amax-amin);
	dnum = d2Num;  fnum = f2Num;  ntic = n2Tic;
	scale = size2/(x2End+p2End-x2Beg-p2Beg);
	base = -scale*(x2Beg+p2Beg);
	ticb = -ticsize;
	numb = ticb+labelCD;
	labelb = numb-labelCH;
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if (anum<amin) continue;
		ya = base+scale*anum;
		moveto(0.0,ya);  lineto(ticb,ya);
		if (anum>-azero && anum<azero)
			sprintf(str,"%.4g",0.0);
		else
			sprintf(str,"%.4g",anum);
		moveto(numb,ya); 
		rotate(90.0); 
		justshow(-0.5,str);
		rotate(-90.0);
	}
	stroke();
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		ya = base+scale*atic;
		moveto(0.0,ya);  lineto(ticb/2,ya);
	}
	stroke();
	if (grid2==SOLID) {
		grided = 1;
		ndash = 0;
	} else if (grid2==DASH) {
		grided = 1;
		ndash = 1;  dash[0] = 10;
	} else if (grid2==DOT) {
		grided = 1;
		ndash = 2;  dash[0] = 1;  dash[1] = 5;
	} else
		grided = 0;
	if (grided) {
		for (anum=fnum; anum<=amax; anum+=dnum) {
			if (anum<amin) continue;
			ya = base+scale*anum;
			moveto(0.0,ya);  lineto(size1,ya);
		}
		setdash(dash,ndash,0.0);
		stroke();
		setdash(dash,0,0.0);
	}
	moveto(labelb,size2/2.0);
	rotate(90.0);
	justshow(-0.5,label2);
	rotate(-90.0);

	/* draw title */
	setfont(titleFont,titleSize);
	if (style==NORMAL) {
		moveto(size1/2.0,size2+0.5*titleCH-titleCD);
		justshow(-0.5,title);
		moveto(size1/2.0,-0.5*titleCH+titleCD+labelb);
		justshow(-0.5,title2);
	} else {
		moveto(size1+0.5*titleCH+titleCA,size2/2.0);
		rotate(90.0);
		justshow(-0.5,title);
		rotate(-90.0);
		moveto(labelb-0.5*titleCH-titleCA,size2/2.0);
		rotate(90.0);
		justshow(-0.5,title2);
		rotate(-90.0);
	}

	/* draw axes box */
	moveto(0.0,0.0);
	lineto(size1,0.0);
	lineto(size1,size2);
	lineto(0.0,size2);
	lineto(0.0,0.0);
	stroke();

	/* restore graphics state */
	grestore();
}

void psAxesBBox3(
	float x, float y, float width, float height,
	char *labelFont, float labelSize,
	char *titleFont, float titleSize,
	int style, int bbox[])

/**************************************************************************
psAxesBBox3 - estimate bounding box for an axes box drawn via psAxesBox
***************************************************************************
Input:
x		x coordinate of lower left corner of box
y		y coordinate of lower left corner of box
width		width of box
height		height of box
labelFont	name of font to use for axes labels
labelSize	size of font to use for axes labels
titleFont	name of font to use for title
titleSize	size of font to use for title
style		NORMAL (axis 1 on bottom, axis 2 on left) 
			  SEISMIC (axis 1 on left, axis 2 on top)
Output:
bbox		bounding box (bbox[0:3] = llx, lly, ulx, uly)
***************************************************************************
Notes:
psAxesBBox uses font sizes to estimate the bounding box for
an axes box drawn with psAxesBox.  To be on the safe side, 
psAxesBBox overestimates.

psAxesBBox assumes that the axes labels and titles do not extend
beyond the corresponding edges of the axes box.

***************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/27/89
Update: Zhiming Li, CSM, 7/1/90	psAxesBBox3
	 1. added more margins for title2 annotation
**************************************************************************/
{
	float labelCH,labelCW,titleCH;

	/* determine font dimensions */
	labelCH = fontheight(labelFont,labelSize);
	labelCW = fontwidth(labelFont,labelSize);
	titleCH = fontheight(titleFont,titleSize);

	/* determine bounding box */
	if (style==NORMAL) {
		/* bbox[0] = x-3.0*labelCH; */
		bbox[0] = x-6.0*labelCH;
		/* bbox[1] = y-3.0*labelCH; */
		bbox[1] = y-3.0*labelCH-2.0*titleCH;
		bbox[2] = x+width+labelCW;
		bbox[3] = y+height+2.0*titleCH;
	} else {
		/* bbox[0] = x-3.0*labelCH; */
		bbox[0] = x-6.0*labelCH;
		bbox[1] = y-2.0*titleCH;
		bbox[2] = x+width+labelCW;
		/* bbox[3] = y+height+3.0*labelCH; */
		bbox[3] = y+height+3.0*labelCH+2.0*titleCH;
	}
}
