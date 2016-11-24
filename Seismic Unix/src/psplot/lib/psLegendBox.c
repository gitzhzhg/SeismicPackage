/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
PSLEGENDBOX - Functions to draw PostScript axes and estimate bounding box

psLegendBox	Draw an legend box via PostScript
psLegendBBox	estimate bounding box for an legend box drawn via psLegendBox

******************************************************************************
Function Prototypes:
void psLegendBox(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
	char *labelFont, float labelSize,
	char *axesColor, char *gridColor,
	int style);

void psLegendBBox(
	float x, float y, float width, float height,
	char *labelFont, float labelSize,
	int style, int bbox[]);


******************************************************************************
psLegendBox:
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
labelFont	name of font to use for axes labels
labelSize	size of font to use for axes labels
axesColor	color to use for axes and axes labels
gridColor	color to use for grid lines
style		VERTLEFT (Vertical, axis label on left side) 
		VERTRIGHT (Vertical, axis label on right side) 
		HORIBOTTOM (Horizontal, axis label on bottom)

*****************************************************************************
psLegendBBox:
Input:
x		x coordinate of lower left corner of box
y		y coordinate of lower left corner of box
width		width of box
height		height of box
labelFont	name of font to use for axes labels
labelSize	size of font to use for axes labels
style		VERTLEFT (Vertical, axis label on left side) 
		VERTRIGHT (Vertical, axis label on right side) 
		HORIBOTTOM (Horizontal, axis label on bottom)
Output:
bbox		bounding box (bbox[0:3] = llx, lly, ulx, uly)

******************************************************************************
Notes:
psLegendBox:
psLegendBox will determine the numbered tic increment and first numbered
tic automatically, if the specified increment is zero.  Axis numbering
is in scientific notation, if necessary and is plotted to four
significant digits.

Pad values must be specified in the same units as the corresponding
Legend values.  These pads are useful when the contents of the Legend box
requires more space than implied by the Legend values.  For example, the
first and last seismic wiggle traces plotted inside an Legend box will
typically extend beyond the Legend values corresponding to the first and
last traces.  However, all tics will lie with the limits specified in
the Legend values (x1Beg, x1End, x2Beg, x2End).

psLegendBBox:
psLegendBBox uses font sizes to estimate the bounding box for
an Legend box drawn with psLegendBox.  To be on the safe side, 
psLegendBBox overestimates.

psLegendBBox assumes that the Legend labels and titles do not extend
beyond the corresponding edges of the Legend box.

******************************************************************************
References:
(see References for basic.c)
******************************************************************************
Author:   Dave Hale,  Colorado School of Mines, 06/27/89
Modified: Ken Larner, Colorado School of Mines, 08/30/90
Modified: Dave Hale, Advance Geophysical, 10/18/92
	Added color parameters for title, axes, and grid.
Modified: Torsten Schoenfelder, Koeln, Germany, 07/06/97
        Display a legend for ps file, move axis from left to right
Modified: Torsten Schoenfelder, Koeln, Germany, 10/02/98
        Corrected width of bbox to include legend title
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"
#include "psplot.h"

#define RIGHT 0
#define LEFT 1
#define IROUND(x) (int)((x) > 0 ? (x) + .5 : (x) - .5) 
#define FMAX(x,y) (float) (x) > (y) ? (x) : (y) 
#define FMIN(x,y) (float) (x) > (y) ? (y) : (x) 

void psLegendBox(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
	char *labelFont, float labelSize,
	char *axesColor, char *gridColor,
	int style)
/*****************************************************************************
Draw an legend box via PostScript
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
labelFont	name of font to use for axes labels
labelSize	size of font to use for axes labels
axesColor	color to use for axes and axes labels
gridColor	color to use for grid lines
style		VERTLEFT (Vertical, axis label on left side) 
		VERTRIGHT (Vertical, axis label on right side) 
		HORIBOTTOM (Horizontal, axis label on bottom)
******************************************************************************
Notes:
psLegendBox will determine the numbered tic increment and first numbered
tic automatically, if the specified increment is zero.  Axis numbering
is in scientific notation, if necessary and is plotted to four
significant digits.

Pad values must be specified in the same units as the corresponding
axes values.  These pads are useful when the contents of the axes box
requires more space than implied by the axes values.  For example, the
first and last seismic wiggle traces plotted inside an axes box will
typically extend beyond the axes values corresponding to the first and
last traces.  However, all tics will lie with the limits specified in
the axes values (x1Beg, x1End, x2Beg, x2End).

** This function has to display a box of width and height. It is not changing
** both according to vertical or horizontal layout. It is only moving the axis
******************************************************************************
Author:   Dave Hale,  Colorado School of Mines, 06/27/89
Modified: Ken Larner, Colorado School of Mines, 08/30/90
Modified: Dave Hale, Colorado School of Mines, 10/18/92
	Added color parameters for title, axes, and grid.
Modified: Torsten Schoenfelder, Koeln, Germany, 07/06/97
        Display a legend for ps file
*****************************************************************************/
{
	int n1num,ntic,ndash=0,grided;
	int ndig,ndigits,nexp,nexpmax=0,nexplot1,
		nplaces,nplacesmax,nformat;
	float xa,ticsize,dnum,fnum,dtic,amin,amax,azero,
		base,scale,anum,anumnorm,atic,fnexp,
		ticb,numb,labelb,dash[2],
		labelCH,labelCW,labelCA,labelCD,
		pnorm,fdexp,azeronorm;
	char str[256],str2[256],sformat[256];

	int ticsside=LEFT;
	if (style==VERTRIGHT) ticsside=RIGHT;
	/* determine font dimensions */
	labelCH = fontheight(labelFont,labelSize);
	labelCW = fontwidth(labelFont,labelSize); /* Not accounting for real width 
						  ** of every char. A '0' needs labelCW/2,
						  ** a '.' needs labelCW/4 */
	labelCA = fontascender(labelFont,labelSize);
	labelCD = fontdescender(labelFont,labelSize);

	/* keep width and height, even if rotating box */
	/* determine numbered tic intervals */
	if (d1Num==0.0) {
	  if ((style==VERTLEFT) || (style==VERTRIGHT)) { /* vertical */
	    n1num = IROUND(height/(2.0*labelCH)); /* 4.* is too much for legend */
	  } else { /* horizontal */
	    n1num = IROUND(width/(3.5*labelCW)); /* 4.* may be OK for legends with large width */
	  }
	  scaxis(x1Beg,x1End,&n1num,&d1Num,&f1Num);
	}
	/* save graphics state */
	gsave();

	/* translate coordinate system, so that origin is at x,y */
	translate(x,y);

	/* start a new path (just to be safe) */
	newpath();

	/* set font and character size */
	setfont(labelFont,labelSize);

	/* determine tic size */
	if ((style==VERTLEFT) || (style==VERTRIGHT)) { /* vertical */
	  ticsize = 0.3*labelCW;
	} else { /* horizontal */
	  ticsize = 0.3*labelCH;
	}

	/* draw axis 1 */
	setcolor(axesColor);
	amin = (x1Beg<x1End)?x1Beg:x1End;
	amax = (x1Beg>x1End)?x1Beg:x1End;
	azero = 0.0001*(amax-amin);
	dnum = d1Num;  fnum = f1Num;  ntic = n1Tic;
	if ((style==VERTLEFT) || (style==VERTRIGHT)) { /* vertical */
	  scale = height/(x1End+p1End-x1Beg-p1Beg);
	} else { /* horizontal */
	  scale = width/(x1End+p1End-x1Beg-p1Beg);
	}
	base = -scale*(x1Beg+p1Beg);
	ticb = -ticsize;
	numb = 1.2*ticb-labelCA;
	if((style==VERTLEFT) || (style==VERTRIGHT)) numb = 1.2*ticb;
	labelb = numb-labelCH;

	/* determine axis1 exponent for scientific notation   */
	ndigits = 0;
	ndig    = 0;
	fdexp = log10(1.001*ABS(dnum));
	if(fdexp<0.)   fdexp -=1.0;
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if(anum==fnum && anum==0.)
			nexpmax = 0;
		nexp    = 0;
		if(anum!=0.)   {
			fnexp = log10(1.001*ABS(anum));
			if(fnexp>0.)   nexp    = (int)fnexp;
			else           nexp    = (int)fnexp-1;		
			if(anum==fnum)
				nexpmax = nexp;
			if(nexpmax<nexp || nexpmax==0)    
				nexpmax = nexp;
			ndig = 1+nexp-(int)fdexp;
		}
		if(ndigits<ndig)   ndigits = ndig;
	}
	nexplot1 = 0;
	if(ABS(nexpmax)>3)   
		nexplot1 = nexpmax;
	if((nexpmax<0) && (ndigits+ABS(nexpmax))>4)   
		nexplot1 = nexpmax;
	
	/* loop for axis1 numbering */
	nplacesmax = 0;
	pnorm      = pow(10.0,(double)nexplot1);
	azeronorm  = azero/pnorm;
	fdexp = log10(1.001*ABS(dnum/pnorm));
	if(fdexp<0.)   fdexp -=1.0;
	for (anum=fnum; anum<=amax; anum+=dnum) {

		if (anum<amin) continue;
		xa = base+scale*anum;
		anumnorm = anum/pnorm;

		/* find the number of places in axis1 numbers */
		nplaces = 1;
		nexp    = 1;
		nexp    = 0;
		if (anumnorm<-azeronorm || anumnorm>azeronorm)   {
			fnexp = log10(1.001*ABS(anumnorm));
			if(fnexp>0.)   {
				nexp    = (int)fnexp;
				nplaces = nexp+1;
			}
			else   {
				nexp    = (int)fnexp-1;
				nplaces = -nexp+2;
			}
		}
		/* numbers limited to four significant digits */
		ndigits = 1+nexp-(int)fdexp;
		if(ndigits>4)   ndigits = 4;

		if((nexp>=0) && ((ndigits+1)>nplaces))    
			nplaces = ndigits+1;
		if(anum<0. )   
			nplaces +=1;
		if(nexp<0)   
			nplaces = nplaces+ndigits-1;
		if(nplacesmax<nplaces)   
			nplacesmax = nplaces;

		nformat = ndigits-(nexp+1);
		if(nformat<0)   nformat = 0;

		if (anumnorm>-azeronorm && anumnorm<azeronorm)
			sprintf(str,"%.4g",0.0);
		else	{
			sprintf(sformat,"%%.%df",nformat);
			sprintf(str,sformat,anumnorm);
		}

		if ((style==VERTLEFT) || (style==VERTRIGHT)) {
		  if (ticsside==LEFT) {
		    moveto(numb,xa-(labelCA/2.));
		    justshow(-1.0,str);
		  } else {
		    moveto(width-numb,xa-(labelCA/2.));
		    justshow(0.0,str);
		  }
		}
		else   {
		  moveto(xa,numb);
		  justshow(-0.5,str);
		}
			
	}
	/* draw exponential multiplier for axis1 */
	if(nexplot1!=0)  {
	  if((style!=VERTLEFT) && (style!=VERTRIGHT))   {
	    moveto(width-numb,0);
	    show("x10");
	    moveto(width-numb+1.5*labelCW,0.5*labelCA);
	    sprintf(str2,"%d",nexplot1);
	    show(str2);
	  }
	  else   {
	    if (ticsside==LEFT) {
	      moveto(-1.5*labelCW,labelb);
	      show("x10");
	      moveto(0.0,labelb+0.5*labelCA);
	      sprintf(str2,"%d",nexplot1);
	      show(str2);
	    } else {
	      moveto(width,labelb);
	      show("x10");
	      moveto(width+1.5*labelCW,labelb+0.5*labelCA);
	      sprintf(str2,"%d",nexplot1);
	      show(str2);
	    }
	  }
	}
	stroke();

	/* draw axis1 tick marks */
	if((style==VERTLEFT) || (style==VERTRIGHT)) 
	  labelb = numb-0.5*nplacesmax*labelCW;
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		xa = base+scale*atic;
		if ((style==VERTLEFT) || (style==VERTRIGHT)) { /* vertical */
		  if (ticsside==LEFT) {
		    moveto(0.0,xa); lineto(ticb/2,xa);
		  }
		  else {
		    moveto(width,xa); lineto(width-ticb/2,xa);
		  }
		} else { /* horizontal */
		  moveto(xa,0.0); lineto(xa,ticb/2);
		}
	}
	stroke();

	/* draw axis1 label */
	if((style!=VERTLEFT) && (style!=VERTRIGHT)) {
	  moveto(width/2.0,labelb);   /* TS: 02/10/99: 1.5 -> 2.0 */
	  justshow(-0.5,label1);
	}
	else {
	  if (ticsside==LEFT) {
	    moveto(labelb+labelCD,height/2.0);
	    rotate(90.);
	    justshow(-0.5,label1);
	    rotate(-90.);
	  } else {
	    moveto(width-labelb+labelCH,height/2.0);
	    rotate(90.);
	    justshow(-0.5,label1);
	    rotate(-90.);
	  }
	}

	/* draw axis1 grid */
	setcolor(gridColor);
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
			if ((style==VERTLEFT) || (style==VERTRIGHT)) { /* vertical */
			  moveto(0.0,xa);  lineto(width,xa);
			} else { /* horizontal */
			  moveto(xa,0.0);  lineto(xa,height);
			}
		}
		setdash(dash,ndash,0.0);
		stroke();
		setdash(dash,0,0.0);
	}
	

	/* draw axes box */
	setcolor(axesColor);
	moveto(0.0,0.0);
	lineto(width,0.0);
	lineto(width,height);
	lineto(0.0,height);
	lineto(0.0,0.0);
	stroke();


	/* restore graphics state */
	grestore();
}

void psLegendBBox(float x, float y, float width, float height,
	char *labelFont, float labelSize,
	int style, int bbox[])
/*****************************************************************************
psLegendBBox - estimate bounding box for an axes box drawn via psLegendBox
*****************************************************************************
Input:
x		x coordinate of lower left corner of box
y		y coordinate of lower left corner of box
width		width of box
height		height of box
labelFont	name of font to use for axes labels
labelSize	size of font to use for axes labels
style		VERTLEFT (Vertical, axis label on left side) 
		VERTRIGHT (Vertical, axis label on right side) 
		HORIBOTTOM (Horizontal, axis label on bottom)
Output:
bbox		bounding box (bbox[0:3] = llx, lly, ulx, uly)
*****************************************************************************
Notes:
psLegendBBox uses font sizes to estimate the bounding box for
a legend box drawn with psLegendBox.  To be on the safe side, 
psLegendBBox overestimates.

psLegendBBox assumes same calculations like the psLegendBox.

*****************************************************************************
Author:   Dave Hale,  Colorado School of Mines, 06/27/89
Modified: Ken Larner, Colorado School of Mines, 07/24/90
Modified: Torsten Schoenfelder, Koeln, Germany, 06/16/97
****************************************************************************/
{
	float labelCH,labelCW,labelCA,labelCD;

	/* determine font dimensions */
	labelCH = fontheight(labelFont,labelSize);
	labelCW = fontwidth(labelFont,labelSize);
	labelCA = fontascender(labelFont,labelSize);
	labelCD = fontdescender(labelFont,labelSize);

	/* determine bounding box */
	if ((style==VERTLEFT) || (style==VERTRIGHT)) { /* Vertical */
	  bbox[1] = IROUND(y-3.0*labelCH)-1;
	  bbox[3] = IROUND(y+height+labelCH)+1;
	  if (style==VERTRIGHT) {
	    bbox[0] = x-2;
	    bbox[2] = IROUND(x+width+4.0*labelCW-labelCD)+1;
	  } else {
	    /* TS: 02/10/99: 2.36 -> 3.36 */
	    bbox[0] = IROUND(x-3.36*labelCW+2.*labelCD-labelCH)-1;
	    bbox[2] = x+width+3; /* +1 should be OK, if really needed */
	  }
	} else { /* Horizontal */
	  bbox[0] = IROUND(x-3.5*labelCW-labelCH)-1;
	  bbox[1] = IROUND(y-1.36*labelCH-labelCA+labelCD)-1;
	  bbox[2] = IROUND(x+width+4.0*labelCW)+1;
	  bbox[3] = IROUND(y+height)+2;
	}
}







