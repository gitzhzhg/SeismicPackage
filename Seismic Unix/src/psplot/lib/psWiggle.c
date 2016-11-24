/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*************************************************************************
PSWIGGLE - draw wiggle-trace with (optional) area-fill via PostScript

psWiggle  draw wiggle-trace with (optional) area-fill via PostScript

**************************************************************************
Function Prototype:
void psWiggle (int n, float z[], float zmin, float zmax, float zbase,
	float yzmin, float yzmax, float xfirst, float xlast, int fill);

**************************************************************************
Inputs:
n		number of samples to draw
z		array to draw
zmin		z values below zmin will be clipped
zmax		z values above zmax will be clipped
zbase		z values between zbase and either zmin or zmax will be filled
yzmin		y-coordinate corresponding to zmin
yzmax		y-coordinate corresponding to zmax
xfirst		x-coordinate corresponding to z[0]
xlast		x-coordinate corresponding to z[n-1]
fill		= 0 for no fill
			  > 0 for fill between zbase and zmax
			  < 0 for fill between zbase and zmin
		+2 for fill solid between zbase and zmax grey between zbase and zmin
		-2 for fill solid between zbase and zmin grey between zbase and zmax
		SHADING: 2<= abs(fill) <=5   abs(fill)=2 light grey  abs(fill)=5 black

**************************************************************************
NOTES:
psWiggle reduces PostScript output by eliminating linetos when
z values are essentially constant.  The tolerance for detecting 
"constant" z values is ZEPS*(zmax-zmin), where ZEPS is a fraction 
defined below.

A more complete optimization would eliminate all connected line 
segments that are essentially colinear.

psWiggle breaks up the wiggle into segments that can be drawn
without exceeding the PostScript pathlimit.

****************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/03/89
Modified:  Craig Artley, Colorado School of Mines, 04/13/92
           Corrected dead trace bug.  Now the last point of each segment
           is guaranteed to be drawn.
MODIFIED: Paul Michaels, Boise State University, 29 December 2000
           added fill=+/-2 option of solid/grey color scheme
***************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"
#include "psplot.h"

/* small number used to eliminate useless linetos */
#define ZEPS 0.001

/* length of segment to keep current path length under limit */
#define NS 200
static float xc[NS*2],zc[NS*2];

/* some macros to simplify the code */
#define MOVETO(x,z) moveto(xbias+(x)*xscale,zbias+(z)*zscale)
#define LINETO(x,z) lineto(xbias+(x)*xscale,zbias+(z)*zscale)

void psWiggle (
	int n, float z[], float zmin, float zmax, float zbase,
	float yzmin, float yzmax, float xfirst, float xlast, int ifill,
	const char *tracecolor)
/*************************************************************************
psWiggle - draw wiggle-trace with (optional) area-fill via PostScript
**************************************************************************
Inputs:
n		number of samples to draw
z		array to draw
zmin		z values below zmin will be clipped
zmax		z values above zmax will be clipped
zbase		z values between zbase and either zmin or zmax will be filled
yzmin		y-coordinate corresponding to zmin
yzmax		y-coordinate corresponding to zmax
xfirst		x-coordinate corresponding to z[0]
xlast		x-coordinate corresponding to z[n-1]
fill		= 0 for no fill
			  > 0 for fill between zbase and zmax
			  < 0 for fill between zbase and zmin
		+2 for fill solid between zbase and zmax grey between zbase and zmin
		-2 for fill solid between zbase and zmin grey between zbase and zmax
		SHADING: 2<= abs(fill) <=5   abs(fill)=2 light grey  abs(fill)=5 black
tracecolor	pointer to trace color, needed to restore from grey plotting
**************************************************************************
NOTES:
psWiggle reduces PostScript output by eliminating linetos when
z values are essentially constant.  The tolerance for detecting 
"constant" z values is ZEPS*(zmax-zmin), where ZEPS is a fraction 
defined below.

A more complete optimization would eliminate all connected line 
segments that are essentially colinear.

psWiggle breaks up the wiggle into segments that can be drawn
without exceeding the PostScript pathlimit.
****************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/03/89
Modified:  Craig Artley, Colorado School of Mines, 04/13/92
           Corrected dead trace bug.  Now the last point of each segment
           is guaranteed to be drawn.
MODIFIED: Paul Michaels, Boise State University, 29 December 2000
           added fill=+/-2 option of solid/grey color scheme
***************************************************************************/
{
	int ic,nc,k1,k2,il;
	float shade;
	float xscale,xbias,zscale,zbias,xl,zl,zeps,dz;

	/* set up scale and bias factors for plot coordinates */
	xscale = (n>1)?(xlast-xfirst)/(n-1):1.0;
	xbias = xfirst;
	zscale = (zmax!=zmin)?(yzmax-yzmin)/(zmax-zmin):1.0;
	zbias = (zmax!=zmin)?yzmin-zmin*zscale:0.5*(yzmin+yzmax);

	/* determine small z used to eliminate useless linetos */
	zeps = ZEPS*((zmax>zmin)?(zmax-zmin):zmin-zmax);

	/* save graphics state */
	gsave();

	/* translate to zbias to prevent large coordinate values
	   from loosing precision */
	translate (0, zbias);
	zbias = 0;
	
	/* start a new path */
	newpath();

	/* chop off (don't miter or round) sharp corners */
	setlinejoin(2);

	/* draw array in segments of NS samples with 2 sample overlap */
	for (k1=k2=0; k2<n-1; k1+=NS-2) {
		k2 = k1+NS-1;
		if (k2>=n) k2 = n-1;


		/* if filling */
		if (ifill!=0) {

			/* APPLY GREY SHADING if abs(ifill)>=2           */
			if (abs(ifill)>=2)
			{
				/* clip trace values depending on sign of fill */
				if (ifill<0)
					nc = yclip(k2-k1+1,1.0,(float)k1,&z[k1],zbase,zmax,xc,zc);
				else
					nc = yclip(k2-k1+1,1.0,(float)k1,&z[k1],zmin,zbase,xc,zc);

				/* set shading color to grey for opposite of fill */
				/* ifill=2 light grey   ifill=5 black       */
				shade=1-0.2*((float) abs(ifill));		
				if (shade < 0.0 ) shade=0.0;
				setrgbcolor(shade,shade,shade);

				/* make disconnected subpaths for each area to fill */
				for (ic=0,xl=xc[0],zl=zbase; ic<nc; ic++) {

					/* if current z is not the base z */
					if (zc[ic]!=zbase) {
	
						/* if last z was the base z, start subpath */
						if (zl==zbase)
							MOVETO(xl,zl);
					
						/* extend subpath to current z */
						LINETO(xc[ic],zc[ic]);
					
					/* else, if current z is the base z */
					} else {
	
						/* if last z was not the base z, end subpath */
						if (zl!=zbase)
							LINETO(xc[ic],zc[ic]);
					}
	
					/* remember last x and z */
					xl = xc[ic]; zl = zc[ic];
				}

				/* if last z was not the base z, extend subpath to base z */
				if (zl!=zbase)
					LINETO(xl,zbase);

				/* fill the wiggle */
				fill();

				/* restore trace color  */
				setcolor(tracecolor);
			}  /*  endif GREY SHADING       */
			
			/* clip trace values depending on sign of fill */
			if (ifill>0)
				nc = yclip(k2-k1+1,1.0,(float)k1,&z[k1],zbase,zmax,xc,zc);
			else
				nc = yclip(k2-k1+1,1.0,(float)k1,&z[k1],zmin,zbase,xc,zc);

			/* make disconnected subpaths for each area to fill */
			for (ic=0,xl=xc[0],zl=zbase; ic<nc; ic++) {

				/* if current z is not the base z */
				if (zc[ic]!=zbase) {

					/* if last z was the base z, start subpath */
					if (zl==zbase)
						MOVETO(xl,zl);
				
					/* extend subpath to current z */
					LINETO(xc[ic],zc[ic]);
				
				/* else, if current z is the base z */
				} else {

					/* if last z was not the base z, end subpath */
					if (zl!=zbase)
						LINETO(xc[ic],zc[ic]);
				}

				/* remember last x and z */
				xl = xc[ic]; zl = zc[ic];
			}

			/* if last z was not the base z, extend subpath to base z */
			if (zl!=zbase)
				LINETO(xl,zbase);

			/* fill the wiggle */
			fill();
		}  /* end fill block  */
		
				/* clip trace values between zmin and zmax */
		nc = yclip(k2-k1+1,1.0,(float)k1,&z[k1],zmin,zmax,xc,zc);

		/* stroke trace values, avoiding linetos for nearly constant z */
		MOVETO(xc[0],zc[0]);
		for (ic=1,il=0,zl=zc[0]; ic<nc; ic++) {
			dz = zc[ic]-zl;
			if (dz<-zeps || dz>zeps) {
				if (il!=ic-1)
					LINETO(xc[ic-1],zc[ic-1]);
				LINETO(xc[ic],zc[ic]);
				il = ic;
				zl = zc[ic];
			}
		}
		if (il!=nc-1)
			LINETO(xc[nc-1],zc[nc-1]);
		stroke();

	}
		
	/* restore graphics state */
	grestore();
}
