/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* XCONTOUR: $Revision: 1.7 $ ; $Date: 2011/11/21 17:02:44 $	*/

/*********************** self documentation **********************/
/**************************************************************************
XCONTOUR - draw contour of a two-dimensional array via X vectorplot calls

xContour	draw contour of a two-dimensional array via X vector plot calls

***************************************************************************
Function Prototype:
void xContour(Display *dpy, Window win,GC gcc, GC gcl, 
	       float *cp,int nx, float x[], int ny, float y[], float z[], 
	       char lcflag,char *lcf,char *lcc, float *w, int nplaces)
***************************************************************************
Input:
c			contour value
nx			number of x-coordinates
x			array of x-coordinates (see notes below)
ny			number of y-coordinates
y			array of y-coordinates (see notes below)
lcf			font name of contour label
lcc			color of contour label
Least Significat Bits:
z			array of nx*ny z(x,y) values (see notes below)
w			array of nx*ny z(x,y) values (see notes below)
***************************************************************************
Notes:
The two-dimensional array z is actually passed as a one-dimensional
array containing nx*ny values, stored with nx fast and ny slow.

The x and y arrays define a grid that is not necessarily 
uniformly-sampled.  Linear interpolation of z values on the 
grid defined by the x and y arrays is used to determine z values 
between the gridpoints.
		 
The two least significant bits of z are used to mark intersections
of the contour with the x,y grid; therefore, the z values will
almost always be altered (slightly) by contour.

xContour is a modified version of psContour where the use of conmove
and condraw call have been changed to match X-Windows.

Since XDrawLine requires a start and end point, the use of a manually update
of the position variables x0 and y0 is used instead of conmove.

The w array is used to restrict the range of contour labeling that 
occurs only if w<1. 

As suggested in the reference, the following scheme is used to refer
to a cell of the two-dimensional array z:

                north (0)
      (ix,iy+1)	--------- (ix+1,iy+1)
                | cell  |
       west (3)	| ix,iy	| east (1)
                |       |
        (ix,iy) --------- (ix+1,iy)
                south (2)

***************************************************************************
Reference:
Cottafava, G. and Le Moli, G., 1969, Automatic contour map:
Commuincations of the ACM, v. 12, n. 7, July, 1969.

***************************************************************************
Author:  Morten Wendell Pedersen Aarhus University 07/20/96
Heavily based on psContour by
  Dave Hale, Colorado School of Mines, 06/28/89
  and with contour labeling added by: Zhenyue Liu, June 1993
(actually most of the credit should go to these two guys)
***************************************************************************/
/**************** end self doc ********************************/

#include "xplot.h"
#define BTWN(a,b,c) (MIN((b),(c))<=(a) && (a)<MAX((b),(c)))
#define DELTA(a,b,c) ((b)!=(c)?((a)-(b))/((c)-(b)):1.0)

/* warning:  these bit masks are non-portable! */
#define SOUTH 01
#define WEST 02

#define SETS(z) (*((int *)(&(z))) |= SOUTH)
#define CLRS(z) (*((int *)(&(z))) &= ~SOUTH)
#define SSET(z) (*((int *)(&(z))) & SOUTH)
#define SETW(z) (*((int *)(&(z))) |= WEST)
#define CLRW(z) (*((int *)(&(z))) &= ~WEST)
#define WSET(z) (*((int *)(&(z))) & WEST)

/* forward declarations of functions defined internally */
static int connect(Display *dpy, Window win,GC gcc,
		   float c, int nx, float *x, int ny, float *y,float *z,
		   int *pcell, float *pxd, float *pyd,float *x0,float *y0);
static void labelc(Display *dpy, Window win, GC gcc,float x, float y,
		   float xw, float yw, char *str,char *font,char *color);
static void wcell (int nx, float *x, int ny, float *y, float *w, 
	int cell, float xc, float yc, float xw, float yw);
 
/* function */
void xContour(Display *dpy, Window win,GC gcc, GC gcl, 
	       float *cp,int nx, float x[], int ny, float y[], float z[], 
	       char lcflag,char *lcf,char *lcc, float *w, int nplaces)
/**************************************************************************
xContour - draw contour of a two-dimensional array via X
***************************************************************************
Input:
dpy                     the display to make the contour
win                     the window to draw in
gcc                     GC for the contour lines
gcl                     GC for the contour labels
cp			pointer to the contour value
nx			number of x-coordinates
x			array of x-coordinates (see notes below)
ny			number of y-coordinates
y			array of y-coordinates (see notes below)
lcflag                  flag that defines if we actually are going to have labels 
Least Significat Bits:
z			array of nx*ny z(x,y) values (see notes below)
w			array of nx*ny z(x,y) values (see notes below)
***************************************************************************
Notes:
The two-dimensional array z is actually passed as a one-dimensional
array containing nx*ny values, stored with nx fast and ny slow.

The x and y arrays define a grid that is not necessarily 
uniformly-sampled.  Linear interpolation of z values on the 
grid defined by the x and y arrays is used to determine z values 
between the gridpoints.
		 
The two least significant bits of z are used to mark intersections
of the contour with the x,y grid; therefore, the z values will
almost always be altered (slightly) by contour.

xContour is a modified version of psContour where the use of conmove
and condraw call have been changed to match X-Windows.

Since XDrawLine requires a start and end point, the use of a manually update
of the position variables x0 and y0 is used instead of conmove.

if lcflag is zero no labels are drawn

The w array is used to restrict the range of contour labeling that 
occurs only if w<1. 

As suggested in the reference, the following scheme is used to refer
to a cell of the two-dimensional array z:

                north (0)
      (ix,iy+1)	--------- (ix+1,iy+1)
                | cell  |
       west (3)	| ix,iy	| east (1)
                |       |
        (ix,iy) --------- (ix+1,iy)
                south (2)

***************************************************************************
Reference:
Cottafava, G. and Le Moli, G., 1969, Automatic contour map:
Commuincations of the ACM, v. 12, n. 7, July, 1969.

***************************************************************************
Author:  Morten Wendell Pedersen Aarhus University 07/20/96
Heavily based on psContour by
  Dave Hale, Colorado School of Mines, 06/28/89
  and with contour labeling added by: Zhenyue Liu, June 1993
(actually most of the credit should go to these two guys)
***************************************************************************/
{
	int ix,iy,non,cell,startcell;
	float d;
	float xmin = MIN(x[0],x[nx-1]), xmax = MAX(x[0],x[nx-1]);
	float ymin = MIN(y[0],y[ny-1]), ymax = MAX(y[0],y[ny-1]);
	float xc=0.0, yc=0.0;	/* contour labeling centered at (xc,yc)	*/
 	float xw, yw;	/* width and length of contour labeling */
	float xd, yd;	/* point on contour			*/
	float x0, y0;    /* start plot values (instead of move operation )*/   
	float xdmin, xdmax, ydmin, ydmax; /* range of contour	*/
	int id;	/* =0 if a point on contour has been used as (xc,yc) */
	int cells=0;
	char str[20];
	XCharStruct overall;
	int dummy;
	float c=*cp; /* stupid thing I had to do since I couldn't transfer
			a float without messing up everything 
			but I could transfer a pointer....strange thing
			I will have to track this thing down one day*/

	/* convert a number into a string */
	sprintf(str,"%.*g",nplaces,c);

	/* determine length and width for printing the string */
	XQueryTextExtents(dpy,XGContextFromGC(gcl),str,(int) strlen(str),&dummy,&dummy,&dummy,&overall);
	xw=overall.width;
        yw=overall.ascent+overall.descent;

	/* restrict contour labeling from edges */
	for (iy=0; iy<ny-1; iy++)  
		for (ix=0,cell=iy*nx; ix<nx-1; ix++,cell++) {
			if(x[ix]<xmin+2.0*xw || x[ix]>xmax-2.0*xw 
 				|| y[iy]<ymin+yw || y[iy]>ymax-yw)
  				w[cell] += 1.;
 		}
 
	/* count intersections with cell boundaries */
	non = 0;

	/* find all the intersections */
	for (iy=0; iy<ny; iy++) {
		for (ix=0,cell=iy*nx; ix<nx; ix++,cell++) {

			/* check for intersection with west edge of cell */
			if (iy<ny-1 && BTWN(c,z[cell],z[cell+nx])) {
				SETW(z[cell]);  non++;
			} else {
				CLRW(z[cell]);
			}

			/* check for intersection with south edge of cell */
			if (ix<nx-1 && BTWN(c,z[cell],z[cell+1])) {
				SETS(z[cell]);  non++;
			} else {
				CLRS(z[cell]);
			}
		}
	}


	/* follow contours intersecting north boundary */
	for (ix=0,startcell=(ny-1)*nx; non>0&&ix<nx-1; ix++,startcell++) {
		if (SSET(z[startcell])) {
			d = DELTA(c,z[startcell],z[startcell+1]);
			x0=(1.0-d)*x[ix]+d*x[ix+1];
			y0=y[ny-1];
			CLRS(z[startcell]);  non--;
			cell = startcell-nx;
			id = 1; 
			xdmin = xmax;
			xdmax = xmin;
			ydmin = ymax;
			ydmax = ymin;
			while (connect(dpy, win,gcc,c,nx,x,ny,y,z,&cell,&xd,&yd,&x0,&y0)){
			  x0=xd;y0=yd;
				 non--;
				if(w[cell]<0.5 && id) {
					xc = xd; yc = yd;
					cells = cell;
					id = 0;
				}
				xdmin = MIN(xdmin,xd);
				xdmax = MAX(xdmax,xd);
				ydmin = MIN(ydmin,yd);
				ydmax = MAX(ydmax,yd);
			}
			if(lcflag && id==0 && xdmax+ydmax-xdmin-ydmin>xw+yw) {
				wcell(nx,x,ny,y,w,cells,xc,yc,xw,yw);
 				labelc(dpy,win,gcl,xc-xw/2,yc-yw/2,xw,yw,str,lcf,lcc);
			}
		}
	}

	/* follow contours intersecting east boundary */
	for (iy=0,startcell=nx-1; non>0&&iy<ny-1; iy++,startcell+=nx) {
		if (WSET(z[startcell])) {
			d = DELTA(c,z[startcell],z[startcell+nx]);
			x0=x[nx-1];
			y0=(1.0-d)*y[iy]+d*y[iy+1];
			CLRW(z[startcell]);  non--;
			cell = startcell-1;
			id = 1; 
			xdmin = xmax;
			xdmax = xmin;
			ydmin = ymax;
			ydmax = ymin;
			while (connect(dpy, win,gcc,c,nx,x,ny,y,z,&cell,&xd,&yd,&x0,&y0)){
			  x0=xd;y0=yd;
				 non--;
				if(w[cell]<0.5 && id) {
					xc = xd; yc = yd;
					cells = cell;
					id = 0;
				}
				xdmin = MIN(xdmin,xd);
				xdmax = MAX(xdmax,xd);
				ydmin = MIN(ydmin,yd);
				ydmax = MAX(ydmax,yd);
			}
			if(lcflag && id==0 && xdmax+ydmax-xdmin-ydmin>xw+yw) {
				wcell(nx,x,ny,y,w,cells,xc,yc,xw,yw);
 				labelc(dpy,win,gcl,xc-xw/2,yc-yw/2,xw,yw,str,lcf,lcc);
			}
		}
	}

	/* follow contours intersecting south boundary */
	for (ix=0,startcell=0; non>0&&ix<nx-1; ix++,startcell++) {
		if (SSET(z[startcell])) {
			d = DELTA(c,z[startcell],z[startcell+1]);
			x0=(1.0-d)*x[ix]+d*x[ix+1];
			y0=y[0];
			CLRS(z[startcell]);  non--;
			cell = startcell;
			id = 1; 
			xdmin = xmax;
			xdmax = xmin;
			ydmin = ymax;
			ydmax = ymin;
			while (connect(dpy, win,gcc,c,nx,x,ny,y,z,&cell,&xd,&yd,&x0,&y0)){
			  x0=xd;y0=yd;
				 non--;
				if(w[cell]<0.5 && id) {
					xc = xd; yc = yd;
					cells = cell;
					id = 0;
				}
				xdmin = MIN(xdmin,xd);
				xdmax = MAX(xdmax,xd);
				ydmin = MIN(ydmin,yd);
				ydmax = MAX(ydmax,yd);
			}
			if(lcflag && id==0 && xdmax+ydmax-xdmin-ydmin>xw+yw) {
				wcell(nx,x,ny,y,w,cells,xc,yc,xw,yw);
 				labelc(dpy,win,gcl,xc-xw/2,yc-yw/2,xw,yw,str,lcf,lcc);
			}
		}		 
	}

	/* follow contours intersecting west boundary */
	for (iy=0,startcell=0; non>0&&iy<ny-1; iy++,startcell+=nx) {
		if (WSET(z[startcell])) {
			d = DELTA(c,z[startcell],z[startcell+nx]);
			x0=x[0];
			y0=(1.0-d)*y[iy]+d*y[iy+1];
			CLRW(z[startcell]);  non--;
			cell = startcell;
			id = 1; 
			xdmin = xmax;
			xdmax = xmin;
			ydmin = ymax;
			ydmax = ymin;
			while (connect(dpy, win,gcc,c,nx,x,ny,y,z,&cell,&xd,&yd,&x0,&y0)){
			  x0=xd;y0=yd;
				 non--;
				if(w[cell]<0.5 && id) {
					xc = xd; yc = yd;
					cells = cell;
					id = 0;
				}
				xdmin = MIN(xdmin,xd);
				xdmax = MAX(xdmax,xd);
				ydmin = MIN(ydmin,yd);
				ydmax = MAX(ydmax,yd);
			}
			if(lcflag && id==0 && xdmax+ydmax-xdmin-ydmin>xw+yw) {
				wcell(nx,x,ny,y,w,cells,xc,yc,xw,yw);
 				labelc(dpy,win,gcl,xc-xw/2,yc-yw/2,xw,yw,str,lcf,lcc);
			}
		}
	}

	/* follow interior contours */
	for (iy=1; iy<ny-1; iy++) {
		for (ix=0,startcell=iy*nx; non>0&&ix<nx-1; ix++,startcell++) {

			/* check south edge of cell */
			if (SSET(z[startcell])) {
				d = DELTA(c,z[startcell],z[startcell+1]);
				x0=(1.0-d)*x[ix]+d*x[ix+1];
				y0=y[iy];

				/* clear south edge where we started */
				CLRS(z[startcell]);  non--;

				cell = startcell;

				/* if another intersection exists in this cell */
				if (connect(dpy, win,gcc,c,nx,x,ny,y,z,&cell,&xd,&yd,&x0,&y0)) {
				  x0=xd;y0=yd;
			  
				  /* set south edge so that we finish where we started */
					SETS(z[startcell]);  non++;
			
					/* follow the contour */
			id = 1; 
			xdmin = xmax;
			xdmax = xmin;
			ydmin = ymax;
			ydmax = ymin;
			while (connect(dpy, win,gcc,c,nx,x,ny,y,z,&cell,&xd,&yd,&x0,&y0)){
			  x0=xd;y0=yd;
				 non--;
				if(w[cell]<0.5 && id) {
					xc = xd; yc = yd;
					cells = cell;
					id = 0;
				}
				xdmin = MIN(xdmin,xd);
				xdmax = MAX(xdmax,xd);
				ydmin = MIN(ydmin,yd);
				ydmax = MAX(ydmax,yd);
			}
			if(lcflag && id==0 && xdmax+ydmax-xdmin-ydmin>xw+yw) {
				wcell(nx,x,ny,y,w,cells,xc,yc,xw,yw);
 				labelc(dpy,win,gcl,xc-xw/2,yc-yw/2,xw,yw,str,lcf,lcc);
			}	
				} 		 
			}
		}
	}
	/* contour drawing is done */
}

static int connect (Display *dpy, Window win,GC gcc,
		    float c, int nx, float *x, int ny, float *y, float *z, 
		    int *pcell, float *pxd, float *pyd,float *x0, float *y0)
/*
connect draws a line from one intersection of the cell(ix,iy) =  z[*pcell]
to another intersection of the cell, provided the latter intersection exists,
and then clears the latter intersection and updates the cell pointer
connect returns 0 if the latter intersection does not exist or if the 
latter intersection is a grid boundary; otherwise returns 1.
*/
{
	int cell= *pcell, ix=cell%nx, iy=cell/nx;
	float d;

	/* if exiting north */
	if (SSET(z[cell+nx])) {
		cell += nx;  iy++;
		d = DELTA(c,z[cell],z[cell+1]);
		*pxd = (1.0-d)*x[ix]+d*x[ix+1];
		*pyd = y[iy];
		XDrawLine(dpy,win,gcc,NINT(*x0),NINT(*y0),
				NINT(*pxd),NINT(*pyd));
		CLRS(z[cell]);
		*pcell = cell;
		if (iy<ny-1)
			return(1);
		else
			return(0);

	/* else if exiting east */
	} else if (WSET(z[cell+1])) {
		cell += 1;  ix++;
		d = DELTA(c,z[cell],z[cell+nx]);
		*pxd = x[ix];
		*pyd = (1.0-d)*y[iy]+d*y[iy+1];
		XDrawLine(dpy,win,gcc,NINT(*x0),NINT(*y0),
				NINT(*pxd),NINT(*pyd));
		CLRW(z[cell]);
		*pcell = cell;
		if (ix<nx-1)
			return(1);
		else
			return(0);

	/* else if exiting south */
	} else if (SSET(z[cell])) {
		d = DELTA(c,z[cell],z[cell+1]);
		*pxd = (1.0-d)*x[ix]+d*x[ix+1];
		*pyd = y[iy];
		XDrawLine(dpy,win,gcc,NINT(*x0),NINT(*y0),
				NINT(*pxd),NINT(*pyd));
		CLRS(z[cell]);
		*pcell = cell-nx;
		if (iy>0)
			return(1);
		else
			return(0);

	/* else if exiting west */
	} else if (WSET(z[cell])) {
		d = DELTA(c,z[cell],z[cell+nx]);
		*pxd = x[ix];
		*pyd = (1.0-d)*y[iy]+d*y[iy+1];
		XDrawLine(dpy,win,gcc,NINT(*x0),NINT(*y0),
				NINT(*pxd),NINT(*pyd));
		CLRW(z[cell]);
		*pcell = cell-1;
		if (ix>0)
			return(1);
		else
			return(0);

	/* else if no intersection exists */
	} else {
		return(0);
	}
}


/* 	draw a label on contour		*/
static void labelc(Display *dpy, Window win, GC gcl, float x, float y, float xw, float yw, char *str, char *font,char *color)
{
	XClearArea(dpy,win,NINT(x),NINT(y),
			(unsigned int) NINT(xw),
				(unsigned int) NINT(yw),False);

	XDrawString(dpy,win,gcl,NINT(x),NINT(y+yw),
		str,(int) strlen(str));
	if(font-font) color += 0; /* keep compiler happy */ 
}

/*	update the w array	*/
static void wcell (int nx, float *x, int ny, float *y, float *w, 
	int cell, float xc, float yc, float xw, float yw)
 
{
	int  ix=cell%nx, iy=cell/nx, ixl=ix, ixh=ix, iyl=iy, iyh=iy;
 
	while(x[ixl]>=xc-2.0*xw && x[ixl]<=xc+2.0*xw && ixl>0) ixl--;
	while(x[ixh]>=xc-2.0*xw && x[ixh]<=xc+2.0*xw && ixh<nx-1) ixh++;
	while(y[iyl]>=yc-1.5*yw && y[iyl]<=yc+1.5*yw && iyl>0) iyl--;
	while(y[iyh]>=yc-1.5*yw && y[iyh]<=yc+1.5*yw && iyh<ny-1) iyh++;

	for(iy=iyl; iy<=iyh; ++iy)
		for(ix=ixl,cell=iy*nx+ixl; ix<=ixh; ix++, cell++)
			w[cell] +=1.0; 

 }

