/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* FX: $Revision: 1.6 $ ; $Date: 2011/11/21 17:05:31 $	*/

/*********************** self documentation **********************/
/*****************************************************************************
FX - Functions to support floating point coordinates in X

FMapFX			map float x to x
FMapFY			map float y to y
FMapFWidth		map float width to width
FMapFHeight		map float height to height
FMapFAngle		map float angle to angle
FMapFPoint		map float x,y to x,y
FMapFPoints		map float points to points
FMapX			inverse map x to float x
FMapY			inverse map y to float y
FMapWidth		inverse map width to float width
FMapHeight		inverse map height to float height
FMapAngle		inverse map angle to float angle
FMapPoint		map x,y to float x,y
FMapPoints		map points to float points
FSetGC			set graphics context
FSetMap			set map (scales and shifts)
FSetClipRectangle	set clip rectangle
FClipOn			turn clip on
FClipOff		turn clip off
FClipPoint		clip point
FClipLine		clip line
FClipRectangle		clip rectangle
FXCreateFGC		create float graphics context
FXFreeFGC		free float graphic context
FXDrawPoint		draw point at float x,y (with clipping)
FXDrawPoints		draw float points (with clipping)
FXDrawLine		draw line from float x1,y1 to float x2,y2
				(with clipping)
FXDrawLines		draw lines between float points (with clipping)
FXDrawRectangle		draw rectangle with float x,y,width,height
				(with clipping)
FXDrawArc		draw arc with float x,y,width,height,angle1,angle2
FXDrawString		draw string at float x,y
FXFillRectangle		fill rectangle with float x,y,width,height (with
				clipping)

******************************************************************************
Function Prototypes:
int FMapFX (FGC fgc, float fx);
int FMapFY (FGC fgc, float fy);
int FMapFWidth (FGC fgc, float fwidth);
int FMapFHeight (FGC fgc, float fheight);
int FMapFAngle (FGC fgc, float fangle);
void FMapFPoint (FGC fgc, float fx, float fy, int *x_return, int *y_return);
void FMapFPoints (FGC fgc, FXPoint fpoints[], int npoints, 
	XPoint points_return[]);
float FMapX (FGC fgc, int x);
float FMapY (FGC fgc, int y);
float FMapWidth (FGC fgc, int width);
float FMapHeight (FGC fgc, int height);
float FMapAngle (FGC fgc, int angle);
void FMapPoint (FGC fgc, int x, int y, float *fx_return, float *fy_return);
void FMapPoints (FGC fgc, XPoint points[], int npoints, 
	FXPoint fpoints_return[]);
void FSetGC (FGC fgc, GC gc);
void FSetMap (FGC fgc, int x, int y, int width, int height,
	float fx, float fy, float fwidth, float fheight);
void FSetClipRectangle(FGC fgc, float fxa, float fya, float fxb, float fyb);
void FClipOn (FGC fgc);
void FClipOff (FGC fgc);
int FClipPoint (FGC fgc, float fx, float fy);
int FClipLine (FGC fgc, float fx1, float fy1, float fx2, float fy2,
	float *fx1c, float *fy1c, float *fx2c, float *fy2c);
int FClipRectangle (FGC fgc, float fx, float fy, float fwidth, float fheight,
	float *fxc, float *fyc, float *fwidthc, float *fheightc);
FGC FXCreateFGC (GC gc, int x, int y, int width, int height,
	float fx, float fy, float fwidth, float fheight);
void FXFreeFGC (FGC fgc);
void FXDrawPoint (Display *display, Drawable d, FGC fgc, float fx, float fy);
void FXDrawPoints (Display *display, Drawable d, FGC fgc, 
	FXPoint fpoints[], int npoints, int mode);
void FXDrawLine (Display *display, Drawable d, FGC fgc,
	float fx1, float fy1, float fx2, float fy2);
void FXDrawLines (Display *display, Drawable d, FGC fgc,
	FXPoint fpoints[], int npoints, int mode);
void FXDrawRectangle (Display *display, Drawable d, FGC fgc, 
	float fx, float fy, float fwidth, float fheight);
void FXDrawArc (Display *display, Drawable d, FGC fgc,
	float fx, float fy, float fwidth, float fheight, 
	float fangle1, float fangle2);
void FXDrawString (Display *display, Drawable d, FGC fgc, 
	float fx, float fy, char *string, int length);
void FXFillRectangle (Display *display, Drawable d, FGC fgc, 
	float fx, float fy, float fwidth, float fheight);

******************************************************************************
Notes:
The functions defined below are designed to resemble the equivalent 
X functions.  For example, FXDrawLine() is analogous to XDrawLine.
Each of the FXDraw<xxx>() functions requires an FGC instead of a GC
(graphics context).  An FGC contains a GC, along with the information 
required to transform floating point coordinates to integer (pixel) 
coordinates.

Additional functions are provided to transform floating point coordinates
to integer coordinates and vice versa.  Where feasible, macros are also
provided to perform these coordinate transformations.

Clipping of floating point coordinates is supported, because clipping
after mapping to integer coordinates is not valid when the mapped
integer coordinates overflow the range of short integers.  By clipping
the floating point coordinates before mapping to integers, this overflow
can be avoided.  By default, clipping is turned off until a clip rectangle
is specified or until clipping is explicitly turned on.  Clipping is
not currently supported for all FXDraw functions.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/24/90
Modified:  Dave Hale, Colorado School of Mines, 05/18/91
	Added floating point clipping capability to some FXDraw functions.
******************************************************************************/
/**************** end self doc ********************************/

#include "Xtcwp/Xtcwp.h"

#define NPBUF 512	/* size of temporary arrays of points */

/* map float x to x */
int FMapFX(FGC fgc, float fx)
{
	return(fgc->xshift+fx*fgc->xscale);
}

/* map float y to y */
int FMapFY(FGC fgc, float fy)
{
	return(fgc->yshift+fy*fgc->yscale);
}

/* map float width to width */
int FMapFWidth(FGC fgc, float fwidth)
{
	return(fwidth*fgc->xscale);
}

/* map float height to height */
int FMapFHeight(FGC fgc, float fheight)
{
	return(fheight*fgc->yscale);
}

/* map float angle to angle */
int FMapFAngle(FGC fgc, float fangle)
{
	return(fangle*64.0 + 0.0*fgc->xscale);
}

/* map float x,y to x,y */
void FMapFPoint(FGC fgc, float fx, float fy, int *x_return, int *y_return)
{
	*x_return = MapFX(fgc,fx);
	*y_return = MapFY(fgc,fy);
}

/* map float points to points */
void FMapFPoints(FGC fgc, FXPoint fpoints[], int npoints, 
	XPoint points_return[])
{
	int i;
	for (i=0; i<npoints; i++) {
		points_return[i].x = MapFX(fgc,fpoints[i].fx);
		points_return[i].y = MapFY(fgc,fpoints[i].fy);
	}
}

/* inverse map x to float x */
float FMapX(FGC fgc, int x)
{
	return((x-fgc->xshift)/fgc->xscale);
}

/* inverse map y to float y */
float FMapY(FGC fgc, int y)
{
	return((y-fgc->yshift)/fgc->yscale);
}

/* inverse map width to float width */
float FMapWidth(FGC fgc, int width)
{
	return(width/fgc->xscale);
}

/* inverse map height to float height */
float FMapHeight(FGC fgc, int height)
{
	return(height/fgc->yscale);
}

/* inverse map angle to float angle */
float FMapAngle(FGC fgc, int angle)
{
	return(angle/64.0 + 0.0*fgc->xscale);
}

/* map x,y to float x,y */
void FMapPoint(FGC fgc, int x, int y, float *fx_return, float *fy_return)
{
	*fx_return = MapX(fgc,x);
	*fy_return = MapY(fgc,y);
}

/* map points to float points */
void FMapPoints(FGC fgc, XPoint points[], int npoints, 
	FXPoint fpoints_return[])
{
	int i;
	for (i=0; i<npoints; i++) {
		fpoints_return[i].fx = MapX(fgc,points[i].x);
		fpoints_return[i].fy = MapY(fgc,points[i].y);
	}
}

/* set graphics context */
void FSetGC(FGC fgc, GC gc)
{
	fgc->gc = gc;
}

/* set map (scales and shifts) */
void FSetMap(FGC fgc, int x, int y, int width, int height,
	float fx, float fy, float fwidth, float fheight)
{
	fgc->xscale = width/fwidth;
	fgc->yscale = height/fheight;
	fgc->xshift = x-fx*fgc->xscale;
	fgc->yshift = y-fy*fgc->yscale;
}

/* set clip rectangle */
void FSetClipRectangle(FGC fgc, float xa, float ya, float xb, float yb)
{
	if (xa<xb) {
		fgc->xmin = xa;
		fgc->xmax = xb;
	} else {
		fgc->xmin = xb;
		fgc->xmax = xa;
	}
	if (ya<yb) {
		fgc->ymin = ya;
		fgc->ymax = yb;
	} else {
		fgc->ymin = yb;
		fgc->ymax = ya;
	}
	FClipOn(fgc);
}

/* turn clip on */
void FClipOn(FGC fgc)
{
	fgc->clip = 1;
}

/* turn clip off */
void FClipOff(FGC fgc)
{
	fgc->clip = 0;
}

/* clip point */
int FClipPoint (FGC fgc, float x, float y)
{
	return x>=fgc->xmin && x<=fgc->xmax && y>=fgc->ymin && y<=fgc->ymax;
}

/* clip line */
int FClipLine (FGC fgc, float x1, float y1, float x2, float y2,
	float *x1c, float *y1c, float *x2c, float *y2c)
{
	int code1,code2;
	float xmin=fgc->xmin,xmax=fgc->xmax,ymin=fgc->ymin,ymax=fgc->ymax,
		dyodx=0.0,dxody=0.0;
	
	/* compute Cohen and Sutherland codes */
	code1 = 0;
	if (x1<xmin) code1 |= 1;
	if (x1>xmax) code1 |= 2;
	if (y1<ymin) code1 |= 4;
	if (y1>ymax) code1 |= 8;
	code2 = 0;
	if (x2<xmin) code2 |= 1;
	if (x2>xmax) code2 |= 2;
	if (y2<ymin) code2 |= 4;
	if (y2>ymax) code2 |= 8;
	
	/* if line is entirely inside or outside clip rectangle */
	if ((code1|code2)==0) {
		*x1c = x1;
		*y1c = y1;
		*x2c = x2;
		*y2c = y2;
		return 1;
	} else if ((code1&code2)!=0) {
		return 0;
	}
	
	/* compute line slopes */
	if (x2!=x1) dyodx = (y2-y1)/(x2-x1);
	if (y2!=y1) dxody = (x2-x1)/(y2-y1);
	
	/* adjust first endpoint */
	if (code1!=0) {
		if (x1<xmin) {
			y1 += (xmin-x1)*dyodx;
			x1 = xmin;
		} else if (x1>xmax) {
			y1 += (xmax-x1)*dyodx;
			x1 = xmax;
		}
		if (y1<ymin) {
			x1 += (ymin-y1)*dxody;
			y1 = ymin;
		} else if (y1>ymax) {
			x1 += (ymax-y1)*dxody;
			y1 = ymax;
		}
	}
	
	/* if first endpoint still not inside */
	if (x1<xmin || x1>xmax || y1<ymin || y1>ymax) return 0;
	
	/* adjust second endpoint */
	if (code2!=0) {
		if (x2<xmin) {
			y2 += (xmin-x2)*dyodx;
			x2 = xmin;
		} else if (x2>xmax) {
			y2 += (xmax-x2)*dyodx;
			x2 = xmax;
		}
		if (y2<ymin) {
			x2 += (ymin-y2)*dxody;
			y2 = ymin;
		} else if (y2>ymax) {
			x2 += (ymax-y2)*dxody;
			y2 = ymax;
		}
	}
	
	/* if second endpoint still not inside */
	if (x2<xmin || x2>xmax || y2<ymin || y2>ymax) return 0;
	
	/* returned clipped endpoints */
	*x1c = x1;
	*y1c = y1;
	*x2c = x2;
	*y2c = y2;
	return 1;
}

/* clip rectangle */
int FClipRectangle (FGC fgc, float x, float y, float width, float height,
	float *xc, float *yc, float *widthc, float *heightc)
{
	float xmin=fgc->xmin,xmax=fgc->xmax,ymin=fgc->ymin,ymax=fgc->ymax;
	float x1=x,y1=y,x2=x+width,y2=y+height;
	
	if (x1<xmin) x1 = xmin;
	if (x1>xmax) x1 = xmax;
	if (y1<ymin) y1 = ymin;
	if (y1>ymax) y1 = ymax;
	if (x2<xmin) x2 = xmin;
	if (x2>xmax) x2 = xmax;
	if (y2<ymin) y2 = ymin;
	if (y2>ymax) y2 = ymax;
	*xc = x1;
	*yc = y1;
	*widthc = x2-x1;
	*heightc = y2-y1;
	return (*widthc!=0.0 && *heightc!=0.0);
}

/* create float graphics context */
FGC FXCreateFGC(GC gc, int x, int y, int width, int height,
	float fx, float fy, float fwidth, float fheight)
{
	FGC fgc;
	fgc = (FGC)malloc(sizeof(*fgc));
	FSetGC(fgc,gc);
	FSetMap(fgc,x,y,width,height,fx,fy,fwidth,fheight);
	FClipOff(fgc);
	return(fgc);
}

/* free float graphic context */
void FXFreeFGC(FGC fgc)
{
	free(fgc);
}

/* draw point at float x,y (with clipping) */
void FXDrawPoint(Display *display, Drawable d, FGC fgc, float fx, float fy)
{
	if (!fgc->clip || FClipPoint(fgc,fx,fy))
		XDrawPoint(display,d,fgc->gc,MapFX(fgc,fx),MapFY(fgc,fy));
}

/* draw float points (with clipping) */
void FXDrawPoints(Display *display, Drawable d, FGC fgc, 
	FXPoint fpoints[], int npoints, int mode)
{
	int i,nbuf;
	FXPoint *fbuf;
	XPoint *points,pbuf[NPBUF];
	if (npoints>NPBUF)
		points = (XPoint *)malloc(npoints*sizeof(*points));
	else
		points = pbuf;
	if (fgc->clip) {
		fbuf = malloc(npoints*sizeof(*fbuf));
		for (i=nbuf=0; i<npoints; ++i)
			if (FClipPoint(fgc,fpoints[i].fx,fpoints[i].fy))
				fbuf[nbuf++] = fpoints[i];
		FMapFPoints(fgc,fbuf,nbuf,points);
		XDrawPoints(display,d,fgc->gc,points,nbuf,mode);
		free(fbuf);
	} else {
		FMapFPoints(fgc,fpoints,npoints,points);
		XDrawPoints(display,d,fgc->gc,points,npoints,mode);
	}
	if (npoints>NPBUF) free(points);
}

/* draw line from float x1,y1 to float x2,y2 (with clipping) */
void FXDrawLine(Display *display, Drawable d, FGC fgc,
	float fx1, float fy1, float fx2, float fy2)
{
	if (!fgc->clip || FClipLine(fgc,fx1,fy1,fx2,fy2,&fx1,&fy1,&fx2,&fy2))
		XDrawLine(display,d,fgc->gc,
			MapFX(fgc,fx1),MapFY(fgc,fy1),
			MapFX(fgc,fx2),MapFY(fgc,fy2));
}

/* draw lines between float points (with clipping) */
void FXDrawLines(Display *display, Drawable d, FGC fgc,
	FXPoint fpoints[], int npoints, int mode)
{
	int i,nbuf,flush;
	float x1,y1,x2,y2,cx1,cy1,cx2,cy2;
	FXPoint *fbuf;
	XPoint *points,pbuf[NPBUF];
	if (npoints>NPBUF)
		points = (XPoint *)malloc(npoints*sizeof(*points));
	else
		points = pbuf;
	if (fgc->clip) {
		fbuf = malloc(npoints*sizeof(*fbuf));
		flush = nbuf = 0;
		x2 = fpoints[0].fx;
		y2 = fpoints[0].fy;
		for (i=1; i<npoints; ++i) {
			x1 = x2;
			y1 = y2;
			x2 = fpoints[i].fx;
			y2 = fpoints[i].fy;
			if (mode==CoordModePrevious) {
				x2 += x1;
				y2 += x1;
			}
			if (!FClipLine(fgc,x1,y1,x2,y2,&cx1,&cy1,&cx2,&cy2)) {
				continue;
			} else {
				if (nbuf==0) {
					fbuf[nbuf].fx = cx1;
					fbuf[nbuf].fy = cy1;
					nbuf++;
				}
				fbuf[nbuf].fx = cx2;
				fbuf[nbuf].fy = cy2;
				nbuf++;
			}
			if (cx2!=x2 || cy2!=y2 || i==npoints-1) flush = 1;
			if (flush && nbuf>0) {
				FMapFPoints(fgc,fbuf,nbuf,points);
				XDrawLines(display,d,fgc->gc,points,nbuf,
					CoordModeOrigin);
				flush = nbuf = 0;
			}
		}
		free(fbuf);
	} else {
		FMapFPoints(fgc,fpoints,npoints,points);
		XDrawLines(display,d,fgc->gc,points,npoints,mode);
	}
	if (npoints>NPBUF) free(points);
}

/* draw rectangle with float x,y,width,height (with clipping) */
void FXDrawRectangle(Display *display, Drawable d, FGC fgc, 
	float fx, float fy, float fwidth, float fheight)
{
	if (!fgc->clip ||
		FClipRectangle(fgc,fx,fy,fwidth,fheight,
			&fx,&fy,&fwidth,&fheight))
		XDrawRectangle(display,d,fgc->gc,
			MapFX(fgc,fx),MapFY(fgc,fy),
			MapFWidth(fgc,fwidth),MapFHeight(fgc,fheight));
}

/* draw arc with float x,y,width,height,angle1,angle2 */
void FXDrawArc(Display *display, Drawable d, FGC fgc,
	float fx, float fy, float fwidth, float fheight, 
	float fangle1, float fangle2)
{          
	XDrawArc(display,d,fgc->gc,
		MapFX(fgc,fx),MapFY(fgc,fy),
		MapFWidth(fgc,fwidth),MapFHeight(fgc,fheight),
		MapFAngle(fgc,fangle1),MapFAngle(fgc,fangle2));
}

/* draw string at float x,y */
void FXDrawString(Display *display, Drawable d, FGC fgc, 
	float fx, float fy, char *string, int length)
{          
	XDrawString(display,d,fgc->gc,MapFX(fgc,fx),MapFY(fgc,fy),
		string,length);
}

/* fill rectangle with float x,y,width,height (with clipping) */
void FXFillRectangle(Display *display, Drawable d, FGC fgc, 
	float fx, float fy, float fwidth, float fheight)
{
	if (!fgc->clip ||
		FClipRectangle(fgc,fx,fy,fwidth,fheight,
			&fx,&fy,&fwidth,&fheight))
		XFillRectangle(display,d,fgc->gc,
			MapFX(fgc,fx),MapFY(fgc,fy),
			MapFWidth(fgc,fwidth),MapFHeight(fgc,fheight));
}
