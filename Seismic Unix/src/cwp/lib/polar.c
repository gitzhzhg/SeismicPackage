/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/**************************************************************************
POLAR - Functions to map data in rectangular coordinates to polar and vise versa

recttopolar	convert a function p(x,y) to a function q(a,r)
polartorect	convert a function q(a,r) to a function p(x,y)

******************************************************************************
Function Prototypes:
void recttopolar ( int nx, float dx, float fx, int ny, float dy, float fy,
			float **p, int na, float da, float fa, int nr,
			float dr, float fr, float **q);
void polartorect ( int na, float da, float fa, int nr, float dr, float fr,
			float **q, int nx, float dx, float fx, int ny,
			float dy, float fy, float **p)

******************************************************************************
recttopolar:
Input:
nx		number of x samples
dx		x sampling interval
fx		first x sample
ny		number of y samples
dy		y sampling interval
fy		first y sample
p		array[ny][nx] containing samples of p(x,y)
na		number of a samples
da		a sampling interval
fa		first a sample
nr		number of r samples
dr		r sampling interval
fr		first r sample

Output:
q		array[nr][na] containing samples of q(a,r)

******************************************************************************
polartorect:
Input:
na		number of a samples
da		a sampling interval
fa		first a sample
nr		number of r samples
dr		r sampling interval
fr		first r sample
nx		number of x samples
dx		x sampling interval
fx		first x sample
ny		number of y samples
dy		y sampling interval
fy		first y sample
q		array[nr][na] containing samples of q(a,r)

Output:
p		array[ny][nx] containing samples of p(x,y)

******************************************************************************
Notes:
The polar angle a is measured in radians, 
x = r*cos(a) and y = r*sin(a).

recttopolar:
Linear extrapolation is used to determine the value of p(x,y) for
x and y coordinates not in the range corresponding to nx, dx, ....

polartorect:
Linear extrapolation is used to determine the value of q(a,r) for
a and r coordinates not in the range corresponding to na, da, ....

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/15/90
******************************************************************************/
/**************** end self doc ********************************/


#include "cwp.h"

void recttopolar (
	int nx, float dx, float fx, int ny, float dy, float fy, float **p,
	int na, float da, float fa, int nr, float dr, float fr, float **q)
/*****************************************************************************
Convert a function of p(x,y) to q(a,r), where x = r*cos(a) and y = r*sin(a)
******************************************************************************
Input:
nx		number of x samples
dx		x sampling interval
fx		first x sample
ny		number of y samples
dy		y sampling interval
fy		first y sample
p		array[ny][nx] containing samples of p(x,y)
na		number of a samples
da		a sampling interval
fa		first a sample
nr		number of r samples
dr		r sampling interval
fr		first r sample

Output:
q		array[nr][na] containing samples of q(a,r)
******************************************************************************
Notes:
The polar angle a is measured in radians.

Linear extrapolation is used to determine the value of p(x,y) for
x and y coordinates not in the range corresponding to nx, dx, ....
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/15/90
******************************************************************************/
{
	int ia,ir,ix,iy;
	float a,r,x,y,xi,yi,sx,sy;
	
	/* for all r */
	for (ir=0,r=fr; ir<nr; ++ir,r+=dr) {
	
		/* for all a */
		for (ia=0,a=fa; ia<na; ++ia,a+=da) {
		
			/* determine x and y */
			x = r*cos(a);
			y = r*sin(a);
			
			/* determine sample indices */
			xi = (x-fx)/dx;
			ix = xi;
			if (ix<0 ) xi = ix = 0; 
			if (ix>nx-2) {ix = nx-2; xi = nx-1;}
			yi = (y-fy)/dy;
			iy = yi;
			if (iy<0) yi = iy = 0;
			if (iy>ny-2) {iy = ny-2; yi = ny-1;}
			
			/* bilinear interpolation */
			sx = xi-ix;
			sy = yi-iy;
			q[ir][ia] = (1.0-sy)*((1.0-sx)*p[iy][ix] + 
						sx*p[iy][ix+1]) +
					sy*((1.0-sx)*p[iy+1][ix] +
						sx*p[iy+1][ix+1]);
		}
	}
}

void polartorect (
	int na, float da, float fa, int nr, float dr, float fr, float **q,
	int nx, float dx, float fx, int ny, float dy, float fy, float **p)
/*****************************************************************************
Convert a function of q(a,r) to p(x,y), where x = r*cos(a) and y = r*sin(a)
******************************************************************************
Input:
na		number of a samples
da		a sampling interval
fa		first a sample
nr		number of r samples
dr		r sampling interval
fr		first r sample
nx		number of x samples
dx		x sampling interval
fx		first x sample
ny		number of y samples
dy		y sampling interval
fy		first y sample
q		array[nr][na] containing samples of q(a,r)

Output:
p		array[ny][nx] containing samples of p(x,y)
******************************************************************************
Notes:
The polar angle a is measured in radians.

Linear extrapolation is used to determine the value of q(a,r) for
a and r coordinates not in the range corresponding to na, da, ....
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/15/90
******************************************************************************/
{
	int ix,iy,ia,ir;
	float x,y,a=0.0,r,ai,ri,sa,sr;
	
	/* for all y */
	for (iy=0,y=fy; iy<ny; ++iy,y+=dy) {
	
		/* for all x */
		for (ix=0,x=fx; ix<nx; ++ix,x+=dx) {
		
			/* determine a and r */
			if (x !=0.0)
				a = atan2((double) y,(double) x);
			else if (y>0.0)
				a = PI/2.0;
			else if (y<0.0)
				a = -PI/2.0;
			else if (y==0.0)
				a = 0.0;
			
			r = sqrt(x*x+y*y);
			
			/* determine sample indices */
			ai = (a-fa)/da;
			ia = ai;
			if (ia<0) ai = ia = 0;
			if (ia>na-2) {ai = na-1; ia = na-2;}
			ri = (r-fr)/dr;
			ir = ri;
			if (ir<0) ri = ir = 0;
			if (ir>nr-2) {ri = nr-1; ir = nr-2;}
			
			/* bilinear interpolation */
			sa = ai-ia;
			sr = ri-ir;
			p[iy][ix] = (1.0-sr)*((1.0-sa)*q[ir][ia] + 
						sa*q[ir][ia+1]) +
					sr*((1.0-sa)*q[ir+1][ia] +
						sa*q[ir+1][ia+1]);
		}
	}
}

