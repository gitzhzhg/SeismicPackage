/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/******************************************************************************
CIRCUM - define CIRCUMcircles for Delaunay triangulation

circum - compute center and radius-squared of circumcircle of 3 (x,y)
          locations
circumTri - compute center and radius-squared of circumcircle of 
            triangular face

*******************************************************************************
Function Prototypes:
void circum (float x1, float y1, float x2, float y2, float x3, float y3,
	float *xc, float *yc, float *rs);
void circumTri (Tri *t);
******************************************************************************
circum:
Input:
x1	x-coordinate of first point
y1	y-coordinate of first point
x2	x-coordinate of second point
y2	y-coordinate of second point
x3	x-coordinate of third point
y3	y-coordinate of third point

Output:
xc	pointer to x-coordinate of center of circumcircle
yc	pointer to y-coordinate of center of circumcircle
rs	pointer radius^2 of circumcircle

circumTri:
Input:
*t	Pointer to Tri	

Returns:
xc	x-coordinate of circumcircle
yc      y-coordinate of circumcircle
rs      radius^2 of circumcircle

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, Fall 1990.
******************************************************************************/
/**************** end self doc ********************************/
#include "Triangles/triP.h"

void circum (float x1, float y1, float x2, float y2, float x3, float y3,
	float *xc, float *yc, float *rs)
/******************************************************************************
circum - compute center and radius-squared of circumcircle of 3 (x,y)
          locations
******************************************************************************
Input:
x1	x-coordinate of first point
y1	y-coordinate of first point
x2	x-coordinate of second point
y2	y-coordinate of second point
x3	x-coordinate of third point
y3	y-coordinate of third point

Output:
xc	pointer to x-coordinate of center of circumcircle
yc	pointer to y-coordinate of center of circumcircle
rs	pointer radius^2 of circumcircle

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	float x21,y21,x31,y31,det,scl,tiny,r21,r31,xcr,ycr;
	
	x21 = x2-x1;
	y21 = y2-y1;
	x31 = x3-x1;
	y31 = y3-y1;
	r21 = x21*x21+y21*y21;
	r31 = x31*x31+y31*y31;
	det = x21*y31-x31*y21;
	tiny = 1e-10*(r21+r31);
	if (det<0.0 && -det<tiny) det = -tiny;
	else if (det>0.0 && det<tiny) det = tiny;
	if (det!=0.0) {
		scl = 0.5/det;
		xcr = scl*(r21*y31-r31*y21);
		ycr = scl*(x21*r31-x31*r21);
		*rs = xcr*xcr+ycr*ycr;
		*xc = x1+xcr;
		*yc = y1+ycr;
	} else {
		*rs = TRI_INFINITY;
		*xc = 0.0;
		*yc = 0.0;
	}
}

void circumTri (Tri *t)
/******************************************************************************
circumTri - compute center and radius-squared of circumcircle of 
            triangular face
******************************************************************************
Input:
*t	Pointer to Tri	

Returns:
xc	x-coordinate of circumcircle
yc      y-coordinate of circumcircle
rs      radius^2 of circumcircle

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	float xc,yc,rs;
	Vertex *v1,*v2,*v3;
	
	v1 = t->eu->vu->v;
	v2 = t->eu->euCW->vu->v;
	v3 = t->eu->euCCW->vu->v;
	circum(v1->x,v1->y,v2->x,v2->y,v3->x,v3->y,&xc,&yc,&rs);
	t->rs = rs;
	t->xc = xc;
	t->yc = yc;
}
