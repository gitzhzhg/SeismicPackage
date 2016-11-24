/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*****************************************************************************
DTE - Distance to Edge

distanceToEdge - return distance to edge from specified (x,y) coordinates

******************************************************************************
Function Prototype:
float distanceToEdge (Edge *e, float x, float y);
******************************************************************************
distanceToEdge:
Input:
e		edge to which distance is to be computed
x		x-coordinate
y		y-coordinate

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/11/90
******************************************************************************/
/**************** end self doc ********************************/

#include "Triangles/triP.h"

float distanceToEdge (Edge *e, float x, float y)
/*****************************************************************************
distanceToEdge - return distance to edge from specified (x,y) coordinates
******************************************************************************
Input:
e		edge to which distance is to be computed
x		x-coordinate
y		y-coordinate

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/11/90
******************************************************************************/
{
	float x1,y1,x2,y2,x1mx,y1my,x2mx1,y2my1,t,xfac,yfac;

	/* edge endpoints */
	x1 = e->eu->vu->v->x;  y1 = e->eu->vu->v->y;
	x2 = e->eu->euCW->vu->v->x;  y2 = e->eu->euCW->vu->v->y;
	
	/* compute distance */
	x1mx = x1-x;  y1my = y1-y;
	x2mx1 = x2-x1;  y2my1 = y2-y1;
	t = -(x1mx*x2mx1+y1my*y2my1)/(x2mx1*x2mx1+y2my1*y2my1);
	if (t<0.0) t = 0.0;
	if (t>1.0) t = 1.0;
	xfac = x1mx+t*x2mx1;
	yfac = y1my+t*y2my1;
	return sqrt(xfac*xfac+yfac*yfac);
}
