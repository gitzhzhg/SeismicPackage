/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "Triangles/triP.h"

/*********************** self documentation **********************/
/*****************************************************************************
PROJECT - project to edge in triangulated model

projectToEdge - Project point with specified (x,y) coordinates to specified
                edge
******************************************************************************
Function Prototype:
void projectToEdge (Edge *e, float *x, float *y)
******************************************************************************

Input:
e		edge to which point is to be projected
x		x-coordinate before projection
y		y-coordinate before projection

Output:
x		x-coordinate after projection
y		y-coordinate after projection

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/11/90
******************************************************************************/
/**************** end self doc ********************************/

void projectToEdge (Edge *e, float *x, float *y)
/*****************************************************************************
projectToEdge - Project point with specified (x,y) coordinates to specified
                edge
******************************************************************************
Input:
e		edge to which point is to be projected
x		x-coordinate before projection
y		y-coordinate before projection

Output:
x		x-coordinate after projection
y		y-coordinate after projection

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/11/90
******************************************************************************/
{
	float x1,y1,x2,y2,x1mx,y1my,x2mx1,y2my1,t;

	/* edge endpoints */
	x1 = e->eu->vu->v->x;  y1 = e->eu->vu->v->y;
	x2 = e->eu->euCW->vu->v->x;  y2 = e->eu->euCW->vu->v->y;
	
	/* compute distance */
	x1mx = x1-*x;  y1my = y1-*y;
	x2mx1 = x2-x1;  y2my1 = y2-y1;
	t = -(x1mx*x2mx1+y1my*y2my1)/(x2mx1*x2mx1+y2my1*y2my1);
	if (t<0.0) t = 0.0;
	if (t>1.0) t = 1.0;
	*x = x1+t*x2mx1;
	*y = y1+t*y2my1;
}
