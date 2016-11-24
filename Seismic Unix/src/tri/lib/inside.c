/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*****************************************************************************
INSIDE -  Is a vertex or point inside a circum circle, etc. of a triangulated
          model

inCircum	determine whether or not a vertex is inside a circum circle
inCircumTri	determine whether or not a vertex is inside a circum circle of
                 a triangle
in3Vertices	determine whether or not a vertex is inside triangle (v1,v2,v3)
inTri		determine whether or not a vertex is inside a triangle 

******************************************************************************
Function Prototypes:
int inCircum (float x, float y, float xc, float yc, float rs);
int inCircumTri (float x, float y, Tri *t);
int in3Vertices (float x, float y, Vertex *v1, Vertex *v2, Vertex *v3);
int inTri (float x, float y, Tri *t);
******************************************************************************
inCircum:
Input:
x	x-coordinate of vertex
y	y-coordinate of vertex
xc	x-coordinate of center of circumcircle
yc	y-coordinate of center of circumcircle
rs	radius^2 of circumcircle

Returns:
1	if x,y inside of circumcircle
0	otherwise

Notes:
A vertex exactly on the edge of a circumcircle is taken as being outside

inCircumTri:
Input:
x	x-coordinate of vertex
y	y-coordinate of vertex
t	pointer to Tri

Returns:
1	if x,y inside of circumcircle of a triangle
0	otherwise

Notes:
A vertex exactly on the edge of a circumcircle is taken as being outside

in3Vertices:
Input:
x	x-coordinate of vertex
y	y-coordinate of vertex
v1	pointer to first Vertex
v2	pointer to second Vertex
v3	pointer to third Vertex

Returns:
1	if x,y inside of v1,v2,v3
0	otherwise

Notes:
A vertex exactly on an edge of the triangle is taken as being inside

inTri:
Input:
x	x-coordinate of vertex
y	y-coordinate of vertex
t	pointer to Tri

Returns:
1	if x,y inside a triangle
0	otherwise
Notes:
A vertex exactly on the edge of a triangle is inside

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/04/91
*****************************************************************************/
/**************** end self doc ********************************/

#include "Triangles/triP.h"

int inCircum (float x, float y, float xc, float yc, float rs)
/*****************************************************************************
inCircum -  determine whether or not a vertex is inside a circum circle
******************************************************************************
Input:
x	x-coordinate of vertex
y	y-coordinate of vertex
xc	x-coordinate of center of circumcircle
yc	y-coordinate of center of circumcircle
rs	radius^2 of circumcircle

Returns:
1	if x,y inside of circumcircle
0	otherwise

******************************************************************************
Notes:
A vertex exactly on the edge of a circumcircle is taken as being outside
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/04/91
*****************************************************************************/
{
	float d,ds;

	if (rs==TRI_INFINITY) return(1);
	d = xc-x;
	ds = d*d;
	if (ds>=rs)
		return(0);
	else {
		d = yc-y;
		ds += d*d;
		if (ds>=rs)
			return(0);
	}
	return(1);
}

int inCircumTri (float x, float y, Tri *t)
/*****************************************************************************
inCircumTri -  determine whether or not a vertex is inside a circum circle of
               a triangle
******************************************************************************
Input:
x	x-coordinate of vertex
y	y-coordinate of vertex
t	pointer to Tri

Returns:
1	if x,y inside of circumcircle of a triangle
0	otherwise

******************************************************************************
Notes:
A vertex exactly on the edge of a circumcircle is taken as being outside
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/04/91
*****************************************************************************/
{
	return inCircum(x,y,t->xc,t->yc,t->rs);
}

int in3Vertices (float x, float y, Vertex *v1, Vertex *v2, Vertex *v3)
/*****************************************************************************
in3Vertices -  determine whether or not a vertex is inside triangle (v1,v2,v3)
******************************************************************************
Input:
x	x-coordinate of vertex
y	y-coordinate of vertex
v1	pointer to first Vertex
v2	pointer to second Vertex
v3	pointer to third Vertex

Returns:
1	if x,y inside of v1,v2,v3
0	otherwise

******************************************************************************
Notes:
A vertex exactly on an edge of the triangle is taken as being inside
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/04/91
*****************************************************************************/
{
	float s1,s2,s3;
	float x1,y1,x2,y2,x3,y3;
	
	x1 = v1->x;  y1 = v1->y;
	x2 = v2->x;  y2 = v2->y;
	x3 = v3->x;  y3 = v3->y;	
	s1 = (x-x1)*(y2-y1)-(x2-x1)*(y-y1);
	s2 = (x-x2)*(y3-y2)-(x3-x2)*(y-y2);
	if (s1*s2<=0) return 0;
	s3 = (x-x3)*(y1-y3)-(x1-x3)*(y-y3);
	if (s2*s3<=0 || s1*s3<=0) return 0;
	return 1;
}

int inTri (float x, float y, Tri *t)
/*****************************************************************************
inTri - determine whether or not a vertex is inside a triangle
******************************************************************************
Input:
x	x-coordinate of vertex
y	y-coordinate of vertex
t	pointer to Tri

Returns:
1	if x,y inside a triangle
0	otherwise

******************************************************************************
Notes:
A vertex exactly on the edge of a triangle is inside
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/04/91
*****************************************************************************/
{
	Vertex *v1,*v2,*v3;
	
	v1 = t->eu->vu->v;
	v2 = t->eu->euCW->vu->v;
	v3 = t->eu->euCCW->vu->v;	
	return in3Vertices(x,y,v1,v2,v3);
}
