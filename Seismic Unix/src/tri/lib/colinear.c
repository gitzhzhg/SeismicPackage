/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/******************************************************************************
COLINEAR - determine if edges or vertecies are COLINEAR in triangulated
           model
edgesColinear		see whether or not two edges are colinear
vertexBetweenVertices	determine whether or not a vertex is on a line
                          between two other vertices
*******************************************************************************
Function Prototypes:
int edgesColinear (Edge *e1, Edge *e2);
int vertexBetweenVertices (Vertex *v, Vertex *v1, Vertex *v2);

*******************************************************************************
edgesColinear:
Input:
e1	pointer to first Edge
e2	pointer to second Edge

Returns: (int)
1	if colinear

vertexBetweenVertices:
Input:
v		pointer to first Vertex in question
v1		pointer to first reference Vertex
v2		pointer to second reference Vertex

Returns: integer
1		if v=v1 or v=v2 or if v is between v1 and v2
0		otherwise

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, Fall 1990.
******************************************************************************/
/**************** end self doc ********************************/

#include "Triangles/triP.h"

int edgesColinear (Edge *e1, Edge *e2)
/******************************************************************************
edgesColinear - see whether or not two edges are colinear
*******************************************************************************
Input:
e1	pointer to first Edge
e2	pointer to second Edge

Returns: (int)
1	if colinear
0	if not colinear

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/10/90
******************************************************************************/
{
	float xa,xb,ya,yb,x1,x2,y1,y2,c12,a1,a2,s12;

	/* edge vectors */
	xa = e1->eu->vu->v->x;  xb = e1->eu->euCW->vu->v->x;
	ya = e1->eu->vu->v->y;  yb = e1->eu->euCW->vu->v->y;
	x1 = xb-xa;
	y1 = yb-ya;
	xa = e2->eu->vu->v->x;  xb = e2->eu->euCW->vu->v->x;
	ya = e2->eu->vu->v->y;  yb = e2->eu->euCW->vu->v->y;
	x2 = xb-xa;
	y2 = yb-ya;
	
	/* cross-product of vectors 1 and 2 */
	c12 = x1*y2-x2*y1;
	
	/* magnitude-squared of x1 and x2 */
	a1 = x1*x1+y1*y1;
	a2 = x2*x2+y2*y2;
	
	/* sin-squared of angle between vectors 1 and 2 */
	s12 = (c12*c12)/(a1*a2);
	
	/* if sin-squared less than tiny number, then colinear */
	if (s12<0.0001)
		return 1;
	else
		return 0;
}

int vertexBetweenVertices (Vertex *v, Vertex *v1, Vertex *v2)
/*****************************************************************************
vertexBetweenVertices - determine whether or not a vertex is on a line
                        between two other vertices
******************************************************************************
Input:
v		pointer to first Vertex in question
v1		pointer to first reference Vertex
v2		pointer to second reference Vertex

Returns: integer
1		if v=v1 or v=v2 or if v is between v1 and v2
0		otherwise

******************************************************************************
Author: Dave Hale, Center for Wave Phenomena, c. 1990 1991.
*****************************************************************************/
{
	float xa,xb,ya,yb,x1,x2,y1,y2,x12,y12,a1,a2,a12,c,s1,s2,d1,d2;

	/* vectors 1, 2, and 12 */
	xa = v1->x;  xb = v->x;  x1 = xb-xa;
	ya = v1->y;  yb = v->y;  y1 = yb-ya;
	xa = v2->x;  xb = v->x;  x2 = xb-xa;
	ya = v2->y;  yb = v->y;  y2 = yb-ya;
	xa = v1->x;  xb = v2->x;  x12 = xb-xa;
	ya = v1->y;  yb = v2->y;  y12 = yb-ya;
	
	/* magnitude-squared of vectors */
	a1 = x1*x1+y1*y1;
	a2 = x2*x2+y2*y2;
	a12 = x12*x12+y12*y12;
	
	/* handle case where v equals v1 or v2 */
	if (a1==0.0 || a2==0.0) return 1;
	
	/* handle case where v1 equals v2 */
	if (a12==0.0) return 0;
	
	/* test sign of product of (1 dot 12) and (2 dot 12) */
	d1 = x1*x12+y1*y12;
	d2 = x2*x12+y2*y12;
	if (d1*d2>0.0) return 0;
	
	/* test sin-squared of angle between vectors 1 and 12 */
	c = x1*y12-x12*y1;
	s1 = (c*c)/(a1*a12);
	if (s1>0.0001) return 0;
	
	/* test sin-squared of angle between vectors 2 and 12 */
	c = x2*y12-x12*y2;
	s2 = (c*c)/(a2*a12);
	if (s2>0.0001) return 0;
	
	/* if passed all the checks, then v is on line between v1 and v2 */
	return 1;
}
