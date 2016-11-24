/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*****************************************************************************
NEAREST - NEAREST edge or vertex in triangulated model

nearestEdgeInModel	Return pointer to edge in model nearest to
                    	specified (x,y) coordinates

nearestVertexInModel	Return pointer to vertex in model nearest
                   	to specified (x,y) coordinates

******************************************************************************
Function Prototypes:
Vertex* nearestVertexInModel (Model *m, Vertex *start, float x, float y);
Edge* nearestEdgeInModel (Model *m, Edge *start, float x, float y);

******************************************************************************
nearestEdgeInModel:
Input:
m		model
start		edge to look at first (NULL to begin looking anywhere)
x		x-coordinate
y		y-coordinate

Returns: pointer to nearest Edge

nearestVertexInModel:
Input:
m		model
start		vertex to look at first (NULL to begin looking anywhere)
x		x-coordinate
y		y-coordinate

Returns: pointer to nearest Vertex

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, Fall 1990
******************************************************************************/
/**************** end self doc ********************************/

#include "Triangles/triP.h"

Edge* nearestEdgeInModel (Model *m, Edge *start, float x, float y)
/*****************************************************************************
nearestEdgeInModel - Return pointer to edge in model nearest to
                     specified (x,y) coordinates
******************************************************************************
Input:
m		model
start		edge to look at first (NULL to begin looking anywhere)
x		x-coordinate
y		y-coordinate

Returns: pointer to nearest Edge

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/11/90
******************************************************************************/
{
	float d,dmin=0.0;
	Edge *emin=NULL;
	EdgeUse *eu;
	Tri *t;
	
	/* find triangle containing point */
	if (start!=NULL) {
		t = (start->eu->f!=NULL?start->eu->f:start->eu->euMate->f);
		t = insideTriInModel(m,t,x,y);
	} else {
		t = insideTriInModel(m,NULL,x,y);
	}
	
	/* loop over edges in triangle */
	eu = t->eu;
	do {
		
		/* compute distance to edge */
		d = distanceToEdge(eu->e,x,y);
		
		/* update minimum distance */
		if (eu==t->eu || d<dmin) {
			dmin = d;
			emin = eu->e;
		}
		
		/* next edge */
		eu = eu->euCW;
		
	} while (eu!=t->eu);
	
	/* return edge corresponding to minimum distance */
	return emin;
}

Vertex* nearestVertexInModel (Model *m, Vertex *start, float x, float y)
/*****************************************************************************
nearestVertexInModel - Return pointer to vertex in model nearest
                       to specified (x,y) coordinates
******************************************************************************
Input:
m		model
start		vertex to look at first (NULL to begin looking anywhere)
x		x-coordinate
y		y-coordinate

Returns: pointer to nearest Vertex

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/10/90
******************************************************************************/
{
	float dx,dy,ds,dsmin;
	Vertex *v,*vmin,*vn;
	VertexUse *vu;
	EdgeUse *eu;

	/* start at some vertex in model */
	vmin = (start==NULL ? m->f->eu->vu->v : start);
	dx = vmin->x-x;
	dy = vmin->y-y;
	dsmin = dx*dx+dy*dy;
	
	/* hop from vertex to vertex, always moving closer to (x,y) */
	do {
		v = vmin;
		vu = v->vu;
		do {
			eu = vu->eu;
			vn = eu->euCW->vu->v;
			dx = vn->x-x;
			dy = vn->y-y;
			ds = dx*dx+dy*dy;
			if (dsmin>ds) {
				dsmin = ds;
				vmin = vn;
			}
			vu = vu->vuNext;
		} while (vu!=v->vu);		
	} while (v!=vmin);
	
	return v;
}
