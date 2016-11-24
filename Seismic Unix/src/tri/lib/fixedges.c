/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
FIXEDGES - FIX or unFIX EDGES between verticies

fixEdgeBetweenVertices		Fix edge(s) between vertices, creating new
                          	  colinear edges as necessary.
unfixEdge			unfix edge 

******************************************************************************
fixEdgesBetweenVertices:
Input:
v1		pointer to first Vertex
v2		pointer to second Vertex

Returns:
0		if unable to fix edges
1		otherwise

unfixEdge:
Input:
e		edge to be unfixed

Returns:
0		if unable to unfix edge
1		otherwise
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/04/91
******************************************************************************/
/**************** end self doc ********************************/

#include "Triangles/triP.h"

/* functions defined and used internally */
static void fixExistingEdgesBetween(Vertex **v1, Vertex**v2);
static void killEdgesBetween(Vertex *v1, Vertex*v2);
static int edgeBetweenVertices (Edge *e, Vertex *v1, Vertex *v2);
static void triangFaceNextToEdge (EdgeUse *eu);

int fixEdgeBetweenVertices (Vertex *v1, Vertex *v2)
/*****************************************************************************
fixEdgeBetweenVertices - Fix edge(s) between vertices, creating new
                         colinear edges as necessary.
******************************************************************************
Input:
v1		pointer to first Vertex
v2		pointer to second Vertex

Returns:
0		if unable to fix edges
1		otherwise

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/04/91
******************************************************************************/
{
	Edge *e;
	Face *f;
	
	/* try to connect with existing edges */
	fixExistingEdgesBetween(&v1,&v2);
	
	/* if connected, simply return */
	if (v1==v2) return 1;
	
	/* kill edges intersected by connection of v1 and v2 */
	killEdgesBetween(v1,v2);
	
	/* connect v1 and v2 with a fixed edge */
	makeEdgeFace(v1,v2,&e,&f);
	e->fixed = 1;
	
	/* triangulate regions on both sides of fixed edge */
	triangFaceNextToEdge(e->eu);
	triangFaceNextToEdge(e->eu->euMate);
	
	/* success! */
	return 1;
}

static void fixExistingEdgesBetween(Vertex **v1, Vertex**v2)
/* used internally in fixEdgeBetweenVertex */
{
	int moved;
	VertexUse *vu;
	Vertex *v,*v1t=*v1,*v2t=*v2;
	
	/* fix edges from vertex v1 to vertex v2 */
	moved = 1;
	while(moved) {
		moved = 0;
		vu = v1t->vu;
		do {
			v = vu->eu->euCW->vu->v;
			if (vertexBetweenVertices(v,v1t,v2t)) {
				vu->eu->e->fixed = 1;
				v1t = v;
				moved = 1;
			}
			vu = vu->vuNext;
		} while(vu!=v1t->vu && !moved && v1t!=v2t);
		if (v1t==v2t) {
			*v1 = v1t;
			*v2 = v2t;
			return;
		}
	}
	
	/* fix edges from vertex v2 to vertex v1 */
	moved = 1;
	while(moved) {
		moved = 0;
		vu = v2t->vu;
		do {
			v = vu->eu->euCW->vu->v;
			if (vertexBetweenVertices(v,v1t,v2t)) {
				vu->eu->e->fixed = 1;
				v2t = v;
				moved = 1;
			}
			vu = vu->vuNext;
		} while(vu!=v2t->vu && !moved && v1t!=v2t);
		if (v1t==v2t) {
			*v1 = v1t;
			*v2 = v2t;
			return;
		}
	}
	
	*v1 = v1t;
	*v2 = v2t;
}

static void killEdgesBetween(Vertex *v1, Vertex*v2)
/* used internally in fixEdgeBetweenVertex */
{
	VertexUse *vu;
	EdgeUse *eu;
	Face *fs;
	Model *m;
	
	/* determine model */
	m = (v1->vu->eu->f!=NULL?v1->vu->eu->f->m:v1->vu->eu->euMate->f->m);
	
	/* find vertex use opposite the intersected edge */
	for (vu=v1->vu;
		!edgeBetweenVertices(vu->eu->euCW->e,v1,v2);
		vu=vu->vuNext);
	
	/* will delete triangle corresponding to this vertex use */
	if (m->tDel!=NULL) m->tDel(m,vu->eu->f);
	
	/* kill edges until v1 and v2 are adjacent to same face */
	eu = vu->eu;
	do {
		if (m->tDel!=NULL) m->tDel(m,eu->euCW->euMate->f);
		killEdge(eu->euCW->e,&fs);
		if (!edgeBetweenVertices(eu->euCW->e,v1,v2)) eu = eu->euCW;
	} while(eu->euCW->vu->v!=v2);
}

static int edgeBetweenVertices (Edge *e, Vertex *v1, Vertex *v2)
/* determine whether or not an edge is intersected by a line  */
/* between two vertices */
/* used internally in fixEdgeBetweenVertex */
{
	Vertex *e1,*e2;
	float dxv2v1,dyv2v1,dxe2e1,dye2e1,dxe1v1,dye1v1,det,s,t;
	
	e1 = e->eu->vu->v;
	e2 = e->eu->euCW->vu->v;
	if (e1==v1 || e1==v2 || e2==v1 || e2==v2) return 0;
	dxv2v1 = v2->x-v1->x;
	dyv2v1 = v2->y-v1->y;
	dxe2e1 = e2->x-e1->x;
	dye2e1 = e2->y-e1->y;
	dxe1v1 = e1->x-v1->x;
	dye1v1 = e1->y-v1->y;
	det = dxe2e1*dyv2v1-dye2e1*dxv2v1;
	if (det==0.0) return 0;
	s = (dxe2e1*dye1v1-dye2e1*dxe1v1)/det;
	if (s<=0.0 || s>=1.0) return 0;
	t = (dxv2v1*dye1v1-dyv2v1*dxe1v1)/det;
	if (t<=0.0 || t>=1.0) return 0;
	return 1;
}

static void triangFaceNextToEdge (EdgeUse *eu)
/* triangulate a face adjacent to an edge */
/* used internally in fixEdgeBetweenVertex */
{
	float xa,ya,xb,yb,x,y,dxa,dya,dxb,dyb,cota,cotamin;
	VertexUse *vu=NULL;
	Edge *e;
	EdgeUse *eua,*eub;
	Face *f;
	Model *m;
	
	/* determine model */
	m = eu->f->m;

	/* if face is already a triangle */
	if (eu->euCW->euCW->euCW==eu) {
		circumTri(eu->f);
		if (m->tAdd!=NULL) m->tAdd(m,eu->f);
		return;
	}
	
	/* endpoint coordinates */
	eua = eu;  eub = eu->euCW;
	xa = eua->vu->v->x;  ya = eua->vu->v->y;
	xb = eub->vu->v->x;  yb = eub->vu->v->y;
	
	/* determine vertex with min cot(angle subtended by edge) */
	cotamin = FLT_MAX;
	eu = eub->euCW;
	do {
		x = eu->vu->v->x;
		y = eu->vu->v->y;
		dxa = xa-x;  dya = ya-y;
		dxb = xb-x;  dyb = yb-y;
		cota = (dxa*dxb+dya*dyb)/(dxb*dya-dxa*dyb);
		if (cota<cotamin) {
			vu = eu->vu;
			cotamin = cota;
		}
		eu = eu->euCW;
	} while (eu!=eua);
	
	/* make required edges (at least one) and triangulate again */
	if (vu->eu->euCCW!=eub) {
		makeEdgeFace(vu->v,eub->vu->v,&e,&f);
		eu = (e->eu->f==eua->f?e->eu->euMate:e->eu);
		triangFaceNextToEdge(eu);
	}
	if (vu->eu->euCW!=eua) {
		makeEdgeFace(vu->v,eua->vu->v,&e,&f);
		eu = (e->eu->f==eua->f?e->eu->euMate:e->eu);
		triangFaceNextToEdge(eu);
	}
	
	/* at least one triangle was added */
	circumTri(eua->f);
	if (m->tAdd!=NULL) m->tAdd(m,eua->f);
}

/* functions declared and used internally */
static void correctTrisAroundVertex(Vertex *v);

int unfixEdge(Edge *e)
/*****************************************************************************
unfixEdge - unfix edge 
******************************************************************************
Input:
e		edge to be unfixed

Returns:
0		if unable to unfix edge
1		otherwise
******************************************************************************
Notes:
Unfixes an edge that was fixed previously via fixEdgeBetweenVertices.
If unable to unfix edge, this function returns 0, otherwise returns 1.
******************************************************************************
Author:  Chris Elmer & Dave Hale, Colorado School of Mines, 06/04/91
******************************************************************************/
{
	/* if edge is on boundary, then cannot unfix it */
	if(e->eu->f==NULL || e->eu->euMate->f==NULL) return 0;

	/* unfix the edge */
	e->fixed = 0;

	/* correct triangles containing endpoints of edge */
	correctTrisAroundVertex(e->eu->vu->v);
	correctTrisAroundVertex(e->eu->euMate->vu->v);  
	
	/* success! */
	return 1;
}

static void correctTrisAroundVertex (Vertex *v)
/******************************************************************************
correctTrisAroundVertex -
*******************************************************************************
Input:
v	pointer to Vertex

*******************************************************************************
Notes:
For each unfixed edge connected to a vertex, check to see if that edge should
be swapped, according to the Delaunay circumcircle test.  If necessary, swap
the edge, and then (recursively) call this function for each of the endpoints
of the old edge (which includes the vertex v passed as an argument).

Used internally in unfixEdge.
*******************************************************************************
Author:  Chris Elmer & Dave Hale, Colorado School of Mines, 06/04/91
******************************************************************************/
{
	Vertex *v2,*vtemp1,*vtemp2;
	VertexUse *vu;
	Edge *enew;
	Face *fnew,*fs,*t;
	Model *m;

	/* determine model */
	m = (v->vu->eu->f!=NULL)?v->vu->eu->f->m:v->vu->eu->euMate->f->m;

	/* loop over all edges connected to (vertex-uses of) this vertex */
	vu = v->vu;
	do {
		/* if edge is not fixed */
		if (!vu->eu->e->fixed) {

			/* find an adjoining triangle to this edge, and the */
			/* vertex opposite this edge in the other triangle */
			t = vu->eu->f;
			vtemp1 = vu->eu->euMate->euCCW->vu->v;

			/* if vertex is inside triangle's circumcircle */
			if (inCircumTri(vtemp1->x,vtemp1->y,t)) {
			
				/* get the other two vertices in quad */
				vtemp2 = vu->eu->euCCW->vu->v;
				v2 = vu->eu->euMate->vu->v;

				/* 2 triangles will be deleted */
				if (m->tDel!=NULL) {
					m->tDel(m,t);
					m->tDel(m,vu->eu->euMate->f);
				}

				/* swap the edge */
				killEdge(vu->eu->e,&fs);
				makeEdgeFace(vtemp1,vtemp2,&enew,&fnew);
				circumTri(fs);
				circumTri(fnew);

				/* 2 triangles were added */
				if (m->tAdd!=NULL) {
					m->tAdd(m,fs);
					m->tAdd(m,fnew);
				}

				/* correct tris around endpoints of old edge */
				correctTrisAroundVertex(v);
				correctTrisAroundVertex(v2);
				break;
			}
		}

		/* next vertex-use */
		vu = vu->vuNext;

	} while (vu!=v->vu);
}
