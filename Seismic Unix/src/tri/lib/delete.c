/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
DELETE - DELETE vertex, model, edge, or boundary edge from triangulated model

deleteVertexFromModel		Delete a vertex from model
killModel			Delete a model along with everything in it
killEdge			Delete an edge
killBoundaryEdge		Kill a boundary edge

******************************************************************************
Function Prototypes:
void deleteVertexFromModel (Model *m, Vertex *v);
void killModel (Model *m);
void killEdge (Edge *e, Face **fs);
void killBoundaryEdge (Edge *e);

******************************************************************************
deleteVertexFromModel:
Input:
m	 	pointer to Model	
v		pointer to Vertex to be deleted

killModel:
Input:
m		pointer to Model

killEdge:
Input:
e		Edge to delete

Output:
fs		surviving Face

killBoundaryEdge:
Input:
e 	boundary Edge

Notes:
Killing a boundary edge is typically done after a new boundary vertex
is inserted on an existing boundary edge.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, Fall 1990.
******************************************************************************/
/**************** end self doc ********************************/

#include "Triangles/triP.h"

void deleteVertexFromModel (Model *m, Vertex *v)
/*****************************************************************************
deleteVertexFromModel - Delete a vertex from model
******************************************************************************
Input:
m	 	pointer to Model	
v		pointer to Vertex to be deleted

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/10/90
******************************************************************************/
{
	int containsb,edgedeleted;
	float x,y,xb,yb,xc,yc,rs;
	Vertex *v1,*v2,*v3;
	VertexUse *vu,*vub;
	Edge *e;
	Face *f,*fs;
	
	/* if vertex is fixed, simply return */
	if (v->fixed) return;
	
	/* if vertex is on boundary, make boundary edge and triangle */
	vu = v->vu;
	do {
		if (vu->eu->f==NULL) {
			makeBoundaryEdgeTri(v,&e,&f);
			break;
		}
		vu = vu->vuNext;
	} while (vu!=v->vu);
	
	/* if specified, do delete vertex function */
	if (m->vDel!=NULL) m->vDel(m,v);
	
	/* (x,y) coordinates of vertex */
	x = v->x;  y = v->y;
	
	/* start loop over edges (vertex-uses) with any edge */
	vu = v->vu;
	
	/* while vertex is used by more than three edges */
	while (vu->vuNext->vuNext->vuNext!=vu) {
	
		/* initially, assume no edge will be deleted */
		edgedeleted = 0;
		
		/* get 3 clockwise vertices opposite the current one */
		v1 = vu->eu->euMate->euCCW->vu->v;
		v2 = vu->eu->euMate->vu->v;
		v3 = vu->eu->euCCW->vu->v;
		
		/* compute circumcircle of (v1,v2,v3) */
		circum(v1->x,v1->y,v2->x,v2->y,v3->x,v3->y,&xc,&yc,&rs);

		/* if circumcircle of (v1,v2,v3) contains the vertex v to */
		/* be deleted, and triangle (v1,v2,v3) does not contain v */
		if ((inCircum(x,y,xc,yc,rs)) && 
			(!in3Vertices(x,y,v1,v2,v3))) {
			
			/* and if circumcircle of (v1,v2,v3) does not  */
			/* contain any other vertices */
			vub = vu->eu->euCCW->euMate->euCCW->vu;
			do {
				xb = vub->v->x;  yb = vub->v->y;
				containsb = inCircum(xb,yb,xc,yc,rs);
				vub = vub->eu->euMate->euCCW->vu;
			} while(vub->v!=v1 && !containsb);
			if (!containsb) {
			
				/* 2 triangles will be deleted */
				if (m->tDel!=NULL) {
					m->tDel(m,vu->eu->f);
					m->tDel(m,vu->eu->euMate->f);
				}
			
				/* kill edge (v,v2) */
				killEdge(vu->eu->e,&fs);
			
				/* make edge (v1,v3) */
				makeEdgeFace(v1,v3,&e,&f);
			
				/* 2 triangles were created */
				if (m->tAdd!=NULL) {
					m->tAdd(m,e->eu->f);
					m->tAdd(m,e->eu->euMate->f);
				}
			
				/* compute circumcircles of 2 new triangles */
				circumTri(e->eu->f);
				circumTri(e->eu->euMate->f);
				
				/* remember that an edge was deleted */
				edgedeleted = 1;
			}
		}
		
		/* if this edge was deleted, try any edge that is left */
		if (edgedeleted)
			vu = v->vu;
		
		/* else, try any edge but this edge */
		else
			vu = vu->vuNext;
	}
	
	/* 3 triangles will be deleted */
	if (m->tDel!=NULL) {
		vu = v->vu;
		do {
			m->tDel(m,vu->eu->f);
			vu = vu->vuNext;
		} while (vu!=v->vu);
	}
	
	/* delete remaining 3 edges connected to vertex */
	killEdge(v->vu->eu->e,&fs);
	killEdge(v->vu->eu->e,&fs);
	killEdge(v->vu->eu->e,&fs);
	
	/* 1 triangle remains */
	if (m->tAdd!=NULL) m->tAdd(m,fs);
	
	/* compute circumcircle of remaining triangle */
	circumTri(fs);
	
	/* debug */
	/* checkModel(m); */
}

void killModel (Model *m)
/*****************************************************************************
killModel - Delete a model along with everything in it
******************************************************************************
Input:
m	pointer to Model

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	Face *f,*fnext;
	EdgeUse *eu,*eunext;

	/* loop over all faces in model */
	f = m->f;
	do {	

		/* determine next f before this one is deleted */
		fnext = f->fNext;
		
		/* loop over all edge-uses in face */
		eu = f->eu;
		do {
		
			/* determine next eu before deleting this one */
			eunext = eu->euCW;
				
			/* if only one vertex-use is left, delete vertex */
			if (eu->vu->vuNext==eu->vu) {
				free(eu->vu->v);
			
			/* else, unhook this vertex-use from linked list */
			} else {
				eu->vu->vuPrev->vuNext = eu->vu->vuNext;
				eu->vu->vuNext->vuPrev = eu->vu->vuPrev;
			}
			
			/* delete vertex-use */
			free(eu->vu);
		
			/* if edge-use has no mate, delete edge now */
			if (eu->euMate==NULL)
				free(eu->e);
				
			/* else, if edge-use has a mate, delete edge later */
			else
				eu->euMate->euMate = NULL;
			
			/* delete edge-use */
			free(eu);
			
			/* next edge-use */
			eu = eunext;
			
		} while (eu!=f->eu);
		
		/* delete face */
		free(f);
		
		/* next face */
		f = fnext;
		
	} while (f!=m->f);
	
	/* delete model */
	free(m);
}

void killEdge (Edge *e, Face **fs)
/*****************************************************************************
killEdge -  Delete an edge
******************************************************************************
Input:
e	Edge to delete

Output:
fs	surviving Face

******************************************************************************
Notes:
For each vertex of the edge, if the edge is the last adjacent to the
vertex, then delete the vertex.  If different faces are adjacent to the
edge, then merge one face into the other. The surviving face (fs) is returned.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/04/91
****************************************************************************/
{
	VertexUse *vu,*vum;
	EdgeUse *eu,*eum,*eut;
	Face *f,*fm;
	
	/* determine edge and vertex uses and faces */
	eu = e->eu;
	eum = eu->euMate;
	vu = eu->vu;
	vum = eum->vu;
	f = eu->f;
	fm = eum->f;
	
	/* if there are two faces */
	if (f!=fm) {
	
		/* delete the one owned by the edge-use mate */
		fm->m->f = f;
		fm->fPrev->fNext = fm->fNext;
		fm->fNext->fPrev = fm->fPrev;
		free(fm);
		
		/* edge-uses in deleted face are now owned by surviving face */
		for (eut=eum->euCW; eut->f!=f; eut=eut->euCW)
			eut->f = f;
		
		/* remember the surviving face */
		*fs = f;
	}
	
	/* ensure surviving face's edge-use is valid */
	f->eu = eu->euCW->euCW;
	
	/* hook up edge uses */
	eu->euCCW->euCW = eum->euCW;
	eum->euCCW->euCW = eu->euCW;
	eu->euCW->euCCW = eum->euCCW;
	eum->euCW->euCCW = eu->euCCW;
	
	/* delete edge uses */
	free(eu);
	free(eum);
	
	/* delete vertex-uses (and vertices if last use deleted) */
	if (vu->vuNext==vu)
		free(vu->v);
	else {
		vu->v->vu = vu->vuNext;
		vu->vuPrev->vuNext = vu->vuNext;
		vu->vuNext->vuPrev = vu->vuPrev;
		free(vu);
	}
	if (vum->vuNext==vum)
		free(vum->v);
	else {
		vum->v->vu = vum->vuNext;
		vum->vuPrev->vuNext = vum->vuNext;
		vum->vuNext->vuPrev = vum->vuPrev;
		free(vum);
	}
}

void killBoundaryEdge (Edge *e)
/*****************************************************************************
killBoundaryEdge -  Kill a boundary edge
******************************************************************************
Input:
e 	boundary Edge

******************************************************************************
Notes:
Killing a boundary edge is typically done after a new boundary vertex
is inserted on an existing boundary edge.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/04/91
*****************************************************************************/
{
	Face *f;
	EdgeUse *eu,*eui,*euo;
	VertexUse *vui,*vuo;
	
	/* determine edge uses and vertex uses inside and outside boundary */
	if (e->eu->f==NULL) {
		euo = e->eu;
		eui = euo->euMate;
	} else {
		eui = e->eu;
		euo = eui->euMate;
	}
	vui = eui->vu;
	vuo = euo->vu;
	
	/* delete face */
	f = eui->f;
	f->fNext->fPrev = f->fPrev;
	f->fPrev->fNext = f->fNext;
	f->m->f = f->fNext;
	free(f);
	
	/* loop over edge uses that will be outside */
	eu = eui->euCW;
	do {
		eu->f = NULL;
		eu->e->fixed = 1;
		eu = eu->euCW;		
	} while(eu!=eui);
	
	/* delete vertex uses */
	vui->vuNext->vuPrev = vui->vuPrev;
	vui->vuPrev->vuNext = vui->vuNext;
	vui->v->vu = vui->vuNext;
	free(vui);
	vuo->vuNext->vuPrev = vuo->vuPrev;
	vuo->vuPrev->vuNext = vuo->vuNext;
	vuo->v->vu = vuo->vuNext;
	free(vuo);
	
	/* delete edge uses */
	eui->euCW->euCCW = euo->euCCW;
	eui->euCCW->euCW = euo->euCW;
	euo->euCW->euCCW = eui->euCCW;
	euo->euCCW->euCW = eui->euCW;
	free(eui);
	free(euo);
	
	/* delete edge */
	free(e);
}
