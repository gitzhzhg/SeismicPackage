/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
CREATE - create model, boundary edge triangles, edge face, edge vertex, add
         a vertex

makeModel		Make and return a pointer to a new model
makeBoundaryEdgeTri	Create a boundary edge and triangle 
makeEdgeFace		Create an edge by connecting two vertices
makeEdgeVertex		Create an edge connecting an existing vertex (v1) to a
                        new vertex
addVertexToModel	Add a vertex to model, and return pointer to new vertex
insideTriInModel	return pointer to triangle in model containing
                   	specified (x,y) coordinates

******************************************************************************
Function Prototypes:
Model *makeModel (float xmin, float ymin, float xmax, float ymax);
void makeBoundaryEdgeTri (Vertex *v, Edge **enew, Tri **tnew);
void makeEdgeFace (Vertex *v1, Vertex *v2, Edge **enew, Face **fnew);
Vertex* addVertexToModel (Model *m, float x, float y);
Tri* insideTriInModel (Model *m, Tri *start, float x, float y);

******************************************************************************
makeModel:
Input:
xmin		minimum x-coordinate
ymin		minimum y-coordinate
xmax		maximum x-coordinate
ymax		maximum y-coordinate

Returns: pointer to a new Model

makeBoundaryEdgeTri:
Input:
v		specified boundary Vertex

Output:
enew		new boundary Edge
tnew		new boundary triangle

Notes:
The specified vertex and the adjacent vertices on the boundary
are assumed to be colinear.  Therefore, the resulting
boundary triangle has zero area, and is intended to enable
deletion of the specified vertex from the boundary.

makeEdgeFace:
Input:
v1		First Vertex
v2		second Vertex

Output:
enew		new Edge
fnew		new Face

Notes:
The vertices must be adjacent to a single common face.
This face is closed off by the new edge, and a new edge and
a new face are made and returned. 

addVertexToModel:
Input:
m		model
x		x-coordinate of new vertex
y		y-coordinate of new vertex

Notes:
If the new vertex is close to an existing vertex, this function returns NULL.

insideTriInModel:
Input:
m		Model
start		triangle to look at first (NULL to begin looking anywhere)
x		x-coordinate
y		y-coordinate

Notes:
Points on an edge of a triangle are assumed to be inside that triangle.
An edge may be used by two triangles, so two triangles may "contain"
a point that lies on an edge.  The first triangle found to contain
the specified point is returned.

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, Fall 1990.
******************************************************************************/
/**************** end self doc ********************************/

#include "Triangles/triP.h"

Model *makeModel (float xmin, float ymin, float xmax, float ymax)
/*****************************************************************************
makeModel - Make and return a pointer to a new model
******************************************************************************
Input:
xmin		minimum x-coordinate
ymin		minimum y-coordinate
xmax		maximum x-coordinate
ymax		maximum y-coordinate

Returns: pointer to a new Model
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	Vertex *v1,*v2,*v3,*v4;
	VertexUse *vu12,*vu14,*vu21,*vu23,*vu24,*vu32,*vu34,*vu41,*vu42,*vu43;
	Edge *e1,*e2,*e3,*e4,*e5;
	EdgeUse *eu12,*eu14,*eu21,*eu23,*eu24,*eu32,*eu34,*eu41,*eu42,*eu43;
	Face *f124,*f234;
	Model *m;

	/* allocate space for 4 vertices, 5 edges, 2 faces, and 1 model */
	v1 = (Vertex*)malloc(sizeof(Vertex));
	v2 = (Vertex*)malloc(sizeof(Vertex));
	v3 = (Vertex*)malloc(sizeof(Vertex));
	v4 = (Vertex*)malloc(sizeof(Vertex));
	vu12 = (VertexUse*)malloc(sizeof(VertexUse));
	vu14 = (VertexUse*)malloc(sizeof(VertexUse));
	vu21 = (VertexUse*)malloc(sizeof(VertexUse));
	vu23 = (VertexUse*)malloc(sizeof(VertexUse));
	vu24 = (VertexUse*)malloc(sizeof(VertexUse));
	vu32 = (VertexUse*)malloc(sizeof(VertexUse));
	vu34 = (VertexUse*)malloc(sizeof(VertexUse));
	vu41 = (VertexUse*)malloc(sizeof(VertexUse));
	vu42 = (VertexUse*)malloc(sizeof(VertexUse));
	vu43 = (VertexUse*)malloc(sizeof(VertexUse));
	e1 = (Edge*)malloc(sizeof(Edge));
	e2 = (Edge*)malloc(sizeof(Edge));
	e3 = (Edge*)malloc(sizeof(Edge));
	e4 = (Edge*)malloc(sizeof(Edge));
	e5 = (Edge*)malloc(sizeof(Edge));
	eu12 = (EdgeUse*)malloc(sizeof(EdgeUse));
	eu21 = (EdgeUse*)malloc(sizeof(EdgeUse));
	eu23 = (EdgeUse*)malloc(sizeof(EdgeUse));
	eu32 = (EdgeUse*)malloc(sizeof(EdgeUse));
	eu34 = (EdgeUse*)malloc(sizeof(EdgeUse));
	eu43 = (EdgeUse*)malloc(sizeof(EdgeUse));
	eu41 = (EdgeUse*)malloc(sizeof(EdgeUse));
	eu14 = (EdgeUse*)malloc(sizeof(EdgeUse));
	eu24 = (EdgeUse*)malloc(sizeof(EdgeUse));
	eu42 = (EdgeUse*)malloc(sizeof(EdgeUse));
	f124 = (Face*)malloc(sizeof(Face));
	f234 = (Face*)malloc(sizeof(Face));
	m = (Model*)malloc(sizeof(Model));
	
	/* vertices */
	v1->x = xmin;  v1->y = ymax;  v1->vu = vu12;  v1->fixed = 1;
	v1->va = NULL;
	v2->x = xmax;  v2->y = ymax;  v2->vu = vu23;  v2->fixed = 1;
	v2->va = NULL;
	v3->x = xmax;  v3->y = ymin;  v3->vu = vu34;  v3->fixed = 1;
	v3->va = NULL;
	v4->x = xmin;  v4->y = ymin;  v4->vu = vu41;  v4->fixed = 1;
	v4->va = NULL;
	vu12->v = v1;  vu12->eu = eu12;
	vu12->vuPrev = vu14;  vu12->vuNext = vu14;  vu12->vua = NULL;
	vu14->v = v1;  vu14->eu = eu14;
	vu14->vuPrev = vu12;  vu14->vuNext = vu12;  vu14->vua = NULL;
	vu21->v = v2;  vu21->eu = eu21;
	vu21->vuPrev = vu24;  vu21->vuNext = vu23;  vu21->vua = NULL;
	vu23->v = v2;  vu23->eu = eu23;
	vu23->vuPrev = vu21;  vu23->vuNext = vu24;  vu23->vua = NULL;
	vu24->v = v2;  vu24->eu = eu24;
	vu24->vuPrev = vu23;  vu24->vuNext = vu21;  vu24->vua = NULL;
	vu32->v = v3;  vu32->eu = eu32;
	vu32->vuPrev = vu34;  vu32->vuNext = vu34;  vu32->vua = NULL;
	vu34->v = v3;  vu34->eu = eu34;
	vu34->vuPrev = vu32;  vu34->vuNext = vu32;  vu34->vua = NULL;
	vu41->v = v4;  vu41->eu = eu41;
	vu41->vuPrev = vu43;  vu41->vuNext = vu42;  vu41->vua = NULL;
	vu42->v = v4;  vu42->eu = eu42;
	vu42->vuPrev = vu41;  vu42->vuNext = vu43;  vu42->vua = NULL;
	vu43->v = v4;  vu43->eu = eu43;
	vu43->vuPrev = vu42;  vu43->vuNext = vu41;  vu43->vua = NULL;
		
	/* edges */
	e1->eu = eu12;  e1->fixed = 1;  e1->ea = NULL;
	e2->eu = eu23;  e2->fixed = 1;  e2->ea = NULL;
	e3->eu = eu34;  e3->fixed = 1;  e3->ea = NULL;
	e4->eu = eu41;  e4->fixed = 1;  e4->ea = NULL;
	e5->eu = eu24;  e5->fixed = 0;  e5->ea = NULL;
	eu12->e = e1;  eu12->euMate = eu21;  eu12->vu = vu12;  eu12->f = f124;
	eu21->e = e1;  eu21->euMate = eu12;  eu21->vu = vu21;  eu21->f = NULL;
	eu23->e = e2;  eu23->euMate = eu32;  eu23->vu = vu23;  eu23->f = f234;
	eu32->e = e2;  eu32->euMate = eu23;  eu32->vu = vu32;  eu32->f = NULL;
	eu34->e = e3;  eu34->euMate = eu43;  eu34->vu = vu34;  eu34->f = f234;
	eu43->e = e3;  eu43->euMate = eu34;  eu43->vu = vu43;  eu43->f = NULL;
	eu41->e = e4;  eu41->euMate = eu14;  eu41->vu = vu41;  eu41->f = f124;
	eu14->e = e4;  eu14->euMate = eu41;  eu14->vu = vu14;  eu14->f = NULL;
	eu24->e = e5;  eu24->euMate = eu42;  eu24->vu = vu24;  eu24->f = f124;
	eu42->e = e5;  eu42->euMate = eu24;  eu42->vu = vu42;  eu42->f = f234;
	eu12->euCW = eu24;  eu12->euCCW = eu41;  eu12->eua = NULL;
	eu21->euCW = eu14;  eu21->euCCW = eu32;  eu21->eua = NULL;
	eu23->euCW = eu34;  eu23->euCCW = eu42;  eu23->eua = NULL;
	eu32->euCW = eu21;  eu32->euCCW = eu43;  eu32->eua = NULL;
	eu34->euCW = eu42;  eu34->euCCW = eu23;  eu34->eua = NULL;
	eu43->euCW = eu32;  eu43->euCCW = eu14;  eu43->eua = NULL;
	eu41->euCW = eu12;  eu41->euCCW = eu24;  eu41->eua = NULL;
	eu14->euCW = eu43;  eu14->euCCW = eu21;  eu14->eua = NULL;
	eu24->euCW = eu41;  eu24->euCCW = eu12;  eu24->eua = NULL;
	eu42->euCW = eu23;  eu42->euCCW = eu34;  eu42->eua = NULL;
		
	/* face */
	f124->m = f234->m = m;
	f124->fPrev = f124->fNext = f234;
	f234->fPrev = f234->fNext = f124;
	f124->eu = eu12;  
	f234->eu = eu23;
	f124->fa = NULL;
	f234->fa = NULL;
	circumTri(f124);
	circumTri(f234);
	
	/* model */
	m->f = f124;
	m->xmin = xmin;
	m->ymin = ymin;
	m->xmax = xmax;
	m->ymax = ymax;
	m->eps = 0.005*sqrt((xmax-xmin)*(xmax-xmin)+(ymax-ymin)*(ymax-ymin));
	m->vAdd = NULL;
	m->vDel = NULL;
	m->tAdd = NULL;
	m->tDel = NULL;
	m->sma = m->sfa = m->seua = m->sea = m->svua = m->sva = 0;

	/* return pointer to model */
	return m;
}

void makeBoundaryEdgeTri (Vertex *v, Edge **enew, Tri **tnew)
/******************************************************************************
makeBoundaryEdgeTri - Create a boundary edge and triangle by connecting two 
                      vertices adjacent to the specified boundary vertex.
*******************************************************************************
Input:
v		specified boundary Vertex

Output:
enew		new boundary Edge
tnew		new boundary triangle

*******************************************************************************
Notes:
The specified vertex and the adjacent vertices on the boundary
are assumed to be colinear.  Therefore, the resulting
boundary triangle has zero area, and is intended to enable
deletion of the specified vertex from the boundary.
*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	Vertex *vi,*vo;
	VertexUse *vu,*vui,*vuo;
	Edge *e;
	EdgeUse *eu,*eui,*euo;
	Face *f;
	
	/* determine boundary edge use corresponding to boundary vertex */
	vu = v->vu;
	do {
		eu = vu->eu;
		vu = vu->vuNext;
	} while (eu->f!=NULL);
	
	/* make boundary edge and face, 2 edge uses, and 2 vertex uses */
	e = (Edge*)malloc(sizeof(Edge));  e->ea = NULL;
	f = (Face*)malloc(sizeof(Face));  f->fa = NULL;
	eui = (EdgeUse*)malloc(sizeof(EdgeUse));  eui->eua = NULL;
	euo = (EdgeUse*)malloc(sizeof(EdgeUse));  euo->eua = NULL;
	vui = (VertexUse*)malloc(sizeof(VertexUse));  vui->vua = NULL;
	vuo = (VertexUse*)malloc(sizeof(VertexUse));  vuo->vua = NULL;
	
	/* vertices corresponding to inner and outer vertex uses */
	vi = eu->euCW->vu->v;
	vo = eu->euCCW->vu->v;
	
	/* vertex uses inside and outside new boundary edge */
	vui->eu = eui;
	vui->vuPrev = vi->vu;
	vui->vuNext = vi->vu->vuNext;
	vui->vuPrev->vuNext = vui;
	vui->vuNext->vuPrev = vui;
	vui->v = vi;
	vuo->eu = euo;
	vuo->vuPrev = vo->vu;
	vuo->vuNext = vo->vu->vuNext;
	vuo->vuPrev->vuNext = vuo;
	vuo->vuNext->vuPrev = vuo;
	vuo->v = vo;
	
	/* new boundary edge */
	e->eu = eui;
	e->fixed = 1;
	
	/* edge uses inside and outside new boundary edge */
	eu->f = eu->euCCW->f = f;
	eui->f = f;
	eui->vu = vui;
	eui->euMate = euo;
	eui->euCW = eu->euCCW;
	eui->euCCW = eu;
	eui->e = e;
	euo->f = NULL;
	euo->vu = vuo;
	euo->euMate = eui;
	euo->euCW = eu->euCW;
	euo->euCCW = eu->euCCW->euCCW;
	euo->e = e;
	eui->euCW->euCCW = eui;
	eui->euCCW->euCW = eui;
	euo->euCW->euCCW = euo;
	euo->euCCW->euCW = euo;
	
	/* face */
	f->m = eu->euMate->f->m;
	f->fPrev = f->m->f;
	f->fNext = f->m->f->fNext;
	f->fPrev->fNext = f;
	f->fNext->fPrev = f;
	f->eu = eu;
	circumTri(f);
	
	/* return new edge and triangle */
	*enew = e;
	*tnew = f;
}

void makeEdgeFace (Vertex *v1, Vertex *v2, Edge **enew, Face **fnew)
/******************************************************************************
makeEdgeFace - Create an edge by connecting two vertices.
*******************************************************************************
Input:
v1		First Vertex
v2		second Vertex

Output:
enew		new Edge
fnew		new Face

*******************************************************************************
Notes:
The vertices must be adjacent to a single common face.
This face is closed off by the new edge, and a new edge and
a new face are made and returned. 

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	VertexUse *vu,*vum,*vu1,*vu2;
	Edge *e;
	EdgeUse *eu,*eum,*eu1,*eu2,*eut;
	Face *f,*fm;
	
	/* find vertex-use of v1 that shares a face with a vu of v2 */
	vu2 = NULL;
	vu1 = v1->vu;
	while (vu2==NULL) {
		for (vu = vu1->eu->euCW->vu; 
			vu!=vu1 && vu->eu->f!=NULL && vu->v!=v2;
			vu = vu->eu->euCW->vu);
		if (vu->v==v2)
			vu2 = vu;
		else
			vu1 = vu1->vuNext;
	}
	
	/* create two vertex-uses, one for each end of edge */
	vu = (VertexUse*)malloc(sizeof(VertexUse));  vu->vua = NULL;
	vu->v = v2;
	vu->vuPrev = vu2;
	vu->vuNext = vu2->vuNext;
	vu2->vuNext->vuPrev = vu;
	vu2->vuNext = vu;
	vum = (VertexUse*)malloc(sizeof(VertexUse));  vum->vua = NULL;
	vum->v = v1;
	vum->vuPrev = vu1;
	vum->vuNext = vu1->vuNext;
	vu1->vuNext->vuPrev = vum;
	vu1->vuNext = vum;
	
	/* create edge and two edge-uses, one for each side of edge */
	eu1 = vu1->eu;
	eu2 = vu2->eu;
	e = (Edge*)malloc(sizeof(Edge));  e->ea = NULL;
	e->eu = eu = (EdgeUse*)malloc(sizeof(EdgeUse));  eu->eua = NULL;
	e->fixed = 0;
	eum = (EdgeUse*)malloc(sizeof(EdgeUse));  eum->eua = NULL;
	eu->e = eum->e = e;
	eu->vu = vu;
	vu->eu = eu;
	eum->vu = vum;
	vum->eu = eum;
	eu->euMate = eum;
	eum->euMate = eu;
	eu->euCW = eu1;
	eu->euCCW = eu2->euCCW;
	eum->euCW = eu2;
	eum->euCCW = eu1->euCCW;
	eu1->euCCW->euCW = eum;
	eu2->euCCW->euCW = eu;
	eu1->euCCW = eu;
	eu2->euCCW = eum;
	
	/* create a face */
	f = eu1->f;
	fm = (Face*)malloc(sizeof(Face));  fm->fa = NULL;
	fm->eu = eum;
	fm->fPrev = f;
	fm->fNext = f->fNext;
	f->fNext->fPrev = fm;
	f->fNext = fm;
	fm->m = f->m;
	
	/* make all edge-uses owned by the appropriate faces */
	f->eu = eu1;
	eu->f = f;
	eum->f = fm;
	for (eut=eu2; eut->f!=fm; eut=eut->euCW)
		eut->f = fm;
	
	/* set output values */
	*enew = e;
	*fnew = fm;
}

void makeEdgeVertex (Vertex *v1, float x, float y, Face *f, 
	Edge **enew, Vertex **vnew)
/******************************************************************************
makeEdgeVertex - Create an edge connecting an existing vertex (v1) to a
                 new vertex (with coordinates x,y) in a specified face f. 
                 Return the new edge and the new vertex.
*******************************************************************************
Input:
v1		existing Vertex
x		x-coordinate of new vertex
y		y-coordinate of new vertex
f		specified Face

Output:
enew		new Edge
vnew		new Vertex

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	Vertex *v;
	VertexUse *vu,*vum,*vu1;
	Edge *e;
	EdgeUse *eu,*eum,*eu1;
	
	/* determine which use of vertex v1 is adjacent to face f */
	for (vu1=v1->vu; vu1->eu->f!=f; vu1=vu1->vuNext);
	
	/* create a new vertex and vertex-use */
	v = (Vertex*)malloc(sizeof(Vertex));  v->va = NULL;
	vu = (VertexUse*)malloc(sizeof(VertexUse));  vu->vua = NULL;
	v->x = x;
	v->y = y;
	v->fixed = 0;
	v->vu = vu;
	vu->v = v;
	vu->vuNext = vu->vuPrev = vu;
	
	/* create a new vertex-use for existing vertex */
	vum = (VertexUse*)malloc(sizeof(VertexUse));  vum->vua = NULL;
	vum->v = v1;
	vum->vuPrev = vu1;
	vum->vuNext = vu1->vuNext;
	vu1->vuNext->vuPrev = vum;
	vu1->vuNext = vum;
	
	/* create edge and two edge-uses, one for each side of edge */
	eu1 = vu1->eu;
	e = (Edge*)malloc(sizeof(Edge));  e->ea = NULL;
	e->eu = eu = (EdgeUse*)malloc(sizeof(EdgeUse));  eu->eua = NULL;
	e->fixed = 0;
	eum = (EdgeUse*)malloc(sizeof(EdgeUse));  eum->eua = NULL;
	eu->e = eum->e = e;
	eu->f = eum->f = f;
	eu->vu = vu;
	vu->eu = eu;
	eum->vu = vum;
	vum->eu = eum;
	eu->euMate = eum;
	eum->euMate = eu;
	eu->euCW = eu1;
	eu->euCCW = eum;
	eum->euCW = eu;
	eum->euCCW = eu1->euCCW;
	eu1->euCCW->euCW = eum;
	eu1->euCCW = eu;
	
	/* set output values */
	*enew = e;
	*vnew = v;
}

Vertex *addVertexToModel (Model *m, float x, float y)
/*****************************************************************************
addVertexToModel - Add a vertex to model, and return pointer to new vertex
******************************************************************************
Input:
m		model
x		x-coordinate of new vertex
y		y-coordinate of new vertex
******************************************************************************
Notes:
If the new vertex is close to an existing vertex, this function returns NULL.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	int first,fixed=0;
	Vertex *v,*vo,*v1,*v2,*v3,*v1fixed=NULL,*v2fixed=NULL;
	VertexUse *vu;
	Edge *e,*e1,*e2,*e3,*eb=NULL;
	EdgeUse *eu,*euo;
	Face *f,*fs,*f1,*f2,*f3;
	
	/* find existing vertex nearest to new vertex */
	v = nearestVertexInModel(m,NULL,x,y);
	
	/* if new vertex is too close to existing vertex, return existing */
	if (sqrt((v->x-x)*(v->x-x)+(v->y-y)*(v->y-y))<m->eps) return NULL;
	
	/* find triangle that contains new vertex */
	f = (v->vu->eu->f!=NULL?v->vu->eu->f:v->vu->eu->euMate->f);
	f = insideTriInModel(m,f,x,y);
	
	/* if the edge nearest new vertex is a fixed edge and very nearby */
	e = nearestEdgeInModel(m,f->eu->e,x,y);
	if (e->fixed && distanceToEdge(e,x,y)<m->eps) {
		
		/* remember the endpoints of the fixed edge */
		v1fixed = e->eu->vu->v;
		v2fixed = e->eu->euCW->vu->v;
		
		/* unfix the fixed edge */
		e->fixed = 0;
		
		/* project new vertex onto edge */
		projectToEdge(e,&x,&y);
		
		/* remember edge was fixed */
		fixed = 1;
		
		/* if edge is on boundary, remember to delete it later */
		if (e->eu->f==NULL || e->eu->euMate->f==NULL) eb = e;
	}
	
	/* this triangle will be deleted */
	if (m->tDel!=NULL) m->tDel(m,f);
	
	/* make edges from new vertex to vertices of this triangle */
	vu = f->eu->vu;
	v1 = vu->v;
	v2 = vu->eu->euCW->vu->v;
	v3 = vu->eu->euCCW->vu->v;
	f1 = f;
	makeEdgeVertex(v1,x,y,f1,&e1,&v);
	makeEdgeFace(v2,v,&e2,&f2);
	makeEdgeFace(v3,v,&e3,&f3);
	
	/* 3 triangles were made */
	if (m->tAdd!=NULL) {
		m->tAdd(m,f1);
		m->tAdd(m,f2);
		m->tAdd(m,f3);
	}
	
	/* compute circumcircles of the 3 new triangles */
	vu = v->vu;
	do {
		circumTri(vu->eu->f);
		vu = vu->vuNext;
	} while (vu!=v->vu);
	
	/* loop over vertex-uses (triangles) in clockwise direction */
	vu = v->vu;
	first = 1;
	do {
		/* determine edge-uses and edge opposite new vertex */
		eu = vu->eu->euCW;
		euo = eu->euMate;
		e = eu->e;
		
		/* if opposite triangle's circumcircle contains new vertex */
		if (euo->f!=NULL && 
			!euo->e->fixed && 
			inCircumTri(x,y,euo->f)) {
			
			/* determine vertex opposite edge */
			vo = euo->euCCW->vu->v;
			
			/* 2 triangles will be deleted */
			if (m->tDel!=NULL) {
				m->tDel(m,e->eu->f);
				m->tDel(m,e->eu->euMate->f);
			}
			
			/* kill opposite edge */
			killEdge(e,&fs);
			
			/* make edge connecting new and opposite vertices */
			makeEdgeFace(v,vo,&e,&f);
			
			/* 2 triangles were created */
			if (m->tAdd!=NULL) {
				m->tAdd(m,e->eu->f);
				m->tAdd(m,e->eu->euMate->f);
			}
			
			/* compute circumcircles of 2 new triangles */
			circumTri(e->eu->f);
			circumTri(e->eu->euMate->f);
		
		/* else, go to next vertex-use (triangle) clockwise */
		} else {
			vu = vu->eu->euCCW->euMate->vu;
			first = 0;
		}
		
	} while (vu!=v->vu || first);
	
	/* if new vertex was added on an edge that was fixed */
	if (fixed) {
		
		/* loop over vertex uses of new vertex */
		vu = v->vu;
		do {
			/* edge use */
			eu = vu->eu;
			
			/* fix edge if it was part of fixed edge */
			if (eu->euCCW->vu->v==v1fixed)
				eu->euCCW->e->fixed = 1;
			if (eu->euCW->vu->v==v2fixed)
				eu->e->fixed = 1;
			
			/* next vertex use */
			vu = vu->vuNext;
			
		} while (vu!=v->vu);
	}
	
	/* if edge on boundary needs to be deleted, delete it */
	if (eb!=NULL) killBoundaryEdge(eb);
	
	/* if specified, do add vertex function */
	if (m->vAdd!=NULL) m->vAdd(m,v);
	
	/* debug */
	/* checkModel(m); */
	
	/* return pointer to vertex */
	return v;
}

Tri* insideTriInModel (Model *m, Tri *start, float x, float y)
/*****************************************************************************
insideTriInModel - return pointer to triangle in model containing
                   specified (x,y) coordinates
******************************************************************************
Input:
m		Model
start		triangle to look at first (NULL to begin looking anywhere)
x		x-coordinate
y		y-coordinate

******************************************************************************
Notes:
Points on an edge of a triangle are assumed to be inside that triangle.
An edge may be used by two triangles, so two triangles may "contain"
a point that lies on an edge.  The first triangle found to contain
the specified point is returned.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/11/90
******************************************************************************/
{
	int inside=0;
	float x1,y1,x2,y2,x3,y3,s1,s2;
	Vertex *v1,*v2,*v3;
	EdgeUse *eu;
	Tri *t;
	
	/* start at some face in model */
	t = (start==NULL ? m->f : start);
	
	/* loop over triangles until point is inside */
	while (!inside) {
		
		/* loop over all edge-uses in triangle */
		eu = t->eu;
		do {
		
			/* vertices at ends of edge */
			v1 = eu->vu->v;  x1 = v1->x;  y1 = v1->y;
			v2 = eu->euCW->vu->v;  x2 = v2->x;  y2 = v2->y;
			
			/* other vertex */
			v3 = eu->euCCW->vu->v;  x3 = v3->x;  y3 = v3->y;
			
			/* cross-products */
			s1 = (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1);
			s2 = (x2-x1)*(y-y1)-(x-x1)*(y2-y1);
			
			/* if cross-products have different sign */
			if (s1*s2<0.0) {
				
				/* if triangle opposite current edge exists */
				if (eu->euMate->f!=NULL) {
				
					/* look at that triangle next */
					t = eu->euMate->f;
					break;
				}
			}	
						
			/* next edge-use */
			eu = eu->euCW;
			
		} while (eu!=t->eu);
		
		/* if did not break, then point is inside triangle */
		if (eu==t->eu) inside = 1;
	}
	
	/* return pointer to triangle containing point */
	return t;
}
