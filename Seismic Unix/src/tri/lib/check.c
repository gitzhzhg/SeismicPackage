/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/******************************************************************************
CHECK - CHECK triangulated models
*******************************************************************************


badModel		bad Model flag
checkVertexUse		check Vertex Use
checkEdgeUse		check Edge Use
checkFace		check Face
checkModel		check Model

*******************************************************************************
Function Prototypes:

void badModel(void);
void checkVertexUse (VertexUse *vu);
void checkEdgeUse (EdgeUse *eu);
void checkFace (Face *f);
void checkModel (Model *m);

*******************************************************************************
checkVertexUse:
Input:
vu	Pointer to VertexUse

checkEdgeUse:
Input:
eu	pointer to EdgeUse

checkFace:
Input:
f	pointer to Face

checkModel:
Input:
m	pointer to Model

*******************************************************************************
Notes: Routines for checking triangulated models.

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/

/**************** end self doc ********************************/

#include "Triangles/triP.h"

void badModel(void)
/******************************************************************************
badModel - bad Model flag

*******************************************************************************
Note:  to be used with checkVertexUse, checkEdgeUse, and checkFace
*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	exit(-1);
}


void checkVertexUse (VertexUse *vu)
/******************************************************************************
checkVertexUse - check Vertex Use
*******************************************************************************
Input:
vu	Pointer to VertexUse
*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	VertexUse *vut;
	/* printf("vertexuse %d\n",vu); */
	if (vu->v!=vu->vuPrev->v) badModel();
	if (vu->v!=vu->vuNext->v) badModel();
	if (vu->vuNext->vuPrev!=vu) badModel();
	if (vu->vuPrev->vuNext!=vu) badModel();
	if (vu->eu->vu!=vu) badModel();
	vut = vu;
	do {
		if (vut->v!=vu->v) badModel();
		vut = vut->vuNext;
	} while(vut!=vu);
}

void checkEdgeUse (EdgeUse *eu)
/******************************************************************************
checkEdgeUse - check Edge Use
*******************************************************************************
Input:
eu	pointer to EdgeUse
*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	/* printf("edgeuse %d\n",eu); */
	if (eu->vu->eu!=eu) badModel();
	if (eu->euMate->euMate!=eu) badModel();
	if (eu->euCW->euCCW!=eu) badModel();
	if (eu->euCCW->euCW!=eu) badModel();
	if (eu->euCW==eu->euCCW) badModel();
	if (eu->e->eu!=eu && eu->e->eu!=eu->euMate) badModel();
	if (eu->euCW->euCW->euCW!=eu && eu->f!=NULL) badModel();
	if (eu->euCCW->euCCW->euCCW!=eu && eu->f!=NULL) badModel();
	checkVertexUse(eu->vu);
	checkVertexUse(eu->euMate->vu);
}	


void checkFace (Face *f)
/******************************************************************************
checkFace - check Face
*******************************************************************************
Input:
f	pointer to Face
*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	EdgeUse *eu;
	/* printf("face %d\n",f); */
	if (f->fPrev->fNext!=f) badModel();
	if (f->fNext->fPrev!=f) badModel();
	if (f->eu->f!=f) badModel();
	eu = f->eu;
	do {
		checkEdgeUse(eu);
		checkEdgeUse(eu->euMate);
		eu = eu->euCW;
	} while(eu!=f->eu);
}


void checkModel (Model *m)
/******************************************************************************
checkModel - check Model
*******************************************************************************
Input:
m	pointer to Model
*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/09/90
******************************************************************************/
{
	Face *f;
	/* printf("model %d\n",m); */
	if (m->f->m!=m) badModel();
	f = m->f;
	do {
		if (f->m!=m) badModel();
		checkFace(f);
		f = f->fNext;
	} while (f!=m->f);
}
