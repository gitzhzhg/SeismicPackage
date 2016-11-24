/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*****************************************************************************
READWRITE - READ or WRITE a triangulated model

readModel		Read a model in the form produced by writeModel
writeModel		Write a model to a file

******************************************************************************
Function Prototypes:
Model *readModel (FILE *fp);
void writeModel (Model *m, FILE *fp);

******************************************************************************
readModel:
Input:
fp		file pointer to file containing model

writeModel:
Input:
m		pointer to Model
fp		file pointer

******************************************************************************
Author:  Jack K. Cohen, Center for Wave Phenomena, 09/21/90
Modified:  Dave Hale, Center for Wave Phenomena, 11/30/90
	Converted representation of model from ascii to binary for speed.
	Added code to read attributes.
Modified (writeModel):  Craig Artley, Center for Wave Phenomena, 04/08/94
	Corrected bug;  previously the edgeuses and vertextuses of the
	exterior face were not written.
******************************************************************************/

/**************** end self doc ********************************/

#include "Triangles/triP.h"

/* functions defined and used internally */
static void createModel (Model **mptr, Address **fheadptr, FILE *fp);
static void createFace (Model *m, Address **fheadptr, Address **euheadptr,
	size_t sfa, FILE *fp);
static void createEdgeUse (Address **fheadptr, Address **euheadptr,
	Address **vuheadptr, Address **eheadptr, size_t seua, FILE *fp);
static void createVertexUse (Address **euheadptr, Address **vuheadptr,
	Address **vheadptr, size_t svua, FILE *fp);
static void createEdge (Address **euheadptr, Address **eheadptr, size_t sea,
	FILE *fp);
static void createVertex (Address **vuheadptr, Address **vheadptr, size_t sva,
	FILE *fp);
static Address *fillAddress (Address *head, address_t oa, address_t na);
static Address *getAddress (Address *head, address_t oa, address_t *value);

/* macros */
#define DONODE(node, headptr, oa, type)				\
{								\
	int found;						\
	address_t value;					\
	headptr = updateAddressTree(headptr, oa, &found);	\
	if (!found) {						\
		node = (type *) malloc(sizeof(type));		\
		headptr = fillAddress(headptr, oa, node);	\
	} else {						\
		headptr = getAddress(headptr, oa, &value);	\
		node = (type *) value;				\
	}							\
}
#define DOFIELD(node, field, headptr, oa, type)			\
{								\
	int found;						\
	address_t value;					\
	type *field;						\
	headptr = updateAddressTree(headptr, oa, &found);	\
	if (!found) {						\
		field = (type *) malloc(sizeof(type));		\
		headptr = fillAddress(headptr, oa, field);	\
		node->field = field;				\
	} else {						\
		headptr = getAddress(headptr, oa, &value);	\
		node->field = (type *) value;			\
	}							\
}

Model *readModel (FILE *fp)
/*****************************************************************************
readModel - Read a model in the form produced by writeModel
******************************************************************************
Input:
fp		file pointer to file containing model

******************************************************************************
Author:  Jack K. Cohen, Center for Wave Phenomena, 09/21/90
Modified:  Dave Hale, Center for Wave Phenomena, 11/30/90
	Converted representation of model from ascii to binary for speed.
	Added code to read attributes.
******************************************************************************/
{
	Model *m=NULL;		/* pointer to model to be returned */
	Address *fhead  = NULL;	/* pointers to binary trees	*/
	Address *euhead = NULL;
	Address *vuhead = NULL;
	Address *ehead  = NULL;
	Address *vhead  = NULL;
	int type;
	
	while(fread(&type,sizeof(int),1,fp)) {
		if (type==MODELTYPE) {
			createModel(&m,&fhead,fp);
		} else if (type==FACETYPE) {
			createFace(m,&fhead,&euhead,m->sfa,fp);
		} else if (type==EDGEUSETYPE) {
			createEdgeUse(&fhead,&euhead,&vuhead,&ehead,
				m->seua,fp);
		} else if (type==VERTEXUSETYPE) {
			createVertexUse(&euhead,&vuhead,&vhead,m->svua,fp);
		} else if (type==EDGETYPE) {
			createEdge(&euhead,&ehead,m->sea,fp);
		} else if (type==VERTEXTYPE) {
			createVertex(&vuhead,&vhead,m->sva,fp);
		} else {
			fprintf(stderr,"invalid type in model\n");
			exit(EXIT_FAILURE);
		}
	}
	return m;
}

static void createModel(Model **mptr, Address **fheadptr, FILE *fp)
{
	Model *m;
	Face *f;
	int found;

	*mptr = m = (Model*)malloc(sizeof(Model));
	fread(m,sizeof(address_t),1,fp);
	if (fread(m,sizeof(Model),1,fp)!=1) {
		fprintf(stderr,"error reading Model\n");
		exit(EXIT_FAILURE);
	}
	*fheadptr = updateAddressTree(*fheadptr,m->f,&found);
	if (!found) {
		f = (Face*)malloc(sizeof(Face));
		*fheadptr = fillAddress(*fheadptr,m->f,f);
		m->f = f;
	} else {
		fprintf(stderr,"Model's first face already exists\n");
		exit(EXIT_FAILURE);
	}
	m->vAdd = NULL;
	m->vDel = NULL;
	m->tAdd = NULL;
	m->tDel = NULL;
	if (m->ma!=NULL && m->sma!=0) {
		m->ma = malloc(m->sma);
		if (fread(m->ma,m->sma,1,fp)!=1) {
			fprintf(stderr,"error reading model attributes\n");
			exit(EXIT_FAILURE);
		}
	} else {
		m->ma = NULL;
	}
}

static void createFace (Model *m, Address **fheadptr, Address **euheadptr,
	size_t sfa, FILE *fp)
{
	Face *of,*f;

	fread(&of,sizeof(Face*),1,fp);
	DONODE(f,*fheadptr,of,Face);
	if (fread(f,sizeof(Face),1,fp)!=1) {
		fprintf(stderr,"error reading Face\n");
		exit(EXIT_FAILURE);
	}
	f->m = m;
	DOFIELD(f,fPrev,*fheadptr,f->fPrev,Face);
	DOFIELD(f,fNext,*fheadptr,f->fNext,Face);
	DOFIELD(f,eu,*euheadptr,f->eu,EdgeUse);
	if (f->fa!=NULL && sfa!=0) {
		f->fa = malloc(sfa);
		if (fread(f->fa,sfa,1,fp)!=1) {
			fprintf(stderr,"error reading FaceAttributes\n");
			exit(EXIT_FAILURE);
		}
	} else {
		f->fa = NULL;
	}
}

static void createEdgeUse (Address **fheadptr, Address **euheadptr,
	Address **vuheadptr, Address **eheadptr, size_t seua, FILE *fp)
{
	EdgeUse *oeu,*eu;
	
	fread(&oeu,sizeof(EdgeUse*),1,fp);
	DONODE(eu,*euheadptr,oeu,EdgeUse);
	if (fread(eu,sizeof(EdgeUse),1,fp)!=1) {
		fprintf(stderr,"error reading EdgeUse\n");
		exit(EXIT_FAILURE);
	}
	if (eu->f==NULL) eu->f = NULL;
	else DOFIELD(eu,f,*fheadptr,eu->f,Face);
	DOFIELD(eu,vu,*vuheadptr,eu->vu,VertexUse);
	DOFIELD(eu,euMate,*euheadptr,eu->euMate,EdgeUse);
	DOFIELD(eu,euCW,*euheadptr,eu->euCW,EdgeUse);
	DOFIELD(eu,euCCW,*euheadptr,eu->euCCW,EdgeUse);
	DOFIELD(eu,e,*eheadptr,eu->e,Edge);
	if (eu->eua!=NULL && seua!=0) {
		eu->eua = malloc(seua);
		if (fread(eu->eua,seua,1,fp)!=1) {
			fprintf(stderr,"error reading edge-use attributes\n");
			exit(EXIT_FAILURE);
		}
	} else {
		eu->eua = NULL;
	}
}

static void createVertexUse (Address **euheadptr, Address **vuheadptr,
	Address **vheadptr, size_t svua, FILE *fp)
{
	VertexUse *ovu,*vu;
	
	fread(&ovu,sizeof(VertexUse*),1,fp);
	DONODE(vu,*vuheadptr,ovu,VertexUse);
	if (fread(vu,sizeof(VertexUse),1,fp)!=1) {
		fprintf(stderr,"error reading VertexUse\n");
		exit(EXIT_FAILURE);
	}
	DOFIELD(vu,eu,*euheadptr,vu->eu,EdgeUse);
	DOFIELD(vu,vuPrev,*vuheadptr,vu->vuPrev,VertexUse);
	DOFIELD(vu,vuNext,*vuheadptr,vu->vuNext,VertexUse);
	DOFIELD(vu,v,*vheadptr,vu->v,Vertex);
	if (vu->vua!=NULL && svua!=0) {
		vu->vua = malloc(svua);
		if (fread(vu->vua,svua,1,fp)!=1) {
			fprintf(stderr,
				"error reading vertex-use attributes\n");
			exit(EXIT_FAILURE);
		}
	} else {
		vu->vua = NULL;
	}
}

static void createEdge (Address **euheadptr, Address **eheadptr, size_t sea,
	FILE *fp)
{
	Edge *oe,*e;
	
	fread(&oe,sizeof(Edge*),1,fp);
	DONODE(e,*eheadptr,oe,Edge);
	if (fread(e,sizeof(Edge),1,fp)!=1) {
		fprintf(stderr,"error reading Edge\n");
		exit(EXIT_FAILURE);
	}
	DOFIELD(e,eu,*euheadptr,e->eu,EdgeUse);
	if (e->ea!=NULL && sea!=0) {
		e->ea = malloc(sea);
		if (fread(e->ea,sea,1,fp)!=1) {
			fprintf(stderr,"error reading edge attributes\n");
			exit(EXIT_FAILURE);
		}
	} else {
		e->ea = NULL;
	}
}

static void createVertex (Address **vuheadptr, Address **vheadptr, size_t sva,
	FILE *fp)
{
	Vertex *ov,*v;
	
	fread(&ov,sizeof(Vertex*),1,fp);
	DONODE(v,*vheadptr,ov,Vertex);
	if (fread(v,sizeof(Vertex),1,fp)!=1) {
		fprintf(stderr,"error reading Vertex");
		exit(EXIT_FAILURE);
	}
	DOFIELD(v,vu,*vuheadptr,v->vu,VertexUse);
	if (v->va!=NULL && sva!=0) {
		v->va = malloc(sva);
		if (fread(v->va,sva,1,fp)!=1) {
			fprintf(stderr,"error reading vertex attributes\n");
			exit(EXIT_FAILURE);
		}
	} else {
		v->va = NULL;
	}
}

static Address *fillAddress(Address *p, address_t oa, address_t na)
/* tree searcher to register the newly allocated address */
{
	off_t sign;

	if (p == NULL) {  /* not in tree--error */
		fprintf(stderr, "fillAddress called on non-existent node\n");
		exit(EXIT_FAILURE);
	}
	else if ((sign = (off_t)oa - (off_t)(p->oaddress)) < 0)
		p->aLeft = fillAddress(p->aLeft, oa, na);
	else if (sign > 0)
		p->aRight = fillAddress(p->aRight, oa, na);
	else /* sign == 0; we've found the entry to fill */
		p->naddress = na;

	return p;
}

static Address *getAddress(Address *p, address_t oa, address_t *value)
/* tree searcher to fetch a newly allocated address */
{
	off_t sign;

	if (p == NULL) {  /* not in tree--error */
		fprintf(stderr, "getAddress called on non-existent node\n");
		exit(EXIT_FAILURE);
	}
	else if ((sign = (off_t)oa - (off_t)(p->oaddress)) < 0)
		p->aLeft = getAddress(p->aLeft, oa, value);
	else if (sign > 0)
		p->aRight = getAddress(p->aRight, oa, value);
	else /* sign == 0; we've found the value to return */
		*value = p->naddress;

	return p;
}


#ifdef TEST
main()
{
	Model *m;

	m = readModel(0,0,0,0,0,0,stdin);
	writeModel(m,0,0,0,0,0,0,stdout);
}
#endif


/* functions defined and used internally */
static void _writeModel (FILE *fp, Model *m, size_t sma);
static void writeFace (FILE *fp, Face *f, size_t sfa);
static void writeEdgeUse (FILE *fp, EdgeUse *eu, size_t seua);
static void writeEdge (FILE *fp, Edge *e, size_t sea);
static void writeVertexUse (FILE *fp, VertexUse *vu, size_t svua);
static void writeVertex (FILE *fp, Vertex *v, size_t sva);

void writeModel (Model *m, FILE *fp)
/*****************************************************************************
writeModel - Write a model to a file
******************************************************************************
Input:
m		pointer to Model
fp		file pointer

******************************************************************************
Author:    Jack K. Cohen, Center for Wave Phenomena, 09/15/90
Modified:  Dave Hale, Center for Wave Phenomena, 11/29/90
	Converted representation of model from ascii to binary for speed.
	Added code to write attributes.
Modified:  Craig Artley, Center for Wave Phenomena, 04/08/94
	Corrected bug;  previously the edgeuses and vertextuses of the
	exterior face were not written.
******************************************************************************/
{
	Face *f;
	EdgeUse *eu,*euFirst;
	Address *vhead=NULL,*ehead=NULL;
	int repeat;


	/* write the model data */
	_writeModel(fp,m,m->sma);

	/* loop over all faces in model */
	f = m->f;
	do {	

		writeFace(fp,f,m->sfa);

		/* loop over all edgeuses in face */
		eu = f->eu;
		do {
		
			/* write edgeuse, edge, vertexuse, and vertex */
			writeEdgeUse(fp,eu,m->seua);
			ehead = updateAddressTree(ehead,eu->e,&repeat);
			if (!repeat) writeEdge(fp,eu->e,m->sea);
			writeVertexUse(fp,eu->vu,m->svua);
			vhead = updateAddressTree(vhead,eu->vu->v,&repeat);
			if (!repeat) writeVertex(fp,eu->vu->v,m->sva);
				
			/* next edgeuse */
			eu = eu->euCW;

		} while (eu!=f->eu);
		
		/* next face */
		f = f->fNext;

	} while (f!=m->f);

	/* loop over all faces in model, looking for the exterior */
	f = m->f;
	do {

		/* find an edgeuse belonging to the exterior face */
		if (f->eu->euMate->f==NULL) {
			eu = f->eu->euMate;
			break;
		} else if (f->eu->euCW->euMate->f==NULL) {
			eu = f->eu->euCW->euMate;
			break;
		} else if (f->eu->euCCW->euMate->f==NULL) {
			eu = f->eu->euCCW->euMate;
			break;
		}

		/* next face */
		f = f->fNext;

	} while (f!=m->f);

	/* confirm the resulting edgeuse belongs to the exterior */
	if (eu->f!=NULL) {
		fprintf(stderr,"writeModel: can't find exterior face\n");
		exit(EXIT_FAILURE);
	}

	/* loop over all edgeuses in exterior face */
	euFirst = eu;
	do {
		/* write edgeuse and vertexuse (edges & vertices done above) */
		writeEdgeUse(fp,eu,m->seua);
		writeVertexUse(fp,eu->vu,m->svua);

		/* next edgeuse */
		eu = eu->euCW;

	} while (eu!=euFirst);
}

static void _writeModel(FILE *fp, Model *m, size_t sma)
{
	int type=MODELTYPE;
	fwrite(&type,sizeof(int),1,fp);
	fwrite(&m,sizeof(Model*),1,fp);
	fwrite(m,sizeof(Model),1,fp);
	if (m->ma!=NULL && sma!=0) fwrite(m->ma,sma,1,fp);
}

static void writeFace(FILE *fp, Face *f, size_t sfa)
{
	int type=FACETYPE;
	fwrite(&type,sizeof(int),1,fp);
	fwrite(&f,sizeof(Face*),1,fp);
 	fwrite(f,sizeof(Face),1,fp);
	if (f->fa!=NULL && sfa!=0) fwrite(f->fa,sfa,1,fp);
}

static void writeEdgeUse(FILE *fp, EdgeUse *eu, size_t seua)
{
	int type=EDGEUSETYPE;
	fwrite(&type,sizeof(int),1,fp);
	fwrite(&eu,sizeof(EdgeUse*),1,fp);
	fwrite(eu,sizeof(EdgeUse),1,fp);
	if (eu->eua!=NULL && seua!=0) fwrite(eu->eua,seua,1,fp);
}

static void writeEdge(FILE *fp, Edge *e, size_t sea)
{
	int type=EDGETYPE;
	fwrite(&type,sizeof(int),1,fp);
	fwrite(&e,sizeof(Edge*),1,fp);
	fwrite(e,sizeof(Edge),1,fp);
	if (e->ea!=NULL && sea!=0) fwrite(e->ea,sea,1,fp);
}

static void writeVertexUse(FILE *fp, VertexUse *vu, size_t svua)
{
	int type=VERTEXUSETYPE;
	fwrite(&type,sizeof(int),1,fp);
	fwrite(&vu,sizeof(VertexUse*),1,fp);
	fwrite(vu,sizeof(VertexUse),1,fp);
	if (vu->vua!=NULL && svua!=0) fwrite(vu->vua,svua,1,fp);
}

static void writeVertex(FILE *fp, Vertex *v, size_t sva)
{
	int type=VERTEXTYPE;
	fwrite(&type,sizeof(int),1,fp);
	fwrite(&v,sizeof(Vertex*),1,fp);
	fwrite(v,sizeof(Vertex),1,fp);
	if (v->va!=NULL && sva!=0) fwrite(v->va,sva,1,fp);
}


#ifdef TEST
main()
{
	Model *m;

	m = makeModel(1.0,2.0,5.0,6.0);
	writeModel(m,stdout);
}
#endif


Address *updateAddressTree(Address *p, address_t oa, int *found)
/* add nodes to a tree on the fly--after Kernighan and Ritchie, */
/* The C Programming Language, 2nd Edition, p. 139ff. */
/* Used internally in readModel() and writeModel(). */
{
	off_t sign;

	if (p == NULL) {  /* not in tree */
		p = (Address*) malloc(sizeof(Address));
		p->oaddress = oa;
		p->aLeft = p->aRight = NULL;
		*found = 0;
	}
	else if ((sign = ((off_t)oa - (off_t)(p->oaddress))) < 0)
		p->aLeft = updateAddressTree(p->aLeft, oa, found);
	else if (sign > 0)
		p->aRight = updateAddressTree(p->aRight, oa, found);
	else /* sign == 0; we've found the entry */
		*found = 1;

	return p;
}
