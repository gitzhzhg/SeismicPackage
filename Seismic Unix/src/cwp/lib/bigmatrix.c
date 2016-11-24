/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
BIGMATRIX - Functions to manipulate 2-dimensional matrices that are too big 
	    to fit in real memory, but that are small enough to fit in
		virtual memory:

bmalloc		allocate a big matrix
bmfree		free a big matrix 
bmread		read a vector from a big matrix
bmwrite		write a vector to a big matrix

******************************************************************************
Function Prototypes:
void *bmalloc (int nbpe, int n1, int n2);
void bmfree (void *bm);
void bmread (void *bm, int dir, int k1, int k2, int n, void *v);
void bmwrite (void *bm, int dir, int k1, int k2, int n, void *v);

******************************************************************************
bmalloc:
Input:
nbpe		number of bytes per matrix element
n1		number of elements in 1st (fastest) dimension
n2		number of elements in 2nd (slowest) dimension

Returned:
bm		pointer to big matrix

bmfree:
Input:
bm		pointer to big matrix state (returned by bmalloc)

bmread:
Input:
bm    		pointer to big matrix state (returned by bmalloc)
d  		= 1 or 2:  direction in which to read matrix elements
k1		1st dimension index of first matrix element to read
k2		2nd dimension index of first matrix element to read
n		number of elements to read

Output:
v		array[n] to contain matrix elements read

bmwrite:
Input:
bm    		pointer to big matrix state (returned by bmalloc)
d  		= 1 or 2:  direction in which to write matrix elements
k1		1st dimension index of first matrix element to write
k2		2nd dimension index of first matrix element to write
n		number of elements to write
v		array[n] containing matrix elements to write

******************************************************************************
Notes:
The bm functions provide access to a big 2-dimensional matrix along
either the 1st or 2nd dimensions.  Although, the matrix must be small
enough to fit in virtual memory, it may be too large to fit in real memory.
These functions provide equally efficient (or equally inefficient) access 
to vectors in a big matrix along either the 1st or 2nd dimensions.

For example, the following algorithm will efficiently transpose an
n1 by n2 array of (n1*n2) floats stored in a file:

	void *bm;
	float *v;
	bm = bmalloc(sizeof(float),n1,n2);
	for (i2=0; i2<n2; i2++) {
		(read n1 floats from input file into array v);
		bmwrite(bm,1,0,i2,n1,(char*)v);
	}
	for (i1=0; i1<n1; i1++) {
		bmread(bm,2,i1,0,n2,(char*)v);
		(write n2 floats in array v to output file);
	}
	bmfree(bm);

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/17/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

/* good size for LPAGE is a multiple of the virtual memory pagesize */
#define LPAGE 1024

/* big matrix state defined for internal use only */
typedef struct _bmstateStruct {
	int l1,l2,m1,m2,nbpe;
	char *p;
} bmstate;

/* functions declared and used internally */
static void bm_index (bmstate *s, int d, int k1, int k2, 
	int *nbpe, char **q, int *qstep, char **p, int *pstep,
	int *i, int *l);


void *
bmalloc (int nbpe, int n1, int n2)
/*****************************************************************************
allocate and return a pointer to a big matrix
******************************************************************************
Input:
nbpe		number of bytes per matrix element
n1		number of elements in 1st (fastest) dimension
n2		number of elements in 2nd (slowest) dimension

Returned:	pointer to big matrix
******************************************************************************
Notes:
Returns NULL if space for the big matrix cannot be allocated.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/17/89
*****************************************************************************/
{
	int l1,l2,l1t,l2t,wmin,w;
	bmstate *s;
		
	/* determine optimal page dimensions - try to make m1==m2 */
	l1 = 1;  l2 = LPAGE;  wmin = abs(l1*n2-l2*n1);
	for (l1t=2,l2t=LPAGE/2; l1t<=LPAGE; l1t*=2,l2t/=2) {
		w = abs(l1t*n2-l2t*n1);
		if (w<wmin) {
			wmin = w;
			l1 = l1t;
			l2 = l2t;
		}
	}

	/* allocate space for big matrix state */
	s = (bmstate *)malloc(sizeof *s);
	if (s==NULL) return NULL;

	/* page dimensions */
	s->l1 = l1;
	s->l2 = l2;

	/* number of pages required to span each matrix dimension */
	s->m1 = 1+(n1-1)/l1;
	s->m2 = 1+(n2-1)/l2;

	/* number of bytes per matrix element */
	s->nbpe = nbpe;

	/* allocate space for matrix */
	s->p = (char *)malloc(s->nbpe*s->l1*s->l2*s->m1*s->m2);
	if (s->p==NULL) {
		free((void*)s);
		return NULL;
	}

	/* return pointer to state */
	return s;
}

void
bmfree (void *bm)
/*****************************************************************************
free a big matrix
******************************************************************************
Input:
bm		pointer to big matrix state (returned by bmalloc)
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/17/89
*****************************************************************************/
{
	bmstate *s=bm;
	free(s->p);
	free(s);
}

void
bmread (void *bm, int d, int k1, int k2, int n, void *v)
/*****************************************************************************
read a vector from a big matrix
******************************************************************************
Input:
bm    		pointer to big matrix state (returned by bmalloc)
d  		= 1 or 2:  direction in which to read matrix elements
k1		1st dimension index of first matrix element to read
k2		2nd dimension index of first matrix element to read
n		number of elements to read

Output:
v		array[n] to contain matrix elements read
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/17/89
*****************************************************************************/
{
	bmstate *s=bm;
	int nbpe,qstep,pstep,k,i,l,iv;
	char *q,*p;
	char *qc,*pc,*vc;
	short *qs,*ps,*vs;
	long *ql,*pl,*vl;
	float *qf,*pf,*vf;
	double *qd,*pd,*vd;

	/* determine indices and steps */
	bm_index(s,d,k1,k2,&nbpe,&q,&qstep,&p,&pstep,&i,&l);

	/* move the matrix elements (optimize for elements of common sizes) */
	if (nbpe==sizeof(char)) {
		qc = (char*)q;
		pc = (char*)p;
		vc = (char*)v;
		for (k=0; k<n; k++) {
			*vc++ = *qc;
			i++;
			if (i<l) {
				qc += qstep;
			} else {
				i = 0;
				pc += pstep;
				qc = pc;
			}
		}
	} else if (nbpe==sizeof(short)) {
		pstep /= nbpe;
		qstep /= nbpe;
		qs = (short*)q;
		ps = (short*)p;
		vs = (short*)v;
		for (k=0; k<n; k++) {
			*vs++ = *qs;
			i++;
			if (i<l) {
				qs += qstep;
			} else {
				i = 0;
				ps += pstep;
				qs = ps;
			}
		}
	} else if (nbpe==sizeof(long)) {
		pstep /= nbpe;
		qstep /= nbpe;
		ql = (long*)q;
		pl = (long*)p;
		vl = (long*)v;
		for (k=0; k<n; k++) {
			*vl++ = *ql;
			i++;
			if (i<l) {
				ql += qstep;
			} else {
				i = 0;
				pl += pstep;
				ql = pl;
			}
		}
	} else if (nbpe==sizeof(float)) {
		pstep /= nbpe;
		qstep /= nbpe;
		qf = (float*)q;
		pf = (float*)p;
		vf = (float*)v;
		for (k=0; k<n; k++) {
			*vf++ = *qf;
			i++;
			if (i<l) {
				qf += qstep;
			} else {
				i = 0;
				pf += pstep;
				qf = pf;
			}
		}
	} else if (nbpe==sizeof(double)) {
		pstep /= nbpe;
		qstep /= nbpe;
		qd = (double*)q;
		pd = (double*)p;
		vd = (double*)v;
		for (k=0; k<n; k++) {
			*vd++ = *qd;
			i++;
			if (i<l) {
				qd += qstep;
			} else {
				i = 0;
				pd += pstep;
				qd = pd;
			}
		}
	} else {
		qc = (char*)q;
		pc = (char*)p;
		vc = (char*)v;
		for (k=0; k<n; k++) {
			for (iv=0; iv<nbpe; iv++)
				vc[iv] = qc[iv];
			vc += nbpe;
			i++;
			if (i<l) {
				qc += qstep;
			} else {
				i = 0;
				pc += pstep;
				qc = pc;
			}
		}
	}
}

void
bmwrite (void *bm, int d, int k1, int k2, int n, void *v)
/*****************************************************************************
write a vector to a big matrix
******************************************************************************
Input:
bm    		pointer to big matrix state (returned by bmalloc)
d  		= 1 or 2:  direction in which to write matrix elements
k1		1st dimension index of first matrix element to write
k2		2nd dimension index of first matrix element to write
n		number of elements to write
v		array[n] containing matrix elements to write
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/17/89
*****************************************************************************/
{
	bmstate *s=bm;
	int nbpe,qstep,pstep,k,i,l,iv;
	char *q,*p;
	char *qc,*pc,*vc;
	short *qs,*ps,*vs;
	long *ql,*pl,*vl;
	float *qf,*pf,*vf;
	double *qd,*pd,*vd;

	/* determine indices and steps */
	bm_index(s,d,k1,k2,&nbpe,&q,&qstep,&p,&pstep,&i,&l);

	/* move the matrix elements (optimize for elements of common sizes) */
	if (nbpe==sizeof(char)) {
		qc = (char*)q;
		pc = (char*)p;
		vc = (char*)v;
		for (k=0; k<n; k++) {
			*qc = *vc++;
			i++;
			if (i<l) {
				qc += qstep;
			} else {
				i = 0;
				pc += pstep;
				qc = pc;
			}
		}
	} else if (nbpe==sizeof(short)) {
		pstep /= nbpe;
		qstep /= nbpe;
		qs = (short*)q;
		ps = (short*)p;
		vs = (short*)v;
		for (k=0; k<n; k++) {
			*qs = *vs++;
			i++;
			if (i<l) {
				qs += qstep;
			} else {
				i = 0;
				ps += pstep;
				qs = ps;
			}
		}
	} else if (nbpe==sizeof(long)) {
		pstep /= nbpe;
		qstep /= nbpe;
		ql = (long*)q;
		pl = (long*)p;
		vl = (long*)v;
		for (k=0; k<n; k++) {
			*ql = *vl++;
			i++;
			if (i<l) {
				ql += qstep;
			} else {
				i = 0;
				pl += pstep;
				ql = pl;
			}
		}
	} else if (nbpe==sizeof(float)) {
		pstep /= nbpe;
		qstep /= nbpe;
		qf = (float*)q;
		pf = (float*)p;
		vf = (float*)v;
		for (k=0; k<n; k++) {
			*qf = *vf++;
			i++;
			if (i<l) {
				qf += qstep;
			} else {
				i = 0;
				pf += pstep;
				qf = pf;
			}
		}
	} else if (nbpe==sizeof(double)) {
		pstep /= nbpe;
		qstep /= nbpe;
		qd = (double*)q;
		pd = (double*)p;
		vd = (double*)v;
		for (k=0; k<n; k++) {
			*qd = *vd++;
			i++;
			if (i<l) {
				qd += qstep;
			} else {
				i = 0;
				pd += pstep;
				qd = pd;
			}
		}
	} else {
		qc = (char*)q;
		pc = (char*)p;
		vc = (char*)v;
		for (k=0; k<n; k++) {
			for (iv=0; iv<nbpe; iv++)
				qc[iv] = vc[iv];
			vc += nbpe;
			i++;
			if (i<l) {
				qc += qstep;
			} else {
				i = 0;
				pc += pstep;
				qc = pc;
			}
		}
	}
}

/* internal function used by bmread and bmwrite */
static void
bm_index (bmstate *s, int d, int k1, int k2, 
	int *nbpe, char **q, int *qstep, char **p, int *pstep, int *i, int *l)
{
	int i1,i2,j01,j2;
	int l1=s->l1, l2=s->l2, m1=s->m1, nb = s->nbpe;
	
	i1 = k1%l1;
	i2 = k2%l2;
	j01 = k1/l1;
	j2 = k2/l2;
	
	if (d==1) {
		*nbpe = nb;
		*p = s->p+nb*(i2*l1+j01*l1*l2+j2*l1*l2*m1);
		*pstep = nb*l1*l2;
		*qstep = nb;
		*q = *p+i1*(*qstep);
		*i = i1;
		*l = l1;
	} else {
		*nbpe = nb;
		*p = s->p+nb*(i1+j01*l1*l2+j2*l1*l2*m1);
		*pstep = nb*l1*l2*m1;
		*qstep = nb*l1;
		*q = *p+i2*(*qstep);
		*i = i2;
		*l = l2;
	}
}

/* function to dump big matrix state (useful for debugging) */
void
bmdump (void *bm)
{
	bmstate *s=bm;
	printf("big matrix state %lu:\n",(unsigned long int) s);
	printf("l1 = %d\n",s->l1);
	printf("l2 = %d\n",s->l2);
	printf("m1 = %d\n",s->m1);
	printf("m2 = %d\n",s->m2);
	printf("nbpe = %d\n",(int) s->nbpe);
	printf("p = %lu\n",(unsigned long int) s->p);
}
