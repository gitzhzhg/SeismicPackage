/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
HANKEL - Functions to compute discrete Hankel transforms

hankelalloc	allocate and return a pointer to a Hankel transformer
hankelfree 	free a Hankel transformer
hankel0		compute the zeroth-order Hankel transform
hankel1		compute the first-order Hankel transform

******************************************************************************
Function Prototypes:
void *hankelalloc (int nfft);
void hankelfree (void *ht);
void hankel0 (void *ht, float f[], float h[]);
void hankel1 (void *ht, float f[], float h[]);

******************************************************************************
hankelalloc:
Input:
nfft		valid length for real to complex fft  (see notes below)

Returned:
pointer to Hankel transformer

******************************************************************************
hankelfree:
Input:
ht		pointer to Hankel transformer (as returned by hankelalloc)

******************************************************************************
hankel0:
Input:
ht		pointer to Hankel transformer (as returned by hankelalloc)
f		array[nfft/2+1] to be transformed

Output:
h		array[nfft/2+1] transformed

******************************************************************************
hankel1:
Input:
ht		pointer to Hankel transformer (as returned by hankelalloc)
f		array[nfft/2+1] to be transformed

Output:
h		array[nfft/2+1] transformed

******************************************************************************
Notes:
The zeroth-order Hankel transform is defined by:

	        Infinity
	h0(k) = Integral dr r j0(k*r) f(r)
		   0

where j0 denotes the zeroth-order Bessel function.

The first-order Hankel transform is defined by:

	        Infinity
	h1(k) = Integral dr r j1(k*r) f(r)
		   0

where j1 denotes the first-order Bessel function.

The Hankel transform is its own inverse.

The Hankel transform is computed by an Abel transform followed by
a Fourier transform.

******************************************************************************
References:
Hansen, E. W., 1985, Fast Hankel transform algorithm:  IEEE Trans. on
Acoustics, Speech and Signal Processing, v. ASSP-33, n. 3, p. 666-671.
(Beware of several errors in the equations in this paper!)

******************************************************************************
Authors:  Dave Hale, Colorado School of Mines, 06/04/90
******************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

/* Hankel transformer state (only used internally) */
typedef struct hankeltStruct {
	int nfft;
	float *gx;
	complex *gk;
	void *at;
} hankelt;

void *hankelalloc (int nfft)
/*****************************************************************************
allocate and return a pointer to a Hankel transformer
******************************************************************************
Input:
nfft		valid length for real to complex fft  (see notes below)

Returned:
		pointer to Hankel transformer
******************************************************************************
Notes:
nfft should be determined via either the npfar() or npfaro() functions.

The number of samples input/output to/from the Hankel transformer is
nfft/2+1.  The Hankel transformer makes a copy of the input nfft/2+1
samples, performs the Abel and Fourier transforms, and finally outputs 
nfft/2+1 samples.
******************************************************************************
Authors:  Dave Hale, Colorado School of Mines, 06/04/90
******************************************************************************/
{
	hankelt *ht;
	
	/* allocate space for temporary arrays and Abel transformer */
	ht = (hankelt *)malloc(sizeof(hankelt));
	ht->nfft = nfft;
	ht->gx = alloc1float(nfft);
	ht->gk = alloc1complex(nfft/2+1);
	ht->at = abelalloc(nfft/2+1);
	return ht;
}


void hankelfree (void *ht)
/*****************************************************************************
free a Hankel transformer
******************************************************************************
Input:
ht		pointer to Hankel transformer (as returned by hankelalloc)
******************************************************************************
Authors:  Dave Hale, Colorado School of Mines, 06/04/90
******************************************************************************/
{
	free1float(((hankelt*)ht)->gx);
	free1complex(((hankelt*)ht)->gk);
	abelfree(((hankelt*)ht)->at);
	free(ht);
}


void hankel0 (void *ht, float f[], float h[])
/*****************************************************************************
compute zero-order Hankel transform
******************************************************************************
Input:
ht		pointer to Hankel transformer (as returned by hankelalloc)
f		array[nfft/2+1] to be transformed

Output:
h		array[nfft/2+1] transformed
******************************************************************************
Authors:  Dave Hale, Colorado School of Mines, 06/04/90
******************************************************************************/
{
	int nfft,n,i;
	float scale,*gx;
	complex *gk;
	void *at;
	
	/* get state variables */
	nfft = ((hankelt*)ht)->nfft;
	gx = ((hankelt*)ht)->gx;
	gk = ((hankelt*)ht)->gk;
	at = ((hankelt*)ht)->at;
	
	/* determine number of input and output samples */
	n = nfft/2+1;
	
	/* g(x) = Abel transform of f(r) */
	abel(at,f,gx);
	
	/* make g(x) even */
	for (i=n; i<nfft; ++i)
		gx[i] = gx[nfft-i];
	
	/* g(k) = Fourier transform of g(x) */
	pfarc(-1,nfft,gx,gk);
	
	/* h(k) = scaled real part of g(k) */
	scale = 1.0/(2.0*PI);
	for (i=0; i<n; ++i)
		h[i] = scale*gk[i].r;
}


void hankel1 (void *ht, float f[], float h[])
/*****************************************************************************
compute first-order Hankel transform
******************************************************************************
Input:
ht		pointer to Hankel transformer (as returned by hankelalloc)
f		array[nfft/2+1] to be transformed

Output:
h		array[nfft/2+1] transformed
******************************************************************************
Authors:  Dave Hale, Colorado School of Mines, 06/04/90
******************************************************************************/
{
	int nfft,n,i;
	float scale,*gx;
	complex *gk;
	void *at;
	
	/* get state variables */
	nfft = ((hankelt*)ht)->nfft;
	gx = ((hankelt*)ht)->gx;
	gk = ((hankelt*)ht)->gk;
	at = ((hankelt*)ht)->at;
	
	/* determine number of input and output samples */
	n = nfft/2+1;
	
	/* g(x) = x * [Abel transform of f(r)/r] */
	for (i=1; i<n; ++i)
		gx[i] = f[i]/i;
	abel(at,gx,gx);
	for (i=0; i<n; ++i)
		gx[i] *= i;
	
	/* make g(x) odd */
	for (i=n; i<nfft; ++i)
		gx[i] = -gx[nfft-i];
	gx[nfft/2] = 0.0;
	
	/* g(k) = Fourier transform of g(x) */
	pfarc(-1,nfft,gx,gk);
	
	/* h(k) = scaled imaginary part of g(k) */
	scale = -1.0/(2.0*PI);
	for (i=0; i<n; ++i)
		h[i] = scale*gk[i].i;
}


#ifdef TEST
/* compute Hankel transform of a jinc function (Bracewell, p. 249) */
#define N 512
#define RMAX 1
main()
{
	int i,n=N,nfft,nr,nk;
	float rmax=RMAX;
	float *f,*h,*e,*g,a,r,k,dr,dk;
	void *ht;

	a = 20.0*2.0*PI;
	nfft = npfar(2*(n-1));
	n = nfft/2+1;	
	nr = n;
	dr = rmax/(nr-1);
	nk = n;
	dk = 2.0*PI/(nfft*dr);
	f = alloc1float(nr);
	g = alloc1float(nr);
	h = alloc1float(nk);
	e = alloc1float(nk);
	fprintf(stderr,"nfft=%d  nr=%d  dr=%g  nk=%d  dk=%g\n",
		nfft,nr,dr,nk,dk);
	
	for (i=0,r=0.0; i<nr; ++i,r+=dr) {
		f[i] = (r==0.0 ? a*a/2.0 :a*j1(a*r)/r);
		/* f[i] *= 0.54+0.46*cos(PI*i/(nr-1)); */
	}		
	ht = hankelalloc(nfft);
	hankel0(ht,f,h);
	for (i=0,k=0.0; i<nk; ++i,k+=dk) {
		h[i] *= dr*dr;
		e[i] = (k<=a ? 1.0 : 0.0);
	}
	hankel0(ht,h,g);
	for (i=0; i<nr; ++i) {
		g[i] *= dk*dk;
		f[i] *= 2.0/(a*a);
		g[i] *= 2.0/(a*a);
	}
	hankelfree(ht);
	fwrite(f,sizeof(float),nr,stdout);
	fwrite(g,sizeof(float),nr,stdout);
	fwrite(h,sizeof(float),nk,stdout);
	fwrite(e,sizeof(float),nk,stdout);
}
#endif
