/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
ABEL - Functions to compute the discrete ABEL transform:

abelalloc	allocate and return a pointer to an Abel transformer
abelfree 	free an Abel transformer
abel		compute the Abel transform

******************************************************************************
Function prototypes:
void *abelalloc (int n);
void abelfree (void *at);
void abel (void *at, float f[], float g[]);

******************************************************************************
Input:
ns		number of samples in the data to be transformed
f[]		array of floats, the function being transformed

Output:
at		pointer to Abel transformer returned by abelalloc(int n)
g[]		array of floats, the transformed data returned by 
		abel(*at,f[],g[])

******************************************************************************
Notes:
The Abel transform is defined by:

	         Infinity
	g(y) = 2 Integral dx f(x)/sqrt(1-(y/x)^2)
		   |y|

Linear interpolation is used to define the continuous function f(x)
corresponding to the samples in f[].  The first sample f[0] corresponds
to f(x=0) and the sampling interval is assumed to be 1.  Therefore, the
input samples correspond to 0 <= x <= n-1.  Samples of f(x) for x > n-1
are assumed to be zero.  These conventions imply that 

	g[0] = f[0] + 2*f[1] + 2*f[2] + ... + 2*f[n-1]

******************************************************************************
References:
Hansen, E. W., 1985, Fast Hankel transform algorithm:  IEEE Trans. on
Acoustics, Speech and Signal Processing, v. ASSP-33, n. 3, p. 666-671.
(Beware of several errors in the equations in this paper!)

******************************************************************************
Authors:  Dave Hale and Lydia Deng, Colorado School of Mines, 06/01/90
******************************************************************************/
/**************** end self doc ********************************/
#include "cwp.h"

/* Abel transformer state (only used internally) */
#define NSE 9
typedef struct abeltStruct {
	int n;
	float **a,**b0,**b1;
} abelt;

void *abelalloc (int n)
/*****************************************************************************
allocate and return a pointer to an Abel transformer
******************************************************************************
Input:
n		number of samples to transform

Returned:
		pointer to Abel transformer
******************************************************************************
Authors:  Dave Hale and Lydia Deng, Colorado School of Mines, 06/01/90
******************************************************************************/
{
	static float h[NSE] = {
		1.000000000000000000,
		0.610926299405048390,
		0.895089852938535935,
		1.34082948787002865,
		2.02532848558443890,
		3.18110895533701843,
		5.90898360396353794,
		77.6000213494180286,
		528.221800846070892,
	};    
	static float lambda[NSE] = {
		0.000000000000000000,
		-2.08424632126539366,
		-5.78928630565552371,
		-14.6268676854951032,
		-35.0617158334443104,
		-83.3258406398958158,
		-210.358805421311445,
		-6673.64911325382036,
		-34897.7050244132261,
	};
	int i,j,nse=NSE;
	float **a,**b0,**b1,fi,hj,lambdaj,scale,temp;
	abelt *at;
	
	/* allocate space for and pre-compute the transformer arrays */
	a = alloc2float(nse,n);
	b0 = alloc2float(nse,n);
	b1 = alloc2float(nse,n);
	for (i=1; i<n; ++i) {
		fi = (float)i+1.0;
		for (j=0; j<nse; ++j) {
			hj = h[j];
			lambdaj = lambda[j];
			a[i][j] = temp = pow(fi/(fi-1.0),lambdaj);
			temp *= fi/(fi-1.0);
			scale = 2.0*hj*(fi-1.0) /
				((lambdaj+1.0)*(lambdaj+2.0));				
			b0[i][j] = scale *
				(fi-1.0+(lambdaj+2.0-fi)*temp);
			b1[i][j] = -scale *
				(lambdaj+1.0+fi-fi*temp);
		}
	}
	
	/* save state variables and return pointer to transformer */
	at = (abelt *)malloc(sizeof(abelt));
	at->n = n;
	at->a = a;
	at->b0 = b0;
	at->b1 = b1;
	return at;
}


void abelfree (void *at)
/*****************************************************************************
free an Abel transformer
******************************************************************************
Input:
at		pointer to Abel transformer (as returned by abelalloc)
******************************************************************************
Authors:  Dave Hale and Lydia Deng, Colorado School of Mines, 06/01/90
******************************************************************************/
{
	free2float(((abelt*)at)->a);
	free2float(((abelt*)at)->b0);
	free2float(((abelt*)at)->b1);
	free(at);
}


void abel (void *at, float f[], float g[])
/*****************************************************************************
compute Abel transform
******************************************************************************
Input:
at		pointer to Abel transformer (as returned by abelalloc)
f		array[n] to be transformed (may be equivalenced to g)

Output:
g		array[n] transformed (may be equivalenced to f)
******************************************************************************
Authors:  Dave Hale and Lydia Deng, Colorado School of Mines, 06/01/90
******************************************************************************/
{
	int i,j,n,nse=NSE;
	float **a,**b0,**b1,xi[NSE],sum,fi,fip1;
	
	/* get state variables */
	n = ((abelt*)at)->n;
	a = ((abelt*)at)->a;
	b0 = ((abelt*)at)->b0;
	b1 = ((abelt*)at)->b1;
	
	/* do the transform */
	fi = f[n-1];
	g[0] = 0.5*f[0]+fi;
	for (j=0,sum=0.0; j<nse; ++j) {
		xi[j] = b1[n-1][j]*fi;
		sum += xi[j];
	}
	g[n-1] = sum;
	for (i=n-2; i>0; --i) {
		fip1 = fi;
		fi = f[i];
		g[0] += fi;
		for (j=0,sum=0.0; j<nse; ++j) {
			xi[j] = a[i][j]*xi[j] +
				b0[i][j]*fip1 +
				b1[i][j]*fi;
			sum += xi[j];
		}
		g[i] = sum;
	}
	g[0] *= 2.0;
}


#ifdef TEST
/* compute Abel transform of a cone function (Bracewell, p. 264) */
#define N 100
main()
{
	int i,n=N;
	float f[N],g[N],e[N],a,r,k,dr,dk;
	void *at;
		
	a = 1.0;
	dr = dk = 1.0/n;
	
	for (i=0,r=0.0; i<n; ++i,r+=dr) {
		f[i] = a-r;
	}		
	at = abelalloc(n);
	abel(at,f,g);
	for (i=0,k=0.0; i<n; ++i,k+=dk) {
		g[i] *= dr;
		e[i] = (k!=0.0 ? a*sqrt(a*a-k*k)-k*k*acosh(a/k) : a*a);
	}
	abelfree(at);
	fwrite(g,sizeof(float),n,stdout);
	fwrite(e,sizeof(float),n,stdout);
}
#endif
