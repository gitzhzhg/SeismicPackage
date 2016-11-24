/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*********************************************************************

MINFUNC - routines to MINimize FUNCtions

bracket_minimum - bracket the minimum of a function with a golden search
golden_bracket - bracket the minimum of a function to a user-specified
		 tolerance using the golden search
brent_bracket - bracket the minimum of a function using Brent's method
powell_minimization - find the minimum of a multi-dimensional function
			using Powell's method
********************************************************************/

/**************** end self doc ********************************/
#include "par.h"


/* Definitions used internally */

#define PFMAGFAC 100.0		/* parabolic fit magnification factor */
#define BRACKET_SIGN(a,b) ((b) > 0.0 ? fabs(a) : -fabs(a))

void bracket_minimum(float *ax, float *bx, float *cx, float *fa,
		float *fb,float *fc, float (*func)(float))
/*********************************************************************
bracket_minimum - bracket the minimum of a function
**********************************************************************
Input:
*ax	pointer to input x value
*bx	pointer to second input x value
func	input function

Output:
*ax	pointer to new x value
*bx	pointer to new x value
*cx	pointer to new x value
*fa	pointer to value of function at ax
*fb	pointer to value of function at bx
*fc	pointer to value of function at cx
*********************************************************************
Notes:
The output brackets the minimum using the Golden search algorithm.

Inspired by codes in Netlib and the text in Press, et all. 
Numerical Recipes in C.
*********************************************************************
Credits: Based on a similar routine in Numerical Recipes in C.
         modified for SU, CWP:Gabriel Alvarez, March 2000.
*********************************************************************/
{

	float fu;
	float gratio = GOLDEN_RATIO;

	*fa=(*func)(*ax);
	*fb=(*func)(*bx);

	if (*fb > *fa) {
		float tmp;

		tmp = *ax;
		*ax = *bx;
		*bx = tmp;

		tmp = *fb;
		*fb = *fa;
		*fa = tmp;
	}
	*cx=(*bx)+gratio*(*bx-*ax);
	*fc=(*func)(*cx);

	while (*fb > *fc) {
		float r=(*bx-*ax)*(*fb-*fc);
		float q=(*bx-*cx)*(*fb-*fa);
		float u=(*bx)-((*bx-*cx)*q-(*bx-*ax)*r)/
			(2.0*BRACKET_SIGN(MAX(fabs(q-r),FLT_EPSILON),q-r));
		float ulim=(*bx)+PFMAGFAC*(*cx-*bx);
		if ((*bx-u)*(u-*cx) > 0.0) {
			fu=(*func)(u);
			if (fu < *fc) {
				*ax=(*bx);
				*bx=u;
				*fa=(*fb);
				*fb=fu;
				return;
			} else if (fu > *fb) {
				*cx=u;
				*fc=fu;
				return;
			}
			u=(*cx)+gratio*(*cx-*bx);
			fu=(*func)(u);
		} else if ((*cx-u)*(u-ulim) > 0.0) {
			fu=(*func)(u);
			if (fu < *fc) {

				*bx = *cx;
				*cx = u;
				u = *cx + gratio*(*cx - *bx);
				
				*fb = *fc;
				*fc = fu;
				fu = (*func)(u);
			}
		} else if ((u-ulim)*(ulim-*cx) >= 0.0) {
			u=ulim;
			fu=(*func)(u);
		} else {
			u=(*cx)+gratio*(*cx-*bx);
			fu=(*func)(u);
		}

		*ax = *bx;
		*bx = *cx;
		*cx = u;	

		*fa = *fb;
		*fb = *fc;
		*fc = fu;
	}
}

float golden_bracket(float ax, float bx, float cx,
			float (*f)(float), float tol,float *xmin)
/*************************************************************************
golden_bracket - bracket the minimum of a function to a user-specified
		 tolerance using the golden search
**************************************************************************
Input:
ax	first x input
bx	second x input
cx	third x input	
f	function
tol	tolerance describing accuracy of minimum location

Output: (within tolerance given by tol)
xmin		abscissa of minimum

Returns:  (within tolerance given by tol)
f(xmin)		value of f at xmin

**************************************************************************
Notes:
Given a bracketing triplet of abcissas, such that  ax < bx < cx
or cx < bx < ax, and that  f(ax) < f(bx) < f(cx), or f(cx) < f(bx) < f(x)


**************************************************************************
Credits: 
Inspired by codes in Netlib and the text of  Press, et al., 
Numerical Recipes in C.
	Adapted for CWP/SU CWP: Gabriel Alvarez, March 2000
*************************************************************************/
{
	float f1,f2,x0,x1,x2,x3;
	float gratio = GOLDEN_RATIO - 1.0;
	float cgratio = 1.0 - ( GOLDEN_RATIO - 1.0 );

	x0=ax;
	x3=cx;
	if (fabs(cx-bx) > fabs(bx-ax)) {
		x1=bx;
		x2=bx+cgratio*(cx-bx);
	} else {
		x2=bx;
		x1=bx-cgratio*(bx-ax);
	}
	f1=(*f)(x1);
	f2=(*f)(x2);
	while (fabs(x3-x0) > tol*(fabs(x1)+fabs(x2))) {
		if (f2 < f1) {

			x0=x1;
			x1=x2;
			x2=gratio*x1+cgratio*x3;

			f1 = f2;
			f2 = (*f)(x2);
		} else {

			x3=x2;
			x2=x1;
			x1=gratio*x2 + cgratio*x0;

			f2 = f1;
			f1 = (*f)(x1);
		}
	}
	if (f1 < f2) {
		*xmin=x1;
		return f1;
	} else {
		*xmin=x2;
		return f2;
	}
}

#define BRENT_MAXITER 100

float brent_bracket(float ax, float bx, float cx,
		float (*f)(float), float tol, float *xmin)
/**************************************************************************

***************************************************************************
***************************************************************************
Credits: Press, et. al, Numerical Recipes in C
**************************************************************************/
{
	int iter;
	float a,b;
	float x,u,v,w;
	float fw,fv,fx,fu;
	int maxiter = BRENT_MAXITER;
	float cgratio = ( 1.0 - (GOLDEN_RATIO - 1.0));
	float e=0.0;

	a=((ax < cx) ? ax : cx);
	b=((ax > cx) ? ax : cx);

	x=w=v=bx;
	fw=fv=fx=(*f)(x);

	for (iter=0;iter<maxiter;iter++) {
		float d=0.0,etemp;
		float p,q,r,tol1,tol2;
		float xm;

		xm=0.5*(a+b);
		tol2=2.0*(tol1=tol*fabs(x)+FLT_EPSILON);
		if (fabs(x-xm) <= (tol2-0.5*(b-a))) {
			*xmin=x;
			return fx;
		}
		if (fabs(e) > tol1) {
			r=(x-w)*(fx-fv);
			q=(x-v)*(fx-fw);
			p=(x-v)*q-(x-w)*r;
			q=2.0*(q-r);
			if (q > 0.0) p = -p;
			q=fabs(q);
			etemp=e;
			e=d;
			if (fabs(p) >= fabs(0.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x))
				d=cgratio*(e=(x >= xm ? a-x : b-x));
			else {
				d=p/q;
				u=x+d;
				if (u-a < tol2 || b-u < tol2)
					d=BRACKET_SIGN(tol1,xm-x);
			}
		} else {
			d=cgratio*(e=(x >= xm ? a-x : b-x));
		}
		u=(fabs(d) >= tol1 ? x+d : x+BRACKET_SIGN(tol1,d));
		fu=(*f)(u);
		if (fu <= fx) {
			if (u >= x) a=x; else b=x;

			v = w;
			w = x;
			x = u;

			fv = fw;
			fw = fx;
			fx = fu;

		} else {
			if (u < x) a=u; else b=u;
			if (fu <= fw || w == x) {
				v=w;
				w=u;
				fv=fw;
				fw=fu;
			} else if (fu <= fv || v == x || v == w) {
				v=u;
				fv=fu;
			}
		}
	}
	fprintf(stderr,"Too many iterations in BRENT\n");
	*xmin=x;
	return fx;
}

extern int ncom;	/* defined in LINMIN */
extern float *pcom,*xicom,(*ffunc)();

float oneDfunction(float x);

#define LINMIN_TOL 2.0e-4

int ncom=0;	/* defining declarations */
float *pcom=NULL,*xicom=NULL,(*ffunc)();

void linmin(float p[],float xi[],int n,float *fret, float (*func)())
/********************************************************************
linmin - minmize a multi-dimensional function along a specified line.
*********************************************************************
Notes: based on similar routine from Numerical Recipes
*********************************************************************
Credits: Press, et. al, Numerical Recipes in C
********************************************************************/
{
	int j;
	float xx,xmin,fx,fb,fa,bx,ax;

	ncom=n;
	pcom=ealloc1float(n);
	xicom=ealloc1float(n);
	ffunc=func;
	for (j=0;j<n;j++) {
		pcom[j]=p[j];
		xicom[j]=xi[j];
	}
	ax=0.0;
	xx=1.0;
	bracket_minimum(&ax,&xx,&bx,&fa,&fx,&fb,oneDfunction);
	*fret=brent_bracket(ax,xx,bx,oneDfunction,LINMIN_TOL,&xmin);
	for (j=0;j<n;j++) {
		xi[j] *= xmin;
		p[j] += xi[j];
	}
	free1float(xicom);
	free1float(pcom);
}


float oneDfunction(float x)
{
	int j;
	float f,*xt;

	xt=ealloc1float(ncom);
	for (j=0;j<ncom;j++) xt[j]=pcom[j]+x*xicom[j];
	f=(*ffunc)(xt);
	free1float(xt);
	return f;
}

#define PITMAX (200)

void powell_minimization(float p[], float **xi, int n,
		float ftol,int *iter,float *fret,float (*func)())
/*********************************************************************
powell_minimization - find the minimum of a multi-dimensional function
			using Powell's method
**********************************************************************
Notes: based on a similar routine in Numerical Recipes in C.
**********************************************************************
Credits: Press, et al., Numerical Recipies in C.
Adapted for the CWP/SU package by CWP: Gabriel Alvarez, March 2000
*********************************************************************/
{
	int i,ibig,j;
	float t,fptt,fp,del;
	float *pt=NULL,*ptt=NULL,*xit=NULL;
	int maxiter = PITMAX;

	pt=ealloc1float(n);
	ptt=ealloc1float(n);
	xit=ealloc1float(n);

	*fret=(*func)(p);

	for (j=0;j<n;++j) pt[j]=p[j];

	for (*iter=1;;++(*iter)) {
		fp=(*fret);
		ibig=0;
		del=0.0;
		for (i=0;i<n;++i) {
			for (j=0;j<n;++j) xit[j]=xi[j][i];
			fptt=(*fret);

			linmin(p,xit,n,fret,func);

			if (fabs(fptt-(*fret)) > del) {
				del=fabs(fptt-(*fret));
				ibig=i;
			}
		}
		if (2.0*fabs(fp-(*fret)) <= ftol*(fabs(fp)+fabs(*fret))) {

			free1float(xit);
			free1float(ptt);
			free1float(pt);

			return;
		}
		if (*iter == maxiter)
			fprintf(stderr,"Too many iterations in routine POWELL\n");
		for (j=0;j<n;++j) {
			ptt[j]=2.0*p[j]-pt[j];
			xit[j]=p[j]-pt[j];
			pt[j]=p[j];
		}
		fptt=(*func)(ptt);
		if (fptt < fp) {
			t=2.0*(fp-2.0*(*fret)+fptt)
				*(fp-(*fret)-del)*(fp-(*fret)-del) 
				- del*(fp-fptt)*(fp-fptt);
;
			if (t < 0.0) {
				linmin(p,xit,n,fret,func);
				for (j=0;j<n;++j) xi[j][ibig]=xit[j];
			}
		}
	}
}
