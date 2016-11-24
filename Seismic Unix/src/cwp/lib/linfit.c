/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "cwp.h"

/* prototypes of subroutines used internally */
float gammln(float xx);
float gammq(float a, float x);
void gcf(float *gammcf, float a, float x, float *gln);
void gser(float *gamser, float a, float x, float *gln);

void linfit( float *x, float *y, int ndata, float *sig, int mwt,
          float *a, float *b, float *siga, float *sigb,
	  float *chi2, float *q)
/********************************************************************** 
 linfit - Given a set of data points x and y with standard 
          deviations sig, fit a straight y = a +bx by minimizing the missfit.
********************************************************************** 
Input:
x	x coordinate array
y	y coordinate array
sig	individual errors (weights )
mwt     =0  error( weigth) not available

Returned:
a
b
siga	probabability of a 
sigb	probabaility of b
chi2	the chi-squares
q	goodness of the fir
********************************************************************** 
Author: Balasz Nemeth (inspired by similar Numerical Recipes code fit.c)
**********************************************************************/
{	float gammq(float a, float x);
	int i;
	float wt,t,sxoss,sx=0.0,sy=0.0,st2=0.0,ss,sigdat;

	*b=0.0;
	if (mwt) {
		ss=0.0;
		for (i=0;i<ndata;i++) {
			wt=1.0/sig[i]*sig[i];
			ss += wt;
			sx += x[i]*wt;
			sy += y[i]*wt;
		}
	} else {
		for (i=0;i<ndata;i++) {
			sx += x[i];
			sy += y[i];
		}
		ss=ndata;
	}
	sxoss=sx/ss;
	if (mwt) {
		for (i=0;i<ndata;i++) {
			t=(x[i]-sxoss)/sig[i];
			st2 += t*t;
			*b += t*y[i]/sig[i];
		}
	} else {
		for (i=0;i<ndata;i++) {
			t=x[i]-sxoss;
			st2 += t*t;
			*b += t*y[i];
		}
	}
	*b /= st2;
	*a=(sy-sx*(*b))/ss;
	*siga=sqrt((1.0+sx*sx/(ss*st2))/ss);
	*sigb=sqrt(1.0/st2);
	*chi2=0.0;
	if (mwt == 0) {
		for (i=0;i<ndata;i++)
			*chi2 += (y[i]-(*a)-(*b)*x[i])*(y[i]-(*a)-(*b)*x[i]);
		*q=1.0;
		sigdat=sqrt((*chi2)/(ndata-2));
		*siga *= sigdat;
		*sigb *= sigdat;
	} else {
		for (i=0;i<ndata;i++)
			*chi2 += ((y[i]-(*a)-(*b)*x[i])/sig[i])*((y[i]-(*a)-(*b)*x[i])/sig[i]);
		*q=gammq(0.5*(ndata-2),0.5*(*chi2));
	}
}

float gammq(float a, float x)
/******************************************************************
gammq - calculate the probability that the chi^2 should exceed
       a particular input value of chi^2 by chance
*******************************************************************
Author: Balasz Nemeth (inspired by similar Numerical Recipes code)
*******************************************************************/
{
	void gcf(float *gammcf, float a, float x, float *gln);
	void gser(float *gamser, float a, float x, float *gln);
	float gamser,gammcf,gln;

	if (x < 0.0 || a <= 0.0) {
		fprintf(stderr,"Invalid arguments in routine gammq\n");
		exit(EXIT_FAILURE);
	}
	if (x < (a+1.0)) {
		gser(&gamser,a,x,&gln);
		return 1.0-gamser;
	} else {
		gcf(&gammcf,a,x,&gln);
		return gammcf;
	}
}

#include <math.h>
#define ITMAX 100
#define EPS 3.0e-7
#define FPMIN 1.0e-30

void gcf(float *gammcf, float a, float x, float *gln)
/******************************************************************
gcf - calculate the probability that the chi^2 should exceed
       a particular value of chi^2 using continued fractions
******************************************************************
Author: Balasz Nemeth (inspired by similar Numerical Recipes code)
******************************************************************/
{
	float gammln(float xx);
	int i;
	float an,b,c,d,del,h;

	*gln=gammln(a);
	b=x+1.0-a;
	c=1.0/FPMIN;
	d=1.0/b;
	h=d;
	for (i=1;i<=ITMAX;i++) {
		an = -i*(i-a);
		b += 2.0;
		d=an*d+b;
		if (fabsf(d) < FPMIN) d=FPMIN;
		c=b+an/c;
		if (fabsf(c) < FPMIN) c=FPMIN;
		d=1.0/d;
		del=d*c;
		h *= del;
		if (fabsf(del-1.0) < EPS) break;
	}
	if (i > ITMAX) {
		fprintf(stderr,"a too large, ITMAX too small in gcf\n");
		exit(EXIT_FAILURE);
	}
	*gammcf=exp(-x+a*log(x)-(*gln))*h;
}
#include <math.h>
#define ITMAX 100
#define EPS 3.0e-7

void gser(float *gamser, float a, float x, float *gln)
/******************************************************************
gser - calculate the probability that the chi^2 should exceed
       a particular value of chi^2 using a series approximation
******************************************************************
Author: Balasz Nemeth (inspired by similar Numerical Recipes code)
******************************************************************/
{
	float gammln(float xx);
	int n;
	float sum,del,ap;

	*gln=gammln(a);
	if (x <= 0.0) {
		if (x < 0.0) {
			fprintf(stderr,"x less than 0 in routine gser\n");
			exit(EXIT_FAILURE);
		}
		*gamser=0.0;
		return;
	} else {
		ap=a;
		del=sum=1.0/a;
		for (n=1;n<=ITMAX;n++) {
			++ap;
			del *= x/ap;
			sum += del;
			if (fabsf(del) < fabsf(sum)*EPS) {
				*gamser=sum*exp(-x+a*log(x)-(*gln));
				return;
			}
		}
		fprintf(stderr,"a too large, ITMAX too small in routine gser\n");

		
		return;
	}
}
#undef ITMAX
#undef EPS
#include <math.h>

float gammln(float xx)
/******************************************************************
gammln - natural logarithm of the gamma function for small positive
         arguments
******************************************************************
Author: Balasz Nemeth (inspired by similar Numerical Recipes code)
*******************************************************************/
{
	double x,y,tmp,ser;
	static double cof[6]={76.18009172947146,-86.50532032941677,
				24.01409824083091,-1.231739572450155,
		0.1208650973866179e-2,-0.5395239384953e-5};
	int j;

	y=x=xx;
	tmp=x+5.5;
	tmp -= (x+0.5)*log(tmp);
	ser=1.000000000190015;
	for (j=0;j<=5;j++) ser += cof[j]/++y;
	return -tmp+log(2.5066282746310005*ser/x);
}

