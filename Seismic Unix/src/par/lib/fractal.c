/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "par.h"

/* function prototype for function used internally */
float distance(float x0, float y0, float x1, float y1);

float hausdorff_dimension( float *ar, int n,int minl,int maxl,int dl)
/*********************************************************************
hausdorff_dimension -  compute the fractal dimensions of an array
                        via the divider method
*********************************************************************
Input:
a	array
n	length 
minl	minimum lenght 
maxl	maximum length 
Returns:
the fractal dimension
*********************************************************************
Author:  Balazs Nemeth, Potash Corporation of Saskatoon Saskatchewan 
         code donated to CWP c. 2008.
*********************************************************************/
{
	int l,i1,i2,i;
	double sum;
	float *ll=NULL,*ls=NULL,*sig=NULL;
	float a,b,siga,sigb,chi2,q;
	int nm;
	
	nm = (maxl-minl)/dl;
	
	ll = ealloc1float(nm);
	ls = ealloc1float(nm);
	
	for(i=0,l=minl;i<nm;i++,l+=dl) {
		sum=0.0;
		i1=0;
		i2=l;	
		while(i2<n) {
			sum += distance((float)i1,ar[i1],(float)i2,ar[i2]);
			i1=i2;
			i2+=l;
		}
		ll[i] = log((float)l);
		ls[i] = log(sum);
	}
	linfit(ll,ls,nm,sig,0,&a,&b,&siga,&sigb,&chi2,&q);
	return(1.0-b);
}
   

float distance(float x0, float y0, float x1, float y1)
/***************************************************************** 
distance - return the distance between two points
*****************************************************************/
{
	return(sqrt((x1-x0)*(x1-x0)+(y1-y0)*(y1-y0)));
}
