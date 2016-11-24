/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "cwp.h"

/*********************** self documentation **********************/
/*****************************************************************************
LINEAR_REGRESSION - Compute linear regression of (y1,y2,...,yn) against 
(x1,x2,...,xn)
******************************************************************************
Function Prototype:
void linear_regression(float *y, float *x, int n, float coeff[4]);
******************************************************************************
Input:
y		array of y values
x		array of x values
n		length of y and x arrays
Output:
coeff[4] where:

coeff[0]	slope of best fit line
coeff[1]	intercept of best fit line
coeff[2]	correlation coefficient of fit (1 = perfect) [dimensionless]
coeff[3]	standard error of fit (0 = perfect) [dimensions of y]
******************************************************************************
Notes: 

y(x) 
    |      *  .    fit is  y(x) = a x + b
    |       .          
    |     .  *
    | * .    
    | . *         
     ------------------- x
     
         n Sum[x*y] - Sum[x]*Sum[y]
     a = --------------------------
         n Sum[x*x] - Sum[x]*Sum[x]
         
         Sum[y] - a*Sum[x]
     b = -----------------
                n
    
     cc = std definition
     
     se = std definition
    
******************************************************************************
Author:  Chris Liner, UTulsa, 11/16/03
******************************************************************************/
                
/**************** end self doc ********************************/


void linear_regression(float *y, float *x, int n, float coeff[4])
/*****************************************************************************
Compute linear regression of (y1,y2,...,yn) against (x1,x2,...,xn)
******************************************************************************
Input:
y		array of y values
x		array of x values
n		length of y and x arrays
Output:
coeff[4] where:

coeff[0]	slope of best fit line
coeff[1]	intercept of best fit line
coeff[2]	correlation coefficient of fit (1 = perfect) [dimensionless]
coeff[3]	standard error of fit (0 = perfect) [dimensions of y]
******************************************************************************
Notes: 

y(x) 
    |      *  .    fit is  y(x) = a x + b
    |       .          
    |     .  *
    | * .    
    | . *         
     ------------------- x
     
         n Sum[x*y] - Sum[x]*Sum[y]
     a = --------------------------
         n Sum[x*x] - Sum[x]*Sum[x]
         
         Sum[y] - a*Sum[x]
     b = -----------------
                n
                
     cc = std definition
     
     se = std definition
    
******************************************************************************
Author:  Chris Liner, UTulsa, 11/16/03
******************************************************************************/
{
        /* local variables */
	float den;	/* generic denomenator */
	float num;	/* generic numerator */
        float sx;	/* sum of x values */
        float sx2;	/* sum of x-squared values */
        float sy;	/* sum of y values */
        float sy2;	/* sum of y-squared values */
        float sxy;	/* sum of x*y values */
        float tmp;	/* temporary variable */
        int i;		/* counter */

	float a;	/* slope of best fit line */
	float b;	/* intercept of best fit line */
	float cc;	/* correlation coefficient of fit */
				/* (1 = perfect) [dimensionless] */
	float se;	/* standard error of fit (0 = perfect)  */
				/* [dimensions of y] */
        
        /* initialize sums */
        sx = x[0];
        sx2 = x[0]*x[0];
        sy = y[0];
        sy2 = y[0]*y[0];
        sxy = x[0]*y[0];
        
        /* calculate sums */
        for (i=1;i<n;++i) {
            sx += x[i]; 
            sx2 += x[i]*x[i];
            sy += y[i];
            sy2 += y[i]*y[i];
            sxy += x[i]*y[i];
        }
        
        /* slope */
        num = n*sxy - sx*sy;
        den = n*sx2 - sx*sx;
        a = num/den;

	coeff[0] = a;
        
        /* intercept */
        b = (sy - a*sx)/n;

	coeff[1] = b;
        
        /* correlation coefficient */
        num = n*sxy - sx*sy;
        den = (n*sx2 - sx*sx)*(n*sy2 - sy*sy);
        den = sqrt(den);

        if (den != 0.0) {
            cc = num/den;
        } else {
            cc = 999;
        }

	coeff[2] = cc;
        
        /* standard error */
        tmp = 0.0;
        for (i=0;i<n;++i) {
            tmp += (y[i] - (a*x[i] + b) )*(y[i] - (a*x[i] + b) ); 
        }

        se = sqrt( tmp / (n - 2) );
	coeff[3] = se;
}
