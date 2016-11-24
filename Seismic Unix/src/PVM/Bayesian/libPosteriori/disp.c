/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*                                                              */
/*  Function displacements()                                    */
/*                                                              */
/*  Computing offset-dependent quantities to be used in the     */
/*  integration over slowness                                   */
/*                                                              */
/*  Input parameters:                                           */
/*  v1, v2.................Potential vectors                    */
/*                         global variable                      */
/*  iU.....................Current slowness                     */
/*                                                              */
/*  Output parameters:                                          */
/*  aux11..................|                                    */
/*  aux12..................|                                    */
/*  aux21..................|                                    */
/*  aux22..................|                                    */
/*  aux11Old...............|Auxiliary quantities                */
/*  aux12Old...............|                                    */
/*  aux21Old...............|                                    */
/*  aux22Old...............|                                    */
/*  aux11Old...............|                                    */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
#include "posteriori.h"

void displacements(int iU)
{
   /* declaration of variables */
   int iR;                 /* counter */
   float arg;              /* argument of the Bessel function */
   
   /* loop over offsets for computing the displacements */
   for (iR = 0; iR < nR; iR++)
   {
      /* Bessel functions */
      arg = sqrt((uC.r * wC.r - uC.i * wC.i) * 
		 (uC.r * wC.r - uC.i * wC.i) + 
		 (wC.i * uC.r + uC.i * wC.r) * 
		 (wC.i * uC.r + uC.i * wC.r)) * ABS(recArray[iR]);
      Bessels(arg);
      
      /* r component */
      if (RADIAL)
      {
	 aux1.r = -J11 * taper[iU] * v1[0][0].r;
	 aux1.i = -J11 * taper[iU] * v1[0][0].i;
	 
	 aux11[0][iR].r += aux1.r + aux11Old[0][iR].r;
	 aux11[0][iR].i += aux1.i + aux11Old[0][iR].i;
	 aux11Old[0][iR] = aux1;
	 
	 aux1.r = J00 * taper[iU] * v2[0][0].r;
	 aux1.i = J00 * taper[iU] * v2[0][0].i;
	 
	 aux21[0][iR].r += aux1.r + aux21Old[0][iR].r;
	 aux21[0][iR].i += aux1.i + aux21Old[0][iR].i;
	 aux21Old[0][iR] = aux1;
      }
      
      /* z component */
      if (VERTICAL)
      {
	 aux1.r = -v1[0][1].i;
	 aux1.i = v1[0][1].r;
	 
	 aux1.r = J00 * taper[iU] * aux1.r;
	 aux1.i = J00 * taper[iU] * aux1.i;
	 
	 aux12[0][iR].r += aux1.r + aux12Old[0][iR].r;
	 aux12[0][iR].i += aux1.i + aux12Old[0][iR].i;
	 aux12Old[0][iR] = aux1;	
	 
	 aux1.r = -v2[0][1].i;
	 aux1.i =  v2[0][1].r;
	 
	 aux1.r = J11 * taper[iU] * aux1.r;
	 aux1.i = J11 * taper[iU] * aux1.i;
	 
	 aux22[0][iR].r += aux1.r + aux22Old[0][iR].r;
	 aux22[0][iR].i += aux1.i + aux22Old[0][iR].i;
	 aux22Old[0][iR] = aux1;
      }
   }
}
/*                                                              */
/*  Function displacementsFrechet()                             */
/*                                                              */
/*  Computing offset-dependent quantities to be used in the     */
/*  integration over slowness for the Frechet derivatives       */
/*                                                              */
/*  Input parameters:                                           */
/*  v1, v2.................Potential vectors                    */
/*  iU.....................Current slowness                     */
/*                                                              */
/*  Output parameters:                                          */
/*  aux11..................|                                    */
/*  aux12..................|                                    */
/*  aux21..................|                                    */
/*  aux22..................|                                    */
/*  aux11Old...............|Auxiliary quantities for each of    */
/*  aux12Old...............|the Frechet derivatives             */
/*  aux21Old...............|                                    */
/*  aux22Old...............|                                    */
/*  aux11Old...............|                                    */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void displacementsFrechet(int iU)
{
   /* declaration of variables */
   int iR, iDer;           /* counters */
   float arg;              /* argument of the Bessel function */

   for (iR = 0; iR < nR; iR++)
   {
      /* Bessel functions */
      arg = sqrt((uC.r * wC.r - uC.i * wC.i) * 
		 (uC.r * wC.r - uC.i * wC.i) + 
		 (wC.i * uC.r + uC.i * wC.r) * 
		 (wC.i * uC.r + uC.i * wC.r)) * ABS(recArray[iR]);
      Bessels(arg);
      
      /* loop over "active" layers */
      for (iDer = 0; iDer < numberPar * limRange; iDer++)
      {
	 /* r component */
	 if (RADIAL)
	 {
	    aux1.r = -J11 * taper[iU] * v1[iDer + 1][0].r;
	    aux1.i = -J11 * taper[iU] * v1[iDer + 1][0].i;
	    
	    aux11[iDer][iR].r += aux1.r + aux11Old[iDer][iR].r;
	    aux11[iDer][iR].i += aux1.i + aux11Old[iDer][iR].i;
	    aux11Old[iDer][iR] = aux1;
	    
	    aux1.r = J00 * taper[iU] * v2[iDer + 1][0].r;
	    aux1.i = J00 * taper[iU] * v2[iDer + 1][0].i;
	    
	    aux21[iDer][iR].r += aux1.r + aux21Old[iDer][iR].r;
	    aux21[iDer][iR].i += aux1.i + aux21Old[iDer][iR].i;
	    aux21Old[iDer][iR] = aux1;
	 }
	 
	 /* z component */
	 if (VERTICAL)
	 {
	    aux1.r = -v1[iDer + 1][1].i;
	    aux1.i = v1[iDer + 1][1].r;
	    
	    aux1.r = J00 * taper[iU] * aux1.r;
	    aux1.i = J00 * taper[iU] * aux1.i;
	    
	    aux12[iDer][iR].r += aux1.r + aux12Old[iDer][iR].r;
	    aux12[iDer][iR].i += aux1.i + aux12Old[iDer][iR].i;
	    aux12Old[iDer][iR] = aux1;	
	    
	    aux1.r = -v2[iDer + 1][1].i;
	    aux1.i =  v2[iDer + 1][1].r;
	    
	    aux1.r = J11 * taper[iU] * aux1.r;
	    aux1.i = J11 * taper[iU] * aux1.i;
	    
	    aux22[iDer][iR].r += aux1.r + aux22Old[iDer][iR].r;
	    aux22[iDer][iR].i += aux1.i + aux22Old[iDer][iR].i;
	    aux22Old[iDer][iR] = aux1;
	 }
      }
   }
}
