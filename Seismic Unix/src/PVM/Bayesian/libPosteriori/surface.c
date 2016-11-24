/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*                                                              */
/*  Function buildFreeSurfaceCompensation()                     */
/*                                                              */
/*  Compute transmissivity matrix that incorporates             */
/*  free surface boundary consitions                            */
/*                                                              */
/*  Input parameters:                                           */
/*  am.....................p-wave vertical slowness at the      */
/*                         surface                              */
/*  bm.....................s-wave vertical slowness at the      */
/*                         surface                              */
/*  wC.....................complex frequency                    */
/*                         global variable                      */
/*  uC.....................complex slowness                     */
/*                         global variable                      */
/*                                                              */
/*  Output parameters:                                          */
/*  h[2][2]................transmissivity matrix with free      */
/*                         surface boundary condition           */
/*                         global variable                      */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
#include "posteriori.h"
void buildFreeSurfaceCompensation(complex am, complex bm) 
{
   /* declaration of variables */
   complex ambm;                 /* - am * bm */
   
   /* computing compensation for free-surface */
   /* auxiliar quantities */
   ambm.r = am.r * bm.r - am.i * bm.i;
   ambm.i = am.r * bm.i + am.i * bm.r;
   
   aux1.r = uC2.r * S2Velocity[0][0].r - uC2.i * S2Velocity[0][0].i;
   aux1.i = uC2.r * S2Velocity[0][0].i + uC2.i * S2Velocity[0][0].r;
   
   aux2.r = 1. - (uuC2.r * S2Velocity[0][0].r - 
		  uuC2.i * S2Velocity[0][0].i);
   aux2.i = -(uuC2.r * S2Velocity[0][0].i + uuC2.i * S2Velocity[0][0].r);
   
   /* S2Velocity[0] * S2Velocity[0] */
   auxm1 = S2Velocity[0][0].r * S2Velocity[0][0].r - 
      S2Velocity[0][0].i * S2Velocity[0][0].i;
   auxm2 = 2 * S2Velocity[0][0].r * S2Velocity[0][0].i;
   
   /* S2Velocity[0] * S2Velocity[0] * 4 * uuC */
   aux = 4 * (uuC.r * auxm1 - uuC.i * auxm2);
   auxm2 = 4 * (uuC.r * auxm2 + uuC.i * auxm1);
   auxm1 = aux;
   
   /* S2Velocity[0] * S2Velocity[0] * 4 * uuC * ambm */
   aux = ambm.r * auxm1 - ambm.i * auxm2;
   auxm2 = ambm.r * auxm2 + ambm.i * auxm1;
   auxm1 = aux;
   
   /* aux2 * aux2 */
   auxm3 = aux2.r * aux2.r - aux2.i * aux2.i;
   auxm4 = 2 * aux2.r * aux2.i;
   
   aux3.r = auxm1 + auxm3;
   aux3.i = auxm2 + auxm4;
   
   /* aux3 = 1. / aux3 */
   aux = aux3.r * aux3.r + aux3.i * aux3.i;
   aux3.r = aux3.r / aux;
   aux3.i = -aux3.i / aux;
   
   /* ambm * aux1 * aux3 */
   auxm1 = ambm.r * aux1.r - ambm.i * aux1.i;
   auxm2 = ambm.r * aux1.i + ambm.i * aux1.r;
   h[0][0].r = auxm1 * aux3.r - auxm2 * aux3.i;
   h[0][0].i = auxm1 * aux3.i + auxm2 * aux3.r;
   
   /* aux2 * aux3 */
   auxm1 = aux2.r * aux3.r - aux2.i * aux3.i;
   auxm2 = aux2.r * aux3.i + aux2.i * aux3.r;
   
   /* bm * aux2 * aux3 */
   h[0][1].r = auxm1 * bm.r - auxm2 * bm.i;
   h[0][1].i = auxm1 * bm.i + auxm2 * bm.r;
   
   /* am * aux2 * aux3 */
   h[1][0].r = auxm1 * am.r - auxm2 * am.i;
   h[1][0].i = auxm1 * am.i + auxm2 * am.r;
   
   h[1][1].r = -h[0][0].r;
   h[1][1].i = -h[0][0].i;
}
/*                                                              */
/*  Function freeSurface()                                      */
/*                                                              */
/*  Apply transmissivity matrix that incorporates               */
/*  free surface boundary consitions in the displacement        */
/*                                                              */
/*  Input parameters:                                           */
/*  v1, v2.................Potential vectors                    */
/*  wC.....................complex frequency                    */
/*                         global variable                      */
/*  uC.....................complex slowness                     */
/*                         global variable                      */
/*                                                              */
/*  Output parameters:                                          */
/*  v1, v2.................Potential vectors dotted into        */
/*                         the transmissivity matrix h          */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void freeSurface(complex **v1, complex **v2)
{
   /* declaration of variables */
   complex v1A, v2A;             /* auxiliary values */
   
   auxm1 = h[0][0].r * v1[0][0].r - h[0][0].i * v1[0][0].i;
   auxm2 = h[0][0].r * v1[0][0].i + h[0][0].i * v1[0][0].r;
   auxm3 = h[0][1].r * v1[0][1].r - h[0][1].i * v1[0][1].i;
   auxm4 = h[0][1].r * v1[0][1].i + h[0][1].i * v1[0][1].r;
   v1A = v1[0][0];              /* for future use */
   v1[0][0].r = auxm1 + auxm3;
   v1[0][0].i = auxm2 + auxm4;
   
   auxm1 = h[0][0].r * v2[0][0].r - h[0][0].i * v2[0][0].i;
   auxm2 = h[0][0].r * v2[0][0].i + h[0][0].i * v2[0][0].r;
   auxm3 = h[0][1].r * v2[0][1].r - h[0][1].i * v2[0][1].i;
   auxm4 = h[0][1].r * v2[0][1].i + h[0][1].i * v2[0][1].r;
   v2A = v2[0][0];              /* for future use */
   v2[0][0].r = auxm1 + auxm3;
   v2[0][0].i = auxm2 + auxm4;
   
   auxm1 = h[1][0].r * v1A.r - h[1][0].i * v1A.i;
   auxm2 = h[1][0].r * v1A.i + h[1][0].i * v1A.r;
   auxm3 = h[1][1].r * v1[0][1].r - h[1][1].i * v1[0][1].i;
   auxm4 = h[1][1].r * v1[0][1].i + h[1][1].i * v1[0][1].r;
   v1[0][1].r = auxm1 + auxm3;
   v1[0][1].i = auxm2 + auxm4;
   
   auxm1 = h[1][0].r * v2A.r - h[1][0].i * v2A.i;
   auxm2 = h[1][0].r * v2A.i + h[1][0].i * v2A.r;
   auxm3 = h[1][1].r * v2[0][1].r - h[1][1].i * v2[0][1].i;
   auxm4 = h[1][1].r * v2[0][1].i + h[1][1].i * v2[0][1].r;
   v2[0][1].r = auxm1 + auxm3;
   v2[0][1].i = auxm2 + auxm4;
}
/*                                                              */
/*  Function freeSurfaceFrechet()                               */
/*                                                              */
/*  Apply transmissivity matrix that incorporates               */
/*  free surface boundary consitions in the Frechet             */
/*  derivarives of the displacement                             */
/*                                                              */
/*  Input parameters:                                           */
/*  v1, v2.................Potential vectors for the Frechet    */
/*                         derivatives                          */
/*  wC.....................complex frequency                    */
/*                         global variable                      */
/*  uC.....................complex slowness                     */
/*                         global variable                      */
/*                                                              */
/*  Output parameters:                                          */
/*  v1, v2.................Frechet derivative potential vectors */
/*                         dotted into the transmissivity       */
/*                         matrix h                             */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void freeSurfaceFrechet(complex **v1, complex **v2)
{
   /* declaration of variables */
   int iDer;                     /* counter */
   complex v1A, v2A;             /* auxiliary values */

   /* loop over "active" layers */
   for (iDer = 1; iDer <= numberPar * limRange; iDer++)
   {
      /* multiplication by matrix h */
      auxm1 = h[0][0].r * v1[iDer][0].r - h[0][0].i * v1[iDer][0].i;
      auxm2 = h[0][0].r * v1[iDer][0].i + h[0][0].i * v1[iDer][0].r;
      auxm3 = h[0][1].r * v1[iDer][1].r - h[0][1].i * v1[iDer][1].i;
      auxm4 = h[0][1].r * v1[iDer][1].i + h[0][1].i * v1[iDer][1].r;
      v1A = v1[iDer][0];              /* for future use */
      v1[iDer][0].r = auxm1 + auxm3;
      v1[iDer][0].i = auxm2 + auxm4;
      
      auxm1 = h[0][0].r * v2[iDer][0].r - h[0][0].i * v2[iDer][0].i;
      auxm2 = h[0][0].r * v2[iDer][0].i + h[0][0].i * v2[iDer][0].r;
      auxm3 = h[0][1].r * v2[iDer][1].r - h[0][1].i * v2[iDer][1].i;
      auxm4 = h[0][1].r * v2[iDer][1].i + h[0][1].i * v2[iDer][1].r;
      v2A = v2[iDer][0];              /* for future use */
      v2[iDer][0].r = auxm1 + auxm3;
      v2[iDer][0].i = auxm2 + auxm4;
      
      auxm1 = h[1][0].r * v1A.r - h[1][0].i * v1A.i;
      auxm2 = h[1][0].r * v1A.i + h[1][0].i * v1A.r;
      auxm3 = h[1][1].r * v1[iDer][1].r - h[1][1].i * v1[iDer][1].i;
      auxm4 = h[1][1].r * v1[iDer][1].i + h[1][1].i * v1[iDer][1].r;
      v1[iDer][1].r = auxm1 + auxm3;
      v1[iDer][1].i = auxm2 + auxm4;
      
      auxm1 = h[1][0].r * v2A.r - h[1][0].i * v2A.i;
      auxm2 = h[1][0].r * v2A.i + h[1][0].i * v2A.r;
      auxm3 = h[1][1].r * v2[iDer][1].r - h[1][1].i * v2[iDer][1].i;
      auxm4 = h[1][1].r * v2[iDer][1].i + h[1][1].i * v2[iDer][1].r;
      v2[iDer][1].r = auxm1 + auxm3;
      v2[iDer][1].i = auxm2 + auxm4;
   }
}


