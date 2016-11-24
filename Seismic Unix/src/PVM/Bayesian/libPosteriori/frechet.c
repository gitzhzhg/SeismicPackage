/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*                                                              */
/*  Function RmFrechet()                                        */
/*                                                              */
/*  Computing reflectivity matrices for a given model at a      */
/*  specific slowness and temporal frequency and its            */
/*  correspondent Frechet derivatives 	                        */
/*                                                              */
/*  Input parameters:                                           */
/*  alpha..................p-wave velocities of the model       */
/*                         global variable                      */
/*  beta...................s-wave velocities of the model       */
/*                         global variable                      */
/*  rho....................densities of the elastic model       */
/*                         global variable                      */
/*  thick..................thicknesses of the model             */ 
/*                         global variable                      */
/*  wC.....................complex frequency                    */
/*                         global variable                      */
/*  uC.....................complex slowness                     */
/*                         global variable                      */
/*                                                              */
/*  Output parameters:                                          */
/*  rm.....................the reflectivity matrix              */
/*  DmB....................Frechet derivatives of the           */
/*                         reflectivity matrix                  */
/*  note the DmB data structure:                                */
/*  DmB[# of Layers][derivatives from the layer (or lim[0])     */
/*                   to the layer (or lim[1])][4 elements]      */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
#include "posteriori.h"
void RmFrechet()
{
   /* declaration of variables */
   int i, iL, iDer;             /* counter */
   complex aux1;                /* auxiliar quantities */
   complex am, amI, bm, bmI;    /* vertical slownesses for P and S waves */
                                /* amI = am * I, bmI = bm * I */
   complex wThick;              /* wC * thickness */
   complex E[2][2];             /* phase-shift matrix */
   complex mT[2][2];            /* reflectivity matrix at top of layer */
   complex mTtD[2][2];          /* mt * tD */
   complex tUinv[2][2];         /* tU * inv */
   complex mB[2][2];            /* reflectivity matrix at bottom of layer */
   complex rD[2][2], tD[2][2];  /* reflec. and transm. coefficients for */
                                /* downgoing waves */
   complex rU[2][2], tU[2][2];  /* reflec. and transm. coefficients for */
                                /* upgoing waves */
   complex mAux[2][2];          /* auxiliar matrix */
   complex inv[2][2];           /* inv = (I - mT * rU)^-1 */
   
   /* initializing the reflectivity matrix at the bottom of half space */
   mT[0][0] = zeroC; mT[0][1] = zeroC;
   mT[1][0] = zeroC; mT[1][1] = zeroC;
   
   /* initializing the reflectivity matrix at the bottom */
   /* of layer nL - 1 */
   RTd(nL - 1, nL);

   mB[0][0] = coeffD[0]; mB[0][1] = coeffD[1]; 
   mB[1][0] = coeffD[2]; mB[1][1] = coeffD[3];

   /* checking depth limits */
   for (iDer = 0; iDer < numberPar; iDer++)
   {
      /* Vp -> Vs -> rho, if all active */
      if (lim[0] <= nL - 1 && nL - 1 < lim[1])
      {
	 /* Indexes: */
	 /*      [ 0  1 ]       */
	 /*      [ 2  3 ]       */
	 frechetRTd(nL - 1, nL, nL - 1);

	 DmB[nL - 1][limRange * iDer + 0][0] = coeffDFr[iDer][0];
	 DmB[nL - 1][limRange * iDer + 0][1] = coeffDFr[iDer][1];
	 DmB[nL - 1][limRange * iDer + 0][2] = coeffDFr[iDer][2];
	 DmB[nL - 1][limRange * iDer + 0][3] = coeffDFr[iDer][3];	       
      }
      if (lim[0] <= nL && nL < lim[1])
      {
	 frechetRTd(nL - 1, nL, nL);

 
	 DmB[nL - 1][limRange * iDer + 1][0] = coeffDFr[iDer][0];
	 DmB[nL - 1][limRange * iDer + 1][1] = coeffDFr[iDer][1];
	 DmB[nL - 1][limRange * iDer + 1][2] = coeffDFr[iDer][2];
	 DmB[nL - 1][limRange * iDer + 1][3] = coeffDFr[iDer][3];
      }
   }

   /* main loop over the nL layers */
   for (iL = nL - 1; iL >= 1; iL--)
   {
      /* square-root of PSlowness^2 - uuC */    
      auxm1 = PSlowness[iL][0].r - uuC.r;
      auxm2 = PSlowness[iL][0].i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      am.r = auxm3 * cos(angle);
      am.i = auxm3 * sin(angle);

      /* am * I */
      amI.r = -am.i;
      amI.i = am.r;

      /* square-root of SSlowness^2 - uuC */    
      auxm1 = SSlowness[iL][0].r - uuC.r;
      auxm2 = SSlowness[iL][0].i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      bm.r = auxm3 * cos(angle);
      bm.i = auxm3 * sin(angle);

      /* bm * I */
      bmI.r = -bm.i;
      bmI.i = bm.r;

      /* computing phase-shift matrix */
      wThick.r = wC.r * (-2 * thick[iL]);
      wThick.i = wC.i * (-2 * thick[iL]);

      /* cexp (amI * wThick) */
      auxm1 = amI.r * wThick.r - amI.i * wThick.i;
      auxm2 = amI.r * wThick.i + amI.i * wThick.r;
      E[0][0].r = exp(auxm1) * cos(auxm2);
      E[0][0].i = exp(auxm1) * sin(auxm2);

      /* cexp((amI + bmI) * (wThick * .5)) */
      auxm1 = amI.r + bmI.r;
      auxm2 = amI.i + bmI.i;
      auxm3 = .5 * (auxm1 * wThick.r - auxm2 * wThick.i);
      auxm4 = .5 * (auxm1 * wThick.i + auxm2 * wThick.r);
      E[0][1].r = exp(auxm3) * cos(auxm4);
      E[0][1].i = exp(auxm3) * sin(auxm4);

      E[1][0] = E[0][1];

      /* cexp (bmI * wThick) */
      auxm1 = bmI.r * wThick.r - bmI.i * wThick.i;
      auxm2 = bmI.r * wThick.i + bmI.i * wThick.r;
      E[1][1].r = exp(auxm1) * cos(auxm2);
      E[1][1].i = exp(auxm1) * sin(auxm2);
	
      /* applying phase-shift */
      mT[0][0].r = mB[0][0].r * E[0][0].r - mB[0][0].i * E[0][0].i;
      mT[0][0].i = mB[0][0].r * E[0][0].i + mB[0][0].i * E[0][0].r;
      mT[0][1].r = mB[0][1].r * E[0][1].r - mB[0][1].i * E[0][1].i;
      mT[0][1].i = mB[0][1].r * E[0][1].i + mB[0][1].i * E[0][1].r;
      mT[1][0].r = mB[1][0].r * E[1][0].r - mB[1][0].i * E[1][0].i;
      mT[1][0].i = mB[1][0].r * E[1][0].i + mB[1][0].i * E[1][0].r;
      mT[1][1].r = mB[1][1].r * E[1][1].r - mB[1][1].i * E[1][1].i;
      mT[1][1].i = mB[1][1].r * E[1][1].i + mB[1][1].i * E[1][1].r;

      /* bottom-layer matrix - need a sequence of Ref and TRANS coeff. */
      RTd(iL - 1, iL);

      rD[0][0] = coeffD[0]; rD[0][1] = coeffD[1]; 
      rD[1][0] = coeffD[2]; rD[1][1] = coeffD[3];
      
      tD[0][0] = coeffD[4]; tD[0][1] = coeffD[5]; 
      tD[1][0] = coeffD[6]; tD[1][1] = coeffD[7];

      /* computing mT * tD */
      mTtD[0][0].r = mT[0][0].r * tD[0][0].r - mT[0][0].i * tD[0][0].i
	           + mT[0][1].r * tD[1][0].r - mT[0][1].i * tD[1][0].i;
      mTtD[0][0].i = mT[0][0].r * tD[0][0].i + mT[0][0].i * tD[0][0].r
	           + mT[0][1].r * tD[1][0].i + mT[0][1].i * tD[1][0].r;
      mTtD[0][1].r = mT[0][0].r * tD[0][1].r - mT[0][0].i * tD[0][1].i
	           + mT[0][1].r * tD[1][1].r - mT[0][1].i * tD[1][1].i;
      mTtD[0][1].i = mT[0][0].r * tD[0][1].i + mT[0][0].i * tD[0][1].r
	           + mT[0][1].r * tD[1][1].i + mT[0][1].i * tD[1][1].r;
      mTtD[1][0].r = mT[1][0].r * tD[0][0].r - mT[1][0].i * tD[0][0].i
                   + mT[1][1].r * tD[1][0].r - mT[1][1].i * tD[1][0].i;
      mTtD[1][0].i = mT[1][0].r * tD[0][0].i + mT[1][0].i * tD[0][0].r
      	           + mT[1][1].r * tD[1][0].i + mT[1][1].i * tD[1][0].r;
      mTtD[1][1].r = mT[1][0].r * tD[0][1].r - mT[1][0].i * tD[0][1].i
      	           + mT[1][1].r * tD[1][1].r - mT[1][1].i * tD[1][1].i;
      mTtD[1][1].i = mT[1][0].r * tD[0][1].i + mT[1][0].i * tD[0][1].r
      	           + mT[1][1].r * tD[1][1].i + mT[1][1].i * tD[1][1].r;

      RTu(iL - 1, iL);

      rU[0][0] = coeffU[0]; rU[0][1] = coeffU[1]; 
      rU[1][0] = coeffU[2]; rU[1][1] = coeffU[3];
      
      tU[0][0] = coeffU[4]; tU[0][1] = coeffU[5]; 
      tU[1][0] = coeffU[6]; tU[1][1] = coeffU[7];

      /* inv = (I - mT * rU)^-1 */
      auxm1 = mT[0][0].r * rU[0][0].r - mT[0][0].i * rU[0][0].i;
      auxm2 = mT[0][0].r * rU[0][0].i + mT[0][0].i * rU[0][0].r;
      auxm3 = mT[0][1].r * rU[1][0].r - mT[0][1].i * rU[1][0].i;
      auxm4 = mT[0][1].r * rU[1][0].i + mT[0][1].i * rU[1][0].r;
      mAux[0][0].r = 1 - (auxm1 + auxm3);
      mAux[0][0].i = - (auxm2 + auxm4);

      auxm1 = mT[0][0].r * rU[0][1].r - mT[0][0].i * rU[0][1].i;
      auxm2 = mT[0][0].r * rU[0][1].i + mT[0][0].i * rU[0][1].r;
      auxm3 = mT[0][1].r * rU[1][1].r - mT[0][1].i * rU[1][1].i;
      auxm4 = mT[0][1].r * rU[1][1].i + mT[0][1].i * rU[1][1].r;
      mAux[0][1].r = - (auxm1 + auxm3);
      mAux[0][1].i = - (auxm2 + auxm4);

      auxm1 = mT[1][0].r * rU[0][0].r - mT[1][0].i * rU[0][0].i;
      auxm2 = mT[1][0].r * rU[0][0].i + mT[1][0].i * rU[0][0].r;
      auxm3 = mT[1][1].r * rU[1][0].r - mT[1][1].i * rU[1][0].i;
      auxm4 = mT[1][1].r * rU[1][0].i + mT[1][1].i * rU[1][0].r;
      mAux[1][0].r = - (auxm1 + auxm3);
      mAux[1][0].i = - (auxm2 + auxm4);

      auxm1 = mT[1][0].r * rU[0][1].r - mT[1][0].i * rU[0][1].i;
      auxm2 = mT[1][0].r * rU[0][1].i + mT[1][0].i * rU[0][1].r;
      auxm3 = mT[1][1].r * rU[1][1].r - mT[1][1].i * rU[1][1].i;
      auxm4 = mT[1][1].r * rU[1][1].i + mT[1][1].i * rU[1][1].r;
      mAux[1][1].r = 1 - (auxm1 + auxm3);
      mAux[1][1].i = - (auxm2 + auxm4);

      /* inverting */
      auxm1 = mAux[0][0].r * mAux[1][1].r - mAux[0][0].i * mAux[1][1].i;
      auxm2 = mAux[0][0].r * mAux[1][1].i + mAux[0][0].i * mAux[1][1].r;
      auxm3 = mAux[0][1].r * mAux[1][0].r - mAux[0][1].i * mAux[1][0].i;
      auxm4 = mAux[0][1].r * mAux[1][0].i + mAux[0][1].i * mAux[1][0].r;
      aux1.r = auxm1 - auxm3;
      aux1.i = auxm2 - auxm4;

      /* 1 / aux1 */
      aux = aux1.r * aux1.r + aux1.i * aux1.i;
      aux1.r = aux1.r / aux;
      aux1.i = -aux1.i / aux;

      inv[0][0].r = mAux[1][1].r * aux1.r - mAux[1][1].i * aux1.i;
      inv[0][0].i = mAux[1][1].r * aux1.i + mAux[1][1].i * aux1.r;
      inv[0][1].r = -1 * (mAux[0][1].r * aux1.r - mAux[0][1].i * aux1.i);
      inv[0][1].i = -1 * (mAux[0][1].r * aux1.i + mAux[0][1].i * aux1.r);
      inv[1][0].r = -1 * (mAux[1][0].r * aux1.r - mAux[1][0].i * aux1.i);
      inv[1][0].i = -1 * (mAux[1][0].r * aux1.i + mAux[1][0].i * aux1.r);
      inv[1][1].r = mAux[0][0].r * aux1.r - mAux[0][0].i * aux1.i;
      inv[1][1].i = mAux[0][0].r * aux1.i + mAux[0][0].i * aux1.r;
      

      /* computing tU * inv */
      tUinv[0][0].r = tU[0][0].r * inv[0][0].r - tU[0][0].i * inv[0][0].i
  	            + tU[0][1].r * inv[1][0].r - tU[0][1].i * inv[1][0].i;
      tUinv[0][0].i = tU[0][0].r * inv[0][0].i + tU[0][0].i * inv[0][0].r
	            + tU[0][1].r * inv[1][0].i + tU[0][1].i * inv[1][0].r;
      tUinv[0][1].r = tU[0][0].r * inv[0][1].r - tU[0][0].i * inv[0][1].i
	            + tU[0][1].r * inv[1][1].r - tU[0][1].i * inv[1][1].i;
      tUinv[0][1].i = tU[0][0].r * inv[0][1].i + tU[0][0].i * inv[0][1].r
	            + tU[0][1].r * inv[1][1].i + tU[0][1].i * inv[1][1].r;
      tUinv[1][0].r = tU[1][0].r * inv[0][0].r - tU[1][0].i * inv[0][0].i
                    + tU[1][1].r * inv[1][0].r - tU[1][1].i * inv[1][0].i;
      tUinv[1][0].i = tU[1][0].r * inv[0][0].i + tU[1][0].i * inv[0][0].r
      	            + tU[1][1].r * inv[1][0].i + tU[1][1].i * inv[1][0].r;
      tUinv[1][1].r = tU[1][0].r * inv[0][1].r - tU[1][0].i * inv[0][1].i
      	            + tU[1][1].r * inv[1][1].r - tU[1][1].i * inv[1][1].i;
      tUinv[1][1].i = tU[1][0].r * inv[0][1].i + tU[1][0].i * inv[0][1].r
      	            + tU[1][1].r * inv[1][1].i + tU[1][1].i * inv[1][1].r;
      
      /* finally the matrix */
      auxm1 = mTtD[0][0].r * tUinv[0][0].r - mTtD[0][0].i * tUinv[0][0].i;
      auxm2 = mTtD[0][0].r * tUinv[0][0].i + mTtD[0][0].i * tUinv[0][0].r;
      auxm3 = mTtD[1][0].r * tUinv[0][1].r - mTtD[1][0].i * tUinv[0][1].i;
      auxm4 = mTtD[1][0].r * tUinv[0][1].i + mTtD[1][0].i * tUinv[0][1].r;
      mB[0][0].r = rD[0][0].r + (auxm1 + auxm3);
      mB[0][0].i = rD[0][0].i + (auxm2 + auxm4);

      auxm1 = mTtD[0][1].r * tUinv[0][0].r - mTtD[0][1].i * tUinv[0][0].i;
      auxm2 = mTtD[0][1].r * tUinv[0][0].i + mTtD[0][1].i * tUinv[0][0].r;
      auxm3 = mTtD[1][1].r * tUinv[0][1].r - mTtD[1][1].i * tUinv[0][1].i;
      auxm4 = mTtD[1][1].r * tUinv[0][1].i + mTtD[1][1].i * tUinv[0][1].r;
      mB[0][1].r = rD[0][1].r + (auxm1 + auxm3);
      mB[0][1].i = rD[0][1].i + (auxm2 + auxm4);

      auxm1 = mTtD[0][0].r * tUinv[1][0].r - mTtD[0][0].i * tUinv[1][0].i;
      auxm2 = mTtD[0][0].r * tUinv[1][0].i + mTtD[0][0].i * tUinv[1][0].r;
      auxm3 = mTtD[1][0].r * tUinv[1][1].r - mTtD[1][0].i * tUinv[1][1].i;
      auxm4 = mTtD[1][0].r * tUinv[1][1].i + mTtD[1][0].i * tUinv[1][1].r;
      mB[1][0].r = rD[1][0].r + (auxm1 + auxm3);
      mB[1][0].i = rD[1][0].i + (auxm2 + auxm4);

      auxm1 = mTtD[0][1].r * tUinv[1][0].r - mTtD[0][1].i * tUinv[1][0].i;
      auxm2 = mTtD[0][1].r * tUinv[1][0].i + mTtD[0][1].i * tUinv[1][0].r;
      auxm3 = mTtD[1][1].r * tUinv[1][1].r - mTtD[1][1].i * tUinv[1][1].i;
      auxm4 = mTtD[1][1].r * tUinv[1][1].i + mTtD[1][1].i * tUinv[1][1].r;
      mB[1][1].r = rD[1][1].r + (auxm1 + auxm3);
      mB[1][1].i = rD[1][1].i + (auxm2 + auxm4);
      
      /* computing Frechet derivatives */
      frechetRm(E, tD, tUinv, mTtD, mT, rU, inv, wThick, am, bm, iL);
   }
   
   /* computing final phase-shift matrix */
   /* square-root of Pslowness^2 - uuC */    
   auxm1 = PSlowness[0][0].r - uuC.r;
   auxm2 = PSlowness[0][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   am.r = auxm3 * cos(angle);
   am.i = auxm3 * sin(angle);

   /* am * I */
   amI.r = -am.i;
   amI.i = am.r;

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[0][0].r - uuC.r;
   auxm2 = SSlowness[0][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   bm.r = auxm3 * cos(angle);
   bm.i = auxm3 * sin(angle);

   /* bm * I */
   bmI.r = -bm.i;
   bmI.i = bm.r;
   
   /* computing phase-shift matrix */
   wThick.r = wC.r * (-2 * thick[0]);
   wThick.i = wC.i * (-2 * thick[0]);

   /* cexp (amI * wThick) */
   auxm1 = amI.r * wThick.r - amI.i * wThick.i;
   auxm2 = amI.r * wThick.i + amI.i * wThick.r;
   E[0][0].r = exp(auxm1) * cos(auxm2);
   E[0][0].i = exp(auxm1) * sin(auxm2);

   /* cexp((amI + bmI) * (wThick * .5)) */
   auxm1 = amI.r + bmI.r;
   auxm2 = amI.i + bmI.i;
   auxm3 = .5 * (auxm1 * wThick.r - auxm2 * wThick.i);
   auxm4 = .5 * (auxm1 * wThick.i + auxm2 * wThick.r);
   E[0][1].r = exp(auxm3) * cos(auxm4);
   E[0][1].i = exp(auxm3) * sin(auxm4);
   
   E[1][0] = E[0][1];

   /* cexp (bmI * wThick) */
   auxm1 = bmI.r * wThick.r - bmI.i * wThick.i;
   auxm2 = bmI.r * wThick.i + bmI.i * wThick.r;
   E[1][1].r = exp(auxm1) * cos(auxm2);
   E[1][1].i = exp(auxm1) * sin(auxm2);

   /* applying phase-shift */
   mT[0][0].r = mB[0][0].r * E[0][0].r - mB[0][0].i * E[0][0].i;
   mT[0][0].i = mB[0][0].r * E[0][0].i + mB[0][0].i * E[0][0].r;
   mT[0][1].r = mB[0][1].r * E[0][1].r - mB[0][1].i * E[0][1].i;
   mT[0][1].i = mB[0][1].r * E[0][1].i + mB[0][1].i * E[0][1].r;
   mT[1][0].r = mB[1][0].r * E[1][0].r - mB[1][0].i * E[1][0].i;
   mT[1][0].i = mB[1][0].r * E[1][0].i + mB[1][0].i * E[1][0].r;
   mT[1][1].r = mB[1][1].r * E[1][1].r - mB[1][1].i * E[1][1].i;
   mT[1][1].i = mB[1][1].r * E[1][1].i + mB[1][1].i * E[1][1].r;

   /* copying to matrix rm */
   rm[0][0] = mT[0][0]; rm[0][1] = mT[0][1];
   rm[1][0] = mT[1][0]; rm[1][1] = mT[1][1];

   /* applying phase-shift in the FRECHET derivatives as well */
   for (iDer = 0; iDer < numberPar; iDer++)
   {
      /* Vp -> Vs -> rho, if all active */
      for (i = 0, iL = MIN(lim[0], 2); i < limRange; iL++, i++) 
      {
	 aux = DmB[0][limRange * iDer + iL][0].r * E[0][0].r - 
	       DmB[0][limRange * iDer + iL][0].i * E[0][0].i;
	 DmB[0][limRange * iDer + iL][0].i = 
	       DmB[0][limRange * iDer + iL][0].r * E[0][0].i + 
	       DmB[0][limRange * iDer + iL][0].i * E[0][0].r;
	 DmB[0][limRange * iDer + iL][0].r = aux;
	 aux = DmB[0][limRange * iDer + iL][1].r * E[0][1].r - 
	       DmB[0][limRange * iDer + iL][1].i * E[0][1].i;
	 DmB[0][limRange * iDer + iL][1].i = 
	       DmB[0][limRange * iDer + iL][1].r * E[0][1].i + 
	       DmB[0][limRange * iDer + iL][1].i * E[0][1].r;
	 DmB[0][limRange * iDer + iL][1].r = aux;
	 aux = DmB[0][limRange * iDer + iL][2].r * E[1][0].r - 
	       DmB[0][limRange * iDer + iL][2].i * E[1][0].i;
	 DmB[0][limRange * iDer + iL][2].i = 
	       DmB[0][limRange * iDer + iL][2].r * E[1][0].i + 
	       DmB[0][limRange * iDer + iL][2].i * E[1][0].r;
	 DmB[0][limRange * iDer + iL][2].r = aux;
	 aux = DmB[0][limRange * iDer + iL][3].r * E[1][1].r - 
	       DmB[0][limRange * iDer + iL][3].i * E[1][1].i;
	 DmB[0][limRange * iDer + iL][3].i = 
	       DmB[0][limRange * iDer + iL][3].r * E[1][1].i + 
	       DmB[0][limRange * iDer + iL][3].i * E[1][1].r;
	 DmB[0][limRange * iDer + iL][3].r = aux;
      }
   }
}
/*                                                              */
/*  Function frechetRm()                                        */
/*                                                              */
/*  Computing Frechet derivatives for the reflectivity          */
/*  matrix rm                                                   */
/*                                                              */
/*  Input parameters:                                           */
/*  E[2][2]................phase shift matrix                   */
/*  tD[2][2]...............downgoing transmission coefficients  */
/*  rU[2][2]...............upgoing reflection coefficients      */
/*  mT.....................partial reflectivity matrix          */
/*  inv....................(I - mT * rU)^-1                     */
/*  wThick.................wC * thickness                       */
/*  am.....................p-wave vertical slowness             */
/*  bm.....................s-wave vertical slowness             */
/*  iL.....................points to the current layer          */
/*                                                              */
/*  Output parameters:                                          */
/*  DmB....................Frechet derivatives of the           */
/*                         reflectivity matrix at level iL      */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void frechetRm(complex E[2][2], complex tD[2][2], complex tUinv[2][2], 
	       complex mTtD[2][2], complex mT[2][2], complex rU[2][2], 
	       complex inv[2][2], complex wThick, complex am, 
	       complex bm, int iL)
{
   /* declaration of variables */
   int iDer, k, kDer=0, kPnt=0;           /* counters */
   complex sInv;                          /* 1 / Slowness */
   static complex DrD[2][2], DtD[2][2];   /* coefficient derivatives */
   static complex DrU[2][2], DtU[2][2];   /* coefficient derivatives */
   static complex factor1[2][2], factor2[2][2], factor3[2][2];
   static complex A[2][2], B[2][2], C[2][2];
                                          /* auxiliar variables */
   static complex invmTtD[2][2];          /* inv * mT * tD */
   
   /* inv * mTtD */
   matMult(invmTtD, inv, mTtD);

   /* pointer to previous derivatives */
   for (k = MAX(iL - 1, lim[0]), kDer = 2; k < lim[1]; k++)
   {
      /* considering special cases */
      if (k == iL - 1)
      {
	 /* differentiating reflection and transmition coefficients */
	 frechetRTd(iL - 1, iL, iL - 1);
	 frechetRTu(iL - 1, iL, iL - 1);
	 
	 for (iDer = 0; iDer < numberPar; iDer++)
	 {
	    /* Vp -> Vs -> rho, if all active */
	    DrD[0][0] = coeffDFr[iDer][0]; DrD[0][1] = coeffDFr[iDer][1]; 
	    DrD[1][0] = coeffDFr[iDer][2]; DrD[1][1] = coeffDFr[iDer][3];
	 
	    DtD[0][0] = coeffDFr[iDer][4]; DtD[0][1] = coeffDFr[iDer][5]; 
	    DtD[1][0] = coeffDFr[iDer][6]; DtD[1][1] = coeffDFr[iDer][7];
	 
	    DrU[0][0] = coeffUFr[iDer][0]; DrU[0][1] = coeffUFr[iDer][1]; 
	    DrU[1][0] = coeffUFr[iDer][2]; DrU[1][1] = coeffUFr[iDer][3];
	 
	    DtU[0][0] = coeffUFr[iDer][4]; DtU[0][1] = coeffUFr[iDer][5]; 
	    DtU[1][0] = coeffUFr[iDer][6]; DtU[1][1] = coeffUFr[iDer][7];

	    /* DtU * invmTtD */
	    matMult(factor1, DtU, invmTtD);
	    /* mT * DrU */
	    matMult(A, mT, DrU);
	    /* A * mTtD */
	    matMult(B, A, invmTtD);
	    /* C * tUinv */
	    matMult(factor2, tUinv, B);
	    
	    /* mT * DtD */
	    matMult(A, mT, DtD);
	    /* tUinv * A */
	    matMult(factor3, tUinv, A);
	    
	    /* compounding factors */
	    /*      [ 0  1 ]       */
	    /*      [ 2  3 ]       */
	    DmB[iL - 1][limRange * iDer + 0][0].r = DrD[0][0].r + 
	       factor1[0][0].r + factor2[0][0].r + factor3[0][0].r;
	    DmB[iL - 1][limRange * iDer + 0][0].i = DrD[0][0].i + 
	       factor1[0][0].i + factor2[0][0].i + factor3[0][0].i;
	    DmB[iL - 1][limRange * iDer + 0][1].r = DrD[0][1].r + 
	       factor1[0][1].r + factor2[0][1].r + factor3[0][1].r;
	    DmB[iL - 1][limRange * iDer + 0][1].i = DrD[0][1].i + 
	       factor1[0][1].i + factor2[0][1].i + factor3[0][1].i;
	    DmB[iL - 1][limRange * iDer + 0][2].r = DrD[1][0].r + 
	       factor1[1][0].r + factor2[1][0].r + factor3[1][0].r;
	    DmB[iL - 1][limRange * iDer + 0][2].i = DrD[1][0].i + 
	       factor1[1][0].i + factor2[1][0].i + factor3[1][0].i;
	    DmB[iL - 1][limRange * iDer + 0][3].r = DrD[1][1].r + 
	       factor1[1][1].r + factor2[1][1].r + factor3[1][1].r;
	    DmB[iL - 1][limRange * iDer + 0][3].i = DrD[1][1].i + 
	       factor1[1][1].i +  factor2[1][1].i + factor3[1][1].i;

	 }
      }
      else if (k == iL)
      {
	 /* differentiating reflection and transmition coefficients */
	 frechetRTd(iL - 1, iL, iL);
	 frechetRTu(iL - 1, iL, iL);
	 
	 /* saving flags */
	 vpF = vpFrechet;
	 vsF = vsFrechet;
	 rhoF = rhoFrechet;
	 for (iDer = 0; iDer < numberPar; iDer++)
	 {
	    /* Vp -> Vs -> rho */
	    DrD[0][0] = coeffDFr[iDer][0]; DrD[0][1] = coeffDFr[iDer][1]; 
	    DrD[1][0] = coeffDFr[iDer][2]; DrD[1][1] = coeffDFr[iDer][3];
	 
	    DtD[0][0] = coeffDFr[iDer][4]; DtD[0][1] = coeffDFr[iDer][5]; 
	    DtD[1][0] = coeffDFr[iDer][6]; DtD[1][1] = coeffDFr[iDer][7];
	    
	    DrU[0][0] = coeffUFr[iDer][0]; DrU[0][1] = coeffUFr[iDer][1]; 
	    DrU[1][0] = coeffUFr[iDer][2]; DrU[1][1] = coeffUFr[iDer][3];
	    
	    DtU[0][0] = coeffUFr[iDer][4]; DtU[0][1] = coeffUFr[iDer][5]; 
	    DtU[1][0] = coeffUFr[iDer][6]; DtU[1][1] = coeffUFr[iDer][7];
	    
	    /* DtU * invmTtD */
	    matMult(factor1, DtU, invmTtD);
	    
	    /* applying phase-shift on D (MB_iL+1) / D (iL+1) */
	    /*      [ 0  1 ]       */
	    /*      [ 2  3 ]       */
	    A[0][0].r = DmB[iL][limRange * iDer + 0][0].r * E[0][0].r
  	              - DmB[iL][limRange * iDer + 0][0].i * E[0][0].i;
	    A[0][0].i = DmB[iL][limRange * iDer + 0][0].r * E[0][0].i 
	              + DmB[iL][limRange * iDer + 0][0].i * E[0][0].r;
	    A[0][1].r = DmB[iL][limRange * iDer + 0][1].r * E[0][1].r 
	              - DmB[iL][limRange * iDer + 0][1].i * E[0][1].i;
	    A[0][1].i = DmB[iL][limRange * iDer + 0][1].r * E[0][1].i 
	              + DmB[iL][limRange * iDer + 0][1].i * E[0][1].r;
	    A[1][0].r = DmB[iL][limRange * iDer + 0][2].r * E[1][0].r 
	              - DmB[iL][limRange * iDer + 0][2].i * E[1][0].i;
	    A[1][0].i = DmB[iL][limRange * iDer + 0][2].r * E[1][0].i 
	              + DmB[iL][limRange * iDer + 0][2].i * E[1][0].r;
	    A[1][1].r = DmB[iL][limRange * iDer + 0][3].r * E[1][1].r 
	              - DmB[iL][limRange * iDer + 0][3].i * E[1][1].i;
	    A[1][1].i = DmB[iL][limRange * iDer + 0][3].r * E[1][1].i 
	              + DmB[iL][limRange * iDer + 0][3].i * E[1][1].r;
	    
	    /* things now are different if the derivative is being */
	    /* taken with respect to P_wave, S_wave velocities or */
	    /* densities */
	    if (vpFrechet) /* P-wave */
	    {
	       /* mT * [i wThick] */
	       /* note that wThick includes the - sign */
	       auxm1 = -wThick.i; auxm2 = wThick.r;
	       B[0][0].r = mT[0][0].r * auxm1 - mT[0][0].i * auxm2;
	       B[0][0].i = mT[0][0].r * auxm2 + mT[0][0].i * auxm1;
	       B[0][1].r = 0.5 * (mT[0][1].r * auxm1 - mT[0][1].i * auxm2);
	       B[0][1].i = 0.5 * (mT[0][1].r * auxm2 + mT[0][1].i * auxm1);
	       B[1][0].r = 0.5 * (mT[1][0].r * auxm1 - mT[1][0].i * auxm2);
	       B[1][0].i = 0.5 * (mT[1][0].r * auxm2 + mT[1][0].i * auxm1);
	       B[1][1].r = mT[1][1].r * auxm1 - mT[1][1].i * auxm2;
	       B[1][1].i = mT[1][1].r * auxm2 + mT[1][1].i * auxm1;
	       
	       /* 1 / am */
	       aux = am.r * am.r + am.i * am.i;
	       sInv.r = am.r / aux;
	       sInv.i = -am.i / aux;
	       
	       /* 1 / am * derFactor[0] */
	       auxm1 = sInv.r * derFactor[iL][0].r - 
		       sInv.i * derFactor[iL][0].i;
	       auxm2 = sInv.r * derFactor[iL][0].i + 
		       sInv.i * derFactor[iL][0].r;
	       
	       C[0][0].r = A[0][0].r - (B[0][0].r * auxm1 - B[0][0].i * auxm2);
	       C[0][0].i = A[0][0].i - (B[0][0].r * auxm2 + B[0][0].i * auxm1);
	       C[0][1].r = A[0][1].r - (B[0][1].r * auxm1 - B[0][1].i * auxm2);
	       C[0][1].i = A[0][1].i - (B[0][1].r * auxm2 + B[0][1].i * auxm1);
	       C[1][0].r = A[1][0].r - (B[1][0].r * auxm1 - B[1][0].i * auxm2);
	       C[1][0].i = A[1][0].i - (B[1][0].r * auxm2 + B[1][0].i * auxm1);
	       C[1][1] = A[1][1];
	       vpFrechet = 0;
	    }
	    else if (vsFrechet) /* S-wave */
	    {
	       /* mT * [i -wThick] */
	       /* note that wThick includes the - sign */
	       auxm1 = -wThick.i; auxm2 = wThick.r;
	       B[0][0].r = mT[0][0].r * auxm1 - mT[0][0].i * auxm2;
	       B[0][0].i = mT[0][0].r * auxm2 + mT[0][0].i * auxm1;
	       B[0][1].r = 0.5 * (mT[0][1].r * auxm1 - mT[0][1].i * auxm2);
	       B[0][1].i = 0.5 * (mT[0][1].r * auxm2 + mT[0][1].i * auxm1);
	       B[1][0].r = 0.5 * (mT[1][0].r * auxm1 - mT[1][0].i * auxm2);
	       B[1][0].i = 0.5 * (mT[1][0].r * auxm2 + mT[1][0].i * auxm1);
	       B[1][1].r = mT[1][1].r * auxm1 - mT[1][1].i * auxm2;
	       B[1][1].i = mT[1][1].r * auxm2 + mT[1][1].i * auxm1;

	       /* 1 / bm */
	       aux = bm.r * bm.r + bm.i * bm.i;
	       sInv.r = bm.r / aux;
	       sInv.i = -bm.i / aux;
	       
	       /* 1 / Slowness * derFactor[1] */
	       auxm1 = sInv.r * derFactor[iL][1].r - 
		       sInv.i * derFactor[iL][1].i;
	       auxm2 = sInv.r * derFactor[iL][1].i + 
		       sInv.i * derFactor[iL][1].r;
	    
	       C[0][0] = A[0][0];
	       C[0][1].r = A[0][1].r - (B[0][1].r * auxm1 - B[0][1].i * auxm2);
	       C[0][1].i = A[0][1].i - (B[0][1].r * auxm2 + B[0][1].i * auxm1);
	       C[1][0].r = A[1][0].r - (B[1][0].r * auxm1 - B[1][0].i * auxm2);
	       C[1][0].i = A[1][0].i - (B[1][0].r * auxm2 + B[1][0].i * auxm1);
	       C[1][1].r = A[1][1].r - (B[1][1].r * auxm1 - B[1][1].i * auxm2);
	       C[1][1].i = A[1][1].i - (B[1][1].r * auxm2 + B[1][1].i * auxm1);
	       vsFrechet = 0;
	    }
	    else if (rhoFrechet) /* Density */
	    {
	       C[0][0] = A[0][0];
	       C[0][1] = A[0][1];
	       C[1][0] = A[1][0];
	       C[1][1] = A[1][1];
	       rhoFrechet = 0;
	    }
	 
	    /* C * rU */
	    matMult(A, C, rU);
	    /* mT * DrU */
	    matMult(B, mT, DrU);
	    A[0][0].r += B[0][0].r;
	    A[0][0].i += B[0][0].i;
	    A[0][1].r += B[0][1].r;
	    A[0][1].i += B[0][1].i;
	    A[1][0].r += B[1][0].r;
	    A[1][0].i += B[1][0].i;
	    A[1][1].r += B[1][1].r;
	    A[1][1].i += B[1][1].i;
	    
	    /* A * invmTtD */
	    matMult(B, A, invmTtD);
	    /* tUinv * B */
	    matMult(factor2, tUinv, B);
	    
	    /* C * tD */
	    matMult(A, C, tD);
	    /* mT * DtD */
	    matMult(B, mT, DtD);
	    
	    A[0][0].r += B[0][0].r; A[0][0].i += B[0][0].i;
	    A[1][0].r += B[1][0].r; A[1][0].i += B[1][0].i;
	    A[0][1].r += B[0][1].r; A[0][1].i += B[0][1].i;
	    A[1][1].r += B[1][1].r; A[1][1].i += B[1][1].i;
	    
	    /* tUinv * A */
	    matMult(factor3, tUinv, A);
	    
	    /* compounding factors */
	    /*      [ 0  1 ]       */
	    /*      [ 2  3 ]       */
	    DmB[iL - 1][limRange * iDer + 1][0].r = DrD[0][0].r + 
	       factor1[0][0].r + factor2[0][0].r + factor3[0][0].r;
	    DmB[iL - 1][limRange * iDer + 1][0].i = DrD[0][0].i + 
	       factor1[0][0].i + factor2[0][0].i + factor3[0][0].i;
	    DmB[iL - 1][limRange * iDer + 1][1].r = DrD[0][1].r + 
	       factor1[0][1].r + factor2[0][1].r + factor3[0][1].r;
	    DmB[iL - 1][limRange * iDer + 1][1].i = DrD[0][1].i + 
	       factor1[0][1].i + factor2[0][1].i + factor3[0][1].i;
	    DmB[iL - 1][limRange * iDer + 1][2].r = DrD[1][0].r + 
	       factor1[1][0].r + factor2[1][0].r + factor3[1][0].r;
	    DmB[iL - 1][limRange * iDer + 1][2].i = DrD[1][0].i + 
	       factor1[1][0].i + factor2[1][0].i + factor3[1][0].i;
	    DmB[iL - 1][limRange * iDer + 1][3].r = DrD[1][1].r + 
	       factor1[1][1].r + factor2[1][1].r + factor3[1][1].r;
	    DmB[iL - 1][limRange * iDer + 1][3].i = DrD[1][1].i + 
	       factor1[1][1].i + factor2[1][1].i + factor3[1][1].i;
	 }
	 /* restoring */
	 vpFrechet = vpF;
	 vsFrechet = vsF;
	 rhoFrechet = rhoF;
      }
      else
      {
	 /* applying phase-shift on D (MB_j+1) / D (j...on) */
	 /*      [ 0  1 ]       */
	 /*      [ 2  3 ]       */
	 for (iDer = 0; iDer < numberPar; iDer++)
	 {
	    /* Vp -> Vs -> rho, if all active */
	    kPnt = ((lim[0] - iL) >= (2) ? (kDer) : (kDer - 1));
	    A[0][0].r = DmB[iL][limRange * iDer + kPnt][0].r * E[0][0].r
	              - DmB[iL][limRange * iDer + kPnt][0].i * E[0][0].i;
	    A[0][0].i = DmB[iL][limRange * iDer + kPnt][0].r * E[0][0].i 
	              + DmB[iL][limRange * iDer + kPnt][0].i * E[0][0].r;
	    A[0][1].r = DmB[iL][limRange * iDer + kPnt][1].r * E[0][1].r 
	              - DmB[iL][limRange * iDer + kPnt][1].i * E[0][1].i;
	    A[0][1].i = DmB[iL][limRange * iDer + kPnt][1].r * E[0][1].i 
	              + DmB[iL][limRange * iDer + kPnt][1].i * E[0][1].r;
	    A[1][0].r = DmB[iL][limRange * iDer + kPnt][2].r * E[1][0].r 
	              - DmB[iL][limRange * iDer + kPnt][2].i * E[1][0].i;
	    A[1][0].i = DmB[iL][limRange * iDer + kPnt][2].r * E[1][0].i 
	              + DmB[iL][limRange * iDer + kPnt][2].i * E[1][0].r;
	    A[1][1].r = DmB[iL][limRange * iDer + kPnt][3].r * E[1][1].r 
	              - DmB[iL][limRange * iDer + kPnt][3].i * E[1][1].i;
	    A[1][1].i = DmB[iL][limRange * iDer + kPnt][3].r * E[1][1].i 
	              + DmB[iL][limRange * iDer + kPnt][3].i * E[1][1].r;
 
	    /* tUinv * A */
	    matMult(C, tUinv, A);
	    
	    /* rU * invmTtD */
	    matMult(B, rU, invmTtD);
	    B[0][0].r += tD[0][0].r; B[0][0].i += tD[0][0].i;
	    B[0][1].r += tD[0][1].r; B[0][1].i += tD[0][1].i;
	    B[1][0].r += tD[1][0].r; B[1][0].i += tD[1][0].i;
	    B[1][1].r += tD[1][1].r; B[1][1].i += tD[1][1].i;
	    
	    /* C * B */
	    matMult(factor1, C, B);
	    
	    /* compounding factors */
	    /*      [ 0  1 ]       */
	    /*      [ 2  3 ]       */
	    DmB[iL - 1][limRange * iDer + kDer][0].r = factor1[0][0].r;
	    DmB[iL - 1][limRange * iDer + kDer][0].i = factor1[0][0].i;
	    DmB[iL - 1][limRange * iDer + kDer][1].r = factor1[0][1].r;
	    DmB[iL - 1][limRange * iDer + kDer][1].i = factor1[0][1].i;
	    DmB[iL - 1][limRange * iDer + kDer][2].r = factor1[1][0].r;
	    DmB[iL - 1][limRange * iDer + kDer][2].i = factor1[1][0].i;
	    DmB[iL - 1][limRange * iDer + kDer][3].r = factor1[1][1].r;
	    DmB[iL - 1][limRange * iDer + kDer][3].i = factor1[1][1].i;
	 }
	 kDer ++;
      }
   }
}
/*                                                              */
/*  Function matMult()                                          */
/*                                                              */
/*  Multiplication of two complex 2x2 matrices                  */
/*                                                              */
/*  Input parameters:                                           */
/*  A[2][2]................input matrix                         */
/*  B[2][2]................input matrix                         */
/*                                                              */
/*  Output parameters:                                          */
/*  C[2][2]................A * B                                */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void matMult(complex A[2][2], complex B[2][2], complex C[2][2])
{
      A[0][0].r = B[0][0].r * C[0][0].r - B[0][0].i * C[0][0].i
  	        + B[0][1].r * C[1][0].r - B[0][1].i * C[1][0].i;
      A[0][0].i = B[0][0].r * C[0][0].i + B[0][0].i * C[0][0].r
	        + B[0][1].r * C[1][0].i + B[0][1].i * C[1][0].r;
      A[0][1].r = B[0][0].r * C[0][1].r - B[0][0].i * C[0][1].i
	        + B[0][1].r * C[1][1].r - B[0][1].i * C[1][1].i;
      A[0][1].i = B[0][0].r * C[0][1].i + B[0][0].i * C[0][1].r
	        + B[0][1].r * C[1][1].i + B[0][1].i * C[1][1].r;
      A[1][0].r = B[1][0].r * C[0][0].r - B[1][0].i * C[0][0].i
                + B[1][1].r * C[1][0].r - B[1][1].i * C[1][0].i;
      A[1][0].i = B[1][0].r * C[0][0].i + B[1][0].i * C[0][0].r
      	        + B[1][1].r * C[1][0].i + B[1][1].i * C[1][0].r;
      A[1][1].r = B[1][0].r * C[0][1].r - B[1][0].i * C[0][1].i
      	        + B[1][1].r * C[1][1].r - B[1][1].i * C[1][1].i;
      A[1][1].i = B[1][0].r * C[0][1].i + B[1][0].i * C[0][1].r
      	        + B[1][1].r * C[1][1].i + B[1][1].i * C[1][1].r;
}
/*                                                              */
/*  Function frechetRTD()                                       */
/*                                                              */
/*  Numerical computation of the derivatives of the             */
/*  reflection and transmission coefficients of downgoing       */
/*  incident waves with respect to the model parameters         */
/*                                                              */
/*  Input parameters:                                           */
/*  i1.....................points to the first layer            */
/*  i2.....................points to the second layer           */
/*  iF.....................indicates if the derivatives are with*/
/*                         respect to the first or second layer */
/*                                                              */
/*  Output parameters:                                          */
/*  coeffDFr...............Derivatives of the elastic           */
/*                         reflection coefficients with respect */
/*                         to Vp, Vs and rho                    */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void frechetRTd(int i1, int i2, int iF)
{ 
   /* declaration of variables */
   int iDer;                       /* counter */
   float rho1=0, rho2=0;           /* densities */
   float rho1rho2;                 /* rho1 * rho2 */
   float DIV=0;                    /* derivative denominator */
   complex aux1, aux2, aux3, c;    
   complex cuu, caux3, aux1aux2;   /* auxiliar quantities */
   complex a1, a2, b1, b2;         /* all vertical slownesses */
   complex a1b1, a1b2, a2b1, a2b2; /* auxiliar quantities */
   complex a1a2b1b2;               /* auxiliar quantities */
   complex d1d, d2d;               /* auxiliar quantities */
   complex dpda, dpdb;             /* auxiliar quantities */
   complex PSlow1, PSlow2;         /* P-wave slownesses */
   complex SSlow1, SSlow2;         /* S-wave slownesses */
   complex S2Vel1, S2Vel2;         /* S-wave velocities */

   /* iDer indicates what parameter is active */
   /* bookkeeping the parameters */
   vpF = vpFrechet;
   vsF = vsFrechet;
   rhoF = rhoFrechet;

   for (iDer = 0; iDer < numberPar; iDer++)
   {
      /* Vp -> Vs -> rho, if all active */
      if (vpFrechet)
      {
	 SSlow1 = SSlowness[i1][0];
	 SSlow2 = SSlowness[i2][0];
	 S2Vel1 = S2Velocity[i1][0];
	 S2Vel2 = S2Velocity[i2][0];
	 rho1 = rho[i1];
	 rho2 = rho[i2];
	 if (iF == i1)
	 {
	    PSlow1 = PSlowness[i1][1];
	    PSlow2 = PSlowness[i2][0];
	    DIV = (FACTOR - 1.) * alpha[i1];
	 }
	 else if (iF == i2)
	 {
	    PSlow1 = PSlowness[i1][0];
	    PSlow2 = PSlowness[i2][1];
	    DIV = (FACTOR - 1.) * alpha[i2];
	 }
	 vpFrechet = 0;
      }
      else if (vsFrechet)
      {
	 PSlow1 = PSlowness[i1][0];
	 PSlow2 = PSlowness[i2][0];
	 rho1 = rho[i1];
	 rho2 = rho[i2];
	 if (iF == i1)
	 {
	    SSlow1 = SSlowness[i1][1];
	    SSlow2 = SSlowness[i2][0];
	    S2Vel1 = S2Velocity[i1][1];
	    S2Vel2 = S2Velocity[i2][0];
	    DIV = (FACTOR - 1.) * beta[i1];
	 }
	 else if (iF == i2)
	 {
	    SSlow1 = SSlowness[i1][0];
	    SSlow2 = SSlowness[i2][1];
	    S2Vel1 = S2Velocity[i1][0];
	    S2Vel2 = S2Velocity[i2][1];
	    DIV = (FACTOR - 1.) * beta[i2];
	 }
	 vsFrechet = 0;
      }
      else if (rhoFrechet)
      {
	 PSlow1 = PSlowness[i1][0];
	 PSlow2 = PSlowness[i2][0];
	 SSlow1 = SSlowness[i1][0];
	 SSlow2 = SSlowness[i2][0];
	 S2Vel1 = S2Velocity[i1][0];
	 S2Vel2 = S2Velocity[i2][0];
	 if (iF == i1)
	 {
	    rho1 = FACTOR * rho[i1];
	    rho2 = rho[i2];
	    DIV = (FACTOR - 1.) * rho[i1];
	 }
	 else if (iF == i2)
	 {
	    rho1 = rho[i1];
	    rho2 = FACTOR * rho[i2];
	    DIV = (FACTOR - 1.) * rho[i2];
	 }
	 rhoFrechet = 0;
      }
      
      /* square-root of PSlowness^2 - uuC */    
      auxm1 = PSlow1.r - uuC.r;
      auxm2 = PSlow1.i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      a1.r = auxm3 * cos(angle);
      a1.i = auxm3 * sin(angle);
      
      /* square-root of SSlowness^2 - uuC */    
      auxm1 = SSlow1.r - uuC.r;
      auxm2 = SSlow1.i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      b1.r = auxm3 * cos(angle);
      b1.i = auxm3 * sin(angle);
      
      /* square-root of PSlowness^2 - uuC */    
      auxm1 = PSlow2.r - uuC.r;
      auxm2 = PSlow2.i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      a2.r = auxm3 * cos(angle);
      a2.i = auxm3 * sin(angle);
      
      /* square-root of SSlowness^2 - uuC */    
      auxm1 = SSlow2.r - uuC.r;
      auxm2 = SSlow2.i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      b2.r = auxm3 * cos(angle);
      b2.i = auxm3 * sin(angle);
      
      rho1rho2 = rho1 * rho2;
      c.r = 2 * (rho1 * S2Vel1.r - rho2 * S2Vel2.r);
      c.i = 2 * (rho1 * S2Vel1.i - rho2 * S2Vel2.i);
      
      cuu.r = c.r * uuC.r - c.i * uuC.i;
      cuu.i = c.r * uuC.i + c.i * uuC.r;
      
      aux1.r = cuu.r - (rho1 - rho2);
      aux1.i = cuu.i;
      
      aux2.r = cuu.r + rho2;
      aux2.i = cuu.i;
      
      aux1aux2.r = aux1.r * aux2.r - aux1.i * aux2.i;
      aux1aux2.i = aux1.r * aux2.i + aux1.i * aux2.r;
      
      aux3.r = cuu.r - rho1;
      aux3.i = cuu.i;
      
      caux3.r = c.r * aux3.r - c.i * aux3.i;
      caux3.i = c.r * aux3.i + c.i * aux3.r;
      
      a1b1.r = a1.r * b1.r - a1.i * b1.i;
      a1b1.i = a1.r * b1.i + a1.i * b1.r;
      a1b2.r = a1.r * b2.r - a1.i * b2.i;
      a1b2.i = a1.r * b2.i + a1.i * b2.r;
      a2b1.r = a2.r * b1.r - a2.i * b1.i;
      a2b1.i = a2.r * b1.i + a2.i * b1.r;
      a2b2.r = a2.r * b2.r - a2.i * b2.i;
      a2b2.i = a2.r * b2.i + a2.i * b2.r;
      a1a2b1b2.r = a1b1.r * a2b2.r - a1b1.i * a2b2.i;
      a1a2b1b2.i = a1b1.r * a2b2.i + a1b1.i * a2b2.r;
      
      /* computing D factors */   
      auxm1 = aux3.r * aux3.r - aux3.i * aux3.i;
      auxm2 = 2 * aux3.r * aux3.i;
      auxm3 = a2b2.r * auxm1 - a2b2.i * auxm2;
      auxm4 = a2b2.r * auxm2 + a2b2.i * auxm1;
      d1d.r = auxm3 + a2b1.r * rho1rho2;
      d1d.i = auxm4 + a2b1.i * rho1rho2;
      
      auxm1 = aux1.r * aux1.r - aux1.i * aux1.i;
      auxm2 = 2 * aux1.r * aux1.i;
      auxm3 = auxm1 * uuC.r - auxm2 * uuC.i;
      auxm4 = auxm1 * uuC.i + auxm2 * uuC.r;
      d1d.r += auxm3;
      d1d.i += auxm4;
      
      auxm1 = aux2.r * aux2.r - aux2.i * aux2.i;
      auxm2 = 2 * aux2.r * aux2.i;
      auxm3 = a1b1.r * auxm1 - a1b1.i * auxm2;
      auxm4 = a1b1.r * auxm2 + a1b1.i * auxm1;
      d2d.r = auxm3 + a1b2.r * rho1rho2;
      d2d.i = auxm4 + a1b2.i * rho1rho2;
      
      auxm1 = c.r * cuu.r - c.i * cuu.i;
      auxm2 = c.r * cuu.i + c.i * cuu.r;
      auxm3 = a1a2b1b2.r * auxm1 - a1a2b1b2.i * auxm2;
      auxm4 = a1a2b1b2.r * auxm2 + a1a2b1b2.i * auxm1;
      d2d.r += auxm3;
      d2d.i += auxm4;
      
      /* more auxiliar quantities */
      /* d1d + d2d */
      dd.r = d1d.r + d2d.r;
      dd.i = d1d.i + d2d.i;
      
      /* 1 / dd */
      aux = dd.r * dd.r + dd.i * dd.i;
      dd.r = dd.r / aux;
      dd.i = -dd.i / aux;
      
      dpda.r = a1.r * dd.r - a1.i * dd.i;
      dpda.i = a1.r * dd.i + a1.i * dd.r;
      dpdb.r = b1.r * dd.r - b1.i * dd.i;
      dpdb.i = b1.r * dd.i + b1.i * dd.r;
      
      /* computing the coefficients - first reflection */
      auxm1 = d2d.r - d1d.r;
      auxm2 = d2d.i - d1d.i;
      /* (d2d - d1d) / (d1d + d2d) */
      coeffDFr[iDer][0].r = auxm1 * dd.r - auxm2 * dd.i;
      coeffDFr[iDer][0].i = auxm1 * dd.i + auxm2 * dd.r;     /* Rpp */

      /* computing the derivative */
      coeffDFr[iDer][0].r = (coeffDFr[iDer][0].r - coeffD[0].r) / DIV;
      coeffDFr[iDer][0].i = (coeffDFr[iDer][0].i - coeffD[0].i) / DIV;
      
      auxm1 = a2b2.r * caux3.r - a2b2.i * caux3.i;
      auxm2 = a2b2.r * caux3.i + a2b2.i * caux3.r;
      coeffDFr[iDer][1].r = aux1aux2.r + auxm1;
      coeffDFr[iDer][1].i = aux1aux2.i + auxm2;              /* Rsp */  
      
      coeffDFr[iDer][2].r = coeffDFr[iDer][1].r;
      coeffDFr[iDer][2].i = coeffDFr[iDer][1].i;             /* Rps */  
   
      auxm3 = dpdb.r * uC2.r - dpdb.i * uC2.i;
      auxm4 = dpdb.r * uC2.i + dpdb.i * uC2.r;
      
      aux = auxm3 * coeffDFr[iDer][1].r - auxm4 * coeffDFr[iDer][1].i;
      coeffDFr[iDer][1].i = auxm3 * coeffDFr[iDer][1].i + 
	                    auxm4 * coeffDFr[iDer][1].r;
      coeffDFr[iDer][1].r = aux;                             /* Rsp */
   
      /* computing the derivative */
      coeffDFr[iDer][1].r = (coeffDFr[iDer][1].r - coeffD[1].r) / DIV;
      coeffDFr[iDer][1].i = (coeffDFr[iDer][1].i - coeffD[1].i) / DIV;
      
      auxm3 = -dpda.r * uC2.r + dpda.i * uC2.i;
      auxm4 = -dpda.r * uC2.i - dpda.i * uC2.r;
      
      aux = auxm3 * coeffDFr[iDer][2].r - auxm4 * coeffDFr[iDer][2].i;
      coeffDFr[iDer][2].i = auxm3 * coeffDFr[iDer][2].i + 
	                    auxm4 * coeffDFr[iDer][2].r;
      coeffDFr[iDer][2].r = aux;                             /* Rps */
      
      /* computing the derivative */
      coeffDFr[iDer][2].r = (coeffDFr[iDer][2].r - coeffD[2].r) / DIV;
      coeffDFr[iDer][2].i = (coeffDFr[iDer][2].i - coeffD[2].i) / DIV;
      auxm1 = d2d.r - d1d.r;
      auxm2 = d2d.i - d1d.i;
      auxm3 = 2 * rho1rho2 * (a1b2.r - a2b1.r);
      auxm4 = 2 * rho1rho2 * (a1b2.i - a2b1.i);
      
      coeffDFr[iDer][3].r = auxm1 - auxm3;
      coeffDFr[iDer][3].i = auxm2 - auxm4;
      aux = coeffDFr[iDer][3].r * dd.r - coeffDFr[iDer][3].i * dd.i;
      coeffDFr[iDer][3].i = coeffDFr[iDer][3].r * dd.i + 
	                    coeffDFr[iDer][3].i * dd.r;
      coeffDFr[iDer][3].r = aux;                           /* Rss */
   
      /* computing the derivative */
      coeffDFr[iDer][3].r = (coeffDFr[iDer][3].r - coeffD[3].r) / DIV;
      coeffDFr[iDer][3].i = (coeffDFr[iDer][3].i - coeffD[3].i) / DIV;
   
      /* now transmition */
      auxm1 = b1.r * aux2.r - b1.i * aux2.i;
      auxm2 = b1.r * aux2.i + b1.i * aux2.r;
      auxm3 = b2.r * aux3.r - b2.i * aux3.i;
      auxm4 = b2.r * aux3.i + b2.i * aux3.r;
      coeffDFr[iDer][4].r = auxm1 - auxm3;     
      coeffDFr[iDer][4].i = auxm2 - auxm4;
      aux = 2 * rho1 * (dpda.r * coeffDFr[iDer][4].r - 
			dpda.i * coeffDFr[iDer][4].i);
      coeffDFr[iDer][4].i = 2 * rho1 * (dpda.r * coeffDFr[iDer][4].i + 
					dpda.i * coeffDFr[iDer][4].r);
      coeffDFr[iDer][4].r = aux;                             /* Tpp */
      
      /* computing the derivative */
      coeffDFr[iDer][4].r = (coeffDFr[iDer][4].r - coeffD[4].r) / DIV;
      coeffDFr[iDer][4].i = (coeffDFr[iDer][4].i - coeffD[4].i) / DIV;
      
      coeffDFr[iDer][5].r = aux1.r + a1b2.r * c.r - a1b2.i * c.i;
      coeffDFr[iDer][5].i = aux1.i + a1b2.r * c.i + a1b2.i * c.r;
      auxm1 = rho1 * (dpdb.r * uC2.r - dpdb.i * uC2.i);
      auxm2 = rho1 * (dpdb.r * uC2.i + dpdb.i * uC2.r);
      aux = coeffDFr[iDer][5].r * auxm1 - coeffDFr[iDer][5].i * auxm2;
      coeffDFr[iDer][5].i = coeffDFr[iDer][5].r * auxm2 + 
	                    coeffDFr[iDer][5].i * auxm1;   
      coeffDFr[iDer][5].r = aux;                             /* Tsp */
      
      /* computing the derivative */
      coeffDFr[iDer][5].r = (coeffDFr[iDer][5].r - coeffD[5].r) / DIV;
      coeffDFr[iDer][5].i = (coeffDFr[iDer][5].i - coeffD[5].i) / DIV;
      
      coeffDFr[iDer][6].r = aux1.r + a2b1.r * c.r - a2b1.i * c.i;
      coeffDFr[iDer][6].i = aux1.i + a2b1.r * c.i + a2b1.i * c.r;
      auxm1 = -rho1 * (dpda.r * uC2.r - dpda.i * uC2.i);
      auxm2 = -rho1 * (dpda.r * uC2.i + dpda.i * uC2.r);
      aux = coeffDFr[iDer][6].r * auxm1 - coeffDFr[iDer][6].i * auxm2;
      coeffDFr[iDer][6].i = coeffDFr[iDer][6].r * auxm2 + 
	                    coeffDFr[iDer][6].i * auxm1;  
      coeffDFr[iDer][6].r = aux;                             /* Tsp */
      
      /* computing the derivative */
      coeffDFr[iDer][6].r = (coeffDFr[iDer][6].r - coeffD[6].r) / DIV;
      coeffDFr[iDer][6].i = (coeffDFr[iDer][6].i - coeffD[6].i) / DIV;
      
      auxm1 = a1.r * aux2.r - a1.i * aux2.i;
      auxm2 = a1.r * aux2.i + a1.i * aux2.r;
      coeffDFr[iDer][7].r = auxm1 - (a2.r * aux3.r - a2.i * aux3.i);
      coeffDFr[iDer][7].i = auxm2 - (a2.r * aux3.i + a2.i * aux3.r);     
      auxm3 = 2 * rho1 * dpdb.r;
      auxm4 = 2 * rho1 * dpdb.i;
      aux = coeffDFr[iDer][7].r * auxm3 - coeffDFr[iDer][7].i * auxm4;
      coeffDFr[iDer][7].i = coeffDFr[iDer][7].r * auxm4 + 
	                    coeffDFr[iDer][7].i * auxm3; 
      coeffDFr[iDer][7].r = aux;                             /* Tss */
      
      /* computing the derivative */
      coeffDFr[iDer][7].r = (coeffDFr[iDer][7].r - coeffD[7].r) / DIV;
      coeffDFr[iDer][7].i = (coeffDFr[iDer][7].i - coeffD[7].i) / DIV;
   }
   /* restoring */
   vpFrechet = vpF;
   vsFrechet = vsF;
   rhoFrechet = rhoF;
}
/*                                                              */
/*  Function frechetRTu()                                       */
/*                                                              */
/*  Numerical computation of the derivatives of the             */
/*  reflection and transmission coefficients of upgoing         */
/*  incident waves with respect to the model parameters         */
/*                                                              */
/*  Input parameters:                                           */
/*  i1.....................points to the first layer            */
/*  i2.....................points to the second layer           */
/*  iF.....................indicates if te derivatives are with */
/*                         respect to the first or second layer */
/*                                                              */
/*  Output parameters:                                          */
/*  coeffUFr...............Derivatives of the elastic           */
/*                         reflection coefficients with respect */
/*                         to Vp, Vs and rho                    */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void frechetRTu(int i1, int i2, int iF)
{
   /* declaration of variables */
   int iDer;                       /* counter */
   float rho1=0, rho2=0;           /* densities */
   float rho1rho2;                 /* rho1 * rho2 */
   float DIV=0;                    /* derivative denominator */
   complex aux1, aux2, aux3, c;    
   complex cuu, caux2, aux1aux3;   /* auxiliar quantities */
   complex a1, a2, b1, b2;         /* all vertical slownesses */
   complex a1b1, a1b2, a2b1, a2b2; /* auxiliar quantities */
   complex a1a2b1b2;               /* auxiliar quantities */
   complex d1u, d2u;               /* auxiliar quantities */
   complex dpda, dpdb;             /* auxiliar quantities */
   complex PSlow1, PSlow2;         /* P-wave slownesses */
   complex SSlow1, SSlow2;         /* S-wave slownesses */
   complex S2Vel1, S2Vel2;         /* S-wave velocities */

   /* iDer indicates what parameter is active */
   /* bookkeeping the parameters */
   vpF = vpFrechet;
   vsF = vsFrechet;
   rhoF = rhoFrechet;
   for (iDer = 0; iDer < numberPar; iDer++)
   {
      /* vp -> vs -> rho, if all active */
      if (vpFrechet)
      {
	 SSlow1 = SSlowness[i1][0];
	 SSlow2 = SSlowness[i2][0];
	 S2Vel1 = S2Velocity[i1][0];
	 S2Vel2 = S2Velocity[i2][0];
	 rho1 = rho[i1];
	 rho2 = rho[i2];
	 if (iF == i1)
	 {
	    PSlow1 = PSlowness[i1][1];
	    PSlow2 = PSlowness[i2][0];
	    DIV = (FACTOR - 1.) * alpha[i1];
	 }
	 else if (iF == i2)
	 {
	    PSlow1 = PSlowness[i1][0];
	    PSlow2 = PSlowness[i2][1];
	    DIV = (FACTOR - 1.) * alpha[i2];
	 }
	 vpFrechet = 0;
      }
      else if (vsFrechet)
      {
	 PSlow1 = PSlowness[i1][0];
	 PSlow2 = PSlowness[i2][0];
	 rho1 = rho[i1];
	 rho2 = rho[i2];
	 if (iF == i1)
	 {
	    SSlow1 = SSlowness[i1][1];
	    SSlow2 = SSlowness[i2][0];
	    S2Vel1 = S2Velocity[i1][1];
	    S2Vel2 = S2Velocity[i2][0];
	    DIV = (FACTOR - 1.) * beta[i1];
	 }
	 else if (iF == i2)
	 {
	    SSlow1 = SSlowness[i1][0];
	    SSlow2 = SSlowness[i2][1];
	    S2Vel1 = S2Velocity[i1][0];
	    S2Vel2 = S2Velocity[i2][1];
	    DIV = (FACTOR - 1.) * beta[i2];
	 }
	 vsFrechet = 0;
      }
      else if (rhoFrechet)
      {
	 PSlow1 = PSlowness[i1][0];
	 PSlow2 = PSlowness[i2][0];
	 SSlow1 = SSlowness[i1][0];
	 SSlow2 = SSlowness[i2][0];
	 S2Vel1 = S2Velocity[i1][0];
	 S2Vel2 = S2Velocity[i2][0];
	 
	 if (iF == i1)
	 {
	    rho1 = FACTOR * rho[i1];
	    rho2 = rho[i2];
	    DIV = (FACTOR - 1.) * rho[i1];
	 }
	 else if (iF == i2)
	 {
	    rho1 = rho[i1];
	    rho2 = FACTOR * rho[i2];
	    DIV = (FACTOR - 1.) * rho[i2];
	 }
	 rhoFrechet = 0;
      }
      
      /* square-root of PSlowness^2 - uuC */    
      auxm1 = PSlow1.r - uuC.r;
      auxm2 = PSlow1.i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      a1.r = auxm3 * cos(angle);
      a1.i = auxm3 * sin(angle);
      
      /* square-root of SSlowness^2 - uuC */    
      auxm1 = SSlow1.r - uuC.r;
      auxm2 = SSlow1.i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      b1.r = auxm3 * cos(angle);
      b1.i = auxm3 * sin(angle);
      
      /* square-root of PSlowness^2 - uuC */    
      auxm1 = PSlow2.r - uuC.r;
      auxm2 = PSlow2.i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      a2.r = auxm3 * cos(angle);
      a2.i = auxm3 * sin(angle);
      
      /* square-root of SSlowness^2 - uuC */    
      auxm1 = SSlow2.r - uuC.r;
      auxm2 = SSlow2.i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      b2.r = auxm3 * cos(angle);
      b2.i = auxm3 * sin(angle);
      
      /* computing auxiliary quantities */
      rho1rho2 = rho1 * rho2;
      c.r = 2 * (rho1 * S2Vel1.r - rho2 * S2Vel2.r);
      c.i = 2 * (rho1 * S2Vel1.i - rho2 * S2Vel2.i);
      
      cuu.r = c.r * uuC.r - c.i * uuC.i;
      cuu.i = c.r * uuC.i + c.i * uuC.r;
      
      aux1.r = cuu.r - (rho1 - rho2);
      aux1.i = cuu.i;
      
      aux2.r = cuu.r + rho2;
      aux2.i = cuu.i;
      
      aux3.r = cuu.r - rho1;
      aux3.i = cuu.i;
      
      caux2.r = c.r * aux2.r - c.i * aux2.i;
      caux2.i = c.r * aux2.i + c.i * aux2.r;
      
      aux1aux3.r = aux1.r * aux3.r - aux1.i * aux3.i;
      aux1aux3.i = aux1.r * aux3.i + aux1.i * aux3.r;
      
      a1b1.r = a1.r * b1.r - a1.i * b1.i;
      a1b1.i = a1.r * b1.i + a1.i * b1.r;
      a1b2.r = a1.r * b2.r - a1.i * b2.i;
      a1b2.i = a1.r * b2.i + a1.i * b2.r;
      a2b1.r = a2.r * b1.r - a2.i * b1.i;
      a2b1.i = a2.r * b1.i + a2.i * b1.r;
      a2b2.r = a2.r * b2.r - a2.i * b2.i;
      a2b2.i = a2.r * b2.i + a2.i * b2.r;
      a1a2b1b2.r = a1b1.r * a2b2.r - a1b1.i * a2b2.i;
      a1a2b1b2.i = a1b1.r * a2b2.i + a1b1.i * a2b2.r;
      
      /* computing D factors */
      auxm1 = aux2.r * aux2.r - aux2.i * aux2.i;
      auxm2 = 2 * aux2.r * aux2.i;
      auxm3 = a1b1.r * auxm1 - a1b1.i * auxm2;
      auxm4 = a1b1.r * auxm2 + a1b1.i * auxm1;
      d1u.r = auxm3 + a1b2.r * rho1rho2;
      d1u.i = auxm4 + a1b2.i * rho1rho2;
      
      auxm1 = aux1.r * aux1.r - aux1.i * aux1.i;
      auxm2 = 2 * aux1.r * aux1.i;
      auxm3 = auxm1 * uuC.r - auxm2 * uuC.i;
      auxm4 = auxm1 * uuC.i + auxm2 * uuC.r;
      d1u.r += auxm3;
      d1u.i += auxm4;
      
      auxm1 = aux3.r * aux3.r - aux3.i * aux3.i;
      auxm2 = 2 * aux3.r * aux3.i;
      auxm3 = a2b2.r * auxm1 - a2b2.i * auxm2;
      auxm4 = a2b2.r * auxm2 + a2b2.i * auxm1;
      d2u.r = auxm3 + a2b1.r * rho1rho2;
      d2u.i = auxm4 + a2b1.i * rho1rho2;
      
      auxm1 = c.r * cuu.r - c.i * cuu.i;
      auxm2 = c.r * cuu.i + c.i * cuu.r;
      auxm3 = a1a2b1b2.r * auxm1 - a1a2b1b2.i * auxm2;
      auxm4 = a1a2b1b2.r * auxm2 + a1a2b1b2.i * auxm1;
      d2u.r += auxm3;
      d2u.i += auxm4;
      
      /* more auxiliar quantities */
      /* d1u + d2u */
      dd.r = d1u.r + d2u.r;
      dd.i = d1u.i + d2u.i;
      
      /* 1 / dd */
      aux = dd.r * dd.r + dd.i * dd.i;
      dd.r = dd.r / aux;
      dd.i = -dd.i / aux;
      
      dpda.r = a2.r * dd.r - a2.i * dd.i;
      dpda.i = a2.r * dd.i + a2.i * dd.r;
      dpdb.r = b2.r * dd.r - b2.i * dd.i;
      dpdb.i = b2.r * dd.i + b2.i * dd.r;
      
      /* computing the coefficients - first reflection */
      auxm1 = d2u.r - d1u.r;
      auxm2 = d2u.i - d1u.i;
      /* (d2u - d1u) / (d1u + d2u) */
      coeffUFr[iDer][0].r = auxm1 * dd.r - auxm2 * dd.i;
      coeffUFr[iDer][0].i = auxm1 * dd.i + auxm2 * dd.r;    /* Rpp */
      
      /* computing the derivative */
      coeffUFr[iDer][0].r = (coeffUFr[iDer][0].r - coeffU[0].r) / DIV;
      coeffUFr[iDer][0].i = (coeffUFr[iDer][0].i - coeffU[0].i) / DIV;
      
      auxm1 = a1b1.r * caux2.r - a1b1.i * caux2.i;
      auxm2 = a1b1.r * caux2.i + a1b1.i * caux2.r;
      coeffUFr[iDer][1].r = aux1aux3.r + auxm1;
      coeffUFr[iDer][1].i = aux1aux3.i + auxm2;             /* Rsp */  
      
      coeffUFr[iDer][2].r = coeffUFr[iDer][1].r;
      coeffUFr[iDer][2].i = coeffUFr[iDer][1].i;            /* Rps */  
      
      auxm3 = -dpdb.r * uC2.r + dpdb.i * uC2.i;
      auxm4 = -dpdb.r * uC2.i - dpdb.i * uC2.r;
      
      aux = auxm3 * coeffUFr[iDer][1].r - auxm4 * coeffUFr[iDer][1].i;
      coeffUFr[iDer][1].i = auxm3 * coeffUFr[iDer][1].i + 
	                    auxm4 * coeffUFr[iDer][1].r;
      coeffUFr[iDer][1].r = aux;                            /* Rsp */
   
      /* computing the derivative */
      coeffUFr[iDer][1].r = (coeffUFr[iDer][1].r - coeffU[1].r) / DIV;
      coeffUFr[iDer][1].i = (coeffUFr[iDer][1].i - coeffU[1].i) / DIV;

      auxm3 = dpda.r * uC2.r - dpda.i * uC2.i;
      auxm4 = dpda.r * uC2.i + dpda.i * uC2.r;
      
      aux = auxm3 * coeffUFr[iDer][2].r - auxm4 * coeffUFr[iDer][2].i;
      coeffUFr[iDer][2].i = auxm3 * coeffUFr[iDer][2].i + 
	                    auxm4 * coeffUFr[iDer][2].r;
      coeffUFr[iDer][2].r = aux;                           /* Rps */
      
      /* computing the derivative */
      coeffUFr[iDer][2].r = (coeffUFr[iDer][2].r - coeffU[2].r) / DIV;
      coeffUFr[iDer][2].i = (coeffUFr[iDer][2].i - coeffU[2].i) / DIV;
      auxm1 = d2u.r - d1u.r;
      auxm2 = d2u.i - d1u.i;
      auxm3 = 2 * rho1rho2 * (a2b1.r - a1b2.r);
      auxm4 = 2 * rho1rho2 * (a2b1.i - a1b2.i);
      
      coeffUFr[iDer][3].r = auxm1 - auxm3;
      coeffUFr[iDer][3].i = auxm2 - auxm4;
      aux = coeffUFr[iDer][3].r * dd.r - coeffUFr[iDer][3].i * dd.i;
      coeffUFr[iDer][3].i = coeffUFr[iDer][3].r * dd.i + 
	                    coeffUFr[iDer][3].i * dd.r;
      coeffUFr[iDer][3].r = aux;                          /* Rss */
      
      /* computing the derivative */
      coeffUFr[iDer][3].r = (coeffUFr[iDer][3].r - coeffU[3].r) / DIV;
      coeffUFr[iDer][3].i = (coeffUFr[iDer][3].i - coeffU[3].i) / DIV;
      /* now transmition */
      auxm1 = b1.r * aux2.r - b1.i * aux2.i;
      auxm2 = b1.r * aux2.i + b1.i * aux2.r;
      auxm3 = b2.r * aux3.r - b2.i * aux3.i;
      auxm4 = b2.r * aux3.i + b2.i * aux3.r;
      coeffUFr[iDer][4].r = auxm1 - auxm3;     
      coeffUFr[iDer][4].i = auxm2 - auxm4;
      aux = 2 * rho2 * (dpda.r * coeffUFr[iDer][4].r - 
			dpda.i * coeffUFr[iDer][4].i);
      coeffUFr[iDer][4].i = 2 * rho2 * (dpda.r * coeffUFr[iDer][4].i + 
					dpda.i * coeffUFr[iDer][4].r);
      coeffUFr[iDer][4].r = aux;                            /* Tpp */
      
      /* computing the derivative */
      coeffUFr[iDer][4].r = (coeffUFr[iDer][4].r - coeffU[4].r) / DIV;
      coeffUFr[iDer][4].i = (coeffUFr[iDer][4].i - coeffU[4].i) / DIV;
      
      coeffUFr[iDer][5].r = aux1.r + a2b1.r * c.r - a2b1.i * c.i;
      coeffUFr[iDer][5].i = aux1.i + a2b1.r * c.i + a2b1.i * c.r;
      auxm1 = rho2 * (dpdb.r * uC2.r - dpdb.i * uC2.i);
      auxm2 = rho2 * (dpdb.r * uC2.i + dpdb.i * uC2.r);
      aux = coeffUFr[iDer][5].r * auxm1 - coeffUFr[iDer][5].i * auxm2;
      coeffUFr[iDer][5].i = coeffUFr[iDer][5].r * auxm2 + 
	                    coeffUFr[iDer][5].i * auxm1;
      /* Tsp */
      coeffUFr[iDer][5].r = aux; 
      
      /* computing the derivative */
      coeffUFr[iDer][5].r = (coeffUFr[iDer][5].r - coeffU[5].r) / DIV;
      coeffUFr[iDer][5].i = (coeffUFr[iDer][5].i - coeffU[5].i) / DIV;
      
      coeffUFr[iDer][6].r = aux1.r + a1b2.r * c.r - a1b2.i * c.i;
      coeffUFr[iDer][6].i = aux1.i + a1b2.r * c.i + a1b2.i * c.r;
      auxm1 = -rho2 * (dpda.r * uC2.r - dpda.i * uC2.i);
      auxm2 = -rho2 * (dpda.r * uC2.i + dpda.i * uC2.r);
      aux = coeffUFr[iDer][6].r * auxm1 - coeffUFr[iDer][6].i * auxm2;
      coeffUFr[iDer][6].i = coeffUFr[iDer][6].r * auxm2 + 
	                    coeffUFr[iDer][6].i * auxm1;          
      /* Tsp */
      coeffUFr[iDer][6].r = aux;
      
      /* computing the derivative */
      coeffUFr[iDer][6].r = (coeffUFr[iDer][6].r - coeffU[6].r) / DIV;
      coeffUFr[iDer][6].i = (coeffUFr[iDer][6].i - coeffU[6].i) / DIV;
      
      auxm1 = a1.r * aux2.r - a1.i * aux2.i;
      auxm2 = a1.r * aux2.i + a1.i * aux2.r;
      coeffUFr[iDer][7].r = auxm1 - (a2.r * aux3.r - a2.i * aux3.i);
      coeffUFr[iDer][7].i = auxm2 - (a2.r * aux3.i + a2.i * aux3.r);     
      auxm3 = 2 * rho2 * dpdb.r;
      auxm4 = 2 * rho2 * dpdb.i;
      aux = coeffUFr[iDer][7].r * auxm3 - coeffUFr[iDer][7].i * auxm4;
      coeffUFr[iDer][7].i = coeffUFr[iDer][7].r * auxm4 + 
	                    coeffUFr[iDer][7].i * auxm3;   
      /* Tss */
      coeffUFr[iDer][7].r = aux;
      
      /* computing the derivative */
      coeffUFr[iDer][7].r = (coeffUFr[iDer][7].r - coeffU[7].r) / DIV;
      coeffUFr[iDer][7].i = (coeffUFr[iDer][7].i - coeffU[7].i) / DIV;
   }
   /* restoring */
   vpFrechet = vpF;
   vsFrechet = vsF;
   rhoFrechet = rhoF;
}
/*                                                              */
/*  Function horSlownessFrechet()                               */
/*                                                              */
/*  Computing the following parameters used in the              */
/*  Frechet derivatives of the reflectivity matrices            */
/*                                                              */
/*  1) P-wave slowness squared                                  */
/*  2) S-wave slowness squared                                  */
/*  3) S-wave velocity squared                                  */
/*                                  ...for all layers           */
/*                                                              */
/*  Input parameters:                                           */
/*  alpha..................p-wave velocities of the model       */
/*                         global variable                      */
/*  beta...................s-wave velocities of the model       */
/*                         global variable                      */
/*  rho....................densities of the elastic model       */
/*                         global variable                      */
/*  thick..................thicknesses of the model             */
/*                         global variable                      */
/*  wC.....................complex frequency                    */
/*                         global variable                      */
/*  uC.....................complex slowness                     */
/*                         global variable                      */
/*                                                              */
/*  Output parameters:                                          */
/*  PSlowness[0..2][nL].......p-wave slowness squared           */
/*  SSlowness[0..2][nL].......s-wave slowness squared           */
/*  S2Velocity[0..2][nL]......s-wave velocity squared           */
/*                                                              */
/*  Notice that those quantities are also computed for the      */
/*  disturbed model, and are used in the derivatives of the     */
/*  reflection coefficients                                     */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void horSlownessFrechet()
{
   /* declaration of variables */
   int iL, iF;                   /* counters */
   float cteQp1, cteQp2;
   float cteQs1, cteQs2;         /* constants used in absorption */
   float a, b;                   /* p and s-wave velocities */

   for (iF = 0; iF < 2; iF++)
   {
      for(iL = 0; iL <= nL; iL++)
      {
	 if (iF == 0)
	 {
	    a = alpha[iL];
	    b = beta[iL];
	 }
	 else
	 {
	    a = FACTOR * alpha[iL];
	    b = FACTOR * beta[iL];
	 }

	 /* constants used in absorption */
	 cteQp1 = a / (PI * qP[iL]);
	 cteQp2 = a / (2 * qP[iL]);
	 cteQs1 = b / (PI * qS[iL]);
	 cteQs2 = b / (2 * qS[iL]);

	 /* p-wave slowness squared */
	 PSlowness[iL][iF].r = a + cteQp1 * log(wCRwR);
	 PSlowness[iL][iF].i = cteQp2 - cteQp1 * wCP;

	 /* 1. / (PSlowness[iL] * PSlowness[iL]) */
	 auxm1 = PSlowness[iL][iF].r * PSlowness[iL][iF].r;
	 auxm2 = PSlowness[iL][iF].i * PSlowness[iL][iF].i;
	 aux = (auxm1 + auxm2) * (auxm1 + auxm2); aux = 1 / aux;
	 auxm3 = (auxm1 - auxm2) * aux;
	 PSlowness[iL][iF].i = -2 * PSlowness[iL][iF].r * 
	                            PSlowness[iL][iF].i * aux;
	 PSlowness[iL][iF].r = auxm3;
	 
	 /* computing an auxiliary quantity used in the gradient */
	 if (iF == 0)
	 {
	    derFactor[iL][0].r = PSlowness[iL][0].r / a;
	    derFactor[iL][0].i = PSlowness[iL][0].i / a;
	 }
      
	 /* s-wave velocity */
	 auxm1 = b + cteQs1 * log(wCRwR);
	 auxm2 = cteQs2 - cteQs1 * wCP;

	 /* S2Velocity[iL] * S2Velocity[iL] */
	 S2Velocity[iL][iF].r = auxm1 * auxm1 - auxm2 * auxm2;
	 S2Velocity[iL][iF].i = 2 * auxm1 * auxm2;

	 /* 1. / S2Velocity^2 */
	 aux = S2Velocity[iL][iF].r * S2Velocity[iL][iF].r + 
	       S2Velocity[iL][iF].i * S2Velocity[iL][iF].i;
	 SSlowness[iL][iF].r = S2Velocity[iL][iF].r / aux;
	 SSlowness[iL][iF].i = -S2Velocity[iL][iF].i / aux;

	 /* computing an auxiliary quantity used in the gradient */
	 if (iF == 0)
	 {
	    derFactor[iL][1].r = SSlowness[iL][0].r / b;
	    derFactor[iL][1].i = SSlowness[iL][0].i / b;
	 }

      }
   }
}
