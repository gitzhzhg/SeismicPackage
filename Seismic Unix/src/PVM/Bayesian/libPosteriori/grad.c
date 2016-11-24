/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*                                                              */
/*  Function gradient()                                         */
/*                                                              */
/*  Compute the FRECHET derivatives for the elastic             */
/*  modeling                                                    */
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
/*                                                              */
/*  Output parameters:                                          */
/*  F......................FRECHET derivative operator          */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
#include "posteriori.h"
void gradient()
{
   /* declaration of variables */
   int i, indexF, iF, iR, iU, iDer, iL, iT, iT1;
                                   /* counters */
   float f;                        /* temporal frequency */
   float w;                        /* radian frequency */
   float u;                        /* slowness */
   float cte;                      /* a constant */
   float *buffer;                  /* auxiliary buffer */
   complex dUCEp1, dUCEp2;         /* dUC * epslon1 and dUC * epslon2 */     
   complex wCCte;                  /* auxiliar variable */ 
   complex am;                     /* vertical P-wave slownesses */
   complex amInv;                  /* 1. / am */
   complex amI;                    /* amI = am * I */
   complex bm;                     /* vertical S-wave slownesses */
   complex bmInv;                  /* 1. / bm */
   complex bmI;                    /* bmI = bm * I */
   complex As1, As2;               /* amplitudes of plane wave components (P)*/
   complex Cs1, Cs2;               /* amplitudes of plane wave components (S)*/
                                   /* downgoing waves */
   complex Bs1, Bs2;               /* amplitudes of plane wave components (P)*/
   complex Ds1, Ds2;               /* amplitudes of plane wave components (S)*/
                                   /* upgoing waves */
   complex g[2];                   /* phase-shift vector */ 
   complex ***displ;               /* Frechet derivative of the */
                                   /* displacements in the frequency domain */
   complex dpl;                    /* auxiliary variable */
   
   /* allocating memory */
   displ = alloc3complex(nSamples / 2 + 1, nR, numberPar * limRange);
   buffer = alloc1float(nSamples);
   
   /* auxiliar constant */
   cte = 1. / (4 * PI * rho[0]);

   /* reseting displ */
   for (iDer = 0; iDer < numberPar * limRange; iDer++)
      for (iR = 0; iR < nR; iR++)
	 for (iF = 0; iF < nSamples / 2 + 1; iF++)
	    displ[iDer][iR][iF] = zeroC;
   
   for (indexF = NINT(f1 / dF), f = f1, iF = 0; iF < nF; iF++, 
	f += dF, indexF++)
   {
      fprintf(stderr,"FRECHET derivatives at frequency (Hz): %f\n", f);
      /* reseting */   
      for (i = 0; i < numberPar * limRange; i++)
      {
	 for (iR = 0; iR < nR; iR++)
	 {  
	    aux11[i][iR] = zeroC;	         aux12[i][iR] = zeroC;
	    aux21[i][iR] = zeroC;	         aux22[i][iR] = zeroC;
	    aux11Old[i][iR] = zeroC;	         aux12Old[i][iR] = zeroC;
	    aux21Old[i][iR] = zeroC;	         aux22Old[i][iR] = zeroC;
	 }
      }
      
      w = 2 * PI * f;
      wC.r = w; wC.i = -tau;
      
      /* module and phase of complex frequency */
      wCR = sqrt(wC.r * wC.r + wC.i * wC.i);
      wCP = atan2(wC.i, wC.r);

      /* complex slowness step */
      dUC.r = w * dU / wCR;
      dUC.i = tau * dU / wCR;

      /* wCR / wR */
      wCRwR = wCR / wR;
      
      /* auxiliary variable */
      wCCte.r = wC.r * cte;
      wCCte.i = wC.i * cte;

      /* compute frequency-dependent horizontal slownesses (squared) */
      /* and also the s-wave VELOCITIES (squared) for all layers */
      horSlownessFrechet();
      
      for (u = u1, iU = 0; iU < nU; iU++, 
	   u += dU, uC.r += dUC.r, uC.i += dUC.i)
      {
	 uC.r = u;
	 uC.i = u * tau / wRef;
	 
	 uC2.r = 2 * uC.r;
	 uC2.i = 2 * uC.i;
	 
	 aux = uC.r * uC.r - uC.i * uC.i;
	 uuC.i = 2 * uC.r * uC.i;
	 uuC.r = aux;

	 uuC2.r = 2 * uuC.r;
	 uuC2.i = 2 * uuC.i;
	 
	 muC.r = uC.r * -1;
	 muC.i = uC.i * -1;

	 /* building reflectivity matrices */
	 RmFrechet();
		Rp();
	 
	 /* reseting */
	 As1 = zeroC;      As2 = zeroC;      /* downgoing waves */
	 Cs1 = zeroC;      Cs2 = zeroC;      /* downgoing waves */
	 Bs1 = zeroC;      Bs2 = zeroC;      /* upgoing waves */
	 Ds1 = zeroC;      Ds2 = zeroC;      /* upgoing waves */
	    
	 /* P-wave potential */
	 /* PSlowness^2 - uuC */
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
	    
	 As1 = uC;
	 if (directWave) Bs1 = muC;
	    
	 /* 1 / am */
	 aux = am.r * am.r + am.i * am.i;
	 amInv.r = am.r / aux;
	 amInv.i = -am.i / aux;
	 
	 /* amInv * uuC */
	 aux2.r = amInv.r * uuC.r - uuC.i * amInv.i;
	 aux2.i = amInv.r * uuC.i + amInv.i * uuC.r;

	 /* aux2 * -I */
	 As2.r = aux2.i;
	 As2.i = -aux2.r;

	 /* notice that Bs2 = As2 */
	 if (directWave) Bs2 = As2;
	 
	 /* S-wave potential */
	 /* SSlowness^2 - uuC */
	 auxm1 = SSlowness[0][0].r - uuC.r;
	 auxm2 = SSlowness[0][0].i - uuC.i;
	    
	 /* computing bm */
	 auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
	 auxm3 = sqrt(auxm3);
	 angle = atan2(auxm2, auxm1) / 2;
	 bm.r = auxm3 * cos(angle);
	 bm.i = auxm3 * sin(angle);

	 /* bm * I */
	 bmI.r = -bm.i;
	 bmI.i = bm.r;

	 /* 1 / bm */
	 aux = bm.r * bm.r + bm.i * bm.i;
	 bmInv.r = bm.r / aux;
	 bmInv.i = -bm.i / aux;

	 /* 1. / bm * uuC */
	 aux1.r = bmInv.r * uuC.r - bmInv.i * uuC.i;
	 aux1.i = bmInv.r * uuC.i + bmInv.i * uuC.r;
	 
	 /* notice that Cs1 = Ds1 */
	 Cs1 = aux1;
	 if (directWave) Ds1 = aux1;

	 Cs2.r = -uC.i;
	 Cs2.i = uC.r;

	 if (directWave)
	 {
            Ds2.r = -Cs2.r;
	    Ds2.i = -Cs2.i;
         }
	 
	 /* computing compensation for free-surface */
	 buildFreeSurfaceCompensation(am, bm);

	 /* computing phase shift (that's the matrix G in Muller's */
	 /* paper eq. (87) */
	 /* exp(j * am * wC * (-zs)) */
	 auxm1 = zs * (- amI.r * wC.r + amI.i * wC.i);
	 auxm2 = -zs * (amI.r * wC.i + amI.i * wC.r);
	 g[0].r = exp(auxm1) * cos(auxm2);
	 g[0].i = exp(auxm1) * sin(auxm2);

	 /* exp(j * bm * wC * (-zs)) */
	 auxm1 = zs * (- bmI.r * wC.r + bmI.i * wC.i);
	 auxm2 = -zs * (bmI.r * wC.i + bmI.i * wC.r);
	 g[1].r = exp(auxm1) * cos(auxm2);
	 g[1].i = exp(auxm1) * sin(auxm2);
	    
	 /* computing the product I - R-R+ */
	 auxm1 = rm[0][0].r * rp[0][0].r - rm[0][0].i * rp[0][0].i;
	 auxm2 = rm[0][0].r * rp[0][0].i + rm[0][0].i * rp[0][0].r;
	 auxm3 = rm[0][1].r * rp[1][0].r - rm[0][1].i * rp[1][0].i;
	 auxm4 = rm[0][1].r * rp[1][0].i + rm[0][1].i * rp[1][0].r;
         irr[0][0].r = 1 - (auxm1 + auxm3);
         irr[0][0].i = - (auxm2 + auxm4);

	 auxm1 = rm[0][0].r * rp[0][1].r - rm[0][0].i * rp[0][1].i;
	 auxm2 = rm[0][0].r * rp[0][1].i + rm[0][0].i * rp[0][1].r;
	 auxm3 = rm[0][1].r * rp[1][1].r - rm[0][1].i * rp[1][1].i;
	 auxm4 = rm[0][1].r * rp[1][1].i + rm[0][1].i * rp[1][1].r;
         irr[0][1].r = - (auxm1 + auxm3);
         irr[0][1].i = - (auxm2 + auxm4);

	 auxm1 = rm[1][0].r * rp[0][0].r - rm[1][0].i * rp[0][0].i;
	 auxm2 = rm[1][0].r * rp[0][0].i + rm[1][0].i * rp[0][0].r;
	 auxm3 = rm[1][1].r * rp[1][0].r - rm[1][1].i * rp[1][0].i;
	 auxm4 = rm[1][1].r * rp[1][0].i + rm[1][1].i * rp[1][0].r;
         irr[1][0].r = - (auxm1 + auxm3);
         irr[1][0].i = - (auxm2 + auxm4);

	 auxm1 = rm[1][0].r * rp[0][1].r - rm[1][0].i * rp[0][1].i;
	 auxm2 = rm[1][0].r * rp[0][1].i + rm[1][0].i * rp[0][1].r;
	 auxm3 = rm[1][1].r * rp[1][1].r - rm[1][1].i * rp[1][1].i;
	 auxm4 = rm[1][1].r * rp[1][1].i + rm[1][1].i * rp[1][1].r;
         irr[1][1].r = 1 - (auxm1 + auxm3);
         irr[1][1].i = - (auxm2 + auxm4);

	 /* inverting irr explicitly */
	 auxm1 = irr[0][0].r * irr[1][1].r - irr[0][0].i * irr[1][1].i;
	 auxm2 = irr[0][0].r * irr[1][1].i + irr[0][0].i * irr[1][1].r;
	 auxm3 = irr[0][1].r * irr[1][0].r - irr[0][1].i * irr[1][0].i;
	 auxm4 = irr[0][1].r * irr[1][0].i + irr[0][1].i * irr[1][0].r;
	 aux1.r = auxm1 - auxm3;
	 aux1.i = auxm2 - auxm4;

	 /* 1 / aux1 */
	 aux = aux1.r * aux1.r + aux1.i * aux1.i;
	 aux1.r = aux1.r / aux;
	 aux1.i = -aux1.i / aux;

	 /* Inverse of irr */
	 irrI[0][0].r = irr[1][1].r * aux1.r - irr[1][1].i * aux1.i;
	 irrI[0][0].i = irr[1][1].r * aux1.i + irr[1][1].i * aux1.r;
	 
	 irrI[0][1].r = -(irr[0][1].r * aux1.r - irr[0][1].i * aux1.i);
	 irrI[0][1].i = -(irr[0][1].r * aux1.i + irr[0][1].i * aux1.r);
	 
	 irrI[1][0].r = -(irr[1][0].r * aux1.r - irr[1][0].i * aux1.i);
	 irrI[1][0].i = -(irr[1][0].r * aux1.i + irr[1][0].i * aux1.r);

	 irrI[1][1].r = irr[0][0].r * aux1.r - irr[0][0].i * aux1.i;
	 irrI[1][1].i = irr[0][0].r * aux1.i + irr[0][0].i * aux1.r;

	 /* computing vectors V1,2, check eq (76) Muller's paper */
	 auxm1 = As1.r * rm[0][0].r - As1.i * rm[0][0].i;
	 auxm2 = As1.r * rm[0][0].i + As1.i * rm[0][0].r;
	 auxm3 = Cs1.r * rm[0][1].r - Cs1.i * rm[0][1].i;
	 auxm4 = Cs1.r * rm[0][1].i + Cs1.i * rm[0][1].r;
	 aux1.r = Bs1.r + (auxm1 + auxm3);
	 aux1.i = Bs1.i + (auxm2 + auxm4);

	 auxm1 = As1.r * rm[1][0].r - As1.i * rm[1][0].i;
	 auxm2 = As1.r * rm[1][0].i + As1.i * rm[1][0].r;
	 auxm3 = Cs1.r * rm[1][1].r - Cs1.i * rm[1][1].i;
	 auxm4 = Cs1.r * rm[1][1].i + Cs1.i * rm[1][1].r;
	 aux2.r = Ds1.r + (auxm1 + auxm3);
	 aux2.i = Ds1.i + (auxm2 + auxm4);

	 auxm1 = aux1.r * irrI[0][0].r - aux1.i * irrI[0][0].i;
	 auxm2 = aux1.r * irrI[0][0].i + aux1.i * irrI[0][0].r;
	 auxm3 = aux2.r * irrI[0][1].r - aux2.i * irrI[0][1].i;
	 auxm4 = aux2.r * irrI[0][1].i + aux2.i * irrI[0][1].r;
	 v1[0][0].r = auxm1 + auxm3;
	 v1[0][0].i = auxm2 + auxm4;

	 auxm1 = aux1.r * irrI[1][0].r - aux1.i * irrI[1][0].i;
	 auxm2 = aux1.r * irrI[1][0].i + aux1.i * irrI[1][0].r;
	 auxm3 = aux2.r * irrI[1][1].r - aux2.i * irrI[1][1].i;
	 auxm4 = aux2.r * irrI[1][1].i + aux2.i * irrI[1][1].r;
	 v1[0][1].r = auxm1 + auxm3;
	 v1[0][1].i = auxm2 + auxm4;

	 /* loop over "active" layers */
	 for (iDer = 1, i = 0; i < numberPar; i++)
	 {
	    /* i = 0 -> Vp  */
	    /* i = 1 -> Vs  */
	    /* i = 2 -> rho */
	    for (iL = MIN(lim[0], 2); iL < MIN(lim[0], 2) + limRange; 
		 iL++, iDer++)
	    {
	       /* rp * [v1[0], v1[1]] + (As1, Cs1)*/
	       auxm1 = rp[0][0].r * v1[0][0].r - rp[0][0].i * v1[0][0].i;
	       auxm2 = rp[0][0].r * v1[0][0].i + rp[0][0].i * v1[0][0].r;
	       auxm1 += rp[0][1].r * v1[0][1].r - rp[0][1].i * v1[0][1].i 
		     + As1.r;
	       auxm2 += rp[0][1].r * v1[0][1].i + rp[0][1].i * v1[0][1].r 
		     + As1.i;
	    
	       auxm3 = rp[1][0].r * v1[0][0].r - rp[1][0].i * v1[0][0].i;
	       auxm4 = rp[1][0].r * v1[0][0].i + rp[1][0].i * v1[0][0].r;
	       auxm3 += rp[1][1].r * v1[0][1].r - rp[1][1].i * v1[0][1].i 
		     + Cs1.r;
	       auxm4 += rp[1][1].r * v1[0][1].i + rp[1][1].i * v1[0][1].r 
		     + Cs1.i;

	       /* DmB[0][active layers][0 1 2 3] * */
	       /*                      ((auxm1, auxm2), (auxm3, auxm4)) */
	       aux1.r = auxm1 * DmB[0][i * limRange + iL][0].r 
		      - auxm2 * DmB[0][i * limRange + iL][0].i 
 	 	      + auxm3 * DmB[0][i * limRange + iL][1].r 
		      - auxm4 * DmB[0][i * limRange + iL][1].i;
	       aux1.i = auxm1 * DmB[0][i * limRange + iL][0].i 
		      + auxm2 * DmB[0][i * limRange + iL][0].r
		      + auxm3 * DmB[0][i * limRange + iL][1].i 
		      + auxm4 * DmB[0][i * limRange + iL][1].r;
	       
	       aux2.r = auxm1 * DmB[0][i * limRange + iL][2].r 
		      - auxm2 * DmB[0][i * limRange + iL][2].i 
		      + auxm3 * DmB[0][i * limRange + iL][3].r 
		      - auxm4 * DmB[0][i * limRange + iL][3].i;
	       aux2.i = auxm1 * DmB[0][i * limRange + iL][2].i 
		      + auxm2 * DmB[0][i * limRange + iL][2].r  
		      + auxm3 * DmB[0][i * limRange + iL][3].i 
		      + auxm4 * DmB[0][i * limRange + iL][3].r;
	       
	       /* irrI * (aux1, aux2) */
	       auxm1 = irrI[0][0].r * aux1.r - irrI[0][0].i * aux1.i;
	       auxm2 = irrI[0][0].r * aux1.i + irrI[0][0].i * aux1.r;
	       auxm3 = irrI[0][1].r * aux2.r - irrI[0][1].i * aux2.i;
	       auxm4 = irrI[0][1].r * aux2.i + irrI[0][1].i * aux2.r;
	       v1[iDer][0].r = auxm1 + auxm3;
	       v1[iDer][0].i = auxm2 + auxm4;

	       auxm1 = irrI[1][0].r * aux1.r - irrI[1][0].i * aux1.i;
	       auxm2 = irrI[1][0].r * aux1.i + irrI[1][0].i * aux1.r;
	       auxm3 = irrI[1][1].r * aux2.r - irrI[1][1].i * aux2.i;
	       auxm4 = irrI[1][1].r * aux2.i + irrI[1][1].i * aux2.r;
	       v1[iDer][1].r = auxm1 + auxm3;
	       v1[iDer][1].i = auxm2 + auxm4;
	    }
	 }

	 auxm1 = As2.r * rm[0][0].r - As2.i * rm[0][0].i;
	 auxm2 = As2.r * rm[0][0].i + As2.i * rm[0][0].r;
	 auxm3 = Cs2.r * rm[0][1].r - Cs2.i * rm[0][1].i;
	 auxm4 = Cs2.r * rm[0][1].i + Cs2.i * rm[0][1].r;
	 aux1.r = Bs2.r + (auxm1 + auxm3);
	 aux1.i = Bs2.i + (auxm2 + auxm4);

	 auxm1 = As2.r * rm[1][0].r - As2.i * rm[1][0].i;
	 auxm2 = As2.r * rm[1][0].i + As2.i * rm[1][0].r;
	 auxm3 = Cs2.r * rm[1][1].r - Cs2.i * rm[1][1].i;
	 auxm4 = Cs2.r * rm[1][1].i + Cs2.i * rm[1][1].r;
	 aux2.r = Ds2.r + (auxm1 + auxm3);
	 aux2.i = Ds2.i + (auxm2 + auxm4);

	 auxm1 = aux1.r * irrI[0][0].r - aux1.i * irrI[0][0].i;
	 auxm2 = aux1.r * irrI[0][0].i + aux1.i * irrI[0][0].r;
	 auxm3 = aux2.r * irrI[0][1].r - aux2.i * irrI[0][1].i;
	 auxm4 = aux2.r * irrI[0][1].i + aux2.i * irrI[0][1].r;
	 v2[0][0].r = auxm1 + auxm3;
	 v2[0][0].i = auxm2 + auxm4;

	 auxm1 = aux1.r * irrI[1][0].r - aux1.i * irrI[1][0].i;
	 auxm2 = aux1.r * irrI[1][0].i + aux1.i * irrI[1][0].r;
	 auxm3 = aux2.r * irrI[1][1].r - aux2.i * irrI[1][1].i;
	 auxm4 = aux2.r * irrI[1][1].i + aux2.i * irrI[1][1].r;
	 v2[0][1].r = auxm1 + auxm3;
	 v2[0][1].i = auxm2 + auxm4;

	 /* loop over "active" layers */
	 for (iDer = 1, i = 0; i < numberPar; i++)
	 {
	    /* i = 0 -> Vp  */
	    /* i = 1 -> Vs  */
	    /* i = 2 -> rho */
	    for (iL = MIN(lim[0], 2); iL < MIN(lim[0], 2) + limRange; 
		 iL++, iDer++)
	    {
	       /* rp * [v2[0], v2[1]] + (As2, Bs2) */
	       auxm1 = rp[0][0].r * v2[0][0].r - rp[0][0].i * v2[0][0].i;
	       auxm2 = rp[0][0].r * v2[0][0].i + rp[0][0].i * v2[0][0].r;
	       auxm1 += rp[0][1].r * v2[0][1].r - rp[0][1].i * v2[0][1].i 
		     + As2.r;
	       auxm2 += rp[0][1].r * v2[0][1].i + rp[0][1].i * v2[0][1].r 
		     + As2.i;

	       auxm3 = rp[1][0].r * v2[0][0].r - rp[1][0].i * v2[0][0].i;
	       auxm4 = rp[1][0].r * v2[0][0].i + rp[1][0].i * v2[0][0].r;
	       auxm3 += rp[1][1].r * v2[0][1].r - rp[1][1].i * v2[0][1].i 
		     + Cs2.r;
	       auxm4 += rp[1][1].r * v2[0][1].i + rp[1][1].i * v2[0][1].r 
		     + Cs2.i;

	       /* DmB[0][active layers][0 1 2 3] * */
	       /*                      ((auxm1, auxm2), (auxm3, auxm4)) */
	       aux1.r = auxm1 * DmB[0][i * limRange + iL][0].r 
	 	      - auxm2 * DmB[0][i * limRange + iL][0].i 
		      + auxm3 * DmB[0][i * limRange + iL][1].r 
		      - auxm4 * DmB[0][i * limRange + iL][1].i;
	       aux1.i = auxm1 * DmB[0][i * limRange + iL][0].i 
		      + auxm2 * DmB[0][i * limRange + iL][0].r 
		      + auxm3 * DmB[0][i * limRange + iL][1].i 
		      + auxm4 * DmB[0][i * limRange + iL][1].r;
	       
	       aux2.r = auxm1 * DmB[0][i * limRange + iL][2].r 
		      - auxm2 * DmB[0][i * limRange + iL][2].i  
		      + auxm3 * DmB[0][i * limRange + iL][3].r 
		      - auxm4 * DmB[0][i * limRange + iL][3].i;
	       aux2.i = auxm1 * DmB[0][i * limRange + iL][2].i 
		      + auxm2 * DmB[0][i * limRange + iL][2].r 
		      + auxm3 * DmB[0][i * limRange + iL][3].i 
		      + auxm4 * DmB[0][i * limRange + iL][3].r;
	       
	       /* irrI * (aux1, aux2) */
	       auxm1 = irrI[0][0].r * aux1.r - irrI[0][0].i * aux1.i;
	       auxm2 = irrI[0][0].r * aux1.i + irrI[0][0].i * aux1.r;
	       auxm3 = irrI[0][1].r * aux2.r - irrI[0][1].i * aux2.i;
	       auxm4 = irrI[0][1].r * aux2.i + irrI[0][1].i * aux2.r;
	       v2[iDer][0].r = auxm1 + auxm3;
	       v2[iDer][0].i = auxm2 + auxm4;
	       
	       auxm1 = irrI[1][0].r * aux1.r - irrI[1][0].i * aux1.i;
	       auxm2 = irrI[1][0].r * aux1.i + irrI[1][0].i * aux1.r;
	       auxm3 = irrI[1][1].r * aux2.r - irrI[1][1].i * aux2.i;
	       auxm4 = irrI[1][1].r * aux2.i + irrI[1][1].i * aux2.r;
	       v2[iDer][1].r = auxm1 + auxm3;
	       v2[iDer][1].i = auxm2 + auxm4;
	    }
	 }
	 
	 /* applying phase-shift to FRECHET derivatives */
	 /* loop over "active" layers */
	 for (iDer = 1; iDer <= numberPar * limRange; iDer++)
	 {
	    aux = v1[iDer][0].r * g[0].r - v1[iDer][0].i * g[0].i;
	    v1[iDer][0].i = v1[iDer][0].r * g[0].i + 
		            v1[iDer][0].i * g[0].r;
	    v1[iDer][0].r = aux;

	    aux = v1[iDer][1].r * g[1].r - v1[iDer][1].i * g[1].i;
	    v1[iDer][1].i = v1[iDer][1].r * g[1].i + 
	                    v1[iDer][1].i * g[1].r;
	    v1[iDer][1].r = aux;

	    aux = v2[iDer][0].r * g[0].r - v2[iDer][0].i * g[0].i;
	    v2[iDer][0].i = v2[iDer][0].r * g[0].i + 
	                    v2[iDer][0].i * g[0].r;
	    v2[iDer][0].r = aux;
	    
	    aux = v2[iDer][1].r * g[1].r - v2[iDer][1].i * g[1].i;
	    v2[iDer][1].i = v2[iDer][1].r * g[1].i + 
	                    v2[iDer][1].i * g[1].r;
	    v2[iDer][1].r = aux;
	 }

	 /* compensating for free surface */
	 freeSurfaceFrechet(v1, v2);
	 
	 /* loop over offsets for computing the displacements */
	 displacementsFrechet(iU);
      }
      /* displacements in the radial or vertical direction */
      /* (frequency domain) */
      /* there's a 2 (free surface) / 2 (trapezoidal integration) */
      /* simplified in the equation below */
      dUCEp1.r = epslon1 * dUC.r;
      dUCEp1.i = epslon1 * dUC.i;
      dUCEp2.r = epslon2 * dUC.r;
      dUCEp2.i = epslon2 * dUC.i;

      /* loop over "active" layers */
      for (iDer = 0; iDer < numberPar * limRange; iDer++)
      {
	 /* loop over offsets */
	 for (iR = 0; iR < nR; iR++)
	 {
	    /* radial ? */
	    if (RADIAL)
	    {
	       auxm1 = aux11[iDer][iR].r * dUCEp1.r - 
		       aux11[iDer][iR].i * dUCEp1.i;
	       auxm2 = aux11[iDer][iR].r * dUCEp1.i + 
		       aux11[iDer][iR].i * dUCEp1.r;
	       auxm3 = aux21[iDer][iR].r * dUCEp2.r - 
	               aux21[iDer][iR].i * dUCEp2.i;
	       auxm4 = aux21[iDer][iR].r * dUCEp2.i + 
	               aux21[iDer][iR].i * dUCEp2.r;
	 
	       dpl.i = (auxm1 + auxm3) * wCCte.r - (auxm2 + auxm4) * wCCte.i;
	       dpl.i = (auxm1 + auxm3) * wCCte.i + (auxm2 + auxm4) * wCCte.r;

	       /* filtering */
	       dpl.r *= window[indexF] * SGN(recArray[iR]);
	       dpl.i *= window[indexF] * SGN(recArray[iR]);
	    }
	    
	    if (VERTICAL)
	    {
	       auxm1 = aux12[iDer][iR].r * dUCEp1.r - 
		       aux12[iDer][iR].i * dUCEp1.i;
	       auxm2 = aux12[iDer][iR].r * dUCEp1.i + 
		       aux12[iDer][iR].i * dUCEp1.r;
	       auxm3 = aux22[iDer][iR].r * dUCEp2.r - 
	               aux22[iDer][iR].i * dUCEp2.i;
	       auxm4 = aux22[iDer][iR].r * dUCEp2.i + 
	               aux22[iDer][iR].i * dUCEp2.r;

	       dpl.r = (auxm1 + auxm3) * wCCte.r - (auxm2 + auxm4) * wCCte.i;
	       dpl.i = (auxm1 + auxm3) * wCCte.i + (auxm2 + auxm4) * wCCte.r;

	       /* filtering */
	       dpl.r *= window[indexF];
	       dpl.i *= window[indexF];
	    }
	    
	    /* storing displacements in matrix displ */
	    displ[iDer][iR][indexF] = dpl;
	 }
      }
   }

   /* going to time domain and correctig for tau */
   for (iDer = 0; iDer < numberPar; iDer++)
   {
      for (iL = 0; iL < limRange; iL++)
      {
	 for (iR = 0; iR < nR; iR++)
 	 {
	    pfacr(1, nSamples, displ[iDer * limRange + iL][iR], buffer);
	   
	    /* correcting for tau */
	    for (iT = 0; iT < nSamples; iT++)
	    {
	       buffer[iT] *= exp(tau * iT * dt);
	    }
 
	    /* copying to operator F */
	    iT1 = NINT(t1 / dt);
	    for (iT = 0; iT < nDM; iT++)
	    {
	       if (IMPEDANCE && vpFrechet && iDer == 0)
	       {
		  F[iDer * limRange + iL][iR * nDM + iT] = 
  	          buffer[iT1 + iT] / rho[iL + lim[0]];
	       }
	       else if (IMPEDANCE && vsFrechet && (iDer == 0 || iDer == 1))
	       {
		  F[iDer * limRange + iL][iR * nDM + iT] = 
		  buffer[iT1 + iT] / rho[iL + lim[0]];
	       }
	       else if (IMPEDANCE && rhoFrechet && iDer == 2)
	       {
		  F[iDer * limRange + iL][iR * nDM + iT] =
		  - alpha[iL + lim[0]] * F[iL][iR * nDM + iT] 
		  - beta[iL + lim[0]] * F[iL + limRange][iR * nDM + iT] 
  	          + buffer[iT1 + iT];
	       }
	       else if (!IMPEDANCE)
	       {
		  F[iDer * limRange + iL][iR * nDM + iT] = buffer[iT1 + iT] ;
	       }
	    }
	 }
      }
   }
   
   /* if in the IMPEDANCE domain rearrange matrix F */
   if (IMPEDANCE)
   {
      if (rhoFrechet && !ipFrechet && !isFrechet)
      {
	 for (iL = 0; iL < limRange; iL++)
	 {
	    for (iR = 0; iR < nR; iR++)
	    {
	       for (iT = 0; iT < nDM; iT++)
	       {
		  F[iL][iR * nDM + iT] = F[iL + 2 * limRange][iR * nDM + iT];
	       }
	    }
	 }
      }
      else if (rhoFrechet && ipFrechet && !isFrechet)
      {
	 for (iL = 0; iL < limRange; iL++)
	 {
	    for (iR = 0; iR < nR; iR++)
	    {
	       for (iT = 0; iT < nDM; iT++)
	       {
		  F[iL + limRange][iR * nDM + iT] = 
		     F[iL + 2 * limRange][iR * nDM + iT];
	       }
	    }
	 }
      }
      else if (rhoFrechet && !ipFrechet && isFrechet)   
      {
	 for (iL = 0; iL < limRange; iL++)
	 {
	    for (iR = 0; iR < nR; iR++)
	    {
	       for (iT = 0; iT < nDM; iT++)
	       {
		  F[iL][iR * nDM + iT] = F[iL + limRange][iR * nDM + iT];
		  F[iL + limRange][iR * nDM + iT] = 
		     F[iL + 2 * limRange][iR * nDM + iT];
	       }
	    }
	 }
      }
   }
   
   /* freeing memory */
   free3complex(displ);
   free1float(buffer);
}
