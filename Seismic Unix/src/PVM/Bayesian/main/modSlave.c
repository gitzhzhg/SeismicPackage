/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "modSlave.h"

void
main ()
{
   /* declaration of variables */
   
   FILE *fp;                     /* file pointer for receiver array */
   char *recFile = " ";          /* file with receiver coordinates */
   int indexF, iU, iF, iR, i;    /* counters */
   int masterId;                 /* master id */
   int die;                      /* if die=1 process stops */
   int nU;                       /* number of slownesses */
   int nR;                       /* number of receivers */
   int wL;                       /* taper length */
   int wInfo[2];                 /* frequency limits */
   int nFreqProc;                /* number of frequencies per processor */
   int directWave;               /* direct wave flag */
   int VERTICAL, RADIAL;         /* specifies geophone orientation */
   int verbose;                  /* dialogue flag */
   float tau;                    /* magnitude of wrap-around attenuation */
   float percU;                  /* percentual of slowness */
                                 /* domain that are windowed */
   float percW;                  /* percentual of frequency */
                                 /* domain that are windowed */
   float r1, dR;                 /* defines receiver array */
   float u1, u2;                 /* slowness window */
   float u, w, f;                /* slowness & frequency */
   float phi;                    /* azimuth angle */
   float F1, F2, F3;             /* source components */
   float epslon1, epslon2;       /* auxiliary quantities */ 
   float cte;                    /* = 4 PI rho */
   float *taper;                 /* taper for slowness domain */
   float arg;                    /* argument of the Bessel function */
   float dU;                     /* slowness interval */
   float wRef;                   /* used in complex slowness */
   float *recArray;              /* array with receiver coordinates */
   complex dUC;                  /* complex slowness interval */
   complex v1A, v2A;             /* auxiliary values */
   complex dUCEp1, dUCEp2;       /* dUC * epslon1 and dUC * epslon2 */
   complex muC;                  /* uC * -1 */
   complex wCCte;                /* auxiliar variable */
   complex am;                   /* vertical P-wave slownesses */
   complex amInv;                /* 1. / am */
   complex amI;                  /* amI = am * I */
   complex bm;                   /* vertical S-wave slownesses */
   complex bmInv;                /* 1. / bm */
   complex bmI;                  /* bmI = bm * I */
   complex ambm;                 /* - am * bm */
   complex As1, As2;             /* amplitudes of plane wave components (P)*/
   complex Cs1, Cs2;             /* amplitudes of plane wave components (S)*/
                                 /* downgoing waves */
   complex Bs1, Bs2;             /* amplitudes of plane wave components (P)*/
   complex Ds1, Ds2;             /* amplitudes of plane wave components (S)*/
                                 /* upgoing waves */
   complex irr[2][2];            /* = (I - R-R+) */
   complex irrI[2][2];           /* = (I - R-R+)^-1 */
   complex v1[2], v2[2];         /* potential vectors */
   complex g[2];                 /* phase-shift vector */
   complex h[2][2];              /* free-surface matrix */
   complex **uW;                 /* displacements in the vertical or radial */
                                 /* direction in the frequency domain */
   complex *aux11, *aux12, *aux21, *aux22;
   complex *aux11Old, *aux12Old, *aux21Old, *aux22Old;
   register complex aux1, aux2, aux3;     /* auxiliar quantities */
   INFO info[1];                 /* basic information for slaves */
   SeisSlave logInfo;            /* report structure */     

   /* logging information */
   InitLog(&logInfo, PROCESS_MODELING);

   /* receiving INFO data structure */
   masterId = RecvINFO(info, 1, -1, GENERAL_INFORMATION);

   recFile = info->recFile;
   directWave = info->directWave;
   nL = info->nL;
   nF = info->nF;
   nR = info->nR;
   nU = info->nU;
   hanningFlag = info->hanningFlag;
   nSamples = info->nSamples;
   nFreqProc = info->nFreqProc;
   VERTICAL = info->vertical;
   RADIAL = info->radial;
   r1 = info->r1;
   dR = info->dR;
   zs = info->zs;
   u1 = info->u1;
   u2 = info->u2;
   dU = info->dU;
   f1 = info->f1;
   f2 = info->f2;
   dF = info->dF;
   F1 = info->F1;
   F2 = info->F2;
   F3 = info->F3;
   percU = info->percU;
   percW = info->percW;
   wR = info->wR;
   tau = info->tau;
   
   if (verbose)
   {
      fprintf(logInfo.fp_log, "Starting modeling for %s\n", info->id);
      fflush(logInfo.fp_log);
   }

   /* constants */
   zeroC = cmplx(0,0);

   /* memory allocation */
   thick = alloc1float(nL + 1);
   alpha = alloc1float(nL + 1);
   beta = alloc1float(nL + 1);
   rho = alloc1float(nL + 1);
   qP = alloc1float(nL + 1);
   qS = alloc1float(nL + 1);
   PSlowness = alloc1complex(nL + 1);
   SSlowness = alloc1complex(nL + 1);
   S2Velocity = alloc1complex(nL + 1);
   recArray = alloc1float(nR);
   aux11 = alloc1complex(nR);
   aux12 = alloc1complex(nR);
   aux21 = alloc1complex(nR);
   aux22 = alloc1complex(nR);
   aux11Old = alloc1complex(nR);
   aux12Old = alloc1complex(nR);
   aux21Old = alloc1complex(nR);
   aux22Old = alloc1complex(nR);

   uW = alloc2complex(nFreqProc, nR);

   /* receiving elastic model */
   RecvFloat(thick, nL + 1, -1, THICKNESS);
   RecvFloat(rho, nL + 1, -1, DENSITY);
   RecvFloat(alpha, nL + 1, -1, ALPHAS);
   RecvFloat(qP, nL + 1, -1, QALPHA);
   RecvFloat(beta, nL + 1, -1, BETAS);
   RecvFloat(qS, nL + 1, -1, QBETA);

   /* DD 
   fprintf(logInfo.fp_log, "nF %d\n", nF);
   fprintf(logInfo.fp_log, "nFreqProc %d\n", nFreqProc);
   fprintf(logInfo.fp_log, "tau %f\n", tau);
   fprintf(logInfo.fp_log, "percW %f\n", percW);
   fprintf(logInfo.fp_log, "percU %f\n", percU);
   fprintf(logInfo.fp_log, "u1 %f\n", u1);
   fprintf(logInfo.fp_log, "u2 %f\n", u2);
   fprintf(logInfo.fp_log, "r1 %f\n", r1);
   fprintf(logInfo.fp_log, "dR %f\n", dR);
   fprintf(logInfo.fp_log, "F1 %f\n", F1);
   fprintf(logInfo.fp_log, "F2 %f\n", F2);
   fprintf(logInfo.fp_log, "F3 %f\n", F3);
   fprintf(logInfo.fp_log, "zs %f\n", zs);
   fprintf(logInfo.fp_log, "nSamples %d\n", nSamples);
   for (iU = 0; iU < nL; iU++)
   {
      fprintf(logInfo.fp_log, "t %f d %f a %f qa %f b %f qb %f\n", 
	      thick[iU], rho[iU], alpha[iU], qP[iU], beta[iU], qS[iU]);
   }
   fflush(logInfo.fp_log);*/

   /* source is at 1st layer */
   thick[0] -= zs;
   
   /* computing the window length for the slowness domain */
   epslon1 = (u2 - u1) * percU;
   wL = NINT(epslon1 / dU);
   wL = 2 * wL + 1;
   u2 += epslon1;
   nU = NINT((u2 - u1) / dU);    /* new nU to preserve last slowness */
                                 /* w/o being windowed */
   /* DD 
   fprintf(logInfo.fp_log, "u1 %f\n", u1);
   fprintf(logInfo.fp_log, "u2 %f\n", u2);
   fprintf(logInfo.fp_log, "dU %f nU %d\n", dU, nU);
   fflush(logInfo.fp_log);*/
      
   /* building window for slowness integration */
   taper = alloc1float(nU);
   for (i = (wL - 1) / 2, iU = 0; iU < nU; iU++)
   {
      taper[iU] = 1;
      if (iU >= nU - (wL - 1) / 2)
      {
         i++;
	 taper[iU] =
	    .42 - .5 * cos(2 * PI * (float) i / ((float) (wL - 1))) +
            .08 * cos(4 * PI * (float) i / ((float) (wL - 1)));
      }
   }

   /* building windowing operator */
   /* DD
   fprintf(logInfo.fp_log, "going to filter\n");
   fflush(logInfo.fp_log);*/
   filter(percW);
   
   /* I will assume that the receivers are in line (at z = 0) so phi = 0 */
   phi = 0;
   epslon1 = F3;
   epslon2 = F1 * cos(phi) + F2 * sin(phi);

   /* auxiliar constant */
   cte = 1. / (4 * PI * rho[0]);

   /* normalization for the complex slowness */
   if (f1 > 7.5)
      wRef = f1 * 2 * PI;
   else
      wRef = 7.5 * 2 * PI;

   /* specific geometry */
   fp = fopen(recFile, "r");
   if (fp == NULL)
   {
      /* standard end-on */
      for (iR = 0; iR < nR; iR++)
      {
	 recArray[iR] = r1 + iR * dR;
      }
   }
   else
   {
      for (iR = 0; iR < nR; iR++)
      {
	 fscanf(fp, "%f\n", &recArray[iR]);
      }
   }
   fclose(fp);

   /* loop over frequencies */
   do
   {
      masterId = RecvInt(wInfo, 2, -1, FREQUENCY_LIMITS);
      
      if (verbose)
      {
	 fprintf(logInfo.fp_log, 
		 "Frequency limits received: [%d, %d] : [%f, %f]\n", 
		 wInfo[0], wInfo[1], wInfo[0] * dF, wInfo[1] * dF);
	 fflush(logInfo.fp_log);
      }
      
      /* first frequency */
      f1 = wInfo[0] * dF;

      for (indexF = NINT(f1 / dF), f = f1, iF = 0; 
	   iF < (wInfo[1] - wInfo[0] + 1); iF++, f += dF, indexF++)
      {
	 if (verbose)
	 {
	    fprintf(logInfo.fp_log, 
		    "Slave modeling frequency (Hz) : %f: \n", f);
	    fflush(logInfo.fp_log);
	 }
	 
	 /* reseting */
	 for (iR = 0; iR < nR; iR++)
	 {
	    aux11[iR] = zeroC;	         aux12[iR] = zeroC;
	    aux21[iR] = zeroC;	         aux22[iR] = zeroC;
	    aux11Old[iR] = zeroC;	 aux12Old[iR] = zeroC;
	    aux21Old[iR] = zeroC;	 aux22Old[iR] = zeroC;
	    uW[iR][iF] = zeroC;
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
	 
	 horSlowness();
	 
	 for (u = u1, iU = 0; iU < nU; iU++, u += dU, 
	      uC.r += dUC.r, uC.i += dUC.i)
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
	    
	    Rm();

	    Rp();
	    
	    /* reseting */
	    As1 = zeroC;      As2 = zeroC;      /* downgoing waves */
	    Cs1 = zeroC;      Cs2 = zeroC;      /* downgoing waves */
	    Bs1 = zeroC;      Bs2 = zeroC;      /* upgoing waves */
	    Ds1 = zeroC;      Ds2 = zeroC;      /* upgoing waves */
	    
	    /* P-wave potential */
	    /* PSlowness^2 - uuC */
	    auxm1 = PSlowness[0].r - uuC.r;
	    auxm2 = PSlowness[0].i - uuC.i;
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
	    auxm1 = SSlowness[0].r - uuC.r;
	    auxm2 = SSlowness[0].i - uuC.i;
	    
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
	    
	    /* DD 
	    Bs1 = zeroC;    Bs2 = zeroC;
	    Ds1 = zeroC;    Ds2 = zeroC;*/
	    
	    /* DD 
	    fprintf(logInfo.fp_log, "Ps0.r %f Ps0.i %f u %f f %d \n", 
		    PSlowness[0].r, PSlowness[0].i,
		    uC.r, iF);
	    fprintf(logInfo.fp_log, "As1.r %f As1.i %f\n", As1.r, As1.i);
	    fprintf(logInfo.fp_log, "As2.r %f As2.i %f\n", As2.r, As2.i);
	    fprintf(logInfo.fp_log, "Bs1.r %f Bs1.i %f\n", Bs1.r, Bs1.i);
	    fprintf(logInfo.fp_log, "Bs2.r %f Bs2.i %f\n", Bs2.r, Bs2.i);
	    fprintf(logInfo.fp_log, "Cs1.r %f Cs1.i %f\n", Cs1.r, Cs1.i);
	    fprintf(logInfo.fp_log, "Cs2.r %f Cs2.i %f\n", Cs2.r, Cs2.i);
	    fprintf(logInfo.fp_log, "Ds1.r %f Ds1.i %f\n", Ds1.r, Ds1.i);
	    fprintf(logInfo.fp_log, "Ds2.r %f Ds2.i %f\n", Ds2.r, Ds2.i);*/

	    /* computing compensation for free-surface */
	    /* auxiliar quantities */
	    ambm.r = am.r * bm.r - am.i * bm.i;
	    ambm.i = am.r * bm.i + am.i * bm.r;
	    
	    aux1.r = uC2.r * S2Velocity[0].r - uC2.i * S2Velocity[0].i;
	    aux1.i = uC2.r * S2Velocity[0].i + uC2.i * S2Velocity[0].r;
	    
	    aux2.r = 1. - 
	       (uuC2.r * S2Velocity[0].r - uuC2.i * S2Velocity[0].i);
	    aux2.i = -(uuC2.r * S2Velocity[0].i + uuC2.i * S2Velocity[0].r);
	    
	    /* S2Velocity[0] * S2Velocity[0] */
	    auxm1 = S2Velocity[0].r * S2Velocity[0].r - 
	       S2Velocity[0].i * S2Velocity[0].i;
	    auxm2 = 2 * S2Velocity[0].r * S2Velocity[0].i;
	    
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
	    v1[0].r = auxm1 + auxm3;
	    v1[0].i = auxm2 + auxm4;
	    
	    auxm1 = aux1.r * irrI[1][0].r - aux1.i * irrI[1][0].i;
	    auxm2 = aux1.r * irrI[1][0].i + aux1.i * irrI[1][0].r;
	    auxm3 = aux2.r * irrI[1][1].r - aux2.i * irrI[1][1].i;
	    auxm4 = aux2.r * irrI[1][1].i + aux2.i * irrI[1][1].r;
	    v1[1].r = auxm1 + auxm3;
	    v1[1].i = auxm2 + auxm4;
	    
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
	    v2[0].r = auxm1 + auxm3;
	    v2[0].i = auxm2 + auxm4;
	    
	    auxm1 = aux1.r * irrI[1][0].r - aux1.i * irrI[1][0].i;
	    auxm2 = aux1.r * irrI[1][0].i + aux1.i * irrI[1][0].r;
	    auxm3 = aux2.r * irrI[1][1].r - aux2.i * irrI[1][1].i;
	    auxm4 = aux2.r * irrI[1][1].i + aux2.i * irrI[1][1].r;
	    v2[1].r = auxm1 + auxm3;
	    v2[1].i = auxm2 + auxm4;

	    /* applying phase-shift */
	    aux = v1[0].r * g[0].r - v1[0].i * g[0].i;
	    v1[0].i = v1[0].r * g[0].i + v1[0].i * g[0].r;
	    v1[0].r = aux;
	    
	    aux = v1[1].r * g[1].r - v1[1].i * g[1].i;
	    v1[1].i = v1[1].r * g[1].i + v1[1].i * g[1].r;
	    v1[1].r = aux;
	    
	    aux = v2[0].r * g[0].r - v2[0].i * g[0].i;
	    v2[0].i = v2[0].r * g[0].i + v2[0].i * g[0].r;
	    v2[0].r = aux;
	    
	    aux = v2[1].r * g[1].r - v2[1].i * g[1].i;
	    v2[1].i = v2[1].r * g[1].i + v2[1].i * g[1].r;
	    v2[1].r = aux;
	    
	    /* multiplication by matrix h */
	    auxm1 = h[0][0].r * v1[0].r - h[0][0].i * v1[0].i;
	    auxm2 = h[0][0].r * v1[0].i + h[0][0].i * v1[0].r;
	    auxm3 = h[0][1].r * v1[1].r - h[0][1].i * v1[1].i;
	    auxm4 = h[0][1].r * v1[1].i + h[0][1].i * v1[1].r;
	    v1A = v1[0];              /* for future use */
	    v1[0].r = auxm1 + auxm3;
	    v1[0].i = auxm2 + auxm4;
	    
	    auxm1 = h[0][0].r * v2[0].r - h[0][0].i * v2[0].i;
	    auxm2 = h[0][0].r * v2[0].i + h[0][0].i * v2[0].r;
	    auxm3 = h[0][1].r * v2[1].r - h[0][1].i * v2[1].i;
	    auxm4 = h[0][1].r * v2[1].i + h[0][1].i * v2[1].r;
	    v2A = v2[0];              /* for future use */
	    v2[0].r = auxm1 + auxm3;
	    v2[0].i = auxm2 + auxm4;
	    
	    auxm1 = h[1][0].r * v1A.r - h[1][0].i * v1A.i;
	    auxm2 = h[1][0].r * v1A.i + h[1][0].i * v1A.r;
	    auxm3 = h[1][1].r * v1[1].r - h[1][1].i * v1[1].i;
	    auxm4 = h[1][1].r * v1[1].i + h[1][1].i * v1[1].r;
	    v1[1].r = auxm1 + auxm3;
	    v1[1].i = auxm2 + auxm4;
	    
	    auxm1 = h[1][0].r * v2A.r - h[1][0].i * v2A.i;
	    auxm2 = h[1][0].r * v2A.i + h[1][0].i * v2A.r;
	    auxm3 = h[1][1].r * v2[1].r - h[1][1].i * v2[1].i;
	    auxm4 = h[1][1].r * v2[1].i + h[1][1].i * v2[1].r;
	    v2[1].r = auxm1 + auxm3;
	    v2[1].i = auxm2 + auxm4;
	    
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
	       aux1.r = -J11 * taper[iU] * v1[0].r;
	       aux1.i = -J11 * taper[iU] * v1[0].i;
	       
	       aux11[iR].r += aux1.r + aux11Old[iR].r;
	       aux11[iR].i += aux1.i + aux11Old[iR].i;
	       aux11Old[iR] = aux1;
	       
	       aux1.r = J00 * taper[iU] * v2[0].r;
	       aux1.i = J00 * taper[iU] * v2[0].i;
	       
	       aux21[iR].r += aux1.r + aux21Old[iR].r;
	       aux21[iR].i += aux1.i + aux21Old[iR].i;
	       aux21Old[iR] = aux1;
	       
	       /* z component */
	       aux1.r = -v1[1].i;
	       aux1.i = v1[1].r;
	       
	       aux1.r = J00 * taper[iU] * aux1.r;
	       aux1.i = J00 * taper[iU] * aux1.i;
	       
	       aux12[iR].r += aux1.r + aux12Old[iR].r;
	       aux12[iR].i += aux1.i + aux12Old[iR].i;
	       aux12Old[iR] = aux1;	
	       
	       aux1.r = -v2[1].i;
	       aux1.i =  v2[1].r;
	       
	       aux1.r = J11 * taper[iU] * aux1.r;
	       aux1.i = J11 * taper[iU] * aux1.i;
	       
	       aux22[iR].r += aux1.r + aux22Old[iR].r;
	       aux22[iR].i += aux1.i + aux22Old[iR].i;
	       aux22Old[iR] = aux1;
	    }
	 }
	 dUCEp1.r = epslon1 * dUC.r;
	 dUCEp1.i = epslon1 * dUC.i;
	 dUCEp2.r = epslon2 * dUC.r;
	 dUCEp2.i = epslon2 * dUC.i;
	 
	 /* loop over offsets */
	 for (iR = 0; iR < nR; iR++)
	 {
	    
	    /* displacements in the radial direction (frequency domain) */
	    if (RADIAL)
	    {
	       /* there's a 2 (free surface) / 2 (trapezoidal integration) */
	       /* simplified in the equation below */
	       auxm1 = aux11[iR].r * dUCEp1.r - aux11[iR].i * dUCEp1.i;
	       auxm2 = aux11[iR].r * dUCEp1.i + aux11[iR].i * dUCEp1.r;
	       auxm3 = aux21[iR].r * dUCEp2.r - aux21[iR].i * dUCEp2.i;
	       auxm4 = aux21[iR].r * dUCEp2.i + aux21[iR].i * dUCEp2.r;
	    
	       uW[iR][iF].r = (auxm1 + auxm3) * wCCte.r - 
		  (auxm2 + auxm4) * wCCte.i;
	       uW[iR][iF].i = (auxm1 + auxm3) * wCCte.i + 
		  (auxm2 + auxm4) * wCCte.r;

	       /* filtering */
	       uW[iR][iF].r *= window[indexF] * SGN(recArray[iR]);
	       uW[iR][iF].i *= window[indexF] * SGN(recArray[iR]);
	    }

	    if (VERTICAL)
	    {
	       /* displacements in the z direction (frequency domain) */
	       /* there's a 2 (free surface) / 2 (trapezoidal integration) */
	       /* simplified in the equation below */
	       auxm1 = aux12[iR].r * dUCEp1.r - aux12[iR].i * dUCEp1.i;
	       auxm2 = aux12[iR].r * dUCEp1.i + aux12[iR].i * dUCEp1.r;
	       auxm3 = aux22[iR].r * dUCEp2.r - aux22[iR].i * dUCEp2.i;
	       auxm4 = aux22[iR].r * dUCEp2.i + aux22[iR].i * dUCEp2.r;
	       
	       uW[iR][iF].r = (auxm1 + auxm3) * wCCte.r - 
		  (auxm2 + auxm4) * wCCte.i;
	       uW[iR][iF].i = (auxm1 + auxm3) * wCCte.i + 
		  (auxm2 + auxm4) * wCCte.r;
	    
	       /* filtering */
	       uW[iR][iF].r *= window[indexF];
	       uW[iR][iF].i *= window[indexF];

	       /* DD 
	       fprintf(logInfo.fp_log, "iF %d fr %f fi %f window %f\n", 
		       iF, uW[iR][iF].r,uW[iR][iF].i, window[indexF]);
	       fflush(logInfo.fp_log);*/
	    }
	 }
      }
      /* sending frequency partition to the master process */
      SendCplx(uW[0], nFreqProc * nR, masterId, FREQUENCY_PARTITION);
      
      /* keep working ? */
      masterId = RecvInt(&die, 1, -1, DIE);
      
      if (verbose)
      {
	 fprintf(logInfo.fp_log, "Slave receiving flag to stop : %d\n", die);
	 fflush(logInfo.fp_log);
      }
      
      if (die) break;
   } while (FOREVER);

   /* quitting PVM */
   EndOfSlave();    
}

/*
   Computing frequency-dependent slowness (squared) for all layer of the
   model
*/
void horSlowness()
{
   /* declaration of variables */
   int iL;                       /* counter */
   float cteQp1, cteQp2;
   float cteQs1, cteQs2;         /* constants used in absorption */

   for(iL = 0; iL <= nL; iL++)
   {

      /* constants used in absorption */
      cteQp1 = alpha[iL] / (PI * qP[iL]);
      cteQp2 = alpha[iL] / (2 * qP[iL]);
      cteQs1 = beta[iL] / (PI * qS[iL]);
      cteQs2 = beta[iL] / (2 * qS[iL]);

      /* p-wave slowness squared */
      PSlowness[iL].r = alpha[iL] + cteQp1 * log(wCRwR);
      PSlowness[iL].i = cteQp2 - cteQp1 * wCP;
      
      /* 1. / (PSlowness[iL] * PSlowness[iL]) */
      auxm1 = PSlowness[iL].r * PSlowness[iL].r;
      auxm2 = PSlowness[iL].i * PSlowness[iL].i;
      aux = (auxm1 + auxm2) * (auxm1 + auxm2); aux = 1 / aux;
      auxm3 = (auxm1 - auxm2) * aux;
      PSlowness[iL].i = -2 * PSlowness[iL].r * PSlowness[iL].i * aux;
      PSlowness[iL].r = auxm3;

      /* s-wave velocity */
      auxm1 = beta[iL] + cteQs1 * log(wCRwR);
      auxm2 = cteQs2 - cteQs1 * wCP;

      /* S2Velocity[iL] * S2Velocity[iL] */
      S2Velocity[iL].r = auxm1 * auxm1 - auxm2 * auxm2;
      S2Velocity[iL].i = 2 * auxm1 * auxm2;

      /* 1. / S2Velocity^2 */
      aux = S2Velocity[iL].r * S2Velocity[iL].r + 
	    S2Velocity[iL].i * S2Velocity[iL].i;
      SSlowness[iL].r = S2Velocity[iL].r / aux;
      SSlowness[iL].i = -S2Velocity[iL].i / aux;
   }
}
/*
*/
void Rm()
{
   /* declaration of variables */
   int iL;                      /* layer index */
                                /* used in absorption */
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

   /* main loop over the nL layers */
   for (iL = nL - 1; iL >= 1; iL--)
   {
      /* square-root of PSlowness^2 - uuC */    
      auxm1 = PSlowness[iL].r - uuC.r;
      auxm2 = PSlowness[iL].i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      am.r = auxm3 * cos(angle);
      am.i = auxm3 * sin(angle);

      /* am * I */
      amI.r = -am.i;
      amI.i = am.r;

      /* square-root of SSlowness^2 - uuC */    
      auxm1 = SSlowness[iL].r - uuC.r;
      auxm2 = SSlowness[iL].i - uuC.i;
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

   }
   /* computing final phase-shift matrix */
   /* square-root of Pslowness^2 - uuC */    
   auxm1 = PSlowness[0].r - uuC.r;
   auxm2 = PSlowness[0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   am.r = auxm3 * cos(angle);
   am.i = auxm3 * sin(angle);

   /* am * I */
   amI.r = -am.i;
   amI.i = am.r;

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[0].r - uuC.r;
   auxm2 = SSlowness[0].i - uuC.i;
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
}
/*
*/
void Rp()
{
   /* declaration of variables */
   complex aux1, aux12;         /* auxiliar variables */
   complex wThick;              /* auxiliar variable */
   complex E[2][2];             /* phase shift matrix */
   complex am, bm;              /* vertical slownesses for P and S waves */
   complex amI, bmI;            /* amI = I * am, bmI = I * bm */
   complex ambm;                /* = am * bm */
   complex ambm4uu;             /* = 4 * am * bm * uC * uC */
   complex den;                 /* denominator of coefficients */

   /* square-root of PSlowness - uuC */    
   auxm1 = PSlowness[0].r - uuC.r;
   auxm2 = PSlowness[0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   am.r = auxm3 * cos(angle);
   am.i = auxm3 * sin(angle);

   /* am * I */
   amI.r = -am.i;
   amI.i = am.r;

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[0].r - uuC.r;
   auxm2 = SSlowness[0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   bm.r = auxm3 * cos(angle);
   bm.i = auxm3 * sin(angle);

   /* bm * I */
   bmI.r = -bm.i;
   bmI.i = bm.r;
   
   aux1.r = SSlowness[0].r - uuC2.r;
   aux1.i = SSlowness[0].i - uuC2.i;
   aux12.r = aux1.r * aux1.r - aux1.i * aux1.i;
   aux12.i = 2 * aux1.r * aux1.i;

   /* let ambm = am * bm */
   ambm.r = am.r * bm.r - am.i * bm.i;
   ambm.i = am.r * bm.i + am.i * bm.r;

   /* and ambm4uu = am * bm * 4 * uu */
   ambm4uu.r = 4 * (ambm.r * uuC.r - ambm.i * uuC.i);
   ambm4uu.i = 4 * (ambm.r * uuC.i + ambm.i * uuC.r);

   den.r = aux12.r + ambm4uu.r;
   den.i = aux12.i + ambm4uu.i;
   
   /* 1/ den */
   aux = den.r * den.r + den.i * den.i;
   den.r = den.r / aux;
   den.i = -den.i / aux;

   auxm1 = ambm4uu.r - aux12.r;
   auxm2 = ambm4uu.i - aux12.i;
   rp[0][0].r = auxm1 * den.r - auxm2 * den.i;
   rp[0][0].i = auxm1 * den.i + auxm2 * den.r;               /* Rpp */

   auxm1 = bm.r * aux1.r - bm.i * aux1.i;
   auxm2 = bm.r * aux1.i + bm.i * aux1.r;
   auxm3 = 4 * (auxm1 * uC.r - auxm2 * uC.i);
   auxm4 = 4 * (auxm1 * uC.i + auxm2 * uC.r);
   rp[0][1].r = den.r * auxm3 - den.i * auxm4;
   rp[0][1].i = den.r * auxm4 + den.i * auxm3;               /* Rsp */

   auxm1 = am.r * aux1.r - am.i * aux1.i;
   auxm2 = am.r * aux1.i + am.i * aux1.r;
   auxm3 = -4 * (auxm1 * uC.r - auxm2 * uC.i);
   auxm4 = -4 * (auxm1 * uC.i + auxm2 * uC.r);
   rp[1][0].r = den.r * auxm3 - den.i * auxm4;
   rp[1][0].i = den.r * auxm4 + den.i * auxm3;               /* Rsp */

   rp[1][1].r = -rp[0][0].r;
   rp[1][1].i = -rp[0][0].i;                                 /* Rss */

   /* computing phase-shift matrix */
   wThick.r = wC.r * (-2 * zs);
   wThick.i = wC.i * (-2 * zs);

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
   aux = rp[0][0].r * E[0][0].r - rp[0][0].i * E[0][0].i;
   rp[0][0].i = rp[0][0].r * E[0][0].i + rp[0][0].i * E[0][0].r;
   rp[0][0].r = aux;
   aux = rp[0][1].r * E[0][1].r - rp[0][1].i * E[0][1].i;
   rp[0][1].i = rp[0][1].r * E[0][1].i + rp[0][1].i * E[0][1].r;
   rp[0][1].r = aux;
   aux = rp[1][0].r * E[1][0].r - rp[1][0].i * E[1][0].i;
   rp[1][0].i = rp[1][0].r * E[1][0].i + rp[1][0].i * E[1][0].r;
   rp[1][0].r = aux;
   aux = rp[1][1].r * E[1][1].r - rp[1][1].i * E[1][1].i;
   rp[1][1].i = rp[1][1].r * E[1][1].i + rp[1][1].i * E[1][1].r;
   rp[1][1].r = aux;
}
/*
   Computation of reflection and transmission coefficients for 
   elastic propagation for a solid-solid boundary.

   Two situations are considered:

   1) Wave incident from layer above (medium 1) (Function RTd)

   2) Wave incident from layer below (medium 2) (Function RTu)

   Both functions return 1 if critical angle is reached

   Input parameters: 

   alpha1:                P-wave velocity of layer1
   beta1:                 S-wave velocity of layer1
   rho1:                  Density of layer1

   alpha2:                P-wave velocity of layer2
   beta2:                 S-wave velocity of layer2
   rho2:                  Density of layer2

   u:                     horizontal slowness
*/
void RTd(int i1, int i2)
{ 
  /* declaration of variables */
   complex aux1, aux2, aux3, c;    
   complex cuu, caux3, aux1aux2;   /* auxiliar quantities */
   complex a1, a2, b1, b2;         /* all vertical slownesses */
   complex a1b1, a1b2, a2b1, a2b2; /* auxiliar quantities */
   complex a1a2b1b2;               /* auxiliar quantities */
   complex d1d, d2d;               /* auxiliar quantities */
   complex dpda, dpdb;             /* auxiliar quantities */

   /* square-root of Pslowness^2 - uuC */    
   auxm1 = PSlowness[i1].r - uuC.r;
   auxm2 = PSlowness[i1].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a1.r = auxm3 * cos(angle);
   a1.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i1].r - uuC.r;
   auxm2 = SSlowness[i1].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b1.r = auxm3 * cos(angle);
   b1.i = auxm3 * sin(angle);

   /* square-root of PSlowness^2 - uuC */    
   auxm1 = PSlowness[i2].r - uuC.r;
   auxm2 = PSlowness[i2].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a2.r = auxm3 * cos(angle);
   a2.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i2].r - uuC.r;
   auxm2 = SSlowness[i2].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b2.r = auxm3 * cos(angle);
   b2.i = auxm3 * sin(angle);

   /* computing auxiliary quantities */
   rho1rho2 = rho[i1] * rho[i2];
   c.r = 2 * (rho[i1] * S2Velocity[i1].r - rho[i2] * S2Velocity[i2].r);
   c.i = 2 * (rho[i1] * S2Velocity[i1].i - rho[i2] * S2Velocity[i2].i);

   cuu.r = c.r * uuC.r - c.i * uuC.i;
   cuu.i = c.r * uuC.i + c.i * uuC.r;
   
   aux1.r = cuu.r - (rho[i1] - rho[i2]);
   aux1.i = cuu.i;

   aux2.r = cuu.r + rho[i2];
   aux2.i = cuu.i;
   
   aux1aux2.r = aux1.r * aux2.r - aux1.i * aux2.i;
   aux1aux2.i = aux1.r * aux2.i + aux1.i * aux2.r;

   aux3.r = cuu.r - rho[i1];
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
   coeffD[0].r = auxm1 * dd.r - auxm2 * dd.i;
   coeffD[0].i = auxm1 * dd.i + auxm2 * dd.r;                 /* Rpp */
   
   auxm1 = a2b2.r * caux3.r - a2b2.i * caux3.i;
   auxm2 = a2b2.r * caux3.i + a2b2.i * caux3.r;
   coeffD[1].r = aux1aux2.r + auxm1;
   coeffD[1].i = aux1aux2.i + auxm2;                          /* Rsp */  
								 
   coeffD[2].r = coeffD[1].r;
   coeffD[2].i = coeffD[1].i;                                 /* Rps*/  

   auxm3 = dpdb.r * uC2.r - dpdb.i * uC2.i;
   auxm4 = dpdb.r * uC2.i + dpdb.i * uC2.r;
   
   aux = auxm3 * coeffD[1].r - auxm4 * coeffD[1].i;
   coeffD[1].i = auxm3 * coeffD[1].i + auxm4 * coeffD[1].r;
   coeffD[1].r = aux;                                         /* Rsp */
  
   auxm3 = -dpda.r * uC2.r + dpda.i * uC2.i;
   auxm4 = -dpda.r * uC2.i - dpda.i * uC2.r;
   
   aux = auxm3 * coeffD[2].r - auxm4 * coeffD[2].i;
   coeffD[2].i = auxm3 * coeffD[2].i + auxm4 * coeffD[2].r;
   coeffD[2].r = aux;                                         /* Rps */

   auxm1 = d2d.r - d1d.r;
   auxm2 = d2d.i - d1d.i;
   auxm3 = 2 * rho1rho2 * (a1b2.r - a2b1.r);
   auxm4 = 2 * rho1rho2 * (a1b2.i - a2b1.i);

   coeffD[3].r = auxm1 - auxm3;
   coeffD[3].i = auxm2 - auxm4;
   aux = coeffD[3].r * dd.r - coeffD[3].i * dd.i;
   coeffD[3].i = coeffD[3].r * dd.i + coeffD[3].i * dd.r;
   coeffD[3].r = aux;                                         /* Rss */
  
   /* now transmition */
   auxm1 = b1.r * aux2.r - b1.i * aux2.i;
   auxm2 = b1.r * aux2.i + b1.i * aux2.r;
   auxm3 = b2.r * aux3.r - b2.i * aux3.i;
   auxm4 = b2.r * aux3.i + b2.i * aux3.r;
   coeffD[4].r = auxm1 - auxm3;     
   coeffD[4].i = auxm2 - auxm4;
   aux = 2 * rho[i1] * (dpda.r * coeffD[4].r - dpda.i * coeffD[4].i);
   coeffD[4].i = 2 * rho[i1] * (dpda.r * coeffD[4].i + dpda.i * coeffD[4].r);
   coeffD[4].r = aux;                                         /* Tpp */

   coeffD[5].r = aux1.r + a1b2.r * c.r - a1b2.i * c.i;
   coeffD[5].i = aux1.i + a1b2.r * c.i + a1b2.i * c.r;
   auxm1 = rho[i1] * (dpdb.r * uC2.r - dpdb.i * uC2.i);
   auxm2 = rho[i1] * (dpdb.r * uC2.i + dpdb.i * uC2.r);
   aux = coeffD[5].r * auxm1 - coeffD[5].i * auxm2;
   coeffD[5].i = coeffD[5].r * auxm2 + coeffD[5].i * auxm1;   /* Tsp */
   coeffD[5].r = aux; 

   coeffD[6].r = aux1.r + a2b1.r * c.r - a2b1.i * c.i;
   coeffD[6].i = aux1.i + a2b1.r * c.i + a2b1.i * c.r;
   auxm1 = -rho[i1] * (dpda.r * uC2.r - dpda.i * uC2.i);
   auxm2 = -rho[i1] * (dpda.r * uC2.i + dpda.i * uC2.r);
   aux = coeffD[6].r * auxm1 - coeffD[6].i * auxm2;
   coeffD[6].i = coeffD[6].r * auxm2 + coeffD[6].i * auxm1;   /* Tsp */
   coeffD[6].r = aux;

   auxm1 = a1.r * aux2.r - a1.i * aux2.i;
   auxm2 = a1.r * aux2.i + a1.i * aux2.r;
   coeffD[7].r = auxm1 - (a2.r * aux3.r - a2.i * aux3.i);
   coeffD[7].i = auxm2 - (a2.r * aux3.i + a2.i * aux3.r);     
   auxm3 = 2 * rho[i1] * dpdb.r;
   auxm4 = 2 * rho[i1] * dpdb.i;
   aux = coeffD[7].r * auxm3 - coeffD[7].i * auxm4;
   coeffD[7].i = coeffD[7].r * auxm4 + coeffD[7].i * auxm3;   /* Tss */
   coeffD[7].r = aux;
}

void RTu(int i1, int i2)
{
   /* declaration of variables */
   complex aux1, aux2, aux3, c;    
   complex cuu, caux2, aux1aux3;   /* auxiliar quantities */
   complex a1, a2, b1, b2;         /* all vertical slownesses */
   complex a1b1, a1b2, a2b1, a2b2; /* auxiliar quantities */
   complex a1a2b1b2;               /* auxiliar quantities */
   complex d1u, d2u;               /* auxiliar quantities */
   complex dpda, dpdb;             /* auxiliar quantities */
   
   /* square-root of PSlowness^2 - uuC */    
   auxm1 = PSlowness[i1].r - uuC.r;
   auxm2 = PSlowness[i1].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a1.r = auxm3 * cos(angle);
   a1.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i1].r - uuC.r;
   auxm2 = SSlowness[i1].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b1.r = auxm3 * cos(angle);
   b1.i = auxm3 * sin(angle);

   /* square-root of PSlowness^2 - uuC */    
   auxm1 = PSlowness[i2].r - uuC.r;
   auxm2 = PSlowness[i2].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a2.r = auxm3 * cos(angle);
   a2.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i2].r - uuC.r;
   auxm2 = SSlowness[i2].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b2.r = auxm3 * cos(angle);
   b2.i = auxm3 * sin(angle);

   /* computing auxiliary quantities */
   rho1rho2 = rho[i1] * rho[i2];
   c.r = 2 * (rho[i1] * S2Velocity[i1].r - rho[i2] * S2Velocity[i2].r);
   c.i = 2 * (rho[i1] * S2Velocity[i1].i - rho[i2] * S2Velocity[i2].i);

   cuu.r = c.r * uuC.r - c.i * uuC.i;
   cuu.i = c.r * uuC.i + c.i * uuC.r;

   aux1.r = cuu.r - (rho[i1] - rho[i2]);
   aux1.i = cuu.i;
   
   aux2.r = cuu.r + rho[i2];
   aux2.i = cuu.i;

   aux3.r = cuu.r - rho[i1];
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
   coeffU[0].r = auxm1 * dd.r - auxm2 * dd.i;
   coeffU[0].i = auxm1 * dd.i + auxm2 * dd.r;                 /* Rpp */

   auxm1 = a1b1.r * caux2.r - a1b1.i * caux2.i;
   auxm2 = a1b1.r * caux2.i + a1b1.i * caux2.r;
   coeffU[1].r = aux1aux3.r + auxm1;
   coeffU[1].i = aux1aux3.i + auxm2;                          /* Rsp */  
								 
   coeffU[2].r = coeffU[1].r;
   coeffU[2].i = coeffU[1].i;                                 /* Rps*/  

   auxm3 = -dpdb.r * uC2.r + dpdb.i * uC2.i;
   auxm4 = -dpdb.r * uC2.i - dpdb.i * uC2.r;
   
   aux = auxm3 * coeffU[1].r - auxm4 * coeffU[1].i;
   coeffU[1].i = auxm3 * coeffU[1].i + auxm4 * coeffU[1].r;
   coeffU[1].r = aux;                                         /* Rsp */

   auxm3 = dpda.r * uC2.r - dpda.i * uC2.i;
   auxm4 = dpda.r * uC2.i + dpda.i * uC2.r;
   
   aux = auxm3 * coeffU[2].r - auxm4 * coeffU[2].i;
   coeffU[2].i = auxm3 * coeffU[2].i + auxm4 * coeffU[2].r;
   coeffU[2].r = aux;                                         /* Rps */

   auxm1 = d2u.r - d1u.r;
   auxm2 = d2u.i - d1u.i;
   auxm3 = 2 * rho1rho2 * (a2b1.r - a1b2.r);
   auxm4 = 2 * rho1rho2 * (a2b1.i - a1b2.i);

   coeffU[3].r = auxm1 - auxm3;
   coeffU[3].i = auxm2 - auxm4;
   aux = coeffU[3].r * dd.r - coeffU[3].i * dd.i;
   coeffU[3].i = coeffU[3].r * dd.i + coeffU[3].i * dd.r;
   coeffU[3].r = aux;                                         /* Rss */

   /* now transmition */
   auxm1 = b1.r * aux2.r - b1.i * aux2.i;
   auxm2 = b1.r * aux2.i + b1.i * aux2.r;
   auxm3 = b2.r * aux3.r - b2.i * aux3.i;
   auxm4 = b2.r * aux3.i + b2.i * aux3.r;
   coeffU[4].r = auxm1 - auxm3;     
   coeffU[4].i = auxm2 - auxm4;
   aux = 2 * rho[i2] * (dpda.r * coeffU[4].r - dpda.i * coeffU[4].i);
   coeffU[4].i = 2 * rho[i2] * (dpda.r * coeffU[4].i + dpda.i * coeffU[4].r);
   coeffU[4].r = aux;                                         /* Tpp */

   coeffU[5].r = aux1.r + a2b1.r * c.r - a2b1.i * c.i;
   coeffU[5].i = aux1.i + a2b1.r * c.i + a2b1.i * c.r;
   auxm1 = rho[i2] * (dpdb.r * uC2.r - dpdb.i * uC2.i);
   auxm2 = rho[i2] * (dpdb.r * uC2.i + dpdb.i * uC2.r);
   aux = coeffU[5].r * auxm1 - coeffU[5].i * auxm2;
   coeffU[5].i = coeffU[5].r * auxm2 + coeffU[5].i * auxm1;   /* Tsp */
   coeffU[5].r = aux; 

   coeffU[6].r = aux1.r + a1b2.r * c.r - a1b2.i * c.i;
   coeffU[6].i = aux1.i + a1b2.r * c.i + a1b2.i * c.r;
   auxm1 = -rho[i2] * (dpda.r * uC2.r - dpda.i * uC2.i);
   auxm2 = -rho[i2] * (dpda.r * uC2.i + dpda.i * uC2.r);
   aux = coeffU[6].r * auxm1 - coeffU[6].i * auxm2;
   coeffU[6].i = coeffU[6].r * auxm2 + coeffU[6].i * auxm1;   /* Tsp */
   coeffU[6].r = aux;

   auxm1 = a1.r * aux2.r - a1.i * aux2.i;
   auxm2 = a1.r * aux2.i + a1.i * aux2.r;
   coeffU[7].r = auxm1 - (a2.r * aux3.r - a2.i * aux3.i);
   coeffU[7].i = auxm2 - (a2.r * aux3.i + a2.i * aux3.r);     
   auxm3 = 2 * rho[i2] * dpdb.r;
   auxm4 = 2 * rho[i2] * dpdb.i;
   aux = coeffU[7].r * auxm3 - coeffU[7].i * auxm4;
   coeffU[7].i = coeffU[7].r * auxm4 + coeffU[7].i * auxm3;   /* Tss */
   coeffU[7].r = aux;
}

/* 
   Computation of bessel functions order 0 and 1.
   Extracted from Numerical Recipes.
*/

void Bessels(float x)
{
   register float xd3, x2, x4, x6, x8, x10, x12, sqrx;
   if (x <= 2.75) 
   {
      xd3 = x / 3;
      x2 = xd3 * xd3;
      x4 = x2 * x2;
      x6 = x2 * x4;
      x8 = x4 * x4;
      x10 = x4 * x6;
      x12 = x6 * x6;
      
      J00 = 1.-2.2499997*x2+1.2656208*x4
              -0.3163866*x6+0.0444479*x8
  	      -0.0039444*x10+0.00021*x12;
      
      J11 = (0.5-0.56249985*x2+0.21093573*x4
	        -0.03954289*x6+0.00443319*x8
	        -0.00031761*x10+0.00001109*x12)*x;
   }
   else
   {
      sqrx = sqrt(2. / (PI * x));
      J00 = sqrx * cos(x - 0.25 * PI);
      J11 = sqrx * cos(x - 0.75 * PI);
   }
}
/*
   Building windowing operator for convolution with seismograms
*/
void filter(float percW)
{
   /* declaration of variables */
   int i, iF, indexF;            /* counters */
   int wL;                       /* window length */

   /* memory allocation */
   window = alloc1float(nSamples / 2 + 1);
   /* reseting array */
   for (i = 0; i < nSamples / 2 + 1; i++)
      window[i] = 0;

   wL = percW * nF;   wL = 2 * wL - 1;
   /* building hanning window */
   for (i = 0, indexF = NINT(f1 / dF), iF = 0; iF < nF; iF++, indexF++)
   {
      if (hanningFlag)
	 window[indexF] =
	    .42 - .5 * cos(2 * PI * (float) iF / ((float) (nF - 1))) +
            .08 * cos(4 * PI * (float) iF / ((float) (nF - 1)));
      else
      {
	 window[indexF] = 1;
	 if (iF < (wL - 1) / 2)
	 {
	    window[indexF] = 
	       .42 - .5 * cos(2 * PI * (float) i / ((float) (wL - 1))) +
  	       .08 * cos(4 * PI * (float) i / ((float) (wL - 1)));
	    i++;
	 }
	 else if (iF > nF - (wL - 1) / 2)
	 {
	    i++;
	    window[indexF] =
	       .42 - .5 * cos(2 * PI * (float) i / ((float) (wL - 1))) +
  	       .08 * cos(4 * PI * (float) i / ((float) (wL - 1)));
	 }
      }
   }
}
 
