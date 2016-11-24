/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*                                                              */
/*  Function gradient()                                         */
/*                                                              */
/*  Compute the FRECHET derivatives for the elastic             */
/*  modeling. Distributed implementation.                       */
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
/*  res....................synthetic model generated for model  */
/*                         global                               */
/*  CD.....................data covariance matrix               */
/*                         global                               */
/*                                                              */
/*  Output parameters:                                          */
/*  grad...................the gradient, a vector which         */
/*                         dimension is the number of layers    */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
#include "frechetSlave.h"
void main()
{
   /* declaration of variables */
   FILE *fp;                       /* file pointer for receiver array */
   char *recFile = " ";            /* file with receiver coordinates */
   int i, indexF, iF, iR, iU, iDer, iL;
                                   /* counters */
   int masterId;                   /* master id */
   int die;                        /* if die=1 process stops */
   int directWave;                 /* direct wave flag */
   int nU;                         /* number of slownesses */
   int wL;                         /* taper length */
   int wInfo[2];                   /* frequency limits */
   int verbose;                    /* dialogue flag */ 
   float *grad;                    /* partial gradient */
   float cte;                      /* a constant */
   float tau;                      /* magnitude of wrap-around attenuation */
   float percU;                    /* percentual of slowness */
                                   /* domain that are windowed */
   float percW;                    /* percentual of frequency */
                                   /* domain that are windowed */
   float r1, dR;                   /* defines receiver array */
   float u1, u2;                   /* slowness window */
   float u, w, f;                  /* slowness & frequency */
   float phi;                      /* azimuth angle */
   float F1, F2, F3;               /* source components */
   float epslon1, epslon2;         /* auxiliary quantities */
   float dU;                       /* slowness interval */
   float wRef;                     /* reference frequency */     
   complex irr[2][2];              /* = (I - R-R+) */
   complex irrI[2][2];             /* = (I - R-R+)^-1 */                     
   complex muC;                    /* auxiliar variable */
   complex dUC;                    /* slowness complex interval */
   complex **resCD;                /* current residual dotted */
                                   /* into covariance */ 
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
   complex displ;                  /* Frechet derivative of displacements */
   INFO info[1];                   /* basic information for slaves */
   
   SeisSlave logInfo;              /* report structure */

   /* logging information */
   InitLog(&logInfo, PROCESS_FRECHET);
    
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
   numberPar = info->numberPar;
   limRange = info->limRange;
   lim[0] = info->lim[0];
   lim[1] = info->lim[1];
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
   vpFrechet = info->vpFrechet;
   vsFrechet = info->vsFrechet;
   rhoFrechet = info->rhoFrechet;

   if (verbose)
   {
      fprintf(logInfo.fp_log, "Starting modeling for %s\n", info->id);
      fflush(logInfo.fp_log);
   }

   /* DD 
   fprintf(logInfo.fp_log, "numberPar %d\n", numberPar);
   fprintf(logInfo.fp_log, "lim0 %d lim1 %d\n", lim[0], lim[1]);
   fprintf(logInfo.fp_log, "limRange %d\n", limRange);*/
   
   /* memory allocation */
   thick = alloc1float(nL + 1);
   alpha = alloc1float(nL + 1);
   beta = alloc1float(nL + 1);
   rho = alloc1float(nL + 1);
   qP = alloc1float(nL + 1);
   qS = alloc1float(nL + 1);
   recArray = alloc1float(nR);
   grad = alloc1float(numberPar * limRange);
   v1 = alloc2complex(2, numberPar * limRange + 1);
   v2 = alloc2complex(2, numberPar * limRange + 1);
   DmB = alloc3complex(4, numberPar * (limRange + 2), nL);
   derFactor = alloc2complex(2, nL + 1);
   aux11 = alloc2complex(nR, numberPar * limRange);
   aux12 = alloc2complex(nR, numberPar * limRange);
   aux21 = alloc2complex(nR, numberPar * limRange);
   aux22 = alloc2complex(nR, numberPar * limRange);
   aux11Old = alloc2complex(nR, numberPar * limRange);
   aux12Old = alloc2complex(nR, numberPar * limRange);
   aux21Old = alloc2complex(nR, numberPar * limRange);
   aux22Old = alloc2complex(nR, numberPar * limRange);
        
   PSlowness = alloc2complex(2, nL + 1);
   SSlowness = alloc2complex(2, nL + 1);
   S2Velocity = alloc2complex(2, nL + 1);                  
   
   /* DD
   fprintf(logInfo.fp_log,  
	   "receiving model info \n");
   fflush(logInfo.fp_log);*/

   /* receiving elastic model */
   RecvFloat(thick, nL + 1, -1, THICKNESS);
   RecvFloat(rho, nL + 1, -1, DENSITY);
   RecvFloat(alpha, nL + 1, -1, ALPHAS);
   RecvFloat(qP, nL + 1, -1, QALPHA);
   RecvFloat(beta, nL + 1, -1, BETAS);
   RecvFloat(qS, nL + 1, -1, QBETA);

   /* DD 
   fprintf(logInfo.fp_log,  
	   "received model info \n");
   fflush(logInfo.fp_log);*/

   /* source is at 1st layer */
   thick[0] -= zs;

   /* computing the window length for the slowness domain */
   epslon1 = (u2 - u1) * percU;
   wL = NINT(epslon1 / dU);   wL = 2 * wL + 1;
   u2 += epslon1;
   nU = NINT((u2 - u1) / dU);   /* new nU to preserve last slowness */
                                /* w/o being windowed */     
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
      /* reseting gradient */
      for (iDer = 0; iDer < numberPar * limRange; iDer++)
	 grad[iDer] = 0;
   
      /* DD 
      fprintf(logInfo.fp_log,  
	      "receiving frequency limits and correlation matrix\n");
      fflush(logInfo.fp_log);*/

      /* receiving frequency limits and covariance matrix */
      masterId = RecvInt(wInfo, 2, -1, FREQUENCY_LIMITS);
      resCD = alloc2complex(wInfo[1] - wInfo[0] + 1, nR);
      masterId = RecvCplx(resCD[0], nR * (wInfo[1] - wInfo[0] + 1), -1,
			  COVARIANCE_PARTITION);
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
		    "Slave modeling Frechet frequency (Hz) : %f: \n", f);
	    fflush(logInfo.fp_log);
	 }
	 
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
	 dUC.i = info->tau * info->dU / wCR;
	 
	 /* wCR / wR */
	 wCRwR = wCR / info->wR;
      
	 /* auxiliary variable */
	 wCCte.r = wC.r * cte;
	 wCCte.i = wC.i * cte;

	 /* compute frequency-dependent horizontal slownesses (squared) */
	 /* and also the s-wave VELOCITIES (squared) for all layers */
	 horSlownessFrechet();
      
	 for (u = u1, iU = 0; iU < nU; iU++, u += dU, uC.r += dUC.r, 
	      uC.i += dUC.i)
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
	    /* exp(j * am * wC * (-info->zs)) */
	    auxm1 = info->zs * (- amI.r * wC.r + amI.i * wC.i);
	    auxm2 = -info->zs * (amI.r * wC.i + amI.i * wC.r);
	    g[0].r = exp(auxm1) * cos(auxm2);
	    g[0].i = exp(auxm1) * sin(auxm2);

	    /* exp(j * bm * wC * (-info->zs)) */
	    auxm1 = info->zs * (- bmI.r * wC.r + bmI.i * wC.i);
	    auxm2 = -info->zs * (bmI.r * wC.i + bmI.i * wC.r);
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
		  /*              ((auxm1, auxm2), (auxm3, auxm4)) */
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
		  
		  /* DD 
		     fprintf(stderr, 
		     "iDer %d i %d iL % d DMB[[%f %f %f %f]\n", 
		     iDer, i, iL, DmB[0][i * limRange + iL][0].r, 
		     DmB[0][i * limRange + iL][1].r, 
		     DmB[0][i * limRange + iL][2].r,
		     DmB[0][i * limRange + iL][3].r);*/
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
	    for (iR = 0; iR < info->nR; iR++)
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
	 
		  displ.r = (auxm1 + auxm3) * wCCte.r - 
		            (auxm2 + auxm4) * wCCte.i;
		  displ.i = (auxm1 + auxm3) * wCCte.i + 
		            (auxm2 + auxm4) * wCCte.r;

		  /* filtering */
		  displ.r *= window[indexF] * SGN(recArray[iR]);
		  displ.i *= window[indexF] * SGN(recArray[iR]);
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

		  displ.r = (auxm1 + auxm3) * wCCte.r - 
		            (auxm2 + auxm4) * wCCte.i;
		  displ.i = (auxm1 + auxm3) * wCCte.i + 
		            (auxm2 + auxm4) * wCCte.r;

		  /* filtering */
		  displ.r *= window[indexF];
		  displ.i *= window[indexF];
	       }
	    
	       /* computing the dot product of */
	       /* (residual^H . data covariance)  with the Frechet */
	       /* displacement vector */
	       grad[iDer] += resCD[iR][iF].r * displ.r + 
		             resCD[iR][iF].i * displ.i;
	       /* DD 
	       if (iDer == 0)
	       {
		  fprintf(logInfo.fp_log, "%f\n", grad[iDer]);
		  fprintf(logInfo.fp_log, "freq %f iR %d iDer %d grad %f\n", 
		  f, iR, iDer, grad[iDer]);
		  fprintf(logInfo.fp_log, 
			  "f %f res.r %f res.i %f displ.r %f displ.i %f\n", 
			  f,
			  resCD[iR][iF].r, resCD[iR][iF].i, displ.r, displ.i);
		  fflush(logInfo.fp_log);
	       }*/
	    }
	 }
      }
      /* sending partial gradient to the master process */
      SendFloat(grad, numberPar * limRange, masterId, PARTIAL_GRADIENT);
      
      /* DD 
      for (i = 0; i < numberPar * limRange; i++)
	 fprintf(logInfo.fp_log, "i %d grad %f\n", i, grad[i]);*/

      /* freeing memory */
      free2complex(resCD);

      /* keep working ? */
      masterId = RecvInt(&die, 1, -1, DIE);
      
      if (verbose)
      {
	 fprintf(logInfo.fp_log, "Slave receiving flag to stop : %d\n", die);
	 fflush(logInfo.fp_log);
      }
      
      if (die) break;
   }
   while (FOREVER);

   /* quitting PVM */
   EndOfSlave();
}
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
/*                                                              */
/*  Function Rp()                                               */
/*                                                              */
/*  Computing the response from the free surface                */
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
/*  rp.....................response from the free surface       */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
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
   
   aux1.r = SSlowness[0][0].r - uuC2.r;
   aux1.i = SSlowness[0][0].i - uuC2.i;
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
/*                                                              */
/*  Function Bessels()                                          */
/*                                                              */
/*  Computing Bessel functions order 1 and 0 for a given        */
/*  real argument                                               */
/*                                                              */
/*  Input parameters:                                           */
/*  arg....................argument for bessel functions        */
/*                                                              */
/*  Output parameters:                                          */
/*  J00....................bessel function order 0 evaluated    */
/*                         at arg  (global variable)            */
/*  J11....................bessel function order 1 evaluated    */
/*                         at arg  (global variable)            */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
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
/*                                                              */
/*  Function filter()                                           */
/*                                                              */
/*  Computes a windowing operator for the frequency domain      */
/*  tp avoid shar edges                                         */
/*                                                              */
/*  Input parameters:                                           */
/*  arg....................argument for bessel functions        */
/*  nSamples...............number of samples per trace          */
/*                         global                               */
/*  percW..................percentage of filtering              */
/*                                                              */
/*  Output parameters:                                          */
/*  window.................array with window coefficients       */
/*                         global                               */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
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
		 (wC.i * uC.r + uC.i * wC.r)) *  ABS(recArray[iR]);
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
/*                                                              */
/*  Function RTu()                                              */
/*                                                              */
/*  Computation of the reflection and transmission coefficients */
/*  of upgoing incident waves                                   */
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
/*  i1.....................points to the first layer            */
/*  i2.....................points to the second layer           */
/*                                                              */
/*  Output parameters:                                          */
/*  coeffU.................Reflection and transmition           */
/*                         coefficients                         */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void RTu(int i1, int i2)
{
   /* declaration of variables */
   float rho1rho2;                 /* rho1 * rho2 */
   complex aux1, aux2, aux3, c;    
   complex cuu, caux2, aux1aux3;   /* auxiliar quantities */
   complex a1, a2, b1, b2;         /* all vertical slownesses */
   complex a1b1, a1b2, a2b1, a2b2; /* auxiliar quantities */
   complex a1a2b1b2;               /* auxiliar quantities */
   complex d1u, d2u;               /* auxiliar quantities */
   complex dpda, dpdb;             /* auxiliar quantities */
   
   /* square-root of PSlowness^2 - uuC */    
   auxm1 = PSlowness[i1][0].r - uuC.r;
   auxm2 = PSlowness[i1][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a1.r = auxm3 * cos(angle);
   a1.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i1][0].r - uuC.r;
   auxm2 = SSlowness[i1][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b1.r = auxm3 * cos(angle);
   b1.i = auxm3 * sin(angle);

   /* square-root of PSlowness^2 - uuC */    
   auxm1 = PSlowness[i2][0].r - uuC.r;
   auxm2 = PSlowness[i2][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a2.r = auxm3 * cos(angle);
   a2.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i2][0].r - uuC.r;
   auxm2 = SSlowness[i2][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b2.r = auxm3 * cos(angle);
   b2.i = auxm3 * sin(angle);

   /* computing auxiliary quantities */
   rho1rho2 = rho[i1] * rho[i2];
   c.r = 2 * (rho[i1] * S2Velocity[i1][0].r - rho[i2] * S2Velocity[i2][0].r);
   c.i = 2 * (rho[i1] * S2Velocity[i1][0].i - rho[i2] * S2Velocity[i2][0].i);

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
/*                                                              */
/*  Function RTd()                                              */
/*                                                              */
/*  Computation of the reflection and transmission coefficients */
/*  of downgoing incident waves                                 */
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
/*  i1.....................points to the first layer            */
/*  i2.....................points to the second layer           */
/*                                                              */
/*  Output parameters:                                          */
/*  coeffD.................Reflection and transmition           */
/*                         coefficients                         */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void RTd(int i1, int i2)
{ 
  /* declaration of variables */

   float rho1rho2;                 /* rho1 * rho2 */
   complex aux1, aux2, aux3, c;    
   complex cuu, caux3, aux1aux2;   /* auxiliar quantities */
   complex a1, a2, b1, b2;         /* all vertical slownesses */
   complex a1b1, a1b2, a2b1, a2b2; /* auxiliar quantities */
   complex a1a2b1b2;               /* auxiliar quantities */
   complex d1d, d2d;               /* auxiliar quantities */
   complex dpda, dpdb;             /* auxiliar quantities */

   /* square-root of Pslowness^2 - uuC */    
   auxm1 = PSlowness[i1][0].r - uuC.r;
   auxm2 = PSlowness[i1][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a1.r = auxm3 * cos(angle);
   a1.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i1][0].r - uuC.r;
   auxm2 = SSlowness[i1][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b1.r = auxm3 * cos(angle);
   b1.i = auxm3 * sin(angle);

   /* square-root of PSlowness^2 - uuC */    
   auxm1 = PSlowness[i2][0].r - uuC.r;
   auxm2 = PSlowness[i2][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a2.r = auxm3 * cos(angle);
   a2.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i2][0].r - uuC.r;
   auxm2 = SSlowness[i2][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b2.r = auxm3 * cos(angle);
   b2.i = auxm3 * sin(angle);

   /* computing auxiliary quantities */
   rho1rho2 = rho[i1] * rho[i2];
   c.r = 2 * (rho[i1] * S2Velocity[i1][0].r - rho[i2] * S2Velocity[i2][0].r);
   c.i = 2 * (rho[i1] * S2Velocity[i1][0].i - rho[i2] * S2Velocity[i2][0].i);

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
