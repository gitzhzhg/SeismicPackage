/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*                                                              */
/*  Function gradient()                                         */
/*                                                              */
/*  Compute the FRECHET derivatives for the elastic             */
/*  modeling. This is a distributed implementation.             */
/*                                                              */
/*  This routine also computes the dot product of the Frechet   */
/*  detivative matrix and the search direction                  */
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
#include "stratInv.h"
void gradient(float *grad)
{
   /* declaration of variables */
   int i, iF, iR, iProc, iDer, iL, iU, offset; 
                                   /* counters */
   int FReceived;                  /* number of frequencies processed */
   int die;                        /* die processor flag */ 
   int apl_pid;                    /* PVM process id control */
   int pid;                        /* process id */
   int masterId;                   /* master id */
   int processControl;             /* monitoring PVM start */
   int FInfo[2];                   /* frequency delimiters */
   float wallcpu;                   /* wall clock time */     
   float *gradPart;                 /* partition of gradients */
   complex **resCDPart;             /* partition of resCD */
   
   /* Clean up log files */
   CleanLog();
     
   /* Reseting synchronization flags */
   for (i = 0; i < nFreqPart; i++)
   {
      statusFreq[i][2] = 0;
   }
      
   /* allocating some memory */
   gradPart = alloc1float(numberPar * limRange);
    
   for (i = 0; i < numberPar * limRange; i++)
   {
      grad[i] = 0;
   }
   
   fprintf(stderr, "Starting communication with PVM for derivatives\n");
   /* starting communication with PVM */
   if ((apl_pid = pvm_mytid()) < 0)
   {
      pvm_perror("Error enrolling master process");
      exit(-1);
   }
   processControl = CreateSlaves(processes, PROCESS_FRECHET, nProc);
   
   if (processControl != nProc)
   {
      fprintf(stderr,"Problem starting PVM daemons\n");
      exit(-1);
   }
      
   /* converting to velocities */
   if (IMPEDANCE)
   {
      for (i = 0; i < info->nL + 1; i++)
      {
         alpha[i] /= rho[i];
         beta[i] /= rho[i];
      }
   }

   /* Broadcasting all processes common information */
   BroadINFO(info, 1, processes, nProc, GENERAL_INFORMATION);
   
   /* sending all profiles */
   BroadFloat(thick, info->nL + 1, processes, nProc, THICKNESS);
   BroadFloat(rho, info->nL + 1, processes, nProc, DENSITY);
   BroadFloat(alpha, info->nL + 1, processes, nProc, ALPHAS);
   BroadFloat(qP, info->nL + 1, processes, nProc, QALPHA);
   BroadFloat(beta, info->nL + 1, processes, nProc, BETAS);
   BroadFloat(qS, info->nL + 1, processes, nProc, QBETA);

   /* sending frequency partitions for each process */
   for (iProc = 0; iProc < nProc; iProc++)
   {
      FInfo[0] = statusFreq[iProc][0];
      FInfo[1] = statusFreq[iProc][1];
      if (info->verbose)
	 fprintf(stderr, 
	 "Master sending frequencies [%d, %d] out of %d to slave Frechet %d [id:%d]\n", FInfo[0], FInfo[1], info->nF, iProc, processes[iProc]);

      procInfo[iProc][0] = FInfo[0];
      procInfo[iProc][1] = FInfo[1];
      SendInt(FInfo, 2, processes[iProc], FREQUENCY_LIMITS);
      statusFreq[iProc][2] = 1;
      
      /* and sending the appropriate correlation chunk */
      /* allocating some memory */
      resCDPart = alloc2complex(FInfo[1] - FInfo[0] + 1, info->nR);

      for (iR = 0; iR < info->nR; iR++)
      {
	 for (i = 0, iF = FInfo[0]; iF <= FInfo[1]; iF++, i++)
	 {
	    resCDPart[iR][i] = resCD[iR][iF - initF];
/*	    fprintf(stderr, "iR %d iF %d [%f %f]\n",
		    iR, iF, resCDPart[iR][i].r, resCDPart[iR][i].i);*/
	 }
      }
      
      /* sending frequency partition to the slave process */
      SendCplx(resCDPart[0], (FInfo[1] - FInfo[0] + 1) * info->nR, 
	       processes[iProc], COVARIANCE_PARTITION);
      free2complex(resCDPart);
   }
   /* waiting modelled frequencies */
   /* master process will send more frequencies if there's more work to do */
   /* measuring elapsed time */
   wallcpu = walltime();


   /* reseting frequency counter */
   FReceived = 0;
   while (FOREVER)
   {
      pid = RecvFloat(gradPart, info->numberPar * info->limRange, -1,
		     PARTIAL_GRADIENT);

      /* finding the frequency limits of this process */
      /* DD 
 fprintf(stderr, "Master finding the frequency limits of this process\n");
      */

      iProc = 0;
      while (pid != processes[iProc])
	 iProc++;
	                       
      /* stacking gradient */
      for (i = 0; i < info->numberPar * info->limRange; i++)
      {
	 grad[i] += gradPart[i];
	 /* DD
	 fprintf(stderr, "i %d grad %f gradPart %f\n", i, grad[i], gradPart[i]);*/
      }
      
      /* summing frequencies that are done */
      FReceived += procInfo[iProc][1] - procInfo[iProc][0] + 1;
      if (info->verbose)
	 fprintf(stderr, "Master received %d frequencies, remaining %d\n",
		 FReceived, info->nF - FReceived);
             
      /* defining new frequency limits */
      i = 0;
      while (i < nFreqPart && statusFreq[i][2])
	 i++;

      /* DD 
      fprintf(stderr, "i %d nFreqPart %d\n", i, nFreqPart);*/
           
      if (i < nFreqPart)
      {
	 /* there is still more work to be done */
	 /* tell this process to not die */
	 die = 0;
	 SendInt(&die, 1, processes[iProc], DIE);
	 FInfo[0] = statusFreq[i][0];
	 FInfo[1] = statusFreq[i][1];
	 
	 if (info->verbose)
	    fprintf(stderr, 
		    "Master sending frequencies [%d, %d] to slave %d\n",
		    FInfo[0], FInfo[1], processes[iProc]);

	 procInfo[iProc][0] = FInfo[0];
	 procInfo[iProc][1] = FInfo[1];
	 SendInt(FInfo, 2, processes[iProc], FREQUENCY_LIMITS);
	 statusFreq[i][2] = 1;

	 /* sending covariance partition */
	 /* allocating some memory */
	 resCDPart = alloc2complex(FInfo[1] - FInfo[0] + 1, info->nR);

	 for (iR = 0; iR < info->nR; iR++)
	 {
	    for (i = 0, iF = FInfo[0]; iF <= FInfo[1]; iF++, i++)
	    {
	       resCDPart[iR][i] = resCD[iR][iF - initF];
	    }
	 }
	 /* sending frequency partition to the slave process */
	 SendCplx(resCDPart[0], (FInfo[1] - FInfo[0] + 1) * info->nR, 
		  processes[iProc], COVARIANCE_PARTITION);
	 free2complex(resCDPart);
      }
      else
      {
	 /* tell this process to die since there is no more work to do */
	 if (info->verbose)
	    fprintf(stderr, "Master ''killing'' slave %d\n", processes[iProc]);
	 die = 1;
	 SendInt(&die, 1, processes[iProc], DIE);
      }
      /* a check to get out the loop */
      if (FReceived >= info->nF) break;
   }

   /* getting elapsed time */
   wallcpu = walltime() - wallcpu;
   fprintf(stderr, "Frechet derivative wall clock time = %f seconds\n\n", 
	   wallcpu);   
   
   /* back to impedances*/
   if (IMPEDANCE)
   {
      for (i = 0; i < info->nL + 1; i++)
      {
         alpha[i] *= rho[i];
         beta[i] *= rho[i];
      }
   }

   /* finally the gradient, the 2 is due Parseval */
   for (iDer = 0; iDer < numberPar * limRange; iDer++)
   {
      grad[iDer] *= 2 / (float) (nTotalSamples * oFNorm);
   }

   /* getting gradient in impedance domain */
   if (IMPEDANCE)
   {
      offset = 0;
      for (i = lim[0], iL = 0; iL < limRange; iL++, i++)
      {
         if (vpFrechet) 
         {
            grad[iL] /= rho[i];
            offset = limRange;
	 }
	 
         if (vsFrechet) 
         {
            grad[iL + offset] /= rho[i];
            offset += limRange;
	 }
	 
         if (rhoFrechet)
         {
            grad[iL + offset] = - alpha[i] * grad[iL] -
	      beta[i] * grad[iL + limRange] + grad[iL + 2 * limRange];
         }
      }
   }

   if (PRIOR)
   {
      auxm1 = 1. / (float) (numberPar * limRange);     /* normalization */
      /* considering the regularization or model covariance term */
      for (i = 0; i < limRange; i++)
      {
	 for (offset = i, iL = 0; iL < limRange; iL++)
	 {
	    iU = 0;
	    if (vpFrechet)
	    {
	       grad[iL] += (alpha[i + lim[0]] - alphaMean[i + lim[0]]) * 
		            CMvP[offset] * auxm1;
	       iU = limRange; /* used as offset in gradient vector */
	    }
	    
	    if (vsFrechet)
	    {
	       grad[iL + iU] += (beta[i + lim[0]] - betaMean[i + lim[0]]) * 
	 	                 CMvS[offset] * auxm1;
	       iU += limRange;
	    }

	    if (rhoFrechet)
	    {
	       grad[iL + iU] += (rho[i + lim[0]] - rhoMean[i + lim[0]]) * 
		                 CMrho[offset] * auxm1;
	    }

	    offset += MAX(SGN0(i - iL) * (limRange - 1 - iL), 1);
	 }
      }
   }

   /* normalizing gradient 
   normalize(grad, numberPar * limRange);*/
   /* freeing memory */
   free1float(gradPart);
}
