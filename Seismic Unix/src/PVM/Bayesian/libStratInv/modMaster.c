/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*                                                              */
/*  Function modeling()                                         */
/*                                                              */
/*  Computing synthethic seismograms for a given model          */
/*  returning the current residual.                             */
/*  This is a distributed implementation, in which this         */
/*  subroutine plays the role of a master processor             */
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
/*  id.....................indicates which module called this   */
/*                         function                             */
/*                                                              */
/*  Output parameters:                                          */
/*  res....................residual data in time domain         */
/*                         global                               */
/*  of.....................value of objective function          */
/*                         in Fourier domain                    */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
#include "stratInv.h"
float modeling()
{
   /* declaration of variables */
   FILE *fp;                       /* to report results */
   int iF, iF1, iR, offset, iT1, iT2, iS, iProc, i, k;
                                   /* counters */
   int wL;                         /* window length */
   int die;                        /* die processor flag */
   int FReceived;                  /* number of frequencies processed */
   int apl_pid;                    /* PVM process id control */
   int pid;                        /* process id */
   int processControl;             /* monitoring PVM start */
   int FInfo[2];                   /* frequency delimiters */
   float wallcpu;                  /* wall clock time */
   float oF;                       /* value of the objective function */
   float residue;                  /* data residue */
   float wdw;                      /* windowing purposes */
   float *buffer, *bufferRCD;      /* auxiliary buffers */
                                   /* upgoing waves */
   complex **dataS;                /* synthethics in the frequency domain */
   complex *bufferC;               /* auxiliary buffer */
   complex **freqPart;             /* frequency arrays sent by the slaves */
   
   /* Clean up log files */
   CleanLog();

   /* Reseting synchronization flags */
   for (i = 0; i < nFreqPart; i++)
   {
      statusFreq[i][2] = 0;
   }
    
   /* allocating some memory */
   dataS = alloc2complex(info->nF, info->nR);
   buffer = alloc1float(info->nSamples);
   bufferRCD = alloc1float(info->nSamples);
   bufferC = alloc1complex(info->nSamples / 2 + 1);
   freqPart = alloc2complex(info->nFreqProc, info->nR);

   /* reseting */
   for (iF = 0; iF < info->nSamples / 2 + 1; iF++)
      bufferC[iF] = zeroC;
   for (iS = 0; iS < info->nSamples; iS++)
   {
      buffer[iS] = 0; bufferRCD[iS] = 0;
   }

   /* DD 
   fprintf(stderr, "nF -> %d\n", info->nF);*/
   fprintf(stderr, "Starting communication with PVM for modeling\n");

   /* starting communication with PVM */
   if ((apl_pid = pvm_mytid()) < 0) 
   {
      pvm_perror("Error enrolling master process");
      exit(-1);
   }
   processControl = CreateSlaves(processes, PROCESS_MODELING, nProc);
   
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
		 "Master sending frequencies [%d, %d] out of %d to slave Modeling %d [id:%d]\n", FInfo[0], FInfo[1], info->nF, iProc, processes[iProc]);
      
      procInfo[iProc][0] = FInfo[0];
      procInfo[iProc][1] = FInfo[1];
      SendInt(FInfo, 2, processes[iProc], FREQUENCY_LIMITS);
      statusFreq[iProc][2] = 1;
   }

   /* waiting modelled frequencies */
   /* master process will send more frequencies if there's more work to do */
   /* measuring elapsed time */
   wallcpu = walltime();

   /* reseting frequency counter */
   FReceived = 0;

   while (FOREVER)
   {
      pid = RecvCplx(freqPart[0], info->nR * info->nFreqProc, -1, 
		     FREQUENCY_PARTITION);

      /* finding the frequency limits of this process */
      /* DD 
      fprintf(stderr, "Master finding the frequency limits of this process\n");
      */

      iProc = 0;
      while (pid != processes[iProc])
	 iProc++;

      /* DD 
      fprintf(stderr, "iProc %d pid %d\n", iProc, pid);*/

      /* copying into proper place of the total frequency array */
      for (iR = 0; iR < info->nR; iR++)
      {
	 for (k = 0, i = procInfo[iProc][0]; i <= procInfo[iProc][1]; i++, k++)
	 {
	    dataS[iR][i - initF] = freqPart[iR][k];
	 }
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
	    fprintf(stderr, "Master sending frequencies [%d, %d] to slave %d\n", FInfo[0], FInfo[1], processes[iProc]);

	 procInfo[iProc][0] = FInfo[0];
	 procInfo[iProc][1] = FInfo[1];
	 SendInt(FInfo, 2, processes[iProc], FREQUENCY_LIMITS);
	 statusFreq[i][2] = 1;
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
   
   /* quitting PVM */
   EndOfMaster();
   
   /* getting elapsed time */
   wallcpu = walltime() - wallcpu;
   fprintf(stderr, "Modeling wall clock time = %f seconds\n", 
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

   /* computing the objective function for the time window */
   for (oF = 0, residue = 0, iR = 0; iR < info->nR; iR++)
   {
      /* windowing as it was done to the input data */
      iT1 = NINT(info->f1 / info->dF);
      iT2 = NINT(info->f2 / info->dF);
      wL = info->nF * PERC_WINDOW / 2;
      wL = 2 * wL + 1;
      for (iS = 0, iF = 0; iF < info->nSamples / 2 + 1; iF++)
      {
	 if (iF < iT1 || iF >= iT2)
         {
            bufferC[iF] = cmplx(0, 0);
         }
         else if (iF - iT1 < (wL - 1) / 2)
         {
            wdw = .42 - .5 * cos(2 * PI * (float) iS / ((float) (wL - 1))) +
                  .08 * cos(4 * PI * (float) iS / ((float) (wL - 1)));
	    bufferC[iF].r = dataS[iR][iF - iT1].r * wdw;
	    bufferC[iF].i = dataS[iR][iF - iT1].i * wdw;
            iS++;
         }
         else if (iF - iT1 >= info->nF - (wL - 1) / 2)
         {
            iS++;
            wdw = .42 - .5 * cos(2 * PI * (float) iS / ((float) (wL - 1))) +
                  .08 * cos(4 * PI * (float) iS / ((float) (wL - 1)));
	    bufferC[iF].r = dataS[iR][iF - iT1].r * wdw;
	    bufferC[iF].i = dataS[iR][iF - iT1].i * wdw;
         }
	 else
	 {
	    bufferC[iF] = dataS[iR][iF - iT1];
	 }
      }
      
      /* going to time domain */
      /* DD 
      fprintf(stderr, "going to time domain \n");*/

      pfacr(1, info->nSamples, bufferC, buffer);

      /* muting ? */
      if (MUTE)
      {
         for (iS = 0; iS <= NINT(t1Mute[iR] / dt); iS++)
         {
	    buffer[iS] = 0;
         }
      }

      /* and computing data misfit and likelihood function */
      iS = NINT(t1 / dt);
      for (iT1 = 0; iT1 < nDM; iT1++)
      {
	 bufferRCD[iT1 + iS] = 0;

	 for (offset = iT1, iT2 = 0; iT2 < nDM; iT2++)
	 {
	    bufferRCD[iT1 + iS] +=  
	                   (buffer[iT2 + iS] - dataObs[iR][iT2]) * CD[offset];
	    offset += MAX(SGN0(iT1 - iT2) * (nDM - 1 - iT2), 1);
	 }
	 oF += (buffer[iT1 + iS] - dataObs[iR][iT1]) * bufferRCD[iT1 + iS];

	 residue += (buffer[iT1 + iS] - dataObs[iR][iT1]) * 
                    (buffer[iT1 + iS] - dataObs[iR][iT1]);

	 /* DD 
	 fprintf(stdout, "%d %f %f %f %f %f %d %f %f\n", 
		 nTotalSamples, oF, dt, auxm1, 
		 info->tau, residue, iT1, buffer[iT1], 
		 dataObs[iR][iT1 - NINT(t1 / dt)]); */
      }

      /* windowing bufferRCD */
      iT1 = NINT(t1 / dt);
      iT2 = NINT(t2 / dt);
      wL = nDM * PERC_WINDOW / 2;
      wL = 2 * wL + 1;
      for (iS = 0, iF = 0; iF < info->nSamples; iF++)
      {
         if (iF < iT1 || iF >= iT2)
         {
            bufferRCD[iF] = 0;
         }
	 else if (iF - iT1 < (wL - 1) / 2)
         {
            wdw =
               .42 - .5 * cos(2 * PI * (float) iS / ((float) (wL - 1))) +
                  .08 * cos(4 * PI * (float) iS / ((float) (wL - 1)));
            bufferRCD[iF] *= wdw;
            iS++;
         }
         else if (iF - iT1 >= nDM - (wL - 1) / 2)
         {
            iS++;
            wdw =
               .42 - .5 * cos(2 * PI * (float) iS / ((float) (wL - 1))) +
                  .08 * cos(4 * PI * (float) iS / ((float) (wL - 1)));
            bufferRCD[iF] *= wdw;
         }
      }
      
      /* going back to Fourier domain */
      pfarc(-1, info->nSamples, bufferRCD, bufferC);          
      
      for (iF1 = 0, iF = NINT(info->f1 / info->dF); 
	   iF <= NINT(info->f2 / info->dF); iF++, iF1++)
      {
	 resCD[iR][iF1] = bufferC[iF];
      }
   }

   /* considering the .5 factor of the exponent of the Gaussian */
   /* and normalizing the likelihood by the number of samples */
   oF /= (2 * nTotalSamples);

   /* freeing some memory */
   /* allocating some memory */
   free2complex(dataS);
   free1float(buffer);
   free1float(bufferRCD);
   free1complex(bufferC);
   free2complex(freqPart);

   /* considering the regularizaton or model covariance term */
   if (PRIOR)
   {
      auxm1 = 1. / (float) (numberPar * limRange);     /* normalization */
      for (auxm2 = 0, iF = 0; iF < limRange; iF++)
      {
	 for (offset = iF, iF1 = 0; iF1 < limRange; iF1++)
	 {
	    if (vpFrechet)
	    {
	       auxm2 += (alpha[iF + lim[0]] - alphaMean[iF + lim[0]]) * 
		         CMvP[offset] * auxm1 * 
		        (alpha[iF1 + lim[0]] - alphaMean[iF1 + lim[0]]);
	    }
	    
	    if (vsFrechet)
	    {
	       auxm2 += (beta[iF + lim[0]] - betaMean[iF + lim[0]]) * 
	                 CMvS[offset] * auxm1 *
		        (beta[iF1 + lim[0]] - betaMean[iF1 + lim[0]]);
	    }
	    
	    if (rhoFrechet)
	    {
	       auxm2 += (rho[iF + lim[0]] - rhoMean[iF + lim[0]]) * 
		         CMrho[offset] * auxm1 *
		        (rho[iF1 + lim[0]] - rhoMean[iF1 + lim[0]]);
	    }
	    offset += MAX(SGN0(iF - iF1) * (limRange - 1 - iF1), 1);
	 }
      }
   }
   /* getting normalization factor */
   fp = fopen("report", "a");
   fprintf(fp,"-----------------------\n");

   if (modCount == 0) 
   {
      oFNorm = oF;
      fprintf(fp,">> Normalization constant for objective function: %f <<\n",
	      oFNorm);
   }
   
   /* normalizing residue */
   residue /= (nTotalSamples);

   if (!DATACOV && noiseVar == 0) noiseVar = residue / 10.;
   
   if (PRIOR)
   {
      fprintf(fp,
      "residue at iteration [%d] : Data residue variance %f , Noise variance %f , Likelihood %f , Prior %f\n", 
      modCount, residue, noiseVar, oF / oFNorm, auxm2 / oFNorm);
   }
   else
   {
      fprintf(fp,"residue at iteration [%d] : Data residue variance %f , Noise variance %f , Likelihood %f , No Prior\n", modCount, residue, noiseVar, oF / oFNorm);
   }

   /* checking if we reached noise variance with the data residue */
   if (residue / noiseVar <= 1)
   {
      /* DATA IS FIT, stop the procedure */
      fprintf(fp, "[][][][][][][][][][][][][][][][][][][][]\n");
      fprintf(fp, "DATA WAS FIT UP TO 1 VARIANCE!\n");
      fprintf(fp, "[][][][][][][][][][][][][][][][][][][][]\n");
      exit(0);
   }
   
   /* adding Likelihood and Prior */
   if (PRIOR) oF += auxm2 / 2;
   fprintf(fp,"TOTAL residue at iteration [%d] : %f\n", 
	   modCount, oF / oFNorm);

   fprintf(fp,"-----------------------\n");
   fclose(fp);


   /* returning objective function value */
   return(oF / oFNorm);
}
