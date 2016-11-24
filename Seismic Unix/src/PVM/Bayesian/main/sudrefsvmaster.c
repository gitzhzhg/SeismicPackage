/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "refMaster.h"

segy trR, trZ;     /* SEGY trace */
/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                        ",
" SUDREFSVMASTER - Distributed REFlectivity modeling MASTER code for     ",
"                  the generation of the vertical and radial component   ",
"                  (SV) of the displacement field due to a point         ",
"                  force. In this code a master-slave algorithm is       ",
"                  implemented. A master processor sends several         ",
"                  frequency partitions to different processors. After   ",
"                  modeling its frequency range the slave processor      ",
"                  sends the result back to the master. The frequency    ",
"                  partitions that are still to be modeled, if any, are  ",
"                  sent by the master to the available slaves. After     ",
"                  composition all the frequency partitions the master   ",
"                  outputs the modeled time seismograms to stdout.       ",
"                                                                        ",
"                  The code SUDREFSVSLAVE implements the slave functions ",
"                  and should be installed with SUDREFSVMASTER. The      ",
"                  communication and synchronization routines are        ",
"                  provided by the PVM library. The version used by      ",
"                  SUDREFSVMASTER is the version 3.3.9 or higher. The user ",
"                  MUST install PVM prior to the installation of this    ",
"                  code.                                                 ",
"                                                                        ",
" Many of the input parameters will be better understood if the reader   ",
" is familiar with the following two references:                         ",
"                                                                        ",
" Muller, G: The Reflectivity Method: a Tutorial, Journal of Geophysics  ",
"            (1985) 58: 153-174                                          ",
"                                                                        ",
" Mallick S. and Fraser N.: Practical Aspects of Reflectivity Modeling   ",
"            Geophysics (1987) 52 no 10: 1355-1364                       ",
"                                                                        ",
"                                                                        ",
" SUDREFSVMAIN requires the following input parameters (Values between   ",
" brackets represent the default value of the parameter).                ",
"                                                                        ",
" model=[model]  Elastic mode ASCII file. This file should contain one   ",
"                row per layer of the model in the following format:     ",
"                                                                        ",
"                Thick(km)  Rho(g/cm^3)  Vp(km/s)  Qp  Vs(km/s)  Qs      ",
"                                                                        ",
"                Where:                                                  ",
"                Thick: Layer thickness                                  ",
"                Rho:   Layer density                                    ",
"                Vp:    P-wave velocity                                  ",
"                Qp:    P-wave quality factor                            ",
"                Vs:    S-wave velocity                                  ",
"                Qs:    S-wave quality factor                            ",
" recfile=[NULL] File with receiver offsets. The format is one offset    ",
"                line. If none is specified (default), the acquisition   ",
"                is end on                                               ",
" directwave=1   Direct wave flag. If zero direct wave is not generated  ",
" r1=[0]         Minimum source-receiver offset (km)                     ",
" nr=[48]        Number of receivers                                     ",
" dr=[0.025]     Receiver spacing (km)                                   ",
" zs=[0.001]     Source depth (km)                                       ",
" u1=[0.0002]    Initial horizontal slowness (s/km)                      ",
" u2=[1]         Final horizontal slowness (s/km)                        ",
" nu=[1000]      Number of slownesses                                    ",
" f1=[2]         Initial frequency for modeling (Hz)                     ",
" f2=[50]        Final frequency for modeling (Hz)                       ",
" dt=[0.004]     Time sampling interval (0.004)                          ",
" tmax=[8]       Maximum modeling time (s)                               ",
" F1=[0]         EAST-WEST point force coordinate                        ",
" F2=[0]         NORTH-SOUTH point force coordinate                      ",
" F3=[1]         VERTICAL point force coordinate                         ",
" Hanning=[1]    Final trace is convolved with a hanning window          ",
" wu=[5]         Percentual of slowness interval that is windowed        ",
" ww=[5]         Percentual of frequency interval that is windowed       ",
" fr=[1]         Reference frequency used in absorption model            ",
" tau=[50]       Wrap-around attenuation parameter                       ",
" nProc=[1]      Number of processors used in the computation            ",
" nfreqproc=[0]  Number of frequencies allocated to each processor       ",
"                The default value means that the frequency              ",
"                components will be evenly split among the slave         ",
"                processors                                              ",
" verbose=[0]    =1 print useful information                             ",
"                                                                        ",
" SUDREFSVMASTER output the synthetic data in SU format to standard      ",
" output. First the vertical then the horizontal component.              ",
"                                                                        ",
" Reminder: YOU SHOULD INSTALL PVM (version 3.3.9 or higher) BEFORE THE  ",
"           INSTALLATION OF THESE CODES                                  ",
"                                                                        ",
" Notes on execution:                                                    ",
"                                                                        ",
" You should start PVM before the execution of SUDREFSVMASTER. Each slave",
" will generate a report file names PSU[XXX], where [XXX] is the process ",
" id number. Those files are placed in the directory specified by the    ",
" environment variable PVMLOG.                                           ",
"                                                                        ",
" Limitations of this version:                                           ",
"                                                                        ",
" 1) No SH displacement field is generated                               ",
"                                                                        ",
" 2) Point source (explosion like) is not implemented                    ",
"                                                                        ",
"                                 Author: Wences Gouveia - 95-11-29      ",
"                                 Center For Wave Phenomena              ",
"                                 Colorado School of Mines               ",
NULL};   
/************************ end self doc ***********************************/ 

void
main (int argc, char **argv)
{
   /* declaration of variables */
   FILE *fp;                     /* file pointer */
   char *auxChar;                /* auxiliar character */
   char *modelFile = " ";        /* elastic model file */
                                 /* THICK - RHO - VP - QP - VS - QS */
   int i, k, iProc, iR;          /* counters */
   int initF, lastF;             /* initial and final frequencies */
   int apl_pid;                  /* PVM process id control */
   int nSamplesOrig;             /* time series length */
   int die;                      /* flag used to kill processes */
   int pid;                      /* process id */
   int nProc;                    /* number of processes */
   int processControl;           /* monitoring PVM start */
   int *processes;               /* array with process ids */
   int FReceived;                /* number of frequencies processed */
   int nFreqProc;                /* number of frequencies per process */
   int nFreqPart;                /* number of frequency partitions */
   int **statusFreq;             /* monitors processed frequencies */
   int FInfo[2];                 /* frequency delimiters */
   int **procInfo;               /* frequency limits for each processor */ 
   float wallcpu;                /* wall clock time */
   float dt;                     /* time sampling interval */
   float f;                      /* current frequency */
   float fR;                     /* reference frequency */
   float tMax;                   /* maximum recording time */
   float *thick, *alpha, *beta,
   *rho, *qP, *qS;               /* elastic constants and thickness */
   complex **freqPart;           /* frequency arrays sent by the slaves */
   complex **uRF, **uZF;         /* final frequency components */
   INFO info[1];                 /* basic information for slaves */
   
   /* Logging information */
   CleanLog();

   /* getting input */
   initargs(argc, argv);
   requestdoc(0);
   
   if (!getparstring("model", &modelFile)) modelFile = "model";
   if (!getparstring("recfile", &auxChar)) auxChar = " ";
   sprintf(info->recFile, "%s", auxChar);
   if (!getparint("directwave", &info->directWave)) info->directWave = 1;
   if (!getparfloat("r1", &info->r1)) info->r1 = 0;
   if (!getparint("nr", &info->nR)) info->nR = 148;
   if (!getparfloat("dr", &info->dR)) info->dR = .025;
   if (!getparfloat("zs", &info->zs)) info->zs = 0.001;
   if (info->zs <= 0) info->zs = 0.001;
   if (!getparfloat("u1", &info->u1)) info->u1 = 0.0002;
   if (!getparfloat("u2", &info->u2)) info->u2 = 1.;
   if (!getparint("nu", &info->nU)) info->nU = 1000;
   if (!getparfloat("f1", &info->f1)) info->f1 = 2;
   if (!getparfloat("f2", &info->f2)) info->f2 = 50;
   if (!getparfloat("dt", &dt)) dt = 0.004;
   if (!getparfloat("tmax", &tMax)) tMax = 8;
   if (!getparfloat("F1", &info->F1)) info->F1 = 0;
   if (!getparfloat("F2", &info->F2)) info->F2 = 0;
   if (!getparfloat("F3", &info->F3)) info->F3 = 1;
   if (!getparint("hanning", &info->hanningFlag)) info->hanningFlag = 0;
   if (!getparfloat("wu", &info->percU)) info->percU = 5; info->percU /= 100;
   if (!getparfloat("ww", &info->percW)) info->percW = 5; info->percW /= 100;
   if (!getparfloat("fr", &fR)) fR = 1; info->wR = 2 * PI * fR;
   if (!getparfloat("tau", &info->tau)) info->tau = 50;
   if (!getparint("nproc", &nProc)) nProc = 1;
   if (!getparint("nfreqproc", &nFreqProc) || nProc == 1) nFreqProc = 0;
   if (!getparint("verbose", &info->verbose)) info->verbose = 0;

   /* how many layers */
   fp = fopen(modelFile,"r");
   if (fp == NULL)
      err("No model file!\n");

   info->nL = 0;
   while (fscanf(fp, "%f %f %f %f %f %f\n", 
		 &f, &f, &f, &f, &f, &f) != EOF)
      info->nL++;
   info->nL--;
   fclose(fp);

   if (info->verbose)
      fprintf(stderr,"Number of layers in model %s : %d\n", 
	      modelFile, info->nL + 1); 
   
   /* if specific geometry, count number of receivers */
   fp = fopen(info->recFile, "r");
   if (fp != NULL)
   {
      info->nR = 0;
      while (fscanf(fp, "%f\n", &f) != EOF)
	 info->nR++;
   fclose(fp);
   }

   /* memory allocation */
   alpha = alloc1float(info->nL + 1);
   beta = alloc1float(info->nL + 1);
   rho = alloc1float(info->nL + 1);
   qP = alloc1float(info->nL + 1);
   qS = alloc1float(info->nL + 1);
   thick = alloc1float(info->nL + 1);
   processes = alloc1int(nProc);
   procInfo = alloc2int(2, nProc);

   /* reading the file */
   fp = fopen(modelFile,"r");
   if (info->verbose)
      fprintf(stderr,"Thickness     rho     vP     qP    vS     qS\n");
   for (i = 0; i < info->nL + 1; i++)
   {
      fscanf(fp, "%f %f %f %f %f %f\n", &thick[i], &rho[i], &alpha[i], 
	     &qP[i], &beta[i], &qS[i]);
      if (info->verbose)
	 fprintf(stderr,"   %7.4f      %4.3f   %3.2f  %5.1f  %3.2f  %5.1f\n",
		 thick[i], rho[i], alpha[i], qP[i], beta[i], qS[i]);
   }
   fclose(fp);

   /* computing frequency interval */
   info->nSamples = NINT(tMax / dt) + 1;
   nSamplesOrig = info->nSamples;
   info->nSamples = npfar(info->nSamples);

   /* slowness increment */
   info->dU = (info->u2 - info->u1) / (float) info->nU;

   /* computing more frequency related quatities */
   tMax = dt * (info->nSamples - 1);
   info->dF = 1. / (tMax);   
   f = info->dF;
   while (f < info->f1) f += info->dF;
   info->f1 = f;
   while (f < info->f2) f += info->dF;
   info->f2 = f; 
   initF = NINT(info->f1 / info->dF);
   lastF = NINT(info->f2 / info->dF);

   info->nF = NINT(info->f2 / info->dF) - NINT(info->f1 / info->dF) + 1;

   if (info->nF%2 == 0)
   {
      info->f2 += info->dF;
      info->nF++;
   }
   initF = NINT(info->f1 / info->dF);
   lastF = NINT(info->f2 / info->dF);  

   /* attenuation of wrap-around */
   info->tau = log(info->tau) / tMax;
   if (info->tau > TAUMAX)
      info->tau = TAUMAX;
      
   if (info->verbose)
      fprintf(stderr, "Discrete frequency range to model: [%d, %d]\n", 
	      initF, lastF);
   
   if (nFreqProc == 0)
   {
      nFreqProc = NINT((float) info->nF / (float) nProc + .5);
      if (nFreqProc > info->nF) nFreqProc = info->nF;
   }
   else
      while (nFreqProc > info->nF) nFreqProc /= 2;
   nFreqPart = NINT((float) info->nF / (float) nFreqProc + .5);

   /* memory allocation for frequency arrays */
   uRF = alloc2complex(info->nSamples / 2 + 1, info->nR);
   uZF = alloc2complex(info->nSamples / 2 + 1, info->nR);
   freqPart = alloc2complex(nFreqProc, info->nR);
   statusFreq = alloc2int(3, nFreqPart);

   /* defining frequency partitions */
   for (k = initF, i = 0; i < nFreqPart; i++, k += nFreqProc)
   {
      statusFreq[i][0] = k;
      statusFreq[i][1] = MIN(k + nFreqProc - 1, lastF);
      statusFreq[i][2] = 0;       
   }

   if (info->verbose)
      fprintf(stderr, "Starting communication with PVM\n");
   
   /* starting communication with PVM */
   if ((apl_pid = pvm_mytid()) < 0) 
   {
      pvm_perror("Error enrolling master process");
      exit(-1);
   } 
   processControl = CreateSlaves(processes, PROCESS, nProc);
   if (processControl != nProc)
   {
      fprintf(stderr,"Problem starting PVM daemons\n");
      exit(-1);
   }

   info->nFreqProc = nFreqProc;
   /* Broadcasting all processes common information */
   BroadINFO(info, 1, processes, nProc, GENERAL_INFORMATION);
   
   if (info->verbose)
      fprintf(stderr, "Broadcasting model information to all slaves\n");

   /* sending all profiles */
   BroadFloat(thick, info->nL + 1, processes, nProc, THICKNESS);
   BroadFloat(rho, info->nL + 1, processes, nProc, DENSITY);
   BroadFloat(alpha, info->nL + 1, processes, nProc, ALPHA);
   BroadFloat(qP, info->nL + 1, processes, nProc, QALPHA);
   BroadFloat(beta, info->nL + 1, processes, nProc, BETA);
   BroadFloat(qS, info->nL + 1, processes, nProc, QBETA);

   /* freeing memory */
   free1float(thick);
   free1float(rho);
   free1float(alpha);
   free1float(qP);
   free1float(beta);
   free1float(qS);

   /* sending frequency partitions for each process */
   for (iProc = 0; iProc < nProc; iProc++)
   {
      FInfo[0] = statusFreq[iProc][0];
      FInfo[1] = statusFreq[iProc][1];

      if (info->verbose)
	 fprintf(stderr, 
	 "Master sending frequencies [%d, %d] out of %d to slave %d [id:%d]\n"
	  ,FInfo[0], FInfo[1], info->nF, iProc, processes[iProc]);

      procInfo[iProc][0] = FInfo[0]; procInfo[iProc][1] = FInfo[1];
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
      pid = RecvCplx(freqPart[0], info->nR * nFreqProc, -1, 
		     FREQUENCY_PARTITION_VERTICAL);

      /* finding the frequency limits of this process */
      iProc = 0;
      while (pid != processes[iProc])
	 iProc++;

      /* copying into proper place of the total frequency array */
      for (iR = 0; iR < info->nR; iR++)
      {
	 for (k = 0, i = procInfo[iProc][0]; i <= procInfo[iProc][1]; i++, k++)
	 {
	    uZF[iR][i] = freqPart[iR][k];
	 }
      }

      pid = RecvCplx(freqPart[0], info->nR * nFreqProc, -1, 
		     FREQUENCY_PARTITION_RADIAL);
      
      /* finding the frequency limits of this process */
      iProc = 0;
      while (pid != processes[iProc])
	 iProc++;
   
      /* copying into proper place of the total frequency array */
      for (iR = 0; iR < info->nR; iR++)
      { 
	 for (k = 0, i = procInfo[iProc][0]; i <= procInfo[iProc][1]; i++, k++)
	 {
	    uRF[iR][i] = freqPart[iR][k];
	 }
      }

      /* summing frequencies that are done */
      FReceived += procInfo[iProc][1] - procInfo[iProc][0] + 1;

      if (info->verbose)
	 fprintf(stderr, "Master received %d frequencies, remaining %d\n", 
	      FReceived, info->nF - FReceived);

/*       if (FReceived >= info->nF) break; */

      /* defining new frequency limits */
      i = 0;
      while (i < nFreqPart && statusFreq[i][2])
	 i++;
      
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
	 
	 procInfo[iProc][0] = FInfo[0]; procInfo[iProc][1] = FInfo[1];
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

   if (info->verbose)
      fprintf(stderr, "Master ''killing'' remaining slaves\n");

   /* getting elapsed time */
   wallcpu = walltime() - wallcpu;
   fprintf(stderr, "Wall clock time = %f seconds\n", wallcpu);  
   
   /* going to time domain */
   memset( (void *) &trZ, (int) '\0', sizeof(trZ));     
   memset( (void *) &trR, (int) '\0', sizeof(trR));     
   trZ.dt = dt * 1000000;
   trZ.ns = nSamplesOrig;
   trR.dt = dt * 1000000;
   trR.ns = nSamplesOrig;
   
   /* z component */
   for (iR = 0; iR < info->nR; iR++)
   {
      trZ.tracl = iR + 1;
      /* inverse FFT */
      pfacr(1, info->nSamples, uZF[iR], trZ.data); 
      for (i = 0; i < info->nSamples; i++)
      {
	 /* compensating for the complex frequency */
	 trZ.data[i] *= exp(info->tau * i * dt);
      }
      puttr(&trZ);
   }

   /* r component */
   for (iR = 0; iR < info->nR; iR++)
   {
      trR.tracl = iR + 1;
      /* inverse FFT */
      pfacr(1, info->nSamples, uRF[iR], trR.data); 
      for (i = 0; i < info->nSamples; i++)
      {
	 /* compensating for the complex frequency */
	 trR.data[i] *= exp(info->tau * i * dt);
      }
      puttr(&trR);
   }
}   
#include <time.h>

float walltime()
/*****************************************************************************
return elapsed time (wall clock time) in seconds using ANSI C built-ins

Notes:
	return value will be an integral number of seconds since t1 and t2,
	as returned by the time() intrinsic, are the number of seconds
	since the epoch
******************************************************************************
Author:		Jack K. Cohen, Colorado School of Mines, 07/27/90
*****************************************************************************/
{
	static int firsttime = 1;	/* First entry?                 */
	static time_t lasttime;		/* Save return for next entry   */
	time_t t1, t2;			/* Resp. last and current times */

	if (firsttime) {
		firsttime = 0;
		lasttime = time(&t1);
		return 0.0;
	} else {
		t1 = lasttime;
		lasttime = time(&t2);
		return (float) difftime(t2, t1);
	}
}
