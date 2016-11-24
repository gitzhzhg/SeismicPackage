/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "stratInvMain.h"
/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                        ",
" stratInv - Bayesian waveform inversion code for stratified (v(z))      ",
"            medium. This codes operates by maximizing a Gaussian        ",
"            a posteriori probability distribution along the lines       ",
"            described by the references below. A nonlinear version of   ",
"            conjugate gradient method is employed in the calculation.   ",
"            This code is implemented in a distributed fashion using the ",
"            PVM library, according to the master-slave topology. The    ",
"            input parameters for this code are as follows.              ",
"                                                                        ",
" model=[model]  Elastic model file. This is the initial model used in   ",
"                optimization. This is an ASCII file that should contain ",
"                one row per layer of the model in the following format: ",
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
" data=[data.su] Input seismograms for the inversion, in SU format.      ",
" scale=[1]      Scale factor for input data.                            ",
" datacov=[0]    If set to 1 data covariance matrix required             ",
" corrData=[covD]Inverse of data covariance matrix.                      ",
" impedance=[0]  If set to 1, inversion is carried out in the impedance -",
"                density domain. Otherwise it is carried out in the      ",
"                velocity - density domain.                              ",
" p=[1]          P-wave (velocity or impedance) active.                  ",
" s=[1]          S-wave (velocity or impedance) active.                  ",
" r=[1]          Density active.                                         ",
" prior=[0]      If set to 1 prior information required.                 ",
" corrP=[covP]   Inverse of the P-wave model covariance matrix.          ",
" corrS=[covS]   Inverse of the S-wave model covariance matrix.          ",
" corrR=[covR]   Inverse of the density model covariance matrix.         ",
" targetbeg=[.5] Begin of the target depth interval (km).                ",
" targetend=[1]  End of the target depth interval (km).                  ",
" dz=0.5         Depth discretization level in the target zone (km)      ",
" t1=[0]         Begining of the time misfit window (s).                 ",
" t2=[1]         End of the time misfit window (s).                      ",
" mutefile=[NULL]Muting information to mute SYNTHETIC data. This is as   ",
"                ASCII file with the following format for each row:      ",
"                time(s)     trace index                                 ",
"                ...                                                     ",
"                The program will linearly interpolate between times.    ",
" recfile=[NULL] File with receiver offsets. The format is one offset    ",
"                line. If none is specified (default), the acquisition   ",
"                is end on                                               ",
" directwave=1   Direct wave flag. If zero direct wave is not generated  ",
" r1=[0]         Minimum source-receiver offset (km)                     ",
" nr=[48]        Number of receivers                                     ",
" dr=[0.025]     Receiver spacing (km)                                   ",
" zs=[0.001]     Source depth (km)                                       ",
" u1=[0]         Initial horizontal slowness (s/km)                      ",
" u2=[1]         Final horizontal slowness (s/km)                        ",
" nu=[100]       Number of slownesses                                    ",
" f1=[2]         Initial frequency for modeling (Hz)                     ",
" f2=[50]        Final frequency for modeling (Hz)                       ",
" dt=[0.004]     Time sampling interval (0.004)                          ",
" tmod=[8]       Maximum modeling time (s)                               ",
" F1=[0]         EAST-WEST point force coordinate                        ",
" F2=[0]         NORTH-SOUTH point force coordinate                      ",
" F3=[1]         VERTICAL point force coordinate                         ",
" hanning=[1]    Final trace is convolved with a hanning window          ",
" wu=[5]         Percentual of slowness interval that is windowed        ",
" ww=[5]         Percentual of frequency interval that is windowed       ",
" tau=[50]       Wrap-around attenuation parameter                       ",
" nProc=[1]      Number of processors used in the computation            ",
" nfreqproc=[0]  Number of frequencies allocated to each processor       ",
"                The default value means that the frequency              ",
"                components will be evenly split among the slave         ",
"                processors                                              ",
" verbose=[0]    =1 print useful information                             ",
"                                                                        ",
" stratInv outputs an ASCII file containing information regarding the    ",
" optimization work. This report file also contains the subsurface model ",
" at each iteration of the optimization.                                 ",
"                                                                        ", 
" Reminder: YOU SHOULD INSTALL PVM (version 3.3.9 or higher) BEFORE THE  ",
"           INSTALLATION OF THIS CODE.                                   ",
"                                                                        ",
" References:                                                            ",
"               Tarantola's Inverse Problem Theory Book, Elsevier        ",
"               Gouveia & Scales: Resolution of Seismic Waveform         ",
"               Inversion: Bayes versus Occam.                           ",
"               Inverse Problems, 13, 1997.                              ",
"               Gouveia & Scales: Bayesian Seismic Waveform Data         ",
"               Inversion: Parameter Estimation and Uncertainty Analysis ",
"               To appear in JGR Solid Earth, 1998                       ",
"                                                                        ",
"                                 Author: Wences Gouveia - 96-08-01      ",
"                                 Center For Wave Phenomena              ",
"                                 Colorado School of Mines               ",
NULL};   
/************************ end self doc ***********************************/ 
void main (int argc, char **argv)
{
   /* declaration of variables */
   FILE *fp;                     /* file pointer */
   char *dataFile = " ";         /* data file */
   char *auxChar = " ";          /* auxiliar character */ 
   char *muteFile = " ";         /* muting file */
   char *orientation = " ";      /* orientation of recordings */
   char *modelFile = " ";        /* elastic model file */
   char *corrDataFile = " ";     /* data covariance file */
   char *corrModelFile[3];       /* model covariance file */
   int maxIter;                  /* maximum iterations at conjugate gradient*/ 
   int notDone;                  /* convergence flag */
   int i, j, k;                  /* counters */
   int nMutePoints;              /* number of traces used in misfit interp */
   float a, b;                   /* used in interpolation */
   float dZ;                     /* layer thickness within target zone */
   float depth;                  /* current depth used in defining limits */
                                 /* for Frechet derivatives */
   float fR;                     /* reference frequency */
   float limZ[2];                /* target interval (Km) */
   float tMod;                   /* maximum modeling time */
   float slope;                  /* gradient . search direction */
   float *grad0, *grad1;         /* gradient vectors */
   float *search;                /* search direction */   
   float *auxP;                  /* auxiliar variable */
   float *t1Aux, *offAux;        /* used in muting interpolation */
   float oF0, oF1;               /* objective function value */
   
   /* allocing for orientation */
   orientation = malloc(1);
   
   /* complex Zero */
   zeroC = cmplx(0, 0);

   /* getting input parameters */
   initargs(argc, argv);
   requestdoc(0);

   /* seismic data and model parameters */
   if (!getparstring("model", &modelFile)) modelFile = "model";
   if (!getparstring("data", &dataFile)) dataFile = "data.su";
   if (!getparfloat("scale", &scaleData)) scaleData = 1;
   if (!getparstring("corrData", &corrDataFile)) corrDataFile = "covD";
   if (!getparint("prior", &PRIOR)) PRIOR = 0;
   if (!getparint("datacov", &DATACOV)) DATACOV = 0;
   if (!getparint("impedance", &IMPEDANCE)) IMPEDANCE = 0;
   if (!getparint("p", &vpFrechet)) vpFrechet = 1;
   if (!getparint("s", &vsFrechet)) vsFrechet = 1;
   if (!getparint("r", &rhoFrechet)) rhoFrechet = 1;
   if (IMPEDANCE && rhoFrechet)
     if (!vpFrechet || !vsFrechet) err("If impedance and density are specified you need also to make the P and S related parameters active.\n");
   
   if (!getparint("verbose", &info->verbose)) info->verbose = 0;
   numberPar = vpFrechet + vsFrechet + rhoFrechet;
      
   if (PRIOR)
   {
      if (vpFrechet)
      {
	 if (!getparstring("corrP", &corrModelFile[0])) 
	    corrModelFile[0] = "covP";
      }
      if (vsFrechet) 
      {
	 if (!getparstring("corrS", &corrModelFile[1])) 
	    corrModelFile[1] = "covS";
      }
      if (rhoFrechet) 
      {
	 if (!getparstring("corrR", &corrModelFile[2])) 
	    corrModelFile[2] = "covRHO";
      }
   }
   
   if (!getparstring("orientation", &orientation)) orientation[0] = 'Z';
   if (orientation[0] == 'z' || orientation[0] == 'Z')
   {
      info->vertical = 1; info->radial = 0;
   }
   else
   {
      info->vertical = 0; info->radial = 1;
   }
   
   if (!getparfloat("dz", &dZ)) dZ = .5;
   if (!getparfloat("targetbeg", &limZ[0])) limZ[0] = 0.5; 
   if (!getparfloat("targetend", &limZ[1])) limZ[1] = 1.0;

   /* geometry */
   if (!getparfloat("r1", &info->r1)) info->r1 = 0.;
   if (!getparint("nr", &info->nR)) info->nR = 48;
   if (!getparfloat("dr", &info->dR)) info->dR = .025;
   if (!getparfloat("zs", &info->zs)) info->zs = .001;
   if (!getparfloat("F1", &info->F1)) info->F1 = 0;
   if (!getparfloat("F2", &info->F2)) info->F2 = 0;
   if (!getparfloat("F3", &info->F3)) info->F3 = 1;

   /* modeling */
   if (!getparstring("receiverfile", &auxChar)) auxChar = " ";
   sprintf(info->recFile, "%s", auxChar);    
   if (!getparfloat("u1", &info->u1)) info->u1 = 0.0;
   if (!getparfloat("u2", &info->u2)) info->u2 = 1.0;
   if (!getparint("directwave", &info->directWave)) info->directWave = 1;
   if (!getparint("nu", &info->nU)) info->nU = 100;
   if (!getparfloat("f1", &info->f1)) info->f1 = 2;
   if (!getparfloat("f2", &info->f2)) info->f2 = 50;
   if (!getparfloat("dt", &dt)) dt = 0.004;
   if (!getparfloat("tmod", &tMod)) tMod = 8;
   if (!getparfloat("tau", &info->tau)) info->tau = 50;

   /* misfit information */
   if (!getparfloat("t1", &t1)) t1 = 0;
   if (!getparfloat("t2", &t2)) t2 = 1; if (t2 > tMod) t2 = tMod;
   if (!getparstring("mutefile", &muteFile)) MUTE = 1; else MUTE = 0;
  
   if (MUTE) 
   {
      fp = fopen(muteFile, "r");
      if (fp == NULL) err("Can't open mute file\n");
      nMutePoints = 0;
      while (fscanf(fp, "%f %f\n", &aux, &aux) != EOF) nMutePoints++;
      t1Aux = alloc1float(nMutePoints);
      offAux = alloc1float(nMutePoints);
   
      rewind(fp);
      i = 0;
      while (fscanf(fp, "%f %f\n,", &t1Aux[i], &offAux[i]) != EOF) i++;
      fclose(fp);
   }

   if (!getparint("hanning", &info->hanningFlag)) info->hanningFlag = 1;
   if (!getparfloat("wu", &info->percU)) info->percU = 10; info->percU /= 100;
   if (!getparfloat("ww", &info->percW)) info->percW = 25; info->percW /= 100;

   /* distributed computation */
   if (!getparint("nproc", &nProc)) nProc = 1;
   if (!getparint("nfreqproc", &info->nFreqProc) || nProc == 1) 
      info->nFreqProc = 0;

   /* checking number of receivers */
   fp = fopen(info->recFile, "r");
   if (fp != NULL)
   {
      info->nR = 0;
      while (fscanf(fp, "%f\n", &auxm1) != EOF) info->nR++;
   }
   fclose(fp);

   /* building muting area */   
   t1Mute = alloc1float(info->nR);
   recArray = alloc1float(info->nR);
   
   /* reading receiver configuration */
   fp = fopen(info->recFile, "r");
   if (fp == NULL)
   {
      /* standard end-on */
      if (info->verbose) fprintf(stderr, "No receiver file available\n");
      for (i = 0; i < info->nR; i++)
      {
	 recArray[i] = info->r1 + i * info->dR;
      }
   }
   else
   {
      if (info->verbose) fprintf(stderr, "Reading receiver file\n");
      for (i = 0; i < info->nR; i++)
      {
	 fscanf(fp, "%f\n", &recArray[i]);
      }
   }
   fclose(fp);

   if (MUTE)
   {
      /* correcting end points */
      /*offAux[0] = 1;
      offAux[nMutePoints - 1] = info->nR;
 
      t1Mute[0] = t1Aux[0];
      t1Mute[info->nR - 1] = t1Aux[nMutePoints - 1];*/

      for (k = 1, i = 0; i < nMutePoints - 1; k = j, i++)
      {
	 /* linear interpolating */
	 a = (t1Aux[i + 1] - t1Aux[i]) / (offAux[i + 1] - offAux[i]);
	 b = t1Aux[i] - a * offAux[i];
	 j = k;
	 while (j <= offAux[i + 1])
	 {
	    t1Mute[j - 1] = a * j + b;
	    j++;
	 }
      }
      /* freeing */
      free1float(t1Aux);
      free1float(offAux);
   }

   /* number of misfit samples */ 
   nDM = NINT((t2 - t1) / dt) + 1;
   nTotalSamples = nDM * info->nR; 

   if (info->verbose)
      fprintf(stderr, "Number of data points in misfit computation: %d\n",
	      nTotalSamples);
   
   /* # of iterations */
   if (!getparint("maxiter", &maxIter)) maxIter = 4;
   
   /* some hard-coded parameters */
   fR = 1; info->wR = 2 * PI * fR;   /* reference frequency */
   
   /* how many layers */
   fp = fopen(modelFile,"r");
   if (fp == NULL)
      err("No model file!\n");
   
   info->nL = 0;
   depth = 0;
   while (fscanf(fp, "%f %f %f %f %f %f\n", 
		 &aux, &aux, &aux, &aux, &aux, &aux) != EOF)
      info->nL++;
   info->nL--;
   fclose(fp);
   
   /* considering the unknown layers */
   limRange = NINT((limZ[1] - limZ[0]) / dZ);

   if (info->verbose)
   {
      fprintf(stderr,"Number of layers: %d\n", info->nL + 1);
      fprintf(stderr,"Number of layers in target zone: %d\n", limRange);
   }

   /* basic time-frequency stuff */
   info->nSamples = NINT(tMod / dt) + 1;
   info->nSamples = npfar(info->nSamples);

   tMod = dt * (info->nSamples - 1);
   info->dF = 1. / (tMod);

   /* adjusting f1 and f2 */
   aux = info->dF;
   while (aux < info->f1) aux += info->dF;
   info->f1 = aux;
   while (aux < info->f2) aux += info->dF;
   info->f2 = aux;
   
   info->nF = NINT(info->f2 / info->dF) - NINT(info->f1 / info->dF) + 1; 
   if (info->nF%2 == 0) 
   {
      info->f2 += info->dF;
      info->nF++;
   }
   initF = NINT(info->f1 / info->dF);
   lastF = NINT(info->f2 / info->dF);
   if (info->nFreqProc == 0)
   {
      info->nFreqProc = NINT((float) info->nF / (float) nProc + .5);
      if (info->nFreqProc > info->nF) info->nFreqProc = info->nF;
   }
   else
      while (info->nFreqProc > info->nF) info->nFreqProc /= 2;
   nFreqPart = NINT((float) info->nF / (float) info->nFreqProc + .5);

   /* memory allocation */
   alpha = alloc1float(info->nL + 1);
   beta = alloc1float(info->nL + 1);
   rho = alloc1float(info->nL + 1);
   if (vpFrechet) alphaMean = alloc1float(info->nL + 1);
   if (vsFrechet) betaMean = alloc1float(info->nL + 1);
   if (rhoFrechet) rhoMean = alloc1float(info->nL + 1);
   if (vpFrechet) alpha0 = alloc1float(info->nL + 1);
   if (vsFrechet) beta0 = alloc1float(info->nL + 1);
   if (rhoFrechet) rho0 = alloc1float(info->nL + 1);
   qP = alloc1float(info->nL + 1);
   qS = alloc1float(info->nL + 1);
   thick = alloc1float(info->nL + 1);

   dataObs = alloc2float(nDM, info->nR);
   resCD = alloc2complex(info->nF, info->nR);
   CD = alloc1float(nDM * (nDM + 1) / 2);

   if (PRIOR)
   {
      if(vpFrechet)
         CMvP = alloc1float(limRange * (limRange + 1) / 2);
      if(vsFrechet)
         CMvS = alloc1float(limRange * (limRange + 1) / 2);
      if(rhoFrechet)
         CMrho = alloc1float(limRange * (limRange + 1) / 2);
   }

   grad0 = alloc1float(numberPar * limRange);
   grad1 = alloc1float(numberPar * limRange);
   search = alloc1float(numberPar * limRange);

   /* distributed stuff */
   processes = alloc1int(nProc);
   procInfo = alloc2int(2, nProc);
   statusFreq = alloc2int(3, nFreqPart);

   /* defining frequency partitions */
   for (k = initF, i = 0; i < nFreqPart; i++, k += info->nFreqProc)
   {
      statusFreq[i][0] = k;
      statusFreq[i][1] = MIN(k + info->nFreqProc - 1, lastF);
      statusFreq[i][2] = 0;
   }
          
   /* reading the model file */
   fp = fopen(modelFile,"r");
   if (info->verbose)
      fprintf(stderr,"  Thickness     rho     vP     qP    vS     qS\n");
   
   for (k = 0; k < info->nL + 1; k++)
   {
      fscanf(fp, "%f %f %f %f %f %f\n", &thick[k], &rho[k], &alpha[k], 
	     &qP[k], &beta[k], &qS[k]);
      if (info->verbose)
	 fprintf(stderr,"   %7.4f      %4.3f   %3.2f  %5.1f  %3.2f  %5.1f\n",
		 thick[k], rho[k], alpha[k], qP[k], beta[k], qS[k]);

      if (IMPEDANCE)
      {
         alpha[k] *= rho[k];
	 beta[k] *= rho[k];
      }

      if (vpFrechet) alphaMean[k] = alpha[k];
      if (vsFrechet) betaMean[k] = beta[k];
      if (rhoFrechet) rhoMean[k] = rho[k];
   }
   fclose(fp);

   /* setting lim[0] and lim[1] */
   for (depth = thick[0], i = 1; i <= info->nL; depth += thick[i], i++)
   {
      if (NINT(depth / dZ) <= NINT(limZ[0] / dZ)) lim[0] = i;
      if (NINT(depth / dZ) < NINT(limZ[1] / dZ)) lim[1] = i;
   }
   lim[1]++;

   /* some modeling parameters */
   /* slowness increment */
   info->dU = (info->u2 - info->u1) / (float) info->nU;

   /* imaginary part of frequency for damping wrap-around */
   info->tau = log(info->tau) / tMod;
   if (info->tau > TAUMAX) info->tau = TAUMAX;

   /* reading data and model covariance matrixes */
   inputCovar(corrDataFile, corrModelFile);
   
   /* starting inverse procedure */
   /* opening report file */
   fp = fopen("report", "w");
   if (IMPEDANCE)
   {
      if (vpFrechet) 
         fprintf(fp, "Inversion with respect to p-wave impedance\n");
      if (vsFrechet) 
         fprintf(fp, "Inversion with respect to s-wave impedance\n");
      if (rhoFrechet) 
	fprintf(fp, "Inversion with respect to density\n");
   }
   else
   {
      if (vpFrechet) 
         fprintf(fp, "Inversion with respect to p-wave velocity\n");
      if (vsFrechet) 
         fprintf(fp, "Inversion with respect to s-wave velocity\n");
      if (rhoFrechet) 
	fprintf(fp, "Inversion with respect to density\n");
   }
   if (!DATACOV)
   {
     fprintf(fp, 
	     "\nSince no data covariance is available, the final target\n");
     fprintf(fp, "residual is defined as .1 times the initial residual\n\n"); 
   }
   fclose(fp);

   /* some initializations */
   modCount = 0;
   gradCount = 0;
   notDone = 1;
   dataIsFit = 0;
   oFNorm = 1;

   /* reading "observed" data */
   inputData(dataFile);

   /* more terms in INFO structure */
   info->lim[0] = lim[0];
   info->lim[1] = lim[1];
   info->limRange = limRange;
   info->numberPar = numberPar;
   info->vpFrechet = vpFrechet;
   info->vsFrechet = vsFrechet;
   info->rhoFrechet = rhoFrechet;
   
   /* modeling data at input model and computing its gradient */
   sprintf(info->id, "Initial guess");
   oF0 = modeling(); 
   sprintf(info->id, "Gradient at initial guess");
   gradient(grad0); 
   modCount++; gradCount++;

   /* computing the search direction */
   for (i = 0; i < numberPar * limRange; i++)
   {
      grad1[i] = grad0[i];
   }
   slope = newSearch(grad0, grad1, search);
      
   /* reporting */
   fp = fopen("report", "a");
   fprintf(fp,"-----------------------\n");
   fprintf(fp,"gradient at iteration [OF:%d][GR:%d]:\n", 
	   modCount, gradCount);
   for (i = 0; i < numberPar * limRange; i++)
      fprintf(fp, "grad[%d]: %f\n", i, grad0[i]);
   fprintf(fp,"search at iteration [OF:%d][GR:%d]:\n", 
	   modCount, gradCount);
   for (i = 0; i < numberPar * limRange; i++)
      fprintf(fp, "search[%d]: %f\n", i, search[i]);
   fprintf(fp,"-----------------------\n");
   fclose(fp);
  
   while (notDone)
   {
      /* computing step length via a line search */
      /* just the real part of the search direction is used */
      oF1 = lineSearch(search, oF0, slope);  
	 
      /* computing the gradient at the new model */
      sprintf(info->id, "Gradient at update model");
      gradient(grad1);
      gradCount++;

      /* and the new search direction */
      slope = newSearch(grad0, grad1, search);
      
      /* reporting */
      fp = fopen("report", "a");
      fprintf(fp,"-----------------------\n");
      fprintf(fp,"gradient at iteration [OF:%d][GR:%d]:\n", 
	      modCount, gradCount);
      for (i = 0; i < numberPar * limRange; i++)
	 fprintf(fp, "grad[%d]: %f\n", i, grad1[i]);
      fprintf(fp,"search at iteration [OF:%d][GR:%d]:\n", 
	      modCount, gradCount);
      for (i = 0; i < numberPar * limRange; i++)
	 fprintf(fp, "search[%d]: %f\n", i, search[i]);
      fprintf(fp,"-----------------------\n");
      fclose(fp);

      /* checking stopping criterion (just the gradient and */
      /* objective function for now ) */
      notDone = stop(grad0, grad1, oF0, oF1, maxIter);

      /* switching gradients */
      auxP = grad0;
      grad0 = grad1;
      grad1 = auxP;

      aux = oF0;
      oF0 = oF1;  
      oF1 = aux;   
   }
}
