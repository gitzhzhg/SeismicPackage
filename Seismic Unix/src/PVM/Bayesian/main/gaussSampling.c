/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "sample.h"
#define EULER      2.7182818
/*********************** self documentation ***************************/
char *sdoc[] = {
"                                                                     ",
"  Program gaussSampling.c                                            ",
"                                                                     ",
"  Samples a multidimensional Gaussian distribution via the LU        ",
"  decomposition technique. This Gaussian distribution is specified   ",
"  in subsurface-model space. The elements of this space consist of   ",
"  P-wave, S-wave impedance and density profiles.                     ",
"  For an overview of the sampling procedure check the book           ",
"  Geophysical Inverse Theory by Robert Parker.                       ",
"                                                                     ",
"  Input parameters:                                                  ",
"                                                                     ",
"  covariance=covar Binary file with the covariance matrix.           ",
"                                                                     ",
"  mean=mean        Mean of the distribution. This mean is a specified",
"                   as a subsurface elastic model, and it is stored in",
"                   an ASCII file that should contain one row per     ",
"                   layer of the model in the following format:       ",
"                                                                     ",
"                   Thick(km)  Rho(g/cm^3)  Vp(km/s)  Qp  Vs(km/s)  Qs",
"                                                                     ",
"                   Where:                                            ",
"                   Thick: Layer thickness                            ",
"                   Rho:   Layer density                              ",
"                   Vp:    P-wave velocity                            ",
"                   Qp:    P-wave quality factor                      ",
"                   Vs:    S-wave velocity                            ",
"                   Qs:    S-wave quality factor                      ",
"                                                                     ",
"  exponential=0    If set to 1, a exponential covariance is fit to   ",
"                   the input covariance matrix.                      ",
"  impedance=0      If set to 1, realizations will be done in         ",
"                   impedance and density domain. Otherwise, they will",
"                   be done in velocity and density domain.           ",
"  p=1              Sample P-wave parameter.                          ",
"  s=1              Sample S-wave parameter.                          ",
"  r=1              Sample density.                                   ",
"  targetbeg=.5     Begin of the target depth interval (km).          ",
"  targetend=1      End of the target depth interval (km).            ",
"  dz=0.01          Depth discretization level (km).                  ",
"  verbose=0        Dialogue flag                                     ",
"                                                                     ",
"  Output parameters:                                                 ",
"                                                                     ",
"  The realizations are vectors which dimension is the number of      ",
"  layers of the mean model. However, only the layers within the      ",
"  target interval change from one realization to the other. The      ",
"  vectors are outputted to stdout in binary format.                  ",
"                                                                     ",  
"                                    Author: Wences Gouveia - 96-09-13",
"                                    Center For Wave Phenomena        ",
"                                    Colorado School of Mines         ",
NULL};
/************************ end self doc ***********************************/ 
void main (int argc, char **argv)   
{
   /* declaration of variables */
   FILE *fp;                  /* file pointer */
   char *covarFile = " ";     /* covariance file */
   char *MAPFile = " ";       /* MAP model file */
   int i, j, k;               /* counters */
   int nL;                    /* number of layers */
   int cl;                    /* correlation length */ 
   int pWave;                 /* P-wave flag */
   int sWave;                 /* S-wave flag */
   int density;               /* density flag */
   int nVar;                  /* dimension of the problem */
   int seed;                  /* input seed */ 
   int lim[2];                /* integer limits for target zone */
   int exponential;           /* exponential flag */
   int impedance;             /* impedance flag */
   int verbose;               /* dialogue flag */
   int nPar;                  /* number of active parameters */
   int shift, shift1;         /* used in simulation of more than one */
                              /* parameter */
   long seed1, seed2;         /* seed for random generator */
   float limZ[2];             /* depth limits of target zone */
   float *thick, *alpha, *beta, *rho;
                              /* medium parameters */
   float *buffer;             /* working buffer */
   float aux1, aux2;          /* auxiliar variables */
   float *parm;     	      /* paramter vector */
   float *mean;     	      /* mean vector */
   float *work;               /* working area */
   float **covar;             /* correlation matrix */
   float **covarExp;          /* exponential correlation matrix */
   float *deviate;            /* random gaussian realization */
   float dz;                  /* depth discretization level */
   float depth;               /* current depth */
   
   /* input parameters */
   initargs(argc, argv);
   requestdoc(0);
   
   /* dimension of the problem */
   if (!getparstring("covariance", &covarFile)) covarFile = "covar";
   if (!getparstring("mean", &MAPFile)) MAPFile = "mean";
   if (!getparint("exponential", &exponential)) exponential = 0;
   if (!getparint("impedance", &impedance)) impedance = 0;
   if (!getparint("p", &pWave)) pWave = 1;
   if (!getparint("s", &sWave)) sWave = 1;
   if (!getparint("r", &density)) density = 1;
   if (!getparfloat("dz", &dz)) dz = .5;
   nPar = pWave + sWave + density;
   if (!getparfloat("targetbeg", &limZ[0])) limZ[0] = 0.5; 
   if (!getparfloat("targetend", &limZ[1])) limZ[1] = 1.0;
   if (!getparint("verbose", &verbose)) verbose = 0;

   
   /* random generator seeding */
   seed = getpid();

   fp = fopen(MAPFile, "r");
   if (fp == NULL) err("No model file!\n");
   nL = 0;
   while (fscanf(fp, "%f %f %f %f %f %f\n", 
		 &aux1, &aux1, &aux1, &aux1, &aux1, &aux1) != EOF)
      nL++;
   nL--;
   rewind(fp);

   /* memory allocation */
   alpha = alloc1float(nL + 1);
   beta = alloc1float(nL + 1);
   rho = alloc1float(nL + 1);
   thick = alloc1float(nL + 1);

   if (verbose)
      fprintf(stderr,"  Thickness     rho     vP     qP    vS     qS\n");
   
   for (k = 0; k < nL + 1; k++)
   {
      fscanf(fp, "%f %f %f %f %f %f\n", &thick[k], &rho[k], &alpha[k], 
	     &aux1, &beta[k], &aux2);
      if (verbose)
	 fprintf(stderr,"   %7.4f      %4.3f   %3.2f  %5.1f  %3.2f  %5.1f\n",
		 thick[k], rho[k], alpha[k], aux1, beta[k], aux2);

      if (impedance)
      {
         alpha[k] *= rho[k];
	 beta[k] *= rho[k];
      }
   }
   fclose(fp);

   /* setting lim[0] and lim[1] */
   for (depth = thick[0], i = 1; i <= nL; depth += thick[i], i++)
   {
      if (NINT(depth / dz) <= NINT(limZ[0] / dz)) lim[0] = i;
      if (NINT(depth / dz) < NINT(limZ[1] / dz)) lim[1] = i;
   }

   /* total dimension */
   nVar = nPar * (lim[1] - lim[0] + 1);
   if (verbose)
      fprintf(stderr, "Total dimension of the problem: %d\n", nVar);
   
   /* more memory allocation */
   covar = alloc2float(nVar, nVar);
   covarExp = alloc2float(nVar, nVar);
   parm = alloc1float(nVar * (nVar + 3) / 2 + 1);
   work = alloc1float(nVar);
   mean = alloc1float(nVar);
   deviate = alloc1float(nVar);
   buffer = alloc1float(nPar * (nL + 1));
   
   fp = fopen(covarFile, "r");
   if (fp == NULL) err("No covariance file!\n");
   fread(&covar[0][0], sizeof(float), nVar * nVar, fp);
   fclose(fp);
   
   /* building the mean */
   shift = 0;
   if (pWave)
   {
      for (k = 0, i = lim[0]; i <= lim[1]; i++, k++)
	 mean[k] = alpha[i];
      shift = nVar / nPar;
   }
   if (sWave)
   {
      for (k = 0, i = lim[0]; i <= lim[1]; i++, k++)
	 mean[k + shift] = beta[i];
      shift += nVar / nPar;
   }
   if (density)
   {
      for (k = 0, i = lim[0]; i <= lim[1]; i++, k++)
	 mean[k + shift] = beta[i];
   }

   /* fitting an exponential model */
   if (exponential)
   {
      for (i = 0; i < nVar; i++) 
	 for (j = 0; j < nVar; j++) 
	    covarExp[i][j] = 0;

      for (i = 0; i < nVar; i++)
      {
	 for (cl = 0, j = i; j < nVar; j++, cl++)
	 {
	    if (covar[i][j] / covar[i][i] < 1. / EULER)  break;
	 }
	 
	 for (j = 0; j < nVar; j++) 
	 {
	    covarExp[i][j] += .5 * covar[i][i] * 
	       exp(-(float) ABS(i - j) / (float) cl);
	 }
	 for (j = 0; j < nVar; j++) 
	 {
	    covarExp[j][i] += .5 * covar[i][i] * 
	       exp(-(float) ABS(i - j) / (float) cl);
	 }
      }
      
      for (i = 0; i < nVar; i++)
	 for (j = 0; j < nVar; j++)
	    covar[i][j] = covarExp[i][j];
   }
     
   /* reseting */
   for (i = 0; i < nVar * (nVar + 3) / 2 + 1; i++)
   {
      parm[i] = 0;
      if (i < nVar)
      {
         work[i] = 0;
         deviate[i] = 0;
      }
   }
  
   /* input data for generating realization of the multivariate */
   /* gaussian */
   setgmn(mean, covar[0], nVar, parm); 
   seed1 = (long) seed; seed2 = (long) seed * seed;
   setall(seed1, seed2);  
   
   /* generating the realization */
   genmn(parm, deviate, work);

   /* copying to buffer */
   shift = 0;
   shift1 = 0;

   if (pWave)
   {
      for (j = 0; j < lim[0]; j++)
	 buffer[j] = alpha[j];
      for (k = 0, j = lim[0]; j <= lim[1]; j++, k++)
	 buffer[j] = deviate[k];
      for (j = lim[1]; j < nL + 1; j++)
	 buffer[j] = alpha[j];
      shift = nL;
      shift1 = nVar / nPar;
   }
   
   if (sWave)
   {
      for (j = 0; j < lim[0]; j++)
	 buffer[j + shift] = beta[j];
      for (k = 0, j = lim[0]; j <= lim[1]; j++, k++)
	 buffer[j + shift] = deviate[k + shift1];
      for (j = lim[1]; j < nL + 1; j++)
	 buffer[j + shift] = beta[j];
      shift += nL;
      shift1 += nVar / nPar;
   }
   if (density)
   {
      for (j = 0; j < lim[0]; j++)
	 buffer[j + shift] = rho[j];
      for (k = 0, j = lim[0]; j <= lim[1]; j++, k++)
	 buffer[j + shift] = deviate[k + shift1];
      for (j = lim[1]; j < nL + 1; j++)
	 buffer[j + shift] = rho[j];
   }
   /* outputting */
   fwrite(buffer, sizeof(float), nPar * (nL + 1), stdout);
}


