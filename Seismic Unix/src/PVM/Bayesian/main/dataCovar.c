/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "dataCovar.h"
#define EULER      2.7182818
segy tr;		        /* segy trace */
/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                           ",
"  dataCovar.c                                                              ",
"                                                                           ",
"  This program computes a sample covariance matrix from an ensemble 	    ", 
"  of realizations. In the context of Bayesiam seismic waveform inversion,  ",
"  this matrix is used as a data covariance matrix in the likelihood        ",
"  function.                                                                ",
"                                                                           ",
"  Input parameters:                                                        ",
"                                                                           ",
"  realizations=noise   SU file containing the ensemble of realizations.    ",
"  nreal=10             number of realizations available for estimating.    ",
"                       the data covariance.                                ",
"  t1=0                 First time considered for the data covariance       ",
"                       computation.                                        ",
"  t2=1                 Last time considered for the data covariance        ",
"                       computation.                                        ",
"  scale=1              Scale factor for the realizations.                  ",
"  exponential=0        If set to 1 an exponential covariance will be fit   ",
"                       to the sample covariance.                           ",
"  window=0             If set to 1 the resulting sample covariance will be ",
"                       tapered by a Hanning window.                        ",
"  perc=5               Percentage of the lags of the covariance matrix that",
"                       will be tapered.                                    ",
"  inverse=0            If set to 1 inverse of covariance also calculated   ",
"  verbose=0            Dialogue flag                                       ",
"                                                                           ",
"  Output parameters:                                                       ",
"                                                                           ",
"  cov=cov              Binary file for the sample covariance matrix.       ",
"  invcov=covinv        Binary file for the inverse of the sample covariance",
"                       matrix.                                             ",
"                                                                           ",
"                                        Author: Wences Gouveia - 96-06-12  ",
"                                        Center For Wave Phenomena          ",
"                                        Colorado School of Mines           ",
NULL};
/************************ end self doc ***********************************/  
/* extern variables */
int nDM;                        /* number of data samples */
int nR;                         /* number of offsets */
int verbose;                    /* dialogue flag */
float scale;                    /* scaling input data */
float **noise;                  /* noise data */
float t1, t2;                   /* window delimiters */

/* functions */
void inputNoise (char *noiseData);

void main (int argc, char **argv)
{
   /* declaration of variables */
   char *noiseFile = " ";                     /* noise data */ 
   char *covInvFile = " ";                    /* inverse covar file */
   char *covFile = " ";                       /* covar file */
   int exponential;                           /* exponential flag */
   int i, j, iS, iS1, iTrace;                 /* counters */
   int traceBeg, traceEnd;                    /* traces used of noise data */
   int nTraces;                               /* number of traces used */
   int cl;                                    /* correlation length */
   int wL;                                    /* window length */
   int WINDOW;                                /* windowing flag */
   int inverse;                               /* inversion flag */
   float stdDeviation;                        /* maximum std dev */
   float *buffer;                             /* auxiliary data */
   float *window, perc;	          	      /* windowing the data */
   FILE *fp;                                  /* file pointer */
   float **covar;                             /* covariance matrix */
   float **covarExp;                          /* covariance matrix (exp) */
   float **covarInv;                          /* inverse of covar. matrix */
   
   /* getting input parameters */
   initargs(argc, argv);
   requestdoc(0);
   
   /* seismic data and model parameters */
   if (!getparstring("realizations", &noiseFile)) noiseFile = "noise";
   if (!getparstring("invcov", &covInvFile)) covInvFile = "invcov";
   if (!getparstring("cov", &covFile)) covFile = "cov";
   if (!getparint("nreal", &nR)) nR = 10;
   traceBeg = 0;
   traceEnd = nR;
   if (!getparfloat("t1", &t1)) t1 = 0;
   if (!getparfloat("t2", &t2)) t2 = 1;
   if (!getparfloat("scale", &scale)) scale = 1;
   if (!getparint("exponential", &exponential)) exponential = 0;
   if (!getparint("window", &WINDOW)) WINDOW = 0;
   if (!getparint("inverse", &inverse)) inverse = 0;
   if (!getparint("verbose", &verbose)) verbose = 0;
   if (!getparfloat("perc", &perc)) perc = 5; perc /= 100;

   /* input data */
   inputNoise(noiseFile);

   /* memory allocation */
   covar = alloc2float(nDM, nDM);
   covarExp = alloc2float(nDM, nDM);
   covarInv = alloc2float(nDM, nDM);
   buffer = alloc1float(nDM);
   nTraces = traceEnd - traceBeg + 1;

   /* building window */
   if (WINDOW)
   {
      window = alloc1float(nDM);
      wL = perc * nDM;   wL = 2 * wL - 1;
   
      /* building hanning window */
      for (iS1 = (wL - 1) / 2 + 1, iS = 0; iS < nDM; iS++)
      {  
         window[iS] = 1;
         if (iS > nDM - (wL - 1) / 2)
         {
            window[iS] =
               .42 - .5 * cos(2 * PI * (float) iS1 / ((float) (wL - 1))) +
               .08 * cos(4 * PI * (float) iS1 / ((float) (wL - 1)));
            iS1++;
         }
      } 
   }
   
   for (iTrace = traceBeg; iTrace < traceEnd; iTrace++)
   {
      for (iS = 0; iS < nDM; iS++)
      {
	 for (iS1 = 0; iS1 < nDM; iS1++)
	 {
	       covar[iS][iS1] += noise[iTrace][iS] * noise[iTrace][iS1]
	                             / (float) nTraces;
	 }
      }
   }

   /* windowing correlations */
   if (WINDOW)
   {
      for (iS = 0; iS < nDM; iS++)
      {
         for (iS1 = 0; iS1 < nDM; iS1++)
         {
	    covar[iS][iS1] *= window[ABS(iS - iS1)];
         }
      }
   }     

   /* exponential fit? */
   if (exponential)
   {
      /* reseting */
      for (iS = 0; iS < nDM; iS++) 
      {
         for (iS1 = 0; iS1 < nDM; iS1++)
         {
            covarExp[iS][iS1] = 0;
	 }
      }

      for (iS = 0; iS < nDM; iS++)
      {
         for (cl = 0, iS1 = iS; iS1 < nDM; iS1++, cl++)
         {
            if (covar[iS][iS1] / covar[iS][iS] < 1. / EULER)  break;
         }

         for (iS1 = 0; iS1 < nDM; iS1++)
         {
            covarExp[iS][iS1] += (.5 * covar[iS][iS]) *
               exp(-(float) ABS(iS - iS1) / (float) cl);
         }
         for (iS1 = 0; iS1 < nDM; iS1++)
         {
            covarExp[iS1][iS] += (.5 * covar[iS][iS]) *
               exp(-(float) ABS(iS - iS1) / (float) cl);  
         }
      }    
      /* back to covar */
      for (iS = 0; iS < nDM; iS++)
         for (iS1 = 0; iS1 < nDM; iS1++)    
	    covar[iS][iS1] = covarExp[iS][iS1];
   }

   /* finding max std dev */
   stdDeviation = 0;
   for (iS = 0; iS < nDM; iS++)
      if ( covar[iS][iS] > stdDeviation) stdDeviation = covar[iS][iS];

   if (verbose)
      fprintf(stderr, "Maximum standard deviation : %f\n", 
	      sqrt(stdDeviation));

   /* outputting */
   fp = fopen(covFile, "w");
   fwrite(covar[0], sizeof(float), nDM * nDM, fp);  
   fclose(fp);
   
   /* inverting */
   if (inverse)
   {
     for (i = 0; i < nDM; i++) for (j = 0; j < nDM; j++) 
       covarInv[i][j] = covar[i][j];
     inverse_matrix(nDM, covarInv);
     
     if (verbose)
       fprintf(stderr, "Sample covariance matrix inverted.\n");

     /* outputting */
     fp = fopen(covInvFile, "w");
     fwrite(covarInv[0], sizeof(float), nDM * nDM, fp);
     fclose(fp);
   }
   
   if (verbose)
      fprintf(stderr, "Dimension of the data covariance matrix: %d x %d\n", 
	      nDM, nDM);
}

/************************************************************************/
/*   function inputNoise                                                */
/*   inputs realization data                                            */
/*                                                                      */
/*   input parameter:                                                   */
/*   noiseFile                    SU file storing the realizations.     */
/*                                                                      */
/*   output parameter:                                                  */
/*   noise                        Matrix with realizations.             */
/************************************************************************/
void inputNoise(char* noiseFile)
{
   /* declaration of variables */
   int iS1, iS, iR;             /* generic counters */
   int ns;			/* # of samples */
   float *buffer = NULL;	/* to input data */
   float dt;                    /* time sampling */
   FILE *fp;			/* input file */

   fp = fopen(noiseFile,"r");
   if (fp == NULL)
      err("Can't open input noise file!\n");

   for (iR = 0; iR < nR; iR++)
   {
      fgettr(fp, &tr);
      if (iR == 0)
      {
	 ns = tr.ns;
	 dt = (float) tr.dt / 1000000;

	 /* allocating memory */
	 buffer = alloc1float(ns);
	 nDM = NINT(t2 / dt) - NINT(t1 / dt) + 1;
	 
	 if (verbose)
	    fprintf(stderr, "Number of samples per realization: %d\n", nDM);

	 noise = alloc2float(nDM, nR);
      }
      memcpy(buffer, tr.data, ns * FSIZE);

      /* buffer -> noise with scaling */
      for (iS1 = 0, iS = NINT(t1 / dt); iS <= NINT(t2 / dt); iS++, iS1++)
      {
	 noise[iR][iS1] = buffer[iS] * scale;
      }
   }

   /* freeing memory */
   free1float(buffer);
   fclose(fp); 
}
