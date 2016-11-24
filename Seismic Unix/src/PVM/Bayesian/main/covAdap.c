/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "covAdap.h"
#define EULER      2.7182818  
/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                     ",
"  Program covAdap.c                                                  ",
"                                                                     ",
"  Computes a covariance matrix from the fluctuations of the          ",
"  subsurface parameters about a smooth medium or generates a         ",
"  Tikhonov second-order difference matrix to be used in a            ",
"  inversion procedure.                                               ",
"                                                                     ",
"  Input parameters:                                                  ",
"                                                                     ",
"  parameter=V  If set to V the covariance matrix is computed for     ",
"               fluctuations of P and S wave velocities and density.  ",
"               If set to I this matrix is computed for fluctuations  ",
"               of P and S wave impedances and density.               ",
"                                                                     ",
"  mode=C       If set to [C] a covariance matrix is computed is      ",
"               from the median fluctuations. Otherwise a Tikhonov    ",
"               regularization matrix is outputted. The next          ",
"               parameters (model and trend) are used only if model=C.",
"                                                                     ",
"  model=model  Elastic model file. This is the model that contains   ",
"               the high frequency information about the subsurface   ",
"               parameters. The following format is used:             ",     
"                                                                     ",
"               Thick(km)  Rho(g/cm^3)  Vp(km/s)  Qp  Vs(km/s)  Qs    ",
"                                                                     ",
"               Where:                                                ",
"               Thick: Layer thickness                                ",
"               Rho:   Layer density                                  ",
"               Vp:    P-wave velocity                                ",
"               Qp:    P-wave quality factor                          ",
"               Vs:    S-wave velocity                                ",
"               Qs:    S-wave quality factor                          ",
"                                                                     ",
" trend=trend   Longwavelength component of the subsurface parameters.",
"               The covariance is computed from the fluctuations of   ",
"               the model [model] about the model [trend]. The format ",
"               is the same as the one used in the parameter model.   ",
"                                                                     ",
" lambda=1      Regularization parameter. Used only if mode not set to C.",
" dz=0.01       Depth discretization level in the target zone (km)    ",
" targetbeg=.5  Begin of the target depth interval (km).              ",
" targetend=1   End of the target depth interval (km).                ",
" exponential=0 If set to 1, an exponential covariance matrix is fit   ",
"               to the covariance matrix computed from the fluctuations.",
" inverse=0     If set to 1 the inverse of the covariance matrix is   ",
"               outputted instead of the covariance matrix itself.    ",
" window=0.5    Window length (km) within which the autorrelation is  ",
"               computed.                                             ",
" verbose=0     Dialogue flag                                         ",
"                                                                     ",
"                                                                     ",
"  Output parameters:                                                 ",
"                                                                     ",
" Three covariance matrices (or their respective inverses) outputted  ",
" in the following binary files:                                      ",
"                                                                     ",
" alpha:        For P-wave velocity or impedance.                     ",
" beta:         For S-wave velocity or impedance.                     ",
" rho:          For density.                                          ",  
"                                                                     ",  
"                                    Author: Wences Gouveia - 96-09-21",
"                                    Center For Wave Phenomena        ",
"                                    Colorado School of Mines         ",  
NULL};   
/************************ end self doc ***********************************/ 
void main (int argc, char **argv) 
{
   /* declaration of variables */
   FILE *fp;                        /* input file */
   char *modelFile = " ";           /* elastic model file */  
   char *initialFile = " ";         /* initial guess file */
   char *parameter = " ";           /* velocity or impedance parameters */
   char *mode = " ";                /* regularization or covariances */
   int wL;                          /* length of window used in computing */
                                    /* the autocorrelation */   
   int i1, i2, i, j, k, m;          /* counters */
   int cl;                          /* correlation length */
   int nL;                          /* number of layers */
   int INVERSE;                     /* if = 1 outputs inverse of covariance */
   int IMPEDANCE;                   /* IMPEDANCE flag */
   int EXPONENTIAL;                 /* EXPONENTIAL flag */
   int REGULARIZATION;              /* regularization flag */
   int n;                           /* actual dimension of the problem */
   int lim[2];                      /* discrete target interval */
   int verbose;                     /* dialogue flag */
   float windowLength;              /* length of window used in computing */
                                    /* the autocorrelation */   
   float lambda;                    /* regularization parameter */
   float dZ;                        /* depth spacing in target zone */
   float limZ[2];                   /* target interval (Km) */
   float depth;                     /* current depth */
   float *buffer;                   /* used to output data */
   float **model, **model0;         /* well-logs and smoothed models */
   float *correlation;              /* correlation sequences */
   float *thick, qP, qS;            /* thicknesses and qualify factors */
   float aux, *auxVec;              /* auxiliar variables */
   float **corr, **corrInv;         /* covariance matrix and its inverse */
   float **reg, **regTreg;          /* regularization matrices */

   /* input parameters */
   initargs(argc, argv);
   requestdoc(0);

   if (!getparstring("mode", &mode)) mode = "C";
   if (mode[0] == 'C' || mode[0] == 'c') REGULARIZATION = 0;
   else REGULARIZATION = 1;

   if (!getparstring("parameter", &parameter)) parameter = "V";
   if (parameter[0] == 'V' || parameter[0] == 'v')
   {
      IMPEDANCE = 0;
   }
   else
   {
      IMPEDANCE = 1;
   }
   if (!getparstring("model", &modelFile)) modelFile = "model";
   if (!getparstring("trend", &initialFile)) initialFile = "trend";
   if (!getparfloat("dz", &dZ)) dZ = .01;
   if (!getparfloat("targetbeg", &limZ[0])) limZ[0] = 1.00; 
   if (!getparfloat("targetend", &limZ[1])) limZ[1] = 1.08;
   if (!getparint("exponential", &EXPONENTIAL)) EXPONENTIAL = 0;
   if (!getparint("inverse", &INVERSE)) INVERSE = 0;
   if (!getparfloat("window", &windowLength)) windowLength = 10;
   if (!getparfloat("lambda", &lambda)) lambda = 1;
   if (!getparint("verbose", &verbose)) verbose = 0;

   /* reading data file */
   nL = 0;
   depth = 0;
   fp = fopen(modelFile, "r");
   if (fp == NULL) err("No model file!\n");
   while (fscanf(fp, "%f %f %f %f %f %f\n",
		 &aux, &aux, &aux, &aux, &aux, &aux) != EOF)
      nL++;
   nL--;
   
   /* allocating necessary memory */
   model = alloc2float(nL, 3);
   model0 = alloc2float(nL, 3);
   thick = alloc1float(nL + 1);
   
   /* actually reading the file */
   rewind(fp);
   if (verbose)
   {
      fprintf(stderr,"  Model file:\n");
      fprintf(stderr,"  Thickness     rho     vP     qP    vS     qS\n");
   }
   
   for (k = 0, i = 0; i < nL + 1; i++, k++)
   {
      fscanf(fp, "%f %f %f %f %f %f\n", &thick[k], &model[0][k], &model[1][k],
	     &qP, &model[2][k], &qS);

      if (verbose)
	 fprintf(stderr,"   %7.4f      %4.3f   %3.2f  %5.1f  %3.2f  %5.1f\n",
		 thick[k], model[0][k], model[1][k], qP, model[2][k], qS);

      if (IMPEDANCE)
      {
	 model[1][k] *= model[0][k];
	 model[2][k] *= model[0][k];
      }
   }
   fclose(fp);

   /* reading initial guess */
   fp = fopen(initialFile,"r");
   if (verbose)
   {
      fprintf(stderr,"  Trend file:\n");
      fprintf(stderr,"  Thickness     rho     vP     qP    vS     qS\n");
   }

   for (k = 0, i = 0; i < nL + 1; i++, k++)
   {
      fscanf(fp, "%f %f %f %f %f %f\n", &aux, &model0[0][k], &model0[1][k],
	     &qP, &model0[2][k], &qS);

      if (verbose)
	 fprintf(stderr,"   %7.4f      %4.3f   %3.2f  %5.1f  %3.2f  %5.1f\n",
		 aux, model0[0][k], model0[1][k], qP, model0[2][k], qS);

      if (IMPEDANCE)
      {
	 model0[1][k] *= model0[0][k];
	 model0[2][k] *= model0[0][k];
      }
   }
   fclose(fp);

   /* setting the target zone */
   for (depth = thick[0], i = 1; i <= nL; depth += thick[i], i++)
   {
      if (NINT(depth / dZ) <= NINT(limZ[0] / dZ)) lim[0] = i;
      if (NINT(depth / dZ) < NINT(limZ[1] / dZ)) lim[1] = i;
   }
   
   /* dimension of the problem */
   n = lim[1] - lim[0] + 1;
   wL = NINT(windowLength / dZ);
   if (wL%2 == 0) wL++; if (wL > n) wL -= 2;
   
   /* memory for covariance matrices and for correlation sequences */
   corr = alloc2float(n, n);
   corrInv = alloc2float(n, n);
   correlation = alloc1float(wL);
   buffer = alloc1float(n);
   auxVec = alloc1float(wL);
   if (REGULARIZATION) 
   {
      reg = alloc2float(n, n);
      regTreg = alloc2float(n, n);
   }
   
   if (verbose)
      fprintf(stderr, "\n\nDimension of the problem [%d %d] : %d\n", 
	      lim[0], lim[1], n);

   /* computing autocorrelations of the difference of the models */
   for (m = 0; m < 3; m++)
   {
      /* reset */
      for (i = 0; i < n; i++)
         for (j = 0; j < n; j++)
            corr[i][j] = 0;

      for (k = 0, i = lim[0]; i <= lim[1]; i++, k++)
      {
	 /* defining window limits */
	 i1 = MAX(lim[0], i - (wL - 1) / 2);
	 i2 = MIN(lim[1], i + (wL - 1) / 2);

	 if (i2 - i1 + 1 < wL)
	 {
	    if (i1 == lim[0]) i2 = i1 + wL - 1;
	    if (i2 == lim[1]) i1 = i2 - (wL - 1);
	 }

	 for (j = i1; j <= i2; j++)
	 {
	    auxVec[j - i1] =  model[m][j] - model0[m][j];
	 }

	 xcor(wL, 0, auxVec, wL, 0, auxVec, wL, 0, correlation);

	 if (EXPONENTIAL)
	 {
	    for (cl = 0, j = 0; j < wL; j++, cl++)
	    {
	       if (correlation[j] / correlation[0] < 1. / EULER)  break;
	    }
	    aux = correlation[0];

	    for (j = 0; j < wL; j++)
	    {
	       correlation[j] = aux * exp(-(float) j / (float) cl);
	    }    	      
	 }

	 for (j = 0; j < n; j++)
	 {
	    if (ABS(k - j) >= wL)
	       corr[k][j] = 0;
	    else
	       corr[k][j] += correlation[ABS(k - j)] / ((float) 2 * wL);
	 }

	 /* honouring symmetry */
	 for (j = 0; j < n; j++)
	 {
	    if (ABS(k - j) >= wL)
	       corr[j][k] = 0;
	    else
	       corr[j][k] += correlation[ABS(k - j)] / ((float) 2 * wL);
	 }
      }

      /* inverting */
      if (INVERSE)
      {
	 for (i = 0; i < n; i++) for (j = 0; j < n; j++) 
	    corrInv[i][j] = corr[i][j];
	 inverse_matrix(n, corrInv);
      }

      /* regularization */
      if (REGULARIZATION)
      {
         for (i = 0; i < n; i++)
         {
            if (i == 0)
            {
               reg[0][0] = -2 * lambda;
               reg[0][1] = lambda;
            }
            else if (i == n - 1)
            {
               reg[n - 1][n - 1] = -2 * lambda;
               reg[n - 1][n - 2] = lambda;
            }
            else
            {
               reg[i][i - 1] = lambda;
               reg[i][i] = -2 * lambda;
               reg[i][i + 1] = lambda;
            }
         }

	 /* forming lambda R^T R */
	 for (i = 0; i < n; i++)
	 {
	    for (j = 0; j < n; j++)
	    {
	       regTreg[i][j] = 0;
	       for (k = 0; k < n; k++)
	       {
		  regTreg[i][j] += reg[i][k] * reg[k][j];
	       }
	    }
	 }

         /* outputting */
         if (m == 0) fp = fopen("rho", "w");
         if (m == 1) fp = fopen("alpha", "w");
         if (m == 2) fp = fopen("beta", "w");
         for (i = 0; i < n; i++)
         {
	    fwrite(regTreg[i], sizeof(float), n, fp);
/*	    fwrite(reg[i], sizeof(float), n, fp);*/
         }
         fclose(fp);
      }
      else
      {
         /* outputting */
	 if (m == 0) fp = fopen("rho", "w");
	 if (m == 1) fp = fopen("alpha", "w");
	 if (m == 2) fp = fopen("beta", "w");
	 for (i = 0; i < n; i++)
         {
	    if (INVERSE) fwrite(corrInv[i], sizeof(float), n, fp);
	    else fwrite(corr[i], sizeof(float), n, fp);
         }
	 fclose(fp);
      }  
   }   
}
