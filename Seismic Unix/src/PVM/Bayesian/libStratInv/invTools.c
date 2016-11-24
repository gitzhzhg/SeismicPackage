/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*  Function inputData()                                        */
/*                                                              */
/*  Seismic data input and transformation to the frequency      */
/*  domain                                                      */
/*                                                              */
/*  Input parameters:                                           */
/*  dataFile...............file name of input data              */
/*  nSamples...............number of samples per trace          */
/*                         global                               */
/*  nR.....................number of offsets                    */
/*                         global                               */
/*                                                              */
/*  Output parameters:                                          */
/*  dataObs................observed data in time domain         */
/*                         global                               */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
#include "stratInv.h"
segy tr;		        /* reading data */
void inputData(char* dataFile)
{
   /* declaration of variables */
   int iS, iR, iF, iF1, iF2;    /* generic counters */
   int ns;			/* # of samples */
   int wL;                      /* window length */
   float *buffer = NULL;	/* to input data */
   float window;                /* windowing purposes */
   complex *bufferC = NULL;	/* to Fourier transform the input data */
   FILE *fp;			/* input file */

   /* memory for bufferC */
   bufferC = alloc1complex(info->nSamples / 2 + 1);
   
   fp = fopen(dataFile,"r");
   if (fp == NULL)
      err("Can't open input data file!\n");

   for (iR = 0; iR < info->nR; iR++)
   {
      fgettr(fp, &tr);
      ns = tr.ns;
      /* DD 
      fprintf(stderr, "ns %d\n", ns);*/

      /* allocating memory */
      if (iR == 0) buffer = alloc1float(MAX(ns, info->nSamples));

      /* reseting */
      for (iS = 0; iS < MAX(ns, info->nSamples); iS++) buffer[iS] = 0;
      memcpy(buffer, tr.data, ns * FSIZE);
      
      /* buffer -> dataObs and compensating for complex frequency */
      for (iS = 0; iS < info->nSamples; iS++)
      {
	 buffer[iS] *= exp(-info->tau * iS * dt);
	 /* DD 
	 fprintf(stderr, "buffer[%d] : %f\n", iS, buffer[iS]);*/
      }

      /* going to the Fourier domain */
      pfarc(-1, info->nSamples, buffer, bufferC);
      
      /* windowing (PERC_WINDOW) spectrum */
      iF1 = NINT(info->f1 / info->dF);
      iF2 = NINT(info->f2 / info->dF);
      wL = info->nF * PERC_WINDOW / 2;
      wL = 2 * wL + 1;
      for (iS = 0, iF = 0; iF < info->nSamples / 2 + 1; iF++)
      {
	 window = 0;
	 if (iF < iF1 || iF >= iF2)
	 {
	    bufferC[iF] = cmplx(0, 0);
	 }
	 else if (iF - iF1 < (wL - 1) / 2)
	 {
	    window =
	       .42 - .5 * cos(2 * PI * (float) iS / ((float) (wL - 1))) +
		  .08 * cos(4 * PI * (float) iS / ((float) (wL - 1)));
	    bufferC[iF].r *= window; bufferC[iF].i *= window;
	    iS++;
	 }
	 else if (iF - iF1 >= info->nF - (wL - 1) / 2)
	 {
	    iS++;
	    window =
	       .42 - .5 * cos(2 * PI * (float) iS / ((float) (wL - 1))) +
		  .08 * cos(4 * PI * (float) iS / ((float) (wL - 1)));
	    bufferC[iF].r *= window; bufferC[iF].i *= window;
	 }
      }

      /* going back to time domain */
      pfacr(1, info->nSamples, bufferC, buffer);

      /* copying to dataObs within target window and scaling */
      for (iF = 0, iS = NINT(t1 / dt); iS <= NINT(t2 / dt); iS++, iF++)
      {
	 dataObs[iR][iF] = (scaleData * buffer[iS]) / (float) info->nSamples;
	 /* DD 
	 fprintf(stderr, "%d %d %f %f %f %f\n", iR, iF, dataObs[iR][iF], 
		 info->f1, info->f2, scaleData);*/
      }
   }
   /* DD 
   fprintf(stderr, "energy %f\n", auxm1 / (nDM * info->nR));
   fwrite(&dataObs[0][0], sizeof(float), nDM * info->nR, stdout);
   exit(-1);*/
   
   /* freeing memory */
   free1float(buffer);
   free1complex(bufferC);

   fclose(fp); 
}
/*  Function inputCovar()                                       */
/*                                                              */
/*  Input of data and model covariance matrixes                 */
/*                                                              */
/*  Input parameters:                                           */
/*  corrDataFile...............file name with data covariance   */
/*  corrModelFile[0 1 2].......file name with model covariance  */
/*                             [0] : (P-wave velocities)        */
/*                             [1] : (S-wave velocities)        */
/*                             [2] : (Density)                  */
/*                             used in case PRIOR = 1           */
/*                                                              */
/*  Output parameters:                                          */
/*  CD.....................data covariance matrix               */
/*  CM.....................model covariance matrix              */
/*                         outputted in case PRIOR = 1          */ 
/*                         (CMvP, CMvS, CMrho)                  */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void inputCovar(char* corrDataFile, char* corrModelFile[3])
{
   /* declaration of variables */
   int iS, iS1, i;              /* generic counters */
   float *buffer = NULL;	/* to input covariances */
   FILE *fp;			/* input file */

   noiseVar = 0;
   /* inputting data covariances */
   if (DATACOV)
   {
      fp = fopen(corrDataFile, "r");
      if (fp == NULL)
	 err("Can't read data covariance file %s\n", corrDataFile);
   }
   
   /* allocating memory for auxiliary buffer */
   if (DATACOV) buffer = alloc1float(nDM);
   
   for (i = 0, iS = 0; iS < nDM; iS++)
   {
      if (DATACOV)
      {
	 fread(buffer, sizeof(float), nDM, fp);
         if (noiseVar < 1. / buffer[iS]) 
            noiseVar = 1. / buffer[iS];
      }
      for (iS1 = iS; iS1 < nDM; iS1++, i++)
      {
	 if (DATACOV)
	    CD[i] = buffer[iS1];
	 else
	    if (iS == iS1) CD[i] = 1;
	    else CD[i] = 0;
      }
   }
   
   /* freeing memory */
   if (DATACOV) free1float(buffer);

   if (PRIOR)
   {
      /* allocating memory for auxiliary buffer */
      buffer = alloc1float(limRange);
      
      if (vpFrechet)
      {
	 fp = fopen(corrModelFile[0], "r");
	 if (fp == NULL)
	    err("Can't read model covariance file %s\n", corrModelFile[0]);
	 
	 for (i = 0, iS = 0; iS < limRange; iS++)
	 {
	    fread(buffer, sizeof(float), limRange, fp);
	    for (iS1 = iS; iS1 < limRange; iS1++, i++)
	    {
	       CMvP[i] = buffer[iS1];
	    }
	 }
	 fclose(fp);
      }

      if (vsFrechet)
      {
	 fp = fopen(corrModelFile[1], "r");
	 if (fp == NULL)
	    err("Can't read model covariance file %s\n", corrModelFile[1]);
	 
	 for (i = 0, iS = 0; iS < limRange; iS++)
	 {
	    fread(buffer, sizeof(float), limRange, fp);
	    for (iS1 = iS; iS1 < limRange; iS1++, i++)
	    {
	       CMvS[i] = buffer[iS1];
	    }
	 }
	 fclose(fp);
      }

      if (rhoFrechet)
      {
	 fp = fopen(corrModelFile[2], "r");
	 if (fp == NULL)
	    err("Can't read model covariance file %s\n", corrModelFile[2]);
	 
	 for (i = 0, iS = 0; iS < limRange; iS++)
	 {
	    fread(buffer, sizeof(float), limRange, fp);
	    for (iS1 = iS; iS1 < limRange; iS1++, i++)
	    {
	       CMrho[i] = buffer[iS1];
	    }
	 }
	 fclose(fp);
      }
      /* freeing memory */
      free1float(buffer);
   }
}
/*  Function lineSearch()                                       */
/*                                                              */
/*  Perform a cubic line search looking for a "reasonable"      */
/*  step length                                                 */
/*                                                              */
/*  Input parameters:                                           */
/*  search.................search direction                     */
/*  fxc....................objective function value at          */
/*                         initial model                        */
/*  slope..................gradient . search                    */
/*                                                              */
/*  Output parameters:                                          */
/*  alpha, beta or rho....updated model                         */
/*                        global                                */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
/*  Adapted from Doug Hart's implementation                     */
float lineSearch(float *search, float fxc, float slope)
{
   int cnt=0, i;                         /* counters */
   int flag;                             /* ==1, minimum relative change */
                                         /* quitting LS */
   static float lambda = INITIAL_LAMBDA; 	
					 /* initial step length */
   float fOld;       
   float lambdaOld;                      /* used to restore the model */
   float lambda2, lambdaprev, lambdaprev2, lambdatemp;
   float fnew, fprev;
   float f2, f1;
   float c, cm11, cm12, cm21, cm22;
   float a, b, disc;                     /* used in interpolation */
 
   /*if (lambda < LAMBDA_MIN) lambda = LAMBDA_MIN;*/

   /* copying model to auxiliary buffers */
   for (i = 0; i <= info->nL; i++)
   {
      if (vpFrechet) alpha0[i] = alpha[i];
      if (vsFrechet) beta0[i] = beta[i];
      if (rhoFrechet) rho0[i] = rho[i];
   }
   
   /* new trial point a full step away */
   flag = update(lambda, search);
   if (flag) return(fxc);    /* marginal change */
   
   /* evaluating new model */
   sprintf(info->id, "Modeling at line search");
   fnew = modeling(); fOld = fnew;
   cnt++; modCount++;
   
   /* Goldstein's test for to prevent small step sizes */
   /* See Fletcher, Practical Methods of Optimization, page 26ff. */
   while (fnew < (fxc + (1 - ALPHA) * lambda * slope)) 
   {
      fOld = fnew;
      lambdaOld = lambda;
      lambda *= 1.5;   /* increase step size by factor of 1.5 */
      flag = update(lambda, search);
      if (flag) return(fnew);              /* marginal change */
      sprintf(info->id, "Modeling at line search");
      fnew = modeling();      /* evaluating new model */
      cnt++; modCount++;
      if (cnt >= MAX_ITER_LS) 
      {
	 if (fOld < fnew)
	 {
	    restore(lambdaOld, search);
	    fnew = fOld;
	 }
	 return(fnew);
      }
   }

   if (fOld < fnew)
   {
      restore(lambdaOld, search);
      lambda = lambdaOld;
      fnew = fOld;
   }

   /* Armijo's test for lambda too large (the backtracking algorithm) */
   /* See Dennis and Schnabel, Numerical Methods for Unconstrained   */
   /* Optimization and Nonlinear Equations, page 126ff.              */
   if(fnew > (fxc + ALPHA * lambda * slope)) 
   {
      fOld = fnew;
      lambdaOld = lambda;
      /* first try a quadratic fit */   
      lambda2 = lambda * lambda;
      f1 = fnew - fxc - slope * lambda;
      lambdatemp = -slope * lambda2 / (2.0 * f1);
      lambdaprev = lambda;
      fprev      = fnew;
      if(lambdatemp < (0.1 * lambda))
         lambda *= 0.1;
      else
         lambda  = lambdatemp;

      /* evaluating new model */
      flag = update(lambda, search);
      if (flag) return(fnew);   /* marginal change */
      sprintf(info->id, "Modeling at line search");
      fnew = modeling();
      cnt++; modCount++;

      if (cnt >= MAX_ITER_LS) 
      {
	 if (fOld < fnew)
	 {
	    restore(lambdaOld, search);
	    fnew = fOld;
	 }
	 return(fnew);
      }
      while(fnew > (fxc + ALPHA * lambda * slope)) 
      { 
	 fOld = fnew;
	 lambdaOld = lambda;
         /* try cubic models if quadratic failed */
         lambda2     = lambda * lambda;
         f1          = fnew - fxc - slope * lambda;
         lambdaprev2 = lambdaprev * lambdaprev;
         f2          = fprev - fxc - lambdaprev * slope;
         c           = 1.0 / (lambda - lambdaprev);
         cm11        = 1.0 / lambda2;
         cm12        = -1.0 / lambdaprev2;
         cm21        = -lambdaprev / lambda2;
         cm22        = lambda/lambdaprev2;
         a           = c * (cm11 * f1 + cm12 * f2);
         b           = c * (cm21 * f1 + cm22 * f2);
         disc        = b * b - 3.0 * a * slope;
         if((fabs(a) > MINFLOAT) && (disc > MINFLOAT)) 
	                                     /* legitimate cubic fit */
            lambdatemp = (-b + sqrt(disc)) / (3.0 * a);
         else                                /* degenerate quadratic fit */
            lambdatemp = - slope * lambda2 / (2.0 * f1);
         if(lambdatemp >= 0.5 * lambda)
            lambdatemp = 0.5 * lambda;
         if(lambdatemp < (0.1 * lambda))
            lambda *= 0.1;
         else
            lambda  = lambdatemp;

	 /* evaluating new model */
	 flag = update(lambda, search);
	 if (flag) return(fnew);    /* marginal change */
	 sprintf(info->id, "Modeling at line search");
	 fnew = modeling();   
	 cnt++; modCount++;
	 if (cnt >= MAX_ITER_LS) 
	 {
	    if (fOld < fnew)
	    {
	       restore(lambdaOld, search);
	       fnew = fOld;
	    }
	    return(fnew);
	 }
      }
   }
   if (fOld < fnew)
   {
      restore(lambdaOld, search);
      fnew = fOld;
   }
   return(fnew);
}
/*  Function dot()                                              */
/*                                                              */
/*  Implements dot products for two vectors                     */
/*                                                              */
/*  Input parameters:                                           */
/*  a......................float vector                         */
/*  b......................float vector                         */
/*  n......................length of the vectors a and b        */
/*                                                              */
/*  Output parameters:                                          */
/*  dotP...................dot product                          */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
float dot(float *a, float *b, int n)
{
   /* declaration of variables */ 
   int i;                        /* counter */
   float dotP;                   /* dot product */

   dotP = 0;
   for (i = 0; i < n; i++)
   {
      dotP += a[i] * b[i];
   }
   
   return(dotP);
}
/*  Function update()                                           */
/*                                                              */
/*  Update the model based on the search direction and          */
/*  step length                                                 */
/*                                                              */
/*  Input parameters:                                           */
/*  search.................search direction                     */
/*  step...................step length                          */
/*                                                              */
/*  Output parameters:                                          */
/*  alpha, beta or density.updated model depending on which one */
/*                         is active                            */
/*                         global                               */
/*  int....................returns 1 if the relative change in  */
/*                         all the parameters is less than      */
/*                         MIN_CHANGE                           */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
int update(float step, float *search)
{
   /* declaration of variables */
   int i, k;                      /* counters */
   int flag;                      /* =1 => change in all the parameter is */
                                  /* smaller than MIN_CHANGE */
   float relChange=0;             /* relative change in parameter */
   float *a, *b, *r;              /* used in computing relative change */
   FILE *fp;                      /* report file */
   
   /* allocating memory */
   if (vpFrechet) a = alloc1float(limRange);
   if (vsFrechet) b = alloc1float(limRange);
   if (rhoFrechet) r = alloc1float(limRange);
   
   for (flag = 0, k = lim[0], i = 0; i < limRange; i++, k++)
   {
      if (vpFrechet)
      {
	 a[i] = alpha[k];
	 alpha[k] = alpha0[k] + step * search[i];
	 
	 /* implementing a hard constraint */
	 if (alpha[k] < (1 - BOUND) * alphaMean[k])
	    alpha[k] = (1 - BOUND) * alphaMean[k];
	 else if (alpha[k] > (1 + BOUND) * alphaMean[k])  
	    alpha[k] = (1 + BOUND) * alphaMean[k];

	 relChange = ABS(alpha[k] - a[i]) / a[i];
	 if (relChange < MIN_CHANGE) flag++;
      }
      
      if (vsFrechet)
      {
	 b[i] = beta[k];
	 beta[k] = beta0[k] + step * search[i + limRange];

	 /* implementing a hard constraint */
	 if (beta[k] < (1 - BOUND) * betaMean[k])
	    beta[k] = (1 - BOUND) * betaMean[k];
	 else if (beta[k] > (1 + BOUND) * betaMean[k])  
	    beta[k] = (1 + BOUND) * betaMean[k];

	 relChange = ABS(beta[k] - b[i]) / b[i];
	 if (relChange < MIN_CHANGE) flag++;
      }
      
      if (rhoFrechet)
      {
	 r[i] = rho[k];
	 rho[k] = rho0[k] + step * search[i + 2 * limRange];

	 /* implementing a hard constraint */
	 if (rho[k] < (1 - BOUND) * rhoMean[k])
	    rho[k] = (1 - BOUND) * rhoMean[k];
	 else if (rho[k] > (1 + BOUND) * rhoMean[k])  
	    rho[k] = (1 + BOUND) * rhoMean[k];

	 relChange = ABS(rho[k] - r[i]) / r[i];
	 if (relChange < MIN_CHANGE) flag++;
      }
   }

   if (flag == numberPar * limRange) 
   {
      flag = 1;
      /* reseting back */
      for (k = lim[0], i = 0; i < limRange; i++, k++)
      {
	 if (vpFrechet) alpha[k] = a[i];
	 if (vsFrechet) beta[k] = b[i];
	 if (rhoFrechet) rho[k] = r[i];
      }
   }
   else
   {
      flag = 0;
   }

   /* reporting */
   fp = fopen("report", "a");
   fprintf(fp,"---------------------------------------------------------\n");
   fprintf(fp,"Objective function evaluations: %d\n", modCount);
   fprintf(fp, "Step length: %f\n", step);
   for (i = 0; i <= info->nL; i++)
      fprintf(fp, "alpha[%d] = %6.2f beta[%d] = %6.2f rho[%d] = %6.2f\n",
	      i, alpha[i], i, beta[i], i, rho[i]);
   if (flag)
   {
      fprintf(fp, 
	      "Change in all parameters smaller than minimum! Quitting LS.\n");
   }
   fprintf(fp,"---------------------------------------------------------\n");
   fclose(fp);
   
   /* freeing memory */
   if (vpFrechet) free1float(a);
   if (vsFrechet) free1float(b);
   if (rhoFrechet) free1float(r);

   return(flag);
}
/*  Function restore()                                          */
/*                                                              */
/*  Restore model in line search if necessary                   */
/*                                                              */
/*  Input parameters:                                           */
/*  search.................search direction                     */
/*  step...................step length                          */
/*                                                              */
/*  Output parameters:                                          */
/*  alpha, beta or density.restored model depending             */
/*                         global                               */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void restore(float step, float *search)
{
   /* declaration of variables */
   int i, k;                      /* counters */
   FILE *fp;                      /* to report */

   for (k = lim[0], i = 0; i < limRange; i++, k++)
   {
      if (vpFrechet)
      {
	 alpha[k] = alpha0[k] + step * search[i];
	 /* implementing a hard constraint */
	 if (alpha[k] < (1 - BOUND) * alphaMean[k])
	    alpha[k] = (1 - BOUND) * alphaMean[k];
	 else if (alpha[k] > (1 + BOUND) * alphaMean[k])  
	    alpha[k] = (1 + BOUND) * alphaMean[k];
      }
      
      if (vsFrechet)
      {
	 beta[k] = beta0[k] + step * search[i + limRange];
	 /* implementing a hard constraint */
	 if (beta[k] < (1 - BOUND) * betaMean[k])
	    beta[k] = (1 - BOUND) * betaMean[k];
	 else if (beta[k] > (1 + BOUND) * betaMean[k])  
	    beta[k] = (1 + BOUND) * betaMean[k];
      }
      
      if (rhoFrechet)
      {
	 rho[k] = rho0[k] + step * search[i + 2 * limRange];
	 /* implementing a hard constraint */
	 if (rho[k] < (1 - BOUND) * rhoMean[k])
	    rho[k] = (1 - BOUND) * rhoMean[k];
	 else if (rho[k] > (1 + BOUND) * rhoMean[k])  
	    rho[k] = (1 + BOUND) * rhoMean[k];
      }
   }

   /* reporting */
   fp = fopen("report", "a");
   fprintf(fp,"---------------------------------------------------------\n");
   fprintf(fp,"Restoring model in line search\n");
   for (i = 0; i <= info->nL; i++)
      fprintf(fp, "alpha[%d] = %6.2f beta[%d] = %6.2f rho[%d] = %6.2f\n",
	      i, alpha[i], i, beta[i], i, rho[i]);
   fclose(fp);
}
/*  Function newSearch()                                        */
/*                                                              */
/*  Computation of the new search direction based on the        */
/*  previous (2) gradients and search direction using the       */
/*  Polak-Ribiere procedure                                     */
/*                                                              */
/*  Input parameters:                                           */
/*  grad0..................gradient at iteration i - 1          */
/*  grad1..................gradient at iteration i              */
/*  search.................direction at iteration i             */
/*                                                              */
/*  Output parameters:                                          */
/*  search.................new search direction                 */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
float newSearch(float *grad0, float *grad1, float *search)
{
   /* declaration of variables */
   FILE *fp;                   /* report results */
   int i;                      /* counter */
   float descent;              /* grad1 dotted into search */
   float den, num;             /* used in computing the step length */
                               /* for search update */
   float beta;                 /* step length for search update */

   /* computing step length for search update */
   if (modCount > 1)
   {
      den = dot(grad0, grad0, numberPar * limRange);

      for (num = 0, i = 0; i < numberPar * limRange; i++)
      {
	 num += grad1[i] * (grad1[i] - grad0[i]);
      }
      
      beta = num / den;
   }
   else
   {
      beta = 0;
   }
   
   for (i = 0; i < numberPar * limRange; i++)
   {
      search[i] = -grad1[i] + beta * search[i];
   }
   
   /* checking descent direction */
   for (descent = 0, i = 0; i < numberPar * limRange; i++)
   {
      descent += grad1[i] * search[i];
   }
   
   if (descent > 0)
   {
      /* reporting reset */
      fp = fopen("report", "a");
      fprintf(fp,"-----------------------\n");
      fprintf(fp, 
	      "At iteration [OF:%d][GR:%d] search was reset to gradient:\n",
	      modCount, gradCount);
      fprintf(fp,"-----------------------\n");
      fclose(fp);
      
      /* reset search to gradient direction */
      for (i = 0; i < numberPar * limRange; i++)
      {
	 search[i] = -grad1[i];
      }
      
      /* computing descent direction */
      for (descent = 0, i = 0; i < numberPar * limRange; i++)
      {
	 descent += grad1[i] * search[i];
      }
   }

   /* reporting */
   fp = fopen("report", "a");
   fprintf(fp,"-----------------------\n");
   fprintf(fp, 
	   "At iteration [OF:%d][GR:%d] the slope is: %f:\n",
	   modCount, gradCount, descent);
   fprintf(fp,"-----------------------\n");
   fclose(fp);
   
   /* returning slope */
   return(descent);
}
/*  Function stop()                                             */
/*                                                              */
/*  Checking the stopping criterions used to verify             */
/*  convergence                                                 */
/*                                                              */
/*  Input parameters:                                           */
/*  grad0..................gradient at iteration i - 1          */
/*  grad1..................gradient at iteration i              */
/*  of0....................objective function at iteration i - 1*/
/*  of1....................objective function at iteration i    */
/*  maxIter................maximum iterations at conjugate      */    
/*                         gradient                             */
/*                                                              */
/*  Output parameters:                                          */
/*  notDone................=1 stop optimization                 */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
int stop(float *grad0, float *grad1, float oF0, float oF1, int maxIter)
{
   /* declaration of variables */
   FILE *fp;                       /* to report */
   float ofChange, gradChange;     /* relative changes on objective */
                                   /* function and gradient */
   int i;                          /* counter */
   int flag;                       /* work-not-done flag */
   
   for (auxm1 = 0, auxm2 = 0, i = 0; i < limRange; i++)
   {
      auxm1 += grad0[i] * grad0[i];
      auxm2 += grad1[i] * grad1[i];
   }
   auxm1 = sqrt(auxm1); auxm2 = sqrt(auxm2);
   
   gradChange = ABS((auxm2 - auxm1) / auxm2);
   ofChange = ABS((oF1 - oF0) / oF1);
 
   if (gradChange < RELATIVE_CHANGE || ofChange < RELATIVE_CHANGE ||
       ABS(auxm2) < MIN_GRAD || gradCount > maxIter)
   {
      flag = 0;  /* convergence */
   }
   else
   {
      flag = 1;
   }
   
   /* reporting */
   fp = fopen("report", "a");
   fprintf(fp,"---------------------------------------------------------\n");
   fprintf(fp, "Number of conjugate gradient iterations: %d\n", gradCount);
   fprintf(fp, "Relative change on gradient module: %f\n", gradChange);
   fprintf(fp, "Relative change on objective function module: %f\n", 
	        ofChange);
   fprintf(fp, "Status: %d - (0) work done (1) work not done\n", flag);
   fprintf(fp,"---------------------------------------------------------\n");
   fclose(fp);
   return(flag);
}
/*  Function normalize()                                        */
/*                                                              */
/*  Normalize a vector in place                                 */
/*                                                              */
/*  Input parameters:                                           */
/*  vector.................input vector                         */
/*  int n..................length of input vector               */
/*                                                              */
/*  Output parameters:                                          */
/*  vector.................normalized vector                    */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void normalize(float *vector, int n)
{
   /* declaration of variables */
   int i;                         /* counter */
   float mod;                     /* vector module */

   for (mod = 0, i = 0; i < n; i++)
      mod += vector[i] * vector[i];
   mod = sqrt(mod);
   
   for (i = 0; i < n; i++) 
      vector[i] /= mod;
}
