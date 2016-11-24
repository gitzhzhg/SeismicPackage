/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "posteriori.h"
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
/*  Output parameters (GLOBAL)                                  */
/*  CD.....................data covariance matrix               */
/*  CM.....................model covariance matrix              */
/*                         outputted in case PRIOR = 1          */ 
/*                         (CMP, CMS, CMrho)                    */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void inputCovar(char* corrDataFile, char* corrModelFile[3])
{
   /* declaration of variables */
   int iS, iS1, i;              /* generic counters */
   float *buffer = NULL;	/* to input covariances */
   FILE *fp;			/* input file */

   /* inputting data covariances */
   fp = fopen(corrDataFile, "r");
   if (fp == NULL)
      err("Can't read data covariance file %s\n", corrDataFile);
   
   /* allocating memory for auxiliary buffer */
   buffer = alloc1float(nDM);
   
   for (i = 0, iS = 0; iS < nDM; iS++)
   {
      fread(buffer, sizeof(float), nDM, fp);
      for (iS1 = iS; iS1 < nDM; iS1++, i++)
      {
	 CD[i] = buffer[iS1];
      }
   }
   
   /* freeing memory */
   free1float(buffer);

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
	       CMP[i] = buffer[iS1];
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
	       CMS[i] = buffer[iS1];
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
