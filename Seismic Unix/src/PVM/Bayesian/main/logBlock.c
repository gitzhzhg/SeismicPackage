/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "cwp.h"
#include "par.h"
/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                           ",
"  logBlock.c:                                                              ",
"                                                                           ",
"  This program blocks a well log using a two step procedure. In the 	    ", 
"  first it applies a recursive median filtering, i.e., a running median    ",
"  filtering that is successively applied until the data is no longer       ",
"  changed. The length of this filter is specified by the user. In the      ",
"  second step the program blocks the result so that the contrast between   ",
"  layers is larger than the a minimum specified by the user.               ",
"                                                                           ",
"  The output of this program is the blocked well log. The format in the    ",
"  output file is the input format required by the elastic distributed      ",
"  reflectivity and inversion codes. This file is outputted to stdout.      ",
"                                                                           ",
"  The implementation of this code was based in the following reference:    ",
"  Compound median filtering applied to sonic logs                          ",
"  Leaney, W. and Ulrych, T.                                                ",
"  SEG Abstracts 1987 - Session: BHG1.7.                                    ",
"                                                                           ",
"  Input parameter (Values between brackets represent the default value     ",
"                   of the parameter).                                      ",
"                                                                           ",
"  logLAS=[logFile]                      LAS format log input file          ",
"                                        (see notes)                        ",
"  lmedian=[10]                          length of median filter operator   ",
"  mincontrast=[5]                       minimum contrast between layers    ",
"  ncols=[5]                             ncols of LAS file (see notes)      ",
"  columns=[1,2,5,3]                     column of the LAS files where the  ",
"                                        depth, P-wave velocity, S-wave     ",
"                                        velocity and density are found     ",
"  QP=[1000]                             quality factor for P-wave to output",
"                                        in the final model                 ",
"  QS=[1000]                             quality factor for S-wave to output",
"                                        in the final model                 ",
"  nullvalue=[-999.25]                   indicates no measurement           ",
"  verbose=[0]                           dialogue flag                      ",
"                                                                           ",
"                                                                           ",
"                                                                           ",
" Notes: The way the LAS file is handled here is probably not general. In   ",
"        order to read this file I just skip its header (36 lines) and      ",
"        go straight to the numerical values of the well logs. The user     ",
"        of this code has to provide some information that can be found     ",
"        in the header. These are:                                          ",
"        . The number of column of this file                                ",
"        . In which columns are stored the depth, P-wave velocity, S-wave   ",
"          velocity and density (in this order)                             ",
"        . The null value which indicates no measurement at a specific depth",
"                                                                           ",
" Author: Wences Gouveia - 96-08-29                                         ",
" Center For Wave Phenomena                                                 ",
" Colorado School of Mines                                                  ",
NULL};
/************************ end self doc ***********************************/  

void median (float **data, int i1, int i2, int which, int length, int verbose);

void main(int argc, char **argv)   
{
   /* declaration of variables */
   FILE *fp;                    /* file pointer */
   char *logLAS = " ";          /* log LAS files */
   char text[80];               /* auxiliar variable */
   int *columns;                /* column where depth, density, vp and vs */
                                /* are stored in the LAS file */
   int nCol;                    /* number of column in LAS file */
   float QP, QS;                /* quality factors */
   float avgVp, avgVs, avgRHO;  /* averages in blocking */
   float **data;                /* log data */
   float *vpData;               /* unblocked p-wave velocity */
   float *vsData;               /* unblocked s-wave velocity */  
   float *rhoData;              /* unblocked density */
   float *depth;                /* unblocked depth */
   float *vpBlock;              /* blocked p-wave velocity */
   float *vsBlock;              /* blocked s-wave velocity */  
   float *rhoBlock;             /* blocked density */
   float *thickBlock;           /* block thickness */
   float **LAS;                 /* store LAS file to select columns */
   float minContrast;           /* minimum contrast to define a layer */
   float aux;                   /*auxiliary variable */
   float NULLVALUE;             /* null value in LAS file */
   int i, j, iFirst, iBlock;    /* counters */
   int iP1, iP2, iS1, iS2, iR1, iR2;
                                /* delimiters */
   int length;                  /* length of median filtering */
   int inBlock;                 /* inside-the-block flag */
   int nBlock;                  /* number of blocks */
   int nInBlock;                /* number of samples within block */
   int nL;                      /* number of well log layers */
   int verbose;                 /* dialogue flag */
   
   columns = alloc1int(4);

   /* getting parameters */
   
   initargs(argc, argv);    
   requestdoc(0); 
   if (!getparstring("logLAS", &logLAS)) logLAS = "logFile";
   if (!getparint("lmedian", &length)) length = 10;
   if (length%2 == 0) length++;
   if (!getparfloat("mincontrast", &minContrast)) minContrast = 5;
   minContrast /= 100;
   if (!getparint("ncols", &nCol)) nCol = 5;
   if (!getparint("columns", columns)) 
   {
      columns[0] = 0;      columns[1] = 1;
      columns[2] = 4;      columns[3] = 2;
   }
   else for (i = 0; i < nCol; i++) columns[i]--;
   
   if (!getparfloat("nullvalue", &NULLVALUE)) NULLVALUE = -999.2500;
   if (!getparfloat("QP", &QP)) QP = 1000;
   if (!getparfloat("QS", &QS)) QS = 1000;
   if (!getparint("verbose", &verbose)) verbose = 0;

   /* reading data */
   fp = fopen(logLAS, "r");
   if (fp == NULL) err("No LAS file available\n");
   nL = 0;
      
   /* skipping header for LAS format */
   for (i = 0; i < 36; i++) fgets(text, 80, fp);

   while (fgets(text, 80, fp) != NULL) nL++;
   rewind(fp);

   for (i = 0; i < 36; i++) fgets(text, 80, fp);

   /* allocating memory */
   LAS = alloc2float(nCol, nL + 1);
   data = alloc2float(4, nL + 1);
   i = 0;
   for (i = 0; i < nL; i++)
   {
      for (j = 0; j < nCol - 1; j++) 
      {
	 fscanf(fp, "%f", &LAS[i][j]);
      }
      fscanf(fp, "%f\n", &LAS[i][j]);
   }
   fclose(fp);
 
   /* transfering to data */
   for (i = 0; i < nL; i++)
   {
      data[i][0] = LAS[i][columns[0]]; /* depth */
      data[i][1] = LAS[i][columns[1]]; /* vp */
      data[i][2] = LAS[i][columns[3]]; /* rho */
      data[i][3] = LAS[i][columns[2]]; /* vs */
   }

   /* freeing */
   free2float(LAS);

   for (i = 0; i < nL; i++)
   {
      /* conversion of units: feet -> kilometers, micro second / foot -> */
      /* kilometer / second */
      
      if (data[i][0] != NULLVALUE)	
	 data[i][0] *= 0.3048 / 1000;
      if (data[i][1] != NULLVALUE)
	 data[i][1] = (1. / data[i][1]) * 0.3048 * 1000;
      if (data[i][3] != NULLVALUE)
	 data[i][3] = (1. / data[i][3]) * 0.3048 * 1000;
   }

   if (verbose) fprintf(stderr, "LAS file %s read (%d layers)\n", logLAS, nL);

   /* applying median filtering on active samples */
   iP1 = 0;
   iP2 = 0;
   i = 0;
   while (data[i][1] == NULLVALUE) i++;
   iP1 = i;
   while (i != nL - 1 && data[i][1] != NULLVALUE) i++;
   iP2 = i;
   
   median(data, iP1, iP2, 1, length, verbose);  /* median on p-wave */

   iR1 = 0;
   iR2 = 0;
   i = 0;
   while (data[i][2] == NULLVALUE) i++;
   iR1 = i;
   while (i != nL - 1 && data[i][2] != NULLVALUE) i++;
   iR2 = i;
   median(data, iR1, iR2, 2, length, verbose);  /* median on density */

   iS1 = 0;
   iS2 = 0;
   i = 0;
   while (data[i][3] == NULLVALUE) i++;
   iS1 = i;
   while (i != nL - 1 && data[i][3] != NULLVALUE) i++;
   iS2 = i;
   median(data, iS1, iS2, 3, length, verbose);  /* median on s-wave */
   

   /* beggining blocking procedure */
   /* allocating memory */
   vpData = alloc1float(nL);
   vsData = alloc1float(nL);
   rhoData = alloc1float(nL);
   depth = alloc1float(nL);
      
   /* filling gaps */
   for (i = 0; i < iP1; i++)  vpData[i] = data[iP1][1];
   for (i = iP1; i <= iP2; i++) vpData[i] = data[i][1];
   for (i = iP2 + 1; i < nL; i++)  vpData[i] = data[iP2][1];

   for (i = 0; i < iS1; i++)  vsData[i] = data[iS1][3];
   for (i = iS1; i <= iS2; i++) vsData[i] = data[i][3];
   for (i = iS2 + 1; i < nL; i++)  vsData[i] = data[iS2][3];

   for (i = 0; i < iR1; i++)  rhoData[i] = data[iR1][2];
   for (i = iR1; i <= iR2; i++) rhoData[i] = data[i][2];
   for (i = iR2 + 1; i < nL; i++)  rhoData[i] = data[iR2][2];

   for (i = 0; i < nL; i++) depth[i] = data[i][0];

   /* block with respect to P wave velocity */
   iFirst = 0;
	    
   if (verbose) fprintf(stderr, "Counting blocks...\n");

   /* counting blocks of constant P-wave velocity */
   nBlock = 0;
   i = iFirst;
   j = i;
   while (i < nL - 1 && j < nL)
   {
      inBlock = 1;
      j = i + 1;
      while (inBlock && j < nL)
      {
	 if (ABS((vpData[i] - vpData[j]) / vpData[i]) >
	     minContrast || j == nL - 1) 
	 {
	    nBlock++;
	    inBlock = 0;
	    i = j;
	 }
	 else
	 {
	    j++;
	 }
      }
   }
	    
   if (verbose) fprintf(stderr, "Number of blocks: %d\n", nBlock);

   /* allocating memory */
   vpBlock = alloc1float(nBlock);
   vsBlock = alloc1float(nBlock);
   rhoBlock = alloc1float(nBlock);
   thickBlock = alloc1float(nBlock);

   /* counting blocks of constant P-wave velocity */
   iBlock = 0;
   i = iFirst;
   j = i;
   while (i < nL - 1 && j < nL)
   {
      inBlock = 1;
      j = i + 1;
      nInBlock = 1;
      avgVp = 1. / vpData[i];
      avgVs = 1. / vsData[i];
      avgRHO = rhoData[i];
      
      if (verbose)
	 fprintf(stderr, "In the block [%d][%d] : %f %f %f\n",
		 iBlock, nInBlock, 
		 vpData[i], vsData[i], rhoData[i]);
      
      while (inBlock && j < nL)
      {
	 if (ABS((vpData[i] - vpData[j]) / vpData[i]) >
	     minContrast || j == nL - 1) 
	 {
	    vpBlock[iBlock] = (float) nInBlock / avgVp;

	    aux = (float) nInBlock / avgVs;
	    if (iBlock - 1 < 0 || 
		ABS((vsBlock[iBlock - 1] - aux) / vsBlock[iBlock - 1])
		> minContrast) vsBlock[iBlock] = aux;
	    else vsBlock[iBlock] = vsBlock[iBlock - 1];

	    aux = avgRHO / (float) nInBlock;
	    if (iBlock - 1 < 0 || 
		ABS((rhoBlock[iBlock - 1] - aux) / rhoBlock[iBlock - 1]) 
		> minContrast) 
		rhoBlock[iBlock] = aux;
	    else
		rhoBlock[iBlock] = rhoBlock[iBlock - 1];

	    thickBlock[iBlock] = depth[j] - depth[i];  

	    if (verbose)
	       fprintf(stderr, "Block [%d][%d] : %f %f %f %f\n",
		       iBlock, nInBlock, 
		       vpBlock[iBlock], vsBlock[iBlock], rhoBlock[iBlock], 
		       thickBlock[iBlock]);
	    
	    iBlock++;
	    inBlock = 0;
	    i = j;
	 }
	 else
	 {
	    avgVp += 1. / vpData[j];
	    avgVs += 1. / vsData[j];
	    avgRHO += rhoData[j];
	    
	    nInBlock++;
	    
	    if (verbose)
	       fprintf(stderr, "In the block [%d][%d] : %f %f %f\n",
		       iBlock, nInBlock, vpData[j], vsData[j], rhoData[j]);
	    j++;
	 }
      }
   }
   
   /* generating model file for reflectivity */
   for (i = 0; i < nBlock; i++)
   {
      fprintf(stdout, "%f %f %f %f %f %f\n", thickBlock[i], rhoBlock[i],
	      vpBlock[i], QP, vsBlock[i], QS);
   }
}

/************************************************************************
 function median(data, i1, i2, i, length)

 Applies a running median filter on well log data

 Input parameters:
 float **data              well log data
                           data[i][0]: depth
                           data[i][1]: p-wave velocity
                           data[i][2]: density
                           data[i][3]: s-wave velocity
 int i1, i2                delimiters for medium filtering
 int i                     0: filter p-wave velocity
                           1: filter density
                           2: filter s-wave velocity
 int length                length of the filter

 Output parameters:
 filtered well log stored in vector data
**************************************************************************/
void median (float **data, int i1, int i2, int which, int length, int verbose)
{
   /* declaration of variables */
   float *dataL;                      /* auxiliar array */
   float *work;                       /* working area */
   float difference;                  /* RMS difference */
   int i, j, k, l, iBegin, iEnd;      /* counters */
   int compQSORT ();                  /* compare function for qsort */

   dataL = alloc1float(length + 1);
   work = alloc1float(i2 - i1 + 1);

   do 
   {
      for (i = i1; i <= i2; i++)
      {
	 iBegin = MAX(i1, i - (length - 1) / 2);
	 iEnd = MIN(i2, i + (length - 1) / 2);
	 l = iEnd - iBegin + 1;
	 if (l%2 == 0) 
	 {
	    iEnd++;
	    if (iEnd > i2) 
	    {
	       iEnd--;
	       iBegin--;
	    }
	    l++;
	 }
	 
	 for (k = 0, j = iBegin; j <= iEnd; j++, k++)
	 {
	    dataL[k] = data[j][which];
	 }
	 
	 qsort(dataL, l, sizeof(int), compQSORT);
	 work[i - i1] = dataL[(length - 1) / 2];
      }
      
      /* computing RMS difference */
      for (difference = 0, i = i1; i <=i2; i++) 
	 difference += (data[i][which] - work[i - i1]) * 
          	       (data[i][which] - work[i - i1]);
      difference = sqrt(difference) / (float) (i2 - i1 + 1);

      if (verbose) fprintf(stderr, "difference %f\n", difference);

      for (i = i1; i <= i2; i++) data[i][which] = work[i - i1];

   } while (difference > 0);
   
   free1float(dataL);
   free1float(work);
}

/* compare function for qsort              */
/* float *p1, *p2        values to compare */
int compQSORT (float *p1, float *p2)
{
   float diff = *p1 - *p2;
   if      (diff > 0)      return(1);
   else if (diff < 0)      return(-1);
   else  /* diff == 0 */   return(0);  
}

   

      




 

      
      
      
