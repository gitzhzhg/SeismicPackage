/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
/*------------------------------------------------------------------
 *USER DOC
 *Name   : imageMedian 
 *Purpose: Calculate the median amplitude used to rescale data for
 *         median ram color displays. Also return a ct factor to derive
 *         a ct based on median value. 
 *
 *Author : Michael L. Sherrill
 *Date   : 02/92 (C++ version 4/97)
 *
 * Function Definition:
 * float image_median (float MaxVal, long NumVals,
 *                     float *median_scale)
 *
 * MaxVal    in         Largest value to scale data to.
 *
 * NumVals   in         Total number of trace samples.
 *
 * median_scale out     Median scale factor.
 *
 * ct_factor out        Ct factor.
 *
 *NOTES:
 * 1. Derived from fortran code originaly used in cstd by Chuck Burch
 *    and Karen Goodger in CPS process COLR
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <math.h>
#include <limits.h>
#include "plot_image.hh"
#include "image_amplitude_recovery.hh"

void  PlotImage::imageMedian(  float           scale_amp, 
                               int             numvals, 
                               long            trace_index,
                               float           *median_scale,
                               float           *ct_factor)

{
long   i, j, k;
long   bins = 128;
long   BinIndex;
long   count = 0;
long   NumToFind;
long   *population;
float  *percents;
float  *amplitude;
float  factor;
long num_traces;
long samples_completed;


 /*allocate the population, amplitude and percentage arrays*/
 /*could use percents array to derive different scale_amps*/
 population = (long *)  calloc( 1, (int)(bins * sizeof(long)));
 amplitude  = (float *) malloc( (int)(8 * sizeof(float)));
 percents   = (float *) malloc( (int)(8 * sizeof(float)));

 if(population == NULL || amplitude == NULL || percents == NULL)
   {
   /*need to put better error handling here later*/
   printf("allocation error in image_median\n");
   return;
   }

/*assign percentages--only 50 percent is currently used*/
  percents[0] = .50;
  percents[1] = .60;
  percents[2] = .70;
  percents[3] = .80;
  percents[4] = .90;
  percents[5] = .95;
  percents[6] = .98;
  percents[7] = .99;


 /*scale largest value into possible number of byte bins*/
 factor = 128.0 / scale_amp;  

 /*fill the population array*/
 if(numvals > 100000) numvals = 100000; /*should be enough points to sample*/ 

 num_traces = numvals  / getSamplesInMemory();

 for(i=0;i<num_traces;i++)
   {
   _amp_recovery->scaleDataForDisplay(0, trace_index + i,0,_nsamp-1);
   for(j=0;j<getSamplesInMemory();j++)
     {
     if((int)_amp_recovery->getScaledByteDataValue(j) != 128) 
       { 
       /* next yields 1 thru 127*/
       BinIndex = abs((int)_amp_recovery->getScaledByteDataValue(j)-128); 
       population[BinIndex] = population[BinIndex] + 1;
       count++;
       }
     }
   samples_completed = (i+1) * getSamplesInMemory();
   if(samples_completed >= numvals) i = num_traces;//end loop
   }
  count--;


 /*find the median value*/
 j = k = 0;
 NumToFind = (long)(((float)count) * percents[k]);

 for(i=1;i<bins;i++)
    {
    j = j + population[i];
    while(j>=NumToFind)
       {
       amplitude[k] = (((float)i) + 0.5) / factor;
       if(k>=7) /*break out of loop*/
          {
          j = NumToFind - 1;
          i = bins;
          }
       else    /*continue loop*/
          {
          k++;
          NumToFind = (long)(((float)count) * percents[k]);
          }
       }
    }



 /*get the median scale factor and the ct factor*/
 *median_scale = 1.0 / amplitude[0];
 *ct_factor = amplitude[0];

 //temporary
 printf("The median scaler is %10.3e based on a median amp of %10.3e\n",
        *median_scale, amplitude[0] );


 free(population);
 free(amplitude);
 free(percents);


}
