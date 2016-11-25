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
 *Name   : imagePercent 
 *Purpose: Calculate amplitude range of data based on user's percent
 *         to color request. Values are approximate of those derived
 *         on the Cray since we may have been in byte format. 
 *Author : Michael L. Sherrill
 *Date   : 02/92 (C++ version 4/97) 
 *
 * Function Definition:
 *
 * void imagePercent( float scale_amp, long LowPercent, long HiPercent,
 *                     long numvals,float *LowAmp, float *HiAmp,
 *                     long trace_index , long histogram[])
 *
 * MaxVal     in         Largest value to scale data to
 * LowPercent in         Percentage of low amplitude limit.
 * HiPercent  in         Percentage of hi amplitude limit.
 * NumVals    in         Total number of values to work with
 * LowAmp     out        New minimum value to color.
 * HiAmp      out        New maximum value to color.
 * trace_index in        index of current trace.
 * histogram  out        History array of amplitudes.
 *
 *NOTES:
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
#include <math.h>
#include "plot_image.hh"
#include "image_amplitude_recovery.hh"


void PlotImage::imagePercent( float scale_amp, long LowPercent, long HiPercent,
                             int numvals,float *LowAmp, float *HiAmp, 
                             long trace_index, long histogram[])

{
long   i,j;
long   bins = 127;
float  ByteRange = 127.0;
long   BinIndex;
long   *population;
float  amplitude;
float  factor;
float  hifactor;
float  lowfactor;
float  temp;
Boolean found_amp = False;
long num_traces;

 hifactor = HiPercent * .01;
 lowfactor = LowPercent * .01;




/*scale largest value into possible number of byte bins*/
 factor = scale_amp / ByteRange;

/*calculate new minimum value to color*/
 if(lowfactor)
    {
    population = (long *)  calloc( 1, (int)((bins + 1) * sizeof(long)));
    if(population == NULL)
       {
       /*need to handle this error better later*/
       printf("allocation error in image_percent\n");
       return;
       }
    num_traces = numvals / getSamplesInMemory(); 
    for(i=0;i<num_traces;i++)
       {
       _amp_recovery->scaleDataForDisplay(0,trace_index+i,0,_nsamp - 1);
       for(j=0;j<getSamplesInMemory();j++)
         {
         if((int)_amp_recovery->getScaledByteDataValue(j) < 128)
           {
           found_amp = True;
           BinIndex = 127 - (int)_amp_recovery->getScaledByteDataValue(j);
           population[BinIndex] = population[BinIndex] + 1;
           }
         }
       }
    if(found_amp)
      {
      for(i=0;i<126;i++) 
         {
         population[i+1] += population[i];
         histogram[i+1] = population[i+1];
         }
      amplitude = lowfactor * population[bins-1];
      i=0;
      while(amplitude > population[i])i++;
      i = min(i+1,127);
      temp = factor * i;
      *LowAmp = -temp;
      }
    else
      {
      *LowAmp = 0.0;
      }
    free(population);
    }
  else
    {
      *LowAmp = 0.0;
    }



 found_amp = False;
 /*calculate new maximum value to color*/
 if(hifactor)
    {
    population = (long *)  calloc( 1, (int)((bins + 1) * sizeof(long)));
    if(population == NULL)
       {
       /*need to handle this error better later*/
       printf("allocation error in image_percent\n");
       return;
       }
    num_traces = numvals / getSamplesInMemory(); 
    for(i=0;i<num_traces;i++)
       {
       _amp_recovery->scaleDataForDisplay(0,trace_index+i,0,_nsamp-1);
       for(j=0;j<getSamplesInMemory();j++)
         {
         if((int)_amp_recovery->getScaledByteDataValue(j) > 128)
           {
           found_amp = True;
           BinIndex = (int)_amp_recovery->getScaledByteDataValue(j)  - 129;
           population[BinIndex] = population[BinIndex] + 1;
           }
         }
       }
    if(found_amp)
      {
      for(i=0;i<126;i++) 
         {
         population[i+1] += population[i];
         histogram[i+128] = population[i+1];
         }
      amplitude = hifactor * population[bins-1];
      i=0;
      while(amplitude > population[i])i++;
      i = min(i+1,127);
      *HiAmp = factor * i;
      }
    else
      {
      *HiAmp = 0.0;
      }
    free(population);
    }
  else
    {
      *HiAmp = 0.0;
    }



 //temporary
 printf("The low and hi amplitudes based on the negative percentage of %d \n\
and postitive percentage of %d are %10.3e and %10.3e\n", 
        LowPercent, HiPercent, *LowAmp, *HiAmp); 

}
