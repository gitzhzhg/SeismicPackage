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
#include "tfio.h"
#include "tfdefs.h"
/*------------------------------------------------------------------
C\USER DOC
 *Name   : byte_to_float_
 *Purpose: Converts a byte trace to floating point and interpolates
 *         from number of samples in to number of samples out. 
 *
 *Author : Michael L. Sherrill 
 *Date   : 01/23/92
 *Revised: 07/17/96  Vunderink
 *
 * int  byte_to_float_(unsigned char ByteIn[], int  *SamplesIn,  
 *                    int  *SamplesOut, float *MaxLav, float FloatOut[])
 *
 * ByteIn       in      Array of byte data to convert
 * SamplesIn    in      Number of byte data points
 * SamplesOut   in      Number of float data points to output
 * MaxLav       in      Largest float value in data set or individual trace.
 *                      If data was read in with norm option 1 we want to
 *                      scale data to header 25, the largest lav of the trace.
 *                      If data was read in with norm option 0 we want to
 *                      scale data to the largest lav in the data set.
 * FloatOut     out     Array of float values returned
 *
 *
 *
 *NOTES:
 * 1.  
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 *96/07/17  Vunderink   Inserted into the conlib library.
 *92/07/15  Day         Changed name for fortran access.
C\END DOC
 *------------------------------------------------------------------*/

int  byte_to_float_(unsigned char  ByteIn[], 
                   int            *SamplesIn,
                   int            *SamplesOut, 
                   float          *MaxLav, 
                   float          FloatOut[])


{
 int   i;
 int   sub;
 int   istat     = 0;
 int   MidPoint  = 128;
 float ByteRange = 127.0;
 float rem;
 float ScaleFactor;
 float stretch;
 float part;


 /*return if we havent received a good lav to rescale to*/
 if(*MaxLav == 0.0)  
 {
  istat = 1;
  return(istat);
 }


 ScaleFactor = *MaxLav /ByteRange;

 if(*SamplesIn == *SamplesOut)
 {
  for(i=0;i<*SamplesIn;i++)  
  {
   FloatOut[i] =((float)ByteIn[i] - (float)MidPoint) * ScaleFactor;
  }
 }
 else
 {
  stretch = (*(float *) SamplesIn - 1.0) / (*(float *) SamplesOut - 1.0);
  for(i=0;i<*SamplesOut;i++)
  {
   rem = i * stretch;
   sub = rem;
   if(sub > *SamplesIn - 2) sub = rem = *SamplesIn - 2;
   part = rem - sub;
   ByteIn[sub]   -= MidPoint;
   ByteIn[sub+1] -= MidPoint;
   FloatOut[i] = 
      ((float)ByteIn[sub] + (part*((float)ByteIn[sub+1]-(float)ByteIn[sub]))) 
      * ScaleFactor;
  }
 }  
  
 return(istat);

}
