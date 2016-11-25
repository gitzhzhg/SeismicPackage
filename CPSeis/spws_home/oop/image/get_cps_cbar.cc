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
 *Name   : getCpsCbar 
 *Purpose: Read and store values from cps type color bar file.      
 *
 *Author : Michael L. Sherrill
 *Date   : 02/92 (C++ version 4/97)
 *
 * Function Definition:
 * long getCpsCbar (char *filename, float *rgb, long *numcolors,
                    long *tracevalues)   
 *
 *
 * filename    in        File name of cps type color bar file.
 * rgb         out       Array of color bar rgbs and amplitudes if any.
 * numcolors   out       The total number of rgb values
 * tracevalues out       True if we have amplitudes in the color file
 *
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
#include <cenv.h>
#include "plot_image.hh"


#define OK 1   
#define ERROR 0
#define MAXCOLORS 256

long PlotImage::getCpsCbar( char  *filename,
                            float *rgb, 
                            long  *numcolors,
                           long  *tracevalues)

{
 FILE *file_in;
 long i = 0;
 long j, k;
 float amplitude = 0.0;
 int stat;

 stat = strlen(filename);
 if(!stat) return(ERROR);

 file_in = fopen(filename,"r");
 if(file_in == NULL) return (ERROR);


/*dummy read of cps hardcoded 999 at first of file*/
 stat = fscanf(file_in, "%10f", &rgb[i]);
 if(stat < 1){ fclose(file_in); return(ERROR);}


/*read in the rgb values*/
 for(k=0;k<MAXCOLORS;k++)
    {
    stat = fscanf(file_in," %f %f %f %f ",&rgb[i],&rgb[i+1],&rgb[i+2], 
                  &rgb[i+3]);
    if(stat == EOF) 
       {
       k = MAXCOLORS;  
       fclose(file_in);
       *numcolors = (i+1) / 4;  /*number of colors in this file*/
       }
    else
       {
       if(stat != 4) 
          {
          fclose(file_in);
          return(ERROR);  /*error on read*/
          }
       i += 4;
       }
    }



/*see if there are any amplitude values in the file*/
 j = 3;
 for(i=0;i<*numcolors-1;i++)
    {
    amplitude = rgb[j];
    if(amplitude) *tracevalues = True;
    j += 4;
    }


 return(OK);

} 
