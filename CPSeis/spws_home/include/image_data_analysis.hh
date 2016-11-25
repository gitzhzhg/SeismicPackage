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
#ifndef IMAGE_DATA_ANALYSIS_HH
#define IMAGE_DATA_ANALYSIS_HH

#include <stdio.h>

class ImageDataAnalysis {

public:

static void doIt
  (unsigned long *int_array,
   unsigned long *bg,
   int num,
   long size,
   int to_print = 0);

static void doIt
  (float *float_array,
   float *bg,
   int num,
   long size,
   int to_print = 0);

static void doIt
  (unsigned int *int_array,
   unsigned int *bg,
   int num,
   long size,
   int to_print = 0);

static void doIt
  (unsigned char *char_array,
   unsigned char *bg,
   int num,
   long size,
   int to_print = 0);

static void printBytes
  (int elem,
   size_t size,
   void *value);
};

#endif
