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
#include "image_data_analysis.hh"

#include <stdio.h>

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define LEN 10

void ImageDataAnalysis::doIt (unsigned long *long_array, unsigned long *bg,
  int num, long size, int to_print)
{
  int k2, k3, k4;
  int foundit = FALSE;
  int count;
  if (to_print < 1) to_print = LEN;
  for (k2 = 0, k3 = 0; k2 < size && k3 < to_print; k2++) {
    for (k4 = 0, count = 0; k4 < num; k4++) {
      if (long_array[k2] != bg[k4]) count++;
    }
    if (foundit || count == num) {
      foundit = TRUE;
      printf ("long_array");
      printBytes (k2, sizeof(unsigned long), (void *)long_array);
      k3++;
    }
  }
}

void ImageDataAnalysis::doIt (float *float_array, float *bg, int num,
  long size, int to_print)
{
  int k2, k3, k4;
  int foundit = FALSE;
  int count;
  if (to_print < 1) to_print = LEN;
  for (k2 = 0, k3 = 0; k2 < size && k3 < to_print; k2++) {
    for (k4 = 0, count = 0; k4 < num; k4++) {
      if (float_array[k2] != bg[k4]) count++;
    }
    if (foundit || count == num) {
      foundit = TRUE;
      printf ("float_array[%d] = %f\n", k2, float_array[k2]);
      k3++;
    }
  }
}

void ImageDataAnalysis::doIt (unsigned int *int_array, unsigned int *bg,
  int num, long size, int to_print)
{
  int k2, k3, k4;
  int foundit = FALSE;
  int count;
  if (to_print < 1) to_print = LEN;
  for (k2 = 0, k3 = 0; k2 < size && k3 < to_print; k2++) {
    for (k4 = 0, count = 0; k4 < num; k4++) {
      if (int_array[k2] != bg[k4]) count++;
    }
    if (foundit || count == num) {
      foundit = TRUE;
      printf ("int_array");
      printBytes (k2, sizeof(unsigned int), (void *)int_array);
      k3++;
    }
  }
}

void ImageDataAnalysis::doIt (unsigned char *char_array, unsigned char *bg,
  int num, long size, int to_print)
{
  int k2, k3, k4;
  int foundit = FALSE;
  int count;
  if (to_print < 1) to_print = LEN;
  for (k2 = 0, k3 = 0; k2 < size && k3 < to_print; k2++) {
    for (k4 = 0, count = 0; k4 < num; k4++) {
      if (char_array[k2] != bg[k4]) count++;
    }
    if (foundit || count == num) {
      foundit = TRUE;
      printf ("char_array");
      printBytes (k2, sizeof(unsigned char), (void *)char_array);
      k3++;
    }
  }
}

void ImageDataAnalysis::printBytes (int elem, size_t size, void *value)
{
  unsigned char *ch = (unsigned char *)value;

  printf ("[%d] = ", elem);
  int k2;
  int k3 = elem * size;
  for (k2 = 0; k2 < size; k2++) {
    if (k2 > 0) {
      printf ("  ");
    }
    printf ("%#4X", ch[k3+k2]);
  }
  printf ("\n");
}
