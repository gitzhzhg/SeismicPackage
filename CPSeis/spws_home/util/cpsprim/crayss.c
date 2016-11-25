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
#include "c2f_interface.h"

#include <math.h>

/* These routines are always called from fortran */
/* Not needed on the cray, since they are cray routines */
#ifdef NEED_CAPITALS
#define isamax_ ISAMAX
#define ismax_  ISMAX
#define ismin_  ISMIN
#define sasum_  SASUM
#define isrchne_ ISRCHNE
#endif
#if (VMS || _AIX || __hpux)
#define isamax_ isamax
#define ismax_  ismax
#define ismin_  ismin
#define sasum_  sasum
#define isrchne_ isrchne
#endif


int isamax_(int *, float x[], int *);
int ismax_(int *, float x[], int *);
int ismin_(int *, float x[], int *);
int isrchne_(int *, int  x[], int *, int *);
float sasum_(int *, float x[], int *);

int isamax_(int *n, float x[], int *inc)
{ double big;
  int i,j,k;
  big = x[0];
  j = 0;
  k = 1;

 for(i=0;i<*n;i++)
   { if(big < fabs(x[j]) )
       { big = fabs(x[j]);
         k   = i+1; }
     j += *inc;
   }

 return k;
}

int ismax_(int *n, float x[], int *inc)
{ float big;
  int i,j,k;
  big = x[0];
  j = 0;
  k = 1;

 for(i=0;i<*n;i++)
   { if(big < x[j] )
       { big = x[j];
         k   = i+1; }
     j += *inc;
   }

 return k;
}

int ismin_(int *n, float x[], int *inc)
{ float small;
  int i,j,k;
  small = x[0];
  j = 0;
  k = 1;

 for(i=0;i<*n;i++)
   { if(small > x[j] )
       { small = x[j];
         k   = i+1; }
     j += *inc;
   }

 return k;
}

int isrchne_(int *n, int  x[], int *inc, int *target)
{ float small;
  int i,j,k;
  j = 0;
  k = 1;

 for(i=0;i<*n;i++)
   { k = i+1;
     if(*target !=  x[j] ) break;
     j += *inc;
   }

 return k;
}

float sasum_(int *n, float  x[], int *inc)
{ float sum;
  int i,j,k;
  double xd;
  j = 0;
  sum = 0.0;

 for(i=0;i<*n;i++)
   { xd = x[j];
     sum = sum +  fabs(xd);
     j += *inc;
   }

 return sum;
}

