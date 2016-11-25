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
#ifndef _TF_HSTATS_
#define _TF_HSTATS_

#include <stdio.h>
#include <string.h>
#include "wrdcnvrt.h"
#include "tfdefs.h"

#ifdef sun
#include "spws_sunos.h"
#endif

/*As far as I can tell this header and code was only used in converting
  segy to gocad and is no longer used MLS 07-2001*/

#define NumCPShd 64 /*Leaving this as is, see above*/

typedef struct _HdrStats
{
  int   count;
  int   nhdr;
  float *hd;
  float hold[NumCPShd];
  float hmin[NumCPShd];
  float hmax[NumCPShd];
  float hbin[NumCPShd];

} HdrStats;

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* prototypes of functions in tf_hstat.c */
HdrStats *tf_hstats_new();
void tf_hstats_del(HdrStats *st);
int  tf_hstats_upd(float *cpshd, HdrStats *stat);
void tf_hstats_pr(char *file, HdrStats *stat);
HdrStats *tf_hstats_ginfo(char *seisin, TF_Global *global);
HdrStats *tf_hstats_ginfo1(char *seisin, TF_Global *global, int nscan);


#ifdef __cplusplus
}                   // for C++
#endif

#endif


