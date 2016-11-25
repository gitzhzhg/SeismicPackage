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
#ifndef _PCARDGOWR_
#define _PCARDGOWR_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cprim.h"
#include "wrdcnvrt.h"
#include "dbutil.h"
#include "pick.h"
#include "model.h"


#ifdef __cplusplus
extern "C" {                 // for C++
#endif
 
/* Prototypes of functions  */
int  pcardgowr(ErsModel *model, char *fileo);
void pcardgowr_pik(ErsModel *model, char *fileo);
void pcardgowr_cpntr(ErsModel *model, char *fileo);
void pcardgowr_bnd(ErsModel *model, char *fileo);
void pcardgowr_mat(ErsModel *model, char *fileo);
void pcardgowr_xslim(ErsModel *model, char *fileo);
void pcardgowr_g3dl(ErsModel *model,char *fileo);
void pcardgowr_grid(GridLimits *glimits, char *dfile, int wtype,char
*fileo, char *obj_name);
#ifdef __cplusplus
}                   // for C++
#endif

#endif

