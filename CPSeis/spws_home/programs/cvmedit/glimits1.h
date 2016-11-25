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
#ifndef _glimits1_
#define _glimits1_
 
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include "wproc.h"
#include "model.h"
#include "model_trans.h"

class SLDelay; 
class SLDialog;
class SLSmartForm;
class SL2Text;
class CvmApp;

/* User data structure for windows */
typedef struct _Glimits
{int   nxg;
 float xorg;
 float dx;
 int   nyg;
 float yorg;
 float dy;
 int   nzg;
 float zorg;
 float dz;
 SL2Text *x1,*x2,*x3;
 SL2Text *y1,*y2,*y3;
 SL2Text *z1,*z2,*z3;

 CvmApp      *cvmapp;
 SLDialog    *shell;
 SLSmartForm *form;
 HelpCtx      Hctx;
} Glimits ;
 
 
/* Declaration of ezed trap names */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif
Glimits *glimitsGui(SLDelay *slparent,int opt, CvmApp *cvmapp, HelpCtx Hctx);
void   glimitsTrap(Widget W, Glimits *udat, caddr_t b);
void   glimitsInit(void *);
void   glimitsGuiUpdate(void *data, GridLimits *glim);
SLSmartForm *glimitsForm(void *limits);
SLDialog *glimitsDial(void *data);
void glimitsGvals(Glimits *gdat, int  *nx, float *dx, float *ox,
                  int  *ny, float *dy, float *oy,
                  int  *nz, float *dz, float *oz);
void glimitsSvals(Glimits *gdat, int  nx, float dx, float ox,
                  int  ny, float dy, float oy,
                  int  nz, float dz, float oz);
void glimitsDelete(Glimits *dat);



#ifdef __cplusplus
}                   // for C++
#endif


#endif

