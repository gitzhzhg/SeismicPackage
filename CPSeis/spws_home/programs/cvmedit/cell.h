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
#ifndef _CellWin_
#define _CellWin_

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <wproc.h>
#include <ez_win.h>
#include <cprim.h>
#include <pick.h>
#include <model.h>

class Vector;

/* User data structure for windows */
typedef struct UD_cell
{long MCELL;
 long NUMCELL;
 long JCSW;
 long MIDPNT;
 long MID[ 99];
 long JXSW;
 float XC[ 99];
 long  JYSW;
 float YC[ 99];
 float S[ 99];
 Vector *vlist[4]; /* list of hilighted cells             */
 Widget shell;       /* optional dialog shell               */
 Widget form;        /* form widget for the glimits display */
 void   *cvmapp;
 struct EZwin *Cwin;
 struct HELPCTX *Hctx;
} UDCell;
 
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* Declaration of ezed trap names */
void cell_trap(Widget W, UDCell *udat, caddr_t b);
void cell_init(void *udat);
void cell_mid_(long *box, long *ncol, long *nline, char *chars,
     long *nchars,char *ENDKEY);

void   cellGetTraps(void **sesi, void **sese);
Widget cellForm(void *data);
void   cellDestroy(void *data);
void   cell_f1 (ErsModel *model,long *NUM,  long *MID,
     float *XC, float *YC,float *S, char *msg);

UDCell *cell_get_data();
void   *cellGui(Widget parent,int opt,HelpCtx Hctx,void *cvmapp);

#ifdef __cplusplus
}                   // for C++
#endif


#endif
