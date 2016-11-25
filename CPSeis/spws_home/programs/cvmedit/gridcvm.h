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
#ifndef _GRID_EDIT_H
#define _GRID_EDIT_H

#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include "grid_data.hh"


class VectorLinkedList;
class SLDialog;
class SLSmartForm;
class SL2Text;
class SLpFile;
class RadioList;
class gridData;
class CvmApp;
class SeisTransf;

typedef struct _gridcvm
 { char        filei[120];
   int         word_out;
   int         mode;
   int         start;
   int         slices;
   void        *glim;  /*pointer to Glimits object */
   SLDialog    *shell;
   SLDialog    *sav_shell;
   SLSmartForm *form;
   SL2Text     *xslice;
   RadioList   *visRad;
   RadioList   *modRad;
   SLpFile     *infcw;
   CvmApp      *cvmapp;
 } gridcvm;

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* Public methods of the gridcvm object */
void     *gridGUI(int , Widget ,struct HELPCTX *,void *cvmapp);
void      gridUpdate(void *grid);
SLSmartForm *gridForm(void *grid);
SLDialog *gridDial(void *grid);
void      gridInit(void *grid);
void      gridPlotUnd(gridcvm *gcvm);
gridData *gridDataPntr(gridcvm *gcvm);
void      gridSetDataPntr(gridcvm *gcvm, gridData *pntr);
CvmApp   *gridCvmAppPntr(gridcvm *gcvm);
gridData *gridRead(char *hfil, gridcvm *gcvm);
char     *gridGetSlice(char *dfile, char *word_string,
           int  N1, int  N2, int  N3, int slice);
int       gridMatchSeis(SeisTransf *sptrans, gridData *gobj);
void      gridKillData(gridcvm *gcvm);

int    grid_to_layr(VectorLinkedList *m_vlist,
        VectorLinkedList *c_vlist,
        int *nx, float *xorg, float *dx,
        int *nz, float *zorg, float *dz, float *garr);

#ifdef __cplusplus
}                   // for C++
#endif

#endif


