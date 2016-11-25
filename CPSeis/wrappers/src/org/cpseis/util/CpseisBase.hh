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
//--------------------------- CpseisBase.hh -----------------------------//
//--------------------------- CpseisBase.hh -----------------------------//
//--------------------------- CpseisBase.hh -----------------------------//

#ifndef _HH_CPSEIS_MODULE_BASE_HH_
#define _HH_CPSEIS_MODULE_BASE_HH_

#include "c2f_interface.h"

class CpseisBase
{

//------------------------------ data ---------------------------------//
//------------------------------ data ---------------------------------//
//------------------------------ data ---------------------------------//

public:

  typedef void ModuleCreate  (F90Pointer *fpoint);
  typedef void ModuleDestroy (F90Pointer *fpoint);
  typedef void ModuleUpdate  (F90Pointer *fpoint);
  typedef void ModuleWrapup  (F90Pointer *fpoint);

  typedef void ModuleOneset  (F90Pointer *fpoint, INTEGER *ntr,
         DOUBLE hd[], REAL tr[],
         const INTEGER *lenhd, const INTEGER *lentr, const INTEGER *maxtr);

  typedef void ModuleTwosets (F90Pointer *fpoint, INTEGER *ntr,
         DOUBLE hd1[], REAL tr1[],
         const INTEGER *lenhd1, const INTEGER *lentr1, const INTEGER *maxtr1,
         DOUBLE hd2[], REAL tr2[],
         const INTEGER *lenhd2, const INTEGER *lentr2, const INTEGER *maxtr2);

private:

  const char         *_name;
  const char         *_label;
  ModuleCreate       *_create;
  ModuleDestroy      *_destroy;
  ModuleUpdate       *_update;
  ModuleWrapup       *_wrapup;
  ModuleOneset       *_oneset;
  ModuleTwosets      *_twosets;
  F90Pointer          _fpoint;

  int                 _created;
  int                 _need_label;
  int                 _need_request;
  int                 _setup_only;

  int                 _nwih1;
  int                 _ndpt1;
  int                 _numtr1;
  int                 _lenhd1;
  int                 _lentr1;
  int                 _maxtr1;
  double             *_hd1;
  float              *_tr1;

  int                 _nwih2;
  int                 _ndpt2;
  int                 _numtr2;
  int                 _lenhd2;
  int                 _lentr2;
  int                 _maxtr2;
  double             *_hd2;
  float              *_tr2;

  int                 _ntraces1;
  int                 _ntraces2;
  int                 _ngathers1;
  int                 _ngathers2;
  float               _mav1;
  float               _mav2;

//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//

public:

           CpseisBase (const char    *name,
                       ModuleCreate  *create,
                       ModuleDestroy *destroy,
                       ModuleUpdate  *update,
                       ModuleWrapup  *wrapup,
                       ModuleOneset  *oneset,
                       ModuleTwosets *twosets);
  virtual ~CpseisBase ();

  void update   ();
  void putTrace (int itr, double *hd, float *tr);
  void getTrace (int itr, double *hd, float *tr);
  int  execute  (int ntr);
  void wrapup   ();

  const char *getName     ()                  { return _name; }
  const char *getLabel    ()                  { return _label; }
  void        setLabel    (const char *label) { if(label != 0) _label = label; }
  int         isInputTool ()                  { return _need_label && !_need_request; }

private:

  void freeBuffers();

//----------------------------- end of class ------------------------------//
//----------------------------- end of class ------------------------------//
//----------------------------- end of class ------------------------------//

};

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
