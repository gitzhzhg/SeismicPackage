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

//----------------------- wbox_link.hh -------------------------//
//----------------------- wbox_link.hh -------------------------//
//----------------------- wbox_link.hh -------------------------//

//              header file for the WboxLink class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_LINK_HH_
#define _WBOX_LINK_HH_

#include <X11/Intrinsic.h>
#include <Xm/ScrollBar.h>


class WboxBox;
class WboxPoint;


class WboxLink
{

//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//

  typedef long WboxNupdateFun (void *data);

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  WboxBox   *_box;        // pointer to windowbox object.
  const int  _itab;       // tab group number (>= 1).
  const int  _iarow;      // row number of first array display row.
  int        _narow;      // number of array display rows.
  int        _narow_init; // number of rows to initially display if at bottom.
  int        _ifirst;     // array element index on first displayed row.
  int        _karray;     // last variable number of this linked array.
  int        _iacol;      // column number of first array display column.
  int        _nacol;      // number of array display columns.
  int        _vmin;       // number of first vector in this linked list.
  int        _vmax;       // number of last vector in this linked list.

  int            *_npoint_int;  // pointer to value of n.
  long           *_npoint;      // pointer to value of n.
  WboxNupdateFun *_nfun;        // pointer to n update function.

  int            *_nmaxpoint_int;  // pointer to value of nmax.
  long           *_nmaxpoint;      // pointer to value of nmax.
  WboxNupdateFun *_nmaxfun;        // pointer to nmax update function.

  Widget          _wscroll;   // scrollbar.
  int             _numrow;    // scrollbar information.
  int             _slider;    // scrollbar information.
  int             _maximum;   // scrollbar information.
  int             _value;     // scrollbar information.
  int             _column;    // scrollbar information.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:    // constructor and destructor.

  WboxLink (WboxBox *box, int itab, int iarow, int narow, int narow_init = 0);

  virtual ~WboxLink();

  void  updateArrayVisibility();
  void  updateScrollbar();

  void  addVector (int vmax)  { if(_vmin == 0) _vmin = vmax; _vmax = vmax; }

public:    // get values.

  WboxBox        *getBoxPointer()          const  { return _box; }
  int             getItab      ()          const  { return _itab; }
  int             getIarow     ()          const  { return _iarow; }
  int             getNarow     ()          const  { return _narow; }
  int             getNarowInit ()          const  { return _narow_init; }
  int             getIfirst    ()          const  { return _ifirst; }
  int             getKarray    ()          const  { return _karray; }
  int             getIacol     ()          const  { return _iacol; }
  int             getNacol     ()          const  { return _nacol; }
  int             getVmin      ()          const  { return _vmin; }
  int             getVmax      ()          const  { return _vmax; }

  int            *getNpointInt ()          const  { return _npoint_int; }
  long           *getNpoint    ()          const  { return _npoint; }
  WboxNupdateFun *getNfun      ()          const  { return _nfun; }

  int            *getNmaxpointInt ()       const  { return _nmaxpoint_int; }
  long           *getNmaxpoint ()          const  { return _nmaxpoint; }
  WboxNupdateFun *getNmaxfun   ()          const  { return _nmaxfun; }

  int             getIndex     (int irow)  const;
  int             getN         ()          const;
  int             getNmax      ()          const;

public:     // registrations etcetera.

  void  registerNPointInt      (int            *npoint_int);
  void  registerNPoint         (long           *npoint);
  void  registerNFun           (WboxNupdateFun *nfun);

  void  registerNmaxPointInt   (int            *nmaxpoint_int);
  void  registerNmaxPoint      (long           *nmaxpoint);
  void  registerNmaxFun        (WboxNupdateFun *nmaxfun);

  void  setNarow               (int narow);
  void  setIfirst              (int ifirst);
  void  setKarray              (int karray);
  void  setIacol               (int iacol);
  void  setNacol               (int nacol);

  void  putN                   (int n)  const;
  int   makeIndexVisible       (int index);

private:

  static void scrollbarCallback        (Widget, WboxLink *link,
                                        XmScrollBarCallbackStruct *call);

  static void destroyScrollbarCallback (Widget, WboxLink *link,
                                        XmAnyCallbackStruct*);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


