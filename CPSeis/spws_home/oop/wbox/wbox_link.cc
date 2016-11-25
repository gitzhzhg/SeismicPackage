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

//---------------------- wbox_link.cc -------------------------//
//---------------------- wbox_link.cc -------------------------//
//---------------------- wbox_link.cc -------------------------//

//          implementation file for the WboxLink class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_link.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_screen.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>



//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxLink::WboxLink
             (WboxBox *box, int itab, int iarow, int narow, int narow_init)
         :
               _box            (box),
               _itab           (itab),
               _iarow          (iarow),
               _narow          (narow),
               _narow_init     (narow_init),
               _ifirst         (1),
               _karray         (0),
               _iacol          (0),
               _nacol          (0),
               _vmin           (0),
               _vmax           (0),

               _npoint_int     (NULL),
               _npoint         (NULL),
               _nfun           (NULL),
               _nmaxpoint_int  (NULL),
               _nmaxpoint      (NULL),
               _nmaxfun        (NULL),

               _wscroll        (NULL),
               _numrow         (0),
               _slider         (0),
               _maximum        (0),
               _value          (0),
               _column         (0)
{
  assert(_box);
  assert(_itab  >= 1);
  assert(_iarow >= 1);
  if(_narow <= 0) _narow = 10;
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxLink::~WboxLink()
{
}



//----------------------- get values -------------------------//
//----------------------- get values -------------------------//
//----------------------- get values -------------------------//


int
WboxLink::getIndex(int irow)
const
{
  assert(irow >= _iarow);
  return (_ifirst + irow - _iarow);
}


int
WboxLink::getN()
const
{
  int n = -1;
  if     (_npoint    ) n = (int)*_npoint;
  else if(_npoint_int) n = (int)*_npoint_int;
  else if(_nfun      ) n = (int) _nfun(_box->getUserData());
  assert(n >= 0);
  int nmax = getNmax();
  if(n > nmax) n = nmax;
  return n;
}


int
WboxLink::getNmax()
const
{
  int nmax = -1;
  if     (_nmaxpoint    ) nmax = (int)*_nmaxpoint;
  else if(_nmaxpoint_int) nmax = (int)*_nmaxpoint_int;
  else if(_nmaxfun      ) nmax = (int) _nmaxfun(_box->getUserData());
  assert(nmax >= 0);
  return nmax;
}



//------------------ registrations etcetera --------------------//
//------------------ registrations etcetera --------------------//
//------------------ registrations etcetera --------------------//


void  WboxLink::registerNPointInt(int *npoint_int)
{
  assert(npoint_int);
  _npoint_int = npoint_int;
  _npoint     = NULL;
  _nfun       = NULL;
}


void  WboxLink::registerNPoint(long *npoint)
{
  assert(npoint);
  _npoint     = npoint;
  _npoint_int = NULL;
  _nfun       = NULL;
}


void  WboxLink::registerNmaxPointInt(int *nmaxpoint_int)
{
  assert(nmaxpoint_int);
  _nmaxpoint_int = nmaxpoint_int;
  _nmaxpoint     = NULL;
  _nfun          = NULL;
}


void  WboxLink::registerNmaxPoint(long *nmaxpoint)
{
  assert(nmaxpoint);
  _nmaxpoint     = nmaxpoint;
  _nmaxpoint_int = NULL;
  _nmaxfun       = NULL;
}


void  WboxLink::registerNFun(WboxNupdateFun *nfun)
{
  assert(nfun);
  _nfun       = nfun;
  _npoint     = NULL;
  _npoint_int = NULL;
}



void  WboxLink::registerNmaxFun(WboxNupdateFun *nmaxfun)
{
  assert(nmaxfun);
  _nmaxfun       = nmaxfun;
  _nmaxpoint     = NULL;
  _nmaxpoint_int = NULL;
}



void  WboxLink::setNarow(int narow)
{
  _narow = narow;
}


void  WboxLink::setIfirst(int ifirst)
{
  _ifirst = MaximumValue(ifirst, 1);
}


void  WboxLink::setKarray(int karray)
{
  _karray = karray;
}


void  WboxLink::setIacol(int iacol)
{
  _iacol = iacol;
}


void  WboxLink::setNacol(int nacol)
{
  _nacol = nacol;
}



// resets value of n in user area if using pointer:
// resets value of n in user area if using pointer:
// resets value of n in user area if using pointer:

void  WboxLink::putN(int n)  const
{
  assert(n >= 0 && n <= getNmax());
  if     (_npoint    ) *_npoint     = (long)n;
  else if(_npoint_int) *_npoint_int = (int)n;
}



//------------------- make index visible --------------------------//
//------------------- make index visible --------------------------//
//------------------- make index visible --------------------------//

        // adjusts ifirst so that index is visible.
        // first puts index within valid range.
        // does nothing if none of array is visible.
        // does nothing if index is already visible.
        // reminder: ifirst >= 1.
        // reminder: index  >= 1.
        // returns the (possibly corrected) index which is now visible.

int WboxLink::makeIndexVisible(int index)
{
  int n      = getN   ();
  int nmax   = getNmax();
  if(index > n + 1) index = n + 1;
  if(index >  nmax) index = nmax;
  if(index <     1) index = 1;

  if(_narow <= 0) return index;
  if(index >= _ifirst && index <= _ifirst + _narow - 1) return index;
  if     (index == _ifirst - 1)      _ifirst--;
  else if(index == _ifirst + _narow) _ifirst++;
  else if(index <= _narow)           _ifirst = 1;
  else                     _ifirst = MaximumValue(index - _narow / 2, 1);
  return index;
}
  


//------------------------ callbacks -----------------------------//
//------------------------ callbacks -----------------------------//
//------------------------ callbacks -----------------------------//

         // private static functions.

void WboxLink::scrollbarCallback(Widget, WboxLink *link,
                                 XmScrollBarCallbackStruct *call)
{
  link->_box->sendImmediateEvent("BSCROLL");
  link->setIfirst(call->value + 1);
  link->_value = call->value;
  link->_box->sendImmediateEvent("SCROLL");
}


void WboxLink::destroyScrollbarCallback(Widget, WboxLink *link,
                                        XmAnyCallbackStruct*)
{
  link->_wscroll = NULL;
}



//-------------------- update array visibility -----------------------//
//-------------------- update array visibility -----------------------//
//-------------------- update array visibility -----------------------//

    // public.

void WboxLink::updateArrayVisibility()
{
  int index = _ifirst;
  int n     = getN();
  int nmax  = getNmax();
  if(index > n + 1) index = n + 1;
  if(index >  nmax) index = nmax;
  if(index <     1) index = 1;
  makeIndexVisible(index);
}



//---------------------- update scrollbar -------------------------//
//---------------------- update scrollbar -------------------------//
//---------------------- update scrollbar -------------------------//

    // public.
    // the scrollbar will not adjust properly until the second
    //   reconfigure if its parent is a form widget.

#define PRMA(number) if(allbox->getDebug() >= number)
#define PRMB         printf("wbox: %d scrollbar %d ", _box->getIbox(), _itab);
#define PRMC(word)   PRMA(2) PRMB; PRMA(2) printf("%s\n", word)
#define PRMD         PRMA(3) PRMB; PRMA(3) printf


void WboxLink::updateScrollbar()
{
  WboxScreen *screen = _box->getScreenPointer();
  WboxAllbox *allbox = _box->getAllboxPointer();

  if(allbox->getScrollbarFlag() <= 0) return;

  int n       = getN();
  int nmax    = getNmax();
  int value   = _ifirst - 1;
  int numrow  = _narow;
  int minimum = 0;
  int maximum = n + 1;
  if(maximum > nmax) maximum = nmax;
  if(maximum < _ifirst + numrow - 1) maximum = _ifirst + numrow - 1;
  int slider  = numrow;
  if(slider > maximum - value) slider = maximum - value;
  int column  = _iacol + _nacol + 1;

  if((allbox->getScrollbarFlag() == 1 && _ifirst == 1 && maximum <= numrow) ||
     _nacol < 1 || _narow < 1)
       {
       if(_wscroll != NULL && XtIsManaged(_wscroll))
            {
            XtUnmanageChild(_wscroll);
            PRMC("unmanaged");
            }
       return;
       }
  else if(_wscroll == NULL)
       {
       }
  else if(numrow == _numrow &&
          slider == _slider && maximum == _maximum &&
          value  == _value  && column  == _column)
       {
       if(!XtIsManaged(_wscroll))
            {
            XtManageChild(_wscroll);
            PRMC("managed");
            }
       return;
       }

  Dimension width = allbox->getScrollbarWidth();
  if(width > 3 * screen->getCellx()) width = 3 * screen->getCellx();
  Dimension height = numrow * screen->getCelly();
  Position x = column * screen->getCellx() + 1;
  Position y = _iarow * screen->getCelly() - screen->getAscent() - 1;

  Arg args[30];
  int i = 0;
  XtSetArg (args[i], XmNsliderSize   , slider ); i++;
  XtSetArg (args[i], XmNmaximum      , maximum); i++;
  XtSetArg (args[i], XmNvalue        , value  ); i++;
  XtSetArg (args[i], XmNpageIncrement, numrow ); i++;
  XtSetArg (args[i], XmNwidth        , width  ); i++;
  XtSetArg (args[i], XmNheight       , height ); i++;
  XtSetArg (args[i], XmNx            , x      ); i++;
  XtSetArg (args[i], XmNy            , y      ); i++;
  XtSetArg (args[i], XmNleftOffset   , (int)x ); i++;
  XtSetArg (args[i], XmNtopOffset    , (int)y ); i++;

  if(_wscroll == NULL)
     {
     XtSetArg (args[i], XmNleftAttachment   , XmATTACH_FORM            ); i++;
     XtSetArg (args[i], XmNtopAttachment    , XmATTACH_FORM            ); i++;
     XtSetArg (args[i], XmNtraversalOn      , FALSE                    ); i++;
     XtSetArg (args[i], XmNinitialResourcesPersistent, FALSE           ); i++;
     XtSetArg (args[i], XmNorientation      , XmVERTICAL               ); i++;
     XtSetArg (args[i], XmNincrement        , 1                        ); i++;
     XtSetArg (args[i], XmNminimum          , minimum                  ); i++;
     XtSetArg (args[i], XmNtopShadowColor   , screen->getTopShadow()   ); i++;
     XtSetArg (args[i], XmNbottomShadowColor, screen->getBottomShadow()); i++;
     XtSetArg (args[i], XmNforeground       , screen->getPeak()        ); i++;
     XtSetArg (args[i], XmNbackground       , screen->getPeak()        ); i++;
     XtSetArg (args[i], XmNtroughColor      , screen->getTrough()      ); i++;

///  _wscroll = XmCreateScrollBar(_box->getW(), "wboxscrollbar", args, i);

///           the above line is replaced by the following two lines
///           to get rid of warning messages:

     static char wboxscrollbar[] = "wboxscrollbar";
     _wscroll = XmCreateScrollBar(_box->getW(),  wboxscrollbar , args, i);

     XtManageChild(_wscroll);
     XtAddCallback(_wscroll, XmNvalueChangedCallback,
                            (XtCallbackProc)scrollbarCallback, this);
     XtAddCallback(_wscroll, XmNdragCallback,
                            (XtCallbackProc)scrollbarCallback, this);
     XtAddCallback(_wscroll, XmNdestroyCallback,
                            (XtCallbackProc)destroyScrollbarCallback, this);
     PRMC("created and managed");
     }
  else
     {
     XtUnmanageChild(_wscroll);  // to force it to resize correctly.
     XtSetValues(_wscroll, args, i);
     PRMC("modified");
     if(!XtIsManaged(_wscroll))
          {
          XtManageChild(_wscroll);
          PRMC("managed");
          }
     }
  PRMD("width=%d height=%d x=%d y=%d\n", width, height, x, y);
  _slider  = slider;
  _maximum = maximum;
  _value   = value;
  _numrow  = numrow;
  _column  = column;

  PRMD("nrow=%d iarow=%d narow=%d numrow=%d column=%d maximum=%d\n",
             _box->getNrow(), _iarow, _narow, _numrow, _column, _maximum);
  PRMD("n=%d nmax=%d ifirst=%d slider=%d value=%d\n",
                         n, nmax, _ifirst, _slider, _value);
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

