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

//------------------------ statbox_compare.hh ---------------------//
//------------------------ statbox_compare.hh ---------------------//
//------------------------ statbox_compare.hh ---------------------//

//            header file for the StatboxCompare class
//                derived from the SLDatabox class
//                      subdirectory statgui

 
#ifndef _STATBOX_COMPARE_HH_
#define _STATBOX_COMPARE_HH_

#include "sl/sl_databox.hh"


class StatboxCompare  :  public SLDatabox
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class StaticManager   *_manager;
  class StatguiCompare  *_gui;

  long  _interp;  // one of the StaticDataset::INTERP enums.
  long  _extrap;  // one of the StaticDataset::EXTRAP enums.
  int   _show;    // what kind of value to plot.
  long  _xdist;   // distance for reduced weights.
  long  _ydist;   // distance for reduced weights.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  StatboxCompare (SLDelay *slparent, StaticManager *manager);
  virtual ~StatboxCompare();

  void    scrollTable();

  StaticManager   *manager             ()  const  { return _manager; }
  StatguiCompare  *getStatguiCompare   ()  const  { return _gui; }
  long             getXindex (long index)  const;
  long             getYindex (long index)  const;

  void setStatguiCompare    (StatguiCompare *gui)         { _gui = gui; }

  long getInterp  ()          const  { return  _interp; }
  long getExtrap  ()          const  { return  _extrap; }
  int  getShow    ()          const  { return  _show; }
  long getXdist   ()          const  { return  _xdist; }
  long getYdist   ()          const  { return  _ydist; }

  void stepInterp ();
  void stepExtrap ();
  void stepShow   (int step);
  void setShow    (int value)        { _show  = value; }
  void setXdist   (long value)       { _xdist = value; }
  void setYdist   (long value)       { _ydist = value; }

private:

  virtual void makeHelper();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
