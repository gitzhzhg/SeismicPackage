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
#ifndef SEIS_LAV_POP_HH
#define SEIS_LAV_POP_HH

#include "wproc.h"
#include "sl/sl_form_pop.hh"
#include "sp/seis_inform.hh"
#include "sp/seis_plot.hh"

class SeisPlot;
class SLRadioBox;
class SeisLavInform;
class SeisLavGui;

class SeisLavPop :  public SLFPopSep {

public:
  SeisLavPop
    (Widget p,
     char *name,
     HelpCtx hctx);

  ~SeisLavPop ();

  enum {
    UNSET = -1,
    MAIN,
    MAIN_UNDER,
    TIE,
    TIE_UNDER,
    COUNT
  };

  void add
    (SeisPlot *sp,
     int which = MAIN);

  void remove
    (SeisPlot *sp,
     int which = MAIN);

  void setCurrent
    (SeisPlot *sp);

  void clearCurrent
    (SeisPlot *sp);

  void clearCurrent
    (int which);

  int which
    (SeisPlot *sp);

  virtual Widget make
    (Widget p);

  int count ();

  int type ();

  void setLav
    (SeisPlot *sp);

  virtual void manage ();

  void display ();

  virtual Boolean notifyComplex
    (SLDelay *obj,
     int ident);

  virtual void reloadDefaults
    (Boolean do_method = True);

  virtual void reloadSystemDefaults
    (Boolean do_method);

  void initialize ();

protected:
  SeisLavGui
    *_lav_gui;

  SLRadioBox
    *_lavtype;

  SeisLavInform
    *_informs[COUNT];

  SeisPlot
    *_sps[COUNT];

  char
    *_filenames[COUNT];

  double
    _lavs[COUNT];                   // largest absolute value >= 0

  long
    _current_type;
};

class SeisLavInform :  public SeisInform {

public:
  SeisLavInform
    (class SeisLavPop *lav_pop);

  virtual void newPlot
    (SeisPlot *sp);

  virtual void postScan
    (SeisPlot *sp,
     SeisPlot::ScanDir);

  virtual void notCurrentInWindow
    (SeisPlot *sp);

  virtual void nowCurrentInWindow
    (SeisPlot *sp);

  virtual void removingTie
    (SeisPlotTie *sp,
     SeisPlot *tie_sp);

  virtual void postMovie
    (SeisPlot *sp,
     SeisPlot::MovieDir);

  void update
    (SeisPlot *sp);

private:
  SeisLavPop
    *_lav_pop;
};

#endif
