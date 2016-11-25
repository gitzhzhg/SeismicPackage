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
#ifndef TRACE_SELECTOR_NDO_POP_H
#define TRACE_SELECTOR_NDO_POP_H

#include "wproc.h"
#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_text_box.hh"

enum {
  FILE1_HEADER,
  FILE2_HEADER,
  FILE3_HEADER,
  FILE4_HEADER,
  FILE5_HEADER,
  SEL1_ORDER,
  SEL2_ORDER,
  SEL3_ORDER,
  SEL4_ORDER,
  SEL5_ORDER,
  FILE1_MIN,
  FILE2_MIN,
  FILE3_MIN,
  FILE4_MIN,
  FILE5_MIN,
  FILE1_MAX,
  FILE2_MAX,
  FILE3_MAX,
  FILE4_MAX,
  FILE5_MAX,
  FILE1_INC,
  FILE2_INC,
  FILE3_INC,
  FILE4_INC,
  FILE5_INC,
  FILE1_TOT,
  FILE2_TOT,
  FILE3_TOT,
  FILE4_TOT,
  FILE5_TOT,
  SEL1_MIN,
  SEL2_MIN,
  SEL3_MIN,
  SEL4_MIN,
  SEL5_MIN,
  SEL1_MAX,
  SEL2_MAX,
  SEL3_MAX,
  SEL4_MAX,
  SEL5_MAX,
  SEL1_NDO,
  SEL2_NDO,
  SEL3_NDO,
  SEL4_NDO,
  SEL5_NDO,
  SEL1_NSKIP,
  SEL2_NSKIP,
  SEL3_NSKIP,
  SEL4_NSKIP,
  SEL5_NSKIP,
  SEL1_TOT,
  SEL2_TOT,
  SEL3_TOT,
  SEL4_TOT,
  SEL5_TOT
};

class TraceSelectorNDoPop : public SeisInform, public SLFPopSep {

public:
  TraceSelectorNDoPop
    (Widget p,
     char *name,
     HelpCtx hctx,
     class SeisSelect *ss);

  virtual ~TraceSelectorNDoPop ();

  virtual Widget make 
    (Widget p);

  virtual void manage ();

  class NDoTraceSelection *selector () { return _select; }

  class NDoTraceFind *results () { return _results; }

  void seisPlotChanged
    (class SeisPlot *sp);

  void checkMovieState
    (long domovie,
     long frames);

  void processMovieState
    (int movie_state,
     int frame_state);

  void clear
    (class SeisPlot *sp);

  Boolean clearOrSetDefaults
    (class SeisPlot *sp);

protected:
  virtual void preScan
    (SeisPlot *sp,
     SeisPlot::ScanDir dir);

  virtual void postScan
    (SeisPlot *sp,
     SeisPlot::ScanDir dir);

  virtual void okButton ();

  virtual void applyButton ();

  virtual void cancelButton ();

private:
  void apply();

  static void textBoxFocused
    (void *data,
     long which);

  static void textBoxDefocused
    (void *data,
     long which);

  int resultsUpdate
    (Boolean redo = False);

  void menuChanged ();

  void fileUpdate ();

  int menuUpdate
    (char *msg = NULL,
     Boolean redo = False);

  Boolean orderValid
    (int *orders,
     int count,
     char **msg);

  void  showInformation
    (char *msg = NULL);

  virtual long findTraces ();

  long getHeader
    (int index);

  long getOrder
    (int index);

  long getFileMin
    (int index);

  long getFileMax
    (int index);

  long getFileInc
    (int index);

  long getFileTot
    (int index);

  long getSelMin
    (int index);

  long getSelMax
    (int index);

  long getSelNDo
    (int index);

  long getSelNSkip
    (int index);

  long getSelTot
    (int index);

  void setValues ();

  void setFixedValues ();

  void initializeValues ();

  void initializeValue
    (int index);

  void setSensitivities ();

  Boolean isActive
    (int index);

  Boolean isUsed
    (int index);

  Boolean canBeRegularized ();

  void setHeader
    (long index,
     long header);

  void fixupOrder
    (long index,
     int order);
   

  void setOrder
    (long index,
     long order);

  static void togChanged
    (void *data,
     long unused,
     Boolean set);

  static void movieFocused
    (void *data,
     long unused);

  static void movieDefocused
    (void *data,
     long unused);

  class NDoTraceSelection
    *_select,
    *_select_info;

  class NDoTraceFind
    *_results;

  class SeisSelect
    *_ss;

  class SeisPlot
    *_sp;

  class SLTextBox
    *_movie,
    *_header_text;

  class SLForm
    *_header_form,
    *_movie_form;

  class SLTogBox
    *_ifmovie;

  class DoAbort
    *_do_abort;

  Widget
    _information;

  Boolean
    _is_regularized;

  long
    _frames,
    _sav_frames,
    _old_frames,
    _domovie;

  float
    _fmin[5],
    _fmax[5],
    _finc[5],
    _smin[5],
    _smax[5],
    _old_smin[5],
    _old_smax[5];

  int
    _header[5],
    _order[5],
    _ftot[5],
    _sndo[5],
    _snskip[5],
    _stot[5],
    _old_header[5],
    _old_order[5],
    _old_sndo[5],
    _old_snskip[5],
    _old_stot[5],
    _old_nplot;
};

#endif
