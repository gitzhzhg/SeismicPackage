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
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include "sl/sl_form.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_error_pop.hh"
#include "sp/trace_selector_ndo_pop.hh"
#include "sp/trace_selector_pop.hh"
#include "sp/seis_select_pop.hh"
#include "sp/do_abort.hh"
#include "trace_selector.hh"


static String  defres[] = {
  "*ndotslab.labelString:             Select Traces for One Frame",
  "*ndotslab.fontList:                -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
  "*ndodescr.labelString:          File Description",
  "*ndoselect.labelString:         Trace Selection",
  "*headerlab.labelString:         Header",
  "*orderlab.labelString:          Order",
  "*ffirstlab.labelString:         First",
  "*flastlab.labelString:          Last",
  "*finclab.labelString:           Incr",
  "*ftotlab.labelString:           Total",
  "*sbeglab.labelString:           Begin",
  "*sendlab.labelString:           End",
  "*sndolab.labelString:           NDo",
  "*snskiplab.labelString:         NSkip",
  "*scntlab.labelString:           Count",
  "*movie_form_Frame.leftOffset:   -10",
  NULL
};


enum { FRAMES, DOMOVIE };

enum {
  UNCHANGED         = 0, // movie or frames didn't change
  TURNED_ON            , // movie turned on
  TURNED_OFF           , // movie turned off
  CHANGED           = 4, // frames changed non-trivially
  CHANGED_TRIVIALLY = 8  // frames changed trivially
};

TraceSelectorNDoPop::TraceSelectorNDoPop (Widget p, char *name, HelpCtx hctx,
  SeisSelect *ss) :
  SeisInform (),
  SLFPopSep (p, name, FP_DOOK | FP_DOAPPLY | FP_DOCANCEL| FP_DOHELP,
    hctx, False, False),
  _ss            (ss),
  _select        (NULL),
  _select_info   (NULL),
  _results       (NULL),
  _sav_frames    (0)
{

  static SLText htext[] = {
    {"file1header" , NULL, NULL, SLType_int,   FILE1_HEADER },
    {"sel1order"   , NULL, NULL, SLType_int,   SEL1_ORDER   },
    {"file1min"    , NULL, NULL, SLType_float, FILE1_MIN    },
    {"file1max"    , NULL, NULL, SLType_float, FILE1_MAX    },
    {"file1inc"    , NULL, NULL, SLType_float, FILE1_INC    },
    {"file1tot"    , NULL, NULL, SLType_int,   FILE1_TOT    },
    {"sel1min"     , NULL, NULL, SLType_float, SEL1_MIN     },
    {"sel1max"     , NULL, NULL, SLType_float, SEL1_MAX     },
    {"sel1ndo"     , NULL, NULL, SLType_int,   SEL1_NDO     },
    {"sel1nskip"   , NULL, NULL, SLType_int,   SEL1_NSKIP   },
    {"sel1tot"     , NULL, NULL, SLType_int,   SEL1_TOT     },
    {"file2header" , NULL, NULL, SLType_int,   FILE2_HEADER },
    {"sel2order"   , NULL, NULL, SLType_int,   SEL2_ORDER   },
    {"file2min"    , NULL, NULL, SLType_float, FILE2_MIN    },
    {"file2max"    , NULL, NULL, SLType_float, FILE2_MAX    },
    {"file2inc"    , NULL, NULL, SLType_float, FILE2_INC    },
    {"file2tot"    , NULL, NULL, SLType_int,   FILE2_TOT    },
    {"sel2min"     , NULL, NULL, SLType_float, SEL2_MIN     },
    {"sel2max"     , NULL, NULL, SLType_float, SEL2_MAX     },
    {"sel2ndo"     , NULL, NULL, SLType_int,   SEL2_NDO     },
    {"sel2nskip"   , NULL, NULL, SLType_int,   SEL2_NSKIP   },
    {"sel2tot"     , NULL, NULL, SLType_int,   SEL2_TOT     },
    {"file3header" , NULL, NULL, SLType_int,   FILE3_HEADER },
    {"sel3order"   , NULL, NULL, SLType_int,   SEL3_ORDER   },
    {"file3min"    , NULL, NULL, SLType_float, FILE3_MIN    },
    {"file3max"    , NULL, NULL, SLType_float, FILE3_MAX    },
    {"file3inc"    , NULL, NULL, SLType_float, FILE3_INC    },
    {"file3tot"    , NULL, NULL, SLType_int,   FILE3_TOT    },
    {"sel3min"     , NULL, NULL, SLType_float, SEL3_MIN     },
    {"sel3max"     , NULL, NULL, SLType_float, SEL3_MAX     },
    {"sel3ndo"     , NULL, NULL, SLType_int,   SEL3_NDO     },
    {"sel3nskip"   , NULL, NULL, SLType_int,   SEL3_NSKIP   },
    {"sel3tot"     , NULL, NULL, SLType_int,   SEL3_TOT     },
    {"file4header" , NULL, NULL, SLType_int,   FILE4_HEADER },
    {"sel4order"   , NULL, NULL, SLType_int,   SEL4_ORDER   },
    {"file4min"    , NULL, NULL, SLType_float, FILE4_MIN    },
    {"file4max"    , NULL, NULL, SLType_float, FILE4_MAX    },
    {"file4inc"    , NULL, NULL, SLType_float, FILE4_INC    },
    {"file4tot"    , NULL, NULL, SLType_int,   FILE4_TOT    },
    {"sel4min"     , NULL, NULL, SLType_float, SEL4_MIN     },
    {"sel4max"     , NULL, NULL, SLType_float, SEL4_MAX     },
    {"sel4ndo"     , NULL, NULL, SLType_int,   SEL4_NDO     },
    {"sel4nskip"   , NULL, NULL, SLType_int,   SEL4_NSKIP   },
    {"sel4tot"     , NULL, NULL, SLType_int,   SEL4_TOT     },
    {"file5header" , NULL, NULL, SLType_int,   FILE5_HEADER },
    {"sel5order"   , NULL, NULL, SLType_int,   SEL5_ORDER   },
    {"file5min"    , NULL, NULL, SLType_float, FILE5_MIN    },
    {"file5max"    , NULL, NULL, SLType_float, FILE5_MAX    },
    {"file5inc"    , NULL, NULL, SLType_float, FILE5_INC    },
    {"file5tot"    , NULL, NULL, SLType_int,   FILE5_TOT    },
    {"sel5min"     , NULL, NULL, SLType_float, SEL5_MIN     },
    {"sel5max"     , NULL, NULL, SLType_float, SEL5_MAX     },
    {"sel5ndo"     , NULL, NULL, SLType_int,   SEL5_NDO     },
    {"sel5nskip"   , NULL, NULL, SLType_int,   SEL5_NSKIP   },
    {"sel5tot"     , NULL, NULL, SLType_int,   SEL5_TOT     },
  };

  htext[0 ].target = &(_header[0]);
  htext[11].target = &(_header[1]);
  htext[22].target = &(_header[2]);
  htext[33].target = &(_header[3]);
  htext[44].target = &(_header[4]);
  htext[1 ].target = &(_order [0]);
  htext[12].target = &(_order [1]);
  htext[23].target = &(_order [2]);
  htext[34].target = &(_order [3]);
  htext[45].target = &(_order [4]);
  htext[2 ].target = &(_fmin  [0]);
  htext[13].target = &(_fmin  [1]);
  htext[24].target = &(_fmin  [2]);
  htext[35].target = &(_fmin  [3]);
  htext[46].target = &(_fmin  [4]);
  htext[3 ].target = &(_fmax  [0]);
  htext[14].target = &(_fmax  [1]);
  htext[25].target = &(_fmax  [2]);
  htext[36].target = &(_fmax  [3]);
  htext[47].target = &(_fmax  [4]);
  htext[4 ].target = &(_finc  [0]);
  htext[15].target = &(_finc  [1]);
  htext[26].target = &(_finc  [2]);
  htext[37].target = &(_finc  [3]);
  htext[48].target = &(_finc  [4]);
  htext[5 ].target = &(_ftot  [0]);
  htext[16].target = &(_ftot  [1]);
  htext[27].target = &(_ftot  [2]);
  htext[38].target = &(_ftot  [3]);
  htext[49].target = &(_ftot  [4]);
  htext[6 ].target = &(_smin  [0]);
  htext[17].target = &(_smin  [1]);
  htext[28].target = &(_smin  [2]);
  htext[39].target = &(_smin  [3]);
  htext[50].target = &(_smin  [4]);
  htext[7 ].target = &(_smax  [0]);
  htext[18].target = &(_smax  [1]);
  htext[29].target = &(_smax  [2]);
  htext[40].target = &(_smax  [3]);
  htext[51].target = &(_smax  [4]);
  htext[8 ].target = &(_sndo  [0]);
  htext[19].target = &(_sndo  [1]);
  htext[30].target = &(_sndo  [2]);
  htext[41].target = &(_sndo  [3]);
  htext[52].target = &(_sndo  [4]);
  htext[9 ].target = &(_snskip[0]);
  htext[20].target = &(_snskip[1]);
  htext[31].target = &(_snskip[2]);
  htext[42].target = &(_snskip[3]);
  htext[53].target = &(_snskip[4]);
  htext[10].target = &(_stot  [0]);
  htext[21].target = &(_stot  [1]);
  htext[32].target = &(_stot  [2]);
  htext[43].target = &(_stot  [3]);
  htext[54].target = &(_stot  [4]);

  static SLText movie_text[] = {
    { "frames", NULL, NULL, SLType_int, FRAMES },
  };
  movie_text[0].target = &_frames;

  static SLTog mtogs[] = {
    { "domovie", NULL, DOMOVIE },
  };
  mtogs[0].target = &_domovie;

  _header_form = new SLForm (this, "headerform", hctx, True);

  _header_text = new SLTextBox (_header_form, "ndoheadertext", hctx, htext,
    XtNumber(htext), False, 11);

  _header_text->setAltFocusAction  (textBoxFocused,   (void*)this);
  _header_text->setAltLosingAction (textBoxDefocused, (void*)this);

  _select  = new NDoTraceSelection (5);
  _results = new NDoTraceFind ();

  _sp = ss->getSP ();
  long nplt, iskp, ndo, nskp;
  _ss->getTracePattern (&nplt, &iskp, &ndo, &nskp, &_frames, &_domovie);
  addSeisPlot (_sp, this);

  setDefaultResources (p, name, defres);

  _movie_form = new SLForm (this, "movie_form", getHelpCtx(), True);

  _ifmovie = new SLTogBox (_movie_form, "ifmovie", getHelpCtx(), mtogs,
    XtNumber(mtogs));
  _ifmovie->setAltChoiceAction (togChanged, (void*)this);

  _movie = new SLTextBox (_movie_form, "movie", getHelpCtx(), movie_text,
    XtNumber(movie_text), False);
  _movie->setAltFocusAction  (movieFocused,   (void*)this);
  _movie->setAltLosingAction (movieDefocused, (void*)this);

  make (p);

  defaultButton (FP_APPLY, True);
}

TraceSelectorNDoPop::~TraceSelectorNDoPop()
{
  delete _select;
  delete _results;
  if ( _select_info) delete _select_info;
}

Widget TraceSelectorNDoPop::make (Widget p)
{
  if (made()) return topWidget ();
  SLFPopSep::make (p);

  setTitle ("Select Frame Traces");

  Widget ndotslab = XtVaCreateManagedWidget ("ndotslab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_FORM,
                               XmNtopOffset,       15,
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      5,
                               XmNrightAttachment, XmATTACH_FORM,
                               XmNrightOffset,     5,
                               NULL);

  Widget sep = make_attached_sep (topWidget(), "sep");
  XtVaSetValues (sep,
                               XmNtopAttachment, XmATTACH_WIDGET,
                               XmNtopWidget,     ndotslab,
                               XmNtopOffset,     15,
                               NULL);

  Widget ndodescr = XtVaCreateManagedWidget ("ndodescr", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       sep,
                               XmNtopOffset,       15,
                               XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,      _header_form->W(),
                               XmNleftOffset,      200,
                               NULL);

  Widget ndoselect = XtVaCreateManagedWidget ("ndoselect", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       sep,
                               XmNtopOffset,       15,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      ndodescr,
                               XmNleftOffset,      300,
                               NULL);

  Widget headerlab = XtVaCreateManagedWidget ("headerlab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,      _header_form->W(),
                               XmNleftOffset,      5,
                               NULL);

  Widget orderlab = XtVaCreateManagedWidget ("orderlab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      headerlab,
                               XmNleftOffset,      30,
                               NULL);

  Widget ffirstlab = XtVaCreateManagedWidget ("ffirstlab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      orderlab,
                               XmNleftOffset,      35,
                               NULL);

  Widget flastlab= XtVaCreateManagedWidget ("flastlab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      ffirstlab,
                               XmNleftOffset,      50,
                               NULL);

  Widget finclab = XtVaCreateManagedWidget ("finclab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      flastlab,
                               XmNleftOffset,      50,
                               NULL);

  Widget ftotlab = XtVaCreateManagedWidget ("ftotlab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      finclab,
                               XmNleftOffset,      40,
                               NULL);

  Widget sbeglab = XtVaCreateManagedWidget ("sbeglab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      ftotlab,
                               XmNleftOffset,      40,
                               NULL);

  Widget sendlab = XtVaCreateManagedWidget ("sendlab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      sbeglab,
                               XmNleftOffset,      45,
                               NULL);

  Widget sndolab = XtVaCreateManagedWidget ("sndolab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      sendlab,
                               XmNleftOffset,      40,
                               NULL);

  Widget snskiplab = XtVaCreateManagedWidget ("snskiplab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      sndolab,
                               XmNleftOffset,      45,
                               NULL);

  Widget scntlab = XtVaCreateManagedWidget ("scntlab", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       ndodescr,
                               XmNtopOffset,       5,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      snskiplab,
                               XmNleftOffset,      35,
                               NULL);

   XtVaSetValues (_header_form->W(),
		               XmNtopAttachment,   XmATTACH_WIDGET,
                    	       XmNtopWidget,       sep,
                    	       XmNtopOffset,       60,
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      10,
		               XmNrightAttachment, XmATTACH_FORM,
		               XmNrightOffset,     10,
                               NULL);

   XtVaSetValues (_movie_form->W(),
		               XmNtopAttachment,   XmATTACH_WIDGET,
                    	       XmNtopWidget,       _header_form->W(),
                    	       XmNtopOffset,       10,
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      350,
                               NULL);

   XtVaSetValues (_movie->W(),
		               XmNleftAttachment,  XmATTACH_WIDGET,
		               XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
		               XmNtopWidget,       _ifmovie->W(),
		               XmNleftWidget,      _ifmovie->W(),
		               NULL);

   _information = XtVaCreateManagedWidget ("information", xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       _movie_form->W(),
                               XmNtopOffset,       10,
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      330,
                               NULL);

   Widget tmp = XtVaCreateManagedWidget ("",
                               xmLabelWidgetClass,  topWidget(),
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _information,
                               XmNbottomAttachment, XmATTACH_WIDGET,
                               XmNbottomWidget,     bottomSeparator(),
                               XmNbottomOffset,     20,
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNleftOffset,       5,
                               XmNtopOffset,        5,
                               NULL);

  wprocShowMsg (tmp, "");

  initializeValues ();

  showInformation ();

  _do_abort = new DoAbort (topWidget(), "SEARCHING...Click to abort");

  _select->setAbortFunction ((AbortFunction)DoAbort::altUserAbort,
    (void*)_do_abort);

  setSensitivities ();
  return topWidget ();
}

void TraceSelectorNDoPop::manage ()
{
  SLFPopSep::manage ();
}


//=======================================================================
//====This is called by SeisSelect when the multiple file option has ====
//====changed the visible SeisPlot                                   ====
//=======================================================================
void TraceSelectorNDoPop::seisPlotChanged (SeisPlot *sp)
{
  if (_sp != sp) {
//  delSeisPlot (_sp);
    _sp = sp;
    addSeisPlot (_sp, this);

    if (!made()) return;

    long nplt, iskp, ndo, nskp, frames, domovie;
    _ss->getTracePattern (&nplt, &iskp, &ndo, &nskp, &frames, &domovie);
    checkMovieState (domovie, frames);

    NDoTraceSelection *select;
    NDoTraceFind *results;
    // get the _sp's existing NDoTraceSelection object if any
    _sp->getSelectorParameters (&select, &results);
    if (select != NULL) {

      // redefine this _select to contain the same as _sp's
      _select->set (select);
      setValues ();

      // redefine this _results to contain the same as _sp's
      _results->set (results);
    }

/*
    // This causes a disk read that seems to be an unnecessary I/O drag
    //   in the case of the multi-file selector
    if (!_sp->matches(_select)) {
      _ss->turnOffSelector ();
      _results->initialize (); // initialize state
    }
 */

    resultsUpdate (True);
  }
}


void TraceSelectorNDoPop::checkMovieState (long domovie, long frames)
{
  if (!made()) return;

  int movie_state;
  int frame_state;

  if (domovie != _domovie) {
    // movie state changed!
    if (domovie != 0) {
      // movie is being turned on
      movie_state = TURNED_ON;
    }
    else {
      // movie is being turned off
      movie_state = TURNED_OFF;
    }
    _domovie = domovie;
  }
  else {
    // movie state did not change
    movie_state = UNCHANGED;
  }

  if (frames != _frames) {
    if (frames < 2) {
      // # of frames changed to a trivially number
      frame_state = CHANGED_TRIVIALLY;
    }
    else {
      // # of frames changed to a non-trivial number
      frame_state = CHANGED;
    }
    _frames = frames;
  }
  else {
    // # of frames did not change
    frame_state = UNCHANGED;
  }
  processMovieState (movie_state, frame_state);
}

void TraceSelectorNDoPop::processMovieState (int movie_state,
  int frame_state)
{
  // based on what was requested, figure out what to do
  Boolean set_movie, set_frames, clr_frames, update_results;
  // cases are defined by adding the movie and frame states
  switch (movie_state + frame_state) {

  case  0 : // movie unchanged, frames unchanged
    set_movie      = False;
    set_frames     = False;
    clr_frames     = False;
    update_results = False;
    break;

  case  1 : // movie turned on, _frames unchanged
    // don't set or clear frames
    set_frames = False;
    clr_frames = False;
    if (_frames < 2) {
      // don't set movie or update results
      set_movie      = False;
      update_results = False;
    }
    else {
      // set movie & update results
      set_movie      = True;
      update_results = True;
    }
    break;

  case  2 : // movie turned off, frames unchanged
    // turn off movies & clear frames
    set_movie  = True;
    clr_frames = True;
    if (_frames > 1) {
      // save & set frames & update results
      set_frames     = True;
      update_results = True;
      _sav_frames    = _frames;
      _frames        = 0;
    }
    else {
      // don't set frames or update results
      set_frames     = False;
      update_results = False;
    }
    break;

  case  4 : // movie unchanged, frames changed non-trivially
    // don't set movie or clear frames
    set_movie  = False;
    clr_frames = False;
    if (_domovie) {
      // set frames & update results
      set_frames     = True;
      update_results = True;
    }
    else {
      // save & don't set frames & don't update results
      set_frames     = False; // should already be cleared!
      update_results = False;
      _sav_frames    = _frames;
    }
    break;

  case  5 : // movie turned on, frames changed non-trivially
    // set movie & set & don't clear frames & update results
    set_movie      = True;
    set_frames     = True;
    clr_frames     = False;
    update_results = True;
    break;

  case  6 : // movie turned off, frames changed non-trivially
    // set movie & save, set & clear frames & update results
    set_movie      = True;
    set_frames     = True;
    clr_frames     = True;
    update_results = True;
    _sav_frames    = _frames;
    _frames        = 0;
    break;

  case  8 : // movie unchanged, frames changed trivially
    // set frames
    set_frames = True;
    clr_frames = True; // now clear but will need to clear again!
    if (_domovie) {
      // set movie off, clear frames, & update results
      _domovie       = 0;
      set_movie      = True;
      update_results = True;
    }
    else {
      // don't set movie, clear frames, or update results
      set_movie      = False;
      update_results = False;
    }
    break;

  case  9 : // movie turned on, frames changed trivially
    // don't set movie, set & clear frames, & don't update results
    set_movie      = False;
    set_frames     = True;
    clr_frames     = True;
    update_results = False;
    break;

  case 10 : // movie turned off, frames changed trivially
    // turn off movies & update results
    set_movie      = True;
    set_frames     = True;
    clr_frames     = True;
    update_results = True;
    break;

  default:
    assert (0); // shouldn't get here
  }

  // do what is needed
  if (set_movie) {
    _ifmovie->SetTog (DOMOVIE, _domovie != 0);
  }
  if (set_frames) {
    _movie->SetValue (FRAMES, _frames);
  }
  if (clr_frames) {
    _movie->clear (FRAMES);
  }
  if (update_results) {
    resultsUpdate ();
  }
}

void TraceSelectorNDoPop::clear (SeisPlot *sp)
{
  if (sp == _sp) {
    _select->set (NULL);
    if (_domovie != 0) {
      checkMovieState (0, _frames);
    }
    menuChanged ();
  }
}

Boolean TraceSelectorNDoPop::clearOrSetDefaults (SeisPlot *sp)
{
  Boolean retval;

  if (sp == NULL || !sp->matches(_select)) {
    // clear if new file does not appear to be useable with current selection
    clear (sp);
    retval = False;
  }
  else {
    menuChanged ();
    retval = True;
  }
  return retval;
}

void TraceSelectorNDoPop::preScan (SeisPlot *sp, SeisPlot::ScanDir dir)
{
  if (sp == _sp) {
    if (dir == SeisPlot::NotChanged) return;

    // scans are only allowed on consolidated NDoTraceSelection object
    if (_select_info == NULL) {
      _select_info = new NDoTraceSelection (_select);
    }
    else {
      _select_info->set (_select);
    }

    Boolean init, next;
    if (_sp->usingSelector() && _sp->getNDoTraceSelector() != NULL) {
      if (_sp->rToL()) {
	if (dir == SeisPlot::Left) next = True;
	else                       next = False;
      }
      else {
	if (dir == SeisPlot::Left) next = False;
	else                       next = True;
      }

      // handle movie scans properly
      int frames;
      if (_domovie) {
	frames = _frames > 0 ? _frames : 1;
      }
      else {
	frames = 1;
      }

      if (_select_info->scanFrame(next,True,frames) == NDoTraceRow::ERROR) {
	init = True; // scan failed
      }
      else {
	// scan succeeded
	_select->setRows (_select_info);
	if (resultsUpdate(True) == NDoTraceRow::NO_ERROR &&
          _results->getTraces() > 0                        ) {
	  _sp->updateSelector (_select); // store state of rows on SeisPlot
	  init = False; // find and update succeeded
	}
	else {
	  init = True; // find or update failed
	}
      }
      if (init) {
        _results->initialize ();
	_sp->resetSelectorNumTraces ();
      }
    }
  }
}


void TraceSelectorNDoPop::postScan (SeisPlot *sp, SeisPlot::ScanDir dir)
{
  if (sp == _sp) {
    if (dir == SeisPlot::NotChanged) {
      resultsUpdate (True);
    }
  }
}


void TraceSelectorNDoPop::okButton ()
{
  SLFPopSep::okButton ();
  apply ();

  if (_results->getTraces() < 1) { //User may have hit OK instead of Cancel
    _ss->turnOffSelector ();
  }
}


void TraceSelectorNDoPop::applyButton ()
{
  SLFPopSep::applyButton ();
  apply ();
}


void TraceSelectorNDoPop::cancelButton ()
{
  SLFPopSep::cancelButton ();
  _ss->turnOffSelector ();
}


void TraceSelectorNDoPop::apply ()
{

  if (_results->getTraces() > 0) {
    if (_domovie != 0) {
      long total_frames = _results->getFrames ();
      if (_results->getLast() > 0) total_frames += 1;
      _ss->setTracePattern (_results->getFirst(), 0, 1, 0, total_frames, 1);
    }
    else {
      _ss->setTracePattern (_results->getTraces(), 0, 1, 0, 0, 0);
    }
    _ss->useSelector (_select, _results);
  }
}


void TraceSelectorNDoPop::textBoxFocused (void *data, long which)
{
  TraceSelectorNDoPop *obj = (TraceSelectorNDoPop *)data;
  switch (which) {
  case FILE1_HEADER :
    obj->_old_header[0] = obj->_header[0];
    break;
  case FILE2_HEADER :
    obj->_old_header[1] = obj->_header[1];
    break;
  case FILE3_HEADER :
    obj->_old_header[2] = obj->_header[2];
    break;
  case FILE4_HEADER :
    obj->_old_header[3] = obj->_header[3];
    break;
  case FILE5_HEADER :
    obj->_old_header[4] = obj->_header[4];
    break;
  case SEL1_ORDER :
    obj->_old_order[0] = obj->_order[0];
    break;
  case SEL2_ORDER :
    obj->_old_order[1] = obj->_order[1];
    break;
  case SEL3_ORDER :
    obj->_old_order[2] = obj->_order[2];
    break;
  case SEL4_ORDER :
    obj->_old_order[3] = obj->_order[3];
    break;
  case SEL5_ORDER :
    obj->_old_order[4] = obj->_order[4];
    break;
  case SEL1_MIN    :
    obj->_old_smin[0] = obj->_smin[0];
    break;
  case SEL2_MIN    :
    obj->_old_smin[1] = obj->_smin[1];
    break;
  case SEL3_MIN    :
    obj->_old_smin[2] = obj->_smin[2];
    break;
  case SEL4_MIN    :
    obj->_old_smin[3] = obj->_smin[3];
    break;
  case SEL5_MIN    :
    obj->_old_smin[4] = obj->_smin[4];
    break;
  case SEL1_MAX    :
    obj->_old_smax[0] = obj->_smax[0];
    break;
  case SEL2_MAX    :
    obj->_old_smax[1] = obj->_smax[1];
    break;
  case SEL3_MAX    :
    obj->_old_smax[2] = obj->_smax[2];
    break;
  case SEL4_MAX    :
    obj->_old_smax[3] = obj->_smax[3];
    break;
  case SEL5_MAX    :
    obj->_old_smax[4] = obj->_smax[4];
    break;
  case SEL1_NDO    :
    obj->_old_sndo[0] = obj->_sndo[0];
    break;
  case SEL2_NDO    :
    obj->_old_sndo[1] = obj->_sndo[1];
    break;
  case SEL3_NDO    :
    obj->_old_sndo[2] = obj->_sndo[2];
    break;
  case SEL4_NDO    :
    obj->_old_sndo[3] = obj->_sndo[3];
    break;
  case SEL5_NDO    :
    obj->_old_sndo[4] = obj->_sndo[4];
    break;
  case SEL1_NSKIP  :
    obj->_old_snskip[0] = obj->_snskip[0];
    break;
  case SEL2_NSKIP  :
    obj->_old_snskip[1] = obj->_snskip[1];
    break;
  case SEL3_NSKIP  :
    obj->_old_snskip[2] = obj->_snskip[2];
    break;
  case SEL4_NSKIP  :
    obj->_old_snskip[3] = obj->_snskip[3];
    break;
  case SEL5_NSKIP  :
    obj->_old_snskip[4] = obj->_snskip[4];
    break;
  case SEL1_TOT  :
    obj->_old_stot[0] = obj->_stot[0];
    break;
  case SEL2_TOT  :
    obj->_old_stot[1] = obj->_stot[1];
    break;
  case SEL3_TOT  :
    obj->_old_stot[2] = obj->_stot[2];
    break;
  case SEL4_TOT  :
    obj->_old_stot[3] = obj->_stot[3];
    break;
  case SEL5_TOT  :
    obj->_old_stot[4] = obj->_stot[4];
    break;
  }
}


void TraceSelectorNDoPop::textBoxDefocused (void *data, long which)
{
  Boolean changed = False;

  TraceSelectorNDoPop *obj = (TraceSelectorNDoPop *)data;
  long nplot;
  double last;
  switch (which) {
  case FILE1_HEADER :
    if (    obj->_header[0] !=
        obj->_old_header[0]   ) {
      obj->setHeader (0L, obj->_header[0]);
      changed = True;
    }
    break;
  case FILE2_HEADER :
    if (    obj->_header[1] !=
        obj->_old_header[1]   ) {
      obj->setHeader (1L, obj->_header[1]);
      changed = True;
    }
    break;
  case FILE3_HEADER :
    if (    obj->_header[2] !=
        obj->_old_header[2]   ) {
      obj->setHeader (2L, obj->_header[2]);
      changed = True;
    }
    break;
  case FILE4_HEADER :
    if (    obj->_header[3] !=
        obj->_old_header[3]   ) {
      obj->setHeader (3L, obj->_header[3]);
      changed = True;
    }
    break;
  case  FILE5_HEADER :
    if (    obj->_header[4] !=
        obj->_old_header[4]   ) {
      obj->setHeader (4L, obj->_header[4]);
      changed = True;
    }
    break;
  case SEL1_ORDER :
    if (    obj->_order[0] !=
        obj->_old_order[0]   ) {
      obj->setOrder (0L, obj->_order[0]);
      changed = obj->_old_order[0] != obj->_select->getRow(0)->getOrder();
    }
    break;
  case SEL2_ORDER :
    if (    obj->_order[1] !=
        obj->_old_order[1]   ) {
      obj->setOrder (1L, obj->_order[1]);
      changed = obj->_old_order[1] != obj->_select->getRow(1)->getOrder();
    }
    break;
  case SEL3_ORDER :
    if (    obj->_order[2] !=
        obj->_old_order[2]   ) {
      obj->setOrder (2L, obj->_order[2]);
      changed = obj->_old_order[2] != obj->_select->getRow(2)->getOrder();
    }
    break;
  case SEL4_ORDER :
    if (    obj->_order[3] !=
        obj->_old_order[3]   ) {
      obj->setOrder (3L, obj->_order[3]);
      changed = obj->_old_order[3] != obj->_select->getRow(3)->getOrder();
    }
    break;
  case SEL5_ORDER :
    if (    obj->_order[4] !=
        obj->_old_order[4]   ) {
      obj->setOrder (4L, obj->_order[4]);
      changed = obj->_old_order[4] != obj->_select->getRow(4)->getOrder();
    }
    break;
  case SEL1_MIN    :
    if (    obj->_smin[0] !=
        obj->_old_smin[0]   ) {
      obj->_select->getRow(0)->setFirst (obj->_smin[0]);
      changed = obj->isUsed (0);
    }
    break;
  case SEL2_MIN    :
    if (    obj->_smin[1] !=
        obj->_old_smin[1]   ) {
      obj->_select->getRow(1)->setFirst (obj->_smin[1]);
      changed = obj->isUsed (1);
    }
    break;
  case SEL3_MIN    :
    if (    obj->_smin[2] !=
        obj->_old_smin[2]   ) {
      obj->_select->getRow(2)->setFirst (obj->_smin[2]);
      changed = obj->isUsed (2);
    }
    break;
  case SEL4_MIN    :
    if (    obj->_smin[3] !=
        obj->_old_smin[3]   ) {
      obj->_select->getRow(3)->setFirst (obj->_smin[3]);
      changed = obj->isUsed (3);
    }
    break;
  case SEL5_MIN    :
    if (    obj->_smin[4] !=
        obj->_old_smin[4]   ) {
      obj->_select->getRow(4)->setFirst (obj->_smin[4]);
      changed = obj->isUsed (4);
    }
    break;
  case SEL1_MAX    :
    if (    obj->_smax[0] !=
        obj->_old_smax[0]   ) {
      obj->_select->getRow(0)->setLast (obj->_smax[0]);
      changed = obj->isUsed (0);
    }
    break;
  case SEL2_MAX    :
    if (    obj->_smax[1] !=
        obj->_old_smax[1]   ) {
      obj->_select->getRow(1)->setLast (obj->_smax[1]);
      changed = obj->isUsed (1);
    }
    break;
  case SEL3_MAX    :
    if (    obj->_smax[2] !=
        obj->_old_smax[2]   ) {
      obj->_select->getRow(2)->setLast (obj->_smax[2]);
      changed = obj->isUsed (2);
    }
    break;
  case SEL4_MAX    :
    if (    obj->_smax[3] !=
        obj->_old_smax[3]   ) {
      obj->_select->getRow(3)->setLast (obj->_smax[3]);
      changed = obj->isUsed (3);
    }
    break;
  case SEL5_MAX    :
    if (    obj->_smax[4] !=
        obj->_old_smax[4]   ) {
      obj->_select->getRow(4)->setLast (obj->_smax[4]);
      changed = obj->isUsed (4);
    }
    break;
  case SEL1_NDO    :
    if (    obj->_sndo[0] !=
        obj->_old_sndo[0]   ) {
      obj->_select->getRow(0)->setNDo ((long)obj->_sndo[0]);
      changed = obj->isUsed (0);
    }
    break;
  case SEL2_NDO    :
    if (    obj->_sndo[1] !=
        obj->_old_sndo[1]   ) {
      obj->_select->getRow(1)->setNDo ((long)obj->_sndo[1]);
      changed = obj->isUsed (1);
    }
    break;
  case SEL3_NDO    :
    if (    obj->_sndo[2] !=
        obj->_old_sndo[2]   ) {
      obj->_select->getRow(2)->setNDo ((long)obj->_sndo[2]);
      changed = obj->isUsed (2);
    }
    break;
  case SEL4_NDO    :
    if (    obj->_sndo[3] !=
        obj->_old_sndo[3]   ) {
      obj->_select->getRow(3)->setNDo ((long)obj->_sndo[3]);
      changed = obj->isUsed (3);
    }
    break;
  case SEL5_NDO    :
    if (    obj->_sndo[4] !=
        obj->_old_sndo[4]   ) {
      obj->_select->getRow(4)->setNDo ((long)obj->_sndo[4]);
      changed = obj->isUsed (4);
    }
    break;
  case SEL1_NSKIP  :
    if (    obj->_snskip[0] !=
        obj->_old_snskip[0]   ) {
      obj->_select->getRow(0)->setNSkip ((long)obj->_snskip[0]);
      changed = obj->isUsed (0);
    }
    break;
  case SEL2_NSKIP  :
    if (    obj->_snskip[1] !=
        obj->_old_snskip[1]   ) {
      obj->_select->getRow(1)->setNSkip ((long)obj->_snskip[1]);
      changed = obj->isUsed (1);
    }
    break;
  case SEL3_NSKIP  :
    if (    obj->_snskip[2] !=
        obj->_old_snskip[2]   ) {
      obj->_select->getRow(2)->setNSkip ((long)obj->_snskip[2]);
      changed = obj->isUsed (2);
    }
    break;
  case SEL4_NSKIP  :
    if (    obj->_snskip[3] !=
        obj->_old_snskip[3]   ) {
      obj->_select->getRow(3)->setNSkip ((long)obj->_snskip[3]);
      changed = obj->isUsed (3);
    }
    break;
  case SEL5_NSKIP  :
    if (    obj->_snskip[4] !=
        obj->_old_snskip[4]   ) {
      obj->_select->getRow(4)->setNSkip ((long)obj->_snskip[4]);
      changed = obj->isUsed (4);
    }
    break;
  case SEL1_TOT  :
    if (    obj->_stot[0] !=
        obj->_old_stot[0]   ) {
      nplot = (long)obj->_stot[0];
      obj->_select->getRow(0)->adjustUsingNPlot (&nplot);
      changed = True;
    }
    break;
  case SEL2_TOT  :
    if (    obj->_stot[1] !=
        obj->_old_stot[1]   ) {
      nplot = (long)obj->_stot[1];
      obj->_select->getRow(1)->adjustUsingNPlot (&nplot);
      changed = True;
    }
    break;
  case SEL3_TOT  :
    if (    obj->_stot[2] !=
        obj->_old_stot[2]   ) {
      nplot = (long)obj->_stot[2];
      obj->_select->getRow(2)->adjustUsingNPlot (&nplot);
      changed = True;
    }
    break;
  case SEL4_TOT  :
    if (    obj->_stot[3] !=
        obj->_old_stot[3]   ) {
      nplot = (long)obj->_stot[3];
      obj->_select->getRow(3)->adjustUsingNPlot (&nplot);
      changed = True;
    }
    break;
  case SEL5_TOT  :
    if (    obj->_stot[4] !=
        obj->_old_stot[4]   ) {
      nplot = (long)obj->_stot[4];
      obj->_select->getRow(4)->adjustUsingNPlot (&nplot);
      changed = True;
    }
    break;
  default:
    obj->setFixedValues ();
    changed = False;
    break;
  }
  if (changed) {
    obj->menuChanged ();
  }
  else {
    obj->setSensitivities ();
  }
}

void TraceSelectorNDoPop::menuChanged ()
{
  // to get here:
  //   a positive numbered header must be picked,
  //   it must have a positive numbered order, and
  //   it must have a positive numbered ndo
  _select->clearList ();
  _results->initialize ();
  fileUpdate ();
  resultsUpdate (True);
}

static char MESSAGE[256];
int TraceSelectorNDoPop::resultsUpdate (Boolean redo)
{
  int retval;

  MESSAGE[0] = '\0';

  if (_is_regularized) {
    if (findTraces() < 1) _results->initialize ();
    retval = menuUpdate (MESSAGE, redo);
    if (retval == NDoTraceRow::ERROR && strlen(MESSAGE) > 0) {
      wprocVAShowMsg (_information, MESSAGE);
    }
    else {
      showInformation (MESSAGE);
    }
  }
  else {
    retval = NDoTraceRow::ERROR;
    showInformation ();
  }

  _do_abort->actionComplete ();

  setSensitivities ();
  return retval;
}

void TraceSelectorNDoPop::fileUpdate ()
{

  if (canBeRegularized()) {
    _do_abort->setNewAction ();
    _is_regularized = _sp->isRegularized (_select);
    _do_abort->actionComplete ();
  }
  else {
    _is_regularized = False;
  }

  // update file info if active
  int k2;
  for (k2 = 0; k2 < 5; k2++) {
    if (isActive(k2)) {
      _header_text->SetValue (getHeader(k2),
        (long)_select->getRow(k2)->getScope()->getHeader());
      _header_text->SetValue (getFileMin(k2),
	(float)_select->getRow(k2)->getScope()->getMinimum());
      _header_text->SetValue (getFileMax(k2),
        (float)_select->getRow(k2)->getScope()->getMaximum());
      _header_text->SetValue (getFileInc(k2),
        (float)_select->getRow(k2)->getScope()->getIncrement());
      _header_text->SetValue (getFileTot(k2),
        _select->getRow(k2)->getScope()->getLength ());
    }
    else {
      // header is 0
      assert (_select->getRow(k2)->getScope()->getHeader() < 1);
      _header_text->SetValue (getHeader(k2), 0L);
      initializeValue (k2);
    }
  }
}

static char BUF[36];

int TraceSelectorNDoPop::menuUpdate (char *msg, Boolean redo)
{
  int retval;

  if (_is_regularized) {
    if (_select->getCount() < 1) {
      retval = NDoTraceRow::ERROR;
      if (msg != NULL) strcpy (msg, "NO headers have been selected");
    }
    else {
      Boolean had_to_fix, had_an_error;
      NDoTraceRow *row;

      // initialize returns
      retval = NDoTraceRow::ERROR;
      if (msg != NULL) strcpy (msg, "NO selected headers are used");

      int k2, act_cnt, err_cnt, orders[5];
      for (k2 = 0, had_to_fix = False, had_an_error = False,
        act_cnt = 0, err_cnt = 0; k2 < 5; k2++) {
	if (isActive(k2)) {
	  orders[act_cnt] = _select->getRow(k2)->getOrder ();
	  act_cnt++;
	  retval = _select->getRow(k2)->valid ();
	  if (retval == NDoTraceRow::FIXED_ERROR) {
	    had_to_fix   = True;
	  }
	  if (retval == NDoTraceRow::ERROR) {
            had_an_error = True;
	    err_cnt++;
	  }
	  if (had_to_fix || redo) {
	    _header_text->SetValue (getOrder(k2),
	      (long)_select->getRow(k2)->getOrder ());
	    _header_text->SetValue (getSelMin(k2),
              (float)_select->getRow(k2)->getFirst());
	    _header_text->SetValue (getSelMax(k2),
              (float)_select->getRow(k2)->getLast ());
	    _header_text->SetValue (getSelNDo(k2),
              _select->getRow(k2)->getNDo());
	    _header_text->SetValue (getSelNSkip(k2),
              _select->getRow(k2)->getNSkip ());
	    _header_text->SetValue (getSelTot(k2),
              _select->getRow(k2)->getNPlot ());
	  }
	}
      }
      if (had_an_error) {
	if (err_cnt == act_cnt) {
	  if (msg != NULL) strcpy (msg, "NO HEADERS have been VALIDATED");
	  retval = NDoTraceRow::ERROR;
	}
	else {
	  char *loc_msg = NULL;
	  if (msg != NULL) {
	    loc_msg = msg;
	  }
	  if (!orderValid(orders,act_cnt,&loc_msg)) {
	    retval = NDoTraceRow::ERROR;
	  }
	  else {
	    retval = NDoTraceRow::NO_ERROR;
	  }
	  if (msg == NULL) free (loc_msg);
	}
      }
      else if (had_to_fix) {
	if (msg != NULL) strcpy (msg,
          "At least one selection had to be FIXED");
	retval = NDoTraceRow::FIXED_ERROR;
      }
      else if (msg != NULL) {
	strcpy (msg, "");
	retval = NDoTraceRow::NO_ERROR;
      }
    }
  }
  else {
    if (msg != NULL) {
      strcpy (msg, "Selected headers do NOT indicate REGULARIZED data");
    }
    retval = NDoTraceRow::ERROR;
  }
  return retval;
}

Boolean TraceSelectorNDoPop::orderValid (int *orders, int count, char **msg)
{
  assert (orders != NULL && msg != NULL);

  Boolean retval;

  // modify count to reflect only valid orders
  int k2, cnt;
  for (k2 = 0, cnt = 0; k2 < count; k2++) {
    if (orders[k2] > 0) cnt++;
  }

  int *loc_orders = NULL;

  if (*msg == NULL) *msg = (char *)malloc (32*sizeof(char));
  if (cnt == count) {
    strcpy (*msg, "");
    loc_orders = (int *)malloc (count*sizeof(int));
    for (k2 = 0; k2 < count; k2++) {
      loc_orders[k2] = orders[k2];
    }
    retval = True;
  }
  else if (cnt > 0) {
    strcpy (*msg, "At least one ORDER is not set");
    loc_orders = (int *)malloc (cnt*sizeof(int));
    for (k2 = 0, cnt = 0; k2 < count; k2++) {
      if (orders[k2] > 0) loc_orders[cnt++] = orders[k2];
    }
    count = cnt;
    retval = True;
  }
  else {
    strcpy (*msg, "NO ORDER has been set");
    retval = False;
  }

  // make sure no order is larger than the count of used orders
  for (k2 = 0; retval && k2 < count; k2++) {
    if (loc_orders[k2] > count) {
      strcpy (*msg, "At least one ORDER is too large");
      retval = False;
    }
  }

  // make sure order is unique and order is contiguous from 1 to count
  int k3;
  Boolean contiguous;
  for (k2 = 0; retval && k2 < count; k2++) {
    for (k3 = 0, contiguous = False; retval && k3 < count; k3++) {
      if (k2         != k3                 &&
          loc_orders[k2] == loc_orders[k3]   ) {
	strcpy (*msg, "The ORDERS are NOT UNIQUE");
	retval = False;
      }
      if (loc_orders[k3]-1 == k2) {
	contiguous = True;
      }
    }
    if (retval && !contiguous) {
      strcpy (*msg, "The ORDERS are NOT CONTIGUOUS");
      retval = False;
    }
  }
  if (loc_orders) {
    free (loc_orders);
  }
  return retval;
}

static char SHOW[128];
void TraceSelectorNDoPop::showInformation (char *msg)
{
  if (_select_info != NULL) {
    if (_select_info->getCount() > 0 && !_is_regularized) {
      wprocVAShowMsg (_information,
      "Headers do NOT indicate regularized data");
    }
    else if (_results->getTraces() > 0) {
      if (_domovie != 0) {
	if (_results->getLast() < 1) {
	  if (msg && strlen(msg) > 0) {
	    sprintf (SHOW, ": Found: %d frames of %d traces",
	      _results->getFrames(), _results->getFirst());
	    strcat (msg, SHOW);
	    wprocVAShowMsg (_information, msg);
	  }
	  else {
	    wprocVAShowMsg (_information,  "Found: %d frames of %d traces",
	      _results->getFrames(), _results->getFirst());
	  }
	}
	else {
	  if (msg && strlen(msg) > 0) {
	    sprintf (SHOW,
              ": Found: %d frames of %d traces, 1 Frame of %d traces",
              _results->getFrames(), _results->getFirst(),
              _results->getLast());
	    strcat (msg, SHOW);
	    wprocVAShowMsg (_information, msg);
	  }
	  else {
	    wprocVAShowMsg (_information,
              "Found: %d frames of %d traces, 1 Frame of %d traces",
              _results->getFrames(), _results->getFirst(),
              _results->getLast());
	  }
	}
      }
      else {
	if (msg && strlen(msg) > 0) {
	  sprintf (SHOW,
            ": Found: %d traces",
            _results->getTraces());
	  strcat (msg, SHOW);
	  wprocVAShowMsg (_information, msg);
	}
	else {
	  wprocVAShowMsg (_information, "Found: %d traces",
            _results->getTraces());
	}
      }
    }
    else if (_select_info->getCount() < 1) {
      wprocShowMsg (_information, "NO HEADERS SPECIFIED OR SELECTED YET");
    }
    else {
      wprocShowMsg (_information, "FOUND NO TRACES: CHECK PARAMETERS");
    }
  }
  else {
    wprocShowMsg (_information, "FOUND NO TRACES");
  }
}


long TraceSelectorNDoPop::findTraces ()
{
  _do_abort->setNewAction ();

  if (_select_info == NULL) {
    _select_info  = new NDoTraceSelection (_select);
  }
  else {
    _select_info->set (_select);
  }

  int frames, last_frame_size;
  long nplt, fnplt;
  if (_domovie != 0) {
    if (_frames < 1) {
      frames = 1;
    }
    else {
      frames = _frames;
    }
    nplt = _select_info->getNPlot ();
    fnplt = nplt;
    frames = _select_info->verifyNFrames (frames, &last_frame_size);
    if (frames < 1) frames = 1;
  }
  else {
    nplt = _sp->totalTraces ();
    fnplt = 0;
    last_frame_size = 0;
    frames = 1;
  }

  _results->setTraces (_sp->findTraces(_sp->filename(),_select_info,nplt,0,1,
    0,frames,0));

  _do_abort->actionComplete ();

  if (_results->getTraces() > 0) {
    _results->setFrames (frames);
    _results->setLast (last_frame_size);
    _results->setFirst (fnplt);
  }
  return _results->getTraces();
}

long TraceSelectorNDoPop::getHeader (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)FILE1_HEADER;
    break;
  case 1 :
    retval = (long)FILE2_HEADER;
    break;
  case 2 :
    retval = (long)FILE3_HEADER;
    break;
  case 3 :
    retval = (long)FILE4_HEADER;
    break;
  case 4 :
    retval = (long)FILE5_HEADER;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getOrder (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)SEL1_ORDER;
    break;
  case 1 :
    retval = (long)SEL2_ORDER;
    break;
  case 2 :
    retval = (long)SEL3_ORDER;
    break;
  case 3 :
    retval = (long)SEL4_ORDER;
    break;
  case 4 :
    retval = (long)SEL5_ORDER;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getFileMin (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)FILE1_MIN;
    break;
  case 1 :
    retval = (long)FILE2_MIN;
    break;
  case 2 :
    retval = (long)FILE3_MIN;
    break;
  case 3 :
    retval = (long)FILE4_MIN;
    break;
  case 4 :
    retval = (long)FILE5_MIN;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getFileMax (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)FILE1_MAX;
    break;
  case 1 :
    retval = (long)FILE2_MAX;
    break;
  case 2 :
    retval = (long)FILE3_MAX;
    break;
  case 3 :
    retval = (long)FILE4_MAX;
    break;
  case 4 :
    retval = (long)FILE5_MAX;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getFileInc (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)FILE1_INC;
    break;
  case 1 :
    retval = (long)FILE2_INC;
    break;
  case 2 :
    retval = (long)FILE3_INC;
    break;
  case 3 :
    retval = (long)FILE4_INC;
    break;
  case 4 :
    retval = (long)FILE5_INC;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getFileTot (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)FILE1_TOT;
    break;
  case 1 :
    retval = (long)FILE2_TOT;
    break;
  case 2 :
    retval = (long)FILE3_TOT;
    break;
  case 3 :
    retval = (long)FILE4_TOT;
    break;
  case 4 :
    retval = (long)FILE5_TOT;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getSelMin (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)SEL1_MIN;
    break;
  case 1 :
    retval = (long)SEL2_MIN;
    break;
  case 2 :
    retval = (long)SEL3_MIN;
    break;
  case 3 :
    retval = (long)SEL4_MIN;
    break;
  case 4 :
    retval = (long)SEL5_MIN;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getSelMax (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)SEL1_MAX;
    break;
  case 1 :
    retval = (long)SEL2_MAX;
    break;
  case 2 :
    retval = (long)SEL3_MAX;
    break;
  case 3 :
    retval = (long)SEL4_MAX;
    break;
  case 4 :
    retval = (long)SEL5_MAX;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getSelNDo (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)SEL1_NDO;
    break;
  case 1 :
    retval = (long)SEL2_NDO;
    break;
  case 2 :
    retval = (long)SEL3_NDO;
    break;
  case 3 :
    retval = (long)SEL4_NDO;
    break;
  case 4 :
    retval = (long)SEL5_NDO;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getSelNSkip (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)SEL1_NSKIP;
    break;
  case 1 :
    retval = (long)SEL2_NSKIP;
    break;
  case 2 :
    retval = (long)SEL3_NSKIP;
    break;
  case 3 :
    retval = (long)SEL4_NSKIP;
    break;
  case 4 :
    retval = (long)SEL5_NSKIP;
    break;
  default:
    assert (0);
  }
  return retval;
}

long TraceSelectorNDoPop::getSelTot (int index)
{
  long retval;

  switch (index) {
  case 0 :
    retval = (long)SEL1_TOT;
    break;
  case 1 :
    retval = (long)SEL2_TOT;
    break;
  case 2 :
    retval = (long)SEL3_TOT;
    break;
  case 3 :
    retval = (long)SEL4_TOT;
    break;
  case 4 :
    retval = (long)SEL5_TOT;
    break;
  default:
    assert (0);
  }
  return retval;
}

void TraceSelectorNDoPop::setValues ()
{
  setFixedValues ();

  int k2;
  for (k2 = 0; k2 < 5; k2++) {
    _header_text->SetValue (getHeader(k2),
      (long)_select->getRow(k2)->getScope()->getHeader());
    _header_text->SetValue (getOrder(k2),
      (long)_select->getRow(k2)->getOrder()+1);
    _header_text->SetValue (getSelMin(k2),
      (float)_select->getRow(k2)->getFirst());
    _header_text->SetValue (getSelMax(k2),
      (float)_select->getRow(k2)->getLast());
    _header_text->SetValue (getSelNDo(k2),
      (long)_select->getRow(k2)->getNDo());
    _header_text->SetValue (getSelNSkip(k2),
      (long)_select->getRow(k2)->getNSkip());
    _header_text->SetValue (getSelTot(k2),
      (long)_select->getRow(k2)->getNPlot());
  }
}

void TraceSelectorNDoPop::setFixedValues ()
{
  int k2;
  for (k2 = 0; k2 < 5; k2++) {
    if (isActive(k2)) {
      _header_text->SetValue (getFileMin(k2),
        (float)_select->getRow(k2)->getScope()->getMinimum());
      _header_text->SetValue (getFileMax(k2),
        (float)_select->getRow(k2)->getScope()->getMaximum());
      _header_text->SetValue (getFileInc(k2),
        (float)_select->getRow(k2)->getScope()->getIncrement());
      _header_text->SetValue (getFileTot(k2),
        (long)_select->getRow(k2)->getScope()->getLength());
    }
    else {
      initializeValue (k2);
    }
  }
}


void TraceSelectorNDoPop::initializeValues ()
{
  int k2;
  for (k2 = 0; k2 < 5; k2++) {
    _header_text->SetValue (getHeader(k2), 0L);
    initializeValue (k2);
  }
}

void TraceSelectorNDoPop::initializeValue (int index)
{
  _header_text->SetValue (getOrder   (index), 0L      );
  _header_text->SetValue (getFileMin (index), (float)0);
  _header_text->SetValue (getFileMax (index), (float)0);
  _header_text->SetValue (getFileInc (index), (float)0);
  _header_text->SetValue (getFileTot (index), 0L      );
  _header_text->SetValue (getSelMin  (index), (float)0);
  _header_text->SetValue (getSelMax  (index), (float)0);
  _header_text->SetValue (getSelNDo  (index), 0L      );
  _header_text->SetValue (getSelNSkip(index), 0L      );
  _header_text->SetValue (getSelTot  (index), 0L      );
}

void TraceSelectorNDoPop::setSensitivities ()
{
  int k2;
  for (k2 = 0; k2 < 5; k2++) {
    _header_text->SetSensitive (getHeader  (k2), True        );
    _header_text->SetSensitive (getOrder   (k2), isActive(k2));
    _header_text->SetSensitive (getFileMin (k2), isActive(k2));
    _header_text->SetSensitive (getFileMax (k2), isActive(k2));
    _header_text->SetSensitive (getFileInc (k2), isActive(k2));
    _header_text->SetSensitive (getFileTot (k2), isActive(k2));
    _header_text->SetSensitive (getSelMin  (k2), isActive(k2));
    _header_text->SetSensitive (getSelMax  (k2), isActive(k2));
    _header_text->SetSensitive (getSelNDo  (k2), isActive(k2));
    _header_text->SetSensitive (getSelNSkip(k2), isActive(k2));
    _header_text->SetSensitive (getSelTot  (k2), isActive(k2));
  }
  if (_results->getTraces() > 0) {
    XtSetSensitive       (_ifmovie->W(), True);
    _movie->SetSensitive (FRAMES       , True);
  }
  else {
    XtSetSensitive       (_ifmovie->W(), False);
    _movie->SetSensitive (FRAMES       , False);
  }

}

Boolean TraceSelectorNDoPop::isActive (int index)
{
  Boolean retval;

  if (index > -1 && index < 5) {
    NDoTraceRow *row = _select->getRow (index);
    if (row != NULL) {
      retval = row->getScope()->getHeader() > 0;
    }
    else {
      retval = False;
    }
  }
  else {
    retval = False;
  }
  return retval;
}

Boolean TraceSelectorNDoPop::isUsed (int index)
{
  Boolean retval;

  if (index > -1 && index < 5) {
    if (isActive(index)) {
      retval = _select->getRow(index)->valid () != NDoTraceRow::ERROR &&
               _select->getRow(index)->getNDo() >  0                    ;
    }
    else {
      retval = False;
    }
  }
  else {
    retval = False;
  }
  return retval;
}

Boolean TraceSelectorNDoPop::canBeRegularized ()
{
  Boolean retval;

  int k2;
  for (k2 = 0, retval = False; !retval && k2 < 5; k2++) {
    if (isActive(k2)) retval = True;
  }
  return retval;
}

// ensure uniqueness of header and preserve information
void TraceSelectorNDoPop::setHeader (long index, long header)
{
  // insist on index being within 5
  assert (index > -1 && index < 5);

  int prev_header = _select->getRow(index)->getScope()->getHeader ();

  // insist on a non-trivial change
  if (header == (long)prev_header) return;

  TraceHeaderScope *
  prev_scope = new TraceHeaderScope (_select->getRow(index)->getScope());

  int   prev_order = _select->getRow(index)->getOrder ();
  float prev_first = _select->getRow(index)->getFirst ();
  float prev_last  = _select->getRow(index)->getLast  ();
  long  prev_ndo   = _select->getRow(index)->getNDo   ();
  long  prev_nskip = _select->getRow(index)->getNSkip ();

  TraceHeaderScope *dupl_scope;
  float dupl_first, dupl_last;
  int  dupl_order;
  long dupl_ndo, dupl_nskip;
  int k2, dupl;

  for (k2 = 0, dupl = -1; header != 0 && k2 < 5; k2++) {
    if (index != (long)k2) {
      if (header == (long)_select->getRow(k2)->getScope()->getHeader()) {
        dupl = k2;
	dupl_scope = new TraceHeaderScope (_select->getRow(k2)->getScope());
	dupl_order = _select->getRow(k2)->getOrder ();
	dupl_first = _select->getRow(k2)->getFirst ();
	dupl_last  = _select->getRow(k2)->getLast  ();
	dupl_ndo   = _select->getRow(k2)->getNDo   ();
	dupl_nskip = _select->getRow(k2)->getNSkip ();
      }
    }
  }
  if (dupl > -1) {
    // swap header values
    _select->getRow(dupl )->getScope()->set (prev_scope, True);
    _select->getRow(dupl )->setOrder        (prev_order      );
    _select->getRow(dupl )->setFirst        (prev_first      );
    _select->getRow(dupl )->setLast         (prev_last       );
    _select->getRow(dupl )->setNDo          (prev_ndo        );
    _select->getRow(dupl )->setNSkip        (prev_nskip      );
    _select->getRow(index)->getScope()->set (dupl_scope, True);
    _select->getRow(index)->setOrder        (dupl_order      );
    _select->getRow(index)->setFirst        (dupl_first      );
    _select->getRow(index)->setLast         (dupl_last       );
    _select->getRow(index)->setNDo          (dupl_ndo        );
    _select->getRow(index)->setNSkip        (dupl_nskip      );
    delete dupl_scope;
  }
  else {
    _select->getRow(index)->getScope()->setHeader (header);
  }
  delete prev_scope;
  if (header < 1 && prev_order > 0) fixupOrder (index, prev_order);
}

// only used when a header was just set to zero.
// verify that the header at the given index is now zero
// any other nonzero header that has its order higher than the given
// one will need to be reduced by one
void TraceSelectorNDoPop::fixupOrder (long index, int order)
{
  int k2;
  for (k2 = 0; k2 < 5; k2++) {
    if (k2 == index) {
      assert (_select->getRow(k2)->getScope()->getHeader() == 0);
    }
    else if (_select->getRow(k2)->getOrder() > order) {
      assert (_select->getRow(k2)->getScope()->getHeader() > 0);
      _select->getRow(k2)->setOrder (order-1);
    }
  }
}

// ensure uniqueness of order and validity
void TraceSelectorNDoPop::setOrder (long index, long order)
{
  // insist on index being within 5
  assert (index > -1 && index < 5);

  // insist on order being within count
  if (order < 1 || order > _select->getCount()) {
    // since order is invalid mark it as zero!
    _select->getRow(index)->setOrder (0);
    return;
  }

  int prev_order = _select->getRow(index)->getOrder ();

  // insist on a change
  if (order == (long)prev_order) return;

  int k2, dupl;
  for (k2 = 0, dupl = -1; k2 < 5; k2++) {
    if (index != (long)k2) {
      if (order == (long)_select->getRow(k2)->getOrder()) {
        dupl = k2;
      }
    }
  }
  if (dupl > -1) {
    // swap order values
    _select->getRow(dupl)->setOrder (prev_order);
  }
  _select->getRow(index)->setOrder (order);
}

void TraceSelectorNDoPop::togChanged (void *data, long /*unused*/,
  Boolean set)
{
  int frames_state;
  TraceSelectorNDoPop *obj = (TraceSelectorNDoPop*)data;
  if (set) {
    if (obj->_frames == 0) {
      if (obj->_sav_frames > 1) {
	obj->_movie->SetValue (FRAMES, obj->_sav_frames);
        frames_state = CHANGED;
      }
      else {
        frames_state = CHANGED_TRIVIALLY;
      }
    }
    else {
      frames_state = UNCHANGED;
    }
  }
  else {
    obj->_sav_frames = obj->_frames;
    if (obj->_frames == 0) {
      frames_state = UNCHANGED;
    }
    else {
      frames_state = CHANGED_TRIVIALLY;
    }
    obj->_movie->SetValue (FRAMES, 0L);
    obj->_movie->clear (FRAMES);
  }
  if (set == 0) {
    obj->processMovieState (TURNED_OFF, frames_state);
  }
  else {
    obj->processMovieState (TURNED_ON, frames_state);
  }
}


void TraceSelectorNDoPop::movieFocused (void *data, long /*unused*/)
{
  TraceSelectorNDoPop *obj = (TraceSelectorNDoPop*)data;
  obj->_old_frames = obj->_frames;
}

void TraceSelectorNDoPop::movieDefocused (void *data, long /*unused*/)
{
  TraceSelectorNDoPop *obj = (TraceSelectorNDoPop*)data;
  if (obj->_old_frames != obj->_frames) {

    if (obj->_frames != 0) obj->_ifmovie->SetTog (DOMOVIE, True );
    else                   obj->_ifmovie->SetTog (DOMOVIE, False);

    if (obj->_frames < 2) {
      obj->processMovieState (UNCHANGED, CHANGED_TRIVIALLY);
    }
    else {
      obj->processMovieState (UNCHANGED, CHANGED);
    }
  }
}
