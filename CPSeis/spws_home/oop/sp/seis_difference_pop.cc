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
//************************ COPYRIGHT NOTICE ****************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
//************************ COPYRIGHT NOTICE ****************************



//************************************************************************
//***    Menu to control the selection of plots to generate a diff plot***
//***    Author:Michael L. Sherrill 12/2001                            ***
//************************************************************************

#include "sp/seis_difference_pop.hh"
#include "sp/seis_difference_plot.hh"
#include "sp/seis_multiplot_control.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_arrow_scale.hh"
#include "sl/sl_tog_box.hh"
#include "sl/slp_push.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_error_pop.hh"
#include "sl/sl_radio_box.hh"
#include <stdio.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>



enum { SELECTED = 1, SEQUENCE, FILENAMES};

static String  defres[]= {
  "*tiL.labelString:         Trace/Inch:",
  "*isL.labelString:         Inches/Sec:",
  "*ctL.labelString:         Ct:",
  "*gs.labelString:          Variable Density",
  "*wonly.labelString:       Wigl Only",
  "*wfill.labelString:       Variable Area Pos",
  "*wfilln.labelString:      Variable Area Neg",
  "*norm.labelString:        Normalize",
  "*ext.labelString:         Scale to Amp",
  //"*stf.labelString:         Scale to File",
  "*stp.labelString:         Scale to Panel",
  "*ti.value:                20.00",
  "*is.value:                2.00",
  "*ct.value:                4.00",
  "*wfill.set:               True",
  "*stp.set:                 True",
  NULL
};


//==========================================================================
//=====                      Constructor                         ===========
//==========================================================================
SeisDifferencePop::SeisDifferencePop( Widget               w,
                                      char                 *name,
                                      HelpCtx              hctx,
                                      SLArrowScale         *scale,
                                      SeisMultiPlotControl *control)
                       : SLDialog(w, name, hctx, FALSE, 33),
                         _arrow_scale(scale), 
                         _control(control),
                         _plot(NULL),
                         _first_seisplot(NULL),
                         _second_seisplot(NULL)

{

static SLText trace_size[]  = {
 { "ti",   "range:0.0 *,default:20.00000",  NULL,  SLType_float,   TI },
 { "is",   "range:0.0 *,default:2.00000",   NULL,  SLType_float,   IS },
 { "ct",   NULL,  NULL,  SLType_float,   CT },
};
trace_size[0].target=&_traces_per_inch; 
trace_size[1].target=&_inches_per_second; 
trace_size[2].target=&_ct; 

static SLRadio plot_mode_rads[]  = {
 { "gs",    PlotImage::PlotGS },
 { "wonly", PlotImage::PlotWONLY },
 { "wfill", PlotImage::PlotWFILL },
 { "wfilln",NEGATIVE_FILL },
};

static SLRadio norm_rads[]  = {
 { "stp",  PlotImage::PANELNORM },
 { "norm", PlotImage::NORM },
 //{ "stf",  PlotImage::FILENORM },
 { "ext",  PlotImage::EXTERNALNORM },
};

static SLText amp_text[]  = {
 { "amp",   NULL,  NULL,  SLType_float,   PlotImage::EXTERNALNORM },
};
amp_text[0].target = &_external_amp;



  setDefaultResources( w, name, defres);

  SLSmartForm *work   = workArea();
  _table = new SeisDifferenceTable(work, name, this);

  _clear_button  = new SLpPush (work, "Start Over" );
  _clear_button->setNotify(this);


  _trace_size_box = new SLTextBox( work,"trace_size_box",getHelpCtx(),
                                  trace_size,XtNumber(trace_size), True);

  _plot_mode_radios= new SLRadioBox( work, "plot_mode_radios", getHelpCtx(),
                                     plot_mode_rads, XtNumber(plot_mode_rads),
                                     NULL, True );

  _norm_type= new SLRadioBox( work, "norm_type", getHelpCtx(),
                              norm_rads, XtNumber(norm_rads), NULL, True);
  _norm_type->setComplexNotify(this);

  _ext_amp= new SLTextBox( work,"ext_amp",getHelpCtx(),
                           amp_text,XtNumber(amp_text), False);
  _ext_amp->setComplexNotify(this);



  work->attach(_clear_button, work, NULL, work, NULL,
               10, 10);
  work->attach(_table, work, work, _clear_button, NULL,  
               0,   0,  10,  0);
  work->attach(_trace_size_box,  work, NULL, _table, NULL,  
               0,   0,  10,  0); 
  work->attach(_plot_mode_radios, _trace_size_box, NULL, _table, NULL,
               70,  0,  10,   0);
  work->attach(_norm_type, _plot_mode_radios, NULL, _table, NULL,
               70,  0,  10,  0);
  work->attach(_ext_amp, _plot_mode_radios, NULL, _norm_type, work,
               70,  0,  0,  10);



  _ok_button     = addBottomOK(1, NULL, this);
  _apply_button  = addBottomApply(2, NULL, this);
  _cancel_button = addBottomCancel(3, NULL, this);
  addBottomHelp();

  setTitle("Difference Plot Menu");
}



//==========================================================================
//=====                      Destructor                          ===========
//==========================================================================
SeisDifferencePop::~SeisDifferencePop()
{
  if(_plot != NULL) delete _plot;
  delete _table;
}


Widget SeisDifferencePop::make(Widget p)
{
  if ( made() ) return topWidget();
  SLDialog::make(p);

  Widget sep1 = XtVaCreateManagedWidget("sep1", xmSeparatorWidgetClass,
                                 topWidget(),
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _ext_amp->W(),
                                 XmNtopOffset,        1,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 NULL);

  return topWidget();

}


//==========================================================================
//===== Handle user input ( time ranges etc)                     ===========
//==========================================================================
Boolean SeisDifferencePop::notifyComplex(SLDelay *obj, int ident)
{
  if (obj == _norm_type) 
    {
    if (ident ==  PlotImage::EXTERNALNORM) 
      {
      _ext_amp->SetValue(PlotImage::EXTERNALNORM, (float)_external_amp );
      _norm_type->SetRadio(PlotImage::EXTERNALNORM);
      }
    else
      {
      _ext_amp->clear(PlotImage::EXTERNALNORM);
      }
    }

  else if (obj == _ext_amp) 
    {
    _norm_type->SetRadio(PlotImage::EXTERNALNORM);
    }

   return True;
}

//=========================================================================
//======== Notify to handle the Ok, Apply, Cancel buttons =================
//=========================================================================
Boolean SeisDifferencePop::notify (SLPrim *gui)
{
int ok = 1;

  

  if(gui == (SLPrim *)_ok_button)
    {
    ok = plot();
    }

  else if(gui == (SLPrim *)_apply_button)
    {
    ok = plot();
    }

  else if(gui == (SLPrim *)_cancel_button)
    {
    }

  else if(gui == (SLPrim *)_clear_button)
    { 
    clearSelections();
    _first_seisplot = _second_seisplot = NULL;
    }

  if(ok) SLDialog::notify(gui);

  return (Boolean)ok;

}



//==========================================================================
//===== Allow the user to clear previous selections to make new  ===========
//==========================================================================
void SeisDifferencePop::clearSelections()
{
  _table->clearSelections();
}

//==========================================================================
//===== Should return the number of files currently displayed    ===========
//==========================================================================
long SeisDifferencePop::getNumFrames()
{
  return _arrow_scale->getNumFrames();
}


//==========================================================================
//===== Get a filename associated with a display                 ===========
//==========================================================================
char *SeisDifferencePop::getFilename(long i)
{
  return _arrow_scale->getFilename((int)i);
}

//==========================================================================
//===== Return the sequential order the user has selected files  ===========
//==========================================================================
long SeisDifferencePop::frameSequenceNumber(int i)
{
  return _table->frameSequenceNumber(i);
}


//==========================================================================
//===== Associate a SeisPlot that will be the primary display    ===========
//==========================================================================
void SeisDifferencePop::setFirstSeisPlot(int index)
{

  _first_seisplot = NULL;

  _first_seisplot = _control->getSeisPlot(_table->getFilename(index));

  assert(_first_seisplot != NULL);

}


//==========================================================================
//===== Associate a SeisPlot that will be subtracted from first  ===========
//==========================================================================
void SeisDifferencePop::setSecondSeisPlot(int index)
{

  _second_seisplot = NULL;

  _second_seisplot = _control->getSeisPlot(_table->getFilename(index));

  assert(_first_seisplot != NULL && _second_seisplot != NULL);

  _allowed_tmin      = max(_first_seisplot->plottedTmin(),
                           _second_seisplot->plottedTmin());
  _allowed_tmax      = min(_first_seisplot->plottedTmax(),
                           _second_seisplot->plottedTmax());
  _allowed_nplt      = min(_first_seisplot->plottedNplt(),
                           _second_seisplot->plottedNplt());

  _nplt              = _allowed_nplt;//may want a menu later
  _tmin              = _allowed_tmin;//may want a menu later
  _tmax              = _allowed_tmax;//may want a menu later

}


//==========================================================================
//===== If the user has destroyed the plot we dont want our      ===========
//===== destructor to try and delete is also                     ===========
//==========================================================================
void SeisDifferencePop::userRemovedPlot(SeisDifferencePlot *sdp)
{
  if(sdp == _plot) _plot = NULL;
}

//==========================================================================
//===== Attempt to generate the difference plot                  ===========
//==========================================================================
int SeisDifferencePop::plot()
{
int ok = 0;
int error;
char errmsg[80];
SLErrorPop *errpop;


  if(_first_seisplot == NULL || _second_seisplot == NULL)//SeisPlots unassigned
    {
    errpop = new SLErrorPop(topWidget(),"Error","Must select files first.\n");
    return ok;
    }


  //May want to allow differing sample rates later, not currently supported
  if(_first_seisplot->srval() != _second_seisplot->srval())
    {
    errpop = new SLErrorPop(topWidget(),"Error",
                    "Sorry, different sample rates are not yet supported.\n");
    return ok;
    }

  if(_tmax <= _tmin)
    {
    errpop = new SLErrorPop(topWidget(),"Error",
                    "Time max is <= time min.\n");
    return ok;
    }



  _plot = new SeisDifferencePlot(topWidget(), "diff_plot",
                                 getHelpCtx(), _first_seisplot,
                                 _second_seisplot,   
                                 this, 33);
  error = _plot->plot(errmsg);

  if(error)
    {
    delete _plot;
    _plot = NULL;
    errpop = new SLErrorPop(topWidget(),"Error",errmsg);
    return ok;
    }  


  return (ok = 1);
}



//=============================================================================
//====  The SLDataBox that handles user's frame selection                   ===
//=============================================================================
SeisDifferenceTable::SeisDifferenceTable( SLDelay             *slparent,
                                          char                *name,
                                          SeisDifferencePop  *pop)
                                    : SLDatabox(slparent,name, NULL, 4),
                                      _pop(pop),
                                      _sequence(0)
{
  for(int i = 0; i < SLArrowScale::MAX_FRAMES; i++)
    _frames_set[i] = 0;
}


SeisDifferenceTable::~SeisDifferenceTable()
{

}


void SeisDifferenceTable::clearSelections()
{
  for(int i = 0; i < SLArrowScale::MAX_FRAMES; i++)
    _frames_set[i] = 0;
  _sequence = 0;
  setSensitivity(True);
}


long SeisDifferenceTable::frameSequenceNumber(int i)
{
  return _frames_set[i];
}

long SeisDifferenceTable::getNumFrames()
{
  return _pop->getNumFrames();
}

int SeisDifferenceTable::getNumSequencedFrames()
{
  return _sequence;
}

char *SeisDifferenceTable::getFilename(long i)
{
  return _pop->getFilename(i);
}



//=============================================================================
//=== Set the number of rows equal to the number of movie frames            ===
//=============================================================================
static long num_frames_update(void *data)
{
SeisDifferenceTable *dt = (SeisDifferenceTable *)data;
static int last_num_frames = 0;
long frames = dt->getNumFrames();

  if(frames != last_num_frames)
    {
      //may need to do something here
    }

  last_num_frames = frames;

  assert(frames < SLArrowScale::MAX_FRAMES);

  return frames;
}



//=============================================================================
//=== Handle the user selection of frames to toggle between                 ===
//=============================================================================
static void select_trap(void *data, long ident, long index,
			long value, long nread, char* endkey)
{
SeisDifferenceTable *dt = (SeisDifferenceTable *)data;

  if(nread == 0) return;

  if(value)
    dt->_frames_set[index] = -1;

}


//==========================================================================
//===== Called when the user toggles a selected file             ===========
//==========================================================================
static long select_update(void *data, long ident, long index)
{
SeisDifferenceTable *dt = (SeisDifferenceTable *)data;


 if(dt->_frames_set[index] == -1)//Set by select_trap
    {
    ++dt->_sequence;
    assert(dt->_sequence < 3);

    dt->_frames_set[index] = dt->_sequence;

    if(dt->_sequence == 1)
      dt->_pop->setFirstSeisPlot(index);
    else
      dt->_pop->setSecondSeisPlot(index);

    if(dt->_sequence > 1)
      {
      dt->setSensitivity(False);
      }
    }

  return dt->_frames_set[index];
  
}



//=============================================================================
//=== Post the sequence number of a frame the user has selected             ===
//=============================================================================
static void sequence_trap(void *data, long ident, long index,
			  long value, long nread, char* endkey)
{
SeisDifferenceTable *dt = (SeisDifferenceTable *)data;

  if(nread == 0) return;
  dt->_frames_set[index] = value;
}

static long sequence_update(void *data, long ident, long index)
{
SeisDifferenceTable *dt = (SeisDifferenceTable *)data;

  return dt->_frames_set[index];
  
}


//=============================================================================
//=== Post the file names of frames for the multiple file movie class       ===
//=============================================================================
static void name_trap(void *data, long /*ident*/, long index,
		      char* value, long nread, char* /*endkey*/)
{
  //Not currently needed
}

static char *name_update(void *data, long /*ident*/, long index)
{
SeisDifferenceTable *dt = (SeisDifferenceTable *)data;
static char filename[255];

  strcpy(filename, dt->getFilename(index));
  return filename; 
}


void SeisDifferenceTable::makeHelper()
{
static long zero  =   0; 
static long three =   3; 
static long five  =   5; 




  //         N                  NMAX              ROW COL NCHAR MAXROWS
  regArrays(num_frames_update, num_frames_update, 3,  0,   6,    45);

  	//   ID        PROMPT              JSW      ISW     COL NCHAR
  regIarray (SELECTED,  "Frame"        , &zero  , &three ,   0,   2);
  regIarray (SEQUENCE,  "Sequence"     , &zero  , &five  ,   0,   3);

  regCarray(FILENAMES,"File name"    , &zero  , &five  ,   0,   48);
  funCvar  (FILENAMES, name_trap,       name_update);

  funIvar  (SELECTED,     select_trap,   select_update   );
  funIvar  (SEQUENCE,     sequence_trap, sequence_update );

}
