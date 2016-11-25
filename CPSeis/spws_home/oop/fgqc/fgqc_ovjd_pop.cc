// Author Michael L. Sherrill 10/95
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
// Installs ovjd picking class and creates menu to be used with FgSeisPlot

#include <Xm/Label.h>
#include <Xm/Separator.h>
#include "fgqc/fgqc_ovjd_pop.hh"
#include "fgqc/fgqc_ovjd_plot.hh"
#include "fgmap/fg_seis_plot.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_prim.hh"

#define    CHANGE_VELOCITY  5000F
#define    PRIMARY_HEADER_VAL 9L
#define    SECONDARY_HEADER_VAL 10L
#define    SKIP_HEADER_VAL  0L
static String defres[]= {
    ".height:                        610",
    ".width:                         450",
    "*ovjdvelocitylab.labelString:   Offset Velocity",
    "*modifyhdrlab.labelString:      Flag Selection",
    "*new_velocityL.labelString:     Velocity",
    "*primary_headerL.labelString:   Primary Header",
    "*secondary_headerL.labelString: Secondary Header",
    "*skip_headersL.labelString:     Label Skip",
    "*select_source.labelString:     Select Source Flag",
    "*select_receiver.labelString:   Select Receiver Flag",
    "*select_pp.labelString:         Select Profile Pattern",
    "*select_rp.labelString:         Select Receiver Pattern",
    "*select_cmp.labelString:        Select Cmp Card Location",
    "*scan.labelString:              Increment label skip on scan",
    "*ovjdvelocitylab.fontList:      -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*modifyhdrlab.fontList:         -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    NULL};


enum {VELOCITY, PRIMARY_HEADER, SECONDARY_HEADER, SKIP_HEADERS};
enum {SELECT_SOURCE, SELECT_RECEIVER, SELECT_PP, SELECT_RP, SELECT_CMP};
enum {USE_HEADERS, USE_LABEL_SKIP, INCREMENT_ON_SCAN};

static char *picking_mode = "Mode: Pick OVJD Trace";
static const char * const help_token = "OVJD";
static char *ovjdhelp= "mouse*OVJD:  BTN#1: Select location, \
BTN#2: None, BTN#3: None";



#define ParentClass SLFPopSep


FgSeisOvjdPop::FgSeisOvjdPop( Widget            p,
                              char              *name,
                              SeisPlot          *sp,
                              SeisOvjd          *so,
                              HelpCtx           hctx,
                              FgQcOvjdPlot      *plot) 
                              : SeisOvjdPop(p,name,so,hctx,True),
                                SeisInform(sp), _plot(plot)

{
  _sp = sp;

  static SLText textb1[]  = {
    {"new_velocity",   NULL, NULL, SLType_float, VELOCITY},
  };
  textb1[0].target= &_new_velocity;

  static SLText textb2[] = {
    {"primary_header", NULL, NULL,   SLType_int,   PRIMARY_HEADER},
    {"secondary_header", NULL, NULL, SLType_int,   SECONDARY_HEADER},
  };
  textb2[0].target= &_primary_header;
  textb2[1].target= &_secondary_header;

  static SLText textb3[] = {
    {"skip_headers",   NULL, NULL, SLType_int,   SKIP_HEADERS},
  };
  textb3[0].target= &_skip_headers;

  static SLRadio select_radio[]  = {
    { "select_source",   SELECT_SOURCE },
    { "select_receiver", SELECT_RECEIVER },
    { "select_pp",       SELECT_PP },
    { "select_rp",       SELECT_RP },
    { "select_cmp",      SELECT_CMP },
  };

  static SLRadio matchtypes[]  = 
     {
       { "Use Headers",   USE_HEADERS },
       { "Use Label Skip", USE_LABEL_SKIP },
     };

   static SLTog scan[]  = 
     {
      { "scan", NULL, INCREMENT_ON_SCAN },
     };

   setDefaultResources( p, name, defres);

   
   _velocity_box= new SLTextBox( this, "velocity_box", getHelpCtx(),
                              textb1,XtNumber(textb1), False, 1, False, False );


   _match_box = new SLTextBox( this, "match_box", getHelpCtx(),
                              textb2,XtNumber(textb2), True, 1, True, False );
   _match_box->setAltLosingAction((SLTextfunc)SkipAction, this);



   _skip_box = new SLTextBox( this, "skip_box", getHelpCtx(),
                              textb3,XtNumber(textb3), True, 1, True, False );
   _skip_box->setAltLosingAction((SLTextfunc)SkipAction, this);

   _scan_box = new SLTogBox(this, "scan_box",getHelpCtx(),scan,
                            XtNumber(scan), True, False, False );


   _select_box= new SLRadioBox(this,"select_box",getHelpCtx(), select_radio, 
                               XtNumber(select_radio), NULL, True,False );
   _select_box->setAltChoiceAction( (SLRadioButtonfunc)tableSelect, this);


   _match_radiobox= new SLRadioBox(this,"coordradiobox",getHelpCtx(),matchtypes,
                                   XtNumber(matchtypes), NULL, True, False );
   _match_radiobox->setAltChoiceAction((SLRadioButtonfunc)MatchAction,this);

   
   _trace_selected = False;
   _tindex = 0;
   _so->_sop = this;
   _pick_ovjd = NULL;
   _primary_header = 1;
   _use_match_header = True;
   _skip_headers = 0;
   _original_skip = 0;
   DoAction();     

}


FgSeisOvjdPop::~FgSeisOvjdPop()
{

}



Widget FgSeisOvjdPop::make(Widget p)
{

   if ( made() ) return topWidget();
   SLFPopSep::make(p);



   Widget ovjdvelocitylab= XtVaCreateManagedWidget(
                            "ovjdvelocitylab",  xmLabelWidgetClass, topWidget(),
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNleftOffset,       150, 
                            XmNtopAttachment,    XmATTACH_FORM,
                            XmNtopOffset,        10,NULL );




   Widget sep_1 = XtVaCreateManagedWidget("sep_1", xmSeparatorWidgetClass,
                             topWidget(),
                             XmNtopAttachment,    XmATTACH_WIDGET,
                             XmNtopWidget,        _scan_box->W(),
                             XmNtopOffset,        10,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       10,
                             XmNrightAttachment,  XmATTACH_FORM, 
                             XmNrightOffset,      10,
                             NULL);

  Widget modifyhdrlab= XtVaCreateManagedWidget(
                            "modifyhdrlab",     xmLabelWidgetClass, topWidget(),
                            XmNtopAttachment,    XmATTACH_WIDGET,
                            XmNtopWidget,        sep_1,
                            XmNtopOffset,        10,
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNleftOffset,       150, NULL );


   XtVaSetValues( _velocity_box->W(), XmNleftAttachment, XmATTACH_POSITION,
                                      XmNleftOffset,     170,
                                      XmNtopAttachment,  XmATTACH_POSITION, 
                                      XmNtopOffset,      30,
                                      NULL);

   _match_radiobox->SetRadio(USE_HEADERS);
   XtVaSetValues( _match_radiobox->W(), 
                                   XmNtopAttachment,    XmATTACH_WIDGET,
                                   XmNtopWidget,        _velocity_box->W(), 
                                   XmNtopOffset,        10,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNleftOffset,       120,
                                   XmNrightAttachment,  XmATTACH_FORM, 
                                   XmNrightOffset,      120,
                                   NULL);
   
   XtVaSetValues( _match_box->W(), 
                                   XmNtopAttachment,    XmATTACH_WIDGET,
                                   XmNtopWidget,        _match_radiobox->W(), 
                                   XmNtopOffset,        10,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNleftOffset,       75,
                                   XmNrightAttachment,  XmATTACH_FORM,
                                   XmNrightOffset,      75,
                                   NULL);

   XtVaSetValues( _skip_box->W(), 
                                   XmNtopAttachment,    XmATTACH_WIDGET,
                                   XmNtopWidget,        _match_box->W(), 
                                   XmNtopOffset,        10,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNleftOffset,       75,
                                   XmNrightAttachment,  XmATTACH_FORM,
                                   XmNrightOffset,      75,
                                   NULL);
    
  XtVaSetValues( _scan_box->W(),   XmNtopAttachment,    XmATTACH_WIDGET,
                                   XmNtopWidget,        _skip_box->W(), 
                                   XmNtopOffset,        0,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNleftOffset,       75,
                                   XmNrightAttachment,  XmATTACH_FORM,
                                   XmNrightOffset,      75,
                                   NULL);



   XtVaSetValues( _select_box->W(), XmNtopAttachment, XmATTACH_WIDGET,
                                    XmNtopWidget,        modifyhdrlab,
                                    XmNtopOffset,        10,
                                    XmNleftAttachment,   XmATTACH_FORM, 
                                    XmNleftOffset,       75, 
                                    XmNrightAttachment,  XmATTACH_FORM, 
                                    XmNrightOffset,      75, NULL);
   
   Widget tmp=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                    XmNtopAttachment,    XmATTACH_WIDGET,
                                    XmNtopWidget,        _select_box->W(),
                                    XmNbottomAttachment, XmATTACH_WIDGET,
                                    XmNbottomWidget,     bottomSeparator(),
                                    XmNleftAttachment,   XmATTACH_FORM,
                                    XmNleftOffset,       5,
                                    XmNtopOffset,        5,
                                    NULL);


   return topWidget();

}

void FgSeisOvjdPop::manage()
{
  if(_first_time) 
     {
     _velocity_box->SetValue(VELOCITY, _so->getVelocity());
     _match_box->SetValue(PRIMARY_HEADER, PRIMARY_HEADER_VAL);
     _match_box->SetValue(SECONDARY_HEADER, SECONDARY_HEADER_VAL); 
     _skip_box->SetValue(SKIP_HEADERS, SKIP_HEADER_VAL);
     }

  XtManageChild(topWidget()); 

  _first_time = False;

}



void FgSeisOvjdPop::DoAction()
{
 ParentClass::DoAction();
 Boolean replot = False;

  if(!_sp->imageIsDisplayed())return; 

  _plot->preparePlot();

  replot = True;

  if(_so->getVelocity() != _new_velocity) 
     {
     _so->changeVelocity(_new_velocity);
     replot = True;
     }

  if(!_first_time)
     {
     switch (whichButton())
        {
        case FP_OK:
        case FP_APPLY:
                   if(_sp->imageIsDisplayed())
                      {
                      _so->makeVisible();
                      replot = True;
		      }
                   break;
        }
     }


  if(replot) 
     {
     changeHeaders();
     _sp->redraw();
     _so->post(_sp);
     }

}



void FgSeisOvjdPop::MatchAction( void *data, long which )
{
FgSeisOvjdPop *obj = (FgSeisOvjdPop *)data;
  
  if(which == USE_HEADERS)
    {
    XtSetSensitive(obj->_match_box->TxtW(PRIMARY_HEADER), True);
    XtSetSensitive(obj->_match_box->TxtW(SECONDARY_HEADER), True);
    XtSetSensitive(obj->_skip_box->TxtW(SKIP_HEADERS), False);
    XtSetSensitive(obj->_scan_box->TogW(INCREMENT_ON_SCAN), False);
    obj->_scan_box->SetTog(INCREMENT_ON_SCAN , False );
    obj->_use_match_header = True;
    }
  else
    {
    XtSetSensitive(obj->_match_box->TxtW(PRIMARY_HEADER), False);
    XtSetSensitive(obj->_match_box->TxtW(SECONDARY_HEADER), False);
    XtSetSensitive(obj->_skip_box->TxtW(SKIP_HEADERS), True);
    XtSetSensitive(obj->_scan_box->TogW(INCREMENT_ON_SCAN), True);
    obj->_use_match_header = False;
    }
}

void FgSeisOvjdPop::SkipAction( void *data, long which )
{
FgSeisOvjdPop *obj = (FgSeisOvjdPop *)data;

//if(which == SKIP_HEADERS && obj->_skip_headers > obj->_sp->totalTraces() - 1)
//   obj->_velocity_box->SetValue(SKIP_HEADERS,  obj->_sp->totalTraces() - 1);

//Until we allow other headers to select by only allow header 1,3,4,9,10.
  if(which == PRIMARY_HEADER || which == SECONDARY_HEADER)
    {
    if(obj->_primary_header != 1 && obj->_primary_header != 3 &&
       obj->_primary_header != 9)
      {
      obj->_primary_header = 9;
      obj->_secondary_header = 10;
      obj->_match_box->SetValue(PRIMARY_HEADER,  obj->_primary_header); 
      obj->_match_box->SetValue(SECONDARY_HEADER,obj->_secondary_header);
      }
    if(obj->_secondary_header != 0 && obj->_secondary_header != 4 &&
       obj->_secondary_header != 10)
      {
      obj->_primary_header = 9;
      obj->_secondary_header = 10;
      obj->_match_box->SetValue(PRIMARY_HEADER,  obj->_primary_header); 
      obj->_match_box->SetValue(SECONDARY_HEADER,obj->_secondary_header);
      }
    if(obj->_primary_header == 1)
      {
      obj->_secondary_header = 0;
      obj->_match_box->SetValue(SECONDARY_HEADER,obj->_secondary_header);
      }
    }
  else if(which == SKIP_HEADERS)
    {
    obj->_original_skip = obj->_skip_headers;
    }


    
}

void FgSeisOvjdPop::preScan( SeisPlot *sp, SeisPlot::ScanDir /*dir*/ )
{
  if(!_sp->imageIsDisplayed())return; 
  _previous_file_skip = sp->plottedISkp();
}


void FgSeisOvjdPop::postScan( SeisPlot *sp, SeisPlot::ScanDir dir )
{
long skip_change;

  if(_scan_box->IsSelected(INCREMENT_ON_SCAN))
    {
     if(!_sp->imageIsDisplayed())return; 
     skip_change = abs( (int)(sp->plottedISkp() - _previous_file_skip) );
     if(dir == SeisPlot::Right)//scanning towards end of file
       {
       _skip_headers += skip_change;
       }
     else if(dir == SeisPlot::Left)//scanning towards beginning of file
       {
       _skip_headers -= skip_change;
       }
     _skip_headers = max(0,_skip_headers);
     _skip_box->SetValue(SKIP_HEADERS, _skip_headers);
     _plot->preparePlot();
     changeHeaders();
     _sp->redraw();
     _so->post(_sp);
    }

}



void FgSeisOvjdPop::newData()
{


  if(!_sp->imageIsDisplayed() || _first_time)return;

  _tindex = 0;

  _so->_headers_modified = False;

  _trace_selected = False;
 
  _plot->preparePlot();
}



void FgSeisOvjdPop::tableSelect( void *data, long button)
{
  FgSeisOvjdPop *obj = (FgSeisOvjdPop *)data;

  if(obj->_pick_ovjd != NULL) delete obj->_pick_ovjd;

  obj->_pick_ovjd = new FgPickOvjd(obj->_plot->sp(), obj);
     
  obj->_table_select = button;
}




//******************** Ovjd's PickBase ********************************

FgPickOvjd::FgPickOvjd (FgSeisPlot    *sp, 
                        FgSeisOvjdPop *sop)
                        : PickBase(sp, picking_mode, 
                                   help_token, ovjdhelp,
                                   XC_tcross, allow, True), _sop(sop)
{

}

void FgPickOvjd::buttonAny(int ev_x1, int /*ev_x2*/, int /*ev_y1*/, 
                         int /*ev_y2*/, int button, Action action, 
                         Modifier /*modifier*/)
{
long trace, fgtrace;
const float *sphd;
long line_index, flag_index;
long primary_index, secondary_index;


 if(!_sop->_sp->imageIsDisplayed())return;
 sphd = _sop->_sp->headers();


 if(button == 1 && action == press)
    {
    trace = (long)(_sop->_sp->xWC(ev_x1) + .5);
    if(trace < 1) trace = 1;
    if(trace > _sop->_sp->plottedNplt()) trace = _sop->_sp->plottedNplt(); 
    _sop->_tindex = (trace-1) * _sop->_sp->numHeaders();
    _sop->_trace_selected = True;

    switch(_sop->_table_select)
      {
      case SELECT_SOURCE:
        _sop->_plot->fg()->startHeadersFromScratch();
        if(_sop->matchHeaderMode())//use seismic headers to match
          {
          primary_index = (trace-1) * _sop->_sp->numHeaders() +
                           _sop->primaryHeader() - 1;
          secondary_index = (trace-1) * _sop->_sp->numHeaders() +
                           _sop->secondaryHeader() - 1;
          if(!_sop->secondaryHeader())// using only one header to match
            {
            _sop->_plot->fg()->calculateHeaderWords((long)sphd[primary_index],
                                                    False);
            }
          else
            {
            fgtrace = _sop->_plot->fg()->findTraceNumber(
                                (long)sphd[primary_index],
                                (long)sphd[secondary_index]);
            _sop->_plot->fg()->calculateHeaderWords(fgtrace,False);
            }
          }
        else//use label skip
          {
          _sop->_plot->fg()->calculateHeaderWords(trace+_sop->_skip_headers,
                                                  False);
          }
        line_index = _sop->_plot->fg()->getHeaderSourceLineIndex();
        flag_index = _sop->_plot->fg()->getHeaderSourceFlagIndex();
        if(line_index != -1 && flag_index != -1)
          {
          _sop->_plot->fg()->setActiveLineIndex(line_index); 
          _sop->_plot->fg()->setActiveFlagIndexOnLine(line_index,flag_index);
          }
        break;          


      case SELECT_RECEIVER:
        _sop->_plot->fg()->startHeadersFromScratch();
        if(_sop->matchHeaderMode())//use seismic header to match
          {
          primary_index = (trace-1) * _sop->_sp->numHeaders() +
                          _sop->primaryHeader() - 1;
          secondary_index = (trace-1) *_sop->_sp->numHeaders()  +
                            _sop->secondaryHeader() - 1;
          if(!_sop->secondaryHeader())// using only one header to match
            {
            _sop->_plot->fg()->calculateHeaderWords((long)sphd[primary_index],
                                                    False);
            }
          else
            {
            fgtrace = _sop->_plot->fg()->findTraceNumber(
                                (long)sphd[primary_index],
                                (long)sphd[secondary_index]);
            _sop->_plot->fg()->calculateHeaderWords(fgtrace,False);
            }
          }
        else//use label skip
          {
          _sop->_plot->fg()->calculateHeaderWords(trace+_sop->_skip_headers,
                                                  False);
          }
        line_index = _sop->_plot->fg()->getHeaderReceiverLineIndex();
        flag_index = _sop->_plot->fg()->getHeaderReceiverFlagIndex();
        if(line_index != -1 && flag_index != -1)
          {
          _sop->_plot->fg()->setActiveLineIndex(line_index); 
          _sop->_plot->fg()->setActiveFlagIndexOnLine(line_index,flag_index);
          }
        break;


      case SELECT_RP:
        _sop->_plot->fg()->startHeadersFromScratch();
        if(_sop->matchHeaderMode())//use seismic header to match
          {
          primary_index = (trace-1) * _sop->_sp->numHeaders() +
                            _sop->primaryHeader() - 1;
          secondary_index = (trace-1) * _sop->_sp->numHeaders() +
                            _sop->secondaryHeader() - 1;
          if(!_sop->secondaryHeader())// using only one header to match
            {
            _sop->_plot->fg()->calculateHeaderWords((long)sphd[primary_index],
                                                    False);
            }
          else
            {
            fgtrace = _sop->_plot->fg()->findTraceNumber(
                                (long)sphd[primary_index],
                                (long)sphd[secondary_index]);
            _sop->_plot->fg()->calculateHeaderWords(fgtrace,False);
            }
          }
        else//use label skip
          {
          _sop->_plot->fg()->calculateHeaderWords(trace+_sop->_skip_headers,
                                                  False);
          }
        if(_sop->_plot->fg()->getHeaderRpCardIndex() != -1)
           _sop->_plot->fg()->setActiveRpCardIndex(
                                    _sop->_plot->fg()->getHeaderRpCardIndex());
        break;


      case SELECT_PP:
        _sop->_plot->fg()->startHeadersFromScratch();
        if(_sop->matchHeaderMode())//use seismic header to match
          {
          primary_index = (trace-1) * _sop->_sp->numHeaders() +
                            _sop->primaryHeader() - 1;
          secondary_index = (trace-1) * _sop->_sp->numHeaders() +
                             _sop->secondaryHeader() - 1;
          if(!_sop->secondaryHeader())// using only one header to match
            {
            _sop->_plot->fg()->calculateHeaderWords((long)sphd[primary_index],
                                                    False);
            }
          else
            {
            fgtrace = _sop->_plot->fg()->findTraceNumber(
                                (long)sphd[primary_index],
                                (long)sphd[secondary_index]);
            _sop->_plot->fg()->calculateHeaderWords(fgtrace,False);
            }
          }
        else//use label skip
          {
          _sop->_plot->fg()->calculateHeaderWords(trace+_sop->_skip_headers,
                                                  False);
          }
        if(_sop->_plot->fg()->getHeaderPpCardIndex() != -1)
           _sop->_plot->fg()->setActivePpCardIndex(
                                    _sop->_plot->fg()->getHeaderPpCardIndex());
        break;


      case SELECT_CMP:
        _sop->_plot->fg()->startHeadersFromScratch();
        if(_sop->matchHeaderMode())//use seismic header to match
          {
          primary_index = (trace-1) * _sop->_sp->numHeaders() +
                            _sop->primaryHeader() - 1;
          secondary_index = (trace-1) * _sop->_sp->numHeaders() +
                            _sop->secondaryHeader() - 1;
          if(!_sop->secondaryHeader())// using only one header to match
            {
            _sop->_plot->fg()->calculateHeaderWords((long)sphd[primary_index],
                                                    False);
            }
          else
            {
            fgtrace = _sop->_plot->fg()->findTraceNumber(
                                (long)sphd[primary_index],
                                (long)sphd[secondary_index]);
            _sop->_plot->fg()->calculateHeaderWords(fgtrace,False);
            }
          }
        else//use label skip
          {
          _sop->_plot->fg()->calculateHeaderWords(trace+_sop->_skip_headers,
                                                  False);
          }
        if( _sop->_plot->fg()->getHeaderCmpIndex() != -1)
            _sop->_plot->fg()->setActiveCmpIndex(
                                    _sop->_plot->fg()->getHeaderCmpIndex());
        break;



      }//end switch


    //_sop->_pick_ovjd = NULL;  
    //delete this;
    }//end if button



  PrimSupport::updateEverything();
}






