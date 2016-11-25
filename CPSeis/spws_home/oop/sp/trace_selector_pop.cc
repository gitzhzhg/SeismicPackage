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
#include "sl/sl_text_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_error_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/trace_selector_pop.hh"
#include "sp/seis_select_pop.hh"
#include "sp/do_abort.hh"
#include "trace_selector.hh"



static String  defres[]= {
     "*tslab.labelString:             Trace Selector Parameters",
     "*tslab.fontList:                -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
     "*headerlab.labelString:         Header #",
     "*minlab.labelString:            Min Value",
     "*maxlab.labelString:            Max Value",
     "*inclab.labelString:            Increment",
     "*doprimary.labelString:         Primary Header:",
     //"*primaryheader.columns:         3",
     "*dosecondary.labelString:       Secondary Header:",
     //"*secondaryheader.columns:       3",
     "*dotertiary.labelString:        Tertiary Header:",
     //"*tertiaryheader.columns:        3",
     "*headerform_Frame.leftOffset:   -10",
     "*primaryminL.labelString:       Min:",
     "*secondaryminL.labelString:     Min:",
     "*tertiaryminL.labelString:      Min:",
     "*primarymaxL.labelString:       Max:",
     "*secondarymaxL.labelString:     Max:",
     "*tertiarymaxL.labelString:      Max:",
     "*search.labelString:            Search File",
NULL};

TraceSelectorPop::TraceSelectorPop( Widget        p,
                                    char          *name,
                                    HelpCtx       hctx,
                                    SeisSelect    *ss)
              : SLFPopSep(p,name,FP_DOOK | FP_DOCANCEL | FP_DOHELP,
                          hctx,False,False),
              _ss(ss), _first_time(True), _traces_found(0)
{

static SLTog htogs[]  = {
 { "doprimary",   NULL,   DOPRIMARY },
 { "dosecondary", NULL,   DOSECONDARY },
 { "dotertiary",  NULL,   DOTERTIARY },
};
htogs[0].target=&_doprimary;
htogs[1].target=&_dosecondary;
htogs[2].target=&_dotertiary;


static SLText htext[]  = {
 { "primaryheader",   NULL,  NULL, SLType_int,     PRIMARY_HEADER },
 { "primarymin",      NULL,  NULL, SLType_float,   PRIMARY_MIN }, 
 { "primarymax",      NULL,  NULL, SLType_float,   PRIMARY_MAX },
 { "primaryinc",      NULL,  NULL, SLType_float,   PRIMARY_INC },
 { "secondaryheader", NULL,  NULL, SLType_int,     SECONDARY_HEADER },
 { "secondarymin",    NULL,  NULL, SLType_float,   SECONDARY_MIN },
 { "secondarymax",    NULL,  NULL, SLType_float,   SECONDARY_MAX },
 { "secondaryinc",    NULL,  NULL, SLType_float,   SECONDARY_INC },
 { "tertiaryheader",  NULL,  NULL, SLType_int,     TERTIARY_HEADER },
 { "tertiarymin",     NULL,  NULL, SLType_float,   TERTIARY_MIN },
 { "tertiarymax",     NULL,  NULL, SLType_float,   TERTIARY_MAX },
 { "tertiaryinc",     NULL,  NULL, SLType_float,   TERTIARY_INC },

};
htext[0].target=&_primary_header;
htext[1].target=&_primary_min;
htext[2].target=&_primary_max;
htext[3].target=&_primary_inc;
htext[4].target=&_secondary_header;
htext[5].target=&_secondary_min;
htext[6].target=&_secondary_max;
htext[7].target=&_secondary_inc;
htext[8].target=&_tertiary_header;
htext[9].target=&_tertiary_min;
htext[10].target=&_tertiary_max;
htext[11].target=&_tertiary_inc;

static SLPush search[]  = {
 { "search",  SEARCH },
};

  _header_form= new SLForm(this,"headerform",hctx,True);


  _header_togs= new SLTogBox(_header_form, "headertogs", getHelpCtx(), 
                         htogs,XtNumber(htogs));
  _header_togs->setAltChoiceAction(headerTogChanged, (void*)this);


  _header_text= new SLTextBox( _header_form,"headertext",hctx,
                         htext,XtNumber(htext), False, 4);
  _header_text->setAltLosingAction(headerNumberChanged, (void*)this);


  _search_button = new SLPushBox(this, "search", getHelpCtx(),
                                 search, XtNumber(search));
  _search_button->setComplexNotify(this);

  _sp = ss->getSP();
  setDefaultResources( p, name, defres);
  make(p);
  
}

TraceSelectorPop::~TraceSelectorPop()
{
}


Widget TraceSelectorPop::make(Widget p)
{
  if ( made() ) return topWidget();
  SLFPopSep::make(p);

  setTitle("Trace Select");

  Widget tslab= XtVaCreateManagedWidget(  "tslab", xmLabelWidgetClass, 
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_FORM,
                               XmNtopOffset,       15,
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      5,
                               XmNrightAttachment, XmATTACH_FORM, 
                               XmNrightOffset,     5,
                               NULL);

  Widget sep= make_attached_sep(topWidget(), "sep");
  XtVaSetValues( sep,          XmNtopAttachment, XmATTACH_WIDGET,
                               XmNtopWidget,     tslab, 
                               XmNtopOffset,     15, NULL);

  
  Widget headerlab= XtVaCreateManagedWidget(  "headerlab", xmLabelWidgetClass, 
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       sep,
                               XmNtopOffset,       15,
                               XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,      _header_text->W(),
                               XmNleftOffset,      160,
                               NULL);

  Widget minlab= XtVaCreateManagedWidget(  "minlab", xmLabelWidgetClass, 
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       sep,
                               XmNtopOffset,       15,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      headerlab,
                               XmNleftOffset,      12,
                               NULL);

  Widget maxlab= XtVaCreateManagedWidget(  "maxlab", xmLabelWidgetClass, 
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       sep,
                               XmNtopOffset,       15,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      minlab,
                               XmNleftOffset,      6,
                               NULL);

  Widget inclab= XtVaCreateManagedWidget(  "inclab", xmLabelWidgetClass, 
                               topWidget(),
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       sep,
                               XmNtopOffset,       15,
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      maxlab,
                               XmNleftOffset,      6,
                               NULL);


  XtVaSetValues( _header_form->W(),
                               XmNtopAttachment,  XmATTACH_WIDGET,
                               XmNtopWidget,      sep,
                               XmNtopOffset,      30,
                               XmNleftAttachment, XmATTACH_FORM,
                               XmNleftOffset,     10,
                               XmNrightAttachment, XmATTACH_FORM,
                               XmNrightOffset,    10, NULL );

   XtVaSetValues( _header_text->W(), 
                               XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNleftWidget,      _header_togs->W(),
                               XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,       _header_togs->W(),
                               XmNtopOffset,       -4, NULL);

   XtVaSetValues( _search_button->W(), 
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      195,
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       _header_text->W(),
                               XmNtopOffset,       35, NULL);


   _information= XtVaCreateManagedWidget( "information", xmLabelWidgetClass, 
                               topWidget(),
                               XmNtopAttachment, XmATTACH_WIDGET,
                               XmNtopWidget, _search_button->W(),
                               XmNtopOffset,       35,
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNleftOffset,      50,
                               XmNrightAttachment, XmATTACH_FORM, 
                               XmNrightOffset,     50,
                               NULL);

   Widget tmp= XtVaCreateManagedWidget( "", 
                                         xmLabelWidgetClass, topWidget(),
                                         XmNtopAttachment, XmATTACH_WIDGET,
                                         XmNtopWidget,     _information,
                                         XmNbottomAttachment, XmATTACH_WIDGET,
                                         XmNbottomWidget,    bottomSeparator(),
                                         XmNbottomOffset,    20,
                                         XmNleftAttachment,  XmATTACH_FORM,
                                         XmNleftOffset,       5,
                                         XmNtopOffset,        5,
                                         NULL);

   wprocShowMsg(tmp,"");


  _header_togs->SetTog( DOPRIMARY, True );

  _header_text->SetValue(PRIMARY_HEADER,   1L);
  _header_text->SetValue(SECONDARY_HEADER, 2L);
  _header_text->SetValue(TERTIARY_HEADER,  3L);

  _header_text->SetValue(PRIMARY_MIN,   0.0F);
  _header_text->SetValue(SECONDARY_MIN, 0.0F);
  _header_text->SetValue(TERTIARY_MIN,  0.0F);
  
  _header_text->SetValue(PRIMARY_MAX,   0.0F);
  _header_text->SetValue(SECONDARY_MAX, 0.0F);
  _header_text->SetValue(TERTIARY_MAX,  0.0F);

  _header_text->SetValue(PRIMARY_INC,   0.0F);
  _header_text->SetValue(SECONDARY_INC, 0.0F);
  _header_text->SetValue(TERTIARY_INC,  0.0F);


  XtSetSensitive(_header_togs->TogW(DOTERTIARY), False);

  _header_text->SetSensitive( SECONDARY_HEADER, False);
  _header_text->SetSensitive( TERTIARY_HEADER, False);
  _header_text->SetSensitive(SECONDARY_MIN, False);
  _header_text->SetSensitive(TERTIARY_MIN, False);
  _header_text->SetSensitive(SECONDARY_MAX, False);
  _header_text->SetSensitive(TERTIARY_MAX, False);
  _header_text->SetSensitive(SECONDARY_INC, False);
  _header_text->SetSensitive(TERTIARY_INC, False);

  wprocShowMsg(_information,"NO TRACES SEARCHED FOR YET");

  _do_abort= new DoAbort(topWidget(), "SEARCHING...Click to abort");

  _sp->getTraceSelector()->setTraceSelectorAbortFunction(
                             (AbortFunction)DoAbort::altUserAbort,
			     (void*)_do_abort);

  return topWidget();
}

void TraceSelectorPop::manage()
{
  SLFPopSep::manage();
}


void TraceSelectorPop::okButton()
{
long total_frames = _frames_found;

  SLFPopSep::okButton();

  if(_traces_found)
    {
    if(_domovie)
      {
      if(_traces_in_last_frame)
        total_frames += 1;
      _ss->setNumMovieFrames(total_frames);
      }
    else
      {
      _ss->setNumTracesToPlot(_traces_found);
      }
    _ss->useSelector(_primary_header,
                     _primary_min,
                     _primary_max,
                     _primary_inc,
                     _secondary_header,
                     _secondary_min,
                     _secondary_max,
                     _secondary_inc,
                     _tertiary_header,
                     _tertiary_min,
                     _tertiary_max,
                     _tertiary_inc,
                     _traces_found,
                     _frames_found,
                     _traces_in_last_frame,
                     _dosecondary,
                     _doprimary);
    }
  else//User may have hit OK instead of Cancel
    {
    _ss->turnOffSelector();
    }
}

void TraceSelectorPop::cancelButton()
{
  SLFPopSep::cancelButton();
  _ss->turnOffSelector();
}


void TraceSelectorPop::headerTogChanged(void *data, long, Boolean )
{
   TraceSelectorPop *obj= (TraceSelectorPop*)data;

   if(obj->_first_time)
     {
     obj->_first_time = False;
     return;
     }

   //Make sure primary header is always selected
   if(!obj->_header_togs->IsSelected(DOPRIMARY))
       obj->_header_togs->SetTog( DOPRIMARY, True );

   //Allow user to use the tertiary only if the secondary is set
   XtSetSensitive(obj->_header_togs->TogW(DOTERTIARY),
                  obj->_header_togs->IsSelected(DOSECONDARY)); 

   //Set appropriate sensitivity of the header number fields
   if( obj->_header_togs->IsSelected(DOTERTIARY) && 
      !obj->_header_togs->IsSelected(DOSECONDARY)    )
         obj->_header_togs->SetTog( DOTERTIARY, False );
 
   obj->_header_text->SetSensitive(SECONDARY_HEADER,
                  obj->_header_togs->IsSelected(DOSECONDARY));
   obj->_header_text->SetSensitive(TERTIARY_HEADER,
                  obj->_header_togs->IsSelected(DOTERTIARY)); 

   //Set appropriate sensitivity of the min value fields
   obj->_header_text->SetSensitive(SECONDARY_MIN,
                  obj->_header_togs->IsSelected(DOSECONDARY));
   obj->_header_text->SetSensitive(TERTIARY_MIN,
                  obj->_header_togs->IsSelected(DOTERTIARY)); 

   //Set appropriate sensitivity of the max value fields
   obj->_header_text->SetSensitive(SECONDARY_MAX,
                  obj->_header_togs->IsSelected(DOSECONDARY));
   obj->_header_text->SetSensitive(TERTIARY_MAX,
                  obj->_header_togs->IsSelected(DOTERTIARY)); 

   //Set appropriate sensitivity of the inc value fields
   obj->_header_text->SetSensitive(SECONDARY_INC,
                  obj->_header_togs->IsSelected(DOSECONDARY));
   obj->_header_text->SetSensitive(TERTIARY_INC,
                  obj->_header_togs->IsSelected(DOTERTIARY)); 
}


void TraceSelectorPop::headerNumberChanged(void *data, long which)
{
   TraceSelectorPop *obj= (TraceSelectorPop*)data;

}


void TraceSelectorPop::headerMinChanged(void *data, long which)
{
   TraceSelectorPop *obj= (TraceSelectorPop*)data;

}


void TraceSelectorPop::headerMaxChanged(void *data, long which)
{
   TraceSelectorPop *obj= (TraceSelectorPop*)data;

}




Boolean TraceSelectorPop::notifyComplex(SLDelay *obj, int ident)
{
SLErrorPop *errpop;

  if (obj == _search_button) 
    {

    if(_primary_min > _primary_max)
      {
      _header_text->SetValue(PRIMARY_MAX,   _primary_min + 1.0F);
      errpop = new SLErrorPop(this, "Error", "Primary min > max");
      return True;
      }
    else if(_header_togs->IsSelected(DOSECONDARY) &&
            _secondary_min > _secondary_max)
      {
      _header_text->SetValue(SECONDARY_MAX,   _secondary_min + 1.0F);
      errpop = new SLErrorPop(this, "Error", "Secondary min > max");
      return True;
      }
    else if(_header_togs->IsSelected(DOTERTIARY) &&
            _tertiary_min > _tertiary_max)
      {
      _header_text->SetValue(TERTIARY_MAX,   _tertiary_min + 1.0F);
      errpop = new SLErrorPop(this, "Error", "Tertiary min > max");
      return True;
      }

    findTraces();
    showInformation();
    }

  return True;

}


void TraceSelectorPop::showInformation()
{

    if(_traces_found)
      {
      if(_domovie)
        {
        if(!_traces_in_last_frame)
          wprocVAShowMsg(_information,  "Found: %d frames of %d traces",
                         _frames_found, _nplt);
        else
          wprocVAShowMsg(_information,  
                      "Found: %d frames of %d traces, 1 Frame of %d traces",
                       _frames_found, _nplt, _traces_in_last_frame);
        }
      else
        {
        wprocVAShowMsg(_information, "Found: %d traces", _traces_found);
        }
      }
    else
      {
      wprocShowMsg(_information,"FOUND NO TRACES");     
      }  

}


long TraceSelectorPop::findTraces()
{
long primary_header = 0;
long secondary_header = 0;
long tertiary_header = 0;


  _do_abort->setNewAction();

  primary_header = _primary_header;
  if(_header_togs->IsSelected(DOSECONDARY))
    {
    secondary_header = _secondary_header;
    if(_header_togs->IsSelected(DOTERTIARY))
      tertiary_header = _tertiary_header;
    }

  _pixmap = 0;
  _ss->getTracePattern(&_nplt, &_iskp, &_ndo, &_nskp, &_frames, &_domovie);
  _frames = _frames < 1 ? 1 : _frames;


  _traces_found = _sp->findTraces(_sp->filename(),  primary_header,
                                  _primary_min,     _primary_max, 
                                  _primary_inc,     secondary_header, 
                                  _secondary_min,   _secondary_max,
                                  _secondary_inc,   tertiary_header,
                                  _tertiary_min,    _tertiary_max,
                                  _tertiary_inc,    _nplt,            
                                  _iskp,            _ndo,
                                  _nskp,            _frames,
                                  _pixmap);

  _do_abort->actionComplete();


  if(_traces_found)
    {
    _frames_found = _traces_found / _nplt;
    if(_domovie && _frames_found < 1) _frames_found = 1;
    _traces_in_last_frame = _traces_found - _frames_found * _nplt;
    }

  return _traces_found;

}

//=======================================================================
//====This is called by SeisSelect when the multiple file option has ====
//====changed the visible SeisPlot                                   ====
//=======================================================================
void TraceSelectorPop::seisPlotChanged(SeisPlot *sp)
{

  if(_sp != sp)
    {
    
    _sp = sp;

    if(!made()) return;

    _ss->getTracePattern(&_nplt, &_iskp, &_ndo, &_nskp, &_frames, &_domovie);
    
    if(_sp->usingSelector())
      {
      _sp->getSelectorParameters(&_primary_header,
                                 &_primary_min,  
                                 &_primary_max,
                                 &_primary_inc,
                                 &_secondary_header, 
                                 &_secondary_min, 
                                 &_secondary_max,
                                 &_secondary_inc,
                                 &_tertiary_header, 
                                 &_tertiary_min, 
                                 &_tertiary_max,
                                 &_tertiary_inc,
                                 &_traces_found,
                                 &_frames_found,
                                 &_traces_in_last_frame,
                                 &_dosecondary,
                                 &_dotertiary);
      _header_text->SetValue(PRIMARY_HEADER,   (long)_primary_header);
      _header_text->SetValue(SECONDARY_HEADER, (long)_secondary_header);
      _header_text->SetValue(TERTIARY_HEADER,  (long)_tertiary_header);
      _header_text->SetValue(PRIMARY_MIN,      _primary_min);
      _header_text->SetValue(SECONDARY_MIN,    _secondary_min);
      _header_text->SetValue(TERTIARY_MIN,     _tertiary_min);
      _header_text->SetValue(PRIMARY_MAX,      _primary_max);
      _header_text->SetValue(SECONDARY_MAX,    _secondary_max);
      _header_text->SetValue(TERTIARY_MAX,     _tertiary_max);
      _header_text->SetValue(PRIMARY_INC,      _primary_inc);
      _header_text->SetValue(SECONDARY_INC,    _secondary_inc);
      _header_text->SetValue(TERTIARY_INC,     _tertiary_inc);
      _header_togs->SetTog( DOSECONDARY, (Boolean)_dosecondary );
      _header_togs->SetTog( DOTERTIARY,  (Boolean)_dotertiary );

      _ss->useSelector(_primary_header,
                       _primary_min,
                       _primary_max,
                       _primary_inc,
                       _secondary_header,
                       _secondary_min,
                       _secondary_max,
                       _secondary_inc,
                       _tertiary_header,
                       _tertiary_min,
                       _tertiary_max,
                       _tertiary_inc,
                       _traces_found,
                       _frames_found,
                       _traces_in_last_frame,
                       _dosecondary,
                       _doprimary);

      }
    else
      {
      _ss->turnOffSelector();
      _header_text->SetValue(PRIMARY_HEADER,   1L);
      _header_text->SetValue(SECONDARY_HEADER, 2L);
      _header_text->SetValue(TERTIARY_HEADER,  3L);
      _header_text->SetValue(PRIMARY_MIN,   0.0F);
      _header_text->SetValue(SECONDARY_MIN, 0.0F);
      _header_text->SetValue(TERTIARY_MIN,  0.0F);
      _header_text->SetValue(PRIMARY_MAX,   0.0F);
      _header_text->SetValue(SECONDARY_MAX, 0.0F);
      _header_text->SetValue(TERTIARY_MAX,  0.0F);
      _header_text->SetValue(PRIMARY_INC,   0.0F);
      _header_text->SetValue(SECONDARY_INC, 0.0F);
      _header_text->SetValue(TERTIARY_INC,  0.0F);      
      _header_togs->SetTog( DOSECONDARY, False );
      _header_togs->SetTog( DOTERTIARY,  False );
      }

    headerTogChanged(this, 0, True);//So sensitivities get set

    showInformation();

    }
}
