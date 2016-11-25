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
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include "sp/seis_anno_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"
#include "sl/shell_watch.hh"
#include "sl/psuedo_widget.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_radio_box.hh"





static String  defres[]= {
     "_popup.title:               Annotation",
     "*ptlL.labelString:          Prim Timing Lines:",
     "*stlL.labelString:          \\ \\ \\ Sec Timing Lines:",
     "*start_traceL.labelString:  Starting Trace:",
     "*trace_incL.labelString:    \\ \\ \\ Trace Increment:",
     "*primhL.labelString:        First Header:",
     "*sechL.labelString:         \\ \\ \\ Second Header:",
     "*trace_incL.labelString:    \\ \\ \\ Trace Increment:",
     "*titleL.labelString:        Plot Title:",
     "*titleL.topPosition:        5",
     "*titleL.leftPosition:       2",
     "*anno_text.leftPosition:    2",
     "*anno_text.numColumns:      3",
     "*anno_text.packing:         PACK_COLUMN",
     "*ptl.value:                  1.0",
     "*stl.value:                  .2",
     "*start_trace.value:          1",
     "*trace_inc.value:            20",
     "*primh.value:                1",
     "*sech.value:                 37",
     "*time.set:                   True",
     "*time.labelString:           Annotate As Time",
     "*depth.labelString:          Annotate As Depth",
    NULL};





SeisAnnoPop::SeisAnnoPop( SLDelay       *contain,
                          char          *name,
                          HelpCtx       hctx,
                          SeisPlot      *sp,
                          SeisPlot      *info_sp)
              : SLFPopSep(contain,name,FP_DOALL,hctx,False), SeisInform(sp),
                _sp(sp), 
                _info_sp(info_sp), _will_plot(False), _first_time(True)
{
  setDefaultResources( (contain->pW())->display(), name, defres);
  init();
}

SeisAnnoPop::SeisAnnoPop( Widget    p,
                        char      *name,
                        HelpCtx   hctx,
                        SeisPlot  *sp,
                        SeisPlot  *info_sp,
                        Boolean   make_now)
              : SLFPopSep(p,name,FP_DOALL,hctx,False,make_now), _sp(sp), 
                _info_sp(info_sp)
{
 setDefaultResources( p, name, defres);
 init();
 if (make_now) make(p);
}


void SeisAnnoPop::init()
{
static SLText texts[]  = {
   { "ptl",       "range:0.001 *,default:1.000", NULL, SLType_float,   PTL },
   { "stl",       "range:0.001 *,default:1.000", NULL, SLType_float,   STL },
   { "start_trace",NULL,               NULL, SLType_int, START_TRACE },
   { "trace_inc",  NULL,               NULL, SLType_int, TRACE_INC },
   { "primh",   "range:1 99999999,default:1",NULL, SLType_int, PRIMH },
   { "sech",    "range:0 99999999,default:1",NULL, SLType_int, SECH },
         };
texts[0].target= &_ptl;
texts[1].target= &_stl;
texts[2].target= &_start_trace; 
texts[3].target= &_trace_inc;
texts[4].target= &_primh; 
texts[5].target= &_sech; 

static SLRadio trads[]  = {
         { "time",  SeisAnnoPop::TIME },
         { "depth", SeisAnnoPop::DEPTH },
       };

  _anno_text= new SLTextBox( this,"anno_text",getHelpCtx(), 
                             texts,XtNumber(texts));
  _plot_title= _pw_topw->childTextDef("title"); 

  
  _anno_type= new SLRadioBox( this, "anno_type", getHelpCtx(),
                              trads, XtNumber(trads), NULL, True );
}

Widget SeisAnnoPop::make(Widget p)
{
   Widget lab;

   if ( made() ) return topWidget();

   p= p ? p : wParent();

   ShellStatMsg  bld_info(p,"Building Annotation Popup...");

   SLFPopSep::make(p);


   lab= XtVaCreateManagedWidget("titleL", xmLabelWidgetClass, topWidget(), 
                                XmNtopAttachment,  XmATTACH_POSITION,
                                XmNleftAttachment, XmATTACH_POSITION, NULL);

   _titlew= XtVaCreateManagedWidget("title", xmTextWidgetClass, topWidget(), 
                                XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                XmNleftAttachment,  XmATTACH_WIDGET,
                                XmNrightAttachment, XmATTACH_FORM,
                                XmNrightOffset,     5,
                                XmNtopWidget,      lab,
                                XmNleftWidget,     lab, NULL);


   XtVaSetValues(_anno_text->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                                  XmNtopWidget,      lab,
                                  XmNtopOffset,      15,
                                  XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                                  XmNleftWidget,     lab,
                                  NULL);

   Widget tmp1=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                               XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,     _anno_text->W(),
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNrightOffset,       5,
                               XmNleftAttachment, XmATTACH_WIDGET,
                               XmNleftWidget,     _anno_text->W(),
                               XmNleftOffset,         5,
                               NULL);
   wprocShowMsg(tmp1,"");

   XtVaSetValues(_anno_type->W(), XmNtopAttachment,    XmATTACH_WIDGET,
                                  XmNtopWidget,        _anno_text->W(),
                                  XmNtopOffset,        15,
                                  XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                  XmNleftWidget,       _anno_text->W(),
                                  NULL);

   Widget tmp2= XtVaCreateManagedWidget( "", 
                                         xmLabelWidgetClass, topWidget(),
                                         XmNtopAttachment, XmATTACH_WIDGET,
                                         XmNtopWidget,     _anno_type->W(),
                                         XmNbottomAttachment, XmATTACH_WIDGET,
                                         XmNbottomWidget,    bottomSeparator(),
                                         XmNbottomOffset,    20,
                                         XmNleftAttachment,  XmATTACH_FORM,
                                         XmNleftOffset,       5,
                                         XmNtopOffset,        5,
                                         NULL);

   wprocShowMsg(tmp2,"");



   setFromOther();
   DoAction();
   ValidInput();

   return topWidget();
}


SeisAnnoPop::~SeisAnnoPop()
{
   delete _anno_text;
   if (_plot_title) free(_plot_title);
}


void SeisAnnoPop::setFromOther(SeisPlot *othersp)
{
 SeisPlot *sp= othersp ? othersp : _info_sp;
 char *tmp, *ns;


 if (sp) {
     _anno_text->SetValue(PTL, (float)sp->primTimingLine() );
     _anno_text->SetValue(STL, (float)sp->secTimingLine() );
     _anno_text->SetValue(START_TRACE, sp->firstLbl() );
     _anno_text->SetValue(TRACE_INC, sp->lblInc() );
     _anno_text->SetValue(PRIMH, sp->header1() );
     _anno_text->SetValue(SECH,  sp->header2() );

     if (sp->depth()) _anno_type->SetRadio(DEPTH);
     else             _anno_type->SetRadio(TIME);

     if (made()) {
          tmp= sp->plotLabel();
	  ns = "";
          wprocShowMsg(_titlew, tmp ? tmp : ns);
     }
 } // End if

}

void SeisAnnoPop::manage()
{
  SLBase::manage();
  
  if (!_first_time) setFromOther(_sp);
  else              _first_time= False;
}


Boolean SeisAnnoPop::ValidInput()
{
 Boolean stat= True;

 if (stat) stat= _anno_text->validate();
 return stat;
}

void SeisAnnoPop::applyParams()
{
char *tmp;

 tmp= wprocGetMsg(_titlew);
 _sp->setTimingLines(_ptl, _stl);
 _sp->setLabeling(_start_trace, _trace_inc);
 _sp->setHeaders((int)_primh, (int)_sech);
 _sp->setPlotLabel((char *)tmp); 
 if (_anno_type->WhichSelected() == DEPTH)
   _sp->setDepth(True);
 else
   _sp->setDepth(False);

  
 if(tmp)
   XtFree(tmp);
}

void SeisAnnoPop::DoAction() 
{ 
  
 applyParams();


 if (_will_plot) {
       ShellWatch sw;
       _sp->plot();
 } // End if
}

void SeisAnnoPop::notCurrentInWindow(SeisPlot *sp)
{
   SeisPlot *oldsp= _sp;
   if (_sp == sp) {
       _sp= _sp->currentSPInWindow();
       if (!find(_sp)) {
           addSeisPlot(_sp);
           setFromOther(oldsp);
           applyParams();
       }
       else if (XtIsManaged(W())) {
           setFromOther(oldsp);
       }
   }
}

void SeisAnnoPop::setSeisPlot(SeisPlot *sp)
{
  _sp = sp;
}
 



void SeisAnnoPop::UndoInput(){}

void SeisAnnoPop::reloadDefaults(Boolean)
{
 _anno_text->reloadDefaults();
 wprocShowMsg(_titlew, _plot_title);
}

void SeisAnnoPop::reloadSystemDefaults(Boolean)
{
  wprocShowMsg(_titlew, "");
  _anno_text->SetValue(PTL, (float)1.0 );
  _anno_text->SetValue(STL, (float)0.5 );
  _anno_text->SetValue(START_TRACE, 1L );
  _anno_text->SetValue(TRACE_INC, 20L );
  _anno_text->SetValue(PRIMH, 1L );
  _anno_text->SetValue(SECH,  1L );
}

void SeisAnnoPop::turnOffTimeDepthOption()
{
  _anno_type->SetRadio(TIME);
  XtVaSetValues(_anno_type->W(), XmNmappedWhenManaged, False, NULL);
}

void SeisAnnoPop::setPrimaryTimingLine(float ptl)
{
  _anno_text->SetValue(PTL, ptl );
}

void SeisAnnoPop::setSecondaryTimingLine(float stl)
{
  _anno_text->SetValue(STL, stl );
}

void SeisAnnoPop::setStartTrace(long t)
{
  _anno_text->SetValue(START_TRACE, t );
}

void SeisAnnoPop::setTraceIncrement(long i)
{
  _anno_text->SetValue(TRACE_INC, i );
}

void SeisAnnoPop::setPrimaryHeader(long h)
{
  _anno_text->SetValue(PRIMH, h );
}

void SeisAnnoPop::setSecondaryHeader(long h)
{
  _anno_text->SetValue(SECH, h );
}

void SeisAnnoPop::setTextState(int which, Boolean state)
{
  _anno_text->SetSensitive(which, state);
}

void SeisAnnoPop::setTimeDepthState(Boolean state)
{
  XtSetSensitive(_anno_type->GetRadioWidget(TIME),  state);
  XtSetSensitive(_anno_type->GetRadioWidget(DEPTH), state);
}

void SeisAnnoPop::setDepthOption(Boolean set)
{
  if(set)
     _anno_type->SetRadio(DEPTH);
  else
     _anno_type->SetRadio(TIME);
}

Boolean SeisAnnoPop::getDepthOption()
{
  if(_anno_type->WhichSelected() == DEPTH)
    return True;
  else
    return False;
}
