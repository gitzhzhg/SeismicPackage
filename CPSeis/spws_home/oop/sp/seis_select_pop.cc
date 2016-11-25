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
#include "sp/seis_select_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/trace_selector_pop.hh"
#include "sp/trace_selector_ndo_pop.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_form.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_push_box.hh"

//    ".height:                  670",
//    ".width:                   520",

static String  defres[]= {
    "*framesL.labelString:      Frames:",
    "*skip_framesL.labelString: Skip Frames:",
    "*domovie.labelString:      Movie Number Frames:",
    "*frames.columns:           4",
    "*movie_form_Frame.leftOffset: -10",
    NULL};


enum { FRAMES, SFRAMES, DOMOVIE };



SeisSelectPop::SeisSelectPop( Widget    p,
                              char      *name,
                              HelpCtx   hctx,
                              SeisPlot  *sp,
                              SeisPlot  *info_sp,
                              Boolean   allow_selector) :
              SeisSelect(p,name,hctx,sp,info_sp,allow_selector)
{

static SLText movie_text[]  = {
 { "frames",        NULL,  NULL,       SLType_int,   FRAMES },
};
movie_text[0].target=&_frames;

static SLTog mtogs[]  = {
    { "domovie", NULL,   DOMOVIE },
};
mtogs[0].target=&_domovie;

  setDefaultResources( p, name, defres);

  _movie_form= new SLForm(this,"movie_form",hctx,True);
  _ifmovie= new SLTogBox(_movie_form, "ifmovie", getHelpCtx(), 
                         mtogs,XtNumber(mtogs));
  _ifmovie->setAltChoiceAction(togChanged, (void*)this);
  _movie= new SLTextBox( _movie_form,"movie",hctx,
                         movie_text,XtNumber(movie_text), False);
  _movie->setAltLosingAction(movieChanged, (void*)this);
}
                 




Widget SeisSelectPop::make(Widget p)
{
   if ( made() ) return topWidget();

   p= p ? p : wParent();

   ShellStatMsg  bld_info(p,"Building Setup/Change plot Popup...");

   SeisSelect::make(p);

   if (_allow_selector) {
     _selector_pop = new TraceSelectorPop (topWidget(), "Selector",
       getHelpCtx(), this);
     _selector_ndo_pop = new TraceSelectorNDoPop (topWidget(), "Selector",
       getHelpCtx(), this);
     updateSelectors ();
   }


   XtVaSetValues( _movie_form->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                                    XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                    XmNtopWidget,      _but->W(),
                                    XmNleftWidget,     _but->W(), 
                                    XmNleftOffset,     4, 
                                    XmNrightAttachment, XmATTACH_FORM,
                                    XmNrightOffset,    10, NULL );

   XtVaSetValues( _movie->W(), XmNleftAttachment,  XmATTACH_WIDGET,
                               XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,       _ifmovie->W(),
                               XmNleftWidget,      _ifmovie->W(), NULL );
   return topWidget();
}


void SeisSelectPop::togChanged(void *data, long, Boolean set)
{
   SeisSelectPop *obj= (SeisSelectPop*)data;
   if (set) {
       if (obj->_frames == 0)
           obj->_movie->SetValue(FRAMES, obj->_sav_frames);
   }
   else {
       obj->_sav_frames= obj->_frames;
       obj->_movie->SetValue(FRAMES, 0L);
       obj->_movie->clear(FRAMES);
   }
   obj->_selector_ndo_pop->checkMovieState (obj->_domovie, obj->_frames);
}


void SeisSelectPop::movieChanged(void *data, long)
{
   SeisSelectPop *obj= (SeisSelectPop*)data;
   if (obj->_frames != 0) 
         obj->_ifmovie->SetTog( DOMOVIE, True);
   else
         obj->_ifmovie->SetTog( DOMOVIE, False);

   obj->_selector_ndo_pop->checkMovieState (obj->_domovie, obj->_frames);
}

SeisSelectPop::~SeisSelectPop()
{
 delete _movie;
 delete _ifmovie;
 delete _movie_form;
}


void SeisSelectPop::manage()
{

 _movie->SetValue(FRAMES,  _sp->frames()     );
 if (!_domovie) {
       _movie->clear(FRAMES);
 }
 if (_allow_selector) {
   _selector_ndo_pop->checkMovieState (_domovie, _sp->frames());
 }
// _movie->SetValue(SFRAMES, _sp->skipFrames() );
 SeisSelect::manage();
}



Boolean SeisSelectPop::ValidInput()
{
  Boolean stat= True;
  if (stat) stat= SeisSelect::ValidInput();
  if (stat) stat= _movie->validate();
  return stat;
}



void SeisSelectPop::DoAction()
{
  _sp->setFrames( _frames);
  if (_frames > 0) 
         _sp->setMovie((Boolean)_domovie);
  else {
         _sp->setMovie(False);
         _ifmovie->SetTog( DOMOVIE, False);
  }

  SeisSelect::DoAction();
}


void SeisSelectPop::reloadDefaults(Boolean do_method)
{
   SeisSelect::reloadDefaults(do_method);
   _movie->reloadDefaults();
}

void SeisSelectPop::reloadSystemDefaults(Boolean do_method)
{
   SeisSelect::reloadSystemDefaults(do_method);
   _movie->SetValue(FRAMES,  10L);
   _movie->SetValue(SFRAMES, 0L );
}


void SeisSelectPop::UndoInput()
{
 SeisSelect::UndoInput();
}

void SeisSelectPop::setNumMovieFrames(long num_frames)
{
  _movie->SetValue(FRAMES, num_frames);
}

void SeisSelectPop::setMovie (Boolean domovie)
{
  _ifmovie->SetTog (DOMOVIE, domovie);
}
