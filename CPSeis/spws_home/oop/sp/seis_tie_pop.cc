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
#include "sp/seis_plot.hh"
#include "sp/seis_tie_pop.hh"
#include "sp/seis_plot_tie.hh"
#include "sp/trace_selector_pop.hh"
#include "sp/trace_selector_ndo_pop.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_push_box.hh"

static String  defres[]= {
    "_popup.title:             Tie Line Plot",
    "*nomovie.labelString:     No Movie",
    "*tiemovie.labelString:    Tie Dual Movie",
    "*sepmovie.labelString:    Tie Indepdent Movie",
    "*tieframesL.labelString:  Frames:",
    "*tieframes.columns:       4",
    "*nomovie.set:             True",
    NULL};



class MovieRadioBox : public SLRadioBox {
protected: 
   SeisTiePop *_stp;
   virtual void ChoiceAction( long button);
public:
   MovieRadioBox( SLDelay          *contain,
                  char             *name,
                  HelpCtx          hctx,
                  SLRadioAry       togary,
                  unsigned int     arycnt,
                  long             *target =NULL, 
                  Boolean          doframe =False,
                  Boolean          dotitle =False,
                  Boolean          make_if_can =True ) :
           SLRadioBox(contain, name, hctx, togary, arycnt, target, doframe,
                      dotitle, make_if_can) {}
   void setStp(SeisTiePop *stp) { _stp= stp;}
};




class MovieTextBox : public SLTextBox {
protected: 
   SeisTiePop *_stp;
   virtual void TextAction( long ident);
public:
   MovieTextBox( SLDelay      *contain,
                 char         *name,
                 HelpCtx      hctx,
                 SLTextAry    textary,
                 unsigned int arycnt,
                 Boolean      DoLabel    =True,
                 long         number_col =1,
                 Boolean      doframe    =False,
                 Boolean      DoTitles   =False,
                 Boolean      make_if_can=True) :
       SLTextBox(contain, name, hctx, textary, arycnt, DoLabel, number_col,
                    doframe, DoTitles, make_if_can) {}

   void setStp(SeisTiePop *stp) { _stp= stp;}
};



enum { FRAMES, NOMOVIE, TIEMOVIE, SEPMOVIE };


SeisTiePop::SeisTiePop( Widget       p,
                        char         *name,
                        HelpCtx      hctx,
                        SeisPlot     *sp,
                        SeisPlotTie  *primsp,
                        Boolean   allow_selector) :
              SeisSelect(p,name,hctx,sp,primsp,allow_selector), _primsp(primsp)
{

static SLText movie_text[]  = {
 { "tieframes",   NULL,  NULL,  SLType_int, FRAMES },
};
movie_text[0].target= &_frames;

static SLRadio rads[]  = {
    { "nomovie",  NOMOVIE  },
    { "tiemovie", TIEMOVIE },
    { "sepmovie", SEPMOVIE },
};

  setDefaultResources( p, name, defres);

  _movie_form= new SLForm(this,"movie_form",hctx,True);
  _which_movie= new MovieRadioBox(_movie_form, "which_movie", getHelpCtx(), 
                                  rads, XtNumber(rads) );
  _movie= new MovieTextBox( _movie_form,"movie",hctx,
                            movie_text,XtNumber(movie_text), True);

  _which_movie->setStp(this);
  _movie->setStp(this);
  addSeisPlot(_primsp);
}
                 




Widget SeisTiePop::make(Widget p)
{
   if ( made() ) return topWidget();

   p= p ? p : wParent();

   ShellStatMsg  bld_info(p,"Building Tie Line Popup...");

   SeisSelect::make(p);

   if (_allow_selector) {
     _selector_pop = new TraceSelectorPop(topWidget(), "Selector",
       getHelpCtx(), this);
     _selector_ndo_pop = new TraceSelectorNDoPop (topWidget(), "Selector",
       getHelpCtx(), this);
   }

   unmangeIS();
   unmangeMAX();

   XtVaSetValues( _movie_form->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                                    XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                    XmNtopOffset,      10,
                                    XmNtopWidget,      _but->W(),
                                    XmNleftWidget,     _but->W(), NULL );

   XtVaSetValues( _movie->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,       _which_movie->W(),
                               XmNleftWidget,      _which_movie->W(), NULL );

   XtVaSetValues( _but->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                             XmNtopWidget,       _traceparams->W(),
                             XmNtopOffset,      10,
                             XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                             XmNleftWidget,     _traceparams->W(),
                             XmNnumColumns,     4, NULL );


   return topWidget();
}






SeisTiePop::~SeisTiePop()
{
 delete _which_movie;
 delete _movie;
 delete _movie_form;
}


void SeisTiePop::manage()
{
 _movie->SetValue(FRAMES, _sp->frames() );

 long frames;

 Widget w= _which_movie->GetRadioWidget(TIEMOVIE);
 XtSetSensitive(w, _primsp->movie());
 if ( (!_primsp->movie()) && (_which_movie->WhichSelected() == TIEMOVIE) ) {
         _which_movie->SetRadio(NOMOVIE);
 }
 if ( (_primsp->movie()) && (_which_movie->WhichSelected() == NOMOVIE) ) {
         _which_movie->SetRadio(TIEMOVIE);
 }

 if (_which_movie->WhichSelected() == NOMOVIE) {
   _movie->clear(FRAMES);
   frames = 0;
 }
 else {
   frames = _sp->frames ();
 }
 _selector_ndo_pop->checkMovieState (_which_movie->WhichSelected()
   != NOMOVIE, frames);

 SeisSelect::manage();
}



Boolean SeisTiePop::ValidInput()
{
  Boolean stat= True;
  if (stat) stat= SeisSelect::ValidInput();
  if (stat) stat= _movie->validate();
  return stat;
}



void SeisTiePop::DoAction()
{
  _sp->setFrames( _frames);
  if (_which_movie->WhichSelected() == NOMOVIE)
       _sp->setMovie(False); 
  else
       _sp->setMovie(True); 
   _is= _primsp->is();

  if (!_primsp->doingTie() ) _primsp->addTie(_sp);

  SeisSelect::DoAction();
}


void SeisTiePop::reloadDefaults(Boolean do_method)
{
   SeisSelect::reloadDefaults(do_method);
   _movie->reloadDefaults();
   _which_movie->reloadDefaults();
}

void SeisTiePop::reloadSystemDefaults(Boolean do_method)
{
 SeisSelect::reloadSystemDefaults(do_method);
 _which_movie->SetRadio(NOMOVIE);
 _movie->SetValue(FRAMES,  10L);
}


void SeisTiePop::UndoInput()
{
 SeisSelect::UndoInput();
}

void SeisTiePop::notCurrentInWindow(SeisPlot *sp)
{
  if (sp == _primsp) {
      _primsp= (SeisPlotTie*)_primsp->currentSPInWindow();
      addSeisPlot(_primsp);
  }
  else {
     SeisSelect::notCurrentInWindow(sp);
  }

}

void SeisTiePop::getTracePattern (long *nplt, long *iskp, long *ndo,
 long *nskp, long *frames, long *domovie)
{
  *nplt    = _nplt;
  *iskp    = _iskp;
  *ndo     = _ndo;
  *nskp    = _nskp;
  *frames  = _frames;
  *domovie = _which_movie->WhichSelected() != NOMOVIE;
}

void SeisTiePop::setNumMovieFrames (long frames)
{
  _movie->SetValue (FRAMES, frames); // set _frames
}

void SeisTiePop::setMovie (Boolean domovie)
{
  if (domovie) {
    if (_which_movie->WhichSelected() == NOMOVIE) {
      // don't change unless no movie selected
      _which_movie->SetRadio (TIEMOVIE); // default, user can always change it
    }
  }
  else {
    // turn off the movie
    _which_movie->SetRadio (NOMOVIE);
  }
}

void MovieRadioBox::ChoiceAction( long button)
{
  long frames;

  switch (button) {
     case NOMOVIE:
		  frames = 0;
                  _stp->_movie->clear (FRAMES);
                  break;
     case TIEMOVIE:
                  frames = _stp->_primsp->frames ();
                  _stp->_movie->SetValue (FRAMES, frames);
                  break;
     case SEPMOVIE:
                  frames = _stp->_sp->frames ();
                  _stp->_movie->SetValue (FRAMES, frames);
                  break;
  }
  _stp->_selector_ndo_pop->checkMovieState (button != NOMOVIE, frames);
}

void MovieTextBox::TextAction( long ident)
{
  long num_frames= GetInt((int)ident);
  long frames;
  
  if (num_frames == 1) {
    _stp->_which_movie->SetRadio( NOMOVIE);
    frames = 0;
  }
  else {
    _stp->_which_movie->SetRadio( SEPMOVIE);
    frames = num_frames;
  }
  _stp->_selector_ndo_pop->checkMovieState (
    _stp->_which_movie->WhichSelected() != NOMOVIE,
    frames);
}
