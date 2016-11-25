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
// $Id: va_movie_pop.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <assert.h>
#include <Xm/Label.h>
#include "vaplots/va_movie_pop.hh"
#include "vaplots/va_plot_control.hh"
#include "vaplots/va_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_movie.hh"
#include "sl/sl_client_message.hh"
#include "sl/psuedo_widget.hh"

static String  defres[]= {
    "_popup.title:                   Movies",
    NULL};


VaMoviePop::VaMoviePop(Widget         p,
                       char          *name,
                       HelpCtx        hctx,
                       VaPlotControl *plot_ctl)
              : SLFPopSep(p,name,FP_DOREMOVE,hctx,False,False),
                SeisInform(),
                _plot_ctl(plot_ctl), _cmessage(NULL)
{
  setFallbackResources( (const char**)defres);
  addSeisPlot( _plot_ctl->semblance()->SP() );
  addSeisPlot( _plot_ctl->cmp()->SP() );
  addSeisPlot( _plot_ctl->gvs()->SP() );
  addSeisPlot( _plot_ctl->iso()->SP() );
  make(p);
}

VaMoviePop::~VaMoviePop()
{
}

Widget VaMoviePop::make(Widget p)
{
   if ( made() ) return topWidget();
   SLFPopSep::make(p);
   p= wParent();

  _sem_ctl= new SeisMovie(topWidget(), "semmovie", 
                          _plot_ctl->cmp()->SP(), NULL);
  _sem_ctl->addControl( _plot_ctl->semblance()->SP());
  _gvs_ctl= new SeisMovie(topWidget(), "gvsmovie", 
                           _plot_ctl->gvs()->SP(), NULL);
  _iso_ctl= new SeisMovie(topWidget(), "isomovie", 
                           _plot_ctl->iso()->SP(), NULL);
  _sem_ctl->useUnmapInsteadOfUnmanage(True);
  _gvs_ctl->useUnmapInsteadOfUnmanage(True);
  _iso_ctl->useUnmapInsteadOfUnmanage(True);

  XtVaSetValues( _sem_ctl->W(), XmNtopAttachment,  XmATTACH_FORM,
                               XmNleftAttachment, XmATTACH_FORM,
                               XmNtopOffset,       5,
                               XmNleftOffset,      5,
                               NULL);

  XtVaSetValues( _gvs_ctl->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                               XmNtopWidget,      _sem_ctl->W(),
                               XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,     _sem_ctl->W(),
                               NULL);

  XtVaSetValues( _iso_ctl->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                               XmNtopWidget,      _gvs_ctl->W(),
                               XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,     _sem_ctl->W(),
                               NULL);

   Widget tmp=  XtVaCreateManagedWidget( "",
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _iso_ctl->W(),
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNbottomAttachment,XmATTACH_WIDGET,
                                   XmNbottomWidget,    bottomSeparator(),
                                   XmNtopOffset,      5,
                                   XmNleftOffset,     5,
                                   NULL);


  XtVaSetValues( _sem_ctl->leftW(), XmNwidth, 25, NULL);
  XtVaSetValues( _sem_ctl->rightW(), XmNwidth, 25, NULL);
  XtVaSetValues( _sem_ctl->scaleW(), XmNwidth, 70, NULL);
  //XtVaSetValues( _sem_ctl->scaleW(), XmNscaleWidth, 70, NULL);
  wprocShowMsg(_sem_ctl->scaleW(),"Sem/CMP");

  XtVaSetValues( _gvs_ctl->leftW(), XmNwidth, 25, NULL);
  XtVaSetValues( _gvs_ctl->rightW(), XmNwidth, 25, NULL);
  XtVaSetValues( _gvs_ctl->scaleW(), XmNwidth, 70, NULL);
  //XtVaSetValues( _gvs_ctl->scaleW(), XmNscaleWidth, 70,NULL);
  wprocShowMsg(_gvs_ctl->scaleW(),"GVS");

  XtVaSetValues( _iso_ctl->leftW(), XmNwidth, 25, NULL);
  XtVaSetValues( _iso_ctl->rightW(), XmNwidth, 25, NULL);
  XtVaSetValues( _iso_ctl->scaleW(), XmNwidth, 70, NULL);
  //XtVaSetValues( _iso_ctl->scaleW(), XmNscaleWidth, 70,NULL);
  wprocShowMsg(_iso_ctl->scaleW(),"ISO Vel.");

  _sem_ctl->unmanage();
  _gvs_ctl->unmanage();
  _iso_ctl->unmanage();

  return topWidget();

}

void VaMoviePop::newPlot(SeisPlot *)
{
  if (!_cmessage) {
      _cmessage= new SLClientMessage(pW()->anyW(), "aname" );
      _cmessage->setComplexNotify(this);
  }
}

void VaMoviePop::noPlotDisplayed(SeisPlot *)
{
  if (!_cmessage) {
      _cmessage= new SLClientMessage(pW()->anyW(), "aname" );
      _cmessage->setComplexNotify(this);
  }
}

void VaMoviePop::postMovie(SeisPlot *, SeisPlot::MovieDir )
{
}

void VaMoviePop::notCurrentInWindow(SeisPlot *)
{
}

Boolean VaMoviePop::notifyComplex(SLDelay *obj, int )
{
  if (obj == _cmessage) {
       showMovieControl();
       _cmessage= NULL;
  }
  return True;
}

void VaMoviePop::showMovieControl()
{
  Boolean manage_it=  False;
  Boolean manage_sem= False;
  Boolean manage_gvs= False;
  Boolean manage_iso= False;
  if (  _plot_ctl->semblance()->SP()->plottedFrames() > 1 ) {
            manage_sem= True;
            manage_it=  True;
  }
  if (  _plot_ctl->cmp()->SP()->plottedFrames() > 1 ) {
            manage_sem= True;
            manage_it=  True;
  }
  if (  _plot_ctl->gvs()->SP()->plottedFrames() > 1 ) {
            manage_gvs = True;
            manage_it=   True;
  }
  if (  _plot_ctl->iso()->SP()->plottedFrames() > 1 ) {
            manage_iso= True;
            manage_it=  True;
  }
  if (manage_it) {
        Position  x,y;
        Dimension wid,my_width;
        if ( !made() ||
             !XtIsManaged(W()) ) {
              makeAndManage();
              Widget top= get_toplevel_shell(W());
              XtVaGetValues(top, XmNwidth,  &wid,
                           XmNx,      &x,
                           XmNy,      &y, NULL);
              XtVaGetValues(W(), XmNwidth,  &my_width, NULL);

              Widget shell= get_shell_widget(W());
              XMoveWindow(XtDisplay(shell), XtWindow(shell), 
                             x+wid-my_width,y+30);
        }
  }
  else          
        unmanage();

  if (manage_sem) _sem_ctl->manage();
  if (manage_gvs) _gvs_ctl->manage();
  if (manage_iso) _iso_ctl->manage();

}
