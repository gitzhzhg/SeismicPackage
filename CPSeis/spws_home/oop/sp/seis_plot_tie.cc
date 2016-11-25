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
#include <Xm/ScrolledW.h>
#include <assert.h>
#include "sp/seis_plot_tie.hh"
#include "sp/inform_list.hh"
#include "sp/seis_inform.hh"
#include "sp/seis_zoomer.hh"
#include "sp/seis_scrwin.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_hard_copy.hh"
#include "sl/view_win.hh"
#include "sl/shell_watch.hh"
#include "sl/error_handler.hh"
#include "hardcopy/paper_plot.hh"
#include "hardcopy/hardcopy_plot.hh"


static String  defres[]= {
     "*XmSeparator.background: red",
    NULL};


class TieInform : public SeisInform {
  private:
   SeisPlotTie *_spt;
  public:
   TieInform( SeisPlotTie *spt, SeisPlot *sp =NULL) 
                      : SeisInform(sp), _spt(spt){};
   virtual void expose(SeisPlot *sp, int x, int y, int width, int height);
   virtual void notCurrentInWindow(SeisPlot *);
};

enum { Primary_moving, Tie_moving, Nobody_moving};


class TieInformSeisZoomer : public SeisZoomer {
 private:
    SeisPlotTie *_primsp;
    SeisPlot    *_tiesp;
 public:
    TieInformSeisZoomer( SeisPlotTie  *primsp,
                      SeisPlot     *tiesp = NULL,
                      float        zoom_factor = 2.0,
                      int          zoom_type     = SpPointer) 
                         : SeisZoomer(primsp,NULL,zoom_factor,zoom_type), 
                           _primsp(primsp), _tiesp(tiesp) 
                           {if (tiesp) setTiePlot(tiesp); }
    virtual void zoomComplete( SeisPlot *sp, SeisPlot *zsp,
                               SeisPlot::ZoomDir direction,int _zoom_stat);
};



void TieInformSeisZoomer::zoomComplete( SeisPlot *, 
                                        SeisPlot *,
                                        SeisPlot::ZoomDir direction,
                                        int               stat) 
{
  _primsp->getSeisWinMan()->unlock();
  if (_tiesp) _tiesp->getSeisWinMan()->unlock();
  if (stat !=ZoomerFail) {
        _primsp->_inform_list->callPostZoom(_primsp, direction);
        _tiesp->_inform_list->callPostZoom(_tiesp, direction);
        _primsp->addPixmaps();
  }
  else {
        Boolean save_show_warnings= _primsp->_show_warnings;
        _primsp->_inform_list->callPostZoom(_primsp, SeisPlot::Abort);
        _tiesp->_inform_list->callPostZoom(_tiesp, SeisPlot::Abort);
        _primsp->_show_warnings= True;
        _primsp->deliverError("Zoom failed- attempting to replot.",
                         ErrorHandler::Error);
        _primsp->_show_warnings= save_show_warnings;
  }
}



SeisPlotTie::SeisPlotTie( const Widget  p,
                          const char    *name,
			  const char    *colorset_name,
                          const int     ptype) :
          SeisPlot(), _lock_pt(0), 
          _whose_moving(Nobody_moving), _tiesp(NULL),
          _header_graph(NULL)
{
 
    setDefRes(XtDisplay(p), (char*)name, defres);

    _widget_manager= new SeisWinMan(this, p, name);
    WinCtl *winctl= _widget_manager->getWinCtl();
    constructor(winctl->W(),name,ptype,True);

    _primsw= _widget_manager->getPrimViewWin();
    _primsw->setOrder(0,1);
    winctl->setMajor(RowMajor);

    _inform= new TieInform(this, this);

    _prim_vsb= _widget_manager->scrolledWindow()->vertSB();
    addAllCallbacks(_prim_vsb);
    _old_prim_sb_value= 0;
}

SeisPlotTie::SeisPlotTie( SeisPlotTie *sp) :
          SeisPlot(sp), _lock_pt(0), 
          _whose_moving(Nobody_moving),
          _header_graph(NULL)
{

    _primsw= _widget_manager->getPrimViewWin();
    _inform= new TieInform(this, this);
    _prim_vsb= _widget_manager->scrolledWindow()->vertSB();
    _old_prim_sb_value= sp->_old_prim_sb_value;
    _tiesp= sp->_tiesp;
    _header_graph= sp->_header_graph;
    if (_tiesp) _inform->addSeisPlot( _tiesp );
}

SeisPlotTie::~SeisPlotTie()
{
  /*
   *  I am about to delete the inform.  It will be delete before
   *  notCurrentInWindow can be called.  Therefore call setSPNotCurrent
   *  to force the notCurrentInWindow then delete the inform.
   */
  getSeisWinMan()->setSPNotCurrent(this);
  delete _inform;
}

SeisPlot& SeisPlotTie::operator=(SeisPlot& sp)
{
 SeisPlot::operator=(sp);
 return *this;
}

void SeisPlotTie::showit()
{
    _primsw->show();
}

void SeisPlotTie::addTie(SeisPlot *tiesp)
{
  if (doingTie()) {
      printf(
         "SeisPlotTie::addTie: attempt to add tieline twice.\n");
      return;
  } 
  _widget_manager->scrolledWindow()->setVertPlacement( SLScrollWin::Left);
  if (_header_graph)  {
         SeisScrWin  *head_scrwin= 
                       _header_graph->_widget_manager->scrolledWindow();
        head_scrwin->setVertPlacement(SLScrollWin::Left);
        _header_graph->showBorders(True, False, True, False);
  }
  _tiesp= tiesp;
  _inform->addSeisPlot( _tiesp );
  _lock_pt= 0;
  ViewWin *tiesw=  _widget_manager->addTieViewWin(tiesp->W());
  tiesw->setOrder(1,1);
  tiesw->show();
  _inform_list->callAddingTie(this, tiesp);

  addAllCallbacks(tiesp->_widget_manager->scrolledWindow()->vertSB());
  _old_prim_sb_value= 0;
  tiesp->shareColorsWith(this);
  showBorders(True, False, True, False);
  _tiesp->showBorders(False, True, True, False);
  displayTimeOffset();
}

void SeisPlotTie::delTie()
{
  if (!doingTie()) {
      printf("SeisPlotTie::delTie: no tieline to delete.\n");
      return;
  } 
  _widget_manager->scrolledWindow()->setVertPlacement( SLScrollWin::Right);
  if (_header_graph) {
         _header_graph->_widget_manager->scrolledWindow()->setVertPlacement(
                                                           SLScrollWin::Right);
         _header_graph->showBorders(True, True, True, False);
  }
  _widget_manager->getTieViewWin()->hide();
  _widget_manager->deleteTieViewWin();
  _tiesp->cleanup();
  Widget vsb, hsb;
  XtVaGetValues(_primsw->W(), XmNverticalScrollBar,   &vsb,
                              XmNhorizontalScrollBar, &hsb, NULL );
  _inform_list->callRemovingTie(this, _tiesp);
  
  _inform->delSeisPlot(_tiesp);
  removeAllCallbacks(_tiesp->_widget_manager->scrolledWindow()->vertSB());
  _tiesp= NULL;
  showBorders(True, True, True, False);
}

Widget SeisPlotTie::W()
{
  return _widget_manager->getWinCtl()->W();
}


void SeisPlotTie::addHeaderGraph(SeisPlot *header_graph)
{
  if (doingHeader()) {
      printf(
         "SeisPlotTie::addHeaderGraph: attempt to add header graph twice.\n");
      return;
  } 
  SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
  SeisScrWin  *head_scrwin= header_graph->_widget_manager->scrolledWindow();
  _header_graph= header_graph;
  ViewWin *header_sw=  _widget_manager->addHeadViewWin(_header_graph->W());
  header_sw->setOrder(0,0);
  header_sw->setReductionPercent(60);
  header_sw->show();
  head_scrwin->slaveHorSBTo(scrwin);
  scrwin->alwaysShowVerticalSB(True);
  head_scrwin->alwaysShowVerticalSB(True);
  scrwin->alwaysShowVerticalSB(True);
  if (_tiesp) {
         head_scrwin->setVertPlacement(SLScrollWin::Left);
         _header_graph->showBorders(True, False, True, False);
  }
}


void SeisPlotTie::delHeaderGraph()
{
  if (!doingHeader()) {
      return;
  } 
  SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
  SeisScrWin  *head_scrwin= _header_graph->_widget_manager->scrolledWindow();
  _widget_manager->getHeadViewWin()->hide();
  _header_graph->showBorders(True, True, True, False);
  head_scrwin->setVertPlacement(SLScrollWin::Right);
  _widget_manager->deleteHeadViewWin(); 
  _header_graph->cleanup();
  scrwin->alwaysShowVerticalSB(False);
  head_scrwin->alwaysShowVerticalSB(False);
  _header_graph= NULL;
}


Boolean SeisPlotTie::doingTie() 
{ 
   void *x;
   SeisPlot *sp;
   SeisWinMan *swm= getSeisWinMan();
   Boolean retval= False;
   for(sp= swm->top(&x); (sp && !retval); sp= swm->next(&x))
               retval= ((SeisPlotTie*)sp)->_tiesp ? True : False;
   return retval;
}

Boolean SeisPlotTie::doingHeader()
{
   void *x;
   SeisPlot *sp;
   SeisWinMan *swm= getSeisWinMan();
   Boolean retval= False;
   for(sp= swm->top(&x); (sp && !retval); sp= swm->next(&x))
               retval= ((SeisPlotTie*)sp)->_header_graph ? True : False;
   return retval;
}

void SeisPlotTie::showBar(Boolean bar )
{
 WinCtl *winctl= _widget_manager->getWinCtl();
 if (bar) winctl->showBars();
 else     winctl->hideBars();
}

void SeisPlotTie::scrollBarsLocked(Boolean lock)
                        { _widget_manager->setLockScrollBar(lock); }
void SeisPlotTie::addOutputWidget(Widget w)
                        { _widget_manager->addOutputWidget(w);}
void SeisPlotTie::delOutputWidget(Widget w)
                        { _widget_manager->delOutputWidget(w);}
void SeisPlotTie::displayTimeOffset()
{
 Widget w;
 Position gy, ty;
 float offset;
 Wlist *time_list= _widget_manager->getTimeList();

 if (_tiesp) {
    XtVaGetValues( imageGraphic(), XmNy, &gy, NULL);
    XtVaGetValues( _tiesp->imageGraphic(), XmNy, &ty, NULL);
    offset= (gy - ty - _lock_pt) * yperPix();

    for(w= time_list->top(); (w); w= time_list->next() ) {
          wprocVAShowMsg(w,"Time Offset: %4.3f", offset);
    }
 }
}

void SeisPlotTie::resetTimeOffset()
{
  if (_tiesp) {
       Position gy, ty;

       XtVaGetValues( imageGraphic(), XmNy, &gy, NULL);
       XtVaGetValues( _tiesp->imageGraphic(), XmNy, &ty, NULL);
       _lock_pt= gy -ty;
       displayTimeOffset();
  }
}

int SeisPlotTie::plot()
{
  int prim_stat;
  int tie_stat= True;;

  prim_stat= SeisPlot::plot();
  if (_tiesp) {
     if (_tiesp->is() != is()) {
            _tiesp->setIS(is());
            tie_stat= _tiesp->plot();
     }
  }
  return (prim_stat && tie_stat);
}



void SeisPlotTie::scrollCallback(Widget w, 
                                 XtPointer udata, 
                                 XmScrollBarCallbackStruct  *CBdata )
{
  SeisWinMan *obj = (SeisWinMan *)udata;
  SeisPlotTie *sp= (SeisPlotTie *)obj->currentSP();
  if (sp->_tiesp) sp->scroll(w, udata, CBdata);
}




static int lock_scrolling( const Widget w, 
                           const Widget othersb, 
                           const int val, 
                           const int oldval)
{
  int diff;
  int otherval;
  int small, large;
  int aval, inc, s_size, page_inc;
  int new_position= val;

  diff= val - oldval;
  XtVaGetValues( othersb, XmNminimum, &small, 
                          XmNmaximum, &large, NULL);
  XmScrollBarGetValues(othersb, &otherval, &s_size, &inc, &page_inc);
  otherval+= diff;

  if ((otherval < small) || (otherval > (large-s_size))) {
      XmScrollBarGetValues(w, &aval, &s_size, &inc, &page_inc);
      XmScrollBarSetValues(w, oldval, s_size, inc, page_inc, True);
      new_position=oldval;
  }
  else {
      XmScrollBarSetValues(othersb, otherval, s_size, inc, page_inc, True);
  }

  return new_position;
}






void SeisPlotTie::scroll(Widget w, 
                         XtPointer, 
                         XmScrollBarCallbackStruct  *CBdata )
{
  Boolean lock_sb= _widget_manager->getLockScrollBar();
  Widget tie_vsb; 
 
     if (_tiesp) tie_vsb= _tiesp->_widget_manager->scrolledWindow()->vertSB();
     else        tie_vsb= NULL; 


  if (w == tie_vsb) {
     if ( lock_sb&&(_whose_moving==Nobody_moving) ) {
         _whose_moving= Tie_moving;
         CBdata->value = lock_scrolling(w, _prim_vsb, 
                                        CBdata->value, _old_tie_sb_value);
         _whose_moving= Nobody_moving;
     }
     _old_tie_sb_value= CBdata->value;
  }
  else if (w == _prim_vsb) {
     if ( lock_sb&&(_whose_moving==Nobody_moving) )  {
         _whose_moving= Primary_moving;
         CBdata->value= lock_scrolling(w, tie_vsb, 
                                       CBdata->value, _old_prim_sb_value);
         _whose_moving= Nobody_moving;
     }
     _old_prim_sb_value= CBdata->value;
  }

  displayTimeOffset();

}


void SeisPlotTie::zoomUp()
{
 TieInformSeisZoomer *dozoom;
 

 if (_tiesp) {
      _widget_manager->setCurrentSP(this);
      _widget_manager->lock("Display locked for zooming.");
      _tiesp->_widget_manager->setCurrentSP(_tiesp);
      _tiesp->_widget_manager->lock("Display locked for zooming.");
      dozoom= new TieInformSeisZoomer(this, _tiesp, _zoom_factor);
      _inform_list->callPreZoom(this, dozoom, SeisPlot::Up);
      _tiesp->_inform_list->callPreZoom(_tiesp, dozoom, SeisPlot::Up);
      dozoom->startZoom();
 }
 else
 {
    SeisPlot::zoomUp();
 }

}

void SeisPlotTie::zoomDown()
{
 SeisPlot::zoomDown();
 if (_tiesp) _tiesp->SeisPlot::zoomDown();
}

void SeisPlotTie::originalSize()
{
 SeisPlot::originalSize();
 if (_tiesp) _tiesp->SeisPlot::originalSize();
}


void TieInform::expose(SeisPlot *, int, int, int, int) 
{
}

void TieInform::notCurrentInWindow(SeisPlot *sp)
{
   if (sp == _spt->_tiesp) {
           _spt->_tiesp= sp->currentSPInWindow();
           addSeisPlot( _spt->_tiesp );
           _spt->_tiesp->showBorders(False, True, True, False);
   }
   if (sp == _spt) {
           SeisPlotTie *new_spt= (SeisPlotTie *)sp->currentSPInWindow();
           new_spt->_tiesp= _spt->_tiesp; 
           new_spt->_inform->addSeisPlot( _spt->_tiesp );
           _spt->_tiesp= NULL;
           new_spt->_header_graph= _spt->_header_graph;
           _spt->_header_graph= NULL;
   }
}


void SeisPlotTie::addAllCallbacks(Widget w)
{
/**/
 XtAddCallback( w, XmNvalueChangedCallback, 
               (XtCallbackProc)scrollCallback, (XtPointer)_widget_manager);
 XtAddCallback( w, XmNdragCallback,         
               (XtCallbackProc)scrollCallback, (XtPointer)_widget_manager);
/**/
}

void SeisPlotTie::removeAllCallbacks(Widget w)
{
/**/
 XtRemoveCallback( w, XmNvalueChangedCallback, 
               (XtCallbackProc)scrollCallback,(XtPointer)_widget_manager);
 XtRemoveCallback( w, XmNdragCallback,         
               (XtCallbackProc)scrollCallback,(XtPointer)_widget_manager);
/**/
}

void SeisPlotTie::computePaneledHardWidthHeight(float   seismic_width,
                                                float   seismic_height,
                                                float  *tot_width,
                                                float  *tot_height,
                                                Boolean left_cbar)
{
 SeisPlot::computePaneledHardWidthHeight(seismic_width,seismic_height,
                                         tot_width, tot_height, left_cbar);
}


void SeisPlotTie::computeHardWidthHeight(float   seismic_width,
                                         float   seismic_height,
                                         float  *tot_width,
                                         float  *tot_height,
                                         Boolean left_cbar,
                                         int     frame,
                                         Boolean panel)
{
  float width,height;
  SeisPlot::computeHardWidthHeight(seismic_width, seismic_height, 
                                   &width, &height, left_cbar, frame, panel);
  if (_header_graph) {
        float h_tbord;
        float h_bbord;
        float head_height;
        Display *dpy=   XtDisplay(W());
        int screen_num= XScreenNumberOfScreen( XtScreen(W()) );
        float vpix=     _image.verticalPixelsPerInch(dpy, screen_num);

        head_height= _header_graph->plottedHeight() / vpix;
        h_tbord= _header_graph->topBorder()         / vpix;
        h_bbord= _header_graph->bottomBorder()      / vpix;
        head_height -= (h_tbord + h_bbord);
        head_height +=  HardCopyPlot::defaultTopBorderHeight();
        height+= head_height;
  } // end if _header_graph
  *tot_width= width;
  *tot_height= height;
}



int SeisPlotTie::writeHardCopy(SeisHardCopy *shc)
{
  int stat;
  _inform_list->callPreWriteHardCopy(this);
  stat=  shc->writePlotWithHeader(this, _header_graph);
  _inform_list->callPostWriteHardCopy(this);
  return stat;
}
