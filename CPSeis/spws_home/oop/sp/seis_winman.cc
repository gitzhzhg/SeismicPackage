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
#include <assert.h>
#include <stdlib.h>
#include "sp/seis_winman.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_plot_tie.hh"
#include "sp/seis_scrwin.hh"
#include "sp/inform_list.hh"
#include "plot/pick_base.hh"
#include "sl/view_win.hh"
///////////////////////// new ///////////////////////////////////////
#include "plot_image.hh"
#include "pixmap_set.hh"
#include "sl/paintset.hh"
#include "sl/paintset_collection.hh"
///////////////////////// new ///////////////////////////////////////




class SPInfo{
   public:
     SPInfo() : status(PlotImage::XYOFF) {}
     int   status;
};






SeisWinMan::SeisWinMan(SeisPlot     *sp,
                       const Widget  p,
                       const char   *name,
                       Boolean       do_scroll) :
                  _topw(NULL), _da(NULL), 
                  _scrwin(NULL), _persistent_anno(True),
                  _current_sp(NULL), _lock_sb(False), _movies_locked(True),
                  _ignore_anno_req(False)
{
  init(sp,p,name,do_scroll);
}

SeisWinMan::SeisWinMan(SeisPlotTie  *sp,
                       const Widget  p,
                       const char   *name) :
                  _topw(NULL), _da(NULL), 
                  _scrwin(NULL), _persistent_anno(True),
                  _current_sp(NULL), _lock_sb(False), _ignore_anno_req(False)
{
  _winctl= new WinCtl(p, (char*)name);
  init(sp,_winctl->W(),name,True);
  _primsw= new ViewWin(_winctl, _scrwin->W(), NULL);

}


void SeisWinMan::init(SeisPlot     *sp,
                      const Widget  p,
                      const char   *name,
                      Boolean       do_scroll)
{
  _dpy= XtDisplay(p);
///////////////////////// new ///////////////////////////////////////
  Arg arglist[22];
  int n = 0;
  Paintset *paintset;
///////////////////////// new ///////////////////////////////////////

  if (do_scroll) {
    Pixel back_pix /*fore_pix*/;
       _scrwin=  new SeisScrWin(p, name, sp);

///////////////////////// new ///////////////////////////////////////
       paintset = PaintsetCollection::fetchExistingByColormap (
         _scrwin->scrollParent());
       paintset->addResources (arglist, &n);
       XtSetArg (arglist[n], XmNbackground, paintset->white()); n++;
       XtSetArg (arglist[n], XmNforeground, paintset->black()); n++;
       _da = XtCreateManagedWidget(name, xmDrawingAreaWidgetClass,
				   _scrwin->scrollParent(), arglist, n);
///////////////////////// new ///////////////////////////////////////


///////////////////////// old ///////////////////////////////////////
// black initial state
       _da= XtVaCreateManagedWidget(name, xmDrawingAreaWidgetClass,
				    _scrwin->scrollParent(), NULL);
       XtVaGetValues(_da, XmNbackground, &back_pix, NULL);
       _scrwin->setCornerFillColor(back_pix);
///////////////////////// old ///////////////////////////////////////

///////////////////////// new ///////////////////////////////////////
// white initial state
//       XtVaGetValues(_da, XmNforeground, &fore_pix, NULL);
//       _scrwin->setCornerFillColor(fore_pix);
///////////////////////// new ///////////////////////////////////////

       _scrwin->setWorkArea(_da);
       _topw = _scrwin->W();
  }
///////////////////////// new ///////////////////////////////////////
  else {
    paintset = PaintsetCollection::fetchExistingByColormap (p);
    paintset->addResources (arglist, &n);
    XtSetArg (arglist[n], XmNbackground, paintset->white()); n++;
    XtSetArg (arglist[n], XmNforeground, paintset->black()); n++;
    _topw = _da = XtCreateManagedWidget(name, xmDrawingAreaWidgetClass,
					p, arglist, n);
  }
///////////////////////// new ///////////////////////////////////////

/*
///////////////////////// old ///////////////////////////////////////
  else
       _topw= _da= XtVaCreateManagedWidget(name, xmDrawingAreaWidgetClass,
                                          p, NULL);
///////////////////////// old ///////////////////////////////////////
*/

  XtAddCallback(_da, XmNexposeCallback, (XtCallbackProc)exposeCallback,
                     (XtPointer)this);
  XtAddCallback(_topw, XmNdestroyCallback, (XtCallbackProc)destroyCallback,
                       (XtPointer)this);
  XtVaSetValues(_da, XmNresizePolicy, XmRESIZE_NONE, NULL);
  addSeisPlot(sp);
  _current_sp= sp;

}



SeisWinMan::~SeisWinMan()
{
  delete _scrwin;
  Widget w=NULL, next_w=NULL;
  for(w= _time_list.top(); (w); w= next_w ) {
         next_w= _time_list.next();
         _time_list.remove(w);
  } // end loop

  if (_topw) {
      XtRemoveCallback(_topw, XmNdestroyCallback, 
                       (XtCallbackProc)destroyCallback,
                       (XtPointer)this);
  }
}

Widget SeisWinMan::drawingArea()
{
  return _da;
}

Widget SeisWinMan::W() 
{ 
  return _topw; 
}

SeisScrWin *SeisWinMan::scrolledWindow()
{
  return _scrwin;
}


void SeisWinMan::destroyCallback(Widget w, XtPointer udata, XtPointer)

{
  SeisWinMan *obj = (SeisWinMan *)udata;
  assert(w == obj->_topw);
  obj->_da= NULL;
  obj->_topw= NULL;
}

void SeisWinMan::exposeCallback(Widget    w,
                                XtPointer udata,
                                XmDrawingAreaCallbackStruct *CBdata)

{
  SeisWinMan *obj = (SeisWinMan *)udata;
  assert(w == obj->_da);
  obj->_current_sp->expose(w, (XtPointer)obj->_current_sp, CBdata);
}


void SeisWinMan::addSeisPlot(SeisPlot *sp)
{
   if (sp) {
       void *x;
       SPInfo *new_info= new SPInfo();
       if (top(&x)) {
            SPInfo *other_info= (SPInfo*)currentUserData(&x);
            new_info->status= other_info->status;
       }
       add(sp,(void*)new_info);
   }
}

Boolean SeisWinMan::delSeisPlot(SeisPlot *sp)
{
   void *x;
   SLScrollWin::WhichCorner wc;
   char *corner_str;
   if (!annotationRequestsIgnored()) {
     wc         = _scrwin->getCornerAnnotationCorner();
     corner_str = _scrwin->getCornerAnnotation();
   }
   else {
     corner_str = NULL;
   }

   if (isLocked()) return False; 
   if (sp) {
      if (find(sp, &x)) {
         SPInfo *info= (SPInfo* )currentUserData(&x);
         delete info;
         if ((sp == _current_sp) && (count() > 1)) {
               SeisPlot *tmpsp= NULL;
               tmpsp= next(&x);
               if (!tmpsp) tmpsp= top(&x);
               setCurrentSP(tmpsp);
         } // endif
         remove(sp);
         if (corner_str) {
            setCornerAnnotation(corner_str, wc);
            free(corner_str);
         }
         if (count() == 0) delete this;
      } // end if
      else
        {
          //Removed next warning because an underlay associated indirectly
          //to this class will not be found. The overlay is the one
          //added to this class.
          //printf("SeisInform::delSeisPlot: SeisPlot element not found\n");
        }
   } // end if sp
   return True;
}

SeisPlot *SeisWinMan::currentSP()
{
  return _current_sp;
}

Boolean SeisWinMan::setSPNotCurrent(SeisPlot *sp)
{
  Boolean retval= False;
  if ( !isLocked() && sp && (sp == _current_sp) && (count() > 1) ) {
       void *x;
       SeisPlot *newsp;
       assert(find(sp, &x));
       newsp= next(&x);
       if (!newsp) newsp= top(&x);
       assert(newsp != sp);
       setCurrentSP(newsp); 
       retval= True;
  }
  return retval;
}

void SeisWinMan::drawNewPixmap(SeisPlot *prev_sp)
{

   if (_movies_locked) {
        int target_frame= (int)prev_sp->currentFrame();
        int goto_frame= 0;
        goto_frame= (_current_sp->plottedFrames() > target_frame) ?
                           target_frame : (int)_current_sp->plottedFrames() - 1;
        if (_current_sp->currentFrame() != goto_frame)
                    _current_sp->movieToFrame(goto_frame);
        else
                    _current_sp->drawNewPixmap ();
   }
   else {
////////////////////////// new //////////////////////////
            prev_sp->_image.pixmapSet()->setRedrawingEnabled (FALSE);
        _current_sp->_image.pixmapSet()->setRedrawingEnabled (TRUE );
////////////////////////// new //////////////////////////
        _current_sp->drawNewPixmap ();
   }



}

Boolean SeisWinMan::setCurrentSP(SeisPlot *sp)
{
  SPInfo *info;
  SeisPlot *prev_sp;
  if (isLocked()) return False; 
  if (sp && (sp != _current_sp)){
         void *x;
         assert(find(_current_sp, &x));
         prev_sp= _current_sp;
         _current_sp= find(sp, &x);
         if (_current_sp) {
              info= (SPInfo* )currentUserData(&x);

                  // change size of width - this may be different in zoom case
              if ( (_da) && (_current_sp->_image.getGraphHeight() > 0)
                             && (_current_sp->_image.getGraphWidth() > 0) ) {
                         XtVaSetValues(_da,
                                XmNheight, _current_sp->_image.getGraphHeight(),
                                XmNwidth,  _current_sp->_image.getGraphWidth(), 
                                NULL);
              } // end if

                  // redraw new image
              _scrwin->setCurrentSP(_current_sp);

              drawNewPixmap(prev_sp);

              if (_da) {
                 if (_current_sp->isPlotDisplayed())
                     _scrwin->setCornerFillColor(
                                  _current_sp->_image.getImageWhitePixel());
                 else {
/*
///////////////////////// old ///////////////////////////////////////
                     Pixel back_pix;
	             XtVaGetValues(_da, XmNbackground, &back_pix, NULL);
	             _scrwin->setCornerFillColor(back_pix);
///////////////////////// old ///////////////////////////////////////
*/
///////////////////////// new ///////////////////////////////////////
                     Pixel fore_pix;
	             XtVaGetValues(_da, XmNforeground, &fore_pix, NULL);
	             _scrwin->setCornerFillColor(fore_pix);
///////////////////////// new ///////////////////////////////////////
                 } // end else
              } // end if _da

                  // set xy output for current SeisPlot
              removeLocationOutput(prev_sp,True);
              if (info->status == PlotImage::XYAUTO)
                    _current_sp->setLocationOutput( _xloc, _yloc, _aout);

              _current_sp->_anno_override_app_request= 
                              prev_sp->_anno_override_app_request;
              _current_sp->_app_wants_persistent_anno= 
                              prev_sp->_app_wants_persistent_anno;
              _current_sp->_new_border = prev_sp->_new_border;
              //_current_sp->_show_left  = prev_sp->_show_left;
              //_current_sp->_show_right = prev_sp->_show_right;
              //_current_sp->_show_top   = prev_sp->_show_top;
              //_current_sp->_show_bottom= prev_sp->_show_bottom;
              _current_sp->showBorders(prev_sp->_show_left,
                                       prev_sp->_show_right,
                                       prev_sp->_show_top,
                                       prev_sp->_show_bottom );
             
              _scrwin->redrawAnnotation();
              PickBase::changePlotBase(prev_sp, _current_sp);


	      // right here take the _col in prev_sp and adopt it for
	      //   _current_sp!


              prev_sp->_inform_list->callNotCurrentInWindow(prev_sp);
              _current_sp->_inform_list->callNowCurrentInWindow(_current_sp);
              _current_sp->displayPixmap();
         
         } // end if _current_sp
         else {
            printf("SeisWinMan::setCurrentSP: SeisPlot element not found\n");
            _current_sp= prev_sp;
         }
  } // end if sp
  return True;
}


void SeisWinMan::setLocationOutput(SeisPlot *sp, 
                                   Widget    xloc, 
                                   Widget    yloc, 
                                   Widget    aout)
{
  void *x;

////////////////// new ////////////////////
  if (PaintsetCollection::fetchExistingByColormap(sp->W())->readOnly() &&
    !xloc && !yloc && !aout) {
    // when these were nil they would break the readout on multi-file
    //   displays!
    xloc = _xloc;
    yloc = _yloc;
    aout = _aout;
  }
////////////////// new ////////////////////
  sp->_image.setXYout(xloc, yloc, aout, PlotImage::XYAUTO);
  sp= find(sp, &x);
  if (sp) {
        SPInfo *info= (SPInfo* )currentUserData(&x);
        info->status= PlotImage::XYAUTO;
        _xloc= xloc;
        _yloc= yloc;
        _aout= aout;
  }
}


void SeisWinMan::removeLocationOutput(SeisPlot *sp, Boolean for_swap)
{
  void *x;
  sp->_image.setXYout( NULL, NULL, NULL, PlotImage::XYOFF);
  sp= find(sp, &x);
  if (sp) {
     SPInfo *info= (SPInfo* )currentUserData(&x);
     if (!for_swap) info->status= PlotImage::XYOFF;
  }
}

Display *SeisWinMan::display()
{
  return _dpy;
}

void SeisWinMan::setCornerAnnotation(char                     *str, 
                                     SLScrollWin::WhichCorner  corner)
{
  _scrwin->setCornerAnnotation(str,  top()->annoBoldFont(), corner );
}

WinCtl *SeisWinMan::getWinCtl() 
{
  return _winctl;
}

ViewWin *SeisWinMan::getPrimViewWin() {
  return _primsw;
}

void     SeisWinMan::addOutputWidget(Widget w)   { _time_list.add(w);}
void     SeisWinMan::delOutputWidget(Widget w)   { _time_list.remove(w);}
Wlist   *SeisWinMan::getTimeList()               { return &_time_list; }
void     SeisWinMan::setLockScrollBar(Boolean v) { _lock_sb= v; }
Boolean  SeisWinMan::getLockScrollBar()          { return _lock_sb; }

ViewWin *SeisWinMan::addTieViewWin(Widget p)
{ 
        _tiesw= new ViewWin( _winctl, p, NULL); 
        return _tiesw;
}
ViewWin *SeisWinMan::addHeadViewWin(Widget p)
{ 
        _header_sw= new ViewWin( _winctl, p, NULL); 
        return _header_sw;
}
void SeisWinMan::deleteTieViewWin() 
{ 
  delete _tiesw;
  _tiesw= NULL;
}
void SeisWinMan::deleteHeadViewWin() 
{ 
  delete _header_sw;
  _header_sw= NULL;
}
ViewWin *SeisWinMan::getTieViewWin()  { return _tiesw; }
ViewWin *SeisWinMan::getHeadViewWin() { return _header_sw; }


void SeisWinMan::lock(char *reason)
{
  _lock_list.add(reason);
}

void SeisWinMan::unlock()
{
  void *x= NULL;
  char *str= _lock_list.bottom(&x);
  if (str) _lock_list.remove(str);
}

Boolean SeisWinMan::isLocked()
{
  return (_lock_list.count() > 0);
}


char *SeisWinMan::getLockReason() 
{
  return _lock_list.bottom();
}

void    SeisWinMan::setMoviesLocked(Boolean l) { _movies_locked= l;}
Boolean SeisWinMan::getMoviesLocked()          { return _movies_locked; }

Boolean SeisWinMan::inList (SeisPlot *sp)
{
  void *x;
  return find(sp,&x) != 0;
}

void SeisWinMan::ignoreAnnotationRequest (Boolean ignore)
{
   _ignore_anno_req = ignore;
}

Boolean SeisWinMan::annotationRequestsIgnored ()
{
  if (_ignore_anno_req) {
    _ignore_anno_req = False;
    return True;
  }
}
