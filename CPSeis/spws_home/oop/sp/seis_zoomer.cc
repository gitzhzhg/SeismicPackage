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
/*
C      SeisZoomer.cc
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y
C             written in c++ -- designed to be called from c++
C
C     Utility Name:  SeisZoomer     Zoom Class
C          Written:  93/09/20  by:  Michael L. Sherrill
C     Last revised:  
C
C          Purpose:  Installs required action translations to enable a
C                    zoomed image. Zooms the image up, down, or back
C                    to original size in whatever drawable is requested
C                    by the application. When finished it re-installs
C                    the actions previously used by the application.
C
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY
C  node:                   pospsv (ultrix)
C  source code directory:  
C  library:                
C  header file:            seis_zoomer.hh            (shared)
C  source file:            seis_zoomer.cc
C
C  static functions:       none
C  documented functions:   SeisZoomer
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     SeisPlot.a     image.a    wproc.a
C  header files:  SeisZoomer.h   SeisPlot.h 
C  functions:     See SeisZoomer.h class function definitions
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date           Author              Description
C     ----           ------              -----------
C  2. 
C  1. 93/09/20  Michael L. Sherrill      Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC
*/

#include "sp/seis_zoomer.hh"
#include "sp/seis_inform.hh"
#include "sp/seis_scrwin.hh"
#include "sp/seis_winman.hh"
#include "sl/shell_mode.hh"
#include "sl/shell_mouse_help.hh"
#include "sl/paintset_collection.hh"
#include "plot_image.hh"
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#define MIN_Z_SIZE 5

static char *picking_mode = "Mode: Zoom";
static const char * const help_token = "ZOOMUP";
static const char * const zoomhelp = 
       "mouse*ZOOMUP:  BTN#1: Zoom Up,  BTN#2: Abort,  BTN#3: Popup Menu";





SeisZoomer::SeisZoomer( SeisPlot   *sp,
                        SeisPlotZoom *zoomsp,
                        float      zoomfactor,
                        int        zoomtype) 
                        : PickBase(sp, picking_mode, help_token, zoomhelp,
                                   XC_tcross, allow, True),
                        _sp(sp), _zsp(sp), _zoomsp(zoomsp),_ip(&sp->_image),
                        _box_size(300), _tiesp(NULL)
{
  _ip = &sp->_image;

  if(!zoomtype){ zoomtype = SpArea; setPixelSize( (int)200 );}
  _zoom_type = zoomtype;

  if(!sp->zoomFactor())
    { 
      zoomfactor = 2.0; 
      this->setZoomFactor( (float)zoomfactor );
    }
  else
    {
      zoomfactor = sp->zoomFactor();
    }
  _zoom_factor = zoomfactor;
  sp->setZoomFactor(zoomfactor);

  _box_size = sp->zoomBoxSize();

  _zoom_mode = PlotImage::Z_UP; 
  _separate_window = False;
  _zoom_gc   = NULL;
  _doing_zoom = False;
}



SeisZoomer::~SeisZoomer()
{
  Display *dpy = XtDisplay( _sp->imageGraphic() );
  if(_zoom_gc)XFreeGC( dpy, _zoom_gc );

  if(_zoomsp != NULL) _zoomsp->setNeedToDeleteZoomer(False); 

  if(_zoom_stat == ZoomerAbort && _zoomsp != NULL)
    {
      if(_zoomsp->_spp)delete _zoomsp->_spp;
      if(_zoomsp)delete _zoomsp;
    }
}

void SeisZoomer::setTiePlot(SeisPlot *sp) 
{ 
  _tiesp= sp;
  _separate_window = False;
  _zoom_type = SpPointerTie;
  _tiesp->setZoomFactor(_zoom_factor);
}


void SeisZoomer::setPixelSize(int box_size)
{
  _box_size = box_size;
} 


void SeisZoomer::setZoomFactor(float zoomfactor)
{
  _zoom_factor = zoomfactor;
  _sp->setZoomFactor(zoomfactor);
  if (_tiesp) _tiesp->setZoomFactor(_zoom_factor);
}


void SeisZoomer::setZoomWindow(SeisPlot *zsp)
{
  _ip = &zsp->_image;
  _zsp = zsp;
  _separate_window = True;
}


void SeisZoomer::startZoom()
{
  Arg args[22];
  int n;
  Screen  *scr = XtScreen (_sp->imageGraphic());
  Display *dpy = XtDisplay(_sp->imageGraphic());

  //get user data
  if(_sp->imageGraphic() == NULL) return;
  n = 0;
  XtSetArg (args[n], XmNuserData, this); n++;
  XtSetValues(_sp->imageGraphic(),args,n);

  //create gc
  if(_zoom_gc == NULL)
    {
      _zoom_gc = XCreateGC( dpy, XtWindow(_sp->imageGraphic()), None, NULL);
      XSetBackground( dpy, _zoom_gc, PaintsetCollection::black(scr) );
      XSetForeground( dpy, _zoom_gc, PaintsetCollection::white(scr) );
      XSetFunction( dpy, _zoom_gc, GXinvert);
      XSetLineAttributes(dpy,_zoom_gc,2,LineSolid,CapButt,JoinBevel);
    }
}


void SeisZoomer::zoomAbort()
{
  _zoom_stat = ZoomerAbort;
  //Following two lines commented by MLS 07/99 to prevent the redrawing
  //of all plots in a multi-plot window. We think that the redrawing
  //was originaly here so that zoom area lines drawn to the display
  //would be erased but that should now be taken care of in buttonAny
  //when button 2 is pressed
  //  _sp->redraw();
  //if (_tiesp) _tiesp->redraw();
  zoomComplete(_sp, _zsp, SeisPlot::Abort, _zoom_stat);
  delete this;
}


static Boolean allMatch(SeisPlot *sp)
{
  Boolean retval= True;
  SeisPlot *q;
  void *x;
  float is, ti;
  long nplt;
  double tmin, tmax;
  SeisWinMan *swm= sp->getSeisWinMan();

  assert(sp && swm);
  assert(swm->count() >= 1);

  q= swm->top(&x);
  is= q->plottedIS();
  ti= q->plottedTI();
  nplt= q->plottedNplt();
  tmin= q->plottedTmin();
  tmax= q->plottedTmax();
  for( q= swm->next(&x); (q); q= swm->next(&x)) 
    {
      if (  !q->isPlotDisplayed()                       ||
            (is   != q->plottedIS()   )                 ||
            (ti   != q->plottedTI()   )                 ||
            (nplt != q->plottedNplt() )                 ||
            (fabs (tmin - q->plottedTmin()) > .1 )      ||
            (fabs (tmax - q->plottedTmax()) > .1 ) )
        retval= False;
    } // end loop

  return retval;
}



void SeisZoomer::buttonAny(int ev_x1, int ev_x2, int ev_y1, int ev_y2, 
                           int button, Action action, Modifier)

{
  Display *dpy;
  static long oldx = 0;
  static long oldy = 0;
  static long first_x = 0;
  static long first_y = 0;
  long x1,y1;
  static long width = 0;
  static long height= 0;
  static Dimension pscroll_width= 0;
  static Dimension tscroll_width= 0;
  static Position tie_posx= 0;
  static Position tie_posy= 0;
  static Position prim_posx= 0;
  static Position prim_posy= 0;
  static long last_draw_width, last_draw_height;
  static long last_draw_x, last_draw_y;
  long tx, ty, twidth, theight;

  dpy = XtDisplay( _sp->imageGraphic());

  // check for button 2 abort
  if( _doing_zoom && button == 2)
    {
      XDrawRectangle(dpy, XtWindow( _sp->imageGraphic()), _zoom_gc,
                     (int)last_draw_x,(int)last_draw_y,
                     (unsigned int)last_draw_width,
                     (unsigned int)last_draw_height);
      oldx = oldy = width = height = first_x = first_y = 0;
      zoomAbort();
      return;
    }

  // if starting zoom
  if(button == 1 && action == press)
    {
      _doing_zoom = True;
      first_x = ev_x1;
      first_y = ev_y1;
      XtVaGetValues(XtParent(_ip->graphicWidget()),XmNwidth,
                    &pscroll_width,NULL);
      XtVaGetValues(_ip->graphicWidget(), XmNx, &prim_posx,
                    XmNy, &prim_posy,NULL);
      if (_tiesp) 
        {
          XtVaGetValues(_tiesp->imageGraphic(), XmNx, &tie_posx,
                        XmNy, &tie_posy,NULL);
          XtVaGetValues(XtParent(_tiesp->imageGraphic()), 
                        XmNwidth, &tscroll_width, NULL);
        }  
    }

  // set region to zoom
  if( _zoom_type == SpArea )
    {
      width = height = _box_size;
    }
  else if( (oldx) && (_zoom_type == SpPointer ) )
    {
      width = max(ev_x2,first_x) - min(ev_x2,first_x);
      height= max(ev_y2,first_y) - min(ev_y2,first_y);
      if(width  < MIN_Z_SIZE) width  = MIN_Z_SIZE;
      if(height < MIN_Z_SIZE) height = MIN_Z_SIZE;
    }
  else if( (oldx) && (_zoom_type == SpPointerTie ) )
    {
      width =  pscroll_width - (ev_x2 + prim_posx) +2;
      height= ev_y2 - first_y;
      if(width  < MIN_Z_SIZE) width  = MIN_Z_SIZE;
      if(height < MIN_Z_SIZE) height = MIN_Z_SIZE;
    }

  // if ready to process zoom
  if(button == 1 && action == release)
    {
      if(!oldx && !oldy) return;
      if( _zoom_type == SpArea )
        {
          x1 = oldx - (width / 2);
          y1 = oldy - (height / 2);
        }
      else if (_zoom_type == SpPointer )
        {
          x1 = min(first_x,ev_x2);
          y1 = min(first_y,ev_y2);
        }
      else if (_zoom_type == SpPointerTie )
        {
          x1 = oldx;
          y1 = first_y;
        }
      if( _zoom_type == SpArea )
        {
          if(_separate_window) //erase the zoom box
            XDrawRectangle(dpy, XtWindow( _sp->imageGraphic()), _zoom_gc,
                           (int)x1,(int)y1,(unsigned int)last_draw_width,
                           (unsigned int)last_draw_height);
        }
      else if (_zoom_type == SpPointer )
        {
          if(_separate_window)//erase last draws
            XDrawRectangle(dpy, XtWindow( _sp->imageGraphic()), _zoom_gc,
                        (int)last_draw_x,(int)last_draw_y,(int)last_draw_width,
                        (int)last_draw_height);
        }
      else if (_zoom_type == SpPointerTie )
        {
          tx =  abs(tie_posx) - 1;
          ty =  first_y + prim_posy + abs(tie_posy); 
          twidth = tscroll_width - (oldx + prim_posx); 
          theight = last_draw_height;
        }
      if( (x1 + width) - x1 < MIN_Z_SIZE || (y1 + height) - y1 < MIN_Z_SIZE)
        {//too small so undraw zoom lines and return
          _zsp->redraw();
          _doing_zoom = False;
          oldx = oldy = 0;
          return;
        }
      _doing_zoom = True;   
      _sp->unmanageZoomAbortButton();
      if(!_separate_window) _sp->setDoAbortNewAction();
      manageZoomWindow();
      if (_sp != _zsp) {
        _zoom_stat= processZoomAll (_zsp, (int)_zoom_mode, (int)x1, (int)y1,
          (int)(x1+width), (int)(y1+height));
        _zsp->showBorders(_zsp->_show_left, _zsp->_show_right, 
        _zsp->_show_top,  _zsp->_show_bottom );
      }
      else {
        _zoom_stat= processZoomAll (_sp, (int)_zoom_mode, (int)x1, (int)y1,
          (int)(x1+width), (int)(y1+height));
      }
      if ( (_zoom_stat > 0) && (_tiesp) ) 
        {
          _zoom_stat= processZoomAll(_tiesp, (int)_zoom_mode, (int)tx,(int) ty,
                                     (int)(tx+twidth), (int)(ty+theight) );
        }
      oldx  = oldy = width = height = first_x = first_y = _doing_zoom = 0;
      if(_zoom_stat != PlotImage::PlotSuccess)
        _zoom_stat = ZoomerFail;
      else {
        SeisScrWin  *zsp_scrwin= _zsp->_widget_manager->scrolledWindow(); 
        _zoom_stat = ZoomerOk;
        if (zsp_scrwin)
          zsp_scrwin->setCornerFillColor(_ip->getImageWhitePixel());
      }

      //Added 5/2002 by MLS because pixmaps were not being added on all
      //multi-file images so the remote plots would fail.
      if(_zoom_stat != ZoomerFail)
        {
          PlotImage *tmp_ip= &_sp->_image;
          SeisWinMan *swm= _sp->getSeisWinMan();
          if ((swm->count() == 1) || (tmp_ip!=_ip && tmp_ip != &_tiesp->_image))
            {
              _sp->addPixmaps();
            }
          else//multiple files
            {
              if(allMatch(_sp))
                {
                  SeisPlot *q;
                  void *x;
                  for( q= swm->top(&x); (q); q= swm->next(&x)) 
                    {
                      q->addPixmaps();
                    }
                }
            }
        }


      zoomComplete(_sp, _zsp, SeisPlot::Up, _zoom_stat);
      if (_zoom_stat == ZoomerFail) {
        _sp->plot();
        if (_tiesp) _tiesp->plot();
      }
      delete this; 
      return;
    }//end of button 1 release

  // do the erase of last draw
  if(_doing_zoom)
    {
      if(oldx || oldy) //erase last box position
        {
          if(_zoom_type == SpArea )
            {
              x1 = oldx - (width / 2);
              y1 = oldy - (height / 2);
            }
          else if (_zoom_type == SpPointer )
            {
              x1 = last_draw_x;
              y1 = last_draw_y;
            }
          else if (_zoom_type == SpPointerTie )
            {
              x1 = oldx;
              y1 = first_y;
              tx =  abs(tie_posx) - 1;
              ty =  first_y + prim_posy + abs(tie_posy); 
              twidth = tscroll_width - (oldx + prim_posx); 
              theight = last_draw_height;
            }
          XDrawRectangle(dpy, XtWindow( _sp->imageGraphic()), _zoom_gc,
                         (int)x1,(int)y1,(unsigned int)last_draw_width,
                         (unsigned int)last_draw_height);
          if (_tiesp)
            XDrawRectangle(dpy, XtWindow( _tiesp->imageGraphic()), _zoom_gc,
                           (int)tx,(int)ty,(unsigned int)twidth,
                           (unsigned int)theight);
        }//end of erase

      //draw new box
      if( _zoom_type == SpArea )
        {
          x1 = ev_x2 - (width / 2);
          y1 = ev_y2 - (height / 2);
        }
      else if (_zoom_type == SpPointer )
        {
          x1 = min(first_x,ev_x2);
          y1 = min(first_y,ev_y2);
        }
      else if (_zoom_type == SpPointerTie )
        {
          x1 = ev_x2;
          y1 = first_y;
          tx =     abs(tie_posx) - 1;
          ty =     first_y + prim_posy + abs(tie_posy);
          twidth =  tscroll_width - (ev_x2 + prim_posx);
          theight = height;
        }
      else
        {
          x1 = (max(ev_x2,first_x) - (width / 2));
          y1 = (max(ev_y2,first_y) - (height/ 2));
        }
      XDrawRectangle(dpy, XtWindow( _sp->imageGraphic()), _zoom_gc,
                     (int)x1,(int)y1,(unsigned int)width,(unsigned int)height);
      if (_tiesp)
        XDrawRectangle(dpy, XtWindow( _tiesp->imageGraphic()), _zoom_gc,
                   (int)tx,(int)ty,(unsigned int)twidth,(unsigned int)theight);
      last_draw_x = x1;
      last_draw_y = y1;
      last_draw_width = width;
      last_draw_height= height;
      oldx = ev_x2;
      oldy = ev_y2;
    }//end if doing zoom

}



void SeisZoomer::zoomDown()
{

  _zoom_mode = PlotImage::Z_DOWN;
  manageZoomWindow();
  _zoom_stat= processZoomAll(_sp, (int)_zoom_mode, 0,0,0,0);
  _zoom_stat = (_zoom_stat != PlotImage::PlotSuccess) ? ZoomerFail : ZoomerOk;

  if ( _zoom_stat == ZoomerOk && _tiesp ) 
    {
      _zoom_stat= processZoomAll(_tiesp, (int)_zoom_mode, 0,0,0,0);
      _zoom_stat = (_zoom_stat != PlotImage::PlotSuccess)
        ? ZoomerFail : ZoomerOk;
      //      _tiesp->redraw();
    }
  zoomComplete(_sp, _zsp, SeisPlot::Down, _zoom_stat);
}



void SeisZoomer::zoomOriginal()
{

  if(_ip->getZoomIndex() == NO_ZOOM) return; //not zoomed
  _zoom_mode = PlotImage::Z_ORIGINAL;
  manageZoomWindow();
  _zoom_stat= processZoomAll(_sp, (int)_zoom_mode, 0,0,0,0);

  if(_zoom_stat != PlotImage::PlotSuccess)
    _zoom_stat = ZoomerFail;
  else
    _zoom_stat = ZoomerOk;

  zoomComplete(_sp, _zsp, SeisPlot::Orginal, _zoom_stat);
}

void SeisZoomer::manageZoomWindow()
{
  Widget shell = NULL;

  if(!XtWindow(_ip->graphicWidget()))
    shell = get_shell_child(_ip->graphicWidget());

  if(shell)XtManageChild(shell); 
}




int SeisZoomer::processZoomAll( SeisPlot *sp,
                                int       mode, 
                                int       x1, 
                                int       y1, 
                                int       x2, 
                                int       y2)
{
  PlotImage *tmp_ip= &sp->_image;
  int stat= PlotImage::PlotSuccess;
  SeisWinMan *swm= sp->getSeisWinMan();
  if ((swm->count() == 1) || (tmp_ip!=_ip && tmp_ip != &_tiesp->_image))
    {
      /*
       *  enter in this block if we are zooming one image or if we
       *  are zooming a separate window.
       */
      if (tmp_ip!=_ip && tmp_ip!=&_tiesp->_image) tmp_ip= _ip; //do sep. window
      sp->delPixmaps();//Added 5/2002 MLS for multifile reasons
      stat= tmp_ip->processZoom(mode, x1, y1, x2, y2, False);
    } // end if
  else 
    {
      /*
       *  possibly zooming multiple images
       */
      if (allMatch(sp))
        {  // if match zoom them all
          int test_stat;
          SeisPlot *q;
          void *x;
          for( q= swm->top(&x); (q); q= swm->next(&x)) {
            q->setZoomFactor(_zoom_factor);
            q->delPixmaps();//Added 5/2002 MLS for multifile reasons
            test_stat= q->_image.processZoom(mode, x1, y1, x2, y2, False);
            if (test_stat != PlotImage::PlotSuccess)
              stat= test_stat;
          } // end loop
          if (stat == PlotImage::PlotSuccess) {
            sp->redraw();
          }
        } // end if
      else {  // don't match so zoom just this one
        sp->delPixmaps();//Added 5/2002 MLS for multifile reasons
        stat= sp->_image.processZoom(mode, x1, y1, x2, y2, False);
      } //end else
    } //end else


  return stat;
}
