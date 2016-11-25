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
// author Michael L. Sherrill 10/93 
// creates arrow widgets and controls the panning of an image
// within a separate window.

#include "sp/seis_panner.hh"
#include "sp/seis_plot_zoom.hh"
#include "sp/seis_plot_zoom_under.hh"
#include "sp/inform_list.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_scrwin.hh"
#include "sl/shell_watch.hh"
#include "plot_image.hh"
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <assert.h>
#include "sp/arrows.h"

//static String  defres[]= {
//    "*pan_ctl.orientation:  VERTICAL",
//    "*pan_ctl.packing:      PACK_COLUMN",
//    "*North.labelString:    Up",
//    "*South.labelString:    Down",
//    "*East.labelString:     Right",
//    "*West.labelString:     Left",
//    "*NE.labelString:       Up/Right",
//    "*NW.labelString:       Up/Left",
//    "*SE.labelString:       Down/Right",  
//    "*SW.labelString:       Down/Left",
//     NULL, };

enum {North, South, East, West, NE, NW, SE, SW, Center };


SeisPanner::SeisPanner(Widget p, char *name, SeisPlotZoom *spz, Boolean movie,
                       HelpCtx hctx)
                      : SLFPopSep(p,name,FP_DOOK|FP_DOHELP,hctx,True)

{
  wunion wu[NUM_PUSH];
  Pixmap arrow_pix[8];
  unsigned long foreg, backg;
  long screen;
  Display *dpy;
  Screen *scr;
  Window rootw;
  int i;
 
  _spz  = NULL;
  _spzu = NULL;

  if(spz)
   {
     _spz = spz;
     _image = &spz->_image;
   }

  _movie = NULL;

  _sep =  _lowsep;

  _pan_ctl = XtVaCreateManagedWidget("pan_ctl",
                                      xmRowColumnWidgetClass, topWidget(),
                                      XmNtopAttachment,    XmATTACH_FORM,
                                      XmNleftAttachment,   XmATTACH_FORM,
                                      XmNbottomAttachment, XmATTACH_WIDGET, 
                                      XmNbottomWidget,     _lowsep, NULL);

  qset_CB( _cb, North, this, NULL);
  mk_pshb_cb( wu, _cb, _pan_ctl, (XtCallbackProc)doPanCallback, 
              &_cb[North], NUM_PUSH, North, "North",    South, "South",
                                     East,  "East",     West,  "West",
                                     NE,    "NE",       NW,    "NW",
                                     SE,    "SE",       SW,    "SW");

  for(i=0;(i<NUM_PUSH); i++) _pan_w[i]= wu[i].w;

  dpy    = XtDisplay(topWidget());
  screen = DefaultScreen(XtDisplay(topWidget()));
  scr    = XtScreen(topWidget());
  rootw  = RootWindowOfScreen(scr);
  XtVaGetValues( _pan_w[0], XmNforeground, &foreg, NULL);
  XtVaGetValues( _pan_w[0], XmNbackground, &backg, NULL);

  arrow_pix[North] = XCreatePixmapFromBitmapData( dpy, rootw, north_bits, 
                                                  arrow_width,
                                                  arrow_height, foreg, backg,  
                                                  DefaultDepth(dpy,screen) );
  arrow_pix[South] = XCreatePixmapFromBitmapData( dpy, rootw, south_bits,
                                                  arrow_width,
                                                  arrow_height, foreg, backg,
                                                  DefaultDepth(dpy,screen) );
  arrow_pix[East] =  XCreatePixmapFromBitmapData( dpy, rootw, east_bits,
                                                  arrow_width,
                                                  arrow_height, foreg, backg,
                                                  DefaultDepth(dpy,screen) );
  arrow_pix[West] =  XCreatePixmapFromBitmapData( dpy, rootw, west_bits,
                                                  arrow_width,
                                                  arrow_height, foreg, backg,
                                                  DefaultDepth(dpy,screen) );
  arrow_pix[NE] =    XCreatePixmapFromBitmapData( dpy, rootw, ne_bits,
                                                  arrow_width,
                                                  arrow_height, foreg, backg,
                                                  DefaultDepth(dpy,screen) );
  arrow_pix[NW] =    XCreatePixmapFromBitmapData( dpy, rootw, nw_bits,
                                                  arrow_width,
                                                  arrow_height, foreg, backg,
                                                  DefaultDepth(dpy,screen) );
  arrow_pix[SE] =    XCreatePixmapFromBitmapData( dpy, rootw, se_bits,
                                                  arrow_width,
                                                  arrow_height, foreg, backg,
                                                  DefaultDepth(dpy,screen) );
  arrow_pix[SW] =    XCreatePixmapFromBitmapData( dpy, rootw, sw_bits,
                                                  arrow_width,
                                                  arrow_height, foreg, backg,
                                                  DefaultDepth(dpy,screen) );
  for(i=0; i<NUM_PUSH; i++)
      XtVaSetValues( _pan_w[i], XmNlabelType,   XmPIXMAP, 
                     XmNlabelPixmap, arrow_pix[i], NULL);
                                         
  //if we have a movie control move the ok help buttons over to make room
  if(movie)
      XtVaSetValues( buttonContainer(),
                     XmNleftAttachment,   XmATTACH_FORM,
                     XmNleftOffset,       250, NULL );

  
}

SeisPanner::~SeisPanner()
{
  if(_movie) delete _movie;
}

void SeisPanner::DoAction()
{
 long i;


  for(i=0; i<MAX_PIXMAP; i++)
      {
      if(_image->getPixmapArray(i) != 0)
         {
         XFreePixmap(XtDisplay(_image->graphicWidget()),
                               _image->getPixmapArray(i));
         _image->setPixmap(i,0);
         }    
      }
  if(_spz != NULL)
    {
    delete _spz; 
    _spz = NULL;
    }

  if(_spzu != NULL)
    {
    delete _spzu; 
    _spzu = NULL;
    }

  delayDelete();
}

void SeisPanner::setPanImage(SeisPlotZoom *spz, SeisPlotZoomUnder *spzu)
{
  _spz = spz;
  _image = &spz->_image;
  _spzu = spzu;
} 


void SeisPanner::doPanCallback(Widget              w, 
                               struct CB           *udata, 
                               XmAnyCallbackStruct *CBdata)
{
  SeisPanner *obj = (SeisPanner *)udata->info;
  obj->doPan(w, udata, CBdata);

}

void SeisPanner::addMovie(SeisMovie *movie)
{
 _movie = movie;
}


void SeisPanner::doPan(Widget, 
                       struct CB           *udata, 
                       XmAnyCallbackStruct *)
{
 float percent = .20;
 long x1, x2, y1, y2;
 long stat;
 static long width, height;
 long numtraces, first_trace;
 long lborder = _image->getLeftBorder();
 long tborder = _image->getTopBorder();
 ShellWatch watch= _image->graphicWidget();


  if(_image->getCurrentPixmap() == 0) return;

  if(_image->getZoomFactor() > 1.0) // first time in with this image
     {
     width = _image->getXimageWidth();
     height= _image->getXimageHeight();
     _image->setZoomFactor(1.0);
     }
 

  numtraces = (int)(_image->getZoomXend() - _image->getZoomXstart() + 1.0);
  


  switch (udata->wconst)
  {
    case North:
               if(_image->getImageInput()->_mode!= PlotImage::PlotGRID && 
                  _image->getImageInput()->_mode != PlotImage::PlotHEADER &&
                  _image->getImageInput()->_mode != PlotImage::PlotARRAY)
                  {
                  first_trace= _image->getFirstTraceInImage();
                  if(first_trace + numtraces - 1 > _image->getTracesInMemory())
                     first_trace = _image->getTracesInMemory() - numtraces + 1;
                  x1=lborder + int( (first_trace-1) * _image->getTraceWidth() );
                  x2 = x1 + int( (numtraces-1) * _image->getTraceWidth());
                  }
               else
                  {
                    //x1 = int( lborder
                    //+ (I_sdatXmin(_image) * I_sdatXperpix(_image)));
                    //x2 = x1 + width;
                  x1 = lborder;     x2 = width + lborder;
                  }
               y1 = tborder - int(percent * height);
               y2 = y1 + height;
               break;

    case South:
               if(_image->getImageInput()->_mode!= PlotImage::PlotGRID &&
                  _image->getImageInput()->_mode != PlotImage::PlotHEADER && 
                  _image->getImageInput()->_mode != PlotImage::PlotARRAY)
                  {
                  first_trace= _image->getFirstTraceInImage();
                  if(first_trace + numtraces - 1 > _image->getTracesInMemory())
                     first_trace = _image->getTracesInMemory() - numtraces + 1;
                  x1=lborder + int( (first_trace-1) * _image->getTraceWidth() );
                  x2 = x1 + int( (numtraces-1) * _image->getTraceWidth());
                  }
               else
                  {
                    //x1 = int( lborder
                    //+ (I_sdatXmin(_image) * I_sdatXperpix(_image)));
                    //x2 = x1 + width;
                  x1 = lborder;     x2 = width + lborder;
                  }
               y1 = tborder + int(percent * height);
               y2 = y1 + height;
               break;

    case East:
               if(_image->getImageInput()->_mode!= PlotImage::PlotGRID &&
                  _image->getImageInput()->_mode != PlotImage::PlotHEADER &&
                  _image->getImageInput()->_mode != PlotImage::PlotARRAY)
                  {
                  if(!_image->getImageInput()->getRightToLeft())
                    first_trace= _image->getFirstTraceInImage() +
                                 int(percent * numtraces);
                  else
                    first_trace= _image->getFirstTraceInImage() - 
                                 int(percent * numtraces);
                  if(first_trace + numtraces - 1 > _image->getTracesInMemory())
                     first_trace = _image->getTracesInMemory() - numtraces + 1;
                  x1 = lborder + int( (first_trace-1) *
                                 _image->getTraceWidth() );
                  x2 = x1 + int( (numtraces-1) * _image->getTraceWidth());
	          }
               else
                  {
                    //x1 = int((lborder + int(percent * width))
                    //+ (I_sdatXmin(_image) * I_sdatXperpix(_image)));
                    //x2 = x1 + width;
                  x1 = lborder + int(percent * width);
                  x2 = x1 + width;
	          }
               y1 = tborder;     y2 = height + tborder;
               break;

    case West:
               if(_image->getImageInput()->_mode!= PlotImage::PlotGRID && 
                  _image->getImageInput()->_mode != PlotImage::PlotHEADER && 
                  _image->getImageInput()->_mode != PlotImage::PlotARRAY)
                  {
                  if(!_image->getImageInput()->getRightToLeft())                  
                    first_trace= _image->getFirstTraceInImage() - 
                                 int(percent * numtraces);
                  else
                    first_trace= _image->getFirstTraceInImage() + 
                                 int(percent * numtraces);
                  if(first_trace < 1) first_trace = 1;
                  x1=lborder + int( (first_trace-1) * _image->getTraceWidth() );
                  x2 = x1 + int( (numtraces-1) * _image->getTraceWidth());
                  }
               else
                  {
                    //x1 = int((lborder - int(percent * width))
                    //+ (I_sdatXmin(_image) * I_sdatXperpix(_image)));
                    //x2 = x1 + width;
                  x1 = lborder - int(percent * width);
                  x2 = x1 + width;
	          }
               y1 = tborder;     y2 = height + tborder;
               break;

    case NE:   
               if(_image->getImageInput()->_mode!= PlotImage::PlotGRID && 
                  _image->getImageInput()->_mode != PlotImage::PlotHEADER &&
                  _image->getImageInput()->_mode != PlotImage::PlotARRAY)
                  {               
                  if(!_image->getImageInput()->getRightToLeft())
                    first_trace= _image->getFirstTraceInImage() + 
                                 int(percent * numtraces);
                  else
                    first_trace= _image->getFirstTraceInImage() - 
                                int(percent * numtraces);
                  if(first_trace + numtraces - 1 > _image->getTracesInMemory())
                     first_trace = _image->getTracesInMemory() - numtraces + 1;
                  x1=lborder + int( (first_trace-1) * _image->getTraceWidth() );
                  x2 = x1 + int( (numtraces-1) * _image->getTraceWidth());
	          }
               else
                  {
                    //x1 = int((lborder + int(percent * width))
                    //+ (I_sdatXmin(_image) * I_sdatXperpix(_image)));
                    //x2 = x1 + width;
                  x1 = lborder + int(percent * width);
                  x2 = x1 + width;
	          }
               y1 = tborder - int(percent * height);
               y2 = y1 + height;
               break;

    case NW:
               if(_image->getImageInput()->_mode!= PlotImage::PlotGRID && 
                  _image->getImageInput()->_mode != PlotImage::PlotHEADER && 
                  _image->getImageInput()->_mode != PlotImage::PlotARRAY)
                  {
                  if(!_image->getImageInput()->getRightToLeft())
                    first_trace= _image->getFirstTraceInImage() - 
                                 int(percent * numtraces);
                  else
                    first_trace= _image->getFirstTraceInImage() + 
                                 int(percent * numtraces);
                  if(first_trace < 1) first_trace = 1;
                  x1=lborder + int( (first_trace-1) * _image->getTraceWidth() );
                  x2 = x1 + int( (numtraces-1) * _image->getTraceWidth());
	          }
               else
                  {
                    //x1 = int((lborder - int(percent * width))
                    // + (I_sdatXmin(_image) * I_sdatXperpix(_image)));
                    //x2 = x1 + width;
                  x1 = lborder - int(percent * width);
                  x2 = x1 + width;
                  }                 
               y1 = tborder - int(percent * height);
               y2 = y1 + height;
               break;

    case SE:
               if(_image->getImageInput()->_mode!= PlotImage::PlotGRID &&
                  _image->getImageInput()->_mode != PlotImage::PlotHEADER && 
                  _image->getImageInput()->_mode != PlotImage::PlotARRAY)
                  {
                  if(!_image->getImageInput()->getRightToLeft())
                    first_trace= _image->getFirstTraceInImage() + 
                                 int(percent * numtraces);
                  else
                    first_trace= _image->getFirstTraceInImage() - 
                                 int(percent * numtraces);
                  if(first_trace + numtraces - 1 > _image->getTracesInMemory())
                     first_trace = _image->getTracesInMemory() - numtraces + 1;
                  x1=lborder + int( (first_trace-1) * _image->getTraceWidth() );
                  x2 = x1 + int( (numtraces-1) * _image->getTraceWidth());
	          }
               else{
                 //x1 = int((lborder + int(percent * width))
                 //+ (I_sdatXmin(_image) * I_sdatXperpix(_image)));
                 //x2 = x1 + width;
                 x1 = lborder + int(percent * width);
                 x2 = x1 + width;
               } 
               y1 = tborder + int(percent * height);
               y2 = y1 + height;
               break;

    case SW:
               if(_image->getImageInput()->_mode!= PlotImage::PlotGRID &&
                  _image->getImageInput()->_mode != PlotImage::PlotHEADER &&
                  _image->getImageInput()->_mode != PlotImage::PlotARRAY)
                  {
                  if(!_image->getImageInput()->getRightToLeft())
                    first_trace= _image->getFirstTraceInImage() - 
                                int(percent * numtraces);
                  else
                    first_trace= _image->getFirstTraceInImage() +
                                 int(percent * numtraces);
                  if(first_trace < 1) first_trace = 1;
                  x1=lborder + int( (first_trace-1) * _image->getTraceWidth() );
                  x2 = x1 + int( (numtraces-1) * _image->getTraceWidth());
                  }
               else
                  {
                    //x1 = int((lborder - int(percent * width))
                    //+ (I_sdatXmin(_image) * I_sdatXperpix(_image)));
                    //x2 = x1 + width;
                  x1 = lborder - int(percent * width);
                  x2 = x1 + width;
                  }
               y1 = tborder + int(percent * height);
               y2 = y1 + height;
               break;

  }//end switch 


  _spz->_inform_list->callPreZoom(_spz, NULL, SeisPlot::Up);

  stat = _image->processZoom(PlotImage::Z_UP,x1,y1,x2,y2,True);
  assert(stat == PlotImage::PlotSuccess);

  //_spz->redraw();
  
  SeisScrWin  *scrwin= _spz->_widget_manager->scrolledWindow();
  if (scrwin) 
    {
    scrwin->redrawAnnotation();
    }

  _spz->_inform_list->callPostZoom(_spz, SeisPlot::Up);
}

