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
C      SeisPlotUnder.cc
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
C     Utility Name:  SeisPlotUnder   Underlay image class
C          Written:  93/12/09   by:  Michael L. Sherrill
C     Last revised:  
C
C          Purpose:  Creates a pixmap image that can be overlayed
C                    by a bitmap image such as seismic traces. 
C                    This class is a subclass of SeisPlot.           

C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY
C  node:                   pospsv (ultrix)
C  source code directory:  
C  library:                
C  header file:            SeisPlotUnder.h          (shared)
C  source file:            SeisPlotUnder.cc
C
C  static functions:       none
C  documented functions:   SeisPlotUnder
C
C  The user should include the above header file in compilation.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     SeisPlot.a      image.a    wproc.a
C  header files:  SeisPlotUnder.h SeisPlot.h 
C  functions:     See SeisPlotUnder.h class function definitions
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date           Author              Description
C     ----           ------              -----------
C  2. 
C  1. 93/12/09  Michael L. Sherrill      Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC */

#include "sp/seis_plot_under.hh"
#include "sp/inform_list.hh"
#include "sp/seis_drag.hh"
#include "sp/seis_scrwin.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_color.hh"
#include "sl/ximage.hh"
#include "sl/colorset_collection.hh"

#include "plot_image.hh"

//SeisInform class to enable reposting of picks in case image is dragged
class SeisDragInform: public SeisDrag {
 public:
    SeisDragInform( SeisPlot *oversp, SeisPlot *undersp) :
              SeisDrag(oversp,undersp) {}
    virtual void dragComplete(SeisPlot *sp) {
         sp->_inform_list->callDragImage(sp);
    };

};



SeisPlotUnder::SeisPlotUnder( SeisPlot *sp,
                              int      ptype,
                              int      do_multi_plane)
    : SeisPlot(sp->_widget_manager,ptype,do_multi_plane),
      _sp(sp)
{
  _type = UNDERLAY;
  _do_drag = False;
  sp->_chained_sp.add(this);
  _under_inform = new SeisPlotUnderInform(_sp,this);
  _sdi = NULL;
}


SeisPlotUnder::~SeisPlotUnder()
{
  _sp->removeChainedSP (this);
  delete _under_inform;
}

SeisPlot& SeisPlotUnder::operator=(SeisPlot& sp)
{
 SeisPlot::operator=(sp);
 return *this;
}

void SeisPlotUnder::displayPixmap()
{
  if (isPlotDisplayed()) {
         SeisPlot::displayPixmap();
         _sp->displayPixmap();
  } // end if
}


void SeisPlotUnder::setReadOut(Boolean underlay_only)
{
 Widget xloc,yloc,zloc;

  if(underlay_only)
     {
     _image._underlay_only = True;
     xloc = _sp->_image._xydisp->xloc;
     yloc = _sp->_image._xydisp->yloc;
     zloc = _sp->_image._xydisp->zloc;
     _sp->removeLocationOutput();
     _sp->_image._xydisp->xloc = xloc;
     _sp->_image._xydisp->yloc = yloc;
     _sp->_image._xydisp->zloc = zloc;
     setLocationOutput(_sp->_image._xydisp->xloc, _sp->_image._xydisp->yloc,
                       _sp->_image._xydisp->zloc);
     }
  else
     {
     _image._underlay_only = False;
     removeLocationOutput();
     _sp->setLocationOutput(_sp->_image._xydisp->xloc,_sp->_image._xydisp->yloc,
                            _sp->_image._xydisp->zloc);
     }
}


long SeisPlotUnder::load_under()
{
 long stat = 1;
 //put non byte file data reading here, iso etc.. or put in input file area
  return(stat);
}


int SeisPlotUnder::plot()
{
 int n=0;
 long stat;
 Arg arglist[22];
 long maxsize;
 
  updateUserForPlot();

  if(_sp->plotType() >= PlotImage::PlotCOLOR)return(PlotImage::ResourceFail);
  
  if(_image._user->_mode == PlotImage::PlotGRID ||
     _image._user->_mode == PlotImage::PlotHEADER)
     {
     _image._user->_tmin = _image._user->_grid_y1;
     _image._user->_tmax = _image._user->_grid_y2;
     }


  if(_image._user->_mode != PlotImage::PlotISO && 
     _image._user->_mode != PlotImage::PlotGRID &&
     _image._user->_mode != PlotImage::PlotHEADER)
     {
     stat = _image.getXys();
     if(stat != PlotImage::DEFS_OK)
         {
         strcpy(_last_errstr,"Error in getting grid defaults\n");
         return(False);
         }
     }

  if(_sp->plotType() != PlotImage::PlotISO && 
     _sp->plotType() != PlotImage::PlotGRID &&
     _sp->plotType() != PlotImage::PlotHEADER  && 
     _sp->isPlotDisplayed() == True)
     {
     stat = _sp->_image.getXys();
     if(stat != PlotImage::DEFS_OK)
        {
        strcpy(_last_errstr,"Error in getting grid defaults\n");
        return(False);
        }
     }


  if( _sp->isPlotDisplayed() && _sp->_image._user->_movie
       && _image._user->_movie )
      _image._user->_frames = _sp->_image._user->_frames;

  ////////////////////////// new ///////////////////////////////
  if (ColorsetCollection::readOnly(_col)) {
    _sp->getSeisColor()->loadToSeisPlot (this);
  }
  ////////////////////////// new ///////////////////////////////

  stat = _image.createUnderlay(&_image,&_sp->_image);
  if(stat != PlotImage::PlotSuccess)
    {
    strcpy(_last_errstr, _image.getErrorString());
    strcpy(_sp->_last_errstr, _image.getErrorString());
    return(False);
    }

  _image._underlay_only = (!_sp->isPlotDisplayed()) ? True : False;

  stat = SeisPlot::plot();
  if(!stat) 
    {
    _sp->_image._chain_image = NULL;
    return(False);
    }


/*
  maxsize = max(_image._graph_height,_sp->plottedHeight());
  _image._graph_height = _sp->_image._graph_height = maxsize;
  XtSetArg (arglist[n], XmNheight, maxsize); n++;
  maxsize = max(_image._graph_width,_sp->plottedWidth());
  _image._graph_width = _sp->_image._graph_width = maxsize;
  XtSetArg (arglist[n], XmNwidth, maxsize); n++;
  XtSetValues(_sp->imageGraphic(), arglist, n );
*/

  maxsize = max( leftBorder() + imageWidth() + rightBorder() + imageXloc(),
                 _sp->leftBorder() + _sp->imageWidth() + _sp->rightBorder() 
                + _sp->imageXloc()  );
  _image._graph_width = _sp->_image._graph_width = maxsize;
  XtSetArg (arglist[n], XmNwidth, maxsize); n++;
  maxsize = max(topBorder() + imageHeight() + bottomBorder() + imageYloc(),
                 _sp->topBorder() + _sp->imageHeight() + _sp->bottomBorder() 
                + _sp->imageYloc()  );
  _image._graph_height = _sp->_image._graph_height = maxsize;
  XtSetArg (arglist[n], XmNheight, maxsize); n++;
  XtSetValues(_sp->imageGraphic(), arglist, n );


  _dest_x = (int)_sp->imageXloc();
  _dest_y = (int)_sp->imageYloc();

  //refresh( &_sp->_image,0,0,ImageAll,ImageAll);
  _sp->redraw();

  if(!_sdi) _sdi = new SeisDragInform(_sp,this);

  if(_dest_x != 0 || _dest_y != 0) 
     _sdi->dragComplete(_sp); // repost picks in possible new position

  if(_sp->isPlotDisplayed() == False && isPlotDisplayed() == True)
     setReadOut(True);

  _color_required_plot = FALSE;
  return(True);
}


int SeisPlotUnder::scan(ScanDir dir)
{
  int stat=  SeisPlot::scan(dir);
  SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
  if (scrwin) scrwin->redrawAnnotation();
  return stat;
}


int SeisPlotUnder::plotArrayTypeToSeismic(int header_number)
{
 int stat;
 Display *dpy;
 int screen;
 float hpixels_per_inch;
 float vpixels_per_inch;
 float pwidth, pheight;
 long index;
 float x1, x2, y1, y2;
 float over_xinc, under_xinc;
 float tempv;

  if(!_sp->isPlotDisplayed()) return PlotImage::ResourceFail;

  _sp->_image._underlay_only = False;
  updateUserForPlot();

  _user->setRightToLeft(False);

  index = _sp->_image.getFirstTraceInImageIndex() *
          _sp->_image.getNumberOfHeaders() + header_number - 1;
  x1 = _sp->_image._hd[index];
  index = ( _sp->_image.getFirstTraceInImageIndex() +
            _sp->_image.getNumberDisplayedTraces() - 1)
        * _sp->_image.getNumberOfHeaders() + header_number - 1;
  x2 = _sp->_image._hd[index];
  y1 = _sp->_image._tmin;
  y2 = _sp->_image._tmax;

  if(!_sp->rToL())
    {
     _image._x_value_per_pixel = (x2 - x1)  
                              / ((_sp->plottedNplt() - 1) * _sp->traceWidth());
     over_xinc = x2 - x1;
     }
  else
    {
     _image._x_value_per_pixel = (x1 - x2)
                              / ((_sp->plottedNplt() - 1) * _sp->traceWidth());
     over_xinc = x1 - x2;
    }

  

  //make sure grids are going the same direction
  under_xinc = gridX2() - gridX1();

  if( (over_xinc > 0.0 && under_xinc < 0.0) ||
      (over_xinc < 0.0 && under_xinc > 0.0))
    {
    tempv = gridX1();
    setGridX1(gridX2());
    setGridX2(tempv);
    }

  _image._y_value_per_pixel = (y2 - y1)
                           / _sp->_image._ximage.height;


  if(_image._x_value_per_pixel == 0.0F || _image._y_value_per_pixel == 0.0F)
    {
    printf("SeisPlotUnder::plotArrayTypeToSeismic error in set up params\n");
    return PlotImage::ResourceFail;
    }


  pwidth = (gridX2() - gridX1()) / _image._x_value_per_pixel+ _sp->traceWidth();
  pheight= (gridY2() - gridY1()) / _image._y_value_per_pixel;
  if(pwidth < 0)pwidth = (-pwidth);
  if(pheight< 0)pheight= (-pheight);

  dpy = XtDisplay(_sp->imageGraphic());
  screen = DefaultScreen( XtDisplay(_sp->imageGraphic()));
  hpixels_per_inch =  _sp->_image.horizontalPixelsPerInch(dpy, screen);
  vpixels_per_inch =  _sp->_image.verticalPixelsPerInch(dpy, screen);
  if(_sp->units()) //metric
    {
     hpixels_per_inch /=  2.54;
     vpixels_per_inch /=  2.54;
   }
  pwidth =  ((float)(pwidth-1))  / hpixels_per_inch;
  pheight=  ((float)(pheight-1)) / vpixels_per_inch;
  setGridWidth(pwidth);
  setGridHeight(pheight);

  setLeftBorder(_sp->leftBorder());
  setRightBorder(_sp->rightBorder());
  setTopBorder(_sp->topBorder());
  setBottomBorder(_sp->bottomBorder());

  stat = plot();

  return stat;

}



void SeisPlotUnder::plotGridToSeismic(int header_number)
{
 Display *dpy;
 int screen;
 float hpixels_per_inch;
 float vpixels_per_inch;
 float pwidth, pheight;
 long index;


  if(!_sp->isPlotDisplayed()) return;


  setPlotType(PlotImage::PlotGRID);
  updateUserForPlot();
  setSeisAnnotation(False);

  index = _sp->_image.getFirstTraceInImageIndex() *
          _sp->_image.getNumberOfHeaders() + header_number - 1;
  setGridX1(_sp->_image._hd[index]);
  index = ( _sp->_image.getFirstTraceInImageIndex() 
           + _sp->_image.getNumberDisplayedTraces() - 1)
        * _sp->_image.getNumberOfHeaders() + header_number - 1;
  setGridX2(_sp->_image._hd[index]);
  setGridY1(_sp->_image._tmin);
  setGridY2(_sp->_image._tmax);

  dpy = XtDisplay(_sp->imageGraphic());
  screen = DefaultScreen( XtDisplay(_sp->imageGraphic()));
  hpixels_per_inch = _sp->_image.horizontalPixelsPerInch(dpy, screen);
  vpixels_per_inch = _sp->_image.verticalPixelsPerInch(dpy, screen);
  if(_sp->units()) //metric
     {
     hpixels_per_inch /=  2.54;
     vpixels_per_inch /=  2.54;
     }
  pwidth =  (float)_sp->_image._ximage.width  / hpixels_per_inch;
  pheight=  (float)_sp->_image._ximage.height / vpixels_per_inch;
  setGridWidth(pwidth);
  setGridHeight(pheight);

  setLeftBorder(_sp->leftBorder());
  setRightBorder(_sp->rightBorder());
  setTopBorder(_sp->topBorder());
  setBottomBorder(_sp->bottomBorder());

  plot();

}

void SeisPlotUnder::plotSeismicToGrid(int)
{
//later
}

void SeisPlotUnder::hide()
{
 long width, height;

  if( !_image.getCurrentPixmap() || !_sp->_image._chain_image) return;

  width = max( _sp->plottedWidth(), _sp->_image._chain_image->_graph_width);
  height= max( _sp->plottedHeight(),_sp->_image._chain_image->_graph_height);
  _sp->_image._dest_x = 0;
  _sp->_image._dest_y = 0;
  _sp->_image._chain_image = NULL;
  _sp->redraw(0,0,width,height);
  _sdi->dragComplete(_sp); //this will cause any picks to be reposted
  _image._displayed = False;
}



void SeisPlotUnder::show()
{
 long width, height;

  if( !_image.getCurrentPixmap()) return;

  _sp->_image._chain_image = &_image;
  _image._displayed = True;
  width = max( _sp->plottedWidth(), _sp->_image._chain_image->_graph_width);
  height= max( _sp->plottedHeight(),_sp->_image._chain_image->_graph_height);
  _sp->_image._dest_x = _dest_x;
  _sp->_image._dest_y = _dest_y;
  _sp->redraw(0,0,width,height);
  _sdi->dragComplete(_sp); //this will cause any picks to be reposted

}


void SeisPlotUnder::drag()
{
  if(_sp->_image._chain_image == NULL) return;
  _sdi->startDrag();
}


void SeisPlotUnder::expose(Widget,
                           XtPointer,
                           XmDrawingAreaCallbackStruct *CBdata)
{

  XExposeEvent *ev;
  ev= (XExposeEvent *)CBdata->event;

  if( store_events(ev) ) return;

    if( (ev->x + ev->width > _image._left_border &&
         ev->x < _image._graph_width + _image._left_border) ||
        (ev->y + ev->height> _image._top_border &&
         ev->y < _image._graph_height + _image._top_border)) 
       {
       if( _image._displayed &&  _image.getCurrentPixmapIndex()) 
          {
          if(!_sp->_image._displayed) //underlay only so refresh it
             _sp->_image.refresh(ev->x, ev->y, ev->width, ev->height);
          _inform_list->callExpose(this, ev->x, ev->y, ev->width, ev->height);
	  }
       }
}


void SeisPlotUnder::getVisibleArea(int *x, int *y, int *width, int *height)
{
  SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
  if (scrwin) scrwin->getVisibleArea(x,y,width,height);
  else {
     *x= 0;
     *y= 0;
     *width= 0;
     *height= 0;
  }
}

SeisPlot *SeisPlotUnder::overlaySP()
{
  return _sp;
}

/*
 * ====================== Public Method ==============================
 *  return True if this SeisPlot is current in the window
 */
Boolean SeisPlotUnder::isCurrentInWindow()
{ 
  assert (_widget_manager);
  Boolean retval= (_widget_manager->currentSP() == _sp);
  return retval;
}

SeisPlotUnderInform::~SeisPlotUnderInform()
{
}

void SeisPlotUnderInform::newPlot(SeisPlot *)
{
  _undersp->setReadOut(False);
}
