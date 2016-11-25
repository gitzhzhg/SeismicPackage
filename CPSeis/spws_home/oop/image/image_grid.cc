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
/*------------------------------------------------------------------
 *USER DOC
 *Name   : imageGrid 
 *Purpose: Set up a user defined grid image for use in model picking.
 *                            
 *Author : Michael L. Sherrill
 *Date   : 09/92 (C++ version 4/97)
 *
 * Function Definition:
 * long imageGrid();
 *
 *
 *NOTES:
 * 1. 
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/
 
#include "plot_image.hh"
#include "xy_display.hh"

#define nograph "Could not identify the drawing area widget."

long PlotImage::imageGrid()

{
long stat;      
Display *dpy;
int screen;
float hpixels_per_inch;
float vpixels_per_inch;
float original_ti;
float original_is;


/*make sure we have a good display widget*/
  if( (dpy = XtDisplay(_graphic)) == NULL)
     {
     strcpy(_errstr,nograph);
     return(ResourceFail);
     } 

  screen = DefaultScreen(XtDisplay(_graphic));
  hpixels_per_inch =  horizontalPixelsPerInch(dpy, screen);
  vpixels_per_inch =  verticalPixelsPerInch(dpy, screen);
  if(_user->getMetricMode()) 
     {
     hpixels_per_inch /=  2.54;
     vpixels_per_inch /=  2.54;
     }


/*populate user input parameters*/
  if(_user->getMode() != PlotHEADER)_user->setMode(PlotGRID);
  if(!_zoomed) 
    {
    _grid_x1        = _user->getX1();
    _grid_x2        = _user->getX2();
    _grid_y1        = _user->getY1();
    _grid_y2        = _user->getY2();
    _user->setVelocityMinimum(_user->getX1());
    _user->setVelocityMaximum(_user->getX2());
    _user->setTmin(_user->getY1());
    _user->setTmax(_user->getY2());
    _user->setXgridIncrement( (_user->getX2() - _user->getX1()) 
                             / (_user->getGridWidth() - 1)); 
    _user->_G.tstrt  = min(_user->getY1(),_user->getY2());
    original_ti           = _user->getTracesPerInch();
    original_is           = _user->getInchesPerSecond();
    _zindex = NO_ZOOM;
    _zoomxary[_zindex][0] = _user->getX1();
    _zoomxary[_zindex][1] = _user->getX2();
    _zoomyary[_zindex][0] = _user->getY1();
    _zoomyary[_zindex][1] = _user->getY2();
    }
  else 
    {
    _grid_x1        = _zoomxary[_zindex][0];
    _grid_x2        = _zoomxary[_zindex][1];
    _grid_y1        = _zoomyary[_zindex][0];
    _grid_y2        = _zoomyary[_zindex][1];
    if(getManualTransformX() || getManualTransformY())
      {
      _manual_grid_x1 = _grid_x1;
      _manual_grid_x2 = _grid_x2;
      _manual_grid_y1 = _grid_y1;
      _manual_grid_y2 = _grid_y2;
      }
    _user->setVelocityMinimum(_grid_x1);
    _user->setVelocityMaximum(_grid_x2);
    _user->setTmin(_grid_y1);
    _user->setTmax(_grid_y2);
    _user->setXgridIncrement( (_grid_x2 - _grid_x1)
                             / (_user->getGridWidth() - 1));
    _user->_G.tstrt  = min(_grid_y1,_grid_y2);
    original_ti           = _user->getTracesPerInch();
    original_is           = _user->getInchesPerSecond();
    }

  _user->setTracesPerInch(((float)_user->getGridWidth())  / hpixels_per_inch);
  _user->setInchesPerSecond(((float)_user->getGridHeight()) / vpixels_per_inch
                            /  (max(_user->getY1(),_user->getY2()) -
                                min(_user->getY1(),_user->getY2())));
  if(_zoomed && _zindex == NO_ZOOM)
     { /*was zoomed now going to original*/
     _is           = _user->getInchesPerSecond();
     _ti           = _user->getTracesPerInch();
     }
  if(_user->getMode() != PlotHEADER)_user->setNumberToPlot(1);
  _user->setInitialSkip(0);
  _user->setNumberToDo(1);
  _user->setNumberToSkip(0);
  _user->setTdec(1);
  _user->_G.ntrfil = (int)_user->getNumberToPlot();
  _nhdrs = 64; 
  _user->_G.nhdwd  =  64;
  _user->_G.srval  = .1;
  _user->_G.nbydp  = 0;
  if(!_user->getPrimaryAnnotationHeader())
     {
     _user->setPrimaryAnnotationHeader(1);
     _user->setFirstTraceToAnnotate(1);
     _user->setXlabelIncrement(20);
     }
      

/*set pixmap to beginning*/
  _cpixm = 0;


/*see if we have enough memory*/
  stat = checkSize();

  _user->setTracesPerInch(original_ti);
  _user->setInchesPerSecond(original_is);
  if(stat != PlotSuccess) return(stat);

  _velfactor = (_ximage.width - 1)
                   / (_vel_max - _vel_min);
  _x_value_per_pixel = (_grid_x2 - _grid_x1)
                           /  (_ximage.width - 1);

/*clear the window if there is already an image in it*/
  if(_displayed) XClearWindow( dpy, XtWindow( _graphic) );

/*create the image area*/
  stat = plot();
  if(stat != PlotSuccess) return(stat);


/*set the mouse into motion*/
  if(_xydisp->xloc && _xydisp->yloc)
     setXYout(_xydisp->xloc, 
               _xydisp->yloc, _xydisp->zloc, XYAUTO ); 


/***********
  XSetForeground(dpy, image->gc1, image->overlay_pixel);
  XSetBackground(dpy, image->gc1, image->white_pixel);
  XFillRectangle(dpy, image->pixmary[image->cpixm], image->gc1,
                  I_gphLeftBorder(image),I_gphTopBorder(image),
                  image->ximage.width, image->ximage.height);
***********/


/*annotate like non-seismic grid*/
  if(_user->getAnnotateImage() == False && _user->_mode != PlotHEADER)
    {
    annotateGrid(_grid_x1,_grid_x2, _grid_y1,_grid_y2);
    refresh( 0, 0, ImageAll, ImageAll);
    }


  return(stat);

}
