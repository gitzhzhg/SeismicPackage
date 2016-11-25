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
C      SeisPlotZoom.cc
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
C     Utility Name:  SeisPlotZoom   Zoom Class
C          Written:  93/09/28  by:  Michael L. Sherrill
C     Last revised:  
C
C          Purpose:  Creates a scrolled window and drawing area to allow
C                    an application to zoom into a separate window. 
C                    This class is a subclass of SeisPlot.           

C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY
C  node:                   pospsv (ultrix)
C  source code directory:  
C  library:                
C  header file:            SeisPlotZoom.h          (shared)
C  source file:            SeisPlotZoom.c
C
C  static functions:       none
C  documented functions:   SeisPlotZoom
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     SeisPlot.a     image.a    wproc.a
C  header files:  SeisPlotZoom.h SeisPlot.h 
C  functions:     See SeisPlotZoom.h class function definitions
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date           Author              Description
C     ----           ------              -----------
C  2. 
C  1. 93/09/28  Michael L. Sherrill      Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC */

#include "sp/seis_plot_zoom.hh"
#include "sp/seis_plot_zoom_under.hh"
#include "sp/seis_zoomer.hh"
#include "sp/inform_list.hh"
#include "sp/seis_inform.hh"
#include "image_amplitude_recovery.hh"
#include "cprim.h"
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>


//this class establishes link to the zoomer class for handling the
//zoom actions etc.
class ZoomInformSeisZoomer : public SeisZoomer {
 public:
  ZoomInformSeisZoomer( SeisPlot *sp,
                    SeisPlotZoom *zoomsp,
                    float zoom_factor = 2.0,
                    int   zoom_type   = SpPointer) :
                    SeisZoomer(sp,zoomsp,zoom_factor,zoom_type){}
  virtual void zoomComplete( SeisPlot *sp, SeisPlot *zsp,
                             SeisPlot::ZoomDir direction,
                             int) {
         if (direction == SeisPlot::Up) {
             sp->_inform_list->callPostZoom(sp, SeisPlot::UpSeparateWin);
             sp->_inform_list->callPostZoomSeparateWindow(sp, zsp);
         }
         else
             sp->_inform_list->callPostZoom(sp, SeisPlot::Abort);
      }
};


// this class exists for the purpose of deleting any separate zoom
// window if data in memory has changed
class ZoomInform : public SeisInform {
  private:
     SeisPlotZoom  *_zoomsp;
  public:
     ZoomInform( SeisPlot *sp, SeisPlotZoom *zoomsp)
              : SeisInform(sp) , _zoomsp(zoomsp) {};
     virtual void newPlot(SeisPlot * /*sp*/)
                   {
                     delete _zoomsp->_spp;
                     _zoomsp->_informerDestroying = True;
                     //sp->delInformer(this);
                     delete _zoomsp;
                     delete this;
                   };
     virtual void noPlotDisplayed(SeisPlot * /*sp*/)
                   {
                     delete _zoomsp->_spp;
                     _zoomsp->_informerDestroying = True;
                     //sp->delInformer(this);
                     delete _zoomsp;
                     delete this;
                   };
     virtual void postScan(SeisPlot * /*sp*/, SeisPlot::ScanDir )
                   {
                     delete _zoomsp->_spp;
                     _zoomsp->_informerDestroying = True;
                     // sp->delInformer(this);
                     delete _zoomsp;
                     delete this;
                   };
     virtual void destroyed(SeisPlot * /*sp*/)
                   {
                     delete _zoomsp->_spp;
                     _zoomsp->_informerDestroying = True;
                     //sp->delInformer(this);
                     delete _zoomsp;
                     delete this;
                   };
};

    


//------------- SeisPlot class that holds the zoomed image ----------

SeisPlotZoom::SeisPlotZoom( Widget   p,
                            char     *name,
                            SeisPlot *sp,
                            Boolean  do_scrolled ) :
  SeisPlot ((const Widget)p, (const char *)name,
    (const Boolean)do_scrolled, (const Boolean)True),
  _sp(sp)
//            : SeisPlot(p,name), _sp(sp) // problem zoom sep w/underlay
{
  _image.setPointToData(True);
  _image.setPointToHeaders(True);
  _image.setColorInfoStructure(_col); // new for zoom with underlay
  _informerDestroying = False; 
  _needToDeleteZoomer = True;
  setSharingXResources();
}

SeisPlotZoom::~SeisPlotZoom()
{
 
  for(int i=0; i<MAX_PIXMAP; i++)
     {
     if(_image.getPixmapArray(i) != 0)
        {
        XFreePixmap(XtDisplay(_image.graphicWidget()),
                              _image.getPixmapArray(i));
        _image.setPixmap(i,0);
        }
     }


  if(_informerDestroying == False)
     {   
     delete _zinform;
     }

  if(_needToDeleteZoomer)
    {
    delete _dozoom;
    }


  delete _image._xydisp;  _image._xydisp = NULL;

}


SeisPlot& SeisPlotZoom::operator=(SeisPlot& sp)
{
 SeisPlot::operator=(sp);
 return *this;
}

void SeisPlotZoom::zoomUp()
{
 int ztype;

  if (_sp->isPlotDisplayed()) 
     {
     _zoom_type = _sp->zoomUpType();
     if (_zoom_type == Box)     ztype= SpArea;
     if (_zoom_type == Pointer) ztype= SpPointer;
     }


  _dozoom= new ZoomInformSeisZoomer(_sp, this, _zoom_factor, ztype);
  _zinform = new ZoomInform( _sp, this);
  //  _sp->addInformer(_zinform); //adds to inform list
 
  if (_zoom_type == SpArea) _dozoom->setPixelSize(_box_size);

  _sp->_inform_list->callPreZoom(_sp, _dozoom, SeisPlot::Up);

  for(int i=0; i<MAX_PIXMAP; i++)
     {
     _image.setPixmap(i,0);
     }

  _dozoom->startZoom();
  load_zoom(); 
  _dozoom->setZoomWindow(this);
}



void SeisPlotZoom::load_zoom()
{
 long i;


  for(i=0; i<MAX_PIXMAP; i++) 
     {
     if(_image.getPixmapArray(i) != 0) 
        {
        XFreePixmap(XtDisplay(_image.graphicWidget()),_image.getPixmapArray(i));
        _image.setPixmap(i,0);
        }
     }

  _zoom_draw = _image.graphicWidget();
  XYdisp *tmpxy= _image._xydisp;
  _image.imageFreeXresources();
  _image._xloc = NULL;
  _image._yloc = NULL;
  //delete _image._xydisp; _image._xydisp = NULL;
  delete _image._amp_recovery; _image._amp_recovery = NULL;
  delete _image._user; _image._user = NULL;
/*///////////////// old /////////////////////////
  memcpy(&_image, &_sp->_image, sizeof (PlotImage) );
*////////////////// old /////////////////////////
/////////////////// new /////////////////////////
  (&(_sp->_image))->memcpyThis (&_image);
/////////////////// new /////////////////////////
  _image.setSharingResources(True); 
  _image.getMouseReadout()->override_x_str= newstr(
                        _sp->_image.getMouseReadout()->override_x_str);
  _image.getMouseReadout()->override_y_str= newstr(
                        _sp->_image.getMouseReadout()->override_y_str);
  _image._user = NULL;
  memcpy(&_user, &_sp->_user, sizeof (ImageInput) );
  _image._user = _user;
  _image._xydisp= tmpxy;
  _image._xydisp->mouse_readout_type = _sp->_image._xydisp->mouse_readout_type;
  _image.setImageXYOutputFunction((ImageXYOutputFunction)xyUpdate, this);
  _image._graphic = _zoom_draw;  
  _image.setPointToData(True);
  _image.setPointToHeaders(True);
  setTransform( _sp->transform() );

  //setLeftBorder(  max(91,_sp->leftBorder())); 
  //setRightBorder( max(91,_sp->rightBorder())); 
  setTopBorder(   _sp->topBorder()); 
  setBottomBorder(_sp->bottomBorder()); 

  if(_image._chain_image != NULL)
     {
     _sp->_image._chain_image->memcpyThis (_image._chain_image);

     _image._chain_image->setSharingResources (True);
     memcpy(_image._chain_image->_user, _sp->_image._chain_image->_user,
       sizeof(ImageInput));
     _image._chain_image->_graphic = _zoom_draw;
     _image._chain_image->_over_image = &_image;
     _image._chain_image->_point_to_data = True;
     _image._chain_image->_point_to_headers = True;
     }

  for(i=0; i<MAX_PIXMAP; i++)
     {
     _image._pixmary[i] = 0;
     //if(_image._chain_image != NULL) _image._chain_image->_pixmary[i] = 0;
     }
}
