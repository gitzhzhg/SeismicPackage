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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================



/*------------------------------------------------------------------
 *USER DOC
 *Name   : initImage 
 *Purpose: Initialize graphics context and fonts to be used in the image
 *                                  
 *Author : Michael L. Sherrill
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:
 * void initImage (         Widget            Graphic,
                            char              *graph_font,
                            struct COLOR_INFO *col,
                            struct COLOR_INFO *col_two,
                            Boolean           frame_buff)
 *
 * Graphic       in     Widget id
 * col           in     Structure defined in image.h 
 * col_two       in     Secondary color structure in image.h
 * frame_buff    in     Flag to indicate a frame buffered device
 *
 *NOTES:
 * 1.
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/
 
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "plot_image.hh"
#include "xy_display.hh"
//////////// new ////////////////////////
#include "pixmap_set.hh"
#include "sl/paintset_collection.hh"
#include "sl/color_info_set.hh"
#include "sl/ximage.hh"
//////////// new ////////////////////////

#define nomem "Not enough memory for velocity array."

void PlotImage::initImage( Widget            Graphic,
                char              *graph_font,
                char              *small_graph_font,
                struct COLOR_INFO *col,
                struct COLOR_INFO *col_two,
                Boolean           frame_buff)


{
Display *dpy;
Screen *scr;
XGCValues values;
Window rootw;
long i, maxwidth;

   _graphic = Graphic;
   strcpy(_graph_font, graph_font);
   dpy = XtDisplay(_graphic);
   scr = XtScreen(_graphic);
   rootw= RootWindowOfScreen(scr);
   
/*default user structure members*/
   _user->setY1(0.0);
   _user->setY2(0.0);
   _user->setX1(0.0);
   _user->setX2(0.0);
   _user->setMatchXheader(1);
   _user->setMatchYheader(8);
   _user->setAnnotateImage(True);
   _user->setDrawXlines(False);
   _user->setDrawYlines(True);
   _user->setPrimaryAnnotationHeader(1);
   _user->setSecondaryAnnotationHeader(37);
   _user->setXLabelHeader(1);
   _user->setPrimaryTimingLine(1.0);
   _user->setSecondaryTimingLine(0.5);
   _user->setUserHeaderAnnotation(True);
   _user->setSymetricalAnnotation(False);
   _user->setActiveHeader(0);
   _user->setYannotationHeader(0);
   _user->setNorm(PANELNORM);
   _user->setScaleToFile(False);
   _user->setScaleToPanel(True);
   _user->setTmin(0.0);
   _user->setTmax(1.0);
   _user->setTdec(1);
   _user->setPlotLabelX(0);
   _user->setPlotLabelY(0);
   _user->setLabelPlotOnly(False);
   _user->setExternalAmplitude(0.0);
   _user->_metric= False;
   _user->_invert_yaxis = False;
   _user->_RtoL = False;
   _user->_depth = False;
   _user->_rp = False;
   _user->_movie = False;
   _user->setReadRequestArray(NULL);
   _user->setNumberOfReadRequests(1);
   _user->setNumberOfContours(0);
   _user->setContourMinimumVelocity(0.0);
   _user->setContourMaximumVelocity(0.0);
   _user->setContoursOnly(False);
   _user->setPlotWithContours(False);
   _user->setMode(PlotWFILL);
   _user->setDoColor(False);
   _user->setDoMedian(False);
   _user->setDoPercent(True);
   _user->setDoAmps(False);
   _user->setPnc(100);
   _user->setPpc(100);
   _user->setColorAmpmin(0.0);
   _user->setColorAmpmax(1.0);
   _user->setNumberCbarColors(32);
   _user->setOverlayIncrement(1);
   _user->setNumberToDo(0);   
   _user->setNumberToPlot(1);
   _user->setInitialSkip(0);
   _user->setNumberToSkip(0);
   _user->setGradeVertical(True);
   _user->setGradeHorizontal(True);
   _user->setTracesPerInch(20.0);
   _user->setInchesPerSecond(2.0);
   _user->setChannelsPerTrace(4.0);
   _user->setClippingFactor(1.0);
   _user->setFirstTraceToAnnotate(1);   
   _user->setNumberYlabels(0);
   _user->setXlabelIncrement(20);
   _user->setHaveColorData(False);
   _user->setXgridIncrement(1.0);
   _user->setYgridIncrement(1.0);
   _user->setGridWidth(0);
   _user->setGridHeight(0);
   _user->setVelocityMinimum(0.0);
   _user->setVelocityMaximum(1.0);
   _user->setVelocityArraySize(0);
   _user->_G.nbydp = 0;
   _user->setHiResolution(0);
   _user->setCenterPercent(0);
   _user->setSkipFrames(0);
   _user->setFrames(0);
   strcpy(_user->_plotLabel,"");
   strcpy(_user->_filename,"");
   strcpy(_user->_G.ftyp,"");
   for(i=0;i<1024;i++)
     {
     _user->setCbarRgb(i,0.0);
     _user->setContourCbarRgb(i, 0.0);
     }
/*default image members*/
   _coordinate_header = 1;
   _neg_fill = 0;
   _errstr[0]= '\0';
   _statusw= NULL;
   _displayed= False;
   _use_old_arrays = False;
   _traces_per_group = 0;
   _chain_image = NULL;
   _over_image  = NULL;
   _zindex = NO_ZOOM;
   _zoom_factor = 2.0;
   _dest_x = 0;
   _dest_y = 0;
/////////////////////////// new //////////////////////////
   _white_pixel     = PaintsetCollection::white (scr);
   _black_pixel     = PaintsetCollection::black (scr);
   _overlay_pixel   = _black_pixel;
   _grid_pixel      = _white_pixel;
/////////////////////////// new //////////////////////////
/*
   _white_pixel     = WhitePixelOfScreen(scr);
   _black_pixel     = BlackPixelOfScreen(scr);
   _overlay_pixel   = BlackPixelOfScreen(scr);   
   _grid_pixel      = WhitePixelOfScreen(scr);
*/
   _manual_annotate = False;
   _manual_grid_x1  = 0.0;
   _manual_grid_x2  = 0.0;
   _manual_grid_y1  = 0.0;
   _manual_grid_y2  = 0.0;
   _manual_scaler   = 1.0;
   _underlay_only   = True;
   _filedata        = True;
   _point_to_data   = False;
   _point_to_headers= False;
   _can_overlay     = False;
   _byte_array      = NULL;
   _float_array     = NULL;
   _hd              = NULL;
   _vary            = NULL;
   _apply_smoother  = 0;

/*
/////////////////////////// old //////////////////////////
   //The application and X itself will set most of these appropriately but
   //I am getting a Purify unitialized warning from some unknown member
   //in X land, so I will initialize them here even though X claims is
   //does this for us.
   _ximage.width            = 0;
   _ximage.height           = 0;
   _ximage.xoffset          = 0;
   _ximage.format           = XYBitmap;
   _ximage.data             = NULL;
   _ximage.byte_order       = LSBFirst;
   _ximage.bitmap_unit      = 8;
   _ximage.bitmap_bit_order = LSBFirst;
   _ximage.bitmap_pad       = 8;
   _ximage.depth            = 1;
   _ximage.bytes_per_line   = 8;
   _ximage.bits_per_pixel   = 0;
   _ximage.red_mask         = 0L;
   _ximage.green_mask       = 0L;
   _ximage.blue_mask        = 0L;
   _ximage.obdata           = NULL;
/////////////////////////// old //////////////////////////
*/   


   _xloc            = NULL;
   _yloc            = NULL;
   _cpixm           = 0;
   _fixedcharwidth  = 0;
   _boldcharwidth   = 0;
   _xydisp->outfunc  = NULL;
   _xydisp->override_x_str  = NULL;
   _xydisp->override_y_str  = NULL;
   _xydisp->interpolate_readout = True;
   _xydisp->not_defined_value   = 0.0;
   _xydisp->init = False;
   _xydisp->xloc = NULL;
   _xydisp->yloc = NULL;
   _xydisp->zloc = NULL;
   _xydisp->status = XYOFF;
   _xydisp->mouse_readout_type = MOUSE_AMP;
   _xydisp->setXReadoutHeader(-1);
   _xydisp->setAltYReadoutHeader(-1);
   setImageAbortFunction(NULL,NULL);
   setImageXYOutputFunction (NULL, NULL);
   setImageExternalFunction(NULL,NULL);
   setLogarithmicXY(False, False);
   _Cl.iskp                = 0;
   _Cl.ndo                 = 0;
   _Cl.nskp                = 0;
   _Cl.axis                = 0;
   _Cl.index               = 0;
   _Cl.trnsps              = 0;
   _Cl.ntot                = 0;
   _Cl.nsamp               = 0;
   _Cl.samp1               = 0;
   _Cl.sdec                = 0;
   _Cl.trnsps              = 0;
   _Cl.cnvrt               = 0;
   _original_samples       = 0;
   _original_traces        = 0;
   _displayed_traces       = 0;
   _original_ti            = 0.0;
   _original_is            = 0.0;
   _scanright              = False;
   _scanleft               = False;
   _zoomed                 = False;
   _zoomup                 = False;
   _zoomdown               = False;   
   _zoom_scan              = False;
   _graph_width            = 0;
   _graph_height           = 0;
   _left_border            = BORDER;
   _right_border           = BORDER;
   _top_border             = BORDER;
   _bottom_border          = BORDER;
   _grid_x1                = 0.0;
   _grid_x2                = 0.0;
   _grid_y1                = 0.0;
   _grid_y2                = 0.0; 
   _sharing_resources      = False;
   _velfactor              = 1.0;
   _vel_min                = 0.0;
   _vel_max                = 1.0;
   _ntot                   = 0;
   _nsamp                  = 0;
   _tmax                   = 0.0;
   _tmin                   = 0.0;
   _nhdrs                  = 64;
   _frames                 = 0;
   _undefined_value        = 0.0;
   _current_selector_panel = 0;
   _col                    = col;
   _col_two                = col_two;
   _frame_buffer           = frame_buff;
   for(i=0;i<256;i++)_histogram[i]= 0;  
   for(i=0; (i<MAX_PIXMAP); i++) _pixmary[i] = 0;

/////////////////////////// new //////////////////////////
   _col_segment = new ColorInfoSegment (this, ColorInfoSet::PLOTIMAGE);
/////////////////////////// new //////////////////////////

/*basic image gc*/
   _gc1 = XCreateGC( dpy, rootw, None, NULL);
   XSetBackground(dpy, _gc1, _white_pixel);
   XSetForeground(dpy, _gc1, _black_pixel);

/*customize gc for wide primary timing line draws*/
   _gc2 = XCreateGC( dpy, rootw, None, NULL);
   XSetBackground(dpy, _gc2, _white_pixel);
   XSetForeground(dpy, _gc2, _black_pixel);
   XSetLineAttributes( dpy, _gc2, 2, LineSolid, CapButt, JoinMiter);

/*gc for marking picks*/
   _pick_gc = XCreateGC( dpy, rootw, None, NULL);
   XSetBackground(dpy, _pick_gc, _white_pixel);
   XSetForeground(dpy, _pick_gc, _black_pixel);
   XSetLineAttributes( dpy, _pick_gc, 2, LineSolid, CapButt, JoinMiter);

/*single plane gc*/
   _bitmap_pixmap = XCreatePixmap(dpy,rootw,16,16,1);
   values.foreground = _white_pixel;
   values.background = _black_pixel;
   _bitmap_gc1 = XCreateGC(dpy, _bitmap_pixmap, 
                         GCForeground | GCBackground, &values);
   values.foreground = 1;
   values.background = 0;
   _bitmap_gc2 = XCreateGC(dpy, _bitmap_pixmap,
                         GCForeground | GCBackground, &values);
   XSetLineAttributes(dpy,_bitmap_gc2,2,LineSolid,CapButt,JoinMiter);


/*use small font for basic annotations*/
   _font_fixed = XLoadQueryFont(dpy,small_graph_font);
   if (_font_fixed != NULL)  /*found custom font*/
     {
     XSetFont(dpy,_gc1,_font_fixed->fid);
     XSetFont(dpy,_bitmap_gc1,_font_fixed->fid);
     _fixedcharwidth = _font_fixed->max_bounds.width;
     _fixedcharheight = _font_fixed->max_bounds.ascent
                            + _font_fixed->max_bounds.descent;
     }
     else
     {
     fprintf(stderr, 
              "Could not load small font %s - using fixed font\n", graph_font);
     _font_fixed = XLoadQueryFont(dpy,"fixed");
     assert(_font_fixed);
     XSetFont(dpy,_gc1,_font_fixed->fid);
     XSetFont(dpy,_bitmap_gc1,_font_fixed->fid);
     _fixedcharwidth = _font_fixed->max_bounds.width;
     _fixedcharheight = _font_fixed->max_bounds.ascent
                            + _font_fixed->max_bounds.descent;
     }

/*try to load custom bold font*/
   _font_bold = XLoadQueryFont(dpy, graph_font);
   if (_font_bold != NULL)     /*found custom font*/
    {
    XSetFont(dpy,_gc2,_font_bold->fid);
    XSetFont(dpy,_bitmap_gc2,_font_bold->fid);
    _boldcharwidth = _font_bold->max_bounds.width;
    _boldcharheight = _font_bold->max_bounds.ascent
                           + _font_bold->max_bounds.descent;
    }
   else                      /*use fixed font for bold*/
    {
    fprintf(stderr, 
             "Could not load bold font %s - using fixed font\n", graph_font);
    _font_bold = XLoadQueryFont(dpy,"fixed");
    assert(_font_fixed);
    XSetFont(dpy,_gc2,_font_fixed->fid);
    XSetFont(dpy,_bitmap_gc2,_font_fixed->fid);
    _boldcharwidth = _font_fixed->max_bounds.width;
    _boldcharheight= _font_fixed->max_bounds.ascent
                         + _font_fixed->max_bounds.descent;
    }

                            /* initialize borders */
    maxwidth = max(_fixedcharwidth,_boldcharwidth);
    maxwidth = 13 * maxwidth;
    _left_border = max(maxwidth,BORDER);
    _right_border = max(maxwidth,BORDER);
    _top_border = BORDER;
    _bottom_border = BORDER;
}




/***********************************************************************
 ****************         allocate velocity array member ***************
 ***********************************************************************/

int PlotImage::allocateVelocityArray( int size)

{
 char errstr[40];

  _user->setVelocityArraySize(size);

  if(_vary == NULL)
     _vary = (float *) calloc(1,
              (int)(_user->getVelocityArraySize()*(2*sizeof(float))));
  else
     _vary = (float *) realloc(_vary,
              (int)(_user->getVelocityArraySize()*(2*sizeof(float))));

  if(_vary == NULL)
     {
     printf("In allocateVelocityArray --\n\
             Not enough memory for velocity array\n");
     strcpy(errstr,nomem);
     if (_statusw) wprocPopMsg(  _statusw, errstr);
     return False;
     }

  return True;
}

/////////////////////////// new //////////////////////////
void PlotImage::colorInfoInit (ColorInfo *col, ColorInfo *col_two)
{
  _col = col;
  _col_two = col_two;
   
  ColorInfoSet *set;
  set = ColorInfoCollection::fetchExisting (_col);
  set->addSharedElement (_col_segment);

  set = ColorInfoCollection::fetchExisting (_col_two);
  set->addSharedElement (_col_segment);

  _ximage_ptr = new Ximage (_col);
  _ximage_ptr->assignTo (&_ximage);
  _pixmap_set = new PixmapSet (this, _ximage_ptr, &_cpixm, _pixmary,
    MAX_PIXMAP);
}

// Do not permit the memcpy to clobber the objects contained in given target
void PlotImage::memcpyThis (PlotImage *to)
{
  Ximage *ximage_ptr;
  ColorInfoSegment *col_segment;
  PixmapSet *pixmap_set;
  int recover;

  // remember the objects contained in given target
  if (to->_ximage_ptr != NULL) {
    ximage_ptr  = to->_ximage_ptr;
    col_segment = to->_col_segment;
    pixmap_set  = to->_pixmap_set;
    recover = TRUE;
  }
  else {
    assert (to->_col_segment == NULL && to->_pixmap_set == NULL);
    recover = FALSE;
  }

  memcpy (to, this, sizeof(PlotImage));
  if (recover == TRUE) {
    ximage_ptr->copyData (_ximage_ptr);

    // restore the objects contained in given target
    to->_ximage_ptr  = ximage_ptr;
    to->_col_segment = col_segment;
    to->_pixmap_set  = pixmap_set;
  }
}
/////////////////////////// new //////////////////////////
