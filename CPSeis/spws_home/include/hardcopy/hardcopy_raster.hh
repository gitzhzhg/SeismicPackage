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
//===========================================================================
//===================== Hardcopy annotation class ===========================
//===================== M.L. Sherrill 03/96       ===========================
//===========================================================================

#ifndef HARDCOPYC_RASTER_H
#define HARDCOPYC_RASTER_H

#include <X11/Intrinsic.h>
#include "hardcopy/hardcopy_plot.hh"

#define MAX_CGM_COLORS 4096

class SeisPlot;

class HardCopyRaster{

  private:
    HardCopyPlot        *_hc;
    SeisPlot            *_sp;
    float               *_rgbs;
    long                _num_colors;
    int                 _cgm_colors[MAX_CGM_COLORS];
    unsigned char       *_map_array;
    unsigned char       *_raster_array;
    float charHeight    (unsigned char bold);
    float charWidth     (unsigned char bold);
    unsigned char       _metric;
    float               _dots_per_inch;
    unsigned int        _raster_width;
    unsigned int        _raster_height;
    int                 _status;
    unsigned int        _contiguous;

  protected:
    HardCopyPlot::CoordSystem  _coordinate_system;
    int rasterizeTraces(long frame_number);
    int rasterizeFloats(long frame_number);
    int rasterizeNonPip(long frame_number);

  public:

    HardCopyRaster(HardCopyPlot  *hc, 
                   SeisPlot      *sp,
                   long          num_colors,
                   float         *rgbs,
                   unsigned char *map_array,
                   float         x1 = 0.0,
                   float         y1 = 0.0,
                   float         x2 = 0.0,
                   float         y2 = 0.0,
                   float         dots_per_inch = 200.0,
                   long          frame_number = 1,
                   Boolean       use_pip_extensions = True);
 
   virtual ~HardCopyRaster();
   void drawColorBar(float x,     float y, 
                     float width, float height,
                     int   deci_places);
   void drawSquare9(float x, float y, float width, float height);
   const unsigned char* getRasterData(){ return _raster_array; }    
   int getStatus() { return _status; }
};
#endif

