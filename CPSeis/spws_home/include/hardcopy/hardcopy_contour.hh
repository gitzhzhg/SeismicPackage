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

#ifndef HARDCOPYC_CONTOUR_H
#define HARDCOPYC_CONTOUR_H

#include "hardcopy/hardcopy_plot.hh"

#define MAX_CGM_COLORS 4096
#define MAX_CONTOURS 20

class SeisPlot;
class AmplitudeRecovery;

class HardCopyContour{

  private:
    HardCopyPlot           *_hc;
    SeisPlot               *_sp;
    float                  _rgbs[100];
    long                   _num_colors;
    int                    _cgm_colors[MAX_CGM_COLORS];
    unsigned char          _metric;
    float                  _dots_per_inch;
    int                    _status;
    AmplitudeRecovery      *_amp_recovery;
    float                  *_hd;
    float                  _bold_char_height;
    float                  _normal_char_height;
    float                  _bold_char_width;
    float                  _normal_char_width;
    float                  _bold_line_width;
    float                  _normal_line_width;

  protected:
    HardCopyPlot::CoordSystem  _coordinate_system;
    void setAnnotationSizes();
    int drawContours(long frame_number);
    void fillRGB();
    
  public:
    HardCopyContour(HardCopyPlot  *hc, 
                    SeisPlot      *sp,
                    float         dots_per_inch = 200.0,
                    long          frame_number = 1);
   virtual ~HardCopyContour();
   int getStatus() { return _status; }
};
#endif

