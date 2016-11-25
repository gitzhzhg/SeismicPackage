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

#ifndef HARDCOPYC_ANNOTATION_H
#define HARDCOPYC_ANNOTATION_H

#include "hardcopy/hardcopy_plot.hh"


class SeisPlot;

class HardCopyAnnotation{

  private:
    HardCopyPlot            *_hc_active;
    HardCopyPlot            *_hc_over;
    HardCopyPlot            *_hc_under;
    SeisPlot                *_sp_active;
    SeisPlot                *_sp_over;
    SeisPlot                *_sp_under;
    int                     _type;
    float                   _ylabel_xoffset;
    void drawPlotLabel      (long frame_number);
    void drawExtraLabels    (long frame_number);
    void drawYannotation    (long frame_number);
    void drawXannotation    (long frame_number);
    void drawBorder         (long frame_number);  
    void annotateManualGridY(long frame_number);
    float charHeight        (unsigned char bold);
    float charWidth         (unsigned char bold);
    float getLogarithmicPointFromValue(float value, float start_coord,
                                       float end_coord, 
                                       float max_dimension);
    void  setAnnotationSizes();
    unsigned char       _metric;
    int                 _over_under;
    float               _over_x1;
    float               _over_x2;
    float               _over_y1;
    float               _over_y2;
    float               _under_x1;
    float               _under_x2;
    float               _under_y1;
    float               _under_y2;
    float               _xoffset;
    float               _yoffset;
    float               _x2;
    float               _image_width;
    float               _image_height;

  protected:
    float                      _bold_char_height;
    float                      _normal_char_height;
    float                      _bold_char_width;
    float                      _normal_char_width;
    float                      _bold_line_width;
    float                      _normal_line_width;
    HardCopyPlot::CoordSystem  _coordinate_system;
    void         gridWithHeaders(long frame_number);
    void         gridWithoutHeaders(long frame_number);
 
  public:
    HardCopyAnnotation( HardCopyPlot *hc_over,
                        SeisPlot     *sp_over,
                        HardCopyPlot *hc_under    = 0,//NULL 
                        SeisPlot     *sp_under    = 0,//NULL 
                        float        over_x1      = 0,
                        float        over_x2      = 0,
                        float        over_y1      = 0,
                        float        over_y2      = 0,
                        float        under_x1     = 0,
                        float        under_x2     = 0,
                        float        under_y1     = 0,
                        float        under_y2     = 0,
                        long         frame_number = 1);
    virtual ~HardCopyAnnotation();
    void annotate       (long frame_number = 1);
    
};
#endif

