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


//===========================================================================
//========== This gui is attached to a parent gui and controls    ===========
//========== contouring parameters for data. Note at some point   ===========
//========== the GpModelContourGui in gpplots and VA's semblance  ===========
//========== guis which were written before this should be made   ===========
//========== to use this as their base class.                     ===========
//========== Michael L. Sherrill 06/2002                          ===========
//===========================================================================


#ifndef CONTOUR_GUI_H
#define CONTOUR_GUI_H

#include "sl/sl_form.hh"

class ContourGui :  public SLForm {

public:
  ContourGui( SLDelay                *parent,
              char                   *name,
              class SeisPlot         *sp,
              HelpCtx                hctx);
  ~ContourGui();
  Widget  make(Widget p);
  void    manage();
  Boolean notifyComplex(SLDelay*, int); 
  Boolean contoursRequested();
  float   minContourValue();
  float   maxContourValue();
  int     numContours();
  virtual void setParameters(class SeisPlot *sp);
  int     plotContoursOnly();
  void    setPlotContoursOnly(int set);
  void    setContourOption(int s);
  void    setContourMinValue(float v);
  float   getContourMinValue() {return _min_contour_val;}
  void    setContourMaxValue(float v);
  float   getContourMaxValue() {return _max_contour_val;}
  enum {MIN_CONTOUR_VAL, MAX_CONTOUR_VAL, NUM_CONTOURS,
        DO_CONTOURS,     CONTOURS_ONLY };



protected:
  class SeisPlot        *_sp;
  Boolean               _first_time;
  int                   _plot_type;
  float                 _min_contour_val;
  float                 _max_contour_val;
  int                   _num_contours;
  class SLTextBox       *_values_box;
  class SLTogBox        *_contour_box;
  class SLTogBox        *_contour_only_box;

private:

};


#endif



