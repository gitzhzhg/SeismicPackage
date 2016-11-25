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
#ifndef CUBE_PARAMS_HH
#define CUBE_PARAMS_HH

#include "cube/cube_visitor.hh"
#include "cube/cube.hh"

class CubeParams : public CubeVisitor {

   private:
     float _tmin;
     float _tmax;
     float _is;
     float _lines_per_inch;
     float _xlines_per_inch;
     float _rlines_per_inch;
     float _ct;
     int   _ptype;
     long  _inline_right_to_left;
     long  _crossline_right_to_left;
     long  _timeslice_right_to_left;
     long  _randomline_right_to_left;
     long  _inline_invert_vertical_axis;
     long  _crossline_invert_vertical_axis;
     long  _timeslice_invert_vertical_axis;
     long  _randomline_invert_vertical_axis;
     Boolean _negative_fill;

   public:
     
     CubeParams();
     virtual ~CubeParams() {}
     virtual void visitCube(Cube*);

     //sets
     void setTmin(float t);
     void setTmax(float t);
     void setIS(float s);
     void setLinesPerInch(float s);
     void setXlinesPerInch(float s);
     void setRlinesPerInch(float s);
     void setCT(float v);
     void setPlotType(int t);
     void setNegativeFill(Boolean f);
     void setRightToLeft(Cube::WhichPlot which_plot, long rl);
     void setInvertVertical(Cube::WhichPlot which_plot, long iv);

     //gets
     float getTmin(){return _tmin;}
     float getTmax(){return _tmax;}
     float getIS(){return _is;}
     float getLinesPerInch(){return _lines_per_inch;}
     float getXlinesPerInch(){return _xlines_per_inch;}
     float getRlinesPerInch(){return _rlines_per_inch;}
     float getCT(){return _ct;}
     int   getPlotType(){return _ptype;}
     long  getRightToLeft(Cube::WhichPlot which_plot);
     long  getInvertVertical(Cube::WhichPlot which_plot);
};
#endif
