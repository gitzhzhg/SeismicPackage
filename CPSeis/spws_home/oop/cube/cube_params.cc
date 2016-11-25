#include "cube/cube_params.hh"
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
#include "cube/cube.hh"

#define N0T_SET_VALUE -5.0
#define N0T_SET_VALUE_INT -1233434

CubeParams::CubeParams() :
     _tmin(N0T_SET_VALUE),            _tmax(N0T_SET_VALUE),
     _is(N0T_SET_VALUE),              _lines_per_inch(N0T_SET_VALUE),
     _xlines_per_inch(N0T_SET_VALUE), _ct(N0T_SET_VALUE),
     _ptype(N0T_SET_VALUE_INT),       
     _inline_right_to_left(N0T_SET_VALUE_INT),
     _crossline_right_to_left(N0T_SET_VALUE_INT),
     _timeslice_right_to_left(N0T_SET_VALUE_INT),
     _randomline_right_to_left(N0T_SET_VALUE_INT), 
     _inline_invert_vertical_axis(N0T_SET_VALUE_INT),
     _crossline_invert_vertical_axis(N0T_SET_VALUE_INT),
     _timeslice_invert_vertical_axis(N0T_SET_VALUE_INT),
     _randomline_invert_vertical_axis(N0T_SET_VALUE_INT)
{}

void CubeParams::setTmin(float t)          { _tmin= t; }
void CubeParams::setTmax(float t)          { _tmax= t; }
void CubeParams::setIS(float s)            { _is= s; }
void CubeParams::setLinesPerInch(float s)  { _lines_per_inch= s; }
void CubeParams::setXlinesPerInch(float s) { _xlines_per_inch= s; }
void CubeParams::setRlinesPerInch(float s) { _rlines_per_inch= s; }
void CubeParams::setCT(float v)            { _ct= v; }
void CubeParams::setPlotType(int t)        { _ptype= t; }
void CubeParams::setNegativeFill(Boolean f){ _negative_fill = f;}


void CubeParams::setRightToLeft(Cube::WhichPlot which_plot, long rl)   
{ 
  switch (which_plot)
    {
      case Cube::InLine:
         _inline_right_to_left = rl;
      break;
      case Cube::CrossLine:
         _crossline_right_to_left = rl;
      break;
      case Cube::TimeSlice:
         _timeslice_right_to_left = rl;
      break;
      case Cube::RandomLine:
         _randomline_right_to_left = rl;
      break;
      case Cube::AllPlots:
      break;
    }
}
void CubeParams::setInvertVertical(Cube::WhichPlot which_plot, long iv)
{ 
  switch (which_plot)
    {
      case Cube::InLine:
         _inline_invert_vertical_axis = iv;
      break;
      case Cube::CrossLine:
         _crossline_invert_vertical_axis = iv;
      break;
      case Cube::TimeSlice:
         _timeslice_invert_vertical_axis = iv;
      break;
      case Cube::RandomLine:
         _randomline_invert_vertical_axis = iv;
      break; 
      case Cube::AllPlots:
      break;
    }
}



long CubeParams::getRightToLeft(Cube::WhichPlot which_plot)   
{ 
long rl = 0;

  switch (which_plot)
    {
      case Cube::InLine:
         rl = _inline_right_to_left;
      break;
      case Cube::CrossLine:
         rl = _crossline_right_to_left;
      break;
      case Cube::TimeSlice:
         rl = _timeslice_right_to_left;
      break;
      case Cube::RandomLine:
         rl = _randomline_right_to_left;
      break;
      case Cube::AllPlots:
      break;
    }

  return rl;
 
}


long CubeParams::getInvertVertical(Cube::WhichPlot which_plot)
{ 
long iv = 0;

  switch (which_plot)
    {
      case Cube::InLine:
         iv = _inline_invert_vertical_axis;
      break;
      case Cube::CrossLine:
         iv = _crossline_invert_vertical_axis;
      break;
      case Cube::TimeSlice:
         iv = _timeslice_invert_vertical_axis;
      break;
      case Cube::RandomLine:
         iv = _randomline_invert_vertical_axis;
      break; 
      case Cube::AllPlots:
      break;
    }

  return iv;

}






void CubeParams::visitCube(Cube *cube)
{
  if (_tmin != N0T_SET_VALUE)  cube->setTmin(_tmin); 
  if (_tmax != N0T_SET_VALUE)  cube->setTmax(_tmax); 
  if (_is != N0T_SET_VALUE)    cube->setIS(_is); 
  if (_lines_per_inch != N0T_SET_VALUE)
                               cube->setLinesPerInch(_lines_per_inch); 
  if (_xlines_per_inch != N0T_SET_VALUE)
                               cube->setXlinesPerInch(_xlines_per_inch); 
  if (_ct != N0T_SET_VALUE)    cube->setCT(_ct); 
  if (_ptype != N0T_SET_VALUE)
    {
    cube->setPlotType(_ptype); 
    cube->setNegativeFill(_negative_fill);
    }
  if (_inline_right_to_left != N0T_SET_VALUE_INT)
   cube->setRightToLeft(Cube::InLine, _inline_right_to_left); 
  if (_crossline_right_to_left != N0T_SET_VALUE_INT)
   cube->setRightToLeft(Cube::CrossLine, _crossline_right_to_left); 
  if (_timeslice_right_to_left != N0T_SET_VALUE_INT)
   cube->setRightToLeft(Cube::TimeSlice, _timeslice_right_to_left); 
  if (_randomline_right_to_left != N0T_SET_VALUE_INT)
   cube->setRightToLeft(Cube::RandomLine, _randomline_right_to_left); 
  if (_inline_invert_vertical_axis != N0T_SET_VALUE_INT)
   cube->setInvertVerticalAxis(Cube::InLine, _inline_invert_vertical_axis);
  if (_crossline_invert_vertical_axis != N0T_SET_VALUE_INT)
   cube->setInvertVerticalAxis(Cube::CrossLine,_crossline_invert_vertical_axis);
  if (_timeslice_invert_vertical_axis != N0T_SET_VALUE_INT)
   cube->setInvertVerticalAxis(Cube::TimeSlice,_timeslice_invert_vertical_axis);
  if (_randomline_invert_vertical_axis != N0T_SET_VALUE_INT)
   cube->setInvertVerticalAxis
                            (Cube::RandomLine,_randomline_invert_vertical_axis);
}
