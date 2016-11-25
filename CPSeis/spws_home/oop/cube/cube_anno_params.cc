#include "cube/cube_anno_params.hh"
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
#include "sp/seis_plot.hh"
#include "cprim.h"

#define N0T_SET_VALUE -5.0
#define N0T_SET_VALUE_INT -5

CubeAnnoParams::CubeAnnoParams() :
     _title(NULL),
     _start_line(N0T_SET_VALUE_INT),
     _line_inc(N0T_SET_VALUE_INT),
     _start_xline(N0T_SET_VALUE_INT),
     _xline_inc(N0T_SET_VALUE_INT),
     _prim_time_line(N0T_SET_VALUE),
     _sec_time_line(N0T_SET_VALUE),
     _line_h1(N0T_SET_VALUE_INT),
     _line_h2(N0T_SET_VALUE_INT),
     _xline_h1(N0T_SET_VALUE_INT),
     _xline_h2(N0T_SET_VALUE_INT)
{}

CubeAnnoParams::~CubeAnnoParams()
{
  if (_title) free(_title);
}

void CubeAnnoParams::setPlotLabel(char *title)
{
  if (_title) free(_title);
  _title= newstr(title);
}

void CubeAnnoParams::setLineLabeling(long start, long inc)
{
  _start_line= start;
  _line_inc= inc;
}
void CubeAnnoParams::setXLineLabeling(long start, long inc)
{
  _start_xline= start;
  _xline_inc= inc;
}

void CubeAnnoParams::setTimingLines(double prim, double sec)
{
  _prim_time_line= prim;
  _sec_time_line= sec;
}

void CubeAnnoParams::setLineHeaders(int h1, int h2)
{
  _line_h1= h1;
  _line_h2= h2;
}

void CubeAnnoParams::setXLineHeaders(int h1, int h2)
{
  _xline_h1= h1;
  _xline_h2= h2;
}


void CubeAnnoParams::visitCube(Cube *cube)
{
  SeisPlot *linesp= cube->inlineSP();
  SeisPlot *xlinesp= cube->crosslineSP();
  SeisPlot *tssp= cube->timesliceSP();

  if (_title) {
      linesp->setPlotLabel(_title);
      xlinesp->setPlotLabel(_title);
      tssp->setPlotLabel(_title);
  } // end if

  if (_start_xline != N0T_SET_VALUE)  {
        if ( (linesp->firstLbl() != _start_xline) || 
             (linesp->lblInc()   != _xline_inc) ) {
           linesp->setLabeling(_start_xline, _xline_inc);
           tssp->setLabeling(_start_xline, _xline_inc);
           cube->setTimeSliceToReplot();
           cube->setInlineSliceToReplot();
        } // end if
  } // end if
  if (_start_line != N0T_SET_VALUE)  {
        if ( (xlinesp->firstLbl() != _start_line) || 
             (xlinesp->lblInc()   != _line_inc) ) {
           xlinesp->setLabeling(_start_line, _line_inc);
           cube->setCrosslineSliceToReplot();
           cube->setTimeSliceToReplot();
           tssp->setTimingLines( _line_inc, _line_inc);
        } // end if
  } // end if

  if (_prim_time_line != N0T_SET_VALUE) {
        if ( (linesp->primTimingLine() != _prim_time_line) ||
             (linesp->secTimingLine()  != _sec_time_line) ) {
           linesp->setTimingLines( _prim_time_line, _sec_time_line);
           xlinesp->setTimingLines( _prim_time_line, _sec_time_line);
           cube->setInlineSliceToReplot();
           cube->setCrosslineSliceToReplot();
           /*
            * don't set timing lines on ts yet because we are
            * still a little clugy
            */
           //tssp->setTimingLines( _prim_time_line, _sec_time_line);
           //cube->setTimeSliceToReplot();
        } // end if
  } // end if


  if (_line_h1 != N0T_SET_VALUE)  {
        if ( (linesp->header1() != _line_h1) ||
             (linesp->header2() != _line_h2) ) {
           linesp->setHeaders(_line_h1, _line_h2);
           tssp->setHeaders(_line_h1, _line_h2);
           cube->setInlineSliceToReplot();
           cube->setTimeSliceToReplot();
        } // end if
  } // end if
  if (_xline_h1 != N0T_SET_VALUE)  {
        if ( (xlinesp->header1() != _xline_h1) ||
             (xlinesp->header2() != _xline_h2) ) {
           xlinesp->setHeaders(_xline_h1, _xline_h2);
           cube->setCrosslineSliceToReplot();
        } // end if
  } // end if
    

  // time slice will need more work
  // for timing lines in y direction and header words

   
}
