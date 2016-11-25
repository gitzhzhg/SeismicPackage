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
#ifndef CUBE_ANNO_PARAMS_HH
#define CUBE_ANNO_PARAMS_HH

#include "cube/cube_visitor.hh"

class CubeAnnoParams : public CubeVisitor {

   private:
     char  *_title;

     long   _start_line;
     long   _line_inc;
     long   _start_xline;
     long   _xline_inc;
     double _prim_time_line;
     double _sec_time_line;
     int    _line_h1;
     int    _line_h2;
     int    _xline_h1;
     int    _xline_h2;

   public:
     
     CubeAnnoParams();
     virtual ~CubeAnnoParams();
     virtual void visitCube(Cube*);

     void setPlotLabel(char *title);

     void setLineLabeling(long start, long inc);
     void setXLineLabeling(long start, long inc);

     void setTimingLines(double prim, double sec);

     void setLineHeaders(int h1, int h2);
     void setXLineHeaders(int h1, int h2);
};
#endif
