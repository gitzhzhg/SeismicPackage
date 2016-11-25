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
//********************************************
//Author Michael L. Sherrill 05/97
//Creates table to control random line generation
//********************************************

#ifndef CUBE_RANDOM_LINE_XYTABLE_H
#define CUBE_RANDOM_LINE_XYTABLE_H

#include "sl/sl_databox.hh"
#include "sl/sl_tog_box.hh"

class CubeRandomLine;
class CubeRandomLinePop;
class CubeRandomLineXYTable;


class CubeRandomLineXYTable : public SLDatabox{

      private:
           CubeRandomLine       *_cube_random_line;
           CubeRandomLinePop    *_random_line_pop;
           float                *_temp_xarray;
           float                *_temp_yarray;
           Boolean               _draw_vectors;

      protected:
           virtual void makeHelper();
           static long rowUpdate(void *data);
           static long maxRowUpdate(void *data);
           static float valueUpdate(void *data, long ident, long index);
           static long  valueSwitchUpdate(void *data, long ident, long index);

      public:
           CubeRandomLineXYTable(SLDelay *slparent,     char *name,
                                 CubeRandomLine       *random_line); 
           static void valueTrap(void *data, long ident, 
                                 long index, float value, 
                                 long nread, char *endkey); 
           enum {XARRAY = 1, YARRAY = 2};           
           void setNumberSegments(long i);
           void changeActiveRandomLine(CubeRandomLine *crl)
                                                   {_cube_random_line = crl;}
           void drawVectors(Boolean draw){_draw_vectors = draw;}
};




#endif
