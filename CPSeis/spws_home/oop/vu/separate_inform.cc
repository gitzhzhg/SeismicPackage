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

//------------------- separate_inform.cc ---------------------//
//------------------- separate_inform.cc ---------------------//
//------------------- separate_inform.cc ---------------------//

//          implementation file for the SeparateInform class
//                  derived from the SeisInform class
//                         subdirectory pick


#include "vu/separate_inform.hh"
#include "sp/seis_plot.hh"


//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//

SeparateInform::SeparateInform(SeisPlot *sp,
                  SeparateInformDestroyedTrap *destroyed_trap,
                  void                        *destroyed_data,
                  SeparateInformExposeTrap    *expose_trap,
                  void                        *expose_data)
                  : SeisInform(NULL),
                    _sp              (sp),
                    _destroyed_trap  (destroyed_trap),
                    _destroyed_data  (destroyed_data),
                    _expose_trap     (expose_trap),
                    _expose_data     (expose_data)
{
}


SeparateInform::~SeparateInform()
{
}


//------------ destroyed (called from SeisPlot) ----------------//
//------------ destroyed (called from SeisPlot) ----------------//
//------------ destroyed (called from SeisPlot) ----------------//

void SeparateInform::destroyed (SeisPlot *zoomsp)
{
  if(_destroyed_trap) _destroyed_trap(_destroyed_data, _sp, zoomsp);
  SeisInform::destroyed(zoomsp);
}


void SeparateInform::expose (SeisPlot *zoomsp,
                            int x, int y, int width, int height)
{
  if(_expose_trap) _expose_trap(_expose_data, _sp, zoomsp,
                            x, y, width, height);
}


//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
