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

//------------------- header_dump_inform.hh ---------------------//
//------------------- header_dump_inform.hh ---------------------//
//------------------- header_dump_inform.hh ---------------------//

//              header file for the HeaderDumpInform class
//                  derived from the SeisInform class
//                         subdirectory pick

#ifndef _HEADER_DUMP_INFORM_HH_
#define _HEADER_DUMP_INFORM_HH_

#include "sp/seis_inform.hh"


class HeaderDumpInform : public SeisInform
{

//----------------------- data --------------------------------//
//----------------------- data --------------------------------//
//----------------------- data --------------------------------//

private:

  class HeaderDumpPop      *_headpop;
  class SeparateInform     *_sep_inform;

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:    // constructor and destructor

  HeaderDumpInform(SeisPlot *sp, HeaderDumpPop *headpop);
  virtual ~HeaderDumpInform();

public:    // methods (overriding SeisInform) called by SeisPlot

  virtual void postZoomSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp);
  virtual void postZoom  (SeisPlot *sp, SeisPlot::ZoomDir dir);
  virtual void postScan  (SeisPlot *sp, SeisPlot::ScanDir dir);
  virtual void postMovie (SeisPlot *sp, SeisPlot::MovieDir dir);
  virtual void newPlot   (SeisPlot *sp);
  virtual void dragImage (SeisPlot *sp);

  virtual void preZoom       (SeisPlot *sp, SeisZoomer *zoomer,
                                            SeisPlot::ZoomDir dir);
  virtual void preScan       (SeisPlot *sp, SeisPlot::ScanDir dir);
  virtual void preMovie      (SeisPlot *sp, SeisPlot::MovieDir dir);
  virtual void preDataChange (SeisPlot *sp);
  virtual void prePlot       (SeisPlot *sp);
  virtual void noPlotDisplayed (SeisPlot *sp);
  virtual void destroyed     (SeisPlot *sp);
  virtual void notCurrentInWindow (SeisPlot *sp); 

private:    // traps registered with SeparateInform

  static void    sepDestroyedTrap (void *data, SeisPlot *sp,
                                               SeisPlot *zoomsp);

//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//

};

#endif

//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
