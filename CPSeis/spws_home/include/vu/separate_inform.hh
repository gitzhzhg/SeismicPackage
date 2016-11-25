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

//------------------- separate_inform.hh ---------------------//
//------------------- separate_inform.hh ---------------------//
//------------------- separate_inform.hh ---------------------//

//              header file for the SeparateInform class
//                  derived from the SeisInform class
//                         subdirectory pick

    //  This class exists solely for notification when
    //  the zoom-up-separate-window SeisPlot is unmanaged
    //  and destroyed, or is exposed.
    //  This method should be called when a separate-window
    //    SeisPlot is created:     addSeisPlot


#ifndef _SEPARATE_INFORM_HH_
#define _SEPARATE_INFORM_HH_

#include "sp/seis_inform.hh"


typedef void SeparateInformDestroyedTrap (void *data, SeisPlot *sp,
                                               SeisPlot *zoomsp);
typedef void SeparateInformExposeTrap    (void *data, SeisPlot *sp,
                                               SeisPlot *zoomsp,
                              int x, int y, int width, int height);

class SeisPlot;


class SeparateInform : public SeisInform
{

//----------------------- data --------------------------------//
//----------------------- data --------------------------------//
//----------------------- data --------------------------------//

private:

  SeisPlot                    *_sp;
  SeparateInformDestroyedTrap *_destroyed_trap;
  void                        *_destroyed_data;
  SeparateInformExposeTrap    *_expose_trap;
  void                        *_expose_data;

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:    // constructor and destructor

  SeparateInform(SeisPlot *sp,
                  SeparateInformDestroyedTrap *destroyed_trap,
                  void                        *destroyed_data,
                  SeparateInformExposeTrap    *expose_trap,
                  void                        *expose_data);
  virtual ~SeparateInform();

public:    // methods called by SeisPlot

  virtual void destroyed (SeisPlot *zoomsp);
  virtual void expose    (SeisPlot *zoomsp,
                            int x, int y, int width, int height);

//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//

};

#endif

//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
