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
#ifndef CUBEINFORM_HH
#define CUBEINFORM_HH

#include "cube/cube_list.hh"
#include "cube/cube.hh"

class Cube;


class CubeInform  : public CubeList {

  protected:
  public:
     CubeInform();
     CubeInform(Cube *cube);
     virtual ~CubeInform(); 

     void addCube(Cube *cube); 
     void delCube(Cube *cube);

     virtual void newInLinePlot(Cube *,    int /*slice*/) {}
     virtual void newCrossLinePlot(Cube *, int /*slice*/) {}
     virtual void newTimeSlicePlot(Cube *, int /*slice*/) {}
     virtual void postPlot(Cube *, 
                           int /*il_slice*/, 
                           int /*xl_slice*/, 
                           int /*ts_slice*/)              {}
     virtual void noPlotDisplayed(Cube *)                 {}
     virtual void cubeIsCurrent(Cube *)                   {} 
     virtual void cubeMovie(Cube *, 
                            Cube::WhichPlot /*face*/, 
                            Cube::MovieDir  /*mdir*/, 
                            int             /*slice*/)    {}
     virtual void cubeIsNolongerCurrent(Cube* /*cube*/,Cube* /*newcube*/ ) {}
     virtual void newCubeCreated(Cube *)                  {}
     virtual void destroyed(Cube *)                       {}
     virtual void maxAmplitudeChanged(double /*amp*/)     {}
     virtual void newFilename(Cube *)                     {}
     virtual void preZoom(Cube *, Cube::WhichPlot ) {}
     virtual void postZoomSeparateWindow(Cube *, 
                                         Cube::WhichPlot, 
                                         SeisPlot *)      {}
};

#endif
