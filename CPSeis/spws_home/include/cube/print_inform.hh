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
#ifndef PRINTINFORM_HH
#define PRINTINFORM_HH

#include "cube/cube_inform.hh"

class Cube;


class PrintInform  : public CubeInform {

  protected:
  public:
     PrintInform(Cube *cube) : CubeInform(cube) {}
     PrintInform();
     virtual void newInLinePlot(Cube *,    int /*slice*/);
     virtual void newCrossLinePlot(Cube *, int /*slice*/);
     virtual void newTimeSlicePlot(Cube *, int /*slice*/);
     virtual void cubeIsCurrent(Cube *);
     virtual void cubeIsNolongerCurrent(Cube* /*cube*/,Cube* /*newcube*/ );
     virtual void newCubeCreated(Cube *);
     virtual void postPlot(Cube *, 
                           int il_slice, 
                           int xl_slice, 
                           int ts_slice);
     virtual void cubeMovie(Cube *, 
                            Cube::WhichPlot face, 
                            Cube::MovieDir  mdir, 
                            int             slice);
     virtual void noPlotDisplayed(Cube *);
     virtual void destroyed(Cube *);
};

#endif
