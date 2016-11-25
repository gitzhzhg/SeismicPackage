#include "cube/print_inform.hh"
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
#include "cube/cube_master_iterator.hh"
#include "cube/cube.hh"
#include <stdio.h>


PrintInform::PrintInform()
{
  CubeMasterIterator iter;
  Cube *q;
  for(q= iter.currentCube(); (q); q= iter.nextCube())
          addCube(q); 
}


void PrintInform::newInLinePlot(Cube *cube, int slice)
{
  printf("newInLinePlot: cube=%x, slice=%1d\n", cube,slice);
}

void PrintInform::newCrossLinePlot(Cube *cube, int slice)
{
  printf("newCrossLinePlot: cube=%x, slice=%1d\n", cube,slice);
}

void PrintInform::newTimeSlicePlot(Cube *cube, int slice)
{
  printf("newTimeSlicePlot: cube=%x, slice=%1d\n", cube,slice);
}

void PrintInform::cubeIsCurrent(Cube *cube)
{
  printf("cubeIsCurrent: cube=%x\n",cube);
}

void PrintInform::destroyed(Cube *cube)
{
  printf("destroyed: cube=%x\n",cube);
}

void PrintInform::cubeIsNolongerCurrent(Cube* cube,Cube* newcube ) 
{
  printf("cubeIsNolongerCurrent: cube=%x, newcube=%x\n", cube,newcube);
}

void PrintInform::newCubeCreated(Cube *cube)
{
  addCube(cube);
  printf("newCubeCreated: cube=%x\n",cube);
}

void PrintInform::postPlot(Cube *cube, 
                           int   il_slice, 
                           int   xl_slice, 
                           int   ts_slice)
{
  printf("postPlot: cube=%x, il_slice= %1d, xl_slice= %1d, ts_slice= %1d\n", 
                         cube,il_slice, xl_slice, ts_slice);
}

void PrintInform::cubeMovie(Cube           *cube, 
                            Cube::WhichPlot face, 
                            Cube::MovieDir  mdir, 
                            int             slice)
{
  printf("postPlot: cube=%x, face= %1d, mdir= %1d, slice= %1d\n", 
                         cube,face, mdir, slice);

}

void PrintInform::noPlotDisplayed(Cube *cube)
{
  printf("noPlotDisplayed: cube=%x\n",cube);
}
