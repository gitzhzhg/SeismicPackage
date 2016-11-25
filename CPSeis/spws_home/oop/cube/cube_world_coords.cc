#include "cube/cube.hh"
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
#include "sp/seis_cube_slice.hh"
#include "cube/cube_world_coords.hh"





CubeWorldCoords::CubeWorldCoords(Cube *cube) : _cube(cube) {}
 
void CubeWorldCoords::update()
{
  _cube->inlineSCS()->getCoords(&_lmin, &_lmax, &_xlmin, 
                                &_xlmax, &_tsmin, &_tsmax);
}


float CubeWorldCoords::lineMin()
{
  update();
  return _lmin;
}

float CubeWorldCoords::lineMax()
{
  update();
  return _lmax;
}
float CubeWorldCoords::lineCurrent()
{
//ehs
//return _cube->convertIndexToWC(Cube::InLine,_cube->currentLine());
  return _cube->convertIndexToWC(Cube::InLine,_cube->inlineSlice());
}
float CubeWorldCoords::crossLineMin()
{
  update();
  return _xlmin;
}
float CubeWorldCoords::crossLineMax()
{
  update();
  return _xlmax;
}
float CubeWorldCoords::crossLineCurrent()
{
//ehs
//return _cube->convertIndexToWC(Cube::CrossLine, _cube->currentCrossLine());
  return _cube->convertIndexToWC(Cube::CrossLine, _cube->crosslineSlice());
}
float CubeWorldCoords::sliceMin()
{
  update();
  return _tsmin;
}
float CubeWorldCoords::sliceMax()
{
  update();
  return _tsmax;
}
float CubeWorldCoords::sliceCurrent()
{
//ehs
//return _cube->convertIndexToWC(Cube::TimeSlice,_cube->currentTimeSlice());
  return _cube->convertIndexToWC(Cube::TimeSlice,_cube->timeSlice());
}
