#include "cube/cube_master_iterator.hh"
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
#include "cube/cube_master.hh"
#include "cube/cube_display.hh"


CubeMasterIterator::CubeMasterIterator() : 
                     _cd(NULL), _cd_dummy(NULL), _cm_dummy(NULL)
{
  CubeMaster *cm= CubeMaster::instance();
  _cd= cm->top(&_cm_dummy);
  _cd->top(&_cd_dummy);
}

Cube *CubeMasterIterator::nextCube()
{
  CubeMaster *cm= CubeMaster::instance();
  Cube *cube= NULL;

  cube= _cd->next(&_cd_dummy);
  if (!cube) {
      _cd= cm->next(&_cm_dummy);
      if (_cd) {
          _cd_dummy= NULL;
          cube= _cd->top(&_cd_dummy);
      }
      else 
          cube= NULL;
  }
  return cube;
}

Cube *CubeMasterIterator::prevCube()
{
  CubeMaster *cm= CubeMaster::instance();
  Cube *cube= NULL;

  cube= _cd->prev(&_cd_dummy);
  if (!cube) {
      _cd= cm->prev(&_cm_dummy);
      if (_cd) {
          _cd_dummy= NULL;
          cube= _cd->top(&_cd_dummy);
      }
      else 
          cube= NULL;
  }
  return cube;
}

Cube *CubeMasterIterator::currentCube()
{
  Cube *cube= NULL;
  if (_cd) cube= _cd->current(&_cd_dummy);
  return cube;
}

CubeDisplay *CubeMasterIterator::currentCubeDisplayOfCurrentCube()
{
  return _cd;
}

Cube *CubeMasterIterator::findCube(Cube *cube_to_find)
{
  CubeMaster *cm= CubeMaster::instance();
  CubeDisplay *findcd;
  void *x, *y;
  Cube *cube= NULL;

  for( findcd= cm->top(&x); (findcd && !cube); findcd= cm->next(&x)) {
        cube= findcd->find(cube_to_find, &y);
  } // end loop
  if (cube) {
       _cm_dummy= x;
       _cd_dummy= y;
  }
  return cube;
}
