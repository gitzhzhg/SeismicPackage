#include "cube/cube_master.hh"
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
#include "cube/cube_display.hh"
#include "cube/cube.hh"
#include "cube/cube_inform_list.hh"
#include "cube/cube_master_iterator.hh"
#include "sp/seis_color_pop.hh"


CubeMaster *CubeMaster::_cube_master= NULL;

CubeMaster *CubeMaster::instance()
{
  if (!_cube_master) _cube_master= new CubeMaster();
  return _cube_master;
}


CubeMaster::CubeMaster() : _curr_cube_disp(NULL)
{
  _inform_list= new CubeInformList();
}

CubeMaster::~CubeMaster()
{
  delete _inform_list;
}


void CubeMaster::add(CubeDisplay *cube_display)
{
  void *x;
  SeisColorPop *cp;
  Boolean found= False;
  Cube *cube= NULL;

  for(CubeDisplay *q= top(&x); (q && !found); q= next(&x)) {
      if (currentColorPop(&x)->W()) {
          if (XtScreen(currentColorPop(&x)->W()) == 
                             XtScreen(cube_display->W())) {
                 found= True;
                 cp= currentColorPop(&x);
          } // end if
      } // end if
  } // end loop 

  cube= cube_display->currentDisplayedCube();

  if (found) {
       cp->addSP(cube->inlineSP());
  } // end if
  else {
       //create SeisColorPop with the sp with the most colors - in this case
       // timeslice
       cp= new SeisColorPop(cube_display->W(), 
                            "color_pop", cube->timesliceSP(), 
                            NULL, True);// need hctx
       cp->make();
  } // end else
  cp->addSP(cube->crosslineSP());
  cp->addSP(cube->inlineSP());

  CubeDisplayList::add(cube_display, cp);
  if (!_curr_cube_disp) _curr_cube_disp= cube_display;
}

SeisColorPop *CubeMaster::colorPop(CubeDisplay *cd)
{
  SeisColorPop *cp= NULL;
  void *x;
  if (find(cd,&x)) {
      assert(cd == current(&x));
      cp = currentColorPop(&x);
  }
  return cp;
}

void CubeMaster::notifyOfNewCubeCreated(Cube *cube)
{
  _inform_list->callNewCubeCreated(cube);
}

void CubeMaster::remove(CubeDisplay *cube_display)
{
  CubeDisplayList::remove(cube_display);
  if (_curr_cube_disp == cube_display) {
            _curr_cube_disp= top();
  }
}


void CubeMaster::addInformer(CubeInform *inform)
{
  if (!_inform_list->find(inform)) 
    {
    _inform_list->add(inform);
    }
}

void CubeMaster::delInformer(CubeInform *inform)
{
  if (_inform_list->find(inform))
    {
    _inform_list->remove(inform);
    }
}

