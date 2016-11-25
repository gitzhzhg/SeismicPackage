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
#ifndef CUBEDISPLAYLIST_H
#define CUBEDISPLAYLIST_H


#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

class SeisPlot;
class CubeDisplay;
class SeisColorPop;


class CubeDisplayElement : public Element
{
  friend class CubeDisplayList;
  protected:
     CubeDisplay     *_cube_display;
     SeisColorPop    *_color_pop;

     CubeDisplayElement(CubeDisplay  *cube_display,
                        SeisColorPop *color_pop =NULL) : 
                 _cube_display(cube_display), _color_pop(color_pop) {}
     ~CubeDisplayElement() {}
     int operator ==(void * const cube_display) const 
               { return((CubeDisplay*) cube_display == _cube_display); }
     virtual void print() const {}
};


class CubeDisplayList : public BaseLinkedList
{
  public:
   void add(CubeDisplay *cube_display, SeisColorPop *color_pop =NULL)
               { CubeDisplayElement *theElement = 
                          new CubeDisplayElement(cube_display,color_pop);
                 BaseLinkedList::add((Element *) theElement); }

   void remove(CubeDisplay *cube_display) 
               { BaseLinkedList::remove((void*)cube_display); };
   CubeDisplay  *top(void **ptr = (void **) 0);
   CubeDisplay  *find(CubeDisplay *cube_display, void **ptr = (void **) 0);
   CubeDisplay  *bottom(void **ptr = (void **) 0);
   CubeDisplay  *next(void **ptr = (void **) 0);
   CubeDisplay  *prev(void **ptr = (void **) 0);
   CubeDisplay  *current(void **ptr = (void **) 0);
   SeisColorPop *currentColorPop(void **ptr = (void **) 0);
};


#endif

