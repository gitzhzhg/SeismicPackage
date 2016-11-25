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
#ifndef CUBELIST_H
#define CUBELIST_H


#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

class SeisPlot;
class Cube;


class CubeElement : public Element
{
  friend class CubeList;
  protected:
     Cube     *_cube;

     CubeElement(Cube *cube) : _cube(cube) {}
     ~CubeElement() {}
     int operator ==(void * const cube) const { return((Cube*) cube == _cube); }
     virtual void print() const {}
};


class CubeList : public BaseLinkedList
{
  public:
   void add(Cube *cube)
               { CubeElement *theElement = new CubeElement(cube);
                 BaseLinkedList::add((Element *) theElement); }

   void remove(Cube *cube) { BaseLinkedList::remove((void*)cube); };
   Cube *top(void **ptr = (void **) 0);
   Cube *find(Cube *cube, void **ptr = (void **) 0);
   Cube *bottom(void **ptr = (void **) 0);
   Cube *next(void **ptr = (void **) 0);
   Cube *prev(void **ptr = (void **) 0);
   Cube *current(void **ptr = (void **) 0);
};


#endif
