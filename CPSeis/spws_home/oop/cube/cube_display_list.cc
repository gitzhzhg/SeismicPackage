#include "cube/cube_display_list.hh"
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

CubeDisplay *CubeDisplayList::top(void **ptr) 
{ 
   CubeDisplayElement* q= (CubeDisplayElement*)BaseLinkedList::top(ptr);
   return (q ? q->_cube_display : NULL);
}

CubeDisplay *CubeDisplayList::bottom(void **ptr)
{
   CubeDisplayElement* q= (CubeDisplayElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_cube_display : NULL);
}


CubeDisplay *CubeDisplayList::next(void **ptr)
{
   CubeDisplayElement* q= (CubeDisplayElement*)BaseLinkedList::next(ptr);
   return (q ? q->_cube_display : NULL);
}

CubeDisplay *CubeDisplayList::prev(void **ptr)
{
   CubeDisplayElement* q= (CubeDisplayElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_cube_display : NULL);
}

CubeDisplay *CubeDisplayList::current(void **ptr)
{
   CubeDisplayElement* q= (CubeDisplayElement*)BaseLinkedList::current(ptr);
   return (q ? q->_cube_display : NULL);
}

SeisColorPop *CubeDisplayList::currentColorPop(void **ptr)
{
   CubeDisplayElement* q= (CubeDisplayElement*)BaseLinkedList::current(ptr);
   return (q ? q->_color_pop : NULL);
}

CubeDisplay *CubeDisplayList::find(CubeDisplay *cube_display, void **ptr)
{
  CubeDisplayElement* q= 
        (CubeDisplayElement*)BaseLinkedList::find((void*)cube_display, ptr);
  return (q ? q->_cube_display : NULL);
}
