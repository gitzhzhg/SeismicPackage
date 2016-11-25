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

//------------------------ paintset_from_c.cc --------------------------//
//------------------------ paintset_from_c.cc --------------------------//
//------------------------ paintset_from_c.cc --------------------------//


#include "paintset_from_c.h"
#include "sl/paintset_collection.hh"


//----------------------------- functions ----------------------------------//
//----------------------------- functions ----------------------------------//
//----------------------------- functions ----------------------------------//


void paintset_add_resources (Screen *screen, Arg *arglist, int *n)
{
  PaintsetCollection::addResources(arglist, n, screen);
}


void paintset_add_resources_w (Widget widget, Arg *arglist, int *n)
{
  PaintsetCollection::addResources(arglist, n, widget);
}


int         paintset_depth      (Screen *screen)
{
  return PaintsetCollection::visualDepth(screen);
}


Visual     *paintset_visual     (Screen *screen)
{
  return PaintsetCollection::visual(screen);
}


Colormap    paintset_colormap   (Screen *screen)
{
  return PaintsetCollection::colormap(screen);
}


Pixel       paintset_white      (Screen *screen)
{
  return PaintsetCollection::white(screen);
}


Pixel       paintset_black      (Screen *screen)
{
  return PaintsetCollection::black(screen);
}


//------------------------------- end -------------------------------------//
//------------------------------- end -------------------------------------//
//------------------------------- end -------------------------------------//

