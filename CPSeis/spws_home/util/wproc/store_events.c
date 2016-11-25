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
/*------------------------------------------------------------------
 *USER DOC
 *Name   : store_events 
 *Purpose: Reduce the number of exposure redraws to maximize refresh
 *         rates. Do not do your exposure redraw until the return
 *         is false. This function assumes Xlib's continued flushing
 *         of all counts of an event before processing another.
 *
 *Author : Michael L. Sherrill
 *Date   : 03/93
 *
 * Function Definition:
 * int store_events ( XExposeEvent *ev )               
 *
 * ev       in/out     Xlib's expose event pointer.      
 *
 *NOTES:
 * 1.
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/

#include <X11/Xlib.h>





/*+++++++++++++++++++++++++ reduce expose redraws +++++++++++++++++++++++++*/

int store_events( XExposeEvent *ev )

{
 static long minx=99999, miny=99999;
 static long maxx= 0, maxy = 0;
 static int store = False;




  if(ev->count)
  {
   minx = (minx < ev->x) ? minx : ev->x;
   miny = (miny < ev->y) ? miny : ev->y;
   maxx = (maxx > ev->x + ev->width ) ? maxx : ev->x + ev->width;
   maxy = (maxy > ev->y + ev->height) ? maxy : ev->y + ev->height;
   store = True;
   return(store);
  }

  if(store)
  {
    store = False;
    minx = (minx < ev->x) ? minx : ev->x;
    miny = (miny < ev->y) ? miny : ev->y;
    maxx = (maxx > ev->x + ev->width ) ? maxx : ev->x + ev->width;
    maxy = (maxy > ev->y + ev->height) ? maxy : ev->y + ev->height;
    ev->x = minx;
    ev->y = miny;
    ev->width = maxx - minx;
    ev->height= maxy - miny;
    minx = miny = 99999;
    maxx = maxy = 0;
  }

  return(store);

}
