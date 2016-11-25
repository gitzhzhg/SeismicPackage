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
 *Name   : imageFree
 *Purpose: Release all memory associated with an image structure.
 *
 *Author : Michael L. Sherrill / Trey Roby
 *Date   : 02/92 (C++ version 4/97)
 *
 * Function Definition:
 * void image_free ( )
 *
 *
 *NOTES:
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/PushB.h>
#include <Xm/SelectioB.h>
#include <stdlib.h>
#include "plot_image.hh"



void PlotImage::imageFree ()

{
Display *dpy = XtDisplay(_graphic);

   long i;


   for(i=0; ( i< MAX_PIXMAP); i++) 
      {
      if(_pixmary[_cpixm]) 
         {
         XFreePixmap ( dpy, _pixmary[i] );
         _pixmary[i]= 0;
         }
      }

    if(_byte_array != NULL && _point_to_data == False) 
       {
       free( _byte_array);
       _byte_array= NULL;
       }

    if(_ximage.data != NULL) 
       {
       free(_ximage.data);
       _ximage.data= NULL;
       }

    if(_hd != NULL && _point_to_headers == False) 
       {
       free(_hd);
       _hd= NULL;
       }

    if(_float_array != NULL && _point_to_data == False) 
       {
       free(_float_array);
       _float_array= NULL;
       }

    if(_xloc != NULL && _sharing_resources == False) 
       {
       free(_xloc);
       _xloc= NULL;
       }
 
    if(_yloc != NULL && _sharing_resources == False) 
       {
       free(_yloc);
       _yloc= NULL;
       }

    _displayed= False;

    if(_chain_image != NULL)
       {
       if(!_chain_image->_underlay_only)
	  {
          _chain_image->imageFree();
          _chain_image = NULL;
	  }
       }
}






void PlotImage::imageFreeXresources ()
{
Display *dpy = XtDisplay(_graphic);

  if(_bitmap_pixmap != 0)
    {
    XFreePixmap(dpy,_bitmap_pixmap);
    _bitmap_pixmap = 0;
    }

  if(_font_bold != NULL)
    {
    XFreeFont(dpy,_font_bold);
    _font_bold = NULL;
    }

  if(_font_fixed != NULL)
    {
    XFreeFont(dpy,_font_fixed);
    _font_fixed = NULL;
    }

  if(_gc1 != NULL)
    {
    XFreeGC(dpy,_gc1);
    _gc1 = NULL;
    }
  
  if(_gc2 != NULL)
    {
    XFreeGC(dpy,_gc2);
    _gc2 = NULL;
    }

  if(_pick_gc != NULL)
    {
    XFreeGC(dpy,_pick_gc);
    _pick_gc = NULL;
    }

  if(_bitmap_gc1 != NULL)
    {
    XFreeGC(dpy,_bitmap_gc1);
    _bitmap_gc1 = NULL;
    }

  if(_bitmap_gc2 != NULL)
    {
    XFreeGC(dpy,_bitmap_gc2);
    _bitmap_gc2 = NULL;
    }


}


