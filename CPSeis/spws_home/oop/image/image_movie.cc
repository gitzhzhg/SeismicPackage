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
/*
C      imageMovie.cc
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C             written in c -- designed to be called from c
C
C     Utility Name:  image_movie        (control movie frames)            
C          Written:  92/03/15  by:  Michael Sherrill
C     Last revised:  94/04/16  by:  M. Sherrill C++ version
C
C  Purpose:       To control movie frames in the image display.   
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY     
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/image   (shared)
C  library:                image.a            (shared)
C  header file:            image.h            (shared)
C  source file:            image_movie.c
C
C  static functions:       none
C  documented functions:   image_movie                           
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES 
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     image.a
C  header files:  image.h    cprim.h
C  functions:     
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author         Description
C     ----      ------         -----------
C  2. 97/04/16  Sherrill       C++ version
C  1. 93/08/24  Sherrill       Initial version.
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C To change movie frames in the display:
C
C         void imageMovie(frame, x, y, width, height)
C
C long   frame = pixmap number to copy in.
C long       x = x-pixel coord of upper left corner of new frame.       
C long       y = y-pixel coord of upper left corner of new frame.       
C long   width = width (in pixels) of new frame.        
C long  height = height (in pixels) of new frame.        
C
C if width  == ImageAll and x == 0, the entire frame is copied in.
C If height == ImageAll and y == 0, the entire frame is copied in.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC
*/


#include "plot_image.hh"
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>



void PlotImage::imageMovie(  long             frame,
                             long             x,
                             long             y,
                             long             width,
                             long             height)

{

   if(_dest_x + _ximage.width  < x) return;
   if(_dest_y + _ximage.height < y) return;
   if(width  == 0)                  return;
   if(height == 0)                  return;


   if(frame > _frames - 1)frame = _frames -1 ;
   if(frame < 0) frame = 0;

   _cpixm = (int)frame;
   refresh(x,y,width,height);


#if 0

*  dpy=  XtDisplay(_graphic);
*  scr = XtScreen(_graphic);

*/*see if we are doing a movie of underlay and overlay if so - refresh method*/
   if(_chain_image != NULL)
*    {
*    if(_cpixm != frame &&
*       _user->getMovieOption() &&
*       _chain_image->_user->getMovieOption())
*      {
*      _cpixm = (int)frame;
*      refresh(0,0,ImageAll,ImageAll);
*      return;
*      }
*    }

*  _cpixm = (int)frame;

*  if ( (width  == ImageAll) && (x == 0) ) width = _graph_width;
*  if ( (height == ImageAll) && (y == 0) ) height= _graph_height;

*  if(_chain_image != NULL)
*     {
*     temp_gc = _chain_image->_gc1;
*     XSetPlaneMask( dpy,temp_gc, _chain_image->_col->pmsk[0]);
*     XSetForeground(dpy,temp_gc, _chain_image->_overlay_pixel );
*     XSetBackground(dpy,temp_gc, 0 );
*     }
*  else
*     {
*     temp_gc = _gc1;
*     }

*  if(_ximage.depth > 1)   /*gray scale or color*/
*     {
*     XCopyArea(  dpy, _pixmary[_cpixm],
*                 XtWindow(_graphic), temp_gc,
*                 (int)x, (int)y, (int)width,(int) height,
*                 (int)(x + _dest_x), (int)(y + _dest_y) );
*     }
*  else /*single bit plane wiggles*/
*     {
*     XCopyPlane(  dpy, _pixmary[_cpixm],
*                  XtWindow(_graphic), temp_gc,
*                  (int)x, (int)y, (int)width, (int)height,
*                  (int)(x + _dest_x), (int)(y + _dest_y), 1);
*     XSetPlaneMask(dpy,temp_gc,XAllPlanes()); /*in case of overlay*/
*     }

#endif

}
