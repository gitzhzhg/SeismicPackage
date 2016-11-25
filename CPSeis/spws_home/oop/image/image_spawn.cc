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
   THIS WILL GO AWAY AFTER SEISPLOT IS FINISHED
*/

/*------------------------------------------------------------------
 *USER DOC
 *Name   : spawnImage 
 *Purpose: Generate a iso image to be used as the background for stacked
 *         trace overlays
 *
 *Author : Michael L. Sherrill
 *Date   : 06/93
 *
 * Function Definition:
 *
 *        PlotImage * spawn_image( struct PlotImage *sourceimage,
 *                                 struct PlotImage *parentimage,
 *                                 long              width,
 *                                 long              height,
 *                                 float             ti,
 *                                 float             is,
 *                                 float             grid_x1,
 *                                 float             grid_x2)
 *
 * sourceimage  in      Base image to create new image from
 * parentimage  in      New image will be chained to this image
 * width        in      Create image to this width
 * height       in      Create image to this height
 * ti           in      Traces per inch of image to match
 * is           in      Inches per second of image to match
 * grid_x1      in      Begin image at this grid id.
 * grid_x2      in      End image at this grid id.
 *
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

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "plot_image.hh"

#define resource_err "Not enough memory for the overlay image."


PlotImage *PlotImage::spawnImage( struct PlotImage *sourceimage,
                                  struct PlotImage *parentimage,
                                  long              width,
                                  long              /*height*/,
                                  float             tmin,
                                  float             tmax,
                                  float             /*ti*/,
                                  float             /*is*/,
                                  float             grid_x1,
                                  float             grid_x2)

{
 struct ImageInput *newuser;
 struct PlotImage *newimage;
 float hpixels_per_inch;
 float vpixels_per_inch;
 Display *dpy;
 int screen;
 long stat;
 float fixed_iso_sample = .004;


/*no chained image memory allocation done yet*/
  if(parentimage->_chain_image == NULL)
     {
     newimage = NULL;
     newuser  = NULL;
     newimage = new PlotImage();
     if(newimage == NULL){strcpy(_errstr,resource_err);return(NULL);}
     newuser = new ImageInput();
     if(newuser == NULL){strcpy(_errstr,resource_err);return(NULL);}
     }
  else
     {
     newimage = parentimage->_chain_image;
     newuser  = parentimage->_chain_image->_user;
     }

/*/////////////// old /////////////////////////
  memcpy(newimage, sourceimage, sizeof (PlotImage) );
*//////////////// old /////////////////////////
///////////////// new /////////////////////////
  sourceimage->memcpyThis (newimage);
///////////////// new /////////////////////////
  memcpy(newuser, sourceimage->_user, sizeof ( ImageInput) );

  newimage->_user = newuser;
  newimage->_graphic = _graphic;    /*replace widget graphic id*/
  sourceimage->_pixmary[0] = newimage->_pixmary[0]; /*prevent loss of original*/
  newimage->_pixmary[0] = 0;
  newimage->_can_overlay = True;

  /*treat new image like a zoom factor of 1 of the
    old image so that new arrays will not be allocated in check_size.*/
  newimage->_zoomed = True;

  dpy = XtDisplay(sourceimage->_graphic);
  screen = DefaultScreen(XtDisplay(sourceimage->_graphic));
  hpixels_per_inch = horizontalPixelsPerInch(dpy, screen);
  vpixels_per_inch = verticalPixelsPerInch(dpy, screen);
  newimage->_user->_ti = ((float)(width-1)) / hpixels_per_inch;
  newimage->_user->_is = parentimage->_user->_is;
  newimage->_grid_x1 = grid_x1;
  newimage->_grid_x2 = grid_x2;
  newimage->_user->_annotate = False;
  newimage->_user->_movie = newimage->_frames = 0;
  newimage->_user->_tmin = max(parentimage->_tmin,tmin);
  newimage->_user->_tmax = min(parentimage->_tmax,tmax);
  newimage->_ximage.height = (int)((tmax - tmin - fixed_iso_sample)
                             * parentimage->_user->_is * vpixels_per_inch);

  if(newimage->_ximage.height < parentimage->_ximage.height)
     {
     newimage->_height_diff = 
         parentimage->_ximage.height-newimage->_ximage.height;
     newimage->_ximage.height = parentimage->_ximage.height;
     }
  else
     {
     newimage->_height_diff = 0;
     }

  stat = newimage->checkSize();
  if(stat < 1)
     {
     newimage->imageFree();   
     return(NULL);
     }

  stat = newimage->plot();
  if(stat < 1) 
     {
     newimage->imageFree();
     return(NULL);
     }
  else
     {
     return(newimage);
     }

}
