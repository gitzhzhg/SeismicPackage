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
 *Name   : labelColorArray
 *Purpose: Fills rgb array with velocity info to label each color by.
 *
 *Author : Michael L. Sherrill
 *Date   : 04/93 (C++ version 4/97)
 *
 * Function Definition:
 * void label_velocity( float    *rgb,
 *                      float    vmin,
 *                      float    vmax,
 *                      long     numcolors)
 *
 * rgb           in out RGB color value array.                     
 * vmin          in     Minimum velocity to label.      
 * vmax          in     Maximum velocity to label.                    
 * numcolors     in     Number of colors in color bar.
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
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <stdio.h>
#include "cenv.h"
#include "wproc.h"
#include "plot_image.hh"
#include "sl/paintset_collection.hh"

void PlotImage::labelColorArray( float    *rgb,
                      float    vmin,
                      float    vmax,
                      long     numcolors)
{

float addamt,labelval;
long labelindex = 3;
long i;



 
  addamt = (vmax - vmin) / ((float)numcolors - 1.0);
  labelval = vmin + (addamt/2.0);
  for(i=0;i<numcolors;i++)
     {
     rgb[labelindex] = labelval;
     labelval += addamt;
     labelindex += 4;
     }


  rgb[labelindex-4]=vmax;/*last velocity value*/


}



/*------------------------------------------------------------------
 *USER DOC
 *Name   : loadColors
 *Purpose: load custom colors and draw the color bar.
 *
 *Author : Michael L. Sherrill
 *Date   : 02/92 (C++ version 4/97)
 *
 * Function Definition:
 * void loadcolors( Widget w,
 *               long              predef,
 *               struct            COLOR_INFO *col_cust,
 *               struct            COLOR_INFO *col_gs,
 *               long              *numcolors,
 *               float             *rgb,
 *               long              *change_image_colors)
 *
 * w             in     ID of drawing area widget.
 * predef        in     Flag indicating a predefined color to load.
 * col_cust      in out Color structure of new colors.
 * col_gs        in out Color structure of standard gray scale colors.
 * numcolors     out    Number of new colors loaded in.
 * rgb           in out Array of color rgb values.
 * change_colors in out Flag to load colors or only redraw existing colors.
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


void PlotImage::loadColors( Widget    w,
                            long      predef,
                            ColorInfo *col_cust,
                            ColorInfo *col_gs,
                            long      *numcolors,
                            float     *rgb,
                            long      *change_image_colors,
			    float     cell_width_factor)



{
 if (cell_width_factor != 1.0) {
   partLoadColors (w, predef, col_cust, numcolors, rgb, change_image_colors,
     cell_width_factor);
   return;
 }
 Display *dpy;
 Screen *scr;
 Window win;
 long istat, i, j;
 int n, x1,y1;
 int lx1, ly1;
 int cellwidth, cellheight;
 Dimension width, height;
 Arg arg[22];
 char amplitude[10];
 // char firstlabel[10];
 //char finallabel[10];
 int labellen = 10;
 long compress = 0;
 long intensity = 10;
 ColorInfo *col;
 long tracevalues = False;
 long maxcolors;
 XFontStruct *font_fixed;
 GC customgc;
 int font_height;
 int old_y;

 dpy = XtDisplay(w);
 scr = XtScreen(w);
 win = XtWindow(w);
 customgc = XCreateGC( dpy, win, None, NULL);
 font_fixed = XLoadQueryFont(dpy,"fixed");
 if(font_fixed == NULL || customgc == NULL)
   {
   printf("loadcolors font problem\n");
   return;
   }
 XSetFont(dpy,customgc,font_fixed->fid);
 font_height = font_fixed->max_bounds.ascent + font_fixed->max_bounds.descent;

 //Following commented since currently the gray scale color structure
 //is not being used (col_gs)
 //if(predef == GRAY)
 //   {
 //   col = col_gs; 
 //   *numcolors = col->cnum;
 //   /*dont count black and white overlays*/
 //   if(col->numplanes || col->pmsk[0]) *numcolors -= 2; 
 //   }
 //else
 //   {
      col = col_cust;
 //   }

/* defined_color fills in the color rgb array and returns the number
   of colors it finds in the defined color. It also returns a flag
   to use trace amplitude values for color processing. The maxcolors
   is sent to defined color to fill out the color map with as many
   colors that are available in the color structure pointer col->,
   that were allocated from the application.
   color_compress does the actual storing of xlib colors and
   optionally compresses and sets intensities.
*/


 if(*change_image_colors)
    {
    if(predef)
       {
       maxcolors = col->cnum;
       /*dont count black and white overlays*/
       if(col->numplanes || col->pmsk[0]) maxcolors -= 2;
       definedColor(rgb,predef, numcolors, &tracevalues, maxcolors);
       if(tracevalues)
	 {
         _user->_color_ampmin = rgb[3];
         _user->_color_ampmax = rgb[*numcolors * 4 - 1];
         }
       }
    istat=storeColors(rgb, *numcolors, &compress, &intensity, col, NULL);
    *change_image_colors = False;
    }

/** no longer needed, this use to label the color pop menu with velocities 
 if(_user->_mode == PlotISO)
    { did use user->_color_ampmin and max instead of the following vel_min & max
    label_velocity(rgb,_user->_vel_min,_user->_vel_max,*numcolors);
    _user->_num_cbar_cols = *numcolors;
    }
**/

 n=0;
 XtSetArg(arg[n], XmNwidth, &width); n++;
 XtSetArg(arg[n], XmNheight,&height);n++;
 XtGetValues(w, arg, n);        
 XSetForeground(dpy, customgc, PaintsetCollection::white(scr));
 XSetBackground(dpy, customgc, PaintsetCollection::black(scr));
 XFillRectangle(dpy, win, customgc, 0,0,width,height);

 cellwidth = (int)(width * .50);
/* cellheight = (height * .97) / *numcolors;*/
 cellheight = (int)(height / (*numcolors + 1));
 x1 = (int)(width * .48);
 y1 = (int)(height * .03);
 lx1 = 3;
 ly1 = y1 + cellheight;
 old_y = 0;
 j = 3;

 for(i=0;i< *numcolors;i++)
    {
    XSetForeground(dpy, customgc, col->pix[i]);
    XFillRectangle(dpy, win, customgc, x1,y1,cellwidth,cellheight);    
    //  if(predef != GRAY && _user->_mode != PlotSEMB)
    //   {
       sprintf(amplitude,"%10.3e",rgb[j]);
       XSetForeground(dpy, customgc, PaintsetCollection::black(scr));
       if( (ly1 - old_y >= font_height - 2) && (i < *numcolors - 1) )
         {
         XDrawString(dpy,win,customgc,lx1,ly1, amplitude,labellen);
         XDrawLine(dpy, win ,customgc,lx1,ly1,x1,ly1);
         old_y = ly1;
         }
       // }
    y1 += cellheight;
    ly1 += cellheight;
    j += 4;
    }

 
/*label as min and max byte values only on gray scale and semblance*/
 /*******
 if(predef == GRAY || _user->_mode == PlotSEMB)
    {
    if(_user->_rp)
       {
       sprintf(firstlabel,"%s","Max");
       sprintf(finallabel,"%s","Min");
       }
    else
       {
       sprintf(firstlabel,"%s","Min");
       sprintf(finallabel,"%s","Max");
       }
    XSetForeground(dpy, customgc, BlackPixelOfScreen(scr));
    ly1 = (int)(height * .03);
    x2 = x1 + cellwidth;
    XDrawString(dpy,win,customgc,lx1,ly1,firstlabel,3);
    XDrawLine(dpy, win ,customgc,lx1,ly1,x2,ly1);
    ly1 = y1;
    XDrawString(dpy,win,customgc,lx1,ly1,finallabel,3);
    XDrawLine(dpy, win ,customgc,lx1,ly1,x2,ly1);
    }
    *****/

 
 XFreeGC(dpy, customgc);
 XFreeFont(dpy,font_fixed);
}

void PlotImage::partLoadColors (Widget    w,
				long      predef,
				ColorInfo *col_cust,
				long      *numcolors,
				float     *rgb,
				long      *change_image_colors,
				float     cell_width_factor)



{
  Display *dpy;
  Screen *scr;
  Window win;
  long istat, i;
  int n, x1,y1;
  int cellwidth, cellheight;
  Dimension width, height;
  Arg arg[22];
  long compress = 0;
  long intensity = 10;
  ColorInfo *col;
  long tracevalues = False;
  long maxcolors;
  GC customgc;

  dpy = XtDisplay (w);
  scr = XtScreen (w);
  win = XtWindow (w);
  customgc = XCreateGC (dpy, win, None, NULL);

  col = col_cust;

/* defined_color fills in the color rgb array and returns the number
   of colors it finds in the defined color. It also returns a flag
   to use trace amplitude values for color processing. The maxcolors
   is sent to defined color to fill out the color map with as many
   colors that are available in the color structure pointer col->,
   that were allocated from the application.
   color_compress does the actual storing of xlib colors and
   optionally compresses and sets intensities.
*/


  if (*change_image_colors) {
    if(predef) {
      maxcolors = col->cnum;
      /*dont count black and white overlays*/
      if (col->numplanes || col->pmsk[0]) maxcolors -= 2;
      definedColor (rgb,predef, numcolors, &tracevalues, maxcolors);
      if (tracevalues) {
	_user->_color_ampmin = rgb[3];
	_user->_color_ampmax = rgb[*numcolors * 4 - 1];
      }
    }
    istat = storeColors (rgb, *numcolors, &compress, &intensity, col, NULL);
    *change_image_colors = False;
  }

  n = 0;
  XtSetArg (arg[n], XmNwidth, &width); n++;
  XtSetArg (arg[n], XmNheight,&height);n++;
  XtGetValues (w, arg, n);        
  XSetForeground (dpy, customgc, PaintsetCollection::white(scr));
  XSetBackground (dpy, customgc, PaintsetCollection::black(scr));

  cellwidth = (int)(cell_width_factor * width * .50);
  if (cellwidth < 1) cellwidth = 1;
  cellheight = (int)(height / (*numcolors + 1));
  x1 = (int)(width * .48);
  y1 = (int)(height * .03);

  for (i = 0; i < *numcolors; i++) {
    XSetForeground (dpy, customgc, col->pix[i]);
    XFillRectangle (dpy, win, customgc, x1, y1, cellwidth, cellheight);    
    y1 += cellheight;
  }

  XFreeGC (dpy, customgc);
  XFlush (dpy);
}
