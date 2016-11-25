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
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  NOTE THIS METHOD NOT BEING USED - loadColors is doing the work
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
/*------------------------------------------------------------------
 *USER DOC
 *Name   : drawcbar        
 *Purpose: Draw color bar popup colors. 
 *
 *Author : Michael L. Sherrill
 *Date   : 02/92 (C++ version 4/97)
 *
 * Function Definition:
 * void drawCbar( Widget w)
 *
 * w             in     ID of drawing area widget.
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
#include "plot_image.hh"
#include "sl/paintset_collection.hh"

//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  NOTE THIS METHOD NOT BEING USED - loadColors is doing the work
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
void PlotImage::drawCbar( Widget w)


{
 Display *dpy;
 Screen *scr;
 Window win;
 long i, j;
 int n;
 int x1, y1, x2;
 int lx1, ly1;
 int cellwidth, cellheight;
 Dimension width, height;
 Arg arg[22];
 char amplitude[20];
 char firstlabel[10];
 char finallabel[10];
 int labellen = 10;
 XFontStruct *font_fixed;
 GC customgc;

 dpy = XtDisplay(w);
 scr = XtScreen(w);
 win = XtWindow(w);
 if (!win) return;

 customgc = XCreateGC( dpy, win, None, NULL);
 font_fixed = XLoadQueryFont(dpy,"fixed");
 if(font_fixed == NULL || customgc == NULL)
   {
   printf("loadcolors font problem\n");
   return;
   }
 XSetFont(dpy,customgc,font_fixed->fid);

 n=0;
 XtSetArg(arg[n], XmNwidth, &width); n++;
 XtSetArg(arg[n], XmNheight,&height);n++;
 XtGetValues(w, arg, n);      
 XSetForeground(dpy, customgc, PaintsetCollection::white(scr));
 XSetBackground(dpy, customgc, PaintsetCollection::black(scr));
 XFillRectangle(dpy, win, customgc, 0,0,width,height);

 cellwidth = (int)((float)width * .50);
 cellheight = (int)(((float)height * .97) / _user->getNumberCbarColors());
 x1 = (int)((float)width * .48);
 y1 = (int)((float)height * .03);
 lx1 = 3;
 ly1 = y1 + cellheight;

  
 
 if(_user->getReversePolarity())
    j = _user->getNumberCbarColors() * 4 - 1;                     
 else
    j = 3;


 for(i=0;i<_user->getNumberCbarColors();i++)
    {
    XSetForeground(dpy, customgc, _col->pix[i]);
    XFillRectangle(dpy, win, customgc, x1,y1,cellwidth,cellheight);    
    if(i < _user->getNumberCbarColors() - 1 && _user->getNumberCbarColors() < 65
                                          && _user->getMode() != PlotSEMB)
       {
       XSetForeground(dpy, customgc, PaintsetCollection::black(scr));
       sprintf(amplitude,"%10.3e",_user->getCbarRgb(j));
       XDrawString(dpy,win,_gc1,lx1,ly1, amplitude,labellen);
       XDrawLine(dpy, win ,_gc1,lx1,ly1,x1,ly1);
       }
    y1 += cellheight;
    ly1 += cellheight;
    if(_user->getReversePolarity())
       j -= 4;
    else
       j += 4;
    }

/*label 1 and 255 byte values only on gray scale*/
 if(_user->getNumberCbarColors() > 64 || _user->getMode() == PlotSEMB)
    {
    if(_user->getReversePolarity()) 
       {
       sprintf(firstlabel,"%s","Max");
       sprintf(finallabel,"%s","Min");
       }
    else
       {
       sprintf(firstlabel,"%s","Min");
       sprintf(finallabel,"%s","Max");
       }
    XSetForeground(dpy, customgc, PaintsetCollection::black(scr));
    ly1 = (int)((float)height * .03);
    x2 = x1 + cellwidth;
    XDrawString(dpy,win,_gc1,lx1,ly1,firstlabel,7);
    XDrawLine(dpy, win ,_gc1,lx1,ly1,x2,ly1);
    ly1 = y1;
    XDrawString(dpy,win,_gc1,lx1,ly1,finallabel,7);
    XDrawLine(dpy, win ,_gc1,lx1,ly1,x2,ly1);
    }
 
 XFreeGC(dpy, customgc);

}
