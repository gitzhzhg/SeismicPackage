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
C      SeisDrag.cc
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y
C             written in c++ -- designed to be called from c++
C
C     Utility Name:  SeisDrag       SeisPlot Class
C          Written:  93/12/15  by:  Michael L. Sherrill
C     Last revised:  
C
C          Purpose:  Installs required action translations to enable 
C                    moving a bitmap image over a pixmap image.
C                    When finished it re-installsthe actions previously 
C                    used by the application.
C
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY
C  node:                   pospsv (ultrix)
C  source code directory:  
C  library:                
C  header file:            SeisDrag.h            (shared)
C  source file:            SeisDrag.cc
C
C  static functions:       none
C  documented functions:   SeisDrag
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     SeisPlot.a     image.a    wproc.a
C  header files:  SeisDrag.h     SeisPlot.h 
C  functions:     See SeisDrag.h class function definitions
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date           Author              Description
C     ----           ------              -----------
C  2. 
C  1. 93/09/20  Michael L. Sherrill      Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC
*/

#include "sp/seis_drag.hh"
#include "sp/inform_list.hh"
#include <stdlib.h>
#include "cprim.h"

static char *draghelp= "mouse*DRAG:  BTN#1: Drag Image, \
BTN#2: Original Image Position, BTN#3: Popup Menu";

XtTranslations    SeisDrag::_pointer_trans=NULL;

/******** drag actions *********/
/** these can be modified to preferred actions*/


static char pointer_actions[] =
    "<Btn1Down>:          DoDrag(s)\n\
     <Btn1Motion>:        DoDrag(m)\n\
     <Btn1Up>:            DoDrag(q)\n\
     <Btn2Down>:          DoDrag(o)";
    





SeisDrag::SeisDrag( SeisPlot   *oversp, SeisPlot *undersp)
                   : _tiesp(NULL), _old_sp_mousehelp(NULL),
                     _old_tie_mousehelp(NULL)
{
 Arg args[22];
 int n;

  _oversp = oversp;
  _undersp= undersp;
  _ip = &oversp->_image;
  _dest_x = (int)oversp->imageXloc();
  _dest_y = (int)oversp->imageYloc();
  //save old translations
  n = 0;
  XtSetArg (args[n], XmNtranslations, &_original_trans); n++;
  XtGetValues(this->_ip->graphicWidget(),args,n);
}



SeisDrag::~SeisDrag()
{
 Display *dpy = XtDisplay( _oversp->imageGraphic() );
 
  HelpCtx hctx= _oversp->getHelpCtx();
  if ((hctx) && (_old_sp_mousehelp))
            ctxhChangeTolken(hctx, _oversp->imageGraphic(), _old_sp_mousehelp);
  if ((hctx) && (_old_tie_mousehelp) && (_tiesp) )
            ctxhChangeTolken(hctx, _tiesp->imageGraphic(), _old_tie_mousehelp);
  if (_old_sp_mousehelp) free(_old_sp_mousehelp);
  if (_old_tie_mousehelp) free(_old_tie_mousehelp);

  XtUninstallTranslations( _oversp->imageGraphic() );
  XtOverrideTranslations ( _oversp->imageGraphic(),  _original_trans);
}



void SeisDrag::setTiePlot(SeisPlot *sp) 
{ 
  _tiesp= sp;
  _separate_window = False;
}




void SeisDrag::startDrag()
{
 Arg args[22];
 int n;
 HelpCtx hctx = _oversp->getHelpCtx();
 Screen  *scr = XtScreen (_oversp->imageGraphic());
 Display *dpy = XtDisplay(_oversp->imageGraphic());
 static XtActionsRec drag_table[] = {
     {"DoDrag", (XtActionProc) &SeisDrag::DragTrans}, };

  if(_oversp->imageGraphic() == NULL) return;
  n = 0;
  XtSetArg (args[n], XmNuserData, this); n++;
  XtSetValues(_oversp->imageGraphic(),args,n);

//install drag help and drag translations
  if ( (!_pointer_trans) && (hctx) )
          ctxhMergeHelpLine(hctx, draghelp);
  if (!_pointer_trans)_pointer_trans= XtParseTranslationTable(pointer_actions);
  XtAppAddActions(XtWidgetToApplicationContext( _oversp->imageGraphic() ),
                  drag_table, XtNumber(drag_table));
  XtUninstallTranslations( _oversp->imageGraphic() );
  XtOverrideTranslations (_oversp->imageGraphic(),  _pointer_trans);

  if (hctx) 
    {
    _old_sp_mousehelp = ctxhGetTolken(hctx, _oversp->imageGraphic() );
    if (_old_sp_mousehelp) 
       {
         _old_sp_mousehelp = newstr(_old_sp_mousehelp);
         ctxhChangeTolken(hctx, _oversp->imageGraphic(), "DRAG");
       }
    if (_tiesp) 
       {
       _old_tie_mousehelp= ctxhGetTolken(hctx, _tiesp->imageGraphic() );
       if (_old_tie_mousehelp) 
          {
            _old_tie_mousehelp= newstr(_old_tie_mousehelp);
            ctxhChangeTolken(hctx, _tiesp->imageGraphic(), "DRAG");
	  }
       }
    }

}



void SeisDrag::DragTrans(Widget w,
                         XEvent *event,
                         char   *mode[])
{
 SeisDrag *sd;
 Arg args[22];
 int n = 0;

  XtSetArg (args[n], XmNuserData,&sd); n++;
  XtGetValues(w,args,n);
  sd->DoDrag(w, event, mode);
}


void SeisDrag::DoDrag( Widget          ,
                       XEvent          *event,
                       char            *mode[])
{
 static long oldx    = 0;
 static long oldy    = 0;
 long newx, newy, movex, movey;
 XMotionEvent *mev = (XMotionEvent *)event;
 Display *dpy = XtDisplay( _oversp->imageGraphic());
 GC temp_gc;
 long x1, y1, width1, height1;
 long x2, y2, width2, height2;
 long maxx, maxy;

  maxx = _oversp->leftBorder() + _oversp->rightBorder()
       + _oversp->_image.getXimageWidth();
  maxy = _oversp->topBorder() + _oversp->bottomBorder()
       + _oversp->_image.getXimageHeight();

//turn it all off if someone has removed the underlay image
  if(_oversp->_image.getChainImage() == NULL)
     {
     _oversp->_image.setXdestination(_oversp->_image.getOrigXdestination());
     _oversp->_image.setYdestination(_oversp->_image.getOrigYdestination());
     _doing_drag = False;
     _oversp->_image.refresh(0,0,PlotImage::ImageAll,PlotImage::ImageAll);
     dragComplete(_oversp);
     dragComplete(_undersp);
     return;
     }


// check for button 2 reset to original position
  if( *mode[0] == 'o')
     {
     _oversp->_image.setXdestination(_oversp->_image.getOrigXdestination());
     _oversp->_image.setYdestination(_oversp->_image.getOrigYdestination());
     _doing_drag = False;
     width1 = max(maxx, _oversp->_image.getChainImage()->getGraphWidth());
     height1= max(maxy, _oversp->_image.getChainImage()->getGraphHeight());
     _oversp->_image.refresh(0,0,width1,height1);
     dragComplete(_oversp);
     dragComplete(_undersp);
     return;
     }

// check for button 1 release
  if( *mode[0] == 'q')
     {
     _doing_drag = False;
     width1 = max(maxx, _oversp->_image.getChainImage()->getGraphWidth());
     height1= max(maxy, _oversp->_image.getChainImage()->getGraphHeight());
     _oversp->_image.refresh(0,0,width1,height1);
     dragComplete(_oversp);
     dragComplete(_undersp);
     return;
     }

// if starting drag
  if(*mode[0] == 's')
     {
     _doing_drag = True;
     oldx = mev->x;
     oldy = mev->y;
     return;
     }


// if ready to move
  if(*mode[0] == 'm' && _doing_drag)
     {
     movex = mev->x - oldx;
     movey = mev->y - oldy;
     newx = movex + _oversp->imageXloc();
     newy = movey + _oversp->imageYloc();
     
     //erase old area
     if(newx >= _oversp->imageXloc() && newy >= _oversp->imageYloc())
        {
        x1      = _oversp->imageXloc(); 
        y1      = _oversp->imageYloc(); 
        width1  = maxx;
        height1 = newy - _oversp->imageYloc();
        x2      = _oversp->imageXloc();
        y2      = newy;
        width2  = newx - _oversp->imageXloc();
        height2 = maxy - (newy - _oversp->imageYloc());
        }
     if(newx <= _oversp->imageXloc() && newy <= _oversp->imageYloc())
        {
        x1      = _oversp->imageXloc();
        y1      = newy + maxy;
        width1  = maxx;
        height1 = _oversp->imageYloc() - newy;
        x2      = newx + maxx;
        y2      = _oversp->imageYloc();
        width2  = _oversp->imageXloc() - newx;
        height2 = maxy - (_oversp->imageYloc() - newy);
        }
     if(newx <= _oversp->imageXloc() && newy >= _oversp->imageYloc())
        {
        x1      = _oversp->imageXloc();
        y1      = _oversp->imageYloc();
        width1  = maxx;
        height1 = newy - _oversp->imageYloc();
        x2      = newx + maxx;
        y2      = newy;
        width2  = _oversp->imageXloc() - newx;
        height2 = maxy - (newy - _oversp->imageYloc());
        }
     if(newx >= _oversp->imageXloc() && newy <= _oversp->imageYloc())
        {
        x1      = _oversp->imageXloc();
        y1      = _oversp->imageYloc();
        width1  = newx - _oversp->imageXloc();
        height1 = maxy;
        x2      = newx;
        y2      = newy + maxy;
        width2  = maxx - (newx - _oversp->imageXloc());
        height2 = _oversp->imageYloc() - newy;
        }
     temp_gc = _oversp->_image.getChainImage()->getPrimaryGC();
     XSetPlaneMask( dpy,temp_gc, 
                _oversp->_image.getChainImage()->getColorStructure()->pmsk[0]);
     XSetBackground(dpy,temp_gc, 
                _oversp->_image.getChainImage()->getOverlayPixel());
     XSetForeground(dpy,temp_gc, 0 );
     XFillRectangle(dpy, XtWindow(_oversp->imageGraphic()),
                    temp_gc,(int)x1,(int)y1,(int)width1,(int)height1);
     XFillRectangle(dpy, XtWindow(_oversp->imageGraphic()),
                    temp_gc,(int)x2,(int)y2,(int)width2,(int)height2);
     _oversp->_image.setXdestination(newx);
     _oversp->_image.setYdestination(newy);
     _oversp->_image.imageMovie(_oversp->_image.getCurrentPixmapIndex(),
                                0,0, maxx,maxy);
     oldx = mev->x;
     oldy = mev->y;
     }

}


void SeisDrag::dragComplete(SeisPlot *sp)
{
  sp->_inform_list->callDragImage(sp);
}
