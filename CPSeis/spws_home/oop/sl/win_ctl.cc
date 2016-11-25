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
#include "sl/sl_base.hh"
#include "sl/view_win.hh"
#include "sl/paintset_collection.hh"
#include "dumb_manage.h"
#include <stdio.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <X11/IntrinsicP.h>


static char move_bar_trans[] =
 "<Btn1Down>:      moveBarStart()\n\
  <Btn1Motion>:    moveBar() \n\
  <Btn1Up>:        moveBarEnd()";


XtTranslations WinCtl::_move_bar= NULL;



WinCtl::WinCtl(Widget p, char *name)
           : _wincnt(0), _barcnt(0), _row_major(False), 
             ViewObj(NULL), _bars_showing(True), _mhelp_widget(NULL)
{ 
  Display *dpy;
  Screen *scr;
  int i;


  static XtActionsRec actionsTable[] = {
         { "moveBarStart", (XtActionProc)ViewBar::moveBarStart },
         { "moveBar", (XtActionProc)ViewBar::moveBar },
         { "moveBarEnd", (XtActionProc)ViewBar::moveBarEnd },
  };

  _resize_external_function = NULL;

  dpy= XtDisplay(p);
  scr = XtScreen(p);
  Widget w;

  w= XtVaCreateManagedWidget( name, dumbManagerWidgetClass, p, NULL);
  setTopWidget(w);

  XtAddEventHandler( w, StructureNotifyMask, False, 
                     (XtEventHandler)resizeEvent, (XtPointer)this);
  XtAddEventHandler( w, SubstructureNotifyMask, False, 
                     (XtEventHandler)mapCallback, (XtPointer)this);
  //XtAddCallback( w, XmNmapCallback, mapCallback, this);

  for(i= 0;(i<MAXWINLIST); _winlist[i++] = NULL);
  for(i= 0;(i<MAXBARLIST); _barlist[i++] = NULL);
  _hcursor= XCreateFontCursor( dpy, XC_double_arrow);
  _vcursor= XCreateFontCursor( dpy, XC_sb_h_double_arrow);

  _xorgc = XCreateGC( dpy, RootWindowOfScreen(scr), None, NULL);
  XSetBackground (dpy, _xorgc, PaintsetCollection::white(scr));
  XSetForeground (dpy, _xorgc, PaintsetCollection::black(scr));
  XSetFunction( dpy, _xorgc, GXinvert);            //xor for rubberbanding
  XSetSubwindowMode( dpy, _xorgc, IncludeInferiors);

  _oldwidth=  myWidth();
  _oldheight= myHeight();

  XtAppAddActions( XtWidgetToApplicationContext(p),
                   actionsTable, XtNumber(actionsTable) );


  if (!_move_bar) _move_bar= XtParseTranslationTable(move_bar_trans);
  XtVaSetValues( w, XmNuserData, this, NULL);


}


WinCtl::~WinCtl() 
{ 
  int i; 
  Display *dpy;

  dpy= XtDisplay(topWidget());
  for (i=0; (i<MAXWINLIST); i++ ) if (_winlist[i]) _winlist[i]->hide();
  for (i=0; (i<MAXWINLIST); delete _winlist[i++] ); 
  XFreeCursor( dpy, _hcursor);
  XFreeCursor( dpy, _vcursor);
  XFreeGC( dpy, _xorgc); 

  XtDestroyWidget(topWidget());
}


void WinCtl::addWin( ViewWin *win)
{
 int i;
 Boolean done;

 for (i=0, done= False; ( (i<MAXWINLIST) && (!done) ); i++) {
      if (_winlist[i] == NULL) {
           done= True;
           _winlist[i]= win;
      } // End if
 } // End loop
 if (done)
     _wincnt++;
 else
     puts("WinCtl::addWin too many windows.");
}



void WinCtl::addBar( ViewBar *bar)
{
 int i;
 Boolean done;

 for (i=0, done= False; ( (i<MAXBARLIST) && (!done) ); i++) {
      if (_barlist[i] == NULL) {
           done= True;
           _barlist[i]= bar;
      } // End if
 } // End loop
 if (done)
     _barcnt++;
 else
     puts("WinCtl::addbar too many bars.");
}



void WinCtl::delWin( ViewWin *win)
{
 int i;
 Boolean done= False;

 for (i=0; ( (i<MAXWINLIST) && (!done) ); i++) {
      if (_winlist[i] == win) {
           done= True;
           _winlist[i]= NULL;
      } // End if
 } // End loop
 if (done)
     _wincnt--;
 else
     puts("WinCtl::delWin window not found.");
}



void WinCtl::delBar( ViewBar *bar)
{
 int i;
 Boolean done= False;

 for (i=0; ( (i<MAXBARLIST) && (!done) ); i++) {
      if (_barlist[i] == bar) {
           done= True;
           _barlist[i]= NULL;
      } // End if
 } // End loop
 if (done)
     _barcnt--;
 else
     puts("WinCtl::delBar bar not found.");
}


void WinCtl::mapCallback(Widget, 
                         XtPointer udata,
                         XtPointer CBdata)
{
  
  WinCtl *obj = (WinCtl *)udata;
  XMapEvent *ev= (XMapEvent*)CBdata;

  if (ev->type == MappingNotify) {
     obj->_oldwidth=  obj->myWidth();
     obj->_oldheight= obj->myHeight();
  }

}


void WinCtl::resizeEvent(Widget    w, 
                         XtPointer udata,
                         XEvent    *event)
{
   XConfigureEvent *ev;
   WinCtl *obj = (WinCtl *)udata;

   ev= (XConfigureEvent *)event;

   if (XtWindow(w) == ev->window) {
      if (ev->type == MapNotify) {
            obj->_oldwidth=  obj->myWidth();
            obj->_oldheight= obj->myHeight();
      } // end if
      else if (ev->type == ConfigureNotify)
            obj->resize(w, udata, ev);
      else
            /**/puts("other");
   }
}


void WinCtl::resize(Widget, XtPointer, XConfigureEvent *ev)
{
  Position obx, oby; 
  Position nbx, nby; 
  Dimension obw, obh; 
  Dimension len;
  int i;

  if ( (ev->width != _oldwidth) || (ev->height != _oldheight) ) {

       for(i= 0; (i< MAXBARLIST); i++ ) {
            if (_barlist[i]) {
                 obx= _barlist[i]->myX(); 
                 oby= _barlist[i]->myY(); 

                 /*
                  *  find new X & Y
                  */
                 nbx= (obx * ev->width) /  _oldwidth;
                 nby= (oby * ev->height) / _oldheight;
                 XtVaSetValues( _barlist[i]->W(), XmNx, nbx,
                                                  XmNy, nby, NULL );
                 /*
                  *  find new Width or Height
                  */
                 if (_barlist[i]->_isvert) {
                       obh= _barlist[i]->myHeight(); 
                       len= (obh * ev->height) / _oldheight;
                       XtVaSetValues( _barlist[i]->W(), XmNheight, len, NULL );
                 } // end if
                 else {
                       obw= _barlist[i]->myWidth(); 
                       len= (obw * ev->width) / _oldwidth;
                       XtVaSetValues( _barlist[i]->W(), XmNwidth, len, NULL );
                 } // end else
                 _barlist[i]->locate(False);
            } // end if
       } // end loop

       for(i= 0; (i< MAXWINLIST); i++ ) {
          if (_winlist[i]) {
            if (_winlist[i]->_showing) {
              if ( (_winlist[i]->_bar[OnLeft]  == NULL) &&
                   (_winlist[i]->_bar[OnRight] == NULL) ) {
                          _winlist[i]->sizeNoBar(OnLeft);
                          _winlist[i]->sizeNoBar(OnRight);
              } // end if

              if ( (_winlist[i]->_bar[OnTop]    == NULL) &&
                   (_winlist[i]->_bar[OnBottom] == NULL) ) {
                          _winlist[i]->sizeNoBar(OnTop);
                          _winlist[i]->sizeNoBar(OnBottom);
              } // end if
            } // end if
          } // end if
 
       } // end loop

       if (ev->width) _oldwidth= ev->width;
       if (ev->height) _oldheight= ev->height;

   //The following was added 12/98 by MLS to accomodate re-display of
   //existing SeisPlots when a resize is done on the main window. This
   //was done so that all SeisPlots could be replotted to fit the new
   //area of the main window as desired in the Geopress program.
   if( _resize_external_function != NULL )
     {
     _resize_external_function(_resize_external_function_data);
     }


  } // end if
}


void WinCtl::hideBars()
{
 _bars_showing= False;
 for (int i=0; (i<MAXBARLIST); i++)
        if (_barlist[i]) _barlist[i]->hide();
}


void WinCtl::showBars()
{
 _bars_showing= True;
 for (int i=0; (i<MAXBARLIST); i++)
        if (_barlist[i]) _barlist[i]->show();
}
