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
#include "sl/sl_scroll_win.hh"
#include "sl/slp_scroll.hh"
#include "sl/scroll_list.hh"
//#include "file_choice.h"
#include <X11/IntrinsicP.h>
#include <Xm/Xm.h>
#include <X11/Xatom.h>
#include "dumb_manage.h"
///////////////////////// new ///////////////////////////////////////
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"
///////////////////////// new ///////////////////////////////////////


SLScrollWin::SLScrollWin( const Widget p, const char *name) 

                    : SLDelay((char*)name,NULL,False),
                      _right_border(50),       _left_border(50),
                      _top_border(50),         _bottom_border(50),
                      _save_right_border(50),  _save_left_border(50),
                      _save_top_border(50),    _save_bottom_border(50),
                      _h_vis(True),            _v_vis(True),
                      _work_area(NULL),        _side_right(True),
                      _side_bottom(True),      _anno_persistent(True),
                      _vert_slaved(False),     _hor_slaved(False),
                      _clip_width(1),          _clip_height(1),
                      _master_hor_sw(NULL),    _master_vert_sw(NULL),
                      _always_show_vsb(False), _always_show_hsb(False),
                      _curr_x(0),              _curr_y(0), 
                      _lv_x(0),                _lv_y(0),
                      _lv_width(0),            _lv_height(0), 
                      _dragging(False),        _corner_anno_str(NULL),
                      _font(NULL),             _which_corner(NW),
                      _vert_slave_list(new ScrollList()),
                      _hor_slave_list(new ScrollList()),

    _left_annotation_on(True), _right_annotation_on(True),
    _top_annotation_on(True), _bottom_annotation_on(True)

{
  Widget w;
  make(p);
///////////////////////// new ///////////////////////////////////////
  Arg arglist[22];
  int n = 0;

  Widget parent = makeFrameIfNeeded (p);
  PaintsetCollection::addResources (arglist, &n, parent);
  Pixel foreground = PaintsetCollection::white (parent);
  Pixel background;
  Paintset *paintset = PaintsetCollection::fetch (parent);
  background = PaintsetCollection::clear (parent);
  XtSetArg (arglist[n], XmNbackground, background); n++;
  XtSetArg (arglist[n], XmNforeground, foreground); n++;
  XtSetArg (arglist[n], XmNresizePolicy, XmRESIZE_NONE); n++;
  XtSetArg (arglist[n], XmNmarginHeight, 0); n++;
  XtSetArg (arglist[n], XmNmarginWidth, 0); n++;
  w = XtCreateManagedWidget (name, topClass(), parent, arglist, n);
///////////////////////// new ///////////////////////////////////////

/*
//////////////// old ///////////////////////////////////////////
   w= XtVaCreateManagedWidget( _name, topClass(), makeFrameIfNeeded(p), 
                       XmNresizePolicy, XmRESIZE_NONE, 
                       XmNmarginHeight, 0,
                       XmNmarginWidth, 0, NULL );
*/
   setTopWidget(w); 
   _vsb= new SLpScroll( this, "vsb", VERT, SLpScroll::_VERTICAL );
   _hsb= new SLpScroll( this, "hsb", HOR,  SLpScroll::_HORIZONTAL );
   _vsb->unmanage();
   _hsb->unmanage();

   _border = XtVaCreateManagedWidget( "border", dumbManagerWidgetClass, w, 
                                      XmNwidth, 2, XmNheight, 2,
///////////////////////// new ///////////////////////////////////////
                                      XmNbackground, background,
                                      XmNforeground, foreground,
///////////////////////// new ///////////////////////////////////////
				      NULL );
   _clip = XtVaCreateManagedWidget( "clip", dumbManagerWidgetClass, _border,
                                    XmNwidth, 600, XmNheight, 600,
///////////////////////// new ///////////////////////////////////////
				    XmNbackground, background,
				    XmNforeground, foreground,
///////////////////////// new ///////////////////////////////////////
				    NULL );

   XtAddEventHandler(_border, ExposureMask|StructureNotifyMask, False,
                    (XtEventHandler)daCB, (XtPointer)this );
   XtAddEventHandler( w, StructureNotifyMask, False,
                     (XtEventHandler)daCB, (XtPointer)this);

   _gc= XCreateGC( XtDisplay(w), RootWindowOfScreen(XtScreen(w)), 0, NULL);

   setCornerFillColor (background);
}


SLScrollWin::~SLScrollWin()
{
  delete _vert_slave_list;
  delete _hor_slave_list;
  delete _vsb;
  delete _hsb;
  XFreeGC(display(), _gc);
  if (_corner_anno_str) free(_corner_anno_str);
}


Widget SLScrollWin::make(Widget p)
{
  SLDelay::make(p);
  return topWidget();
}



void SLScrollWin::setWorkArea(Widget w)
{
 XtAddCallback( w, XmNresizeCallback, (XtCallbackProc)daCB, (XtPointer)this);
 _work_area= w;
 resize();
}


void SLScrollWin::setCornerFillColor(Pixel c)
{ 
  _fill_color= c;
}

void SLScrollWin::resetCornerFillColor()
{
  if (made()) {
      Pixel back_pix;
      XtVaGetValues(_work_area, XmNbackground, &back_pix, NULL);
      setCornerFillColor(back_pix);
  }
  else
      printf("SLScrollWin::resetCornerFillColor object must be made.\n");
}

void SLScrollWin::setCornerAnnotation(char        *str, 
                                      XFontStruct *font,
                                      WhichCorner  corner)
{
  _which_corner= corner; 
  if (_corner_anno_str) free(_corner_anno_str);
  _corner_anno_str= newstr(str);
  _font= font;
  if (made() && XtWindow(W())) redrawCorners();
}

char *SLScrollWin::getCornerAnnotation()
{
  return newstr(_corner_anno_str);
}

SLScrollWin::WhichCorner SLScrollWin::getCornerAnnotationCorner()
{
  return _which_corner;
}



void SLScrollWin::clearAnnotation()
{
  if (made()) {
      XClearWindow( XtDisplay(_border), XtWindow(_border) );
  }
  else
      printf("SLScrollWin::clearAnnotation object must be made.\n");
}


void SLScrollWin::setLeftBorder(int b)
{
  if (_anno_persistent) _left_border= _save_left_border= b;
  else                  _save_left_border= b;
  resize();
}

void SLScrollWin::setRightBorder(int b)
{
  if (_anno_persistent) _right_border= _save_right_border= b;
  else                  _save_right_border= b;
  resize();
}

void SLScrollWin::setTopBorder(int b)
{
  if (_anno_persistent) _top_border= _save_top_border= b;
  else                  _save_top_border= b;
  resize();
}

void SLScrollWin::setBottomBorder(int b)
{
  if (_anno_persistent) _bottom_border= _save_bottom_border= b;
  else                  _save_bottom_border= b;
  resize();
}


void SLScrollWin::setLeftAnnotationOn(Boolean s)
{
  _left_annotation_on= s;
  resize();
}

void SLScrollWin::setRightAnnotationOn(Boolean s)
{
  _right_annotation_on= s;
  resize();
}

void SLScrollWin::setTopAnnotationOn(Boolean s)
{
  _top_annotation_on= s;
  resize();
}

void SLScrollWin::setBottomAnnotationOn(Boolean s)
{
  _bottom_annotation_on= s;
  resize();
}





void SLScrollWin::showAnnotation(Boolean show)
{
  if (show != _anno_persistent) {
     if (show) {
          _left_border= _save_left_border;
          _right_border= _save_right_border;
          _top_border= _save_top_border;
          _bottom_border= _save_bottom_border;
     }
     else {
          _left_border= _right_border= _top_border= _bottom_border= 0;
     }
     _anno_persistent= show;
     resize();
  } // end if (show != _anno_persistent)
}


Boolean SLScrollWin::annotationShowing()
{
  return _anno_persistent;
}




void SLScrollWin::computePlotSize(long&      new_h_size,   
                                  long&      new_v_size,
                                  Dimension& w_width, 
                                  Dimension& w_height)
{
 Dimension curr_hsb_height= _hsb->oHeight();
 Dimension curr_vsb_width=  _vsb->oWidth();
 if (_work_area)
    XtVaGetValues(_work_area, XmNwidth, &w_width, XmNheight, &w_height, NULL);
 else {
   w_height= 500;
   w_width=  500;
 }
 new_h_size= w_width  - _clip_width;
 new_v_size= w_height - _clip_height;
}




void SLScrollWin::setScrollSize()
{
 Dimension w_width, w_height;
 long new_v_size;
 long new_h_size;
 long imax;

 computePlotSize(new_h_size, new_v_size, w_width, w_height);

 if ( (new_h_size > (_left_border+_right_border)) && (_work_area) ) {
        imax= w_width-_right_border;
        _hsb->setImin(_left_border);
        _hsb->setImax(imax);
        if (_hsb->scrollValue() < _left_border) 
                   _hsb->setScrollValue(_left_border);
        if (_hsb->scrollValue() > w_width-_right_border) 
                   _hsb->setScrollValue(w_width-_right_border);
        if ( (imax - _hsb->scrollValue()) < _clip_width)
                   _hsb->setScrollValue(imax-_clip_width);
        _hsb->setSlider(_clip_width);
        _hsb->setIpage(_clip_width/2);
        _hsb->setIstep(_clip_width/30);
        _h_vis= _hor_slaved ? False : True; 
        notifyScrollBars(_hsb);
 }
 else { 
        _h_vis=  _always_show_hsb;  // false-unless we want to always see hsb
        _hsb->setScrollValue(_left_border);
        _hsb->setSlider(w_width);
        if (_work_area) notifyScrollBars(_hsb);
 }

 if ( (new_v_size > (_top_border+_bottom_border))  && (_work_area) ){
        imax= w_height-_bottom_border;
        _vsb->setImin(_top_border);
        _vsb->setImax(imax);
        if (_vsb->scrollValue() < _top_border) 
                   _vsb->setScrollValue(_top_border);
        if (_vsb->scrollValue()>w_height-_bottom_border)
                   _vsb->setScrollValue(w_height-_bottom_border);
        if ( (imax - _vsb->scrollValue()) < _clip_height)
                   _vsb->setScrollValue(imax-_clip_height);
        _vsb->setSlider(_clip_height);
        _vsb->setIpage(_clip_height/2);
        _vsb->setIstep(_clip_height/30);
        _v_vis= _vert_slaved ? False : True; 
        notifyScrollBars(_vsb);
 }
 else {
        _v_vis= _always_show_vsb;  // false-unless we want to always see vsb
        _vsb->setScrollValue(_top_border);
        _vsb->setSlider(w_height);
        if (_work_area) notifyScrollBars(_vsb);
 }

}

void SLScrollWin::daCB(Widget w, 
                       XtPointer udata,
                       XEvent *event)
{
  SLScrollWin *obj = (SLScrollWin *)udata;
  if ( (w == obj->_work_area)||(w == obj->_clip) ) {
      obj->resizeWorkArea();
  }
  if (w == obj->W()) {
      obj->horizontalSB(obj->_h_vis);
      obj->verticalSB(obj->_v_vis);
  }
  else {
      if (event->type == Expose)  {
          if (!store_events((XExposeEvent*)event))
                   obj->expose(w, udata, event);
      }
      else if (event->type == ConfigureNotify) obj->resize();
  }
}

void SLScrollWin::sendEH( Widget, XtPointer udata, XEvent*)
{
  SLScrollWin *obj = (SLScrollWin *)udata;
  obj->setAfter();
}


void SLScrollWin::setAfter()
{
     horizontalSB(_h_vis);
     verticalSB(_v_vis);
}

void SLScrollWin::resizeWorkArea()
{
   if (_h_vis || _v_vis) {
        horizontalSB(False);
        verticalSB(False);
   }
   else 
        resize();
}


void SLScrollWin::resize()
{
 Dimension cw, ch, ww, wh;
 Dimension horizontal_factor= 0;
 Dimension vertical_factor  = 0;
 Boolean using_method_2_for_width= False;
 Boolean using_method_2_for_height= False;


 horizontal_factor+= _left_annotation_on   ? _left_border   : 0;
 horizontal_factor+= _right_annotation_on  ? _right_border  : 0;
 vertical_factor  += _top_annotation_on    ? _top_border    : 0;
 vertical_factor  += _bottom_annotation_on ? _bottom_border : 0;


 /*
  * method 1 - when window is smaller than total plot size
  */
 XtVaGetValues(_border, XmNwidth, &_bord_width, XmNheight, &_bord_height, 
                        NULL);

 if (_bord_width > horizontal_factor)
          _clip_width=  _bord_width - horizontal_factor;
 else
          _clip_width= 1;

 if (_bord_height > vertical_factor)
          _clip_height= _bord_height - vertical_factor;
 else
          _clip_height= 1;



 /*
  * method 2 - when window is bigger than total plot size
  */
 if (_work_area) {
     XtVaGetValues(_work_area, XmNwidth, &ww, XmNheight, &wh, NULL);
     if (ww > horizontal_factor)
              cw= ww - _left_border - _right_border;
     else     cw= 1;

     if (wh > vertical_factor)
              ch= wh - _top_border - _bottom_border;
     else     ch= 1;

     if ((_clip_width > cw)&&(cw>1)) {
              _clip_width= cw;
              using_method_2_for_width= True;
     }
     if ((_clip_height > ch)&&(ch>1)) {
              _clip_height= ch;
              using_method_2_for_height= True;
     }
 }

 Position x= _left_annotation_on ? _left_border : 0;
 Position y= _top_annotation_on ?  _top_border  : 0;
 

 XtConfigureWidget( _clip, x, y, _clip_width, _clip_height, 0);

 setScrollSize();
 clearUnused();

//--------------
 horizontalSB(_h_vis);
 verticalSB(_v_vis);

 callVisibleAreaChange(_curr_x, _curr_y, _clip_width, _clip_height);

}


void SLScrollWin::expose(Widget, XtPointer, XEvent *event)
{
  int x2, y2;
  XExposeEvent *ev= (XExposeEvent *)event;
  Dimension top_adder = _top_annotation_on  ? _top_border  : 0; 
  Dimension left_adder= _left_annotation_on ? _left_border : 0;

  x2= ev->x + ev->width;
  y2= ev->y + ev->height;

  if (ev->y < top_adder) {
         annotateTopBorder();
  }

  if (y2 > (_clip_height+top_adder) ) {
         annotateBottomBorder();
  }

  if (ev->x < left_adder) {
         annotateLeftBorder();
  }

  if (x2 > (_clip_width+left_adder) ) {
         annotateRightBorder();
  }


  
  if (XtWindow(_border)) {
     if (_top_border && _left_border && 
         _left_annotation_on && _top_annotation_on) {
          if ( (ev->x <= _left_border) && (ev->y <= _top_border) )
            drawCorner(0, 0, _left_border, _top_border,_which_corner == NW);
     }
     if (_top_border && _right_border &&
         _right_annotation_on && _top_annotation_on) {
          if ( (x2 >= (_left_border+_clip_width)) && (ev->y <= _top_border) )
            drawCorner(_clip_width+  (_left_annotation_on ? _left_border : 0),
                        0, _right_border, _top_border, _which_corner == NE);
     }
     if (_bottom_border && _left_border &&
         _left_annotation_on && _bottom_annotation_on) {
          if ( (ev->x <= _left_border) && (y2 >= (_clip_height+_top_border)))
            drawCorner(0, _clip_height+ (_top_annotation_on ? _top_border : 0), 
                       _left_border, _bottom_border, _which_corner == SW);
     }
     if (_bottom_border && _left_border &&
         _right_annotation_on && _bottom_annotation_on) {
          if ( (x2 >= (_left_border+_clip_width)) && 
               (y2 >= (_clip_height+_top_border)) )
            drawCorner(_clip_width+  (_left_annotation_on ? _left_border : 0),
                       _clip_height+ (_top_annotation_on  ? _top_border  : 0), 
                       _right_border, _bottom_border, _which_corner == SE);
     }
  }
}

void SLScrollWin::annotateTopBorder()
{
  if (_top_border && _top_annotation_on)  {
       int x= _left_annotation_on ? _left_border : 0;
       annotate( _border, x, 0, _clip_width, _top_border, 
                        _curr_x, _curr_y, Top);
  }
}

void SLScrollWin::annotateBottomBorder()
{
  if (_bottom_border && _bottom_annotation_on) { 
       int x= _left_annotation_on ? _left_border :0; 
       int y= _top_annotation_on ? _clip_height+_top_border :_clip_height;
       annotate( _border, x, y, _clip_width, _bottom_border, 
                 _curr_x, _curr_y, Bottom);
  }
}

void SLScrollWin::annotateLeftBorder()
{
  if (_left_border && _left_annotation_on) {
       int y= _top_annotation_on ? _top_border : 0;
       annotate( _border, 0, y, _left_border, _clip_height, 
                 _curr_x, _curr_y, Left);
  }
}

void SLScrollWin::annotateRightBorder()
{
  if (_right_border && _right_annotation_on) {
       int x= _left_annotation_on ? _clip_width+_left_border :_clip_width; 
       int y= _top_annotation_on  ? _top_border : 0;
       annotate( _border, x, y, _right_border, _clip_height, 
                 _curr_x, _curr_y, Right);
  }

}


void SLScrollWin::drawCorner(int          x, 
                             int          y,
                             unsigned int width,
                             unsigned int height,
                             Boolean      draw_anno)
 
{
   Display *dpy= XtDisplay(_hsb->W());
   Screen  *scr= XtScreen(_hsb->W());
   Window bwin= XtWindow(_border);
  
   XSetForeground(dpy, _gc, _fill_color);
   XFillRectangle(dpy, bwin, _gc, x, y, width, height);
   if (draw_anno && _corner_anno_str && _font) {
        XSetForeground (dpy, _gc, PaintsetCollection::black(scr));
        XSetFont(dpy, _gc, _font->fid); 
        XDrawString( dpy, bwin, _gc, x+5, height/3, 
                     _corner_anno_str, strlen(_corner_anno_str) );
   }


}




void SLScrollWin::redrawCorners()
{
   Window bwin= XtWindow(_border);
   if (bwin) {
      Display *dpy= XtDisplay(_hsb->W());
      if (_left_annotation_on && _top_annotation_on)
          drawCorner(0, 0, _left_border, _top_border, _which_corner == NW);
      if (_right_annotation_on && _top_annotation_on)
          drawCorner(_clip_width+  (_left_annotation_on ? _left_border : 0) ,
                     0, _right_border, _top_border,
                     _which_corner == NE);
      if (_left_annotation_on && _bottom_annotation_on)
          drawCorner(0, _clip_height+ (_top_annotation_on ? _top_border : 0), 
                      _left_border, _bottom_border, _which_corner == SW);
      if (_right_annotation_on && _bottom_annotation_on)
          drawCorner(_clip_width+  (_left_annotation_on ? _left_border : 0),
                     _clip_height+ (_top_annotation_on  ? _top_border  : 0), 
                     _right_border, _bottom_border, _which_corner == SE);
   }
}





void SLScrollWin::clearUnused()
{
  Window bwin= XtWindow(_border);
  if (bwin) {
     Display *dpy= XtDisplay(_hsb->W());
     /*
      * clear corners
      */
     redrawCorners();
     /*
      * clear any left over on right and bottom
      */
     int work_width= _clip_width;
     work_width+= _left_annotation_on  ? _left_border  : 0;
     work_width+= _right_annotation_on ? _right_border : 0;

     int work_height=_clip_height;
     work_height+=   _top_annotation_on    ? _top_border    : 0;
     work_height+=   _bottom_annotation_on ? _bottom_border : 0;


     int clear_width= _bord_width-work_width;
     int clear_height= _bord_height-work_height;
     clear_width = (clear_width > 0) ? clear_width : 0;
     clear_height = (clear_height > 0) ? clear_height : 0;

     XClearArea(dpy, bwin, work_width, 0, clear_width, _bord_height, False);
     XClearArea(dpy, bwin, 0, work_height, _bord_width, clear_height, False);
  }
}


void SLScrollWin::getVisibleArea(int *x, int *y, int *width, int *height)
{
  *x= _curr_x;
  *y= _curr_y;
  *width= _clip_width;
  *height=_clip_height;
}

Boolean SLScrollWin::notify(SLPrim *obj)
{
  XmScrollBarCallbackStruct *cb;
  Display *dpy= XtDisplay(obj->W());
  XAnyEvent *event; 
  XEvent new_event; 
  XEvent save_event; 
  Bool more;
  int i;

  cb= (XmScrollBarCallbackStruct*)obj->lastCallback();
  if (cb) {
    if (cb->event) {
        event= (XAnyEvent*)cb->event;
        for(i=0, more= True; (more); i++) {
            more= XCheckWindowEvent(dpy, event->window, 
                                    PointerMotionMask, &new_event);
            save_event= new_event;
            if ((!more)&&(i>0)) XPutBackEvent(dpy, &save_event); 
        } //end loop
    } // end if event
  } // end if cb

  notifyScrollBars(obj);
  callVisibleAreaChange(_curr_x, _curr_y, _clip_width, _clip_height);
  return True;
}

void SLScrollWin::notifyScrollBars(SLPrim *obj)
{
  void *x;
  SLScrollWin *q;

  moveSB(obj);
  if (obj->id() == HOR)
     for(q= _hor_slave_list->top(&x); (q); q= _hor_slave_list->next(&x) )
             q->moveSB(obj);
  else
     for(q= _vert_slave_list->top(&x); (q); q= _vert_slave_list->next(&x) )
             q->moveSB(obj);
}

void SLScrollWin::moveSB(SLPrim *obj)
{
  XmScrollBarCallbackStruct *cb;
  Position x, y, newx, newy;
  Window bwin= XtWindow(_border);
  Dimension w_width=0, other_w_width=0, other_w_height=0, w_height=0;
  long new_v_size=0;
  long new_h_size=0;

  cb= (XmScrollBarCallbackStruct*)obj->lastCallback();
  if (cb) {
     if (cb->reason == XmCR_DRAG) {
         if (!_dragging) {
               _dragging= True;
               startingDrag();
         }
     }
     else {
        if (_dragging) {
              _dragging= False;
              endingDrag();
        }
     }
  }
  else {
        if (_dragging) {
              _dragging= False;
              endingDrag();
        }
  }

  if ((bwin)&&(_work_area)) {
     if (obj->id() == HOR) {
        if (_hor_slaved) {
             computePlotSize(new_h_size, new_v_size, w_width, w_height);
             _master_hor_sw->computePlotSize(new_h_size, new_v_size, 
                                             other_w_width, w_height);
             newx= (short)(_master_hor_sw->_hsb->scrollValue() * 
                                    ((float)w_width / (float)other_w_width));
        }
        else
             newx= (short)_hsb->scrollValue();

        XtVaGetValues(_work_area, XmNy, &y, NULL);
        XtMoveWidget(_work_area, -1*newx, y);
        newy= -1*y;
        _curr_x= newx;
        _curr_y= newy;
        annotateTopBorder();
        annotateBottomBorder();
                                          
     }
     else {
        if (_vert_slaved) {
             computePlotSize(new_h_size, new_v_size, w_width, w_height);
             _master_vert_sw->computePlotSize(new_h_size, new_v_size, 
                                              w_width,   other_w_height);
             newy= (short)(_master_vert_sw->_vsb->scrollValue() * 
                                    ((float)w_height / (float)other_w_height));
        }
        else
             newy= (short)_vsb->scrollValue();

        XtVaGetValues(_work_area, XmNx, &x, NULL);
        XtMoveWidget(_work_area, x, -1*newy);
        newx= -1*x;
        _curr_x= newx;
        _curr_y= newy;
        annotateLeftBorder();
        annotateRightBorder();
     }
     //_curr_x= newx;
     //_curr_y= newy;
  } // bwin
}





Widget SLScrollWin::horSB() { return _hsb->W();}
Widget SLScrollWin::vertSB(){ return _vsb->W();}


void SLScrollWin::redrawAnnotation()
{
  notifyScrollBars(_hsb);
  notifyScrollBars(_vsb);
  redrawCorners();
}

void SLScrollWin::callVisibleAreaChange(int          x, 
                                        int          y, 
                                        unsigned int width, 
                                        unsigned int height)
{
  if ( (x != _lv_x) || (y != _lv_y) || 
       (width != _lv_width) || (height != _lv_height) ) {
          _lv_x= x;
          _lv_y= y;
          _lv_width= width;
          _lv_height= height;
          visibleAreaChange(x,y,width,height);
  }
}


void SLScrollWin::annotate( Widget, int, int, unsigned int, unsigned int,
                            long, long, int) {}

void SLScrollWin::visibleAreaChange(int, int, unsigned int, unsigned int)
{}


void SLScrollWin::verticalSB(Boolean showing)
{
     Position  sbx, sby;
     Dimension window_height= oHeight();
     Dimension window_width=  oWidth();
     Dimension curr_hsb_height= _hsb->oHeight();
     Dimension curr_vsb_width=  _vsb->oWidth();
     Dimension new_sb_height;
     
     if ((showing) && (window_height > curr_hsb_height) ) {
        new_sb_height= _h_vis ? window_height - curr_hsb_height 
                                : window_height; 
        sby= ((_h_vis)&&(!_side_bottom)) ? curr_hsb_height : 0;
        if (_side_right) {
             sbx= window_width - curr_vsb_width;
        }
        else {
             sbx= 0;
        }
        XtConfigureWidget(_vsb->W(), sbx, sby, curr_vsb_width,
                          new_sb_height, 0);
        _vsb->manage();

        sbx= _side_right ? 0 : curr_vsb_width;
        if (_h_vis) {
             XtConfigureWidget(_hsb->W(), sbx, _hsb->yPos(), 
                                          window_width-curr_vsb_width, 
                                          _hsb->oHeight(), 0);
        }
        
        Dimension width= window_width-curr_vsb_width-1;
        XtConfigureWidget(_border,  sbx, _border->core.y, 
                          (width>0) ? width : 1,
                          _border->core.height, 0);
     } // end if
     else { // not showing
             window_width= (window_width>0) ? window_width : 1;
             XtConfigureWidget( _border, 0, _border->core.y,
                                window_width, _border->core.height, 0);
             if (_h_vis) {
                     XtConfigureWidget( _hsb->W(), 0, _hsb->yPos(),
                                        window_width, curr_hsb_height, 0);
             }
             _vsb->unmanage();
     }
}

void SLScrollWin::horizontalSB(Boolean showing)
{
  Dimension window_height= oHeight();
  Dimension window_width=  oWidth();
  Dimension curr_hsb_height= _hsb->oHeight();
  Dimension curr_vsb_width=  _vsb->oWidth();
  Dimension new_sb_width;

  if ((showing)&& (window_width > curr_vsb_width)) {
     if (_side_bottom) {
          Dimension tmp_height;
          Position  sby= window_height - curr_hsb_height;
          new_sb_width= _v_vis ? window_width - curr_vsb_width : window_width; 
          XtConfigureWidget(_hsb->W(),1, sby,new_sb_width, curr_hsb_height, 0);
          _hsb->manage();

          if (_v_vis) {
               tmp_height= window_width-curr_hsb_height-1;
               XtResizeWidget(_vsb->W(), curr_vsb_width, 
                                         (tmp_height > 0) ? tmp_height : 1, 0);
          }
          tmp_height= window_height-curr_hsb_height-1;
          XtResizeWidget(_border,   _border->core.width, 
                                (tmp_height > 0) ? tmp_height : 1, 0);
     }
     else {//which side (left side)
     }
  }
  else { // not showing
          window_height= (window_height>0) ? window_height : 1;
          XtResizeWidget(_border, _border->core.width, window_height, 0);
          if (_v_vis) {
               XtResizeWidget(_vsb->W(), curr_vsb_width, window_height, 0);
          }
          _hsb->unmanage();
  }
}

void SLScrollWin::setHorPlacement(const int ) { }

void SLScrollWin::setVertPlacement(const int side)
{
   switch (side) {
       case Left:    _side_right= False; break;
       case Right:   _side_right= True;  break;
       default:     
         printf("SLScrollWin::setVertPlacement: value %d is invalid\n",side);
         break;
   } // end switch
   verticalSB(_v_vis);
}

void SLScrollWin::slaveHorSBTo(SLScrollWin *obj)
{
 if (obj) {
    obj->_hor_slave_list->add(this);
    _h_vis= False;
    _hor_slaved= True;
    _master_hor_sw= obj;
    horizontalSB(False);
 } // end if
 else
    freeHorSB();
}

void SLScrollWin::freeHorSB()
{
 if (_master_hor_sw) {
    _master_hor_sw->_hor_slave_list->remove(this);
    _h_vis= True;
    _hor_slaved= False;
    _master_hor_sw= NULL;
    resize();
 } // end if
}


void SLScrollWin::slaveVertSBTo(SLScrollWin *obj)
{
 if (obj) {
    obj->_vert_slave_list->add(this);
    _v_vis= False;
    _vert_slaved= True;
    _master_vert_sw= obj;
    setScrollSize();
    verticalSB(False);
 } // end if
 else 
    freeVertSB();
}

void SLScrollWin::freeVertSB()
{
 if (_master_vert_sw) {
    _master_vert_sw->_vert_slave_list->remove(this);
    _v_vis= True;
    _vert_slaved= False;
    _master_vert_sw= NULL;
    resize();
 } // end if
}


void SLScrollWin::alwaysShowVerticalSB(const Boolean s)   
{
 _always_show_vsb= s;
 setScrollSize();
}

void SLScrollWin::alwaysShowHorizontalSB(const Boolean s)
{
 _always_show_hsb= s;
 setScrollSize();
}

void SLScrollWin::backingStore(Boolean doit)
{
  if (_border) {
    Window win= XtWindow(_border);
    if (win) {
        XSetWindowAttributes wattr;
        Screen *scr= XtScreen(_border);
        unsigned long flags= 0;
        if (doit) {
             if (DoesBackingStore(scr)) {
                  wattr.backing_store= WhenMapped;
                  flags|= CWBackingStore;
             }
             if (DoesSaveUnders(scr)) {
                  wattr.save_under=    True;
                  flags|= CWSaveUnder;
             }
        }
        else {
             wattr.backing_store= NotUseful;
             wattr.save_under=    False;
             flags= CWBackingStore|CWSaveUnder;
      
        }
        XChangeWindowAttributes( XtDisplay(_border), win, flags, &wattr );
    }
    else {
      printf("SLScrollWin::backingStore cannot be changed to after realize.\n");
    }
  }
}

void SLScrollWin::centerOnX(Position x) 
{
   Dimension work_width;
   Position y;
   int target_x;

   XtVaGetValues(_work_area , XmNwidth, &work_width, NULL);
   work_width-= (_right_border + _left_border);

   target_x=  x - (_lv_width/2);
   if (target_x < _left_border) target_x= _left_border;
   else if (target_x > (work_width+_left_border - _lv_width) )
           target_x= (work_width+_left_border - _lv_width);

   XtVaGetValues(_work_area, XmNy, &y, NULL);
   XtMoveWidget(_work_area, -1 * (Position)target_x, y);
   _hsb->setScrollValue(target_x);

   //the following line added by MLS 11/98 so that when annotateBorder
   //is called it will have the correct current x value
   _curr_x = target_x;

   annotateTopBorder();
   annotateBottomBorder();
}

Boolean SLScrollWin::leftAnnotationOn()   { return _left_annotation_on; }
Boolean SLScrollWin::rightAnnotationOn()  { return _right_annotation_on; }
Boolean SLScrollWin::topAnnotationOn()    { return _top_annotation_on; }
Boolean SLScrollWin::bottomAnnotationOn() { return _bottom_annotation_on; }

void SLScrollWin::startingDrag() { }

void SLScrollWin::endingDrag() { }


