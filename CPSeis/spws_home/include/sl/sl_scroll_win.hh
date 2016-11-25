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
#ifndef SL_SCROLL_WIN_HH
#define SL_SCROLL_WIN_HH


#include <Xm/BulletinB.h>
#include <Xm/DrawingA.h>
#include "sl/sl_delay.hh"
 
class SLpScroll;
class ScrollList;


class SLScrollWin : public SLDelay {

  public:
     enum {Left, Right, Top, Bottom};
     enum WhichCorner {NW, NE, SW, SE};

  private:
    static void daCB(Widget, XtPointer, XEvent*);
    static void sendEH( Widget w, XtPointer udata, XEvent *event);
    void expose(Widget, XtPointer, XEvent*);

    SLpScroll   *_hsb;
    SLpScroll   *_vsb;
    Boolean  _h_vis;
    Boolean  _v_vis;
    Widget   _clip;
    Widget   _border;
    Widget   _work_area;

    Boolean   _left_annotation_on;
    Boolean   _right_annotation_on;
    Boolean   _top_annotation_on;
    Boolean   _bottom_annotation_on;

    Dimension _right_border;
    Dimension _left_border;
    Dimension _top_border;
    Dimension _bottom_border;
    Dimension _save_right_border;
    Dimension _save_left_border;
    Dimension _save_top_border;
    Dimension _save_bottom_border;
    Boolean  _anno_persistent;
    Boolean  _side_right;
    Boolean  _side_bottom;
    Dimension _clip_width;  // clip widget width
    Dimension _clip_height; // clip widget height
    Dimension _bord_width;  // border widget width
    Dimension _bord_height; // border widget height
    Pixel        _fill_color;
    GC           _gc;
    Boolean      _dragging;
    ScrollList  *_vert_slave_list;    // list of objects we are controlling
    ScrollList  *_hor_slave_list;     // list of objects we are controlling
    Boolean      _vert_slaved;        // we are control by another object
    Boolean      _hor_slaved;         // we are control by another object
    SLScrollWin *_master_hor_sw;     // the obj that controls this one 
    SLScrollWin *_master_vert_sw;    // the obj that controls this one 
    Boolean      _always_show_vsb;    // true is vsb should never be unmanaged
    Boolean      _always_show_hsb;    // true if hsb should never be unmanaged
    Position     _curr_x, _curr_y;    // current x, y of work area
    int          _lv_x, _lv_y;          // last x, y from a visibleAreaChange
    unsigned int _lv_width, _lv_height; // last w, h from a visibleAreaChange
    char        *_corner_anno_str;
    XFontStruct *_font;
    WhichCorner  _which_corner;

  protected:
     enum { HOR, VERT };
     void setScrollSize();
     void setAfter();
     void resize();
     void resizeWorkArea();
     void clearUnused();
     void notifyScrollBars(SLPrim *obj);
     void horizontalSB(Boolean showing);
     void verticalSB(Boolean showing);
     void moveSB(SLPrim *obj);
     void redrawCorners();
     void computePlotSize(long&      new_h_size,   
                          long&      new_v_size,
                          Dimension& w_width, 
                          Dimension& w_height);
     void callVisibleAreaChange(int          x, 
                                int          y, 
                                unsigned int width, 
                                unsigned int height);
     void drawCorner( int          x, 
                      int          y,
                      unsigned int width,
                      unsigned int height,
                      Boolean      draw_anno= False);
     void annotateTopBorder();
     void annotateBottomBorder();
     void annotateLeftBorder();
     void annotateRightBorder();

  public :
     SLScrollWin( const Widget p, const char *name);
     virtual ~SLScrollWin();

     void setWorkArea(Widget w);

     Widget scrollParent() const { return _clip;}
     virtual Widget make(Widget p =NULL);

     virtual Boolean notify(SLPrim *obj);

     virtual void annotate( Widget       anno_da, 
                            int          x, 
                            int          y,
                            unsigned int width,
                            unsigned int height,
                            long         window_x,
                            long         window_y,
                            int          which_side);
     virtual void visibleAreaChange(int          x, 
                                    int          y, 
                                    unsigned int width, 
                                    unsigned int height);
     virtual void startingDrag();
     virtual void endingDrag();

     void setCornerFillColor(Pixel c);
     void setCornerAnnotation(char        *str, 
                              XFontStruct *font, 
                              WhichCorner  corner= NW);
     char *getCornerAnnotation();
     WhichCorner getCornerAnnotationCorner();
     void resetCornerFillColor();
     void clearAnnotation();

     void slaveHorSBTo(SLScrollWin *obj);
     void slaveVertSBTo(SLScrollWin *obj);
     void freeHorSB();
     void freeVertSB();

     void redrawAnnotation();
     void showAnnotation(Boolean);
     Boolean annotationShowing();
     void clipWindowLeft(int b);
     void clipWindowRight(int b);
     void clipWindowTop(int b);
     void clipWindowBottom(int b);
     void setLeftBorder(int b);
     void setRightBorder(int b);
     void setTopBorder(int b);
     void setBottomBorder(int b);

     void setLeftAnnotationOn(Boolean s);
     void setRightAnnotationOn(Boolean s);
     void setTopAnnotationOn(Boolean s);
     void setBottomAnnotationOn(Boolean s);
     Boolean leftAnnotationOn();
     Boolean rightAnnotationOn();
     Boolean topAnnotationOn();
     Boolean bottomAnnotationOn();

     long leftBorder()  const {return _left_border;}
     long rightBorder() const {return _right_border;}
     long topBorder()   const {return _top_border;}
     long bottomBorder()const {return _bottom_border;}
 
     Widget workArea() const {return _work_area;}

     void getVisibleArea(int *x, int *y, int *width, int *height);

     virtual WidgetClass topClass() { return(xmBulletinBoardWidgetClass); };
     virtual Boolean isContainer()  { return True; }

     void setHorPlacement(const int side);
     void setVertPlacement(const int side);
     void alwaysShowVerticalSB(const Boolean);
     void alwaysShowHorizontalSB(const Boolean);
     Widget horSB();
     Widget vertSB();

     void addHSB();
     void remHSB();
     void backingStore(Boolean doit =True);
     void centerOnX(Position x);
};
#endif
