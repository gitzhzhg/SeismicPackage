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
#ifndef VIEWWIN_H
#define VIEWWIN_H

#include "sl/sl_base.hh"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>




#define MAXWINLIST 10
#define MAXBARLIST 20
#define BARBUFF    2

typedef void(*WinCtlResizeExternalFunction)(void*);

static const int RowMajor=    True;
static const int ColumnMajor= False;

enum{ OnTop=0, OnBottom=1, OnLeft=2, OnRight=3};
enum{ LowDir=0, HighDir=1};
enum{ DirNum= 4, BARSIZE = 6, MASTERBARSIZE = 17, DontChange = 9999 };

class ViewBar;
class ViewWin;
class WinCtl;

class ViewObj : public SLBase {
  protected:
  public:
      ViewObj( HelpCtx hctx =NULL) : SLBase(hctx)
                                       {_use_requested_x_location = False;
                                        _use_requested_width      = False;
                                        _requested_x_location     = 0;
                                        _requested_left_x         = 0;
                                        _requested_right_x        = 0;
                                        _requested_width          = 0;}

      void setDim( const Dimension width, const Dimension height);
      void setXY( Position x, Position y);
      int opposite(int dir );
      Dimension parentWidth();
      Dimension parentHeight();
      Dimension myWidth();
      Dimension myHeight();
      Position myX();
      Position myY();
      Position wX(Widget);
      Position wY(Widget);
      Dimension wWidth(Widget w);
      Dimension wHeight(Widget w);
      Position  _requested_x_location;
      Position  _requested_left_x;
      Position  _requested_right_x;
      Dimension _requested_width;
      Boolean   _use_requested_x_location;
      Boolean   _use_requested_width;
      Position getRequestedXLocation()   {return _requested_x_location;}
      Dimension getRequestedWidth()      {return _requested_width;}
      Boolean useRequestedXLocation()    {return _use_requested_x_location;}
      Boolean useRequestedWidth()        {return _use_requested_width;}
      void setRequestedXLocation(Boolean use_requested_x, Position x)
                       {_use_requested_x_location = use_requested_x;
                        _requested_x_location     = x;}
      void setRequestedWidth(Boolean use_requested_width, Dimension width)
                       {_use_requested_width = use_requested_width;
                        _requested_width     = width;}
};



class ViewWin : public ViewObj {
  protected:
       ViewBar  *_bar[DirNum];
       long     _h_order;
       long     _v_order;
       Boolean  _showing;
       Boolean  _will_be_showing;
       WinCtl   *_winctl;
       int      _reduction_percent;

  public:
      ViewWin( WinCtl      *winctl,
               WidgetClass wclass,
               char        *name,
               HelpCtx     hctx);
      ViewWin( WinCtl      *winctl,
               Widget      w,
               HelpCtx     hctx);
               
      ~ViewWin();
      friend class WinCtl;
      void setBars( ViewBar *t, ViewBar *b, ViewBar *l, ViewBar *r);  // DONE
      void setViewBar(ViewBar *b, long dir);                  // DONE
      ViewBar *getViewBar(long dir);                          // DONE
      void setOrder( long h_order, long v_order);             // DONE
      void replaceBar( ViewBar *old, ViewBar *neww);          // DONE
      void sizeNoBar( int dir);                               // DONE
      void show();                                            // DONE
      void hide();                                            // DONE
      Boolean isShowing();
      Boolean willBeShowing();
      void setWillBeShowing(Boolean s) {_will_be_showing = s;}
      Boolean _balance_by_plot_widths;
      void setBalanceByPlotWidths(Boolean b){_balance_by_plot_widths = b;}
      Boolean balanceByPlotWidths()         {return _balance_by_plot_widths;}
      void setXandWidth(Boolean use_x, 
                        int left_side_x,
                        int right_side_x, 
                        Boolean use_width, int width );
      void findBar( ViewWin *win,
                    ViewWin *oppowin,
                    int dir, 
                    WinCtl  *winctl);                         // DONE
      void decisionMaker(  ViewWin **lwin,
                           ViewWin **rwin,
                           ViewWin **twin,
                           ViewWin **bwin );
      void winEqual(  ViewWin **sidewin, ViewWin **endwin);
      void setReductionPercent(int);
      int  reductionPercent() { return _reduction_percent;}
      void findBetter(  ViewWin **testwin, int dir);
      void makeNullWinList( ViewWin *wlist[],
                            int     *wcnt,
                            int     dir );
       
};



class ViewBar : public ViewObj {
  protected:
       ViewWin   *_winlist[2][MAXWINLIST];
       int        _wincnt[2];
       WinCtl    *_winctl;
       Boolean    _isvert;
       Cursor     _cursor;
       Boolean    _curdef;
       Boolean    _showing;
       Boolean    _never_displayed;
       Boolean    _balance_by_plot_widths;
       /*
        * following for doing bar drags
        */
       Position   _rootx;
       Position   _rooty;
       Position   _lowpos;
       Position   _highpos;
       Boolean    _doing_drag;
       Boolean    _first;
       int        _cx, _cy, _oldx, _oldy, _clen;

       static void moveBarStart( Widget w, XEvent *ev, 
                                 String *params, Cardinal nump);
       static void moveBar( Widget w, XEvent *ev, 
                                 String *params, Cardinal nump);
       static void moveBarEnd( Widget w, XEvent *ev, 
                                 String *params, Cardinal nump);
       void moveStart( Widget w);
       void move( Widget w, XEvent *ev );
       void moveEnd( Widget w);
       void hide();
       void show();
       int  spaceWithFactor(int);

  public:
      ViewBar( WinCtl  *winctl,
               Boolean isvert, 
               HelpCtx hctx  =NULL);
      ~ViewBar();
       friend class WinCtl;

      // routines for comming up
      void locate(Boolean fix_others = True);                  // DONE - HELP
      void balance(Boolean balance_by_plot_widths = False);    // DONE
      Dimension calcBarLength();                               // DONE
      Position findBarPos();                                   // DONE
      void setBarSizes( int dir);                              // DONE
      void adjustBarLength();


      // routines for going away
      void removeWindow( const int dir, ViewWin *vw );   // DONE
      void goingAway(const int dir, ViewWin *vw);       // DONE
      void replaceWinList( ViewWin *new_win_list[],
                           int     newcnt,
                           int     dir );                // DONE


      // manipulate the list of windows
      void removeFromStacking( int dir, ViewWin *vw);    // DONE 
      void addToStacking( int dir, ViewWin *vw);         // DONE

      // routines - general
      Boolean winInList( int dir, ViewWin *vw);               // DONE
      int  winLoc( int dir, ViewWin *vw);                     // DONE
      void replaceWin( ViewWin *old, ViewWin *neww, int dir); // DONE
      void setWin(ViewWin *vw, int dir);                      // DONE
      virtual void manage();                                  // DONE
      int  consolidateDir(const int dir);                     // DONE
};

class WinCtl : public ViewObj {
  protected:
       ViewWin    *_winlist[MAXWINLIST];
       int         _wincnt;
       ViewBar    *_barlist[MAXBARLIST];
       int         _barcnt;
       Cursor      _vcursor;
       Cursor      _hcursor;
       Dimension   _oldwidth;
       Dimension   _oldheight;
       Boolean     _row_major;
       GC          _xorgc;
       static XtTranslations _move_bar;
       Widget      _mhelp_widget;
       Boolean     _bars_showing;
     
       static void resizeEvent(Widget w, XtPointer udata,  XEvent *event);
       static void mapCallback(Widget w, XtPointer udata,   XtPointer CBdata);
       void resize(Widget w,  XtPointer udata,  XConfigureEvent *ev);

       WinCtlResizeExternalFunction _resize_external_function;
       void        *_resize_external_function_data;
       
  public:
       WinCtl(Widget p, char *name);
       ~WinCtl();
       void addWin( ViewWin *win);
       void delWin( ViewWin *win);
       void addBar( ViewBar *bar);
       void delBar( ViewBar *bar);
       void setMajor( const int m) { if (m) _row_major= True;
                                     else   _row_major= False; }
       void addMouseHelpWidget( Widget w) { _mhelp_widget= w; }
       void hideBars();
       void showBars();
       void setResizeExternalFunction(WinCtlResizeExternalFunction func,
                                      void *data)
                                       { _resize_external_function = func; 
                                         _resize_external_function_data = data;}
       friend class ViewWin;
       friend class ViewBar;
};

#endif
