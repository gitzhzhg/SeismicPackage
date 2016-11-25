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
#ifndef SEISWINMAN_HH
#define SEISWINMAN_HH

#include "sl/sl_scroll_win.hh"
#include "sl/wlist.hh"
#include "sp/sp_list.hh"
#include "sp/str_list.hh"
#include <Xm/DrawingA.h>


class SeisPlot;
class SeisPlotTie;
class SeisScrWin;
class SPInfo;
class WinCtl;
class ViewWin;




class SeisWinMan  : public SPList {

  protected:
     Display       *_dpy;
     Widget         _topw;
     Widget         _da;
     SeisScrWin    *_scrwin; 
     Boolean        _persistent_anno;
     SeisPlot      *_current_sp;
     Widget         _xloc;
     Widget         _yloc;
     Widget         _aout;
     WinCtl        *_winctl;
     ViewWin       *_primsw, *_tiesw, *_header_sw;
     Wlist          _time_list;
     StrList        _lock_list;
     Boolean        _lock_sb;
     Boolean        _movies_locked;
     Boolean        _ignore_anno_req;

     static void destroyCallback( Widget, XtPointer, XtPointer );
     static void exposeCallback( Widget, XtPointer,
                                 XmDrawingAreaCallbackStruct* );
     void drawNewPixmap(SeisPlot *prev_sp);


  public:
     SeisWinMan(SeisPlot     *sp,
                const Widget  p,
                const char   *name,
                Boolean       do_scroll);
     SeisWinMan(SeisPlotTie  *sp,
                const Widget  p,
                const char   *name);
     void init(SeisPlot     *sp,
               const Widget  p,
               const char   *name,
               Boolean       do_scroll);
     virtual ~SeisWinMan(); 
     void addSeisPlot(SeisPlot *sp); 
     Boolean delSeisPlot(SeisPlot *sp);
     Boolean setCurrentSP(SeisPlot *sp);
     Boolean setSPNotCurrent(SeisPlot *sp);
     SeisPlot *currentSP();
     void setCornerAnnotation(char *str, 
                              SLScrollWin::WhichCorner corner 
                                                      =SLScrollWin::NW);
     Widget drawingArea();
     Widget W();
     void addOutputWidget(Widget w);
     void delOutputWidget(Widget w);
     Wlist *getTimeList();
     Display *display();
     SeisScrWin  *scrolledWindow();
     void setLocationOutput(SeisPlot *sp, 
                            Widget    xloc, 
                            Widget    yloc, 
                            Widget    aout);
     void removeLocationOutput(SeisPlot *sp, Boolean for_swap =False);
     void    setLockScrollBar(Boolean v);
     Boolean getLockScrollBar();

     void    setMoviesLocked(Boolean);
     Boolean getMoviesLocked();

     WinCtl  *getWinCtl();
     ViewWin *addTieViewWin(Widget p);
     ViewWin *addHeadViewWin(Widget p);
     void     deleteTieViewWin();
     void     deleteHeadViewWin();
     ViewWin *getPrimViewWin();
     ViewWin *getTieViewWin();
     ViewWin *getHeadViewWin();

     /*
      * For locking the current seisplot in window
      */
     void lock(char *reason);
     void unlock();
     Boolean isLocked();
     char *getLockReason();
     Boolean inList(SeisPlot *sp);

     void ignoreAnnotationRequest (Boolean ignore);
     Boolean annotationRequestsIgnored ();

     // add routines to manage the setLocationOutput setting for each sp
};

#endif
