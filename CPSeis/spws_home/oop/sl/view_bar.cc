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
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "sl/view_win.hh"
#include <Xm/Separator.h>
#include <stdio.h>
#include <assert.h>

#define max(a, b) (  (a) > (b) ? (a) : (b)  ) 
#define min(a, b) (  (a) < (b) ? (a) : (b)  )

ViewBar::ViewBar( WinCtl  *winctl,
                  Boolean isvert,
                  HelpCtx hctx)  :

    ViewObj(hctx), _isvert(isvert), _curdef(False), _winctl(winctl),
    _showing(_winctl->_bars_showing), _never_displayed(True), _oldx(0),
    _oldy(0)

{

  Widget w;
  w = XtVaCreateWidget( "sep", xmSeparatorWidgetClass, _winctl->W(), 
               XmNorientation,  isvert ? XmVERTICAL : XmHORIZONTAL, NULL);

  setTopWidget(w);

  if (_winctl->_mhelp_widget )  {
      if (_isvert) ctxhMouseHelp( hctx, _winctl->_mhelp_widget, w, "VBAR" );
      else         ctxhMouseHelp( hctx, _winctl->_mhelp_widget, w, "HBAR" );
  }



  if (isvert) _cursor= winctl->_vcursor;
  else        _cursor= winctl->_hcursor;

  for (int i=0; (i<2); i++) {
      _wincnt[i]= 0;
      for (int j=0; (j<MAXWINLIST); j++) _winlist[i][j]= NULL;
  } // End loop

  _winctl->addBar(this);


  //  add translations to bar
  XtOverrideTranslations(topWidget(), WinCtl::_move_bar);

  XtVaSetValues( topWidget(), XmNuserData, this, NULL);


}

ViewBar::~ViewBar()
{
 Display *dpy= XtDisplay(topWidget());

/*
 if (topWidget()) {
       XtDestroyWidget(topWidget());
       printf("removing bar wid= %x, this = %x\n", topWidget(), this);
       _topw= NULL;
 }
*/

 _winctl->delBar(this);

  XSync( dpy, False);

}



void ViewBar::removeWindow( const int dir, ViewWin *vw)
{
  int cdir= consolidateDir(dir);

  if ( winInList(cdir, vw) ) {
        if (_wincnt[cdir] == 1)
              goingAway(dir,vw);
        else
              removeFromStacking(cdir,vw);
  }
}

/*
 * bar calls this routine if it decides it needs to go away
 */
void ViewBar::goingAway(  const int dir, ViewWin *vw )
{
  ViewBar *workbar;
  ViewWin *oppowin;
  int i;


   workbar= vw->getViewBar(dir);

   int cdir= consolidateDir(dir);
   int oppo_cdir= opposite(cdir);
   
   oppowin= _winlist[opposite(cdir)][0];
   if (workbar) {
        for(i=0; (i < _wincnt[oppo_cdir] ); i++) {
               workbar->replaceWinList( _winlist[oppo_cdir], 
                                        _wincnt[oppo_cdir], oppo_cdir);
               _winlist[oppo_cdir][i]->replaceBar( this, workbar);
               workbar->locate();
        } 
   } // End if
   else {
        for(i=0; (i < _wincnt[oppo_cdir] ); i++) 
               _winlist[oppo_cdir][i]->sizeNoBar(dir);
        setBarSizes( oppo_cdir);
        for(i=0; (i < _wincnt[oppo_cdir] ); i++)
               _winlist[oppo_cdir][i]->replaceBar( this, NULL);
   } // end else

   delete this;
}



void ViewBar::replaceWin( ViewWin *old,
                          ViewWin *neww,
                          int     dir )

{
   int cdir= consolidateDir(dir);
   int i= winLoc(cdir,old);
   _winlist[cdir][i]= neww;
}



void ViewBar::replaceWinList( ViewWin *new_win_list[],
                              int     newcnt,
                              int     dir )

{
   int cdir= consolidateDir(dir);

   _wincnt[cdir]= newcnt;
   for( int i=0; (i < newcnt ); i++)
           _winlist[cdir][i]= new_win_list[i];
}


void ViewBar::setWin( ViewWin *neww,
                      int     dir )

{
  int cdir= consolidateDir(dir);
  if (_wincnt[cdir] == 0) {
         _winlist[cdir][0]= neww;
         _wincnt[cdir]++;
  } // end if
  else
         addToStacking(cdir, neww);
}



Boolean ViewBar::winInList( int dir, ViewWin *vw)
{
  Boolean found= False;
  int i;

  dir= consolidateDir(dir);

  for(i= 0; ( (i<MAXWINLIST) && (!found) ); i++) {
      if (_winlist[dir][i] == vw) found= True;
  } // end loop
  i--;

  return (found);
}


int ViewBar::winLoc( int dir, ViewWin *vw)
{
  Boolean found= False;
  int i;

  dir= consolidateDir(dir);

  for(i= 0; ( (i<MAXWINLIST) && (!found) ); i++) {
      if (_winlist[dir][i] == vw) found= True;
  } // end loop
  i--;
  if (!found) puts("ViewBar::winLoc: not found");

  return (i);
}

/*
 *     compress 4 directions into just two
 */
int ViewBar::consolidateDir( const int dir)
{
 int cdir;

    switch (dir) {
       case OnLeft:   cdir = OnTop;    break;
       case OnRight:  cdir = OnBottom; break;
       case OnTop:    cdir = OnTop;    break;
       case OnBottom: cdir = OnBottom; break;
    } // end switch

 return (cdir);
}


void ViewBar::hide()
{
 _showing= False;
 unmanage();
 locate();
}

void ViewBar::show()
{
 _showing= True;
 manage();
 locate();
}




void ViewBar::locate( Boolean fix_others)
{
  Dimension newwidth;
  Dimension newheight;
  Position rx, newx, newy;
  Position by;
  ViewBar *lbar, *rbar, *tbar, *bbar;
  int dir, i;
  Widget w= NULL;

  lbar= _winlist[0][0]->getViewBar(OnLeft);
  rbar= _winlist[1][0]->getViewBar(OnRight);
  tbar= _winlist[0][0]->getViewBar(OnTop);
  bbar= _winlist[1][0]->getViewBar(OnBottom);

  if ( (_never_displayed) && (_showing) ) {
       _never_displayed= False;
       if (_isvert) {
             if ( XtIsManaged( _winlist[0][0]->W() ) )
                          w=  _winlist[0][0]->W(); 
             else if ( XtIsManaged( _winlist[1][0]->W() ) )
                          w=  _winlist[0][0]->W(); 
             assert(w);
             XtVaSetValues( topWidget(), XmNy,      wY(w), 
                                         XmNwidth,  5, 
                                         XmNheight, calcBarLength(), NULL );
       } // End if
       else {
             if ( XtIsManaged( _winlist[0][0]->W() ) )
                          w=  _winlist[0][0]->W(); 
             else if ( XtIsManaged( _winlist[1][0]->W() ) )
                          w=  _winlist[1][0]->W(); 
             assert(w);
             XtVaSetValues( topWidget(), XmNx,      wX(w),
                                         XmNheight, BARSIZE,
                                         XmNwidth,  calcBarLength(), NULL );
       } // End else
       manage();
  } // End if

  if (_isvert) {
           /*
            * set left window     (we only need to change XmNwidth)
            */
           dir= consolidateDir(OnLeft);
           newwidth= max(myX(), wX(  _winlist[dir][0]->W() )) -
                     min(myX(), wX(  _winlist[dir][0]->W() ));
           if(newwidth <= 0) newwidth = 1;
           for( i= 0; (i < _wincnt[dir]); i++)
               {
                 XtVaSetValues( _winlist[dir][i]->W(), XmNwidth,  newwidth,
                                                       NULL);
               }
           /*
            * set right window    (we need to change XmNx & XmNwidth)
            */
           dir= consolidateDir(OnRight);
           newx=  myX()+ (_showing ? BARSIZE : 0);
           if (rbar) {
                  rx= wX( rbar->W() );
                  if (rx == 0) rx= parentWidth();
           }
           else      rx= parentWidth();
           newwidth= max(rx, newx) - min(rx, newx) ;
           for( i= 0; (i < _wincnt[dir]); i++)
               {
                XtVaSetValues( _winlist[dir][i]->W(),  XmNx,      newx,
                                                       XmNwidth,  newwidth,
                                                       NULL);
               }
           /*
            * as window change we need to change the bars between them
            */
           if (fix_others) {
                  setBarSizes( OnLeft);
                  setBarSizes( OnRight);
           }
  }
  else {
           /*
            * set top windows       (we only need to change XmNheight)
            */
           dir= consolidateDir(OnTop);
           newheight= myY() - wY(  _winlist[dir][0]->W() );
           for( i= 0; (i < _wincnt[dir]); i++)
                 XtVaSetValues( _winlist[dir][i]->W(), XmNheight, newheight, 
                                                       NULL);

           /*
            * set bottom windows    (we need to change XmNy & XmNheight)
            */
           dir= consolidateDir(OnBottom);
           newy=  myY()+ (_showing ? BARSIZE : 0);
           if (bbar) {
                   by= wY( bbar->W() );
                   if (by == 0) by= parentHeight();
           }
           else      by= parentHeight();
           newheight= by - newy ;


           for( i= 0; (i < _wincnt[dir]); i++)
                XtVaSetValues( _winlist[dir][i]->W(),  XmNy,       newy, 
                                                       XmNheight,  newheight,
                                                       NULL);
           /*
            * as window change we need to change the bars between them
            */
           if (fix_others) {
                  setBarSizes( OnTop);
                  setBarSizes( OnBottom);
           }
  }

}


void ViewBar::addToStacking( int dir, ViewWin *vw)
{
  Boolean found= False;
  dir= consolidateDir(dir);


  for(int i= 0; ( (i<MAXWINLIST) && (!found) ); i++) {
      if (_winlist[dir][i] == NULL) {
              _winlist[dir][i]= vw;
              found = True;
      }
  } // end loop
  if (found)
         _wincnt[dir]++;
   else
        printf("ViewBar::addToStacking: cannot add window %x\n", vw);
}
 


void ViewBar::removeFromStacking( int dir, ViewWin *vw)
{
  Boolean found= False;
  dir= consolidateDir(dir);
  int i;


  for(i= 0; ( (i<MAXWINLIST) && (!found) ); i++) {
      if (_winlist[dir][i] == vw) {
              _winlist[dir][i]= NULL;
              found = True;
      }
  } // end loop
  i--;
  if (found) {
       _wincnt[dir]--;
       for(; (i< (MAXWINLIST-1) ); i++) {
               _winlist[dir][i]=  _winlist[dir][i+1];
       }
  } // End if
  else
         printf("ViewBar::removeFromStacking: window not found %x\n", vw);




}


void addBarToArray( ViewBar *blist[],
                    ViewBar  *bar)
{
 Boolean found;
 int i;
 if (bar) {
     for( i= 0, found= False; ( (i<MAXWINLIST) && (!found) ); i++) {
            if (blist[i] == bar) {
                  found= True;
            } // End if
            if (blist[i] == NULL) {
                  blist[i]= bar;
                  found= True;
            } // End if
      } // End loop
 } // End if
}


//Added balance_by_plot_widths to allow the application to set the 
//position of the plots when the application wants varying plot width
//MLS 12/98
void ViewBar::balance(Boolean balance_by_plot_widths)
{
ViewBar *blist[MAXWINLIST], *p, *q; 
int cnt, i;
Dimension win_space; 
int highdir, lowdir;
Position setpos;

  _balance_by_plot_widths = balance_by_plot_widths;

  if (_isvert) {
      lowdir=  OnLeft;
      highdir= OnRight;
  } // end if
  else {
      lowdir=  OnTop;
      highdir= OnBottom;
  } // end else

  for (i= 0; (i<MAXWINLIST); i++) blist[i]= NULL;

  /*
   * Find the leftmost or topmost bar
   */
  for( p= this; (p != NULL); p= p->_winlist[0][0]->getViewBar(lowdir) ) {
        q= p;
  } // end loop

  /*
   * make a list of all the bars (either vertical or horizontal) with a count
   * that are on the same stacking level is the current object.
   */
  for( p= q, cnt= 0; (p != NULL); p= p->_winlist[1][0]->getViewBar(highdir) ) {
        q= p;
        blist[cnt]= p;
        cnt++;
  } // end loop

  /*
   * - Set x(verical) or y(horizontal) of every bar found
   * - this will lay out each bar.
   * - If the application is setting x and width, use that width for balancing
   * - the plots instead of the normal way of using the entire drawing
   * - area widget width. This allows the application to set the balance 
   * - when plot borders vary. (MLS 12/98) 
   */
  if(!_balance_by_plot_widths)
    {
    if (_isvert)
      win_space= _winctl->myWidth()  / (cnt+1);
    else
      win_space= _winctl->myHeight() / (cnt+1);
    setpos= blist[0]->spaceWithFactor(win_space);
    for( i=0; (i<cnt); i++ )
      {
      if (_isvert)
        blist[i]->setXY( setpos, DontChange);
      else
        blist[i]->setXY( DontChange, setpos);
      setpos+= blist[i]->spaceWithFactor(win_space) + (_showing ? BARSIZE : 0);
      blist[i]->locate();
      } // end loop
    }
  else//Application setting variable plot borders
    {
    if (!_isvert)
      {
      win_space = _winctl->myHeight() / (cnt+1);
      setpos= blist[0]->spaceWithFactor(win_space);
      }
    for( i=0; (i<cnt); i++ )
      {
      if (_isvert)
        {
        setpos = blist[i]->getRequestedXLocation();
        blist[i]->setXY( setpos, DontChange);
        }
      else
        {
        blist[i]->setXY( DontChange, setpos);
        setpos+= blist[i]->spaceWithFactor(win_space)+(_showing ? BARSIZE : 0);
        }
      blist[i]->locate();
      } // end loop
    }



 _winctl->_oldwidth=  _winctl->myWidth();
 _winctl->_oldheight= _winctl->myHeight();

}



int ViewBar::spaceWithFactor(int win_space)
{
  /*
   * - go thru top or left windows and find the lowest reduction factor
   * - LowDir means top or left windows
   * - modify win_space to be smaller if necessary
   */
  int factor= 100;
  int win_percent=0;
  for (int i=0; (i<MAXWINLIST); i++) {
        if (_winlist[LowDir][i]) {
           win_percent= _winlist[LowDir][i]->reductionPercent();
           if (win_percent < factor) factor= win_percent;
        }
  }
  if (factor != 0) win_space-= (int)(win_space * (float)(factor* .01));

  return (win_space);
}




void ViewBar::adjustBarLength()
                
{
   if (_isvert) 
         XtVaSetValues( topWidget(), XmNheight,  calcBarLength(), 
                                     XmNy,       findBarPos(),
                                     NULL);
   else
         XtVaSetValues( topWidget(), XmNwidth,  calcBarLength(), 
                                     XmNx,       findBarPos(),
                                     NULL);
}


void setBarWidget( char     *postype,
                   char     *lentype,
                   ViewBar  *blist[] )
{
  int i;
 for( i= 0; (i < MAXWINLIST); i++)
        if (blist[i])
          {
               XtVaSetValues(  blist[i]->W(), 
                               postype,  blist[i]->findBarPos(),
                               lentype,  blist[i]->calcBarLength(), 
                               NULL);
          }
}



void ViewBar::setBarSizes( int dir)
{
  ViewBar *blist[MAXWINLIST], *abar;
  int i, qdir;
  dir= consolidateDir(dir);

  if (_isvert) qdir= OnTop;
  else         qdir= OnLeft;

  for (i= 0; (i<MAXWINLIST); i++) blist[i]= NULL;


  for (i= 0; (i < _wincnt[dir]); i++) {
             abar= _winlist[dir][i]->getViewBar( qdir);
             addBarToArray(blist, abar);
             abar= _winlist[dir][i]->getViewBar( opposite(qdir) );
             addBarToArray(blist, abar);
  }


  if (_isvert) {
     setBarWidget( XmNx, XmNwidth, blist);
  }
  else {
     setBarWidget( XmNy, XmNheight, blist);
  }

}


Dimension ViewBar::calcBarLength()
{

  Dimension total_len[2];
  int i, j;


  for (i= 0; (i< 2); i++) {
      for (j= 0, total_len[i]= 0; (j < _wincnt[i]); j++) {

           if (_isvert) {
                total_len[i]+= wHeight( _winlist[i][j]->W() );
           }
           else  {
                total_len[i]+= wWidth( _winlist[i][j]->W() );
           }

      } // end loop
      total_len[i]+= (_wincnt[i]-1) * (BARSIZE + (2*BARBUFF));
  } // end loop
  

  i = (total_len[0] > total_len[1]) ? 0 : 1;

  //printf( "Bar length for bar: %x = %d\n", this, total_len[i]);

  return (total_len[i]);
}


Position ViewBar::findBarPos()
{
 Position pos= 9999, tmppos;
 int i, j;

 j= (_wincnt[0] > 0) ? 0 : 1;

 if (_isvert) {
     for(i= 0; (i<_wincnt[j]); i++) {
        tmppos= _winlist[j][i]->myY();
        if (tmppos < pos) pos= tmppos;
     } // end loop
 } // end if 
 else {
     for(i= 0; (i<_wincnt[j]); i++) {
        tmppos= _winlist[j][i]->myX();
        if (tmppos < pos) pos= tmppos;
     } // end loop
 } // end else

 return (pos);
}

void ViewBar::manage()
{
  SLBase::manage();

  if (XtWindow(topWidget())) {
       if ( (_cursor) && (!_curdef) ) {
               XDefineCursor( XtDisplay(topWidget()), 
                              XtWindow(topWidget()), _cursor);
               _curdef= True;
       }

  }
}




void ViewBar::moveBarStart( Widget w, XEvent*, String*, Cardinal)
{
  void *udata;
  XtVaGetValues(w, XmNuserData, &udata, NULL);
  ViewBar *obj = (ViewBar *)udata;

  obj->moveStart(w);
}


void ViewBar::moveStart( Widget w)
{
  Display *dpy= XtDisplay(w);
  Screen *scr = XtScreen(w);
  int x, y; 
  Window chd;
  ViewBar *lowbar= NULL, *highbar= NULL;



  XTranslateCoordinates( dpy, XtWindow(topWidget()), 
                          XtWindow(_winctl->W()), 0, 0, &x, &y, &chd );
  _rootx= x;
  _rooty= y;
  
  if (_isvert) {
       if (_wincnt[0] > 0) lowbar= _winlist[0][0]->getViewBar(OnLeft);
       if (_wincnt[1] > 0) highbar= _winlist[1][0]->getViewBar(OnRight);

       if (lowbar) _lowpos= lowbar->myX();
       else        _lowpos= 0;

       if (highbar) _highpos= highbar->myX();
       else         _highpos= _winctl->myWidth();
         
       _oldx= myX();
       _oldy= _cy= myY();
       _clen= myHeight();


  }
  else {
       if (_wincnt[0] > 0) lowbar= _winlist[0][0]->getViewBar(OnTop);
       if (_wincnt[1] > 0) highbar= _winlist[1][0]->getViewBar(OnBottom);

       if (lowbar) _lowpos= lowbar->myY();
       else        _lowpos= 0;

       if (highbar) _highpos= highbar->myY();
       else         _highpos= _winctl->myHeight();

       _oldy= myY();
       _oldx= _cx= myX();
       _clen= myWidth();
  }  

  _first= True;
}




void ViewBar::moveBar( Widget w,
                       XEvent *event,
                       String *,
                       Cardinal)
{
  void *udata;
  XtVaGetValues(w, XmNuserData, &udata, NULL);
  ViewBar *obj = (ViewBar *)udata;

  obj->move(w,event);

}

void ViewBar::move( Widget w,
                    XEvent *event )
{
  Display *dpy= XtDisplay(w);
  Window dw= XtWindow(_winctl->W());
  XMotionEvent *ev= (XMotionEvent *)event;
  int newx, newy;

  if (_isvert) {
    newx= _rootx + ev->x;
    if (newx > _highpos -20) newx= _highpos - 20;
    if (newx < _lowpos  +20) newx= _lowpos  + 20;

    XDrawLine( dpy, dw, _winctl->_xorgc, newx, _cy, newx, _cy+_clen);
    if (!_first) XDrawLine( dpy, dw, _winctl->_xorgc, 
                            _oldx, _cy, _oldx, _cy+_clen);

    _oldx= newx;

  } 
  else {
    newy= _rooty + ev->y;
    if (newy > _highpos -20) newy= _highpos - 20;
    if (newy < _lowpos  +20) newy= _lowpos  + 20;

    XDrawLine( dpy, dw, _winctl->_xorgc, _cx, newy, _cx+_clen, newy);
    if (!_first) XDrawLine( dpy, dw, _winctl->_xorgc, 
                    _cx, _oldy, _cx+_clen, _oldy);

    _oldy= newy;
  }

  _first= False;
}




void ViewBar::moveBarEnd( Widget w,
                          XEvent *,
                          String *,
                          Cardinal)
{
  void *udata;
  XtVaGetValues(w, XmNuserData, &udata, NULL);

  ViewBar *obj = (ViewBar *)udata;
  obj->moveEnd(w);
}



void ViewBar::moveEnd( Widget w)
{
  Display *dpy= XtDisplay(w);
  Window dw= XtWindow(_winctl->W());
  Position newx= _oldx;
  Position newy= _oldy;

  if (_isvert) {
    XDrawLine( dpy, dw, _winctl->_xorgc, _oldx, _cy, _oldx, _cy+_clen);
    if(_balance_by_plot_widths) return;
    XtVaSetValues( topWidget(), XmNx, newx, NULL);
    _requested_x_location = newx;
  } 
  else {
    XDrawLine( dpy, dw, _winctl->_xorgc, _cx, _oldy, _cx+_clen, _oldy);
    if(_balance_by_plot_widths) return;
    XtVaSetValues( topWidget(), XmNy, newy, NULL);
  }

  locate();
}

