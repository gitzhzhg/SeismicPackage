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
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>



ViewWin::ViewWin( WinCtl      *winctl,
                  WidgetClass wclass,
                  char        *name,
                  HelpCtx     hctx) : 

    ViewObj(hctx), _h_order(0), _v_order(0), _showing(False), _winctl(winctl),
    _reduction_percent(0)
{
    Widget w;
 
    _balance_by_plot_widths = False;
    _will_be_showing = False;

    for(int i= 0;(i<4); _bar[i++]= NULL );

    w= XtVaCreateWidget( name, wclass, _winctl->W(), NULL);
    XtUnmanageChild(w);
    XtVaSetValues( w, XmNwidth,  1, 
                      XmNheight, 1, NULL );

    setTopWidget(w);
    _winctl->addWin(this);
}


ViewWin::ViewWin( WinCtl      *winctl,
                  Widget      w,
                  HelpCtx     hctx) : 

    ViewObj(hctx), _h_order(0), _v_order(0), _showing(False), _winctl(winctl),
    _reduction_percent(0)
{

    _balance_by_plot_widths = False;
    _will_be_showing = False;
    XtUnmanageChild(w);
    for(int i= 0;(i<4); _bar[i++]= NULL );
    setTopWidget(w);
    XtVaSetValues( w, XmNwidth,  1, 
                      XmNheight, 1, NULL );
    destroyWidget(False);
    _winctl->addWin(this);
}




ViewWin::~ViewWin()
{

 hide();
 _winctl->delWin(this);
}

void ViewWin::setBars(  ViewBar *t, ViewBar *b, ViewBar *l, ViewBar *r)
{
 _bar[OnLeft]  = l;
 _bar[OnRight] = r;
 _bar[OnTop]   = t;
 _bar[OnBottom]= b;
}


void ViewWin::setViewBar(ViewBar *b, long dir)
{ 
  _bar[dir]= b;
}

ViewBar *ViewWin::getViewBar(long dir)
{ 
  return _bar[dir];
}


void ViewWin::setOrder( long h_order, long v_order)
{
  if (h_order != DontChange) _h_order= h_order;
  if (v_order != DontChange) _v_order= v_order;
}

void ViewWin::setReductionPercent(int percent)
{
 _reduction_percent= percent;
}

void ViewWin::hide()
{
   if (!_showing) return;
   if (_bar[OnRight])  _bar[OnRight]->removeWindow( OnLeft, this);
   if (_bar[OnLeft])   _bar[OnLeft]->removeWindow( OnRight, this);
   if (_bar[OnTop])    _bar[OnTop]->removeWindow( OnBottom, this);
   if (_bar[OnBottom]) _bar[OnBottom]->removeWindow( OnTop, this);

   for(int i= 0;(i<4); _bar[i++]= NULL );
   unmanage();
   _showing= False;
}

void ViewWin::replaceBar(  ViewBar *old, ViewBar *newb)
{

  for (int i=0; (i < DirNum); i++) 
                if (_bar[i] == old) _bar[i]= newb;
}


void ViewWin::sizeNoBar( int dir)
{
  Dimension newwid;
  Dimension newheight;

  switch(dir) {
        case OnLeft : 
                if (_bar[OnRight]) {
                     newwid= _bar[OnRight]->myX()- BARBUFF;
                     setXY( 0, DontChange);
                }
                else {
                     setXY( 0, DontChange);
                     newwid= parentWidth()- myX();
                }
                setDim( newwid, DontChange);
                break;
        case OnTop : 
                if (_bar[OnBottom]) {
                     newheight= _bar[OnBottom]->myY()- BARBUFF;
                     setXY( DontChange, 0);
                }
                else {
                     setXY( DontChange, 0);
                     newheight= parentHeight() - myY();
                }
                setDim( DontChange, newheight);
                break;
        case OnRight : 
                newwid= parentWidth() - myX() ;
                setDim( newwid, DontChange);
                break;
        case OnBottom : 
                newheight= parentHeight() - myY() ;
                setDim( DontChange, newheight);
                break;
  } // End Switch
}

void ViewWin::decisionMaker(  ViewWin **lwin, 
                              ViewWin **rwin, 
                              ViewWin **twin, 
                              ViewWin **bwin )
{

  long lwkno= -1, rwkno= 1000, twkno= -1, bwkno= 1000;
  long l_close_v_order= 1000, r_close_v_order= 1000, 
      t_close_h_order= 1000, b_close_h_order= 1000; 
  long curr_h_order, curr_v_order;

  for( int i=0; (i<MAXWINLIST); i++) {
     if (_winctl->_winlist[i]) {
         curr_h_order= _winctl->_winlist[i]->_h_order;
         curr_v_order= _winctl->_winlist[i]->_v_order;

         /*
          *     find left window
          */
         if (_winctl->_winlist[i]->_showing  ) {
           if ((labs(_v_order-curr_v_order) == labs(_v_order-l_close_v_order)&&
                lwkno    <= curr_h_order                                     && 
                _h_order > curr_h_order ) ||
              (labs(_v_order-curr_v_order) < labs(_v_order-l_close_v_order) &&
               _h_order > curr_h_order) ) {
                          lwkno=  curr_h_order;
                          *lwin=  _winctl->_winlist[i];
                          l_close_v_order=  curr_v_order;
           } // End if
         } // End if

         /*
          *     find right window
          */
         if (_winctl->_winlist[i]->_showing  ) {
           if ((labs(_v_order-curr_v_order) == labs(_v_order-r_close_v_order)&&
                rwkno    >= curr_h_order                                     && 
                _h_order < curr_h_order ) ||
              (labs(_v_order-curr_v_order) < labs(_v_order-r_close_v_order) &&
                _h_order < curr_h_order) ) {
                          rwkno=  _winctl->_winlist[i]->_h_order;
                          *rwin=  _winctl->_winlist[i];
                          r_close_v_order=  curr_v_order;
           } // End if
         } // End if

         /*
          *     find top window
          */
         if ( (twkno    <=curr_v_order )     && 
              (_v_order > curr_v_order ) &&
              ( _winctl->_winlist[i]->_showing)  ) {
                  if ( labs(_h_order - curr_h_order) <= 
                       labs(_h_order - t_close_h_order  )  ) {
                            twkno=  _winctl->_winlist[i]->_v_order;
                            *twin=  _winctl->_winlist[i];
                            t_close_h_order=  curr_h_order;
                  } // End if
         } // End if

         /*
          *     find bottom window
          */
         if ( (bwkno    >=curr_v_order)     && 
              (_v_order < curr_v_order) &&
              ( _winctl->_winlist[i]->_showing)  ) {
                  if ( labs(_v_order - curr_v_order) <= 
                       labs(_v_order - b_close_h_order  )  ) {
                            bwkno=  _winctl->_winlist[i]->_v_order;
                            *bwin=  _winctl->_winlist[i];
                            b_close_h_order=  curr_h_order;
                  } // End if
         } // End if

     } // End if (_winctl->_winlist[i])
  } // End for Loop


  /*
   * make decision base on what sort order we are
   */
  findBetter(lwin, OnLeft);
  findBetter(rwin, OnRight);
  findBetter(twin, OnTop);
  findBetter(bwin, OnBottom);

}

void ViewWin::findBetter(  ViewWin **testwin, int dir)
{

  if (*testwin) {
      if ( (dir == OnLeft) || (dir == OnRight) ) {
           if ( _v_order != (*testwin)->_v_order ) {
                     if (!_winctl->_row_major) *testwin= NULL;
           }
      }
      else {
           if ( _h_order != (*testwin)->_h_order ) {
                     if (_winctl->_row_major) *testwin= NULL;
           }
       }
   } // end if

}



void ViewWin::show()
{
   if (_showing) return;
   ViewWin *lwin= NULL, *rwin= NULL, *twin= NULL, *bwin= NULL;

   decisionMaker(&lwin, &rwin, &twin, &bwin);

   findBar(lwin, rwin, OnLeft,   _winctl);
   findBar(rwin, lwin, OnRight,  _winctl);
   findBar(twin, bwin, OnTop,    _winctl);
   findBar(bwin, twin, OnBottom, _winctl);
  
   manage();
   for(int i=0; (i<DirNum); i++ )
             if (_bar[i]) _bar[i]->adjustBarLength();
   _showing= True;
}



//Set the x location of the right vertical ViewBar and assign a width
//to use for plots.
void ViewWin::setXandWidth(Boolean use_x,     
                           int left_side_x,
                           int right_side_x, 
                           Boolean use_width, int width )
{
ViewBar *workbar;

  
  _balance_by_plot_widths   = use_width;
  _use_requested_x_location = use_x;
  _use_requested_width      = use_width;
  _requested_left_x         = left_side_x;
  _requested_x_location     = left_side_x;
  _requested_right_x        = right_side_x;
  _requested_width          = width;

  workbar = _bar[OnRight];
  if(workbar) 
    {
    workbar->setRequestedXLocation(use_x, (Position)right_side_x);
    workbar->setRequestedWidth(use_width, (Dimension)width);
    }
}



void ViewWin::findBar( ViewWin *win, 
                       ViewWin *oppowin,
                       int     dir, 
                       WinCtl  *)
{
   ViewBar *workbar;
   ViewWin    *winlist[MAXWINLIST];
   int        wcnt, i;
   int        x_coord;

   if (win) {
         workbar= win->getViewBar( opposite(dir) );
         if (workbar) {                                    // go between
              if (oppowin)  {
                if (oppowin->_bar[dir] == workbar) {
                     workbar->replaceWin( oppowin, this, opposite(dir) );
                     oppowin->setViewBar(NULL, dir );
                     setViewBar(workbar, dir );
                     workbar->locate();
                } // End if
                else {            // stacking situation
                     workbar->setWin( this,  opposite(dir) );
                     setViewBar(workbar, dir );
                     workbar->locate();
                } // End if
              } // End if
              else {              // stacking situation
                workbar->setWin( this,  opposite(dir) );
                setViewBar(workbar, dir );
                workbar->locate();
              } // End if
         } // End if
         else {                                           // on end
              if ( (dir ==  OnLeft) || (dir == OnRight) )
                {
                workbar= new ViewBar( _winctl, True,  getHelpCtx());
                if(_balance_by_plot_widths)
                  {
                  if(dir == OnLeft)
                     x_coord = _requested_left_x;
                  else
                     x_coord = _requested_right_x;
                  workbar->setRequestedXLocation(_use_requested_x_location,
                                          (Position)x_coord);
                  workbar->setRequestedWidth(_use_requested_width,
                                          (Dimension)_requested_width);
                  }
                }
              else
                {
                workbar= new ViewBar( _winctl, False, getHelpCtx());
                }
              win->makeNullWinList( winlist, &wcnt, opposite(dir) );
              workbar->replaceWinList( winlist, wcnt, dir );


              workbar->setWin( this,  opposite(dir) );

              for(i=0; (i<wcnt); i++ )
                            winlist[i]->setViewBar(workbar, opposite(dir) );
              setViewBar(workbar, dir );
              //workbar->locate();
              workbar->balance(_balance_by_plot_widths);
         }
   } // End if
   else {
        _bar[dir]= NULL;
        sizeNoBar(dir);
   } // End else

}

void addWinToArray( ViewWin *wlist[],
                    ViewWin  *win)
{
 Boolean found;
 int     i;
 if (win) {
     for( i= 0, found= False; ( (i<MAXWINLIST) && (!found) ); i++) {
            if (wlist[i] == win) {
                  found= True;
            } // End if
            if (wlist[i] == NULL) {
                  wlist[i]= win;
                  found= True;
            } // End if
      } // End loop
 } // End if
}


void ViewWin::makeNullWinList( ViewWin *wlist[],
                               int     *wcnt,
                               int     dir )
{
 int i, j;
 Boolean is_a_canidate= False;
 

 for(i=0,j=0; (i< _winctl->_wincnt); i++, is_a_canidate= False) {

      if ( (dir == OnLeft) || (dir == OnRight) ) {
           if ( this == _winctl->_winlist[i] ) {
                  is_a_canidate= True;
           }
           else {
                  if (_winctl->_row_major) is_a_canidate= True;
           }
      }
      else {
           if ( this == _winctl->_winlist[i] ) {
                  is_a_canidate= True;
           }
           else {
                  if (!_winctl->_row_major) is_a_canidate= True;
           }
      }
                
      if ( ( _winctl->_winlist[i]->_bar[dir]  == NULL) && 
           ( _winctl->_winlist[i]->_showing)           &&
           (is_a_canidate) )
                  wlist[j++]=  _winctl->_winlist[i];
 } // End Loop

 *wcnt= j;

}

Boolean ViewWin::isShowing()
{
  return _showing;
}

Boolean ViewWin::willBeShowing()
{
  return _will_be_showing;
}
