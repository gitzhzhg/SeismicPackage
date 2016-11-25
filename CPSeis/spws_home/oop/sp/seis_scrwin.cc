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
#include "sl/view_win.hh"
#include "sp/seis_scrwin.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_winman.hh"
#include <assert.h>




SeisScrWin::SeisScrWin(const Widget p, const char *name, SeisPlot *sp) :
                      SLScrollWin(p,name), _sp(sp) {}

void SeisScrWin::setCurrentSP(SeisPlot *sp)
{
  assert(sp);
  _sp= sp;
}

void SeisScrWin::annotate( Widget       anno_da,
                              int          x,
                              int          y,
                              unsigned int width,
                              unsigned int height,
                              long         window_x,
                              long         window_y,
                              int          which_side )
{
     Boolean chained_displayed= (Boolean)(_sp->_chained_sp.top() ?
                      _sp->_chained_sp.top()->isPlotDisplayed() : False);
 

     if(!_sp->isPlotDisplayed() && !chained_displayed) return;

     if (which_side==SeisScrWin::Top){
        if(_sp->imageXloc()){ 
            x = (int)((x + _sp->imageXloc()) - window_x);
            if(x < _sp->leftBorder() && leftAnnotationOn())
               x = (int)_sp->leftBorder();
            if(x < 0) x = 0;
       	  }
          _sp->redrawToWidget(anno_da, window_x, 0, width, height, x, y); 
	}
     else if (which_side==SeisScrWin::Bottom)
          _sp->redrawToWidget(anno_da, window_x, 
                               _sp->plottedHeight()-_sp->bottomBorder(), 
                               width, height, x, y); 

     else if (which_side==SeisScrWin::Left)
          _sp->redrawToWidget(anno_da, 0, window_y, width, height, x, y); 


     else if (which_side==SeisScrWin::Right)
          _sp->redrawToWidget(anno_da, _sp->plottedWidth()-_sp->rightBorder(), 
                               window_y, width, height, x, y); 

     else assert(False);
          
}


void SeisScrWin::visibleAreaChange(int          x,
                                   int          y,
                                   unsigned int width,
                                   unsigned int height)
{
  _sp->callVisibleAreaChange(x,y,width,height);
}



void SeisScrWin::startingDrag()
{
  _sp->callStartingDragScroll();
}

void SeisScrWin::endingDrag()
{
  _sp->callEndingDragScroll();
}



//==========================================================================
//==== The following two methods are intended to be used to          =======
//==== set the the scroll bar attachments for adjacent displays and  =======
//==== set a flag that SeisPlots need replotting in the event the    =======
//==== calling program wants to vary the border widths of SeisPlots  =======
//==== when they are rearranged, managed, unmanaged etc.             =======
//==== These methods use pointers to other SeisScrWin classes so     =======
//==== so that only one call using only one SeisScrWin class the     =======
//==== the calling program has is needed to set scroll bars and      =======
//==== borders. SeisScrWin::show must be called on any plots you     ======= 
//==== want showing and the ViewWin/SeisPlot arrays must be ordered  =======
//==== left to right.                                                =======
//==========================================================================
void SeisScrWin::setBordersAndScrollBars( ViewWin  **view_win_array,
                                          SeisPlot **seisplot_array,
                                          long     number_of_elements,
                                          char     *need_to_replot_array,
                                          long     narrow_border,
                                          long     wide_border)
{
long        i,j;
SeisScrWin  *ssw;
ViewWin     *vw;
SeisPlot    *sp;
SeisPlot    *left_sp;
SeisPlot    *right_sp;
SeisPlot    *middle_sp;
long        num_showing = 0;
long        the_only_one_showing;
long        showing_array[100];


  assert(number_of_elements < 100);

  //Initialize internal arrays
  for(i=0; i<number_of_elements; i++)
    {
    need_to_replot_array[i] = False;
    showing_array[i]        = -1;
    }

  //Determine which plots will be showing
  for(i=0, j=0; i<number_of_elements; i++)
    {
    vw = view_win_array[i];
    if(vw->isShowing())
      {
      sp = seisplot_array[i];
      ssw = sp->getSeisWinMan()->scrolledWindow();
      ssw->slaveVertSBTo(NULL);//set all showing to null
      showing_array[j] = i;
      the_only_one_showing = i;
      ++num_showing;
      ++j;
      }
    }
  if(!num_showing)return;


  //Only one plot showing 
  if(num_showing == 1)
    {
    right_sp = seisplot_array[the_only_one_showing];
    ssw = right_sp->getSeisWinMan()->scrolledWindow();
    ssw->slaveVertSBTo(NULL);
    ssw->setLeftBorder((int)wide_border);
    ssw->setRightBorder((int)wide_border);
    if(right_sp->leftBorder()  != wide_border ||
       right_sp->rightBorder() != wide_border     )
      {
      right_sp->setLeftBorder(wide_border);
      right_sp->setRightBorder(wide_border);
      need_to_replot_array[the_only_one_showing] = True;
      }
    return;
    }


  //More than one plot so set the extreme left and right plots
  left_sp = seisplot_array[showing_array[0]];
  ssw = left_sp->getSeisWinMan()->scrolledWindow();
  ssw->setLeftBorder((int)wide_border);
  ssw->setRightBorder((int)narrow_border);
  if(left_sp->leftBorder() != wide_border)
    { 
     need_to_replot_array[showing_array[0]] = True;
     left_sp->setLeftBorder(wide_border);
    }
  if(left_sp->rightBorder() != narrow_border) 
    {
     need_to_replot_array[showing_array[0]] = True;
     left_sp->setRightBorder(narrow_border);
    }
  right_sp = seisplot_array[showing_array[num_showing-1]];
  ssw = right_sp->getSeisWinMan()->scrolledWindow();
  ssw->setLeftBorder((int)narrow_border);
  ssw->setRightBorder((int)wide_border);
  if(right_sp->leftBorder() != narrow_border) 
    {
    need_to_replot_array[showing_array[num_showing-1]] = True;
    right_sp->setLeftBorder(narrow_border);
    }
  if(right_sp->rightBorder() != wide_border) 
    {
    need_to_replot_array[showing_array[num_showing-1]] = True;
    right_sp->setRightBorder(wide_border);
    }
  if(num_showing == 2) //Done
    {
    ssw = left_sp->getSeisWinMan()->scrolledWindow();
    ssw->slaveVertSBTo(right_sp->getSeisWinMan()->scrolledWindow());
    return;
    }

  //Do middle plot borders
  for(i=1; i <= num_showing - 2; i++)
    {
    middle_sp = seisplot_array[showing_array[i]];
    ssw       = middle_sp->getSeisWinMan()->scrolledWindow();
    ssw->setLeftBorder((int)narrow_border);
    ssw->setRightBorder((int)narrow_border);
    if(middle_sp->leftBorder()  != narrow_border || 
       middle_sp->rightBorder() != narrow_border)
         need_to_replot_array[showing_array[i]] = True;
    middle_sp->setLeftBorder(narrow_border);
    middle_sp->setRightBorder(narrow_border);
    }

  //Attach all plots to right plot's scroll bar (right plot owns bar)
  right_sp = seisplot_array[showing_array[num_showing - 1]];
  for(i = 0; i < num_showing - 1; i++)
    {
    left_sp   = seisplot_array[showing_array[i]];
    ssw       = left_sp->getSeisWinMan()->scrolledWindow();
    ssw->slaveVertSBTo(right_sp->getSeisWinMan()->scrolledWindow());
    }

}  

