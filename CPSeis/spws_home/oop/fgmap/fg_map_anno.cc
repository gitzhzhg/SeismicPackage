#include "fgmap/fg_map_anno.hh"
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
#include "geom/field_geometry.hh"
#include "vect/ll_tag.hh"
#include "vect/tag.hh"
#include "fgxp/fgxp_data.hh"
#include "fgxp/fgxp_2d_plot.hh"
#include "fgxp/ll_fgxp_vect.hh"
#include "sp/seis_plot.hh"
#include <X11/Xatom.h>
#include <stdio.h>

typedef enum _SlopeType { NONE, NEGATIVE, POSITIVE, ZERO, UNDEFINED} SlopeType;

struct MarkLine {
    int                  index;
    FgMapAnno::SlopeType slope;
    Tag                  *label_tag;
};


FgMapAnno::FgMapAnno(FieldGeometry *fg, SeisPlot *sp, Boolean is_grid) :
                       SeisInform(sp),
                       _annotation_showing(False), _anno_inc(3),
                       _fg(fg), _sp(sp), _tlist(NULL),
                       _plot_annotated(False),
                       _scrolling(False), _marked_line_ary(NULL),
                       _is_grid(is_grid)
{
}



FgMapAnno::~FgMapAnno()
{
   hide();
   delete [] _marked_line_ary;
}

FgMapAnno::SlopeType FgMapAnno::getSlope(long line_index)
{
  int x1, y1, x2, y2;
  int xval, yval;
  float slope;
  SlopeType retval= UNDEFINED;

  
  long tot_points= _fg->numFlagsOnLine(line_index);
  
  if (_is_grid) {
        x1= _sp->xPixel((float)_fg->getXgrid(line_index,0));
        y1= _sp->yPixel((float)_fg->getYgrid(line_index,0));
        x2= _sp->xPixel((float)_fg->getXgrid(line_index,tot_points-1));
        y2= _sp->yPixel((float)_fg->getYgrid(line_index,tot_points-1));
  } // end if _is_grid
  else {
        x1= _sp->xPixel((float)_fg->getXloc(line_index,0));
        y1= _sp->yPixel((float)_fg->getYloc(line_index,0));
        x2= _sp->xPixel((float)_fg->getXloc(line_index,tot_points-1));
        y2= _sp->yPixel((float)_fg->getYloc(line_index,tot_points-1));
  } // end else

  xval= x2-x1;
  yval= y2-y1;
  if (xval !=0) {
       slope=  (float)yval/(float)xval;
       if (yval==0)        retval= ZERO;
       else if (slope > 0) retval= POSITIVE;
       else                retval= NEGATIVE;
  }
  else {
       retval= UNDEFINED;
  }
  return retval;
}







void FgMapAnno::whichToMark()
{
  int i,j=0;
  _tot_num_lines= (int)_fg->numLines(); 
  
  if (_marked_line_ary) hide();
  _plot_annotated= True;

  if (_tot_num_lines == 1) {
     _tot_marked_lines= 0;

  }
  else if (_tot_num_lines < (_anno_inc*2) ) {  // few lines
     _tot_marked_lines= _tot_num_lines;     
     _marked_line_ary= new MarkLine [_tot_num_lines];
     for(i= 0, j=0; (i<_tot_num_lines); i++) {
         if (_fg->numFlagsOnLine(i) > 0) {
              _marked_line_ary[j].index= i;
              _marked_line_ary[j].slope= getSlope(i);
              _marked_line_ary[j].label_tag= NULL;
              j++;
              //printf("annotating index %d, line %d, slope =%d\n", 
              //          i,  _fg->getLineNumber(i), _marked_line_ary[i].slope);
         }
     } // end loop
     _tot_marked_lines= j;
  } // end else if
  else {  // many lines
     if ((_tot_num_lines % _anno_inc) == 0)
              _tot_marked_lines= (_tot_num_lines / _anno_inc);
     else 
              _tot_marked_lines= (_tot_num_lines / _anno_inc) + 1;
     _marked_line_ary= new MarkLine [_tot_marked_lines];
     for(i= 0, j=0; (i<_tot_num_lines); i++) {
         if (((i % _anno_inc) == 0) && (_fg->numFlagsOnLine(i) > 0)) {
              _marked_line_ary[j].index= i;
              _marked_line_ary[j].slope= getSlope(i);
              _marked_line_ary[j].label_tag= NULL;
              j++;
              //printf("annotating index %d, line %d, slope =%d\n", 
              //        i,  _fg->getLineNumber(i), _marked_line_ary[j].slope);
         }  // end if
     }  // end loop
     _tot_marked_lines= j;
  } // end else many lines
}


Boolean FgMapAnno::inVisibleArea(int x, 
                                 int y,
                                 int x_num_inside, 
                                 int y_num_inside)
{
  int spx, spy, width, height;
  Boolean retval= False;
  if ((x>0) && (y>0))  {
        _sp->getVisibleArea(&spx,&spy,&width,&height);
        if ( (x >= spx+x_num_inside) && (x <= spx+width-x_num_inside) &&
             (y >= spy+y_num_inside) && (y <= spy+height-y_num_inside) )
                  retval= True;
  }
  return retval;
}

Boolean FgMapAnno::getStartingXY(long    line_index,
                                 float  *x, 
                                 float  *y, 
                                 int     x_num_inside, 
                                 int     y_num_inside)
{
   int testx, testy;
   Boolean found= False;
   long tot_points= _fg->numFlagsOnLine(line_index);

   for(int i=0; ((i<tot_points)&&(!found));  i++) {
     if (_is_grid) {
          testx= _sp->xPixel((float)_fg->getXgrid(line_index,i));
          testy= _sp->yPixel((float)_fg->getYgrid(line_index,i));
     }
     else {
          testx= _sp->xPixel((float)_fg->getXloc(line_index,i));
          testy= _sp->yPixel((float)_fg->getYloc(line_index,i));
     }

     if (inVisibleArea(testx, testy, x_num_inside, y_num_inside)) {
          found= True;
          *x= _sp->xWC(testx);
          *y= _sp->yWC(testy);
     }
   }
   return found;
}


void FgMapAnno::annotate()
{
  char linestr[30];
  float x,y;
  int i;

  if (_tot_num_lines!=(int)_fg->numLines()) 
          whichToMark();
  if (!_tlist) {
        _tlist= new TagLinkedList(_sp, DeadCenter,
                    "-*-helvetica-bold-r-*-*-*-180-*-*-*-*-*-*", "black" );
  }

  for(i=0; (i< _tot_marked_lines); i++) {
     if (getStartingXY(_marked_line_ary[i].index, &x, &y, 25, 15)) {
        if (_marked_line_ary[i].label_tag) {
              _marked_line_ary[i].label_tag->setPosition(x,y);
        } // end if
        else {
           VectorLabelPlacement place;
           switch (_marked_line_ary[i].slope) {
               case NEGATIVE:   place= LowerRight;   break;
               case POSITIVE:   place= LowerLeft;    break;
               case ZERO:       place= CenterLeft;   break;
               case UNDEFINED:  place= LowerCenter;  break;
               default:         assert(0);           break;
           } // end switch
           sprintf(linestr, "%1d", 
                   _fg->getLineNumber(_marked_line_ary[i].index) );
           _marked_line_ary[i].label_tag= new Tag(_tlist, linestr, x,y, place);

        } // end else
     }  // end if getStartingXY
     else {
        delete _marked_line_ary[i].label_tag;
        _marked_line_ary[i].label_tag= NULL;
     } // end else
   } // end loop
}





void FgMapAnno::changeInNumLines()
{

}

void FgMapAnno::show()
{
   whichToMark();
   annotate();
   _plot_annotated= True;
}

void FgMapAnno::hide()
{
  void *x;
  Tag *p;
  Tag *nextP = (Tag *) NULL;
  _plot_annotated= False;
  if (_tlist) {
      for( p= _tlist->top(&x); (p); p = nextP ) {
          nextP = _tlist->next(&x);
          delete p;
      }
      delete _tlist;
      _tlist= NULL;
  }
  if (_marked_line_ary) {
          delete _marked_line_ary;
          _marked_line_ary= NULL;
  }
}

void FgMapAnno::visableAreaChange(SeisPlot *, int, int, int, int)
{
  if ((_plot_annotated)&&(!_scrolling)) annotate();
}

void FgMapAnno::postZoom(SeisPlot *, SeisPlot::ZoomDir)
{
  if (_plot_annotated) annotate();
}
void FgMapAnno::startingDragScroll(SeisPlot *)
{
  _scrolling= True;
}

void FgMapAnno::endingDragScroll(SeisPlot *)
{
  _scrolling= False;
  if (_plot_annotated) annotate();
}

void FgMapAnno::setPlotIsGrid(Boolean v)
{
  _is_grid= v;
  if (_plot_annotated) show();
}

void FgMapAnno::setIncrement(int inc)
{
  _anno_inc= inc;
  if (_plot_annotated) {
      hide();
      show();
  }
}

