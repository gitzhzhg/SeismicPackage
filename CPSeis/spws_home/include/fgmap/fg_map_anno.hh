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
#ifndef FG_MAP_ANNO_HH
#define FG_MAP_ANNO_HH

#include <Xm/Xm.h>
#include "sp/seis_inform.hh"

class SeisPlot;
class TagLinkedList;
class FieldGeometry;
class Vector;
struct MarkLine;
//class PickBase;


class FgMapAnno : public SeisInform{
  private:
      Boolean             _annotation_showing;
      int                 _anno_inc;
      MarkLine           *_marked_line_ary;
      int                 _tot_num_lines;
      int                 _tot_marked_lines;
      Boolean             _is_grid;
  //int                 _num_marked;
      TagLinkedList      *_tlist;
      FieldGeometry      *_fg;
      SeisPlot           *_sp;
      Boolean             _plot_annotated;
      Boolean             _scrolling;

  public:
      typedef enum _SlopeType 
          { NONE, NEGATIVE, POSITIVE, ZERO, UNDEFINED} SlopeType;
  protected:
      void changeInNumLines();
      void whichToMark();
      SlopeType getSlope(long line_index);
      void annotate();
      Boolean inVisibleArea(int x, int y, 
                            int x_num_inside, int y_num_inside);
      Boolean getStartingXY(long line_index, float *x, float *y, 
                            int x_num_inside, int y_num_inside);
      virtual void visableAreaChange(SeisPlot *, int, int, int, int);
      virtual void startingDragScroll(SeisPlot *);
      virtual void endingDragScroll(SeisPlot *);
      virtual void postZoom(SeisPlot *, SeisPlot::ZoomDir);
   
  public:
      FgMapAnno(FieldGeometry *fg, SeisPlot *sp, Boolean is_grid =False);
      virtual ~FgMapAnno();
      void setLineIncrement(int inc);
      void show();
      void hide();
      void setPlotIsGrid(Boolean);
      void setIncrement(int inc);
};
#endif
