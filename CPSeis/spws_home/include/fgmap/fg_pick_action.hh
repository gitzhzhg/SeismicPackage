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
#ifndef FGPICKACTION_HH
#define FGPICKACTION_HH


class PlotBase;
class SeisPlot;
class FgMap;
class FgXpPlotLinkedList;
class FieldGeometry;
class Vector;
class SeisVectLinkedList;
class FgXpData;
class FgLocOut;
class FgMapToFlag;


class FgPickAction {
  public:
     enum action_type { NO_ACTION, SELECT, UNSELECT, INTERPOLATE_VAL};
  protected:
     action_type           _mode;
     FgXpPlotLinkedList   *_plot_list;
     FieldGeometry        *_fg;
     SeisPlot             *_sp;
     Boolean              _drag_in_progress;
     Vector               *_working_vect;
     SeisVectLinkedList   *_vlist;
     float                 _xsqr[5];
     float                 _ysqr[5];
     Vector               *_curr_line_vect;
     int                   _xfield;
     int                   _yfield;

     void selectOneFlag(int x, int y);
     void operateOnFlagsOnLine(int x1, int y1, int x2, int y2);
     void interpolateFlagValues(long line_index, long flag_index);
     void interpolateOneFlag(int x, int y);
     void interpolateFlagsInArea(int x1, int y1, int x2, int y2);
     void startGroup(int x, int y);
     void startLineSnap(int x, int y);
     Vector* getCurrentIndex( int   x,
                              int   y,
                              long *line_index,
                              long *flag_index,
                              long *vect_index   =NULL,
                              long *source_index =NULL);


  public:
     FgPickAction( SeisPlot           *sp,
                   FieldGeometry      *fg,
                   FgXpPlotLinkedList *plot_list);
     virtual ~FgPickAction();
     void startGroupSelection(int x, int y, Boolean set);
     void startLineSnapSelection(int x, int y, Boolean set);
     void startLineSnapInterpolation(int x, int y, int xfield, int yfield);
     void startGroupInterpolation(int x, int y, int xfield, int yfield);
     void dragGroup(int x, int y);
     void endGroup(int x1, int y1, int x2, int y2);
     void dragLineSnap(int x, int y);
     void endLineSnap(int x1, int y1, int x2, int y2);

};
#endif
