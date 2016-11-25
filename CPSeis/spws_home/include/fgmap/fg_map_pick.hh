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
#ifndef FGMAPPICK_HH
#define FGMAPPICK_HH

#include "plot/pick_base.hh"

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
class FgPickAction;

class FgMapPick : public PickBase {
 
  protected:
     int                  _mode;
     FgXpPlotLinkedList   *_plot_list;
     FieldGeometry        *_fg;
     Boolean              _drag_in_progress;
     Vector               *_working_vect;
     SeisVectLinkedList   *_vlist;
     float                 _xsqr[2];
     float                 _ysqr[2];
     FgLocOut             *_fg_loc_out;
     FgMapToFlag          *_xlat;
     FgPickAction         *_action;
     Boolean               _is_grid;
     Boolean               _only_selected;
     
     

     virtual void noModButtonOnePress(int x, int y);
     virtual void noModButtonOneMotion(int x1, int x2, int y1, int y2);
     virtual void noModButtonOneRelease(int x1, int x2, int y1, int y2);

     virtual void shiftButtonOnePress(int x, int y);
     virtual void shiftButtonOneMotion(int x1, int x2, int y1, int y2);
     virtual void shiftButtonOneRelease(int x1, int x2, int y1, int y2);

     virtual void cntlButtonOnePress(int x, int y);
     virtual void cntlButtonOneMotion(int x1, int x2, int y1, int y2);
     virtual void cntlButtonOneRelease(int x1, int x2, int y1, int y2);

     virtual void noModButtonTwoPress(int x, int y);
     virtual void noModButtonTwoMotion(int x1, int x2, int y1, int y2);
     virtual void noModButtonTwoRelease  (int x1, int x2, int y1, int y2);

     virtual void shiftButtonTwoPress(int x, int y);
     virtual void shiftButtonTwoMotion(int x1, int x2, int y1, int y2);
     virtual void shiftButtonTwoRelease  (int x1, int x2, int y1, int y2);

     virtual void cntlButtonTwoPress(int x, int y);
     virtual void cntlButtonTwoMotion(int x1, int x2, int y1, int y2);
     virtual void cntlButtonTwoRelease  (int x1, int x2, int y1, int y2);

     

     void startDistanceSelection(int x, int y);
     void dragDistanceSelection(int x, int y);
     void endDistanceSelection(int x, int y);
     void changeSelectionOfFlages();
     Vector* getCurrentIndex(int   x,
                             int   y, 
                             long *line_index, 
                             long *flag_index,
                             long *vect_index   =NULL,
                             long *source_index =NULL);
     void changeHelp();
     void findFlag(int x, int y);
     void showFlags(int show_type, int x, int y);
     void selectActiveLine(int x, int y);
     void selectActiveFlag(int x, int y);
     void selectLine(int x, int y, Boolean select);
     void setClosestLineActive(int x, int y);

     void init(SeisPlot *sp);

  public:
     FgMapPick( SeisPlot             *sp, 
                FieldGeometry        *fg, 
                FgXpPlotLinkedList   *plot_list,
                int                   mode,
                FgLocOut             *fg_loc_out =NULL);
     FgMapPick( SeisPlot           *sp, 
                FieldGeometry      *fg, 
                FgMapToFlag        *xlat,
                Boolean            _is_grid);
     virtual ~FgMapPick();
     void    setMode(int mode);
     void setPlotIsGrid(Boolean g);
     void setPlotIsSelected(Boolean g);
     enum { FlagInfo, ShowRec, ShowShot, SelectFlags, Distance,
            ClosestLine, Interpolate};
};


#endif
