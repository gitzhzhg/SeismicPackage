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
#ifndef SEISLABEL_H
#define SEISLABEL_H

#include "wproc.h"
#include "sp/seis_inform.hh"

class SeisPlot;
class SPList;
class PickLabel;
class SeisVectLinkedList;
class Vector;
class LabelInfoList;
class LabelInput;
class PlotBase;

class SeisLabel : public SeisInform {

 protected:
       static const char   *_no_label_string;
       LabelInfoList       *_lab_list;
       LabelInput          *_label_pop;
       HelpCtx             _hctx;
       Boolean             _just_one;
       Boolean             _show_pop;
       Vector              *_working_vect;
       char                *_insert_color;

 public:
       SeisLabel(SeisPlot *sp, HelpCtx hctx);
       ~SeisLabel();
       void insertMoveOneLabel();
       void doInsertLabel(PickLabel *picker, int x, int y, Boolean ismod);
       void doDeleteLabel(PickLabel *picker, int x, int y);
       void doMoveLabel(PickLabel *picker, int x, int y);
       void placeLabel(PlotBase *plot, Vector *vect, int x, int y);
       void doFinishLabel(PickLabel *picker, int x, int y);
       void doNameLabel(char *name, 
                        Vector *vect, 
                        const char *fontstr=
                            "-*-*-bold-r-normal-*-*-180-*-*-*-*-iso8859-1",
                        const char *color = "red",
                        const Boolean floating_lab= False);
       void setLabelType( Vector *vect, const Boolean floating_lab);
       void deleteAllLabels();
       void insertManyLabels();
       void addPlot(SeisPlot *sp);
       void removePlot(SeisPlot *sp);
       void setInsertColor(char *color);
       const char *noLabelString() { return _no_label_string; }
       virtual void notCurrentInWindow(SeisPlot *);
       virtual void destroyed(SeisPlot *);
};


#endif
