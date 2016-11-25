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
#ifndef FGLOCOUT_H
#define FGLOCOUT_H


#include "sl/sl_row_column.hh"
#include "sp/seis_inform.hh"
#include "geom/fg_constants.hh"
#include "fgmap/fg_map_to_flag.hh"

class SeisPlot;
class FieldGeometry;
class FgXpPlotLinkedList;
class SLPullPop;
class FgMapToFlag;

enum { SP_Z_OUTPUT=  LAST_FIELD_FLAG_VARIABLE +1,
       FG_SKID_TYPE= LAST_FIELD_FLAG_VARIABLE +2,
       APP_CONTROL=  LAST_FIELD_FLAG_VARIABLE +3,
       SHOT_GRP_NO=  LAST_FIELD_FLAG_VARIABLE +4,
       IL_SRC_SKID=  LAST_FIELD_FLAG_VARIABLE +5,
       XL_SRC_SKID=  LAST_FIELD_FLAG_VARIABLE +6 };

class FgLocOut : public SLRowColumn, public SeisInform {

   private:
       FieldGeometry  *_fg;
       Widget       _xloc, _yloc, _line, _flag, _attr1, _attr2;
       int          _attr1_type;
       int          _attr2_type;
       int          _previous_attr1_type;
       int          _previous_attr2_type;
       SLPullPop   *_attr1_pop;
       SLPullPop   *_attr2_pop;
       FgMapToFlag *_xlat;
       Boolean      _trans_type_grid;
       Boolean      _select_lines_only;


       void outputInfo(SeisPlot           *sp, 
                       FgXpPlotLinkedList *plot_list,
                       int                x,
                       int                y );

       virtual Boolean outputByVectors(SeisPlot              *sp, 
                                       FgXpPlotLinkedList    *plot_list,
                                       int                    x,
                                       int                    y,
                                       long                  *line_index,
                                       long                  *flag_index,
                                       long                  *shot_index,
                                       FgMapToFlag::SkidType *skid_type);

       virtual Boolean outputByXlat(SeisPlot              *sp, 
                                    int                    x,
                                    int                    y,
                                    long                  *line_index,
                                    long                  *flag_index,
                                    long                  *shot_index,
                                    FgMapToFlag::SkidType *skid_type);
       virtual Boolean outputByOther(SeisPlot              *sp, 
                                     int                    x,
                                     int                    y,
                                     long                  *line_index,
                                     long                  *flag_index,
                                     long                  *shot_index,
                                     FgMapToFlag::SkidType *skid_type);

       void outputToFields(long                  line_index, 
                           long                  flag_index, 
                           FgMapToFlag::SkidType skid_type,
                           long                  shot_index);

       void showAttribute(int type, 
                          long ixl, 
                          long ixf, 
                          FgMapToFlag::SkidType skid_type,
                          long shot_index,
                          Widget w);
       void blankAttribute(int type, Widget w);
       void addPopButtons(SLPullPop *pop, int curr_val);
       void blankIt();

   public:
       FgLocOut( Widget   p,
                 char     *name,
                 HelpCtx  hctx,
                 FieldGeometry *fg,
                 SeisPlot *sp,
                 FgXpPlotLinkedList *plot_list =NULL);
       ~FgLocOut();
       void addControl(SeisPlot *sp, FgXpPlotLinkedList *plot_list =NULL);
       void modifyControl(SeisPlot *sp, FgXpPlotLinkedList *plot_list);
       void removeControl(SeisPlot *sp);
       virtual void postZoomSeparateWindow(SeisPlot *, SeisPlot *zoomsp);
       virtual void mouseOutputUpdate(SeisPlot*, float x, float y);
       virtual Boolean notifyComplex(SLDelay*, int ident);
       void resetLastAttr1();
       void resetLastAttr2();
       void setAttr1(int attr);
       void setAttr2(int attr);
       int  attr1();
       int  attr2();
       void setAttr1Field(char *str);
       void setAttr2Field(char *str);
       void setTranslator(FgMapToFlag *xlat) { _xlat= xlat;}
       void translateByGrid(Boolean t) {_trans_type_grid= t;}
       void drawGrid(SeisPlot *sp, Boolean by_grid);
       void setSelectedLinesOnly(Boolean s);
};
#endif
