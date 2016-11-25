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
//********************************************************
//Author Michael L. Sherrill
//Class that creates a sub menu of the main qc pop up menu
//********************************************************

#ifndef FG_QC_POP_H
#define FG_QC_POP_H

#include "sl/sl_text_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_list_picker.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_error_pop.hh"
#include "sl/sl_pull_pop.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_cbar_pop.hh"
#include "oprim/element.hh"
#include "oprim/ll_base.hh"


enum {OFFSETD, AZIMUTHALD};
enum {PWIDTH, PHEIGHT, XMIN, XMAX, YMIN, YMAX};
enum {COORDOP, MAPOP, COLOROP, CBAROP};
enum {NEWINSTANCE, OLDINSTANCE, ENFORCE};
enum {EXTENDED_COLORS};


class FgQcPlot;
class FgQcPopSubMenu;
class StaticsReadPop;
class StaticsFile;

class PlotElement : public Element
{
  friend class PlotList;
  
  protected:
     FgQcPlot *_fp;
     void     *_user_data;
     PlotElement(FgQcPlot *fp) : _fp(fp) {}
     ~PlotElement() {}
     int operator ==(void * const fp) const { return((FgQcPlot*) fp == _fp); }
     virtual void print() const {}
};



class PlotList : public BaseLinkedList
{
  public:
   void add(FgQcPlot *fp, void *user_data =NULL)
               { PlotElement *theElement = new PlotElement(fp);
                 BaseLinkedList::add((Element *) theElement);
                 theElement->_user_data = user_data; 
               }
   void remove(FgQcPlot *fp) { BaseLinkedList::remove((void*)fp); };
   FgQcPlot *top(void **ptr = (void **) 0)
            { 
            PlotElement* q= (PlotElement*)BaseLinkedList::top(ptr);
            return (q ? q->_fp : NULL);
            };
   FgQcPlot *find(FgQcPlot *fp)
            {
            PlotElement* q= (PlotElement*)BaseLinkedList::find((void*)fp);
            return (q ? q->_fp : NULL);
            };
   FgQcPlot *bottom(void **ptr = (void **) 0)
            {
            PlotElement* q= (PlotElement*)BaseLinkedList::bottom(ptr);
            return (q ? q->_fp : NULL);
            };
   FgQcPlot *next(void **ptr = (void **) 0)
            {
            PlotElement* q= (PlotElement*)BaseLinkedList::next(ptr);
            return (q ? q->_fp : NULL);
            };
   FgQcPlot *prev(void **ptr = (void **) 0)
            {
            PlotElement* q= (PlotElement*)BaseLinkedList::prev(ptr);
            return (q ? q->_fp : NULL);
            };
   FgQcPlot *current(void **ptr = (void **) 0)
            {
            PlotElement* q= (PlotElement*)BaseLinkedList::current(ptr);
            return (q ? q->_fp : NULL);
            };
};




class FgQcPop :  public SLFPopSep {

 friend class FgQcComputeGridPop;

 private:
       int                 _numcolors;
       int                 _numgraphics;
       int                 _plot_type;
       int                 _previous_plot_type;
       char                *_previous_selected;
       long                _focus_id;
       float               _focus_value;
       char                *_selected;
       Boolean             _questions_answered;
       Boolean             _ok_button;
       Boolean             _use_centers;
       void setQuestionsAnswered();
       
 protected:
       PlotList                        _plist;
       FgQcPlot                        *_active_plot;
       FgQcPlot                        *_previous_active_plot;
       class OffsetDistributionMenu    *_distribution_menu;
       class NormalizedBinMenu         *_normalized_bin_menu;
       class HeaderWordMenu            *_header_word_menu;
       SeisColorPop                    *_color_pop;
       SeisCbarPop                     *_cbar;
       class FieldGeometry             *_fg;
       class FgSeisPlotList            *_fgsp_list;
       class SLApp                     *_app;
       class SLQuestPop                *_question_pop;
       HelpCtx                         _hctx;
       Boolean                         _first_time;
       Boolean                         _first_plot_made;
       Boolean                         _plot_on_doaction;
       Boolean                         _use_file_defaults;
       Boolean                         _new_appdefaults;
       int                             _coord_system;
       float                           _pwidth;
       float                           _pheight;
       float                           _left;
       float                           _right;
       float                           _top;
       float                           _bottom;
       StaticsReadPop                  *_srp;
       long                            _data_loc;
       int                             _header_word;
       long                            _newinstance;
       SLTextBox                       *_coord_box;
       SLRadioBox                      *_coordradiobox;
       SLPushBox                       *_coordop;
       SLPushBox                       *_buttons;
       SLListPicker                    *_attribute;
       SLRadioBox                      *_newbox;   
       SLTogBox                        *_extendedcolorsbox;
       SLTogBox                        *_enforcebox;
       virtual void addPlot(FgQcPlot *fp);
       virtual void UndoInput();
       virtual void DoAction();
       virtual void okButton();
       virtual void applyButton();
       virtual Boolean ValidInput();
       virtual void checkTimes();
       static void buttonCoordinates( void *data, long which );
       static void button_control( void *data, long which );
       static void CoordFocusAction( void *data, long which );
       static void CoordLosingFocusAction( void *data, long which );
       static void GridSurveyAction( void *data, long which );
       static void InstanceAction( void *data, long which );
       static void ExtendedColorsAction( void *data, long which );
       static void EnforceAction( void *data, long which );
       virtual int makeIndirectPlot(int plot_type, float width, float height,
                                    float xmin, float ymin, float xmax,
                                    float ymax, int coord_system);
       
 public:
       FgQcPop( Widget               p,
                char                 *name,
                HelpCtx              hctx,
                class FieldGeometry  *fg,
                class FgSeisPlotList *fgsp_list);
       FgQcPop( Widget               p,
                char                 *name,
                HelpCtx              hctx,
                class FieldGeometry  *fg,
                class FgSeisPlotList *fgsp_list,
                class SLApp          *app = NULL);
       virtual ~FgQcPop();
       friend  class FgQcPlot;
       friend  class FgQcPopSubMenu;
       friend  class OffsetDistributionMenu;
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method = True);
       virtual void setActivePlot(FgQcPlot *fp, Boolean remove = False, 
                                  Boolean do_destroy = False);
       virtual Boolean notifyComplex(SLDelay*, int ident);
       virtual int getCoordinateSystem() {return _coord_system;}
       virtual Boolean setPlotTypeParameters();
       virtual void setSubMenu();
       Boolean shaderRequested();
       SLErrorPop *_errpop;
       SLPullPop *pulldown();
       Widget PW();
       SLFormPop *P(){return (SLFormPop *)_active_plot;}
       FieldGeometry *fg(){return _fg;}
       StaticsFile *getStaticsFile ();
       void setDataLocation (long data_loc) {_data_loc = data_loc;}
       long getDataLocation () {return _data_loc;}
       void setHeaderWord (int header_word) {_header_word = header_word;}
       long getHeaderWord () {return _header_word;}
       long getOffsetLimitType ();
       long getAzimuthLimitType ();
       float getOffsetMinimum ();
       float getOffsetMaximum ();
       float getAzimuthMinimum ();
       float getAzimuthMaximum ();
       float getUserLeft () {return _left;}
       float getUserRight () {return _right;}
       float getUserTop () {return _top;}
       float getUserBottom () {return _bottom;}
       float getNormalizedBinPercent();
       char *getAttributeName (int plot_type);
       int getVersionNumber (FgQcPlot *plot);
       int numberInPrivateColorMap ();
       void updateCoordinates();
};







#endif
