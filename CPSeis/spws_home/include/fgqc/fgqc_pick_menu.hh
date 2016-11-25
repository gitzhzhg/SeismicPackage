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
#ifndef FGQCPICKING_MENU_HH
#define FGQCPICKING_MENU_HH

#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"
#include "fgqc/fgqc_plot.hh"
#include "fgqc/fgqc_plot_type.hh"
#include "plot/pick_base.hh"
#include "fgmap/fg_seis_plot.hh"
#include "sl/sl_form_pop.hh"
#include "geom/grid_lines.hh"

enum {MOVE_GRID, CENTER_GRID, CHANGE_ANGLE, REFINE_ANGLE, DEFINE_ORIGIN,
      ORIGINAL_TRANSFORM, NEW_TRANSFORM, SELECT_CMP, SHOW_ORIGINAL, 
      SHOW_NEW, CMP_PLOT};
//=============================================================================
//========= Next two are base classes for the pick menu and picker ============
//=============================================================================
class FgQcPickerMenu : public SLFPopSep {
  private:

  protected:
    FgQcPlot          *_fgqc_plot;
    FgQcPlotType      *_plot_class;
    class FgQcPicker  *_fgqc_picker;
    Boolean           _first_time;

  public:
    FgQcPickerMenu(  Widget            p,
                     char              *name,
                     HelpCtx           hctx,
                     FgQcPlot          *fgqc_plot,
                     FgQcPlotType      *plot_class,
                     int               plot_type) : 
                       SLFPopSep(p,name,FP_DOREMOVE,hctx,True,False),
                       _fgqc_plot(fgqc_plot), _plot_class(plot_class),
                       _plot_type(plot_type){};
    FgQcPlot *fgqcPlot(){return _fgqc_plot;}
    FgQcPlotType *fgqcPlotClass(){return _plot_class;}
    long     _mode;
    int      _plot_type;
    virtual Boolean createVectors(){return(False);}
    virtual void createPicker(int /*can_show*/){};
    virtual void removePicker(){};
};


class FgQcPicker : public PickBase {

  private:

  protected:
    FgSeisPlot      *_sp;
    FgQcPickerMenu  *_menu;
    float           _start_x;
    float           _start_y;
    float           _end_x;
    float           _end_y;

  public:
    FgQcPicker(FgSeisPlot         *sp, 
               FgQcPickerMenu     *menu,
               char               *mode,
               const char * const helpToken,
               const char * const helpFallback) :
                 PickBase(sp, mode, helpToken, helpFallback,
                          XC_tcross, allow, True), _sp(sp), _menu(menu){};
    virtual Boolean createVectors(){return(False);}
};




//=============================================================================
//========= Put the derivations of the two above after this  ==================
//=============================================================================

class FgQcGridDefinitionPickerMenu : public FgQcPickerMenu {
  private:
  
    SLRadioBox   *_radiobox;
    SLTogBox     *_showbox;
    SLPushBox    *_buttons;

  protected: 
    static void GridSelectAction(void *data, long which );
    static void button_control  (void *data, long which );
    static void ShowBoxAction   (void *data, long which );

  public:
     FgQcGridDefinitionPickerMenu( Widget            p,
                                   char              *name,
                                   HelpCtx           hctx,
                                   FgQcPlot          *fgqc_plot,
                                   FgQcPlotType      *plot_class,
                                   int               plot_type);
     virtual ~FgQcGridDefinitionPickerMenu();
     virtual Widget make(Widget p);
     virtual void manage();
     virtual void removeButton();
     Boolean createVectors();
     void createPicker(int can_show);
     void removePicker();
};

class FgQcGridDefinitionPicker : public FgQcPicker {

  private:
    GridLines                *_grid_lines;
    float                    *_xvect;
    float                    *_yvect;
    class SeisVectLinkedList *_vect_ll;
    class VectData           *_vect_data;
    class Vector             *_Vector;
    float                    *_angle_xvect;
    float                    *_angle_yvect;
    class VectData           *_angle_vect_data;
    class Vector             *_angle_Vector;
    long                     _previous_points;
    float                    _new_xmin;
    float                    _new_xmax;
    float                    _new_ymin;
    float                    _new_ymax;
    float                    _image_xmin;
    float                    _image_xmax;
    float                    _image_ymin;
    float                    _image_ymax;
    float                    _x_change;
    float                    _y_change;
    Boolean                  _vector_stat;
    Boolean                  _can_show;
    int                      _old_pick_mode;

  protected:
    virtual void buttonAny(int x1, int x2, int y1, int y2, int button,
                           Action action, Modifier modifier);
    virtual void moveGrid(int x1, int x2, int y1, int y2, int button,
                           Action action);
    virtual void centerGrid(int x1, int x2, int y1, int y2, int button,
                           Action action);
    virtual void changeAngle(int x1, int x2, int y1, int y2, int button,
                           Action action);
    virtual void refineAngle(int x1, int x2, int y1, int y2, int button,
                           Action action);
    virtual void defineOrigin(int x1, int x2, int y1, int y2, int button,
                           Action action);
    virtual void selectCmp(int x1, int x2, int y1, int y2, int button,
                           Action action);
    virtual void selectPlot(int x1, int x2, int y1, int y2, int button,
                           Action action);
    
  public: 
    FgQcGridDefinitionPicker(FgSeisPlot *sp,
                             FgQcPickerMenu  *menu,
                             int can_show,
                             char               *mode,
                             const char * const helpToken,
                             const char * const helpFallback); 
    virtual ~FgQcGridDefinitionPicker();
    Boolean createVectors();
    void ShowVectors(Boolean show);
    GridLines *gridLines(){return _grid_lines;}
    void saveTransform();
    void originalTransform();
};                 
#endif



