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
//********************************************
//Author Michael L. Sherrill 05/97
//Creates menu to control random line generation
//********************************************

#ifndef CUBE_RANDOM_LINE_POP_H
#define CUBE_RANDOM_LINE_POP_H

#include "sl/sl_form_pop.hh"
#include "sl/sl_databox.hh"
#include "sl/sl_tog_box.hh"
#include "plot/pick_base.hh"
#include "cube/cube_inform.hh"

class CubeRandomLinePicker;
class CubeRandomLineXYTable;
class CubeRandomLinePlot;
class SeisPlot;
class CubeRandomLine;
class CubeDisplay;
class CubeSectionGui;
class CubeAnnotationGui;

class CubeRandomLinePop :  public SLFPopSep, public CubeInform
{

 friend class CubeRandomLinePicker;
 friend class CubeRandomLinePlot;

 private:
     CubeRandomLine           *_cube_random_line;
     CubeDisplay              *_cube_display;
     CubeRandomLinePlot       *_cube_random_line_plot;
     CubeSectionGui           *_cube_plot_parameters;
     CubeAnnotationGui        *_cube_annotation_parameters;
     HelpCtx                  _hctx; 
     Widget                   _cube_id_label;
     Widget                   _creation_widget;
     CubeRandomLineXYTable    *_xy_table;
     SLTogBox                 *_sort_toggle;
     long                     _enforce_sort;
     char                     _cube_id[256];  
     Boolean                  _first_time;
     int                      _cube_number;

 protected:
     void    DoAction();
     Boolean ValidInput();
     void    removeButton();
     void    UndoInput();
     void    setAttachments();       
     CubeSectionGui *getPlotParameters(){return _cube_plot_parameters;}
     CubeAnnotationGui *getAnnotationParameters()
                                        {return _cube_annotation_parameters;}

 public:
     CubeRandomLinePop( Widget              p,
                        char                *name,
                        HelpCtx             hctx,
                        CubeRandomLine      *cube_random_line,
                        CubeDisplay         *cube_display); 
     ~CubeRandomLinePop();
     enum{NUMCOLORS = 0};
     virtual Widget make(Widget p =NULL);
     virtual void manage();     
     virtual void makeAndManage(Widget p =NULL);
     virtual void extraButton(int ident);
     void updateCubeInfo();
     void changeActiveRandomLine(CubeRandomLine *crl);
     SeisPlot *getSeisPlot();
     
     //inform methods
     void newCubeCreated(Cube *);
     void cubeIsCurrent(Cube *cube);
     void newInLinePlot(Cube *,    int slice);
     void newCrossLinePlot(Cube *, int slice);
     void newTimeSlicePlot(Cube *, int slice);
     void destroyed(Cube *);
     void newFilename(Cube *);
    
     //misc cube methods
     char *primaryFilename();
     Cube *getDisplayedCube(); 
     void setPlotParameters(CubeSectionGui *cs){_cube_plot_parameters = cs;}
     void setAnnotationParameters(CubeAnnotationGui *ca)
                                             {_cube_annotation_parameters = ca;}
     //XY table methods
     void resetXYTable(CubeRandomLine *crl);
     void clearXYTable(CubeRandomLine *crl);
     
     //Accelerator action method to read data
     void acceleratorRequest(long which);

     //zoom
     void zoomUpSeparateWin();

     //Sort picks when they are made
     static void sortActionToggle( void *data, long which );
     long enforceSort(){return _enforce_sort;}
};






#endif
