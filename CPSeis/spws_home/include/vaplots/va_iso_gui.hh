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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//==========================================================================
//==================== GUI for iso velocity plots       ====================
//==================== Michael L. Sherrill 09/97        ====================
//==========================================================================


#ifndef VA_ISO_GUI_H
#define VA_ISO_GUI_H

#include "sl/sl_form_pop.hh"

class SLTextBox;
class SLRadioBox;
class SLTogBox;
class VaIsoPlot;
class SeisCbar;
class SeisPlot;
class SeisCtype;
class VfDataset;
class VaIsoPlotter;
class ContourGui;

class VaIsoGui :  public SLFPopSep
{

public:
  VaIsoGui( Widget              p,
            char                *name,
            HelpCtx             hctx,
            VaIsoPlot           *plot); 
  ~VaIsoGui();
  virtual void seisPlotChanged(); 
  virtual void manage();
  virtual Widget make(Widget p = NULL);
  void  updateParams(Boolean update_file_limits = True);
  void  setParameters();
  void  assignIsoPlotter(VaIsoPlotter *plotter){_iso_plotter = plotter;}
  void  setColorOptions(Boolean turn_on);
  void  setDontPlotYet(Boolean dont_plot_yet);
  int   doMovie()                 {return _do_movie;}
  float firstFrame()              {return _first_panel;}
  long  totalFrames()             {return _total_panels;}       
  float skipFrames()              {return _skip_panels;}
  int   getPlotType()             {return _plot_type;}
  int   getPlottedVelType()       {return _plotted_vel_type;}
  int   getLineType()             {return _line_type;}
  int   gradeVertical()           {return _grad_ver;}
  int   gradeHorizontal()         {return _grad_hor;}
  int   underlay()                {return _underlay;}
  float getTmin()                 {return _tmin;}
  float getTmax()                 {return _tmax;}
  float getY1()                   {return _y1;}
  float getY2()                   {return _y2;}
  float getX1()                   {return _x1;}
  float getX2()                   {return _x2;}
  float getXbin()                 {return _xbin;}
  float getYbin()                 {return _ybin;}
  float getTime()                 {return _time;}
  float getHeight()               {return _pheight;}
  long  getNumberYlines()         {return _number_y_lines;}
  long *getVelRequestList()       {return _vel_request_list;}
  long *getVelYlinesList()        {return _vel_ylines_list;}
  float getTimeOfFrame(long i)    {return _time_of_frame[i];}
  long  getFunctionsInFrame(long i){return _functions_in_frame[i];}
  Boolean changePlot(float x1, float x2, float y1, float y2,
                     Boolean refresh_only = False);       

  enum {PWIDTH, PHEIGHT, X1, X2, Y1, Y2, TMIN, TMAX, VMIN, VMAX,
        X_ANNO, PRIMARY_TIMING, SECONDARY_TIMING, GRADEH, GRADEV,  
        XBIN, YBIN, TIME, DO_MOVIE, UNDERLAY, FIRST_PANEL, SKIP_PANELS, 
        TOTAL_PANELS};

protected:
  virtual void reloadSystemDefaults(Boolean do_method =True);
  virtual void DoAction();
  virtual Boolean ValidInput();
  void setToFileDefaults();
  Boolean reconcileParameters();
  void setMovieParameters(int ident);
  Boolean findFirstMoviePanel();
  VaIsoPlotter *_iso_plotter;    
  Widget     _plotlab;
  Widget     _rangelab;
  Widget     _vellab;
  Widget     _linelab;
  SLTextBox  *_params;
  SLRadioBox *_ptypebox;
  SLTogBox   *_grade;
  SLRadioBox *_linebox;
  SLTextBox  *_binbox;
  SLTogBox   *_movie_tog;
  SLTextBox  *_movie_box;
  ContourGui *_contour_gui;

  float   _pwidth;
  float   _pheight;
  float   _x1;
  float   _x2;
  float   _y1;
  float   _y2;
  float   _tmin;
  float   _tmax;
  float   _vmin;
  float   _vmax;
  float   _ybin;
  float   _xbin;
  float   _time;
  int     _x_annotation_increment;
  float   _primary_timing;
  float   _secondary_timing;
  float   _orig_vmin;
  float   _orig_vmax;
  float   _oldtval;
  float   _olddval;
  float   _oldy;
  float   _oldx;
  float   _oldt;
  float   _user_vmin;
  float   _user_vmax;
  long    _grad_hor;
  long    _grad_ver;
  long    _underlay;
  Boolean _first_time;
  Boolean _was_time;
  Boolean _was_depth;
  Boolean _user_visited;
  Boolean _data_initialized;
  Boolean _dont_plot_yet;
  long    _do_movie;
  float   _first_panel;
  float   _skip_panels;
  long    _total_panels;
  int     _plot_type;
  int     _plotted_vel_type;
  int     _line_type;
  long    _number_y_lines;
  long    *_vel_request_list;
  long    *_vel_ylines_list;
  long    *_functions_in_frame;
  float   *_time_of_frame;

  //do these with notify complex later?
  static  void plotTypeAction( void *data, long which );
  static  void lineAction  ( void *data, long which );
  static  void binFocusAction( void *data, long which );
  static  void binLosingFocusAction  ( void *data, long which );
  static  void paramFocusAction( void *data, long which );
  static  void paramLosingFocusAction  ( void *data, long which );
  Boolean notifyComplex(SLDelay *obj, int ident);

     
private:
  VaIsoPlot          *_plot;
  SeisPlot           *_sp;
  SeisCbar           *_colorbar;
  SeisCtype          *_color_control;
  VfDataset          *_vfd;  
};

#endif
