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
//==================== GUI for va eta plot        ====================
//==================== Michael L. Sherrill 08/97        ====================
//==========================================================================
#ifndef VA_ETA_GUI_H
#define VA_ETA_GUI_H

#include "wproc.h"
#include "sl/sl_file_choice.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_scale.hh"
#include "sl/slp_file.hh"
#include "sl/sl_quest_pop.hh"
#include "oprim/file_base.hh"

class VaEtaPlot;
class VaCmpPlot;
class SLTextBox;
class SLTogBox;
class SeisPlot;
class SLRadioBox;
class SeisCtype;
class VfFileBase;








class VaEtaGui :  public SLFPopSep
{

public:
  VaEtaGui( Widget              p,
            char                *name,
            HelpCtx             hctx,
            VaEtaPlot           *eta_plot,
            VaCmpPlot           *cmp_plot); 
  ~VaEtaGui();
  virtual void seisPlotChanged(); 
  Widget make(Widget p = NULL);
  void manage();     
  void updateParams(Boolean update_file_limits = True);
  void setPlotParameters();
  void setFirstPanelToPlot(long first_panel);
  long getFirstMoviePanel();
  long getSkipMoviePanels();
  void commonReplot();
  float windowLength() {return _window_length;}
  float startingNmoVelocity();
  float endingNmoVelocity();
  float nmoVelocityIncrement(){return _nmo_velocity_increment;}
  float startingHorzVelocity();
  float endingHorzVelocity();
  float horzVelocityIncrement(){return _horz_velocity_increment;}
  float getPlotWidth(){return _pwidth;}
  float getPlotHeight(){return _pheight;}
  void  replaceData(Boolean b){_replace_data = b;}
  void  possibleFileChange();
  //Temporary
  void updateFile();

protected:
  Boolean _ok_to_retry;
  void    DoAction();
  void    applyButton();
  void    okButton();
  void    cancelButton();
  Boolean ValidInput();
  void    UndoInput();
  void    setToFileDefaults();
  void    reloadSystemDefaults(Boolean do_method);
  virtual Boolean notifyComplex(SLDelay*, int ident);

private:
  //These are temporary until vel files support eta
  VfFileBase         *_vel_infilebase;
  VfFileBase         *_vel_outfilebase;
  SLFileChoice       *_vel_infile_choice;
  SLFileChoice       *_vel_outfile_choice;

  VaEtaPlot          *_eta_plot;
  VaCmpPlot          *_cmp_plot;
  SeisPlot           *_eta_sp;
  SeisPlot           *_cmp_sp;
  SLTextBox          *_eta_params_box;
  SLTogBox           *_grade_box;
  Widget             _plotlab;
  Boolean            _first_time;
  float              _window_length;
  float              _starting_nmo_velocity;
  float              _ending_nmo_velocity;
  float              _nmo_velocity_increment;
  float              _starting_horz_velocity;
  float              _ending_horz_velocity;
  float              _horz_velocity_increment;
  float              _nmo_velocity_width;
  float              _horz_velocity_width;
  int                _connum;
  float              _pwidth;
  float              _pheight;
  long               _grade_vert;
  long               _grade_horz;
  long               _plot_con;
  void               setVelocityRanges();
  Boolean            _have_input_file;
  Boolean            _have_output_file;
  Boolean            _replace_data;
  char               _previous_input[512];
};



//===========================================================================
//===================== Class to ask about replacing ETA data ===============
//===========================================================================
class MySLQuestPop : public SLQuestPop
{
public:

  MySLQuestPop (SLDelay *slparent, char *name, const char *question,
                VaEtaGui *gui);

protected:
  void answerYes();
  void answerNo();

private:
  VaEtaGui *_gui;

};
#endif
