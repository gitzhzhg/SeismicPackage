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
//==================== GUI for va semblance plot        ====================
//==================== Michael L. Sherrill 08/97        ====================
//==========================================================================
#ifndef VA_SEMBLANCE_GUI_H
#define VA_SEMBLANCE_GUI_H

#include "wproc.h"
#include "file_choice.h"
#include "sl/sl_form_pop.hh"
#include "sl/sl_scale.hh"

// $Id: va_semblance_gui.hh,v 1.3 2004/07/21 16:58:09 wjdone Exp $
// $Name:  $


class VaSemblancePlot;
class SLTextBox;
class SLTogBox;
class SeisCbar;
class SeisPlot;
class SLRadioBox;
class SeisCtype;
class VaCommonParamsGui;
class VaCommonMovieGui;
class SeisAnnoPop;
class SLPushBox;

class VaSemblanceGui :  public SLFPopSep
{

public:
  VaSemblanceGui( Widget              p,
                  char                *name,
                  HelpCtx             hctx,
                  VaSemblancePlot     *plot); 
  ~VaSemblanceGui();
  virtual void seisPlotChanged(); 
  Widget make(Widget p = NULL);
  void manage();     
  void updateParams(Boolean update_file_limits = True);
  void setParameters();
  void setFirstPanelToPlot(long first_panel);
  long getFirstMoviePanel();
  long getSkipMoviePanels();
  void commonReplot();
  void annotationChanged(float primary, float secondary, Boolean depth);

  /*! \name Automatic Copy Picks Option
      Option to automatically copy previous active panel semblance picks to
      new panel.
   */
  //@{
  //! Determine if automatic copy option has been selected by user.
  Boolean isAutoCopySelected() const;
  //@}

  /*! \name Snap Mode for Semblance Picking
      Interface for using the semblance picking snap mode.
   */
  //@{
  //! Determine if snap pick option has been selected by user.
  Boolean isSnapPickSelected() const;
  //! Get velocity half window size for snap option.
  float   getSnapVelocity() const;
  //! Get time half window size for snap option.
  float   getSnapTime() const;
  //@}

protected:
  Boolean _ok_to_retry;
  void    DoAction();
  Boolean ValidInput();
  void    UndoInput();
  void    setToFileDefaults();
  void    reloadSystemDefaults(Boolean do_method);
  virtual Boolean notifyComplex(SLDelay*, int ident);
  VaCommonParamsGui *getCommonGui(){return _common_gui_box;}  

private:
  VaSemblancePlot    *_plot;
  SeisPlot           *_sp;
  SeisCbar           *_colorbar;
  SeisCtype          *_color_control;
  SLTextBox          *_params_box;
  SLTextBox          *_snap_box;
  SLTogBox           *_grade_box;
  SLTogBox           *_pick_box;
  SLPushBox          *_anno_but;
  VaCommonParamsGui  *_common_gui_box;
  VaCommonMovieGui   *_common_movie_gui_box;
  SeisAnnoPop        *_anno_pop;
  Widget             _plotlab;
  Widget             _timerangelab;
  Widget             _velrangelab;
  Boolean            _first_time;
  int                _color_num;
  float              _tmin;
  float              _tmax;
  float              _vmin;
  float              _vmax;
  float              _pdmin;
  float              _pdmax;
  int                _connum;
  float              _pwidth;
  float              _is;
  float              _startvel;
  float              _bot_time;
  float              _snap_vel;
  float              _snap_time;
  long               _grade_vert;
  long               _grade_horz;
  long               _plot_con;
  long               _auto_copy;
  long               _snap_pick;
  char               _color_file[300];
  Boolean            _new_color;
  Boolean             _annotation_changed;

  /*! \name Snap Mode Text Fields
        Interface for actions connected with the snap mode velocity and
        time text fields.
     */
    //@{
    //! Receives focus and loss of focus actions from text fields.
          static void handleSnapTextFields(void *instance, long ident);
    //! Action of static version passed to here. Passes values of
    //! fields to picks object via plot object.
          void handleSnapTextFields(long ident);
    //@}
};



#endif
