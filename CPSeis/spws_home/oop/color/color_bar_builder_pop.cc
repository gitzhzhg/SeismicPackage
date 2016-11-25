// class that creates the color bar builder menu
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
#include "color/color_bar_builder_pop.hh"
#include "color/cbb_levels_gui.hh"
#include "color/cbb_col_sys_gui.hh"
#include "color/cbb_attr_rng_gui.hh"
#include "color/cbb_col_fill_gui.hh"
#include "color/cbb_col_ext_gui.hh"
#include "color/cbb_col_ro_gui.hh"
#include "color/cbb_col_fio_gui.hh"
#include "color/cbb_col_set_gui.hh"
#include "color/cbb_rgb_set.hh"
#include "color/cbb_cps_amp_proc.hh"
#include "color/color_selector_pop.hh"
#include "color/color_file_io.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_centering_form.hh"
#include <Xm/Label.h>

static String defres[] = {
  "*cbb_title.labelString:          Color Bar Builder",
  "*cbb_title.fontList:             -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
  0};

ColorBarBuilderPop::ColorBarBuilderPop (Widget parent, char *name,
  HelpCtx hctx, int max_levels, ColorFileIO *fio) :
  SLFPopSep (parent, name, FP_DOALL, hctx, False, False, True, max_levels),
  _name        (name),
  _max_levels  (max_levels),
  _fio         (fio),
  _amp         (0),
  _status      (1)
{
  setDefaultResources (parent, name, defres);
  init ();
}

ColorBarBuilderPop::ColorBarBuilderPop (SLDelay *container, char *name,
  HelpCtx hctx, int max_levels, ColorFileIO *fio) :
  SLFPopSep (container, name, FP_DOALL, hctx, False, False, True,
    max_levels),
  _name        (name),
  _max_levels  (max_levels),
  _fio         (fio),
  _amp         (0),
  _status      (1)
{
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
    make (container->topWidget());
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
  init();
}

ColorBarBuilderPop::~ColorBarBuilderPop ()
{
  if (_col_sel_pop)  delete _col_sel_pop,  _col_sel_pop  = 0;

  if (_levels_gui)   delete _levels_gui,   _levels_gui   = 0;
  if (_col_sys_gui)  delete _col_sys_gui,  _col_sys_gui  = 0;
  if (_attr_rng_gui) delete _attr_rng_gui, _attr_rng_gui = 0;
  if (_col_fill_gui) delete _col_fill_gui, _col_fill_gui = 0;
  if (_col_ext_gui)  delete _col_ext_gui,  _col_ext_gui  = 0;
  if (_col_fio_gui)  delete _col_fio_gui,  _col_fio_gui  = 0;
  if (_col_ro_gui)   delete _col_ro_gui,   _col_ro_gui   = 0;
  if (_col_set_gui)  delete _col_set_gui,  _col_set_gui  = 0;

  if (_cbt) delete _cbt, _cbt = 0;
  if (_cfg) delete _cfg, _cfg = 0;
  if (_ceg) delete _ceg, _ceg = 0;
  if (_crg) delete _crg, _crg = 0;

  if (_rgb_fill_settings) delete _rgb_fill_settings, _rgb_fill_settings = 0;
  if (_bhs_fill_settings) delete _bhs_fill_settings, _bhs_fill_settings = 0;
  if (_rgb_ext_settings)  delete _rgb_ext_settings,  _rgb_ext_settings  = 0;
  if (_bhs_ext_settings)  delete _bhs_ext_settings,  _bhs_ext_settings  = 0;
}

Widget ColorBarBuilderPop::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLFPopSep::make (parent);

  Widget cbb_title = XtVaCreateManagedWidget ("cbb_title",
                         xmLabelWidgetClass,  _cbt->topWidget(),
                         NULL);

  XtVaSetValues (_cbt->W(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNrightAttachment,  XmATTACH_FORM,
                         NULL);

  XtVaSetValues (_levels_gui->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _cbt->W(),
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       1,
                         NULL);

  XtVaSetValues (_col_sys_gui->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _levels_gui->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _levels_gui->W(),
                         XmNleftOffset,       10,
                         NULL);

  XtVaSetValues (_attr_rng_gui->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _levels_gui->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _col_sys_gui->W(),
                         XmNleftOffset,       10,
                         NULL);

  XtVaSetValues (_col_set_gui->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _levels_gui->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _attr_rng_gui->W(),
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _crg->W(),
                         XmNwidth,            50,
                         NULL);

  XtVaSetValues (_cfg->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _col_sys_gui->W(),
                         XmNtopOffset,        7,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _levels_gui->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _col_sys_gui->W(),
                         NULL);

  XtVaSetValues (_ceg->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _cfg->W(),
                         XmNtopOffset,        10,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _cfg->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _attr_rng_gui->W(),
                         NULL);

  XtVaSetValues (_col_fio_gui->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _ceg->W(),
                         XmNtopOffset,        10,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _levels_gui->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _attr_rng_gui->W(),
                         NULL);

  XtVaSetValues (_crg->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _col_fio_gui->W(),
                         XmNtopOffset,        10,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _levels_gui->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _attr_rng_gui->W(),
                         NULL);

  Widget phantomR = XtVaCreateManagedWidget ("",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _col_set_gui->W(),
                         XmNrightAttachment,  XmATTACH_FORM,
                         NULL);

  Widget phantomB = XtVaCreateManagedWidget ("",
                         xmLabelWidgetClass,  topWidget(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _crg->W(),
                         XmNtopOffset,        1,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     bottomSeparator(),
                         XmNbottomOffset,     1,
                         NULL);

  defaultButton (FP_APPLY);

  return topWidget ();
}

void ColorBarBuilderPop::DoAction ()
{
  int status;

  if (_col_fio_gui) {
    if (_col_fio_gui->good()) {
// here write the data if possible
      status = _fio->writeColorFile (this, _col_fio_gui->outputFile(),
        _col_set_gui->RGBSet());
    }
    else {
      status = 0;
    }
    if (status) {
// communicate beyond that this is the current file
      _fio->setFileOut (_col_fio_gui->outputFile());
    }
  }
// store the current RGB set 
  _col_set_gui->storeRGBSet ();
}

void ColorBarBuilderPop::UndoInput ()
{
// store the current RGB set 
  _col_set_gui->storeRGBSet ();
}

CBBColFillSettings *ColorBarBuilderPop::colFillSetter ()
{
  if (_col_sys_gui) {
    if (_col_sys_gui->BHSSelected()) {
      return _bhs_fill_settings;
    }
  }
  return _rgb_fill_settings;
}

CBBColExtSettings *ColorBarBuilderPop::colExtSetter ()
{
  if (_col_sys_gui) {
    if (_col_sys_gui->BHSSelected()) {
      return _bhs_ext_settings;
    }
  }
  return _rgb_ext_settings;
}

void ColorBarBuilderPop::getAmps ()
{
  if (_amp) delete _amp, _amp = 0;
  _amp = new CBBCPSAmpProc ();
  _amp->setMinimum ((float)0);
  _amp->setMaximum ((float)1);
  _amp->setLevels (_max_levels);
  _amp->setCellsPerLevel (2);
}

void ColorBarBuilderPop::managing ()
{
  _col_set_gui->manage ();
}

Boolean ColorBarBuilderPop::setFileIn (char *filename)
{
  return _col_fio_gui->setInFile (filename);  
}

void ColorBarBuilderPop::changeRGB ()
{
  _col_fio_gui->setInFile (0);
  _col_set_gui->changeRGB ();
}

void ColorBarBuilderPop::init ()
{
  _rgb_fill_settings = 0;
  _bhs_fill_settings = 0;
  _rgb_ext_settings  = 0;
  _bhs_ext_settings  = 0;

  _col_sys_gui  = 0;
  _attr_rng_gui = 0;
  _col_fill_gui = 0;
  _col_ext_gui  = 0;
  _col_fio_gui  = 0;
  _col_ro_gui   = 0;
  _col_set_gui  = 0;

  _rgb_fill_settings = new CBBColFillSettings ();
  _bhs_fill_settings = new CBBColFillSettings ();
  _rgb_ext_settings  = new CBBColExtSettings ();
  _bhs_ext_settings  = new CBBColExtSettings ();

  _bhs_ext_settings->setCC0Cycles (.5);
  _bhs_ext_settings->setCC1Cycles (3);
  _bhs_ext_settings->setCC2Cycles (0);

  _cbt = new SLCenteringForm (this, "BB", getHelpCtx());
  _cfg = new SLCenteringForm (this, "BB", getHelpCtx());
  _ceg = new SLCenteringForm (this, "BB", getHelpCtx());
  _crg = new SLCenteringForm (this, "BB", getHelpCtx());

  getAmps ();

  _levels_gui   = new CBBLevelsGui  (this, _name, getHelpCtx(), this);
  _col_sys_gui  = new CBBColSysGui  (this, _name, getHelpCtx(), this);
  _attr_rng_gui = new CBBAttrRngGui (this, _name, getHelpCtx(), this);
  _col_fill_gui = new CBBColFillGui (_cfg, _name, getHelpCtx(), this);
  _col_ext_gui  = new CBBColExtGui  (_ceg, _name, getHelpCtx(), this);
  _col_set_gui  = new CBBColSetGui  (this, _name, getHelpCtx(), this);
  _col_fio_gui  = new CBBColFIOGui  (this, _name, getHelpCtx(), this);
  _col_ro_gui   = new CBBColROGui   (_crg, _name, getHelpCtx(), this);

  _col_sel_pop  = new ColorSelectorPop (this, "color_selector",
    getHelpCtx(), this, (int)33);

  DoAction ();
}
