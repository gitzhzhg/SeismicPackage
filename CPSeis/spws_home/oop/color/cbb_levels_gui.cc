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
// class that creates the color levels gui for a color bar builder

#include "color/cbb_levels_gui.hh"
#include "color/color_bar_builder_pop.hh"
#include "color/color_selector_pop.hh"
#include "color/cbb_col_set_gui.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_scale_text_arrow.hh"
#include "sl/sl_push_box.hh"
#include <Xm/Label.h>

static String defres[] = {
  "*levels.labelString:        Levels:",
  "*selector.labelString:      Selector...",
  "*controller.labelString:    Controller...",
  0};

static SLPush buttons[] = {
  { "selector",   CBBLevelsGui::SELECTOR   },
  { "controller", CBBLevelsGui::CONTROLLER }
};

CBBLevelsGui::CBBLevelsGui (Widget parent, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (parent, name, hctx, True),
  _cbb_pop  (cbb_pop)
{

// set the default resources
  setDefaultResources (parent, name, defres);
  init ();
}

CBBLevelsGui::CBBLevelsGui (SLDelay *container, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (container, name, hctx, True),
  _cbb_pop  (cbb_pop)
{

// set the default resources
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
    make (container->topWidget());
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
  init ();
}

void CBBLevelsGui::init ()
{
  _levels_arrow = 0;
  _parms_box    = 0;

// levels specifier
  assert (_cbb_pop->_max_levels > 1);
  if (_cbb_pop->_max_levels >= 32)
    _levels = (float)32;
  else
    _levels = _cbb_pop->_max_levels;

  _levels_arrow = new SLScaleTextArrow (this, "lev_arrow", getHelpCtx(),
    &_levels, True, _levels, (float)2, (float)(_cbb_pop->_max_levels),
    (float)1, (int)0, (int)3, XmHORIZONTAL, "levels");

  _levels_arrow->setComplexNotify (this);

// parameter selection push buttons
  _parms_box = new SLPushBox (this, "parms", getHelpCtx(), buttons,
    XtNumber(buttons));

  _parms_box->setComplexNotify (this);
}

CBBLevelsGui::~CBBLevelsGui ()
{
  if (_parms_box)    delete _parms_box,    _parms_box    = 0;
}

Widget CBBLevelsGui::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make (parent);

  XtVaSetValues (_levels_arrow->W(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNtopOffset,        1,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       1,
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNrightOffset,      1,
                         NULL);

  XtVaSetValues (_parms_box->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _levels_arrow->W(),
                         XmNtopOffset,        1,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _levels_arrow->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _levels_arrow->W(),
                         NULL);

  return topWidget ();
}

Boolean CBBLevelsGui::notifyComplex (SLDelay *obj, int ident)
{
  int dummy;
  if (obj == _levels_arrow) {
// levels were changed
    _cbb_pop->_col_set_gui->levelsChanged ();
  }
  else if (obj == _parms_box) {
// push button was pushed
    if (ident == SELECTOR) {
// manage color selector
      _cbb_pop->_col_sel_pop->manage ();
    }
    else if (ident == CONTROLLER) {
// manage color bar controller
      dummy = 2;  // do something useful
    }
  }
  return True;
}

void CBBLevelsGui::setLevels (int levels)
{
  if (_levels_arrow) {
    if (levels < 1 || levels > _cbb_pop->_max_levels)
      levels = _cbb_pop->_max_levels;
    _levels_arrow->setValue ((float)levels);
  }
}
