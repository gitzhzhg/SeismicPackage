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
// class that creates the color system gui for color bar builder

#include "color/cbb_col_sys_gui.hh"
#include "color/color_bar_builder_pop.hh"
#include "color/cbb_col_fill_gui.hh"
#include "color/cbb_col_ext_gui.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"
#include <Xm/Label.h>

static String defres[] = {
  "*title.labelString:   Color System",
  "*rgb.labelString:     RGB",
  "*bhs.labelString:     BHS",
  "*gray.labelString:    Gray",
  0};

static SLRadio colsys[] = {
  { "rgb",   CBBColSysGui::RGB },
  { "bhs",   CBBColSysGui::BHS }
};

static SLTog gray[] = {
  { "gray",  NULL, CBBColSysGui::GRAY }
};

CBBColSysGui::CBBColSysGui (Widget parent, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (parent, name, hctx, True),
  _cbb_pop  (cbb_pop)
{
// set the default resources
  setDefaultResources (parent, name, defres);
  init ();
}

CBBColSysGui::CBBColSysGui (SLDelay *container, char *name, HelpCtx hctx,
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

void CBBColSysGui::init ()
{
// color system radio box
  _colsys_box = new SLRadioBox (this, "colsys", getHelpCtx(), colsys,
     XtNumber(colsys), NULL, True, False);

  _colsys_box->setComplexNotify (this);

// gray toggle
  _gray_box = new SLTogBox (this, "gray", getHelpCtx(), gray,
    XtNumber(gray), True);

  _gray_box->setComplexNotify (this);

}

CBBColSysGui::~CBBColSysGui ()
{
  if (_colsys_box) delete _colsys_box, _colsys_box = 0;
  if (_gray_box)   delete _gray_box,   _gray_box   = 0;
}

Widget CBBColSysGui::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make (parent);

  Widget lab = XtVaCreateManagedWidget ("title",
                         xmLabelWidgetClass,  topWidget(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNleftAttachment,   XmATTACH_FORM,
                         NULL);

  XtVaSetValues (_colsys_box->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        lab,
                         XmNbottomOffset,     10,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       lab,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      lab,
                         NULL);

  XtVaSetValues (_gray_box->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _colsys_box->W(),
                         XmNbottomOffset,     10,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       lab,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      lab,
                         NULL);

  selectRGB ();
  selectGRAY (False);
  _gray_selected = False;
  return topWidget ();

}

Boolean CBBColSysGui::notifyComplex (SLDelay *obj, int ident)
{
  if (obj == _colsys_box) {
// radio button was pushed
    if (ident == RGB) {
// RGB color system selected
      selectRGB ();
    }
    else if (ident == BHS) {
// BHS color system selected
      selectBHS ();
    }
  }
  if (obj == _gray_box) {
// toggle was changed
    if (ident == GRAY) {
      if      (!GRAYSelected() &&  _gray_selected) selectGRAY (False);
      else if ( GRAYSelected() && !_gray_selected) selectGRAY (True);
    }
  }
  if (_cbb_pop->_col_fill_gui) _cbb_pop->_col_fill_gui->setState ();
  if (_cbb_pop->_col_ext_gui)  _cbb_pop->_col_ext_gui ->setState ();
  return True;
}

void CBBColSysGui::selectRGB ()
{
  _colsys_box->SetRadio (RGB);
}

void CBBColSysGui::selectBHS ()
{
  _colsys_box->SetRadio (BHS);
}

void CBBColSysGui::selectGRAY (Boolean set)
{
  _gray_selected = set;
  _gray_box->SetTog (GRAY, set);
}

int CBBColSysGui::RGBSelected ()
{
  return (int)(_colsys_box->WhichSelected() == RGB);
}

int CBBColSysGui::BHSSelected ()
{
  return (int)(_colsys_box->WhichSelected() == BHS);
}

int CBBColSysGui::GRAYSelected ()
{
  return (int)(_gray_box->IsSelected(GRAY));
}
